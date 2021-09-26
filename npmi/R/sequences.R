#' Count sequences and calculate probabilities and npmi
#'
#' @param data A data frame containing the columns item, feature and optionally weight.
#'             - item contains the ID of a document (e.g. a comment)
#'             _ item_prev contains the ID of a preceding document (e.g. a post)
#'             - feature contains a category  (e.g. the topic of the comment)
#'             - weight  The weight of the feature ranges from 0 to 1 (e.g. how prevalent is the topic in the comment?). Will be initialized with 1 if missing.
#'             Items with multiple features should occur multiple times in the data table.
#'             Make sure not to include duplicates (that's where the weight kicks in, use it!).
#'             The feature and item columns can contain numeric IDs, character strings or factors.
#'             Provide character strings or factors to complete combinations not found in the data.
#' @return A tibble
#' @import data.table
#' @export
get_sequences <- function(data, .progress = NULL) {
  if (!is.null(.progress)) {
    .progress$tick()$print()
  }

  # Convert to data.table
  setDT(data)
  data <- unique(data, by = c("item", "item_prev", "feature"))

  # Count sequences
  pairs <- count_sequences(data)
  setDT(pairs)

  # Maximum number of possible sequences
  seq_count <- length(unique(data[!is.na(item_prev),item]))

  # Share of all possible sequences
  pairs[, p := n / seq_count]

  pairs
}




#' Shuffle features for resampling
#'
#' @param data A data frame containing the columns item, feature and optionally weight.
#'             - item contains the ID of a document (e.g. a comment)
#'             - feature contains a category  (e.g. the topic of the comment)
#'             - weight  The weight of the feature ranges from 0 to 1 (e.g. how prevalent is the topic in the comment?). Will be initialized with 1 if missing.
#'             Items with multiple features should occur multiple times in the data table.
#'             Make sure not to include duplicates (that's where the weight kicks in, use it!).
#'             The feature and item columns can contain numeric IDs, character strings or factors.
#'             Provide character strings or factors to complete combinations not found in the data.
#' @param .progress F|T For internal use of resample_cooccurrence. Passes the progress bar from the furrr package.
#' @return A tibble
#' @import data.table
#' @export
shuffle_sequences <- function(data, .progress = NULL) {

  # Convert to data.table
  setDT(data)
  data <- copy(data)

  #TODO: Beim shufflen keine Duplikate erzeugen
  data[, feature := sample(feature,length(feature),replace=F)]
  data <- unique(data, by = c("item", "feature"))

  get_sequences(data, .progress)
}


#' Calculate npmi and probabilities on the basis of resampling
#'
#' @param data A data frame containing the columns item, feature and optionally weight.
#'             - item contains the ID of a document (e.g. a comment)
#'             - feature contains a category  (e.g. the topic of the comment)
#'             - weight  The weight of the feature ranges from 0 to 1 (e.g. how prevalent is the topic in the comment?). Will be initialized with 1 if missing.
#'             Items with multiple features should occur multiple times in the data table.
#'             Make sure not to include duplicates (that's where the weight kicks in, use it!).
#'             The feature and item columns can contain numeric IDs, character strings or factors.
#'             Provide character strings or factors to complete combinations not found in the data.
#' @param trials Number of resamples
#' @param smoothing Set smoothing to 1 to apply Laplace's rule of succession
#' @return A tibble containing resampleped (random permutation test) cooccurrence metrics.
#'         Resampled metrics are to be found in columns with the boot prefix.
#'         Use boot.npmi for the p_cond_source metric to find extraordinary cooccurrence
#' @import data.table
#' @export
resample_sequences <- function(data, trials=10000, smoothing=0, sig.level=0.05) {

  # Get shuffled data
  data_resample <- tibble(no = c(1:trials)) %>%
    mutate(co = furrr::future_map(
      no,
      ~shuffle_sequences(data),
      .progress = T,
      .options = furrr::furrr_options(seed = TRUE))
    ) %>%
    unnest(co)

  # Get trace of mean
  trace <- data_resample %>%
    dplyr::group_by(source,target)  %>%
    dplyr::arrange(no) %>%
    dplyr::mutate(p = cumsum(p) / no) %>%
    dplyr::ungroup() %>%
    dplyr::select(source,target,no,p)

  # Calculate confidence interval
  data_resample <- data_resample %>%
    dplyr::group_by(source,target)  %>%
    dplyr::summarize(p_lo = quantile(p, sig.level / 2, type=1),
                     p_med = quantile(p, 0.5, type=1),
                     p_mean = mean(p),
                     p_hi = quantile(p, 1 - (sig.level / 2), type=1),
                     .groups = 'keep') %>%
    dplyr::ungroup()

  # Check if all cases were resampled
  m_0 <- dplyr::filter(data_resample,p_mean == 0) %>% dplyr::count()
  m_1 <- dplyr::filter(data_resample,p_mean == 1) %>% dplyr::count()

  if (m_0 > 0)
    warning(paste0(m_0," pairs never occured in resampling, probability not reliable. Increase number of trials!"))

  if (m_1 > 0)
    warning(paste0(m_1," pairs always occured in resampling, probability not reliable. Increase number of trials!"))


  # Compare to data
  pairs <- get_sequences(data) %>%
    dplyr::left_join(data_resample,by=c("source", "target"))

  # Calculate npmi
  if (smoothing > 0) {
    smoothing <- smoothing / (smoothing * trials)
  }

  pairs <- pairs  %>%
    npmi(p, p_mean, smoothing) %>%
    conf(p, p_lo, p_hi)


  return (list("pairs"=pairs,"trace"=trace))
}

#' Add id of previous item
#' @export
add_previous_item <- function(data, col_item=item, col_parent=item_parent, col_order=item) {
  col_item <- enquo(col_item)
  col_parent <- enquo(col_parent)
  col_order <- enquo(col_order)

  items <- data %>%
    distinct(!!col_item,.keep_all=T) %>%
    group_by(!!col_parent) %>%
    arrange(!!col_order) %>%
    mutate(item_prev = ifelse(row_number() == 1,!!col_parent, lag(!!col_item))) %>%
    ungroup() %>%
    select(.id = !!col_item,item_prev)

  data %>%
    mutate(.id = !!col_item) %>%
    left_join(items,by=".id") %>%
    select(-.id)
}
