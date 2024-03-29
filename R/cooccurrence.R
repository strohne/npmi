#' Count cooccurrence and calculate probabilities and npmi.
#' Cooccurrence is calculated on the basis of conditional probabilities.
#'
#' @param data A data frame containing the columns item, feature and optionally weight.
#'             - item contains the ID of a document (e.g. a comment)
#'             - feature contains a category  (e.g. the topic of the comment)
#'             - weight  The weight of the feature ranges from 0 to 1 (e.g. how prevalent is the topic in the comment?). Will be initialized with 1 if missing.
#'             Items with multiple features should occur multiple times in the data table.
#'             Make sure not to include duplicates (that's where the weight kicks in, use it!).
#'             The feature and item columns can contain numeric IDs, character strings or factors.
#'             Provide character strings or factors to complete combinations not found in the data.
#' @return A tibble
#' @import data.table
#' @export
get_cooccurrence <- function(data, .progress = NULL) {
  if (!is.null(.progress)) {
    .progress$tick()$print()
  }

  # Convert to data.table
  setDT(data)

  # Create weight if not present
  if (!("weight" %in% colnames(data)))
    data[, weight := 1]


  # Count cooccurrence and rename columns
  pairs <- count_pairs(data)
  setDT(pairs)


  # Share of feature in all items
  items_count <- length(unique(data[,item]))

  features <- copy(data)
  features <- features[, .(n_items = sum(weight, na.rm = T)), by = feature]
  features[, p_items := n_items / items_count]

  # Share of items with cooccurring feature in all items
  pairs[, p := n / items_count]
  pairs <- pairs[features[, .(source = feature,p_source=p_items)],on="source"]

  # Conditional probability
  pairs[, p_cond := p / p_source]

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
shuffle_cooccurrence <- function(data, .progress = NULL) {

  # Convert to data.table
  setDT(data)
  data <- copy(data)

  #TODO: Beim shufflen keine Duplikate erzeugen
  data[, feature := sample(feature,length(feature),replace=F)]
  data <- unique(data, by = c("item", "feature"))

  get_cooccurrence(data, .progress)
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
resample_cooccurrence <- function(data, trials=10000, smoothing=0, sig.level = 0.05) {

  # Get shuffled data
  data_resample <- tibble(no = c(1:trials)) %>%
    mutate(co = furrr::future_map(
      no,
      ~shuffle_cooccurrence(data),
      .progress = T,
      .options = furrr::furrr_options(seed = TRUE))
    ) %>%
    unnest(co) %>%
    filter(source != target)


  # Get trace of mean
  trace <- data_resample %>%
    dplyr::group_by(source,target)  %>%
    dplyr::arrange(no) %>%
    dplyr::mutate(p_cond = cumsum(p_cond) / no) %>%
    dplyr::ungroup() %>%
    dplyr::select(source,target,no,p_cond)

  # Calculate confidence interval
  data_resample <- data_resample %>%
    dplyr::group_by(source,target)  %>%
    dplyr::summarize(p_cond_lo = quantile(p_cond,  sig.level / 2, type=1),
                     p_cond_med = quantile(p_cond, 0.5, type=1),
                     p_cond_mean = mean(p_cond),
                     p_cond_hi = quantile(p_cond, 1 - (sig.level /2), type=1),
                     .groups = 'keep') %>%
    dplyr::ungroup()

  # Check if all cases were resampled
  m_0 <- dplyr::filter(data_resample,p_cond_mean == 0) %>% dplyr::count()
  m_1 <- dplyr::filter(data_resample,p_cond_mean == 1) %>% dplyr::count()

  if (m_0 > 0)
    warning(paste0(m_0," pairs never occured in resampling, probability not reliable. Increase number of trials!"))

  if (m_1 > 0)
    warning(paste0(m_1," pairs always occured in resampling, probability not reliable. Increase number of trials!"))


  # Compare to data
  pairs <- get_cooccurrence(data) %>%
    dplyr::left_join(data_resample,by=c("source", "target"))

  # Calculate npmi
  if (smoothing > 0) {
    smoothing <- smoothing / (smoothing * trials)
  }

  pairs <- pairs  %>%
    npmi(p_cond, p_cond_mean, smoothing) %>%
    conf(p_cond, p_cond_lo, p_cond_hi)

  return (list("pairs"=pairs,"trace"=trace))
}
