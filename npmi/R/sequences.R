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
resample_sequences <- function(data, trials=10000, smoothing=0) {

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
    dplyr::group_by(feature_source,feature_target)  %>%
    dplyr::arrange(no) %>%
    dplyr::mutate(p = cumsum(p) / no) %>%
    dplyr::ungroup() %>%
    dplyr::select(feature_source,feature_target,no,p)

  # Calculate confidence interval
  data_resample <- data_resample %>%
    dplyr::group_by(feature_source,feature_target)  %>%
    dplyr::summarize(p_lo = quantile(p, 0.025, type=1),
                     p_med = quantile(p, 0.5, type=1),
                     p_mean = mean(p),
                     p_hi = quantile(p, 0.975, type=1),
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
    dplyr::left_join(data_resample,by=c("feature_source", "feature_target"))

  # Smoothing / pseudocount
  # -> set smoothing to 1  to apply Laplace's rule of succession
  if (smoothing > 0) {
    smoothing <- smoothing / (smoothing * trials)
  }

  # Ratio of resampled values
  # -> boot.pmi and boot.npmi only meaningful for p values (including p_cond_source & p_cond_target)
  # -> if value.mean == 0 -> too few samples, should be avoided
  pairs <- pairs %>%
    dplyr::mutate(ratio = (p + smoothing) / (p_mean + smoothing)) %>%

    dplyr::mutate(pmi =   case_when(
      p == 0 ~ -Inf,
      p_mean == 0 ~ Inf,
      TRUE ~ log2(ratio)
    )) %>%
    dplyr::mutate(npmi =  case_when(
      pmi == -Inf ~ -1,
      pmi == Inf ~ 1,
      pmi == 0 ~ 0,
      pmi > 0 ~ pmi / -log2(p_mean + smoothing), # -log(x) == log(1/x)
      pmi < 0 ~ pmi / -log2(p + smoothing)
    )) %>%

    # Significance compared to CI
    dplyr::mutate(sig_hi = (p > p_hi) ) %>%
    dplyr::mutate(sig_lo = (p < p_lo) ) %>%
    dplyr::mutate(sig = sig_hi | sig_lo)

  return (list("pairs"=pairs,"trace"=trace))
}
