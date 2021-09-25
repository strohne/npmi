# "Pointwise  mutual  information  (PMI,  5)  is  a  measure  of
# how  much  the  actual probability of a particular co-occurrence
# of events p(x, y) differs from what we would expect it to be on the
# basis of the probabilities of the individual eventsand the assumption
# of independence p(x)p(y)." (Bouma 2009)
# Problem: PMI und relative risk werden höher für seltenere Kombinationen,
# weil sich das Maximum bewegt (Bouma 2009)
# Besser: NPMI mit festen Grenzen -1 bis +1

# https://pdfs.semanticscholar.org/1521/8d9c029cbb903ae7c729b2c644c24994c201.pdf
# https://stats.stackexchange.com/questions/140935/how-does-the-logpx-y-normalize-the-point-wise-mutual-information


#' Count cooccurrence and calculate probabilities and npmi
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
  pairs <- pairs[features[, .(feature_source = feature,p_source=p_items)],on="feature_source"]

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
resample_cooccurrence <- function(data, trials=10000, smoothing=0) {

  # Get shuffled data
  data_resample <- tibble(no = c(1:trials)) %>%
    mutate(co = furrr::future_map(
      no,
      ~shuffle_cooccurrence(data),
      .progress = T,
      .options = furrr::furrr_options(seed = TRUE))
    ) %>%
    unnest(co)

  # Get trace of mean
  trace <- data_resample %>%
    dplyr::group_by(feature_source,feature_target)  %>%
    dplyr::arrange(no) %>%
    dplyr::mutate(p_cond = cumsum(p_cond) / no) %>%
    dplyr::ungroup() %>%
    dplyr::select(feature_source,feature_target,no,p_cond)

  # Calculate confidence interval
  data_resample <- data_resample %>%
    dplyr::group_by(feature_source,feature_target)  %>%
    dplyr::summarize(p_cond_lo = quantile(p_cond, 0.025, type=1),
                     p_cond_med = quantile(p_cond, 0.5, type=1),
                     p_cond_mean = mean(p_cond),
                     p_cond_hi = quantile(p_cond, 0.975, type=1),
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
    dplyr::left_join(data_resample,by=c("feature_source", "feature_target"))

  # Smoothing / pseudocount
  # -> set smoothing to 1  to apply Laplace's rule of succession
  if (smoothing > 0) {
    smoothing <- smoothing / (smoothing * trials)
  }

  # Ratio of resampled values
  # -> boot.pmi and boot.npmi only meaningful for p values (including p_cond)
  # -> if value.mean == 0 -> too few samples, should be avoided
  pairs <- pairs %>%
    dplyr::mutate(ratio = (p_cond + smoothing) / (p_cond_mean + smoothing)) %>%
    dplyr::mutate(pmi =   case_when(
      p_cond == 0 ~ -Inf,
      p_cond_mean == 0 ~ Inf,
      TRUE ~ log2(ratio)
    )) %>%
    dplyr::mutate(npmi =  case_when(
      pmi == -Inf ~ -1,
      pmi == Inf ~ 1,
      pmi == 0 ~ 0,
      pmi > 0 ~ pmi / -log2(p_cond_mean + smoothing), # -log(x) == log(1/x)
      pmi < 0 ~ pmi / -log2(p_cond + smoothing)
    )) %>%

    # Significance compared to CI
    dplyr::mutate(sig_hi = (p_cond > p_cond_hi) ) %>%
    dplyr::mutate(sig_lo = (p_cond < p_cond_lo) ) %>%
    dplyr::mutate(sig = sig_hi | sig_lo)

  return (list("pairs"=pairs,"trace"=trace))
}
