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
#' @param metrics T|F Whether to calculate p, p_cond_source, p_cond_target
#' @param npmi T|F Whether to calculate npmi (without resampling)
#' @return A tibble
#' @import data.table
#' @export
get_cooccurrence <- function(data, metrics = T, npmi = F,.progress = NULL) {
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


  if (metrics) {

    # Share of feature in all items
    # -> propability of feature in an item
    features <- copy(data)
    features <- features[, .(n_items = sum(weight, na.rm = T)), by = feature]
    features[, p_items := n_items / length(unique(data[,item]))]

    # Share of items with cooccurring feature in all items
    pairs[, p := n / length(unique(data[,item]))]
    pairs <- pairs[features[, .(feature_source = feature,n_source=n_items,p_source=p_items)],on="feature_source"]
    pairs <- pairs[features[, .(feature_target = feature,n_target=n_items,p_target=p_items)],on="feature_target"]

    # Conditional probability
    pairs[, p_cond_source := p / p_source]
    pairs[, p_cond_target := p / p_target]

    # LLR
    #codings.lag0 <- calculate_llr(codings.lag0)

  }

  if (npmi) {

    # Relative Risk
    pairs[, irr := (p / (p_target * p_source))]

    # PMI
    pairs[, pmi := log2(irr)]
    pairs[, npmi := pmi / ( -log2(p))]
    pairs[, npmi := ifelse(is.nan(npmi),-1,npmi)]

  }

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
#' @param metrics T|F Whether to calculate p, p_cond_source, p_cond_target
#' @param npmi T|F Whether to calculate npmi (without resampling)
#' @param .progress F|T For internal use of resample_cooccurrence. Passes the progress bar from the furrr package.
#' @return A tibble
#' @import data.table
#' @export
shuffle_cooccurrence <- function(data, metrics = T, npmi = F, .progress = NULL) {

  # Convert to data.table
  setDT(data)
  data <- copy(data)

  #TODO: Beim shufflen keine Duplikate erzeugen
  data[, feature := sample(feature,length(feature),replace=F)]
  data <- unique(data, by = c("item", "feature"))

  get_cooccurrence(data, metrics,npmi,.progress)
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
  data.resample <- tibble(no = c(1:trials)) %>%
    mutate(co = furrr::future_map(
      no,
      ~shuffle_cooccurrence(data, metrics=T),
      .progress = T,
      .options = furrr::furrr_options(seed = TRUE))
    ) %>%
    unnest(co)

  # Calculate confidence interval
  data.resample <- data.resample %>%
    tidyr::gather(metric,value,-no,-feature_source,-feature_target) %>%
    dplyr::group_by(feature_source,feature_target,metric)  %>%
    dplyr::summarize(value_lo = quantile(value, 0.025, type=1),
                     value_med = quantile(value, 0.5, type=1),
                     value_mean = mean(value),
                     value_hi = quantile(value, 0.975, type=1),
                     .groups = 'keep') %>%
    dplyr::ungroup()

  # Check if all cases were resampled
  m_0 <- dplyr::filter(data.resample,value_mean == 0) %>% dplyr::count()
  m_1 <- dplyr::filter(data.resample,value_mean == 1) %>% dplyr::count()

  if (m_0 > 0)
    warning(paste0(m_0," pairs never occured in resampling, probability not reliable. Increase number of trials!"))

  if (m_1 > 0)
    warning(paste0(m_1," pairs always occured in resampling, probability not reliable. Increase number of trials!"))


  # Compare to data
  pairs <- get_cooccurrence(data,metrics = T, npmi = F) %>%
    tidyr::gather(metric,value,-feature_source,-feature_target) %>%
    dplyr::left_join(data.resample,by=c("feature_source", "feature_target", "metric"))

  # Smoothing / pseudocount
  # -> set smoothing to 1  to apply Laplace's rule of succession
  if (smoothing > 0) {
    smoothing <- smoothing / (smoothing * trials)
  }

  # Ratio of resampled values
  # -> boot.pmi and boot.npmi only meaningful for p values (including p_cond_source & p_cond_target)
  # -> if value.mean == 0 -> too few samples, should be avoided
  pairs <- pairs %>%
    dplyr::mutate(boot_ratio = (value + smoothing) / (value_mean + smoothing)) %>%
    dplyr::mutate(boot_pmi =   ifelse(boot_ratio != 0, log2(boot_ratio),-Inf)) %>%
    dplyr::mutate(boot_npmi =  ifelse(boot_pmi != -Inf, boot_pmi / -log2(value_mean + smoothing),-1)) %>%

    # Significance compared to CI
    dplyr::mutate(sig_hi = (value > value_hi) ) %>%
    dplyr::mutate(sig_lo = (value < value_lo) ) %>%
    dplyr::mutate(sig = sig_hi | sig_lo)

  return (pairs)
}
