#'  Calculate ratio of probabilities and npmi
#'
#' @param data
#' @param p_actual Name of column ame containing observed p
#' @param p_expected Name of column containing expected p
#' @param smoothing Add pseudocount. Calculate the pseudocount based on the number of trials
#'        to apply Laplace's rule of succession.
npmi <- function(data, p_actual, p_expected, smoothing = 0) {
  p_actual <- dplyr::enquo(p_actual)
  p_expected <- dplyr::enquo(p_expected)

  data %>%
    dplyr::mutate(ratio = (!!p_actual + smoothing) / (!!p_expected + smoothing)) %>%
    dplyr::mutate(pmi =   case_when(
      !!p_actual == 0 ~ -Inf,
      !!p_expected == 0 ~ Inf,
      TRUE ~ log2(ratio)
    )) %>%
    dplyr::mutate(npmi =  case_when(
      pmi == -Inf ~ -1,
      pmi == Inf ~ 1,
      pmi == 0 ~ 0,
      pmi > 0 ~ pmi / -log2(!!p_expected + smoothing), # -log(x) == log(1/x)
      pmi < 0 ~ pmi / -log2(!!p_actual + smoothing)
    ))
}

#' Get significance compared to a confidence interval
conf <- function(data, p_actual, p_lo, p_hi) {
  p_actual <- dplyr::enquo(p_actual)
  p_lo <- dplyr::enquo(p_lo)
  p_hi <- dplyr::enquo(p_hi)

  data %>%
    dplyr::mutate(sig_hi = (!!p_actual > !!p_hi) ) %>%
    dplyr::mutate(sig_lo = (!!p_actual < !!p_lo) ) %>%
    dplyr::mutate(sig = sig_hi | sig_lo)
}
