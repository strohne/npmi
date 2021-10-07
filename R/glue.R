#' Extract the items that glue together the features. The result will contain an additional
#' column with the item in which always two features occur.
#'
#' @param pairs A data frame with cooccurrences or sequences, e.g. the result of
#'              resample_cooccurrence, with at least the following columns:
#'              - source The source feature
#'              - target The target feature
#' @param data A data frame containing the items with their features, e.g. the input tu resample_cooccurrence,
#'             with at least the following columns:
#'             - item contains the ID of a document (e.g. a comment)
#'             - feature contains a category  (e.g. the topic of the comment)
#' @return A tibble
#' @export
#'
get_glue <- function(pairs, items) {
  pairs %>%
    inner_join(select(items,item,feature) ,by=c("source"="feature")) %>%
    semi_join(select(items,item,feature),by=c("target"="feature","item"))
}
