#
# Count functions
#

#' Count cooccurrence of features in items
#'
#' @param data A data frame containing the columns item, feature and optionally weight.
#'             - item contains the ID of a document (e.g. a comment)
#'             - feature contains a category  (e.g. the topic of the comment)
#'             - weight  The weight of the feature ranges from 0 to 1 (e.g. how prevalent is the topic in the comment?).
#'             - weight will be initialized with 1 if missing.
#'             Items with multiple features should occur multiple times in the data table.
#'             Make sure not to include duplicates (that's where the weight kicks in, use it!).
#'             The feature and item columns can contain numeric IDs, character strings or factors.
#'             Provide character strings or factors to complete combinations not found in the data.
#' @return A tibble
#' @export
count_pairs <- function(data) {

  # Add weight
  if (!("weight" %in% colnames(data))) {
    data$weight <- 1
  }

  # Convert to numeric
  if (!is.numeric(data$item))
    data$item <- as.numeric(as.factor(data$item))

  features <- NULL
  if (!is.numeric(data$feature)) {
    data$feature <- as.factor(data$feature)
    features <- levels(data$feature)
    data$feature <- as.numeric(data$feature)
  }

  # Convert to matrix
  data <- Matrix::sparseMatrix(
    i = data$item,
    j = data$feature,
    x = data$weight
  )

  # Pairwise count
  pairs <- Matrix::t(data) %*% (data > 0)

  #diag(pairs) <- 0

  # To data.frame
  pairs <- as.data.frame(Matrix::summary(pairs))
  attr(pairs,"header") <- NULL

#   if (!upper) {
#     pairs <- filter(pairs, i <= j)
#   }
#   if (!diag) {
#     pairs <- filter(pairs, i != j)
#   }

  # Rename columns
  colnames(pairs) <- c("source","target","n")

  # Add labels
  if (!is.null(features)) {
    pairs$source <- factor(pairs$source,levels=c(1:length(features)),labels=features)
    pairs$target <- factor(pairs$target,levels=c(1:length(features)),labels=features)
  }

  # Complete
  pairs <- pairs %>%
    tibble::as_tibble() %>%
    tidyr::complete(source,target,fill=list(n=0))


  # Convert labels to character
  if (!is.null(features)) {
     pairs$source <- as.character(pairs$source)
     pairs$target <- as.character(pairs$target)
  }


  return(pairs)
}



#' Count sequences of features from sequences of items
#'
#' @param data A data.frame containing the columns item, item_prev, feature.
#'             - item contain the ID of a document (e.g. a comment)
#'             - item_prev contains the ID of the preceding document (e.g. the parent comment)
#'             - feature contains a category  (e.g. the topic of the comment)
#'             - weight contains the prevalence of the category in the range 0 to 1.
#'               weight Will be initialized with 1 if missing.
#'             Items with multiple features should occur multiple times in the data table.
#'             TODO: add weight option (instead of distinct item-feature-combinations)
#' @return A tibble
#' @export
#' @import data.table
count_sequences <- function(data) {

  # Add weight
  if (!("weight" %in% colnames(data))) {
    data$weight <- 1
  }

  # Convert to data.table
  data.table::setDT(data)
  data <- data.table::copy(data)

  # # Convert to factor (in order to later complete combinations)
  # features <- NULL
  # if (!is.numeric(data$feature)) {
  #   data$feature <- as.factor(data$feature)
  #   features <- levels(data$feature)
  #   data$feature <- as.numeric(data$feature)
  # }

  # Distinct item-feature-combinations
  data <- unique(data[weight > 0,.(item,item_prev,feature,weight)])

  # Inner self join
  data <- data[data[!is.na(item_prev),.(source = feature, item_prev = item,weight_prev=weight)],
               on = "item_prev", nomatch = 0, allow.cartesian=TRUE]

  # Count
  data <- data[, .(n=sum(weight*weight_prev)), by=c("feature", "source")]

  # Rename columns
  data <- data[,.(source,target=feature,n)]

  # Complete
  data <- data %>%
    tibble::as_tibble() %>%
    tidyr::complete(source,target,fill=list(n=0))

  # Alternative using data.table (but empty factors are ommitted)
  # data <- data[CJ(feature = source,
  #                 source = source,
  #                 unique=TRUE
  #                 ),
  #                 on=.(feature, source)]
  # setnafill(data, fill = 0, cols = 'n')

  # # Convert labels to character
  # if (!is.null(features)) {
  #   data$source <- as.character(data$source)
  #   data$target <- as.character(data$target)
  # }

  return(data)
}
