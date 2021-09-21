#
# Count functions
#

# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Count cooccurrence of features in items
#'
#' @param data A data frame containing the columns item, feature and optionally weight.
#'             - item contains the ID of a document (e.g. a comment)
#'             - feature contains a category  (e.g. the topic of the comment)
#'             - weight  The weight of the feature ranges from 0 to 1 (e.g. how prevalent is the topic in the comment?). Will be initialized with 1 if missing.
#'             Items with multiple features should occur multiple times in the data table.
#'             Make sure not to include duplicates (that's where the weight kicks in, use it!).
#' @return A tibble
#' @export
count_pairs <- function(data) {

  # Add weight
  if (!("weight" %in% colnames(data))) {
    data$weight <- 1
  }

  # Convert to factors
  data$item <- as.factor(data$item)
  data$feature <- as.factor(data$feature)
  features <- levels(data$feature)

  # Convert to matrix
  data <- Matrix::sparseMatrix(
    i = as.numeric(data$item),
    j = as.numeric(data$feature),
    x = data$weight,
    dimnames=list(
      levels(data$item),
      levels(data$feature)
      )
    )

  # Pairwise count
  pairs <- Matrix::t(data) %*% (data > 0)

  diag(pairs) <- 0

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
  colnames(pairs) <- c("feature1","feature2","n")

  # Add labels
  pairs$feature1 <- factor(pairs$feature1,levels=c(1:length(features)),labels=features)
  pairs$feature2 <- factor(pairs$feature2,levels=c(1:length(features)),labels=features)

  # Complete
  pairs <- pairs %>%
    tibble::as_tibble() %>%
    tidyr::complete(feature1,feature2,fill=list(n=0))

  # Convert labels to character
  pairs$feature1 <- as.character(pairs$feature1)
  pairs$feature2 <- as.character(pairs$feature2)


  return(pairs)
}



#' Count sequences of features from sequences of items
#'
#' @param data A data.frame containing the columns item, item_prev, feature.
#'             - item contain the ID of a document (e.g. a comment)
#'             - item_prev contains the ID of the preceding document (e.g. the parent comment)
#'             - feature contains a category  (e.g. the topic of the comment)
#'             Items with multiple features should occur multiple times in the data table.
#'             TODO: add weight option (instead of distinct item-feature-combinations)
#' @return A tibble
#' @export
#' @import data.table
count_sequences <- function(data) {

  # Convert to data.table
  if (!data.table::is.data.table(data)) {
    data.table::setDT(data)
  }
  data <- data.table::copy(data)

  # Convert to factor (in order to later complete combinations)
  data$feature <- as.factor(data$feature)

  # Distinct item-feature-combinations
  data <- unique(data[,.(item,item_prev,feature)])

  # Inner self join
  data <- data[data[,.(feature_prev = feature, item_prev = item)],
               on = "item_prev", nomatch = 0]

  # Count
  #data <- unique(data, by = c("item","feature","item_prev","feature_prev"))
  data <- data[, .(n=.N), by=c("feature", "feature_prev")]

  # Rename columns
  data <- data[,.(feature_prev,feature_next=feature,n)]

  # Complete
  data <- data %>%
    tibble::as_tibble() %>%
    tidyr::complete(feature_prev,feature_next,fill=list(n=0))


  # Alternative using data.table (but empty factors are ommitted)
  # data <- data[CJ(feature = feature_prev,
  #                 feature_prev = feature_prev,
  #                 unique=TRUE
  #                 ),
  #                 on=.(feature, feature_prev)]
  # setnafill(data, fill = 0, cols = 'n')

  # Convert labels to character
  data$feature_prev <- as.character(data$feature_prev)
  data$feature_next <- as.character(data$feature_next)

  return(data)
}



#' Add a column containing the ID of the previous item
#'
#' (not implemented yet)
add_prev <- function(data) {

}

