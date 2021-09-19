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
#' @param data Data table containin the columns item, feature and optionally weight.
#'             The weight column will be initialized with 1 if missing.
#' @return A data table
#' @export
count_pairs <- function(data, upper=F, diag=F) {

  # Convert to matrix
  data$item <- as.factor(data$item)
  data$feature <- as.factor(data$feature)
  features <- levels(data$feature)

  data <- Matrix::sparseMatrix(
    i = as.numeric(data$item),
    j=as.numeric(data$feature),
    x=data$weight,
    dimnames=list(
      levels(data$item),
      levels(data$feature)
      )
    )

  # Pairwise count
  pairs <- Matrix::t(data) %*% data

  # To data.frame
  pairs <- as.data.frame(Matrix::summary(pairs))
  attr(pairs,"header") <- NULL

  if (!upper) {
    pairs <- filter(pairs, i <= j)
  }
  if (!diag) {
    pairs <- filter(pairs, i != j)
  }

  # Rename columns
  colnames(pairs) <- c("item1","item2","n")

  # Add labels
  pairs$item1 <- factor(pairs$item1,levels=c(1:length(features)),labels=features)
  pairs$item2 <- factor(pairs$item2,levels=c(1:length(features)),labels=features)
  pairs$item1 <- as.character(pairs$item1)
  pairs$item2 <- as.character(pairs$item2)

  return(pairs)
}
