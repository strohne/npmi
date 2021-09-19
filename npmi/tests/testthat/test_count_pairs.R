test_that("count_pairs equals pairwise_count", {

  library(dplyr)
  library(readr)
  library(widyr)
  library(npmi)

  data <- read_csv2(system.file("extdata", "threads.csv", package = "npmi"))

  # Use widyr::pairwise_count
  data_expected <- data %>%
    widyr::pairwise_count(feature,id,upper=F, diag=F) %>%
    arrange(item1,item2)

  # Use npmi::count_pairs
  data_actual <- data %>%
    select(item=id,feature,weight) %>%
    count_pairs() %>%
    as_tibble()

  testthat::expect_identical(data_actual,data_expected)
})


test_that("count_pairs equals content of csv-file", {

  library(dplyr)
  library(readr)
  library(npmi)

  threads <- read_csv2(system.file("extdata", "threads.csv", package = "npmi"))
  data_expected <- read_csv2(system.file("extdata", "pairs.csv", package = "npmi"))

  # Remove spec from loaded file
  data_expected <- as_tibble(data_expected)
  attr(data_expected,"spec") <- NULL

  # Count pairs
  data_actual <- threads %>%
    select(item=id,feature,weight) %>%
    count_pairs() %>%
    as_tibble()

  testthat::expect_identical(data_actual,data_expected)
})
