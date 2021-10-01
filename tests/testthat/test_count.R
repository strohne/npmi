test_that("count_pairs equals pairwise_count", {

  library(dplyr)
  library(tidyr)
  library(readr)
  library(widyr)
  library(npmi)

  data <- read_csv2(system.file("extdata", "threads.csv", package = "npmi"))

  # Use widyr::pairwise_count
  data_expected <- data %>%
    widyr::pairwise_count(feature,id,wt = weight, upper=T, diag=T) %>%
    dplyr::rename(feature_source=item1,feature_target=item2) %>%
    complete(feature_source,feature_target,fill=list(n=0)) %>%
    arrange(feature_source,feature_target)

  # Use npmi::count_pairs
  data_actual <- data %>%
    select(item=id,feature,weight) %>%
    count_pairs() %>%
    dplyr::rename(feature_source=source,feature_target=target) %>%
    arrange(feature_source,feature_target)

  testthat::expect_identical(data_actual,data_expected)
})


test_that("count_pairs equals content of csv-file", {

  library(dplyr)
  library(readr)
  library(npmi)

  threads <- read_csv2(system.file("extdata", "threads.csv", package = "npmi"))
  data_expected <- read_csv2(system.file("extdata", "pairs.csv", package = "npmi"))

  # Remove spec from loaded file
  data_expected <- data.frame(data_expected)

  # Count pairs
  data_actual <- threads %>%
    select(item=id,feature,weight) %>%
    count_pairs() %>%
    data.frame() %>%
    rename(feature1=source,feature2=target)

  testthat::expect_identical(data_actual,data_expected)
})


test_that("count_sequences equals the tidyverse equivalent", {

  library(dplyr)
  library(readr)
  library(npmi)

  threads <- read_csv2(system.file("extdata", "threads.csv", package = "npmi"))

  # Get sequences of items
  items <- threads %>%
    distinct(id,parent_id) %>%
    na.omit()

  items <- items %>%
    group_by(parent_id) %>%
    arrange(id) %>%
    mutate(id_prev = ifelse(row_number() == 1,parent_id, lag(id))) %>%
    ungroup()

  threads <- threads %>%
    left_join(select(items,id,id_prev),by="id")

  rm(items)

  # Distinct
  threads <- threads %>%
    distinct(item=id,feature,item_prev = id_prev) %>%
    na.omit()


  # Expected feature sequences
  features_expected <- threads %>%
    rename(item_next=item,feature_target=feature) %>%

    inner_join(
      select(threads,item_prev=item,feature_source=feature),
      by=c("item_prev")
    ) %>%
    distinct(item_next,feature_target,item_prev,feature_source) %>%
    count(feature_source,feature_target) %>%
    tidyr::complete(feature_source,feature_target,fill=list(n=0)) %>%
    arrange(feature_source,feature_target)  %>%
    data.frame()


  # Actual feature sequences
  features_actual <- threads %>%
    count_sequences() %>%
    dplyr::rename(feature_source=source,feature_target=target) %>%
    arrange(feature_source,feature_target) %>%
    data.frame()

  # Compare
  testthat::expect_identical(features_actual,features_expected)
})

test_that("count_sequences equals a csv file", {

  library(dplyr)
  library(readr)
  library(npmi)

  threads <- read_csv2(system.file("extdata", "threads.csv", package = "npmi"))

  # Get sequences of items
  items <- threads %>%
    distinct(id,parent_id) %>%
    na.omit()

  items <- items %>%
    group_by(parent_id) %>%
    arrange(id) %>%
    mutate(id_prev = ifelse(row_number() == 1,parent_id, lag(id))) %>%
    ungroup()

  threads <- threads %>%
    left_join(select(items,id,id_prev),by="id")

  rm(items)

  # Distinct
  threads <- threads %>%
    distinct(item=id,feature,item_prev = id_prev)


  # Expected feature sequences
  data_expected <- read_csv2(system.file("extdata", "sequences.csv", package = "npmi"))

  # Remove spec from loaded file
  data_expected <- data.frame(data_expected)

  # Actual feature sequences
  data_actual <- threads %>%
    count_sequences() %>%
    dplyr::rename(feature_source=source,feature_target=target) %>%
    arrange(feature_source,feature_target) %>%
    data.frame()


  # Compare
  testthat::expect_identical(data_actual,data_expected)
})


