
library(tidyverse)
library(widyr)
library(data.table)

data <- read_csv2("inst/extdata/threads.csv")

data_co_widyr <- data %>%
  pairwise_count(feature,id,upper=F, diag=F) %>%
  arrange(item1,item2)

data_co_npmi <- data %>%
  select(item=id,feature,weight) %>%
  count_pairs() %>%
  as_tibble()

data <- read_csv2("inst/extdata/threads.csv")
