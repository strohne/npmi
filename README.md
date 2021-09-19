# npmi
Explore correlations and sequences in qualitative and quantitative content analysis


# Installation
```
library(devtools)
install_github("strohne/npmi/npmi")
```

# Usage
```
# Load example data
data <- read_csv2(system.file("extdata", "threads.csv", package = "npmi"))

# Count pairs
data_actual <- data %>%
  select(item=id,feature,weight) %>%
  count_pairs()
```    

