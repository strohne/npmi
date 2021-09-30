# npmi
Explore correlations and sequences in qualitative and quantitative content analysis

*The package is in pre alpha state. Use with care. Numbers might be hurt.*

# Installation
```
library(devtools)
install_github("strohne/npmi/npmi", build_vignettes = TRUE)
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

See the vignettes for further examples (either browse the vignettes folder or open vignettes from the package help). Vignettes are not polished yet.
