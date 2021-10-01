# Resampled normalized pointwise mutual information (rnpmi)
Explore correlations and sequences in qualitative and quantitative content analysis.

*The package is in pre alpha state. Use with care. Numbers might be hurt.*

# Installation
```
library(devtools)
install_github("strohne/npmi", build_vignettes = TRUE)
```

# Usage
```
# Packages
library(tidyverse)
library(npmi)

# Load example data
data <- read_csv2(system.file("extdata", "threads.csv", package = "npmi"))

# Cooccurence
pairs_coo <- data %>%
  select(item=id,feature,weight) %>%
  count_pairs()

# Resample npmi for cooccurrence
pairs_coo <- data %>%
  select(item=id,feature,weight) %>%
  get_cooccurrence()

# Sequences
pairs_seq <- data %>%
  rename(item=id,item_parent=parent_id) %>% 
  add_previous_item() %>% 
  count_sequences()

# Resample npmi for sequences
pairs_seq <- %>%
  rename(item=id,item_parent=parent_id) %>% 
  add_previous_item() %>% 
  get_sequences()

# Plot absolute numbers
pairs_seq %>% 
  rename(value=n) %>% 
  matrixmap() 

# Plot probabilities
pairs_seq %>% 
  rename(value=p) %>% 
  matrixmap() 

# Plot rnpmi
pairs_seq %>% 
  rename(value=npmi) %>% 
  matrixmap()   
  
# Plot network
pairs_seq %>% 
  rename(value=npmi) %>% 
  network() 
```    

See the vignettes for further examples (either browse the vignettes folder or open vignettes from the package help). Vignettes are not polished yet.

Resampling can be parallelized, just call:
```
library(future)
plan(multisession)
```
