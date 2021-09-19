#
# Calculates significance of cooccurrence and sequences
#

library(data.table)
# library(widyr)
# library(readxl)
# library(writexl)
# library(furrr)
# library(skimr)
# library(lubridate)
# library(zoo)


# "Pointwise  mutual  information  (PMI,  5)  is  a  measure  of 
# how  much  the  actual probability of a particular co-occurrence 
# of eventsp(x, y) differs from what wewould expect it to be on the 
# basis of the probabilities of the individual eventsand the assumption 
# of independencep(x)p(y)." (Bouma 2009)
# Problem: PMI und relative risk werden höher für seltenere Kombinationen, 
# weil sich das Maximum bewegt (Bouma 2009)
# Besser: NPMI mit festen Grenzen -1 bis +1

# https://pdfs.semanticscholar.org/1521/8d9c029cbb903ae7c729b2c644c24994c201.pdf
# https://stats.stackexchange.com/questions/140935/how-does-the-logpx-y-normalize-the-point-wise-mutual-information

# Berechnet gemeinsames Auftreten von Codes und Kookkurrenz
# @metrics T/F Whether to calculate p, p_cond_source, p_cond_target
# @npmi T/F Whether to calculate npmi
# @codings Tibble mit den Spalten item und feature (ohne Duplikate)
#          und weight. Ist keine Spalte weight vorhanden, wird sie mit 1 initialisiert.
get_cooccurrence <- function(codings,metrics = T, npmi = F,.progress = NULL) {
  if (!is.null(.progress)) {
    .progress$tick()$print()
  }  
  
  # Check if codings is a data.table. If not, setDT
  if (!is.data.table(codings)) {
    setDT(codings)
  }
  
  # Create weight if not present
  if (!("weight" %in% colnames(codings)))
    codings[, weight := 1]
  
  codings.lag0 <- count_pairs(codings)
  
  if (metrics) {
    
    # Share of code in all messages
    # -> propability of code in a message
    codes <- copy(codings)
    codes <- codes[, .(n.msg = sum(weight, na.rm = T)), by = item]
    codes[, p.msg := n.msg / length(unique(codings[,feature]))]

    # Share of messages with cooccurring code in all messages
    codings.lag0[, p := n /  length(unique(codings[,feature]))]
    codings.lag0 <- codings.lag0[codes[, .(code_source = item,n_source=n.msg,p_source=p.msg)],on="code_source"] 
    codings.lag0 <- codings.lag0[codes[, .(code_target = item,n_target=n.msg,p_target=p.msg)],on="code_target"]      
    
    # Conditional probability
    codings.lag0[, p_cond_source := p / p_source] 
    codings.lag0[, p_cond_target := p / p_target] 
    
    # LLR
    #codings.lag0 <- calculate_llr(codings.lag0)
    
  }
  
  if (npmi) {
    
    # Relative Risk
    codings.lag0[, irr := (p / (p_target * p_source))]
    
    # PMI
    codings.lag0[, pmi := log2(irr)]
    codings.lag0[, npmi := pmi / ( -log2(p))]
    codings.lag0[, npmi := ifelse(is.nan(npmi),-1,npmi)]
    
  }
  
  codings.lag0
}



# Shuffled die Codings für resampling, indem die Codes neu zugeteilt werden
# @codings Data.Table mit den Spalten item und feature
get_cooccurrence_shuffled <- function(codings,metrics = T,npmi = F,.progress = NULL) {
  
  # Check if codings is a data.table. If not, setDT
  if (!is.data.table(codings)) {
    setDT(codings)
  }
  
  #TODO: Beim shufflen keine Duplikate erzeugen
  codings[, item := sample(item,length(item),replace=T)]
  codings <- unique(codings, by = c("item", "feature"))
  get_cooccurrence(codings,metrics,npmi,.progress)
}




# Berechnet Sequenzen von Codes und PMI auf Basis der Mitteilungen
# @codings Data.Table mit den Spalten
# - item (z.B. eine Kategorie), 
# - feature (z.B. ID eines Kommentars) 
# - feature_prev (z.B. ID des vorangegangenen Kommentars)
get_sequence <- function(codings,metrics = T, npmi = F,.progress = NULL) {
  if (!is.null(.progress)) {
    .progress$tick()$print()
  }  
  
  # Check if codings is a data.table. If not, setDT
  if (!is.data.table(codings)) {
    setDT(codings)
  }
  
  codings <- unique(codings, by = c("item", "feature", "feature_prev"))
  
  # Hierachy -> Adjacency
  codings.lag1 <- count_sequences(codings)
  
  if (metrics) {
    codes <- copy(codings)
    codes <- codes[, .(n.msg = .N), by = "item"]
    codes[, p.msg := n.msg / n_distinct(codings[, feature])]
    
    # Anzahl maximal möglicher  Sequenzen
    #nmax <-  (n_distinct(codings$feature) - n_distinct(codings$diskussion_no))
    nmax <- codings[!is.na(feature_prev)]
    nmax <- nrow(unique(nmax, by = "feature"))
    
    #Anteil an allen empirisch möglichen Sequenzen
    codings.lag1[, p := n / nmax]
    codings.lag1 <- codings.lag1[codes[, .(code_next = item,n_next=n.msg,p_next=p.msg)],on="code_next"] 
    codings.lag1 <- codings.lag1[codes[, .(code_prev = item,n_prev=n.msg,p_prev=p.msg)],on="code_prev"]      
    
    # Bedingte Wahrscheinlichkeit
    codings.lag1[, p_cond_next := p / p_next] 
    codings.lag1[, p_cond_prev := p / p_prev] 
  }
  
  # NPMI
  if (npmi) {
    
    codings.lag1[, irr := (p / (p_next * p_prev))]
    codings.lag1[, pmi := log2(irr)]
    codings.lag1[, npmi := pmi / ( -log2(p))]
    codings.lag1[, npmi := ifelse(is.nan(npmi),-1,npmi)]
    
  }
  
  codings.lag1
}


# Shuffled die Codings für Resampling, indem die Codes neu zugeteilt werden
# @codings Data.Table mit den Spalten item, feature und feature_prev
get_sequence_shuffled <- function(codings,metrics = T,npmi = F,.progress = NULL) {
  
  # Check if codings is a data.table. If not, setDT
  if (!is.data.table(codings)) {
    setDT(codings)
  }
  
  codings[, item := sample(item,length(item),replace=T)]
  get_sequence(codings,metrics,npmi,.progress)
}

# Function to count pairs
count_pairs <- function(codings) {
  codings.lag <- copy(codings)
  
  # To wide
  codings.lag <- dcast(codings.lag[, .(feature, item, weight)],
                        feature ~ item, value.var = "weight")
  
  # Remove feature for matrix multiplication (pairwise count)
  codings.lag[, feature := NULL]
  
  # Convert to matrix
  codings.lag <- as.matrix(codings.lag)
  
  # Set NA to 0
  codings.lag[is.na(codings.lag)] <- 0
  
  # Matrix multiplication / Pairwise count
  codings.lag <- t(codings.lag) %*% codings.lag
  
  # Set diag to 0 (should lead to the same result as tidyr "complete" in old code)
  diag(codings.lag) <- 0
  
  # Matrix to data.table
  codings.lag <- as.data.table(codings.lag)
  
  # Create code_source column
  codings.lag[, code_source := .I]
  
  # To long
  codings.lag <- melt(codings.lag,
                       measure.vars = patterns("[1-9]"),
                       variable.name = "code_target", 
                       value.name = "n")
  
  # code_target is factor, set to numeric
  codings.lag[, code_target := as.numeric(code_target)]
  
  return(codings.lag)
}

# Function to count sequences
count_sequences <- function(codings) {
  codings.lag <- copy(codings)
  codings.lag <- codings.lag[, code_next := item]
  codings.lag <- codings.lag[codings[, .(feature_prev = feature, code_prev = item)], on = "feature_prev", nomatch = 0]
  codings.lag <- unique(codings.lag, by = c("feature","code_next","code_prev")) 
  
  codings.lag <- codings.lag[, .(n=.N), by=c("code_next", "code_prev")]
  codings.lag <- codings.lag[CJ(code_next = code_next, 
                                  code_prev = code_prev, 
                                  unique=TRUE), 
                               on=.(code_next, code_prev)]
  setnafill(codings.lag, fill = 0, cols = 'n')
  return(codings.lag)
}

