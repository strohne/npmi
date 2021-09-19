#
# Calculates significance of cooccurrence and sequences
#

library(tidyverse)
library(widyr)
library(readxl)
library(writexl)
library(furrr)
library(skimr)
library(lubridate)
library(zoo)



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
  
  # Create weight if not present
  if (!("weight" %in% colnames(codings)))
    codings$weight = 1

  codings.lag0 <- codings %>%
    pairwise_count(item,feature,weight,upper=T, diag=F) %>%
    rename(code_source=item1,code_target=item2) %>%
    complete(code_source,code_target,fill=list(n=0))
  
  if (metrics) {
    
    # Share of code in all messages
    # -> propability of code in a message
    codes <- codings %>% 
      group_by(item) %>% 
      summarize(n.msg=sum(weight), .groups = 'keep') %>% 
      ungroup() %>% 
      
      mutate(p.msg = n.msg / n_distinct(codings$feature))
    
    codings.lag0 <- codings.lag0 %>%
      
      # Share of messages with cooccurring code in all messages
      mutate(p = n / n_distinct(codings$feature)) %>% 
      left_join(select(codes,item,n_source=n.msg,p_source=p.msg),by=c("code_source"="item")) %>% 
      left_join(select(codes,item,n_target=n.msg,p_target=p.msg),by=c("code_target"="item")) %>%

      # Conditional probability
      mutate(p_cond_source = p / p_source) %>% 
      mutate(p_cond_target = p / p_target)
    
    # LLR
    #codings.lag0 <- calculate_llr(codings.lag0)
    
  }
  
  if (npmi) {
  
    codings.lag0 <- codings.lag0 %>% 
      
      # Relative risk
      mutate(irr = (p / (p_source * p_target)))  %>% 
      
      # PMI
      mutate(pmi = log2(irr)) %>% 
      mutate(npmi = pmi / ( -log2(p))) %>% 
      mutate_at("npmi",~ifelse(is.nan(.),-1,.))
  }
  
  codings.lag0
}



# Shuffled die Codings für resampling, indem die Codes neu zugeteilt werden
# @codings Tibble mit den Spalten item und feature
get_cooccurrence_shuffled <- function(codings,metrics = T,npmi = F,.progress = NULL) {
  
  #TODO: Beim shufflen keine Duplikate erzeugen
  codings$item <- sample(codings$item,length(codings$item),replace=T)
  codings <- codings %>%
    distinct(item,feature) 
  
  get_cooccurrence(codings,metrics,npmi,.progress)
}


# Berechnet Sequenzen von Codes und PMI auf Basis der Mitteilungen
# @codings Tibble mit den Spalten
# - item (z.B. eine Kategorie), 
# - feature (z.B. ID eines Kommentars) 
# - feature_prev (z.B. ID des vorangegangenen Kommentars)
get_sequence <- function(codings,metrics = T, npmi = F,.progress = NULL) {
  if (!is.null(.progress)) {
    .progress$tick()$print()
  }  

  codings <- codings %>%
    distinct(item,feature,feature_prev) 
  
  # Hierachy -> Adjacency
  codings.lag1 <- codings %>%
    mutate(code_next=item) %>% 
    
    inner_join(
      select(codings,feature_prev=feature,code_prev=item),
      by=c("feature_prev")
    ) %>% 
    
    
    distinct(feature,code_next,code_prev) %>% 
    count(code_next,code_prev) %>% 
    complete(code_next,code_prev,fill=list(n=0))

  if (metrics) {
    codes <- codings %>% 
      count(item) %>% rename(n.msg = n) %>% 
      mutate(p.msg = n.msg / n_distinct(codings$feature))
    
    # Anzahl maximal möglicher  Sequenzen
    #nmax <-  (n_distinct(codings$feature) - n_distinct(codings$diskussion_no))
    nmax <- codings %>%
      filter(!is.na(feature_prev)) %>% 
      distinct(feature) %>% 
      nrow()
        
    codings.lag1 <- codings.lag1 %>% 
      
      #Anteil an allen empirisch möglichen Sequenzen
      mutate(p = n / nmax) %>%  
    
      left_join(select(codes,item,n_next=n.msg,p_next=p.msg),by=c("code_next"="item")) %>% 
      left_join(select(codes,item,n_prev=n.msg,p_prev=p.msg),by=c("code_prev"="item")) %>%
      

      # Bedingte Wahrscheinlichkeit
      mutate(p_cond_next = p / p_next) %>% 
      mutate(p_cond_prev = p / p_prev)

  }
  
  # NPMI
  if (npmi) {
    
    codings.lag1 <- codings.lag1 %>% 
      mutate(irr = (p / (p_next * p_prev))) %>%
      mutate(pmi = log2(irr)) %>% 
      mutate(npmi = pmi / ( -log2(p))) %>% 
      mutate_at("npmi",~ifelse(is.nan(.),-1,.))
      
  }
  
  codings.lag1
}


# Shuffled die Codings für Resampling, indem die Codes neu zugeteilt werden
# @codings Tibble mit den Spalten item, feature und feature_prev
get_sequence_shuffled <- function(codings,metrics = T,npmi = F,.progress = NULL) {
  codings$item <- sample(codings$item,length(codings$item),replace=T)
  get_sequence(codings,metrics,npmi,.progress)
}


#
# Calculates significant cooccurrence (p=0.05)
#

# @codings Tibble mit den Spalten item, feature and weight.
#          The default weight is 1, if the column is not present.
# @smoothing Set smoothing to 1 to apply Laplace's rule of succession
# @return  resampleped (random permutation test) cooccurrence metrics
#          -> boot.pmi and boot.npmi only meaningful for p values (?)
resampleCooccurrence <- function(codings, trials=10000, npmi=F, smoothing=0) {
  
  
  # Get shuffled data
  lag0.resample <- tibble(no = c(1:trials)) %>% 
    mutate(co = future_map(no,~get_cooccurrence_shuffled(codings,metrics=T,npmi=npmi),
                           .progress = T,
                           .options = furrr_options(seed = TRUE))) %>% 
    unnest(co)
  
  # Calculate confidence interval
  lag0.conf <- lag0.resample %>% 
    gather(metric,value,-no,-code_source,-code_target) %>% 
    group_by(code_source,code_target,metric)  %>% 
    dplyr::summarize(value.lo = quantile(value, 0.025, type=1),
                     value.med = quantile(value, 0.5, type=1),
                     value.mean = mean(value),
                     value.hi = quantile(value, 0.975, type=1),
                     .groups = 'keep') %>% 
    ungroup()
  
  rm(lag0.resample)
  
   # Check if all cases were resampleped
   m_0 <- filter(lag0.conf,value.mean == 0) %>% count()
   m_1 <- filter(lag0.conf,value.mean == 1) %>% count()
   
   if (m_0 > 0)
     warning(paste0(m_0,"pairs never occured in resampling, probability not reliable. Increase number of trials!"))

   if (m_1 > 0)
     warning(paste0(m_1,"pairs always occured in resampling, probability not reliable. Increase number of trials!"))
  

  # Compare to data
  lag0.codings <- get_cooccurrence(codings,metrics = T, npmi = F) %>% 
    gather(metric,value,-code_source,-code_target) %>% 
    left_join(lag0.conf) 
  
  # Smoothing / pseudocount 
  # -> set smoothing to 1  to apply Laplace's rule of succession
  if (smoothing > 0) {
    smoothing <- smoothing / (smoothing * trials)
  }
  
  # Ratio of resampleped values
  # -> boot.pmi and boot.npmi only meaningful for p values (including p_cond_source & p_cond_target)
  # -> if value.mean == 0 -> nicht genug geresampleped, sollte nicht auftreten
  lag0.codings <- lag0.codings %>% 
    mutate(boot.ratio = (value + smoothing) / (value.mean + smoothing)) %>% 
    mutate(boot.pmi =   ifelse(boot.ratio != 0, log2(boot.ratio),-Inf)) %>% 
    mutate(boot.npmi =  ifelse(boot.pmi != -Inf, boot.pmi / -log2(value.mean),-1)) %>% 

    # Significance compared to CI
    mutate(sig.hi = (value > value.hi) ) %>%
    mutate(sig.lo = (value < value.lo) ) %>% 
    mutate(sig = sig.hi | sig.lo)
    

  # # Keep significant only
  # lag0.codings.sig.n <- get_cooccurrence(codings) %>%
  #   semi_join(filter(lag0.codings,metric=="n",sig==T),by=c("code_source","code_target"))
  
  return (lag0.codings)
}

  
#
# Calculates significant sequences (p=0.05)
#

# Berechnet Sequenzen von Codes und PMI auf Basis der Mitteilungen
# @codings Tibble mit den Spalten
# - item (z.B. eine Kategorie), 
# - feature (z.B. ID eines Kommentars) 
# - feature_prev (z.B. ID des vorangegangenen Kommentars)

resampleSequences <- function(codings, trials=10000, npmi=F, smoothing=0) {

  # Get shuffled data
  furrr_options(seed=T)
  lag1.resample <- tibble(no = c(1:trials)) %>% 
    mutate(npmi = future_map(no,~get_sequence_shuffled(codings,metrics=T, npmi=npmi),.progress = T)) %>% 
    unnest(npmi)


  # Calculate confidence interval
  lag1.conf <- lag1.resample %>% 
    gather(metric,value,-no,-code_next,-code_prev) %>% 
    group_by(code_next,code_prev,metric)  %>% 
    dplyr::summarize(value.lo = quantile(value, 0.025, type=1),
                     value.med = quantile(value, 0.5, type=1),
                     value.mean = mean(value),
                     value.hi = quantile(value, 0.975, type=1),
                     .groups = 'keep') %>% 
    ungroup()
  
  
  rm(lag1.resample)
  
  
  # Compare to data
  lag1.codings <- get_sequence(codings) %>% 
    gather(metric,value,-code_next,-code_prev) %>% 
    left_join(lag1.conf) 
  
  # Smoothing / pseudocount 
  # -> set smoothing to 1  to apply Laplace's rule of succession
  if (smoothing > 0) {
    smoothing <- smoothing / (smoothing * trials)
  }
  
  # Ratio of resampled values
  # -> boot.pmi and boot.npmi only meaningful for p values (including p_cond_source & p_cond_target)
  # -> if value.mean == 0 -> nicht genug geresampleped, sollte nicht auftreten
  lag1.codings <- lag1.codings %>% 
    mutate(boot.ratio = (value + smoothing) / (value.mean + smoothing)) %>% 
    mutate(boot.pmi =   ifelse(boot.ratio != 0, log2(boot.ratio),-Inf)) %>% 
    mutate(boot.npmi =  ifelse(boot.pmi != -Inf, boot.pmi / -log2(value.mean),-1)) %>% 
    
    # Significance compared to CI
    mutate(sig.hi = (value > value.hi) ) %>%
    mutate(sig.lo = (value < value.lo) ) %>% 
    mutate(sig = sig.hi | sig.lo)

  
  # # Keep significant only
  # lag1.codings.sig.n <- get_sequence(codings) %>%
  #   semi_join(filter(lag1.codings,metric=="n",sig==T),by=c("code_next","code_prev"))
  
  return (lag1.codings)
}




#
# Creates a heatmap 
#
# @data A tibble with columns source, target, target_label,
#       source_group, target_group, value
# @caption The legend caption
#

matrixmap <- function(data, caption = "", value.min=NULL,value.max=NULL, value.mid=NULL) {
  #grob <- grobTree(textGrob("Source", x=0.9,  y=1, hjust=0,gp=gpar(fontsize=8)))

  #
  # Scale min-max
  #
  if (is.null(value.min))
    value.min <- min(data$value,na.rm=T)
  if (is.null(value.max))
    value.max <- max(data$value,na.rm=T)
  if (is.null(value.mid))
    value.mid <- (value.max-value.min) / 2
  
  # Für 0,0.25,0.5,0.75 und 1.0
  if (value.min < 0)
    value.colors <- c("#cc1237","#cc1237","#fbfbd9","#28cc12","#28cc12")
  else
    value.colors <- c("#fbfbd9","#e38a0c","#e38a0c","#e38a0c","#ae344c")
  
  #
  # Defaults
  #

  
  if (!("target_label" %in% colnames(data))) {
    data$target_label <- data$target
  }
  
  if (!("source_group" %in% colnames(data))) {
    data$source_group = ""
  }
  
  
  if (!("target_group" %in% colnames(data))) {
    data$target_group = ""
  }

  #
  # Gruppen und Labels  
  #
  
  label.y.data <- data %>% 
    distinct(source,source_group) %>% 
    arrange(source_group,source) %>% 
    mutate(no = row_number()) %>% 
    mutate(newgroup =  (lag(source_group) != source_group)   | (no==1))
  
  label.y <- label.y.data$source
  groups.y <- filter(label.y.data,newgroup)$no - 0.5
  
  groups.breaks.y <- groups.y + 0.5
  groups.label.y <- filter(label.y.data,newgroup)$source_group
  
  items.x.data <- data %>% 
    distinct(target,target_group,target_label) %>% 
    arrange(target_group,target) %>% 
    mutate(no = row_number()) %>% 
    mutate(newgroup = (lag(target_group) != target_group)  | (no==1))
  
  label.x <- items.x.data$target
  items.x.label <- items.x.data$target_label
  groups.x <- filter(items.x.data,newgroup)$no - 0.5
  
  groups.breaks.x <- groups.x+0.5
  groups.label.x <- filter(items.x.data,newgroup)$target_group
  
  data <- data %>% 
    mutate(source=factor(source,levels=label.y)) %>%
    mutate(target=factor(target,levels=label.x)) 
  
  
  #
  # Plot 
  #
  
  pl <- data %>%
    
    # Create labels
    mutate(value_label=str_remove(format(round(value,2)),"NA")) %>%
    mutate(value_label=str_replace(value_label,"0\\.","\\.")) %>%
    mutate(value_label=str_replace(value_label,"-1\\.00","-1.0")) %>% 
    
    # Fill
    complete(source,target,fill=list(value=NA_real_,value_label="")) %>% 
    
    mutate(source=as.numeric(source)) %>%
    mutate(target=as.numeric(target)) %>% 
    
    ggplot(aes(y=source,x=target,fill=value,group=source)) +
    geom_tile(color="lightgray")+
    geom_text(aes(label=value_label),size=1.8)+
    
    #annotation_custom(grob) +
    
    
    scale_fill_gradientn(
      colours = value.colors,
      #values = c(0,0.25,0.5,0.75,1),
      values = c(0,0.1,0.5,0.9,1),
      na.value = "white",
      limits=c(value.min,value.max),
      name=caption) +
    
    scale_x_reverse(
      breaks=c(1:length(label.x)),
      labels=items.x.label,
      position="top",
      name="" ,
      sec.axis=sec_axis(
        trans= ~.,
        breaks=groups.breaks.x,
        labels=groups.label.x,
        name="Target"
      )
    ) +
    scale_y_reverse(
      #limits = c(1,27),
      position="right",
      breaks=c(1:length(label.y)),
      labels=label.y,
      name="",
      sec.axis=sec_axis(
        trans= ~.,
        breaks=groups.breaks.y,
        labels=groups.label.y,
        name="Source"
      )
    ) +
    
    
    coord_fixed(expand=F) +
    
    theme_bw(base_size = 8)+
    theme(
      #plot.margin = unit(c(.5,6,.5,.5),"lines"),
      strip.background =element_rect(fill="white"),
      strip.text = element_text(face="bold"),
      axis.ticks.x=element_blank(),
      axis.ticks.y=element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.text.x.top = element_text(angle = 90, vjust = 0.5, hjust=0) ,
      
      axis.text.x.bottom = element_text(hjust=1,size=8),
      axis.text.y.left = element_text(hjust=1,angle=90,size=8),
      
      legend.justification = "left",
      legend.position="bottom",
      legend.text = element_text(size=8),
      legend.title= element_text(size=8)
    ) 
  
  # Add lines between groups
  for (y in groups.y) {
    pl <- pl +
      geom_hline(yintercept = y,size=0.5)
  }
  
  for (x in groups.x) {
    pl <- pl +
      geom_vline(xintercept = x,size=0.5)
  }  
  
  
  pl
  
}


# Log-Likelihood-Ratio (Dunning, 1993) ----
#

# Siehe auch http://tdunning.blogspot.com/2008/03/surprise-and-coincidence.html


# Berechnet gemeinsames Auftreten von Codes und PMI (Kookkurrenz)
# @codings Tibble mit den Spalten item und feature

# Wesentliche Grundlage der Berechnung ist für immer zwei Wörter:
# - wie häufig sie zusamen auftreten (n)
# - wie häufig sie jeweils ohne das andere Wort auftreten (n1not2, n2not1)
# - wie häufig andere Wörter gemeinsam auftreten

get_llr <- function(codings) {

  codings <- codings %>%
    distinct(item,feature) 
  
  # Anzahl der Kombination = n
  codings.lag0 <- codings %>% 
    pairwise_count(item,feature, diag=F,upper=T) %>% 
    rename(code_source=item1,code_target=item2)
  
  llr <- calculate_llr(codings.lag0)
  return (llr)
  
}

# Als Vorbereitung zur Berechnung des Log-Likelihood-Ratio wird
# eine Funktion zur Berechnung der Shannon-Entropy H definiert
# k = Vektor mit Ereignisanzahlen
calculate_entropy <- function(k) {
  N = sum(k)
  return (sum(k/N * log(k/N + (k==0))))
}


# Berechnet LLR aus Kookkurrenz
# @codings.lag0 Tibble mit den Spalten code_source, code_target und n
#               n = Anzahl gemeinsamer Vorkommen von code_source und code_target

calculate_llr <- function(codings.lag0) {

  llr <- codings.lag0 %>% 
    
    # Anzahl für Wort 1 = n1
    group_by(code_source) %>% 
    mutate(n1=sum(n)) %>% 
    ungroup() %>% 
    
    # Anzahl  für Wort 2 = n2
    group_by(code_target) %>% 
    mutate(n2=sum(n)) %>% 
    ungroup() %>% 
    
    # Anzahl Wort 1 ohne Wort 2 = n1not2
    mutate(n1not2 = n1 - n) %>%
    
    # Anzahl Wort 2 ohne Wort 1 = n2not1
    mutate(n2not1 = n2 - n) %>%
    
    # Kombinationen ohne Wort 1 und ohne Wort 2 = not12
    mutate(not12 = sum(n) - n1 - n2not1) %>% 
    
    # Log-Likelihood-Ratio (G^2)
    mutate(llr= 2 * n * (
        calculate_entropy(c(n, n2not1, n1not2, not12)) -
        calculate_entropy(c(n2, n1not2)) - 
        calculate_entropy(c(n1, n2not1))
      )
    ) 
  
  return (llr)
}


#
# Verteilung NPMI besser verstehen ----
#
# 
# 
# # Künstliche Threads erzeugen
# # @codings Tibble mit den Spalten item, feature und feature_prev
# codings.random <- tibble(feature = c(1:100000)) %>% 
#   mutate(feature_prev=lag(feature)) %>%
#   
#   # In Threads zerschneiden (20% ohne Eltern)
#   mutate(feature_prev = ifelse(runif(nrow(.)) < 0.2,NA,feature_prev )) %>%  
#   
#   # Vervielfältigung der Mitteilungen, so dass 
#   # eine Mitteilung mehrere Codes haben kann
#   bind_rows(sample_frac(.,2.0,replace=T)) %>%  
#   
#   # Codes zufällig verteilen
#   mutate(item = sample(10,nrow(.),replace=T))  %>% 
#   
#   distinct(feature,feature_prev,item)
# 
# # ERgebnis: 300,000 Mitteilungen in 60,000 Threads minus Diplikate
# # -> Verschiebung des NPMI Richtung -0.19 durch Mehrfachkodierung
# 
# get_sequence(codings.random) %>% 
#   gather(metric,value,-code_next,-code_prev) %>% 
#   filter(metric=="npmi") %>% 
#   ggplot(aes(value)) +
#   geom_density() +
#   facet_wrap(~metric,scales="free")
# 
# 
# get_cooccurrence(codings.random) %>% 
#   gather(metric,value,-code_source,-code_target) %>% 
# #  filter(metric=="n") %>% 
#   ggplot(aes(value)) +
#   geom_histogram() +
#   facet_wrap(~metric,scales="free")
# 
# 
# # lag1.codings <- get_sequence(filter(codings,bereich=="rex"))
# # lag1.codings <- get_sequence(filter(codings,bereich=="sj"))
# lag1.codings <- get_sequence_shuffled(filter(codings,bereich=="rex"))





# 
# pairwise_count <- function (tbl, item, feature, ...)
# {
#   
#   # Recursivley process grouped data_frame
#   if (inherits(tbl, "grouped_df")) {
#     
#     # Empty
#     if (nrow(tbl) == 0) {
#       ret <- select(tbl,dplyr::group_vars(tbl)) %>% 
#         mutate(item1=character(0),item2=character(0),value=numeric(0))
#     } 
#     
#     else {
#       ret <- tbl %>% tidyr::nest(..data = c(!! enquo(item), !! enquo(feature))) %>% 
#         mutate(..data = purrr::map(..data,pairwise_count,!! enquo(item),!! enquo(feature))) %>% 
#         tidyr::unnest(..data) 
#     }
#     
#     ret <- ret  %>%
#       group_by(.dots = dplyr::groups(tbl))
#     
#     return(ret)
#   }
#   
#   # Empty
#   if (nrow(tbl) == 0) {
#     ret <- tibble(item1=character(0),item2=character(0),value=numeric(0))
#     return(ret)
#   } 
#   
#   
#   # Quote input
#   item <- enquo(item) #enquo(item)
#   feature <- enquo(feature) #enquo(feature)
#   
#   # Distinct
#   tbl <- tbl %>% 
#     distinct(!!item, !!feature, .keep_all = TRUE)
#   
#   # Create sparse matrix
#   row_col <- quo_name(item)
#   column_col <- quo_name(feature)
#   
#   row_names <- tbl[[row_col]]
#   col_names <- tbl[[column_col]]
#   values <- 1
#   
#   row_u <- unique(row_names)
#   i <- match(row_names, row_u)
#   
#   col_u <- unique(col_names)
#   j <- match(col_names, col_u)
#   
#   mat <- Matrix::sparseMatrix(i = i, j = j, x = values, dimnames = list(row_u,col_u), ...)
#   
#   # Matrix multiplication
#   t <- selectMethod("t", class(mat))
#   mat <-  mat %*% t(mat)
#   
#   # Reshape to data_frame
#   ret <- reshape2::melt(as.matrix(mat),varnames = c("item1", "item2"), as.is = TRUE) %>% 
#     tbl_df() %>%
#     rename(n = value) %>% 
#     filter(item1 != item2)
#   
#   ret
#   
# }



# Funktion, um Colinking auszuzählen
# ...inner join ist schneller als pairwise count
#    außerdem können die verbindenden Elemente beibehalten werden
count_via <- function(data) {
  
  inner_join(
    select(data,source,via=target),
    select(data,target=source,via=target),
    by="via"
  ) %>% 
    filter(source != target) %>% 
    group_by(source,target) %>% 
    summarize(n = n(),via = paste0(unique(via),collapse = ";") ) %>% 
    ungroup()
  
  
}


#
# Timing - requests per minute ----
#

fp_timing <- function(data,f.objecttype="offcut",f.querystatus="fetched (200)") {
  
  
  f.objecttype="offcut"
  f.querystatus="fetched (200)"
  
  timing <- data %>% 
    filter(query_status == f.querystatus) %>% 
    filter(object_type == f.objecttype) %>% 
    mutate(query_time = ymd_hms(query_time)) %>% 
    mutate(query_minute = floor_date(query_time,unit="minute")) %>% 
    mutate(query_hour = floor_date(query_time,unit="hour")) %>% 
    mutate(query_day = date(query_time))
  
  cat("Beginn: ",format(min(timing$query_time)),"\n")
  cat("Ende: ",format(max(timing$query_time)),"\n")
  cat("Dauer: ",as.character(as.period(interval(min(timing$query_time),max(timing$query_time)))),"\n")
  cat("Aktive Erhebungsstunden: ",n_distinct(timing$query_hour), " \n")
  cat("Aktive Erhebungsminuten: ",n_distinct(timing$query_minute), " \n")
  
  cat("Requests per day (",f.objecttype,"): \n")
  timing %>% 
    count(query_day) %>% 
    print(n=100)
  
  timeline <- seq(min(timing$query_minute), max(timing$query_minute), by = "min")
  timeline <- timing %>% 
    count(timeslot=query_minute) %>% 
    tidyr::complete(timeslot=timeline,fill=list(n=0))
  
  
  
  
  # Requests per minute  
  pl <- timeline %>% 
    
    ggplot(aes(x=timeslot,y=n)) +
    geom_line(color="grey") +
    xlab("Time")  +
    ylab(paste0("Requests per minute (",f.objecttype,")") )
  
  # Rolling average if more than 60 minutes
  if (nrow(timeline) > 60) {
    
    timeline <- timeline %>% 
      mutate(perhour = rollmean(n, 60, na.pad=TRUE,align = "right")) %>% 
      na.omit()
    
    pl <- pl +
      geom_line(aes(x=timeslot,y=perhour),data=timeline,color="red")
  }  
  print(pl)
  
  return(pl)
}


#
# Network ----
#


# 
# Get nodes from edge list and prepare labels
#
# @edges Tibble with columns source and target

get_nodes <- function(edges) {
  
  nodes <- edges %>% 
    pivot_longer(c(source,target),names_to="type",values_to="id") %>% 
    distinct(id)
  
  nodes <- nodes %>% 
    mutate(label = id) %>% 
    mutate(label = str_replace(label," ([0-9]+)","\n\\1"))
    # mutate(label= str_replace_all(label,"[\n ]+"," ")) %>% 
    # mutate(label= str_replace(label,"[ \r\n]+","\n")) %>%
    # mutate(label= str_replace(label,"([a-z])([A-Z])","\\1\n\\2")) %>% 
    # mutate(label= str_trunc(str_replace(label,"^.{20}","\\0\n"),21,ellipsis = ""))
  
  nodes
}

#
# Keep only distinct edges of given nodes
#
# @edges Tibble with columns source and target
# @nodes Tibble with column id

get_edges <- function(edges,nodes) {
  edges %>% 
    semi_join(nodes,by=c("source"="id")) %>% 
    semi_join(nodes,by=c("target"="id")) %>% 
    distinct(source,target,.keep_all = T)
  
}