#' Creates a heatmap
#'
#' @param data A tibble with columns
#'            source, target,
#'            source_label, target_label,
#'            source_group, target_group,
#'            value
#' @param caption The legend caption
#' @import ggplot2
#' @export
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


  if (!("source_label" %in% colnames(data))) {
    data$source_label <- data$source
  }


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

  y.label.data <- data %>%
    distinct(source,source_group) %>%
    arrange(source_group,source) %>%
    mutate(no = row_number()) %>%
    mutate(newgroup =  (lag(source_group) != source_group)   | (no==1))

  # label.y <- y.label.data$source
  y.groups <- filter(y.label.data,newgroup)$no - 0.5

  y.groups.breaks <- y.groups + 0.5
  y.groups.label <- filter(y.label.data,newgroup)$source_group

  x.features.data <- data %>%
    distinct(target,target_group,target_label) %>%
    arrange(target_group,target) %>%
    mutate(no = row_number()) %>%
    mutate(newgroup = (lag(target_group) != target_group)  | (no==1))

  x.label <- x.features.data$target
  x.features.label <- x.features.data$target_label
  x.groups <- filter(x.features.data,newgroup)$no - 0.5

  x.groups.breaks <- x.groups+0.5
  x.groups.label <- filter(x.features.data,newgroup)$target_group

  data <- data %>%
    mutate(source=factor(source,levels=label.y)) %>%
    mutate(target=factor(target,levels=x.label))


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
    geom_text(aes(label=value_label))+ #,size=1.8

    #annotation_custom(grob) +


    scale_fill_gradientn(
      colours = value.colors,
      #values = c(0,0.25,0.5,0.75,1),
      values = c(0,0.1,0.5,0.9,1),
      na.value = "white",
      limits=c(value.min,value.max),
      name=caption) +

    scale_x_reverse(
      breaks=c(1:length(x.label)),
      labels=x.features.label,
      position="top",
      name="" ,
      sec.axis=sec_axis(
        trans= ~.,
        breaks=x.groups.breaks,
        labels=x.groups.label,
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
        breaks=y.groups.breaks,
        labels=y.groups.label,
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
  for (y in y.groups) {
    pl <- pl +
      geom_hline(yintercept = y,size=0.5)
  }

  for (x in x.groups) {
    pl <- pl +
      geom_vline(xintercept = x,size=0.5)
  }


  pl

}
