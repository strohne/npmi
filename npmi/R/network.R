#' Plots a network
#'
#' @param data A tibble with columns
#'            source, target,
#'            source_label, target_label,
#'            source_group, target_group,
#'            value, sig
#' @import ggplot2
#' @import ggraph
#' @import tidygraph
#' @export
network <- function(data) {
  edges <- data %>%
    mutate(weight=ifelse(value > 0,value,-value)) %>%
    mutate(type=ifelse(value > 0,"foster","hinder")) %>%
    slice_max(weight,n=60)

  nodes <- bind_rows(
    distinct(pairs_coo$pairs,id=source,label=source_label,no=source_no,group=source_group),
    distinct(pairs_coo$pairs,id=target,label=target_label,no=target_no,group=target_group)
  ) %>%
    distinct(id,.keep_all = T)

  gr <- tbl_graph(nodes=nodes,edges=edges,directed=T)

  gr <- gr %E>%
    filter(!edge_is_loop()) %N>%
    filter(!node_is_isolated())

 pl <-  ggraph(gr)+

    geom_edge_fan(
      aes(width=weight, color=type),
      label_colour = "darkgray",angle_calc="along", #size=0.3 #,label_dodge = unit(0.22,"cm")
      arrow = arrow(type="closed",length=unit(0.25,"cm")),
      end_cap = circle(0.6, 'cm'),
    ) +

    scale_edge_width(range=c(0.2,3.0)) +
    scale_edge_color_manual(values=c("green","red")) +
    geom_node_circle(aes(color=group,fill=group,r=0.15),size=0.9) +
    geom_node_text(aes(label=label),lineheight=0.8,hjust =0, nudge_x = -0.1) +

    coord_fixed()+

    theme_graph(base_size=10,base_family = "sans")

 pl
}
