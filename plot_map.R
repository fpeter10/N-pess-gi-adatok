plot_map <- function(data_map, category, year_input, label, title_) {
  
  ggplot(data_map) +
    geom_sf(aes(fill = .data[[category]]), color = "white") +
     scale_fill_viridis_d(
    option = "cividis",
    direction = -1,
    na.value = "grey50",
    drop = FALSE
  ) +
  theme_minimal() +
    labs(title = paste(title_, year_input),
       fill = label) +
    theme(
      plot.title = element_text(size = 15),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      plot.caption = element_text(size = 10)
    )
}



