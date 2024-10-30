plot_demographic <- function(data_line, data_point, demographic_column, y_label, x_label, title_, 
                             year_input_min, year_input_max, year_interested) {
  
  ggplot(data_line, aes(x = Time, y = .data[[demographic_column]], color = Hungarian_name, group = Hungarian_name)) +
    geom_line() +
    labs(title = paste(title_, year_input_min, " - ", year_input_max, "\nKiválasztott év: ", year_interested), 
         x = x_label, y = y_label, color = "Kiválasztott \nország \nvagy régió") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      plot.caption = element_text(size = 10)
    ) +
    
    # pont
    geom_point(data = data_point, aes(x = Time, y = .data[[demographic_column]]), color = "red", size = 3) +
    
    # felirat ponthoz
    geom_text(data = data_point, 
              aes(x = Time, y = .data[[demographic_column]], label = round(.data[[demographic_column]], 2)), 
              vjust = -1, color = "black", check_overlap = TRUE)
}


