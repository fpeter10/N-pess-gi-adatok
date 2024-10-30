calculation_demographic <- function(data, demographic_col, year_selected, year_min, year_max) {
  
  demographic <- sym(demographic_col)
  # Tendencia kiszámítása
  calc_trend <- function(data, year_selected, demographic_col) {
    #Ellenőrzi hogy a kiválasztott pont előtt és után van e elég év.
    years_before <- data %>%
      filter(Time >= year_selected - 2, Time < year_selected)
    years_after <- data %>%
      filter(Time > year_selected, Time <= year_selected + 2)
    if (nrow(years_before) < 2 || nrow(years_after) < 2) {
      return("Nincs elég adat")
    }
    
    before_data <- years_before %>% pull(!!sym(demographic_col))
    after_data <- years_after %>% pull(!!sym(demographic_col))
    
    #Meredekségek kiszámítása
    slope_before <- lm(before_data ~ seq_along(before_data))$coefficients[2]
    slope_after <- lm(after_data ~ seq_along(after_data))$coefficients[2]
    #Merdekség alapján besorolás
    if (slope_before > 0 & slope_after > 0) {
      return("Növekvő")
    } else if (slope_before < 0 & slope_after < 0) {
      return("Csökkenő")
    } else {
      return("Trend változás")
    }
  }
  
  # Demográfiai adat összefoglaló
  summary_data <- data %>%
    group_by(Hungarian_name) %>%
    summarise(
      Átlag = label_comma(big.mark = " ", accuracy = 0.01)
      (mean(as.numeric(!!demographic, na.rm = TRUE))),
      
      Medián = label_comma(big.mark = " ", accuracy = 0.01)
      (median(as.numeric(!!demographic, na.rm = TRUE))),
      
      'Medián év' = Time[which.min(abs(!!demographic - median(!!demographic)))],
      
      Max = label_comma(big.mark = " ", accuracy = 0.01)
      (max(as.numeric(!!demographic, na.rm = TRUE))),
      
      'Max év' = Time[which.max(!!demographic)],
      
      Min = label_comma(big.mark = " ", accuracy = 0.01)
      (min(as.numeric(!!demographic, na.rm = TRUE))),
      
      'Min év' = Time[which.min(!!demographic)],
      
      'Választott év' = year_selected
  )
  
  # A kiválasztott évhez adat, illetve trend hozzá adása
  selected_year_data <- data %>%
    filter(Time == year_selected) %>%
    group_by(Hungarian_name) %>%
    summarise(
      pop_selected_year = unique(as.numeric(!!demographic))
    )
  
  trend_data <- data %>%
    group_by(Hungarian_name) %>%
    summarise(
      Trend = calc_trend(cur_data(), year_selected, demographic_col)
    )
  
  # Az adatok mergelése a név alapján
  final_summary_data <- summary_data %>%
    left_join(selected_year_data, by = "Hungarian_name") %>%
    left_join(trend_data, by = "Hungarian_name") %>%
    mutate(
      pop_selected_year = label_comma(big.mark = " ", accuracy = 0.01)(pop_selected_year)
    )
  
  # Magyar nevek szerinti ABC rendbe sorolás
  final_summary_data <- final_summary_data[order(stri_rank(final_summary_data$Hungarian_name, locale = "hu_HU")), ]
  datatable(final_summary_data)
  
  return(final_summary_data)
}

