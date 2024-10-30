calculation_pyramid <- function(filtered_data_pyramid, pyramid_col) {
  
  pyramid_col <- sym(pyramid_col)
  
  # definiáljuk a csoportokat
  young_age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24")
  middle_age_groups <- c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59")
  old_age_groups <- c("60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+")
  
  # átlag számítás csoportokra
  Pop_young <- sum(filtered_data_pyramid[[as.character(pyramid_col)]][filtered_data_pyramid$AgeGrp %in% young_age_groups], na.rm = TRUE)
  Pop_middle <- sum(filtered_data_pyramid[[as.character(pyramid_col)]][filtered_data_pyramid$AgeGrp %in% middle_age_groups], na.rm = TRUE)
  Pop_old <- sum(filtered_data_pyramid[[as.character(pyramid_col)]][filtered_data_pyramid$AgeGrp %in% old_age_groups], na.rm = TRUE)
  
  # összefogalalás
  summary_data <- filtered_data_pyramid %>%
    group_by(Hungarian_name) %>%
    summarise(
      Nem = if_else(pyramid_col == "PopFemale", "Nő", "Férfi"), 
      Átlag = label_comma(big.mark = " ", accuracy = 1)(mean(as.numeric(!!pyramid_col), na.rm = TRUE)),
      Medián = label_comma(big.mark = " ", accuracy = 1)(median(as.numeric(!!pyramid_col), na.rm = TRUE)),
      Year_median = ifelse(n() > 0, AgeGrp[which.min(abs(!!pyramid_col - median(!!pyramid_col, na.rm = TRUE)))], NA),
      Maximum = label_comma(big.mark = " ", accuracy = 1)(max(as.numeric(!!pyramid_col), na.rm = TRUE)),
      Year_max = ifelse(n() > 0, AgeGrp[which.max(!!pyramid_col)], NA),
      Minimum = label_comma(big.mark = " ", accuracy = 1)(min(as.numeric(!!pyramid_col), na.rm = TRUE)),
      Year_min = ifelse(n() > 0, AgeGrp[which.min(!!pyramid_col)], NA),
      
      # korfa típusok besorololása
      Pyramid_type = case_when(
        Pop_young > Pop_middle & Pop_young > Pop_old ~ "Expanzív",
        Pop_old > Pop_middle & Pop_old > Pop_young ~ "Kontraktív ",
        TRUE ~ "Stacioner"
      )
    )
  return(summary_data)
}
