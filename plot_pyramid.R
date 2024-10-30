plot_pyramid <- function(data, years, countries) {
  max_pop <- ceiling(max(max(data$PopMale, na.rm = TRUE), max(data$PopFemale, na.rm = TRUE)) / 1e6) * 1e6
  
  # Kiszámítja az átlagos korcsoportot a férfi és női korfára is.
  avg_pop_male <- mean(data$PopMale, na.rm = TRUE)
  avg_pop_female <- mean(data$PopFemale, na.rm = TRUE)
  
  # Plotly ábra
  plot_ly(
    data,
    x = ~ PopMale,
    y = ~ AgeGrp,
    name = 'Férfi',
    type = 'bar',
    orientation = 'h',
    marker = list(color = 'blue')
  ) %>%
    add_trace(
      x = ~ -PopFemale,
      name = 'Nő',
      marker = list(color = 'red')
    ) %>%
    layout(
      barmode = 'overlay',
      title = paste(
        years,
        countries,
        "népességi piramisa"
      ),
      xaxis = list(
        title = 'Népesség',
        tickvals = seq(-max_pop, max_pop, by = max_pop / 5),
        ticktext = c(
          abs(seq(-max_pop, 0, by = max_pop / 5)),
          seq(max_pop / 5, max_pop, by = max_pop / 5)
        ),
        range = c(-max_pop, max_pop)
      ),
      yaxis = list(
        title = 'Korosztály',
        categoryorder = "array",
        categoryarray = data$AgeGrp
      ),
      shapes = list(
        # Vízszintes vonal férfi
        list(
          type = "line",
          y0 = min(data$AgeGrp),
          y1 = max(data$AgeGrp),
          x0 = avg_pop_male,
          x1 = avg_pop_male,
          line = list(color = "blue", dash = "dash", width = 1)
        ),
        # Vízszintes vonal nő
        list(
          type = "line",
          y0 = min(data$AgeGrp),
          y1 = max(data$AgeGrp),
          x0 = -avg_pop_female,  
          x1 = -avg_pop_female,
          line = list(color = "red", dash = "dash", width = 1)
        )
      )
    )
}


