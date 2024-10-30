table_pyramid <- function(data_table) {
  datatable(
    data_table(),
    class = c("specific-table"),
    options = list(
      dom = 't',
      paging = FALSE,
      ordering = FALSE,
      searching = FALSE
      ),
    colnames = c(
      'Terület',
      'Nem',
      'Átlag',
      'Medián',
      'Medián \nk. cs.',
      'Max',
      "Max \nk. cs.",
      'Min',
      "Min \nk. cs.",
      "Típus"
      ),
    rownames = FALSE
  )
}


