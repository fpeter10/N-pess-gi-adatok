table_demographic <- function(data_table) {
  datatable(
    data_table(),
    class = c("specific-table"),
    options = list(
      dom = 't',
      paging = TRUE,
      ordering = TRUE,
      searching = FALSE,
      columnDefs = list(
        list(orderable = TRUE, targets = 0),
        list(orderable = FALSE, targets = 1)
      )
    ),
    colnames = c(
      'Terület',
      'Átlag',
      'Medián',
      'Medián év',
      'Max',
      "Max év",
      'Min',
      "Min év",
      "Kiválasztott\név",
      "Érték",
      "Trend"
    ),
    rownames = TRUE
  )
  
}