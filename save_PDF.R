save_pdf <- function(plot, file) {
  ggsave(filename = file, plot = plot, device = "pdf", width = 12, height = 6)
}

