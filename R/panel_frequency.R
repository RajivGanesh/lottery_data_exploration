panel_frequency <- function (panel) {
  formatted_panel <- table(panel)
  max_count <- max(formatted_panel)
  max_panel_value <- names(formatted_panel)[which(formatted_panel == max_count, arr.ind = TRUE)]
  output_vector <- data.frame(max_count)
  names(output_vector) <- c(max_panel_value)
  
  return(output_vector)
}