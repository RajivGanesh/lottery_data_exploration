# this function returns the most repeated draw number for a user defined panel
panel_frequency <- function (panel) {
  # convert panel to a table
  formatted_panel <- table(panel)
  # get the maximum value from the table (panel) for the most frequent number
  max_count <- max(formatted_panel)
  # get the most repeated draw number using the maximum value from the table
  max_panel_value <- names(formatted_panel)[which(formatted_panel == max_count, arr.ind = TRUE)]
  # create output vector as a data frame that contains the maximum value of the repeated number
  output_vector <- data.frame(max_count)
  # change the name of the data frame's column to the most repeated draw number
  names(output_vector) <- c(max_panel_value)
  # return output vector to main
  return(output_vector)
}