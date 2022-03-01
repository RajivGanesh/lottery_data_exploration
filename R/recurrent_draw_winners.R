# this function returns the total number of winners for each draw that has been repeated
recurrent_draw_winners <- function (origin_df, formatted_df, recurring_df) {
  # vector that would return total winners
  output_vector <- data.frame(draw <- as.character(), total_winners <- as.numeric())
  names(output_vector) <- c("draw", "total_winners")
  # get the total number of rows of the data frame with the repeated draws
  limit <- nrow(recurring_df)
  for (loopIteration in 1:limit) {
    # get the indexes of the draw being processed
    indexes <- which(formatted_df$draws == recurring_df[loopIteration,1])
    # get the number of indexes found
    index_length <- length(indexes)
    winners_vector <- as.numeric()
    # get the winners from each index
    for (innerIteration in 1:index_length) {
      # get the index of the date being processed
      index_value <- indexes[innerIteration]
      # append the number of winners to a vector
      winners_vector <- append(winners_vector, origin_df[index_value,10])
    }
    # determine the total number of winners
    total <- sum(winners_vector)
    # prepare a row for insertion into the vector being returned
    stage <- data.frame(draw <- as.character(recurring_df[loopIteration,1]), total_winners <- as.numeric(total))
    names(stage) <- c("draw", "total_winners")
    # insert row to returning vector
    output_vector <- rbind(output_vector, stage)
  }
  # return vector to main
  return(output_vector)
}
