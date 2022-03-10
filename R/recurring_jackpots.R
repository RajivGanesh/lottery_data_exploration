recurring_jackpots <- function (formatted_df, recurring_df){
  # initialize output vector to store jackpots and total
  output_vector <- data.frame()
  # get the number of rows in the data frame being processed
  limit <- nrow(recurring_df)
  for (loopIteration in 1:limit) {
    # get the jackpots that are associated with the specified draw
    temp <- data.frame(formatted_df[formatted_df$draws == recurring_df[loopIteration,1],]$`nlcb_df$jackpot`)
    # convert from long data to wide data
    temp <- data.frame(t(temp))
    # add column to store the total of the jackpots
    temp <- cbind(temp, sum(temp))
    # set row name to appropriate numeric value
    rownames(temp)[1] <- loopIteration
    # add row to output data frame
    output_vector <- rbind(output_vector, temp)
  }
  # enforce that row names are appropriately named
  row.names(output_vector) <- 1 : nrow(output_vector)
  # return data frame
  return(output_vector)
}