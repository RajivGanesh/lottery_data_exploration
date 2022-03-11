recurring_jackpots <- function (formatted_df, recurring_df) {
  # initialize output vector for return
  output_vector <- data.frame(recurring_df)
  # initialize vector to store jackpots and total
  jackpot_data <- data.frame()
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
    jackpot_data <- rbind(jackpot_data, temp)
  }
  # enforce that row names are appropriately named
  row.names(jackpot_data) <- 1 : nrow(jackpot_data)
  # combine jackpot and original data frame
  output_vector <- cbind(output_vector, jackpot_data)
  # return data frame
  return(output_vector)
}