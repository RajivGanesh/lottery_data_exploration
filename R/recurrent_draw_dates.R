# this function returns a data frame of all of dates a draw was repeated on
recurrent_draw_dates <- function (formatted_df, recurring_df) {
  # initialize output vector to store dates
  output_vector <- data.frame()
  # get the number of rows in the data frame being processed
  limit <- nrow(recurring_df)
  for (loopIteration in 1:limit) {
    # initialize data frame to format row
    temp <- data.frame()
    # initialize vector to store dates
    dates <- dmy()
    # get all the rows corresponding to a specific draw
    dates <- formatted_df[formatted_df$draws == recurring_df[loopIteration,1],]
    # only store years
    dates <- dates[,c(1)]
    # format variable as list
    dates <- as.list(dates)
    # append dates to temp variable
    temp <- append(temp, dates)
    # convert list to data frame
    temp <- data.frame(temp)
    # pass temp as a row into output vector
    output_vector <- rbind(output_vector, setNames(temp, names(output_vector)))
  }
  # return data frame
  return(output_vector)
}