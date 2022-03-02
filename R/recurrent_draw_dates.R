recurrent_draw_dates <- function (formatted_df, recurring_df) {
  output_vector <- data.frame()
  limit <- nrow(recurring_df)
  for (loopIteration in 1:limit) {
    dates <- dmy()
    dates <- formatted_df[formatted_df$draws == recurring_df[loopIteration,1],]
    dates <- dates[,c(1)]
    dates <- as.list(dates)
    temp <- append(temp, dates)
    temp <- data.frame(temp)
    output_vector <- rbind(output_vector, setNames(temp, names(output_vector)))
  }
  return(output_vector)
}