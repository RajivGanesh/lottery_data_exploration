year_data <- function (origin_df) {
  # initialize output vector for return
  output_vector <- data.frame()
  # get the number of years in the data frame
  limit <- year(origin_df$draw_date[nrow(origin_df)]) - year(origin_df$draw_date[1])
  # find earliest year in data frame
  earliest_year <-min(year(origin_df$draw_date))
  # set year increment variable equal to the earliest year
  f_year <- earliest_year
  for (i in 1:limit) {
    # initialize data frame to stage row for binding to output data set
    stage <- data.frame(y <- numeric(), mj <- numeric(), mjt <- numeric(), j <- numeric(), a <- numeric(), w <- numeric())
    # the records of the data frame for a specific year
    year_record <- subset(origin_df, format(as.Date(draw_date),"%Y") == f_year)
    # the minimum jackpot for a specific year
    min_jackpot <- min(year_record$jackpot[year_record$jackpot > 0])
    # the maximum jackpot for a specific year
    max_jackpot <- max(year_record$jackpot)
    # the sum of jackpots for the entire year
    total_jackpot <- sum(year_record$jackpot)
    # the average jackpot value throughout the year
    avg_jackpot <- format(round(mean(year_record$jackpot),2), nsmall = 2)
    # the sum of winners for the entire year
    total_winners <- sum(year_record$num_of_wins)
    # add findings to staging data frame
    stage[1,1] <- f_year
    stage[1,2] <- min_jackpot
    stage[1,3] <- max_jackpot
    stage[1,4] <- total_jackpot
    stage[1,5] <- avg_jackpot
    stage[1,6] <- total_winners
    # bind staged data frame to output data frame
    output_vector <- rbind(output_vector, stage)
    # increment year
    f_year <- f_year + 1
  }
  names(output_vector) <- c("year", "minimum_jackpot", "maximum_jackpot", "total_annual_jackpot", "average_jackpot_value","total_winners")
  # return output data set
  return(output_vector)
}