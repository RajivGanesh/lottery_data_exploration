library(readr)
library(tidyverse)
library(lubridate)
library(dplyr)

# data import
nlcb_cashpot <- read_csv("~/data/lottery/nlcb_cashpot.csv")

# data frame of imported data
nlcb_df <- data.frame(nlcb_cashpot)

# check structure of data frame
str(nlcb_df)

  # # cleaning data frame # #

# convert date column from char to date format
nlcb_df$draw_date <- dmy(nlcb_df$draw_date)
# order rows chronologically
nlcb_df <- nlcb_df[order(nlcb_df$draw_date),]

# convert jackpot column from char to numeric
jackpot <- gsub(",", "", nlcb_df$jackpot)
jackpot <- as.numeric(jackpot)
nlcb_df$jackpot <- jackpot

# check for missing values
summary(nlcb_df)

# replace NA values with 0
nlcb_df$jackpot[is.na(nlcb_df$jackpot)] <- 0
nlcb_df$multiplier[is.na(nlcb_df$multiplier)] <- 0
nlcb_df$num_of_wins[is.na(nlcb_df$num_of_wins)] <- 0

# add draw date to new data frame
year_draws <- data.frame(nlcb_df$draw_date)
# combine all 5 drawn numbers into one column
year_draws$draws <- paste(nlcb_df$number1, nlcb_df$number2, nlcb_df$number3, nlcb_df$number4, nlcb_df$number5)

# data frame that contains no duplicated draw data
sans_duplicates <- data.frame(unique(year_draws$draws,))
names(sans_duplicates) <- c("draws")
# remove empty draws
sans_duplicates <- sans_duplicates[!(sans_duplicates$draws == "0 0 0 0 0"),]
sans_duplicates <- data.frame(sans_duplicates)

# define a data frame to hold the draw frequency data
repeat_frequency <- data.frame(draw <- as.character(), num_repeats <- as.numeric())
# get the total number of repeated records
limit <- nrow(sans_duplicates)

# for loop to populate the repeat_frequency data frame
for(loopIteration in 1:limit) {
  draw <- sans_duplicates[loopIteration,1]
  all_duplicates <- year_draws[year_draws$draws == draw,]
  num_repeats <- nrow(all_duplicates)
  # structure row for data frame
  temp <- data.frame(draw, num_repeats)
  # add row to data frame
  repeat_frequency <- rbind(repeat_frequency, temp)
}

# get only recurring draws
repeat_frequency <- repeat_frequency[!(repeat_frequency$num_repeats == "1"),]

# get the maximum number of repeats among all draws
max_repeats <- max(repeat_frequency$num_repeats)

# organize repeat frequency records by frequency
two_repeats <- repeat_frequency[repeat_frequency$num_repeats == 2,]
three_repeats <- repeat_frequency[repeat_frequency$num_repeats == 3,]
four_repeats <- repeat_frequency[repeat_frequency$num_repeats == 4,]
five_repeats <- repeat_frequency[repeat_frequency$num_repeats == 5,]

# create a data frame to store the dates of draws that recurred twice
draws_repeated_twice <- data.frame(draw <- as.character(), first_date <- dmy(), second_date <- dmy())
names(draws_repeated_twice) <- c("draw", "first_date", "second_date")

# get the number of rows in two_repeats
limit <- nrow(two_repeats)

for (loopIteration in 1:limit) {
  # create variable to store dates
  dates <- dmy()
  # get dates for specific draw
  dates <- year_draws[year_draws$draws == two_repeats[loopIteration,1],]
  # isolate dates for draw
  dates <- dates$nlcb_df.draw_date
  # convert date to list type
  dates <- as.list(dates)
  # temporary data frame to structure a row for final data frame
  temp <- data.frame(draw <- as.character(two_repeats[loopIteration,1]))
  # add dates to temporary data frame
  temp <- append(temp, dates)
  # temp converted from list to data frame
  temp <- data.frame(temp)
  # set column names for temp to add as row in final data frame
  names(temp) <- c("draw", "first_date", "second_date")
  # add temp as row in data frame
  draws_repeated_twice <- rbind(draws_repeated_twice, temp)
}









