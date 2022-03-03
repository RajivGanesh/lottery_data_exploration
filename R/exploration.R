library(readr)
library(tidyverse)
library(lubridate)
library(dplyr)

source("R/recurrent_draw_winners.R")
source("R/recurrent_draw_dates.R")

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
two_repeats <- data.frame(repeat_frequency[repeat_frequency$num_repeats == 2,]$draw)
three_repeats <- data.frame(repeat_frequency[repeat_frequency$num_repeats == 3,]$draw)
four_repeats <- data.frame(repeat_frequency[repeat_frequency$num_repeats == 4,]$draw)
five_repeats <- data.frame(repeat_frequency[repeat_frequency$num_repeats == 5,]$draw)

# create a data frame to store the dates of draws that recurred two times
two_repeat_dates <- recurrent_draw_dates(year_draws, two_repeats)
two_recurring_draws <- data.frame(append(two_repeats, two_repeat_dates))
names(two_recurring_draws) <- c("draw", "first_date", "second_date")

# create a data frame to store the dates of draws that recurred three times
three_repeat_dates <- recurrent_draw_dates(year_draws, three_repeats)
three_recurring_draws <- data.frame(append(three_repeats, three_repeat_dates))
names(three_recurring_draws) <- c("draw", "first_date", "second_date", "third_date")

# create a data frame to store the dates of draws that recurred four times
four_repeat_dates <- recurrent_draw_dates(year_draws, four_repeats)
four_recurring_draws <- data.frame(append(four_repeats, four_repeat_dates))
names(four_recurring_draws) <- c("draw", "first_date", "second_date", "third_date", "fourth_date")

# create a data frame to store the dates of the draws that recurred five times
five_repeat_dates <- recurrent_draw_dates(year_draws, five_repeats)
five_recurring_draws <- data.frame(append(five_repeats, five_repeat_dates))
names(five_recurring_draws) <- c("draw", "first_date", "second_date", "third_date", "fourth_date", "fifth_date")

# the total number of winners for each repeated draw
two_repeat_winners <- recurrent_draw_winners(nlcb_df, year_draws, two_repeats)
three_repeat_winners <- recurrent_draw_winners(nlcb_df, year_draws, three_repeats)
four_repeat_winners <- recurrent_draw_winners(nlcb_df, year_draws, four_repeats)
five_repeat_winners <- recurrent_draw_winners(nlcb_df, year_draws, five_repeats)

