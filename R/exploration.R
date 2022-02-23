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

  ## cleaning data frame ##

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

# find all duplicate draws if any exist
repeats <- year_draws[duplicated(year_draws$draws),]
# drop all rows with "0 0 0 0 0" as the numbers drawn
repeats <- repeats[!(repeats$draws == "0 0 0 0 0"),]


repeated_draws <- data.frame(draw <- as.character(NA), num_repeats <- as.numeric(NA), dates <- dmy(NA))
names(repeated_draws) <- c("draw", "num_repeats", "dates")
# get repeated draw
draw <- repeats[496,2]
# get the records of all the repeated draw
all_duplicates <- year_draws[year_draws$draws == draw,]
# count the number of repeated times the draw has been repeated
num_repeats <- nrow(all_duplicates)
dates <- dmy()

for (i in 1:num_repeats) {
  dates <- append(dates, all_duplicates[i,1])
  i <- i+1
}






