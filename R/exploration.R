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

# keep first instance of duplicates
repeats <- year_draws[duplicated(year_draws$draws),]
# keep later instance of duplicates
repeats_l <- year_draws[duplicated(year_draws$draws, fromLast = TRUE),]

draws_repeated <- merge(x=repeats, y=repeats_l, by = "draws")

