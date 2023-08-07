# Clearing the current R environment
rm(list = ls())

# Loading necessary packages
library(tidyverse)
library(dplyr)

# Reading in the dataset of US prison/jail rates for Washington state from 1990
df_WA <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990-WA.csv")

# Reading in the dataset of US prison/jail rates for Washington state from 1990
df_US <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990.csv")

# Calculate the average value of my variable across all the counties 
# (in the most recent year)
df_filter <- df_US %>%
  mutate(difference_race = black_jail_pop_rate - white_jail_pop_rate, na.rm = TRUE) %>%
  group_by(state) %>%
  summarise(black_jail_pop_rate_mean = mean(black_jail_pop_rate, na.rm = TRUE),
            white_jail_pop_rate_mean = mean(white_jail_pop_rate, na.rm = TRUE),
            difference = mean(difference_race, na.rm = TRUE))

highest_jail_pop_rate_black <- df_filter %>%
  filter(black_jail_pop_rate_mean == max(black_jail_pop_rate_mean,
                                         na.rm = TRUE)) %>%
  select(black_jail_pop_rate_mean) %>% 
  pull(black_jail_pop_rate_mean)

lowest_jail_pop_rate_black <- df_filter %>%
  filter(black_jail_pop_rate_mean == min(black_jail_pop_rate_mean,
                                         na.rm = TRUE)) %>%
  select(black_jail_pop_rate_mean) %>% 
  pull(black_jail_pop_rate_mean)
  
highest_jail_pop_rate_white <- df_filter %>%
  filter(white_jail_pop_rate_mean == max(white_jail_pop_rate_mean,
                                         na.rm = TRUE)) %>%
  select(white_jail_pop_rate_mean) %>% 
  pull(white_jail_pop_rate_mean)

lowest_jail_pop_rate_white <- df_filter %>%
  filter(white_jail_pop_rate_mean == min(white_jail_pop_rate_mean,
                                         na.rm = TRUE)) %>%
  select(white_jail_pop_rate_mean) %>% 
  pull(white_jail_pop_rate_mean)

highest_jail_pop_rate_diff <- df_filter %>%
  filter(difference == max(difference, na.rm = TRUE)) %>%
  select(difference) %>% 
  pull(difference)

lowest_jail_pop_rate_diff <- df_filter %>%
  filter(difference == min(difference, na.rm = TRUE)) %>%
  select(difference) %>% 
  pull(difference)

highest_jail_pop_rate_diff_state <- df_filter %>%
  filter(difference == highest_jail_pop_rate_diff) %>%
  select(state) %>% 
  pull(state)

lowest_jail_pop_rate_diff_state <- df_filter %>%
  filter(difference == lowest_jail_pop_rate_diff) %>%
  select(state) %>% 
  pull(state)







