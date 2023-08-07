# Clearing the current R environment
rm(list = ls())

# Loading necessary packages
library(tidyverse)
library(dplyr)
library(ggplot2)

# Reading in the dataset of US prison/jail rates for Washington state from 1990
df_WA <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990-WA.csv")

# Filtering the data to only keep rows with the highest jail population rate 
# for each urbanicity per year.
highest_rate_per_urbanicity_WA <- df_WA %>% group_by(year, urbanicity) %>%
  filter(total_jail_pop_rate == max(total_jail_pop_rate, na.rm = TRUE)) %>%
  select(year, urbanicity, total_jail_pop_rate) %>%
  arrange(year)

# Transforming data from long to wide format for visualization
urbanicity_trend_WA <- spread(
  highest_rate_per_urbanicity_WA,
  key = urbanicity,
  value = total_jail_pop_rate
)

# Plotting the trends in jail population rates by urbanicity over the years
line_chart <- ggplot(urbanicity_trend_WA, aes(x = year)) + 
  geom_line(aes(y = rural, color = "Rural")) +
  geom_line(aes(y = `small/mid`, color = "Small/mid")) +
  geom_line(aes(y = suburban, color = "Suburban")) +
  geom_line(aes(y = urban, color = "Urban")) +
  xlab("Year") +
  ylab("Total Jail Population Rate(per 100,000 people)") +
  labs(color = "Urbanicity") +
  labs(title= "Trend of Highest Jail Population Rate by Urbanicity 
       Over the Years(1990 - 2018)")

print(line_chart)

