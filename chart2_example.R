# Clearing the current R environment
rm(list = ls())

# Loading necessary packages
library(tidyverse)
library(dplyr)
library(ggplot2)

# Reading in the dataset of US prison/jail rates for Washington state from 1990
df_WA <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990-WA.csv")

# Convert data from wide to long format using gather
df_long <- df_WA %>% group_by(year, urbanicity) %>%
  summarise(mean_year_black = mean(black_jail_pop_rate, na.rm = TRUE), 
            mean_year_white = mean(white_jail_pop_rate, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(urbanicity) %>%
  summarise(Black = mean(mean_year_black, na.rm = TRUE),
            White = mean(mean_year_white, na.rm = TRUE),
            .groups = "drop") %>%
  gather(key = "Race", value = "Population_rate", -urbanicity)


# Calculating mean jail population rates for Black and White populations 
# by urbanicity
bar_chart <- ggplot(df_long) +
  geom_col(
    mapping = aes(x = urbanicity, y = Population_rate, fill = Race),
    position = "dodge"
  ) +
  labs(title = "Average Jail Population Rate by Race Across 
       Urbanicity Levels(through 1990 - 2018)", 
       x = "Urbanicity", 
       y = "Jail Population Rate(per 100,000 people)",
  )

print(bar_chart)
