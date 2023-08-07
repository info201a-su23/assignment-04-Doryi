# Clearing the current R environment
rm(list = ls())

# Loading necessary packages
library(tidyverse)
library(dplyr)
library(ggplot2)

# Reading in the dataset of US prison/jail rates for Washington state from 1990
df_US <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990.csv")

# Calculate the average jail population rate and difference between black and 
# white jail population rates for each state
df_filter <- df_US %>%
  mutate(difference_race = black_jail_pop_rate - white_jail_pop_rate, na.rm = TRUE) %>%
  group_by(state) %>%
  summarise(value = mean(total_jail_pop_rate, na.rm = TRUE),
            difference = mean(difference_race, na.rm = TRUE)) 

# Create a lookup table to map state abbreviations to their full names
state_lookup <- data.frame(
  state_abbrev = c(
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
    "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
    "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
    "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
    "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
  ),
  state_full = c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "California",
    "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
    "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
    "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
    "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana",
    "Nebraska", "Nevada", "New Hampshire", "New Jersey",
    "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
    "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
    "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
  )
)

# Merge the aggregated data with the state lookup to replace state 
# abbreviations with full names
df_filter <- left_join(df_filter, state_lookup, by = c("state" = "state_abbrev"))

# Convert full state names to lowercase to match with mapping data
df_filter$state_full <- tolower(df_filter$state_full)

# Create the US states map data
states_map <- map_data("state")

# Join the map data with the jail population data based on state names
df_joined <- left_join(states_map, df_filter, by = c("region" = "state_full"))

# Define a theme for the map visualization that removes unnecessary elements
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank() # remove border around plot
  )

# Construct the map visualization with polygons representing states and color 
# gradients based on jail population
total_map <- ggplot(df_joined) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = value),
    color = "white", # show state outlines
    size = .1 # thinly stroked
  ) +
  scale_fill_gradient(
    low = "lightblue", high = "darkred",
    limits = c(0, max(df_filter$value))
  ) +
  labs(
    fill = "Jail Population",
    title = "Average Statewide Jail Population (1990-2018)"
  ) +
  blank_theme

# Create a visualization of the difference in jail population rates between 
# white and black populations by state
race_diff_map <- ggplot(df_joined) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = difference),
    color = "white", # show state outlines
    size = .1 # thinly stroked
  ) +
  scale_fill_gradient(
    low = "lightblue", high = "darkred",
    limits = c(0, max(df_filter$value))
  ) +
  labs(
    fill = "Population Difference",
    title = "Average Statewide Difference in Jail Population Between Black 
            and White Races (1990-2018)"
  ) +
  blank_theme

print(total_map)
print(race_diff_map)
