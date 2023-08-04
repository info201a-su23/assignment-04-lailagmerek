# Load necessary libraries
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(maps)

# Read the data from a CSV file
data <- read_csv("~/Desktop/info201_summer2023/assignment-04-lailagmerek/us-prison-pop.csv")

# Calculate the mean of the total population
mean_total_pop <- mean(data$total_pop, na.rm = TRUE)
print(mean_total_pop)

# Define a function to calculate the mean of the total population
mean_total_pop_function <- function(){
  i <- mean(data$total_pop, na.rm = TRUE)
  return(i)
}

# Calculate the median of the total population
median_total_pop <- median(data$total_pop, na.rm = TRUE)
print(median_total_pop)

# Define a function to calculate the median of the total population
median_total_pop_function <- function(){
  i <- median(data$total_pop, na.rm = TRUE)
  return(i)
}

# Calculate the standard deviation of the total population
sd_total_pop <- sd(data$total_pop, na.rm = TRUE)
print(sd_total_pop)

# Define a function to calculate the standard deviation of the total population
sd_total_pop_function <- function(){
  i <- sd(data$total_pop, na.rm = TRUE)
  return(i)
}

# Calculate the maximum of the total population
max_total_pop <- max(data$total_pop, na.rm = TRUE)
print(max_total_pop)

# Define a function to calculate the maximum of the total population
max_total_pop_function <- function(){
  i <- max(data$total_pop, na.rm = TRUE)
  return(i)
}

# Calculate the minimum of the total population
min_total_pop <- min(data$total_pop, na.rm = TRUE)
print(min_total_pop)

# Define a function to calculate the minimum of the total population
min_total_pop_function <- function(){
  i <- min(data$total_pop, na.rm = TRUE)
  return(i)
}

# Create a trends over time chart
top_2_states <- data %>%
  group_by(state) %>%
  summarise(total_pop = sum(total_pop, na.rm = TRUE)) %>%
  arrange(desc(total_pop)) %>%
  top_n(2)  # Replace N with the desired number of states

data_filtered <- data %>%
  filter(state %in% top_2_states$state)

my_plot <- ggplot(data_filtered, aes(x = year, y = total_pop, color = state)) +
  geom_line() +
  xlab("Year") +
  ylab("Total Population of People Incarcerated in the US") +
  ggtitle("Prison Population Over Time") +
  scale_color_discrete(name = "States", breaks = top_2_states$state, labels = top_2_states$state)

print(my_plot)

# Create a variable comparison chart
white_prison_pop <- data$white_prison_pop
black_prison_pop <- data$black_prison_pop

comparison_plot <- ggplot(data, aes(x = white_prison_pop, y = black_prison_pop)) +
  geom_point() +
  xlab("White Prison Population") + 
  ylab("Black Prison Population")

print(comparison_plot)

# Prepare data for the map
data <- data %>%
  mutate(state = tolower(state))
usa_map <- map_data("state")

# Merge data with map data
data_state_map <- left_join(usa_map, data, by = c("region" = "state"))

# Define the color scale and custom breaks
custom_breaks <- c(0, 50000, 100000, 250000, 500000, 1000000)
color_scale <- scale_fill_gradient(
  low = "lightcoral",
  high = "darkred",
  breaks = custom_breaks,
  labels = comma_format(scale = 1e-6),
  name = "Total Prison Population"
)

# Create the map using the merged data, where the fill is based on the prison population in each state.
prison_pop_map <- ggplot() +
  geom_polygon(data = data_state_map, aes(x = long, y = lat, group = group, fill = total_pop),
               color = "white", size = 0.1) +
  color_scale +
  labs(title = "Prison Population in the USA",
       subtitle = "Data source: Vera Institute",
       caption = "Note: Map not to scale") +
  theme_minimal() +
  coord_fixed()  # Use map-based coordinate system to set the aspect ratio

# Display the map
print(prison_pop_map)