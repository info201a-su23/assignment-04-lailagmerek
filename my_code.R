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

# Create trends over time chart
average_prison_ratios <- data %>%
  group_by(state, year) %>%
  summarize(
    black_to_white_ratio = sum(black_prison_pop, na.rm = TRUE) / sum(white_prison_pop, na.rm = TRUE),
    average_prison_pop = mean(total_prison_pop, na.rm = TRUE)
  ) %>%
  filter(!is.na(average_prison_pop) & !is.na(black_to_white_ratio) & year >= 1990) %>%
  arrange(state, year)

chart_1 <- ggplot(average_prison_ratios, aes(x = year, y = black_to_white_ratio, color = state)) +
  geom_line() +
  labs(title = "Black to White Inmate Population Ratio in US States Over Time",
       x = "Year",
       y = "Black to White Ratio",
       color = "State") +
  theme_minimal()

print(chart_1)

# Create a variable comparison chart
filtered_data <- data %>%
  filter(county_name == "King County" & year >= 1990 & 
           year <= 2018 & !is.na(black_prison_pop)) %>%
  select(year, black_prison_pop)

chart_2 <- ggplot(filtered_data, aes(x = year, y = black_prison_pop)) +
  geom_point() +
  labs(title = "Black Prison Population in King County, WA",
       x = "Year (1990-2018)",
       y = "Black Prison Population") +
  theme_minimal()

print(chart_2)

# creating a map
prison_pop <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-pop.csv")
prison_rate <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates.csv")

black_prison_rate_2010 <- prison_rate %>%
  filter(year == 2010) %>%
  group_by(state) %>%
  summarize(states_black_prison_pop_rate = mean(black_prison_pop_rate, na.rm = TRUE))

us_map <- map_data("state")
us_map$region <- toupper(str_sub(us_map$region, 1, 2))
map_black_prison_pop_rate <- left_join(us_map, black_prison_rate_2010,
                                       by = c("region" = "state"))

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank(),
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
  )

data_map <- ggplot(data = map_black_prison_pop_rate, aes(x = long, 
                                                         y = lat, 
                                                         group = group, 
                                                         fill = states_black_prison_pop_rate)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(name = "Avg. Prison Population Rate", low = "lightpink", high = "darkred") +
  labs(title = "Average Black Prison Population per 100,00 People (Rate) by State (2010)") +
  coord_map() +
  blank_theme

print(data_map)
