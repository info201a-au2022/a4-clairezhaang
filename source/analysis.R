library(tidyverse)
library(dplyr)
library(ggplot2)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num = 6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>

# Getting the data
url <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
destination <- "/Users/clairezhang/Documents/info201/data/incarceration_trends_csv"
download.file(url, destination)

data <- read.csv("~/Documents/info201/data/incarceration_trends_csv")

# General dataframe
incarceration_df <- read.csv("~/Documents/info201/data/incarceration_trends_csv", stringsAsFactors = FALSE) %>% 
  select(year, 
         state, 
         county_name, 
         total_pop, 
         total_pop_15to64, 
         aapi_pop_15to64, 
         black_pop_15to64, 
         latinx_pop_15to64, 
         native_pop_15to64, 
         white_pop_15to64, 
         region, 
         division, 
         total_jail_pop, 
         aapi_jail_pop, 
         black_jail_pop, 
         latinx_jail_pop, 
         native_jail_pop, 
         white_jail_pop, 
         other_race_jail_pop, 
         total_prison_pop, 
         aapi_prison_pop, 
         black_prison_pop, 
         latinx_prison_pop, 
         native_prison_pop, 
         other_race_prison_pop, 
         white_prison_pop, 
         aapi_female_prison_pop,
         aapi_male_prison_pop, 
         black_female_prison_pop, 
         black_male_prison_pop, 
         latinx_female_prison_pop, 
         latinx_male_prison_pop, 
         native_female_prison_pop, 
         native_male_prison_pop, 
         other_race_female_prison_pop, 
         other_race_male_prison_pop, 
         white_female_prison_pop, 
         white_male_prison_pop, 
         total_jail_pop_rate, 
         female_jail_pop_rate, 
         male_jail_pop_rate, 
         aapi_jail_pop_rate, 
         black_jail_pop_rate, 
         latinx_jail_pop_rate, 
         native_jail_pop_rate, 
         white_jail_pop_rate, 
         total_prison_pop_rate, 
         aapi_prison_pop_rate, 
         black_prison_pop_rate, 
         latinx_prison_pop_rate, 
         native_prison_pop_rate, 
         white_prison_pop_rate
         )

# Total 15-64 population and total for each race 15-64 by year
totals_by_race <- incarceration_df %>%
  group_by(year) %>%
  summarise(
    across(c(total_pop_15to64, aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64, white_pop_15to64), sum)
  )
  
# Proportion of people 15-64 in each race in compared to total 15-64 population by year
race_to_total_ratios <- totals_by_race %>%
  mutate(
    aapi_ratio = aapi_pop_15to64/total_pop_15to64,
    black_ratio = black_pop_15to64/total_pop_15to64,
    latinx_ratio = latinx_pop_15to64/total_pop_15to64,
    native_ratio = native_pop_15to64/total_pop_15to64,
    white_ratio = white_pop_15to64/total_pop_15to64, .keep = "unused"
  )

# 1990 proportions
race_total_ratio_1990 <- race_to_total_ratios %>%
  filter(year == 1990) %>%
  summarise(
    aapi_1990 = aapi_ratio,
    black_1990 = black_ratio,
    latinx_1990 = latinx_ratio,
    native_1990 = native_ratio,
    white_1990 = white_ratio
  )

# 1990 AAPI
aapi_1990 <- race_to_total_ratios %>%
  filter(year == 1990) %>%
  pull(aapi_ratio)

# 1990 Black
black_1990 <- race_to_total_ratios %>%
  filter(year == 1990) %>%
  pull(black_ratio)

# 1990 Latinx
latinx_1990 <- race_to_total_ratios %>%
  filter(year == 1990) %>%
  pull(latinx_ratio)

# 1990 Native
native_1990 <- race_to_total_ratios %>%
  filter(year == 1990) %>%
  pull(native_ratio)

# 1990 White
white_1990 <- race_to_total_ratios %>%
  filter(year == 1990) %>%
  pull(white_ratio)

# 2018 AAPI
aapi_2018 <- race_to_total_ratios %>%
  filter(year == 2018) %>%
  pull(aapi_ratio)

# 2018 Black
black_2018 <- race_to_total_ratios %>%
  filter(year == 2018) %>%
  pull(black_ratio)

# 2018 Latinx
latinx_2018 <- race_to_total_ratios %>%
  filter(year == 2018) %>%
  pull(latinx_ratio)

# 2018 Native
native_2018 <- race_to_total_ratios %>%
  filter(year == 2018) %>%
  pull(native_ratio)

# 2018 White
white_2018 <- race_to_total_ratios %>%
  filter(year == 2018) %>%
  pull(white_ratio)

# White prop change 1990-2018
white_prop_change <- white_2018 - white_1990

# Black prop change 1990-2018
black_prop_change <- black_2018 - black_1990

# Total jail population and total for each race 15-64 in jail by year
totals_by_race_jail <- incarceration_df %>%
  group_by(year) %>%
  summarise(
    across(c(total_jail_pop, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop), sum, na.rm = TRUE)
  )

# Proportion of people 15-64 in each race in jail compared to total 15-64 population in jail by year
race_to_total_jail_ratios <- totals_by_race_jail %>%
  mutate(
    aapi_ratio_jail = aapi_jail_pop/total_jail_pop,
    black_ratio_jail = black_jail_pop/total_jail_pop,
    latinx_ratio_jail = latinx_jail_pop/total_jail_pop,
    native_ratio_jail = native_jail_pop/total_jail_pop,
    white_ratio_jail = white_jail_pop/total_jail_pop, .keep = "unused"
  )

# Average proportions of people in jail 
avg_race_jail_ratios <- race_to_total_jail_ratios %>%
  summarise(
    avg_aapi_ratio_jail = mean(aapi_ratio_jail),
    avg_black_ratio_jail = mean(black_ratio_jail),
    avg_latinx_ratio_jail = mean(latinx_ratio_jail),
    avg_native_ratio_jail = mean(native_ratio_jail),
    avg_white_ratio_jail = mean(white_ratio_jail), .keep = "unused"
  )
  
# Avg prop of aapi people in jail
avg_aapi_jail_prop <- avg_race_jail_ratios %>%
  pull(avg_aapi_ratio_jail)

# Avg prop of black people in jail
avg_black_jail_prop <- avg_race_jail_ratios %>%
  pull(avg_black_ratio_jail)

# Avg prop of latinx people in jail
avg_latinx_jail_prop <- avg_race_jail_ratios %>%
  pull(avg_latinx_ratio_jail)

# Avg prop of native people in jail
avg_native_jail_prop <- avg_race_jail_ratios %>%
  pull(avg_native_ratio_jail)

# Avg prop of white people in jail
avg_white_jail_prop <- avg_race_jail_ratios %>%
  pull(avg_white_ratio_jail)

# 1990 AAPI Jail
aapi_1990_jail <- race_to_total_jail_ratios %>%
  filter(year == 1990) %>%
  pull(aapi_ratio_jail)

# 1990 Black Jail
black_1990_jail <- race_to_total_jail_ratios %>%
  filter(year == 1990) %>%
  pull(black_ratio_jail)

# 1990 Latinx Jail
latinx_1990_jail <- race_to_total_jail_ratios %>%
  filter(year == 1990) %>%
  pull(latinx_ratio_jail)

# 1990 Native Jail
native_1990_jail <- race_to_total_jail_ratios %>%
  filter(year == 1990) %>%
  pull(native_ratio_jail)

# 1990 White Jail
white_1990_jail <- race_to_total_jail_ratios %>%
  filter(year == 1990) %>%
  pull(white_ratio_jail)

# 2018 AAPI Jail
aapi_2018_jail <- race_to_total_jail_ratios %>%
  filter(year == 2018) %>%
  pull(aapi_ratio_jail)

# 2018 Black Jail
black_2018_jail <- race_to_total_jail_ratios %>%
  filter(year == 2018) %>%
  pull(black_ratio_jail)

# 2018 Latinx Jail
latinx_2018_jail <- race_to_total_jail_ratios %>%
  filter(year == 2018) %>%
  pull(latinx_ratio_jail)

# 2018 Native Jail
native_2018_jail <- race_to_total_jail_ratios %>%
  filter(year == 2018) %>%
  pull(native_ratio_jail)

# 2018 White Jail
white_2018_jail <- race_to_total_jail_ratios %>%
  filter(year == 2018) %>%
  pull(white_ratio_jail)

# White jail prop change 1990-2018
white_jail_prop_change <- white_2018_jail - white_1990_jail

# Black jail prop change 1990-2018
black_jail_prop_change <- black_2018_jail - black_1990_jail

# Total populations 15-64 in jail by race and state
races_jail_total_state <- incarceration_df %>% 
  group_by(state) %>% 
  summarise(
    total_jail_state = sum(total_jail_pop, na.rm = TRUE), 
    total_15to64_state = sum(total_pop_15to64, na.rm = TRUE), 
    aapi_jail_total_state = sum(aapi_jail_pop, na.rm = TRUE), 
    aapi_15to64_total_state = sum(aapi_pop_15to64, na.rm = TRUE), 
    black_jail_total_state = sum(black_jail_pop, na.rm = TRUE), 
    black_15to64_total_state = sum(black_pop_15to64, na.rm = TRUE),
    latinx_jail_total_state = sum(latinx_jail_pop, na.rm = TRUE), 
    latinx_15to64_total_state = sum(latinx_pop_15to64, na.rm = TRUE), 
    native_jail_total_state = sum(native_jail_pop,na.rm = TRUE), 
    native_15to64_total_state = sum(native_pop_15to64, na.rm = TRUE), 
    white_jail_total_state = sum(white_prison_pop, na.rm = TRUE), 
    white_15to64_total_state = sum(white_pop_15to64, na.rm = TRUE)
  )

# Proportion of population in jail compared to total 15-64 population by race and state
races_jail_ratio_state <- races_jail_total_state %>% 
  mutate(
    total_jail_ratio_state = total_jail_state/total_15to64_state, 
    aapi_jail_ratio_state = aapi_jail_total_state/aapi_15to64_total_state, 
    black_jail_ratio_state = black_jail_total_state/black_15to64_total_state, 
    latinx_jail_ratio_state = latinx_jail_total_state/latinx_15to64_total_state, 
    native_jail_ratio_state = native_jail_total_state/native_15to64_total_state, 
    white_jail_ratio_state = white_jail_total_state/white_15to64_total_state, .keep = "unused"
         )


# Average of proportions of populations in jail by race
races_jail_ratio_average <- races_jail_ratio_state %>%
  summarise(
    total_jail_ratio_average = mean(total_jail_ratio_state),
    aapi_jail_ratio_average = mean(aapi_jail_ratio_state),
    black_jail_ratio_average = mean(black_jail_ratio_state), 
    latinx_jail_ratio_average = mean(latinx_jail_ratio_state), 
    native_jail_ratio_average = mean(native_jail_ratio_state), 
    white_jail_ratio_average = mean(white_jail_ratio_state)
  )

# Plot of proportion in jail by race and state
plot_races_jail_ratio <- races_jail_ratio_state %>%
  select(state, aapi_jail_ratio_state, black_jail_ratio_state, latinx_jail_ratio_state, native_jail_ratio_state, white_jail_ratio_state) %>%
  gather(key = race, value = ratio, -state)

jail_ratio_state_plot <- ggplot(plot_races_jail_ratio) +
  geom_col(mapping = aes(x = state, y = ratio, fill = race))

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function gets the total number of people who were in jail each year
get_year_jail_pop <- function() {
  year_jail_pop <- incarceration_df %>% 
    group_by(year) %>% 
    summarise(
      total_jail_population = sum(total_jail_pop, na.rm = TRUE)
      )
  return(year_jail_pop)   
}

# This function plots the total number of people who were in jail each year
plot_jail_pop_for_us <- function()  {
  year_jail_pop <- get_year_jail_pop()
  plot_us_jail_pop <- ggplot(data = year_jail_pop) + 
    geom_col(mapping = aes(x = year, y = total_jail_population)) +
    labs(title = "Growth of the US Prison Population 1970-2018")
  return(plot_us_jail_pop)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas

# States included: Washington, New York, California, Texas
states <- c("WA","NY","CA", "TX")

# Function that gets the number of people in jail per year for each state
get_jail_pop_by_state <- function(states) {
  year_jail_pop_all_states <- incarceration_df %>% 
    group_by(year, state) %>%
    summarise(jail_pop_total_state = sum(total_jail_pop, na.rm = TRUE)) %>% 
    spread(key = state, value = jail_pop_total_state)
  year_jail_pop_states <- year_jail_pop_all_states
  return(year_jail_pop_states)
}

# Function that plots a line for the number of people in jail over time for each state within the states vector
plot_jail_pop_by_state <- function(states) {
  year_jail_pop_states <- get_jail_pop_by_state(states)
  lineplot <- ggplot(data = year_jail_pop_states, aes(x = year)) +
    geom_line(aes(y = year_jail_pop_states[[states[1]]]), color = "red") +
    geom_line(aes(y = year_jail_pop_states[[states[2]]]), color = "green") +
    geom_line(aes(y = year_jail_pop_states[[states[3]]]), color = "blue") +
    geom_line(aes(y = year_jail_pop_states[[states[4]]]), color = "purple") +
    labs(title = "Growth of Prison Population in WA, NY, CA, and TX") +
    xlab("Year") +
    ylab("# of People in Jail")
  return(lineplot)
}

test <- function(states) {
  year_jail_pop_states <- get_jail_pop_by_state(states)
  
}
plot_jail_pop_by_state(states)

# Resource: https://www.geeksforgeeks.org/how-to-create-a-plot-using-ggplot2-with-multiple-lines-in-r/
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas

# Gender dataframe
gender_df <- read.csv("~/Documents/info201/data/incarceration_trends_csv", stringsAsFactors = FALSE) %>% 
  select(year, state, county_name, total_pop_15to64, female_pop_15to64, male_pop_15to64, total_jail_pop, female_jail_pop, male_jail_pop)

# Function that gets the male and female populations in jail in each state for the year entered
get_jail_gender_pop_states <-function(year){
  year_jail_gender_pop_states <- gender_df %>% 
    filter(year == year) %>% 
    group_by(state) %>%
    summarise(
      male_jail_pop_state = sum(male_jail_pop, na.rm = TRUE), 
      female_jail_pop_state = sum(female_jail_pop, na.rm = TRUE)
      )
  return(year_jail_gender_pop_states)
}

# Function that plots the male and female populations in jail in each state for the year entered
plot_jail_gender_ratio_states <- function(year)  {
  year_jail_gender_pop_states <- get_jail_gender_pop_states(year)
  year_jail_gender_pop_states_plot <- year_jail_gender_pop_states %>%
    gather(key = gender, value = ratio, -state)
  gender_plot <- ggplot(data = year_jail_gender_pop_states_plot) +
    geom_col(mapping = aes(x = state, y = ratio, fill = gender)) +
    xlab("State") +
    ylab("# of People in Jail") +
    labs(title = "Male and Female Prison Population by State")
  return(gender_plot)
}

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
library(maps)
library(mapproj)

# State dataframe
state_df <- races_jail_ratio_state %>%
  mutate(state = state.name[match(state, state.abb)]) %>%
  mutate(state = tolower(state))

# Editing state names and codes dataframe for joining purposes
state_names_and_codes <- read.csv("~/Documents/info201/assignments/a4-clairezhaang/source/state_names_and_codes.csv", stringsAsFactors = FALSE) %>% 
  rename(state = State, code = Code) %>% 
  select(state, code) %>% 
  mutate(state = tolower(state))

# Dataframe after joining state df with state names and codes to be used when creating maps
state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(state_df, by = "state")


# Cloropleth map of ratio of Black population in jail
black_jail_ratio_map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_ratio_state),
    color = "black",
    linewidth = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "#FFE188", high = "#CE0C00",  limits = c(0, 0.02)) +
  labs(fill = "Ratio") +
  theme(legend.key.size = unit(0.4, 'cm')) +
  labs(title = "Proportion of Black Population in Jail Compared to Total 15-64 Black Population in Each State") +
  theme(plot.title = element_text(size = 12))

# Cloropleth map of ratio of White population in jail
white_jail_ratio_map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = white_jail_ratio_state),
    color = "black",
    linewidth = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "#FFE188", high = "#CE0C00", limits = c(0, 0.02)) +
  labs(fill = "Ratio") +
  theme(legend.key.size = unit(0.4, 'cm')) +
  labs(title = "Proportion of White Population in Jail Compared to Total 15-64 White Population in Each State") +
  theme(plot.title = element_text(size = 12))

# Cloropleth map of ratio of AAPI population in jail
aapi_jail_ratio_map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = aapi_jail_ratio_state),
    color = "black",
    linewidth = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "#FFE188", high = "#CE0C00", limits = c(0, 0.02)) +
  labs(fill = "Ratio") +
  theme(legend.key.size = unit(0.4, 'cm')) +
  labs(title = "Proportion of AAPI Population in Jail Compared to Total 15-64 White Population in Each State") +
  theme(plot.title = element_text(size = 12))

# Cloropleth map of ratio of Latinx population in jail
latinx_jail_ratio_map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = latinx_jail_ratio_state),
    color = "black",
    linewidth = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "#FFE188", high = "#CE0C00", limits = c(0, 0.02)) +
  labs(fill = "Ratio") +
  theme(legend.key.size = unit(0.4, 'cm')) +
  labs(title = "Proportion of Latinx Population in Jail Compared to Total 15-64 White Population in Each State") +
  theme(plot.title = element_text(size = 12))

# Cloropleth map of ratio of Native population in jail
native_jail_ratio_map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = native_jail_ratio_state),
    color = "black",
    linewidth = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "#FFE188", high = "#CE0C00", limits = c(0, 0.02)) +
  labs(fill = "Ratio") +
  theme(legend.key.size = unit(0.4, 'cm')) +
  labs(title = "Proportion of Native Population in Jail Compared to Total 15-64 White Population in Each State") +
  theme(plot.title = element_text(size = 12))


#----------------------------------------------------------------------------#

## Load data frame ---- 


