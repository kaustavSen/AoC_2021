library(tidyverse)

# Day 1: Part 1
data <- read_csv("data_01.csv")

data %>% 
  mutate(
    readings_lag = lag(readings),
    is_increase = if_else(readings > readings_lag, "yes", "no")
  ) %>% 
  count(is_increase)

# Day 1: Part 2
data %>% 
  mutate(
    lead_0 = lead(readings, 0),
    lead_1 = lead(readings, 1),
    lead_2 = lead(readings, 2)
  ) %>% 
  rowwise() %>% 
  mutate(avg = mean(c_across(lead_0:lead_2))) %>% 
  ungroup() %>% 
  mutate(
    avg_lag = lag(avg),
    is_increase = if_else(avg > avg_lag, "yes", "no")
  ) %>% 
  count(is_increase)
