library(tidyverse)

# Day 2: Part 1
data <- read_delim("data_02.txt", delim = " ")

final_horizontal <- 
  data %>% 
  filter(direction == "forward") %>% 
  summarise(sum = sum(value)) %>% 
  pull()

final_depth <- 
  data %>% 
  filter(direction != "forward") %>% 
  mutate(value = if_else(direction == "down", value, -value)) %>% 
  summarise(sum = sum(value)) %>% 
  pull()

final_horizontal * final_depth

# Day 2: Part 2
calc_aim <- function(curr_aim, dir, value) {
  if (dir == "forward") {
    depth <<-  depth + curr_aim * value
    return(curr_aim)
  } else if(dir == "down") {
    return(curr_aim + value)
  } else {
    return(curr_aim - value)
  }
}

depth <- 0 # starting depth
reduce2(data$direction, data$value, calc_aim, .init = 0)
depth

depth * final_horizontal
