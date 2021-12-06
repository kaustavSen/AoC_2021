library(tidyverse)

# Day 3: Part 1
data <- read_delim("data_03.txt", delim = " ")

get_min_max <- function(col, type = "max") {
  num_zeroes =  sum(col == 0)
  num_ones = sum(col == 1)
  
  if(type == "max")
    return(if(num_zeroes > num_ones) 0 else 1)
  else
    return(if(num_zeroes > num_ones) 1 else 0)
}

data_split <- 
  data %>% 
  separate(binary, into = paste0("bit_", seq(0, 12)), sep = "") %>% 
  select(-bit_0) %>% 
  mutate(across(.fns = as.numeric))

gamma_rate <- data_split %>% 
  map_dfc(get_min_max) %>% 
  unite(col = "num", everything(), sep = "") %>% 
  pull()

epsilon_rate <- data_split %>% 
  map_dfc(get_min_max, type = "min") %>% 
  unite(col = "num", everything(), sep = "") %>% 
  pull()

strtoi(gamma_rate, base = 2) * strtoi(epsilon_rate, base = 2)

# Day 3: Part 2
num_cols <- ncol(data_split)
n <- 1
data_oxygen <- data_split

while(n <= num_cols) {
  col_to_filter <- paste0("bit_", n)
  
  filter_val_df <- data_oxygen %>% 
    pivot_longer(cols = everything(), names_to = "bit") %>% 
    filter(bit == col_to_filter) %>% 
    count(value) %>% 
    arrange(-value)
  
  filter_val <- which.max(filter_val_df$n) %% 2
  
  data_oxygen <- data_oxygen %>% 
    filter(!!as.symbol(col_to_filter) == filter_val)
  n <- n + 1
  if(nrow(data_oxygen) == 1)
    break
}

n <- 1
data_co2 <- data_split

while(n <= num_cols) {
  col_to_filter <- paste0("bit_", n)
  
  filter_val_df <- data_co2 %>% 
    pivot_longer(cols = everything(), names_to = "bit") %>% 
    filter(bit == col_to_filter) %>% 
    count(value) %>% 
    arrange(value)
  
  filter_val <- which.min(filter_val_df$n) - 1
  
  data_co2 <- data_co2 %>% 
    filter(!!as.symbol(col_to_filter) == filter_val)
  
  n <- n + 1
  if(nrow(data_co2) == 1)
    break
}

oxygen <- data_oxygen %>% 
  unite(col = "num", everything(), sep = "") %>% 
  pull() %>% 
  strtoi(base = 2)

co2 <- data_co2 %>% 
  unite(col = "num", everything(), sep = "") %>% 
  pull() %>% 
  strtoi(base = 2)

oxygen * co2