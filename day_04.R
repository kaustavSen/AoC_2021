library(tidyverse)

numbers <- c(67,3,19,4,64,39,85,14,84,93,79,26,61,24,65,63,15,69,48,8,82,75,36,96,16,49,28,40,97,38,76,91,83,7,62,94,21,95,6,10,43,17,31,34,81,23,52,60,54,29,70,12,35,0,57,45,20,71,78,44,90,2,33,68,53,92,50,73,88,47,58,5,9,87,22,13,18,30,59,56,99,11,77,55,72,32,37,89,42,27,66,41,86,51,74,1,46,25,98,80)
data_raw <- read_csv("data_04.txt", col_names = FALSE)

numbers_table <- tibble(
  draw = seq_along(numbers),
  value = numbers
)

# Part 1
data <- data_raw %>% 
  separate(col = "X1", into = paste0("num_", 1:5)) %>% 
  mutate(across(.fns = as.numeric)) %>% 
  mutate(
    board = rep(1:100, each = 5),
    row = rep(1:5, times = 100),
    .before = 1
  ) %>% 
  pivot_longer(cols = num_1:num_5, names_to = "col") %>% 
  mutate(col = str_replace(col, "num_", "") %>% as.numeric()) %>% 
  left_join(numbers_table, by = "value")

row_match <- 
  data %>% 
  group_by(board, row) %>% 
  summarise(draw = max(draw)) %>% 
  group_by(board) %>% 
  summarise(draw = min(draw)) %>% 
  mutate(match = "row")

col_match <- 
  data %>% 
  group_by(board, col) %>% 
  summarise(draw = max(draw)) %>% 
  group_by(board) %>% 
  summarise(draw = min(draw)) %>% 
  mutate(match = "col")

winning_board <- bind_rows(row_match, col_match) %>% 
  slice_min(order_by = draw, n = 1)

sum_unmarked <- data %>% 
  filter(board == winning_board$board) %>% 
  anti_join(head(numbers_table, winning_board$draw)) %>% 
  pull(value) %>% 
  sum()

sum_unmarked * numbers[winning_board$draw]

# Part 2
losing_board <- bind_rows(row_match, col_match) %>% 
  group_by(board) %>% 
  filter(draw == min(draw)) %>% 
  ungroup() %>% 
  slice_min(order_by = -draw, n = 1)

sum_unmarked <- data %>% 
  filter(board == losing_board$board) %>% 
  anti_join(head(numbers_table, losing_board$draw)) %>% 
  pull(value) %>% 
  sum()

sum_unmarked * numbers[losing_board$draw]
