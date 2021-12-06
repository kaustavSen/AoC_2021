library(tidyverse)

data <- read_delim("data_05.txt", delim = "->", col_names = FALSE)

data_cleaned <- 
  data %>% 
  mutate(across(.fns = str_trim)) %>%
  separate(col = "X1", into = c("x1", "y1")) %>% 
  separate(col = "X2", into = c("x2", "y2")) %>% 
  mutate(across(.fns = as.numeric)) %>%
  mutate(is_hline_vline = (x1 == x2) | (y1 == y2))

# Part 1
part_1_points <- data_cleaned %>% 
  filter(is_hline_vline) %>% 
  rowwise() %>% 
  mutate(
    x_seq = list(seq(from = x1, to = x2)),
    y_seq = list(seq(from = y1, to = y2))
  ) %>% 
  select(x_seq, y_seq) %>% 
  unnest(cols = everything()) 

part_1_points %>%   
  count(x_seq, y_seq) %>% 
  filter(n > 1) %>% 
  nrow()

# Part 2
part_2_points <- 
  data_cleaned %>% 
  filter(!is_hline_vline) %>% 
  mutate(intercept_pos = y1 - x1,
         intercept_neg = y1 + x1,
         slope = (y1 - y2) / (x1 - x2)) %>% 
  rowwise() %>% 
  mutate(x_seq = list(seq(from = x1, to = x2))) %>% 
  select(x_seq, intercept_pos, intercept_neg, slope) %>% 
  unnest(x_seq) %>% 
  mutate(y_seq = if_else(slope == 1, x_seq + intercept_pos, -x_seq + intercept_neg)) %>% 
  select(x_seq, y_seq)

bind_rows(part_1_points, part_2_points) %>% 
  count(x_seq, y_seq) %>% 
  filter(n > 1) %>% 
  nrow()
