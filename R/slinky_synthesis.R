# synthesize pot experiments

library(tidyverse)

catch <- read_csv('results/synthesis/clean_catch_2021.csv') %>% 
  bind_rows(read_csv('results/synthesis/clean_catch_2022.csv'))

lens <- read_csv('results/synthesis/clean_lengths_2021.csv') %>% 
  bind_rows(read_csv('results/synthesis/clean_lengths_2022.csv'))

catch %>% 
  filter(gear == 'Slinky pots')
