# Purpose: To synthesize slinky pot experiments across 2021 and 2022 
# Date Created: 3/17/23
# Creator: Matthew LH. Cheng (UAF-CFOS)


# Set up ------------------------------------------------------------------
library(tidyverse)
library(here)
library(lubridate)
library(ggthemes)

# Bring in catch data
catch <- read_csv('results/synthesis/clean_catch_2021.csv') %>% 
  bind_rows(read_csv('results/synthesis/clean_catch_2022.csv'))

# Bind in length data
lens <- read_csv('results/synthesis/clean_lengths_2021.csv') %>% 
  bind_rows(read_csv('results/synthesis/clean_lengths_2022.csv'))

# Catch Rates -------------------------------------------------------------

# Get catch rates for sablefish - per 2021 analysis
sab_catchrates <- catch %>% 
  mutate(gear2 = ifelse(!is.na(pot_size), paste(pot_size, gear), gear)) %>% 
  filter(species == "Sablefish") 

# aggregated across years - catch per skate/pot across all gear types
sab_catchrates %>% 
  ggplot(aes(x = gear2, y = catch)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2, width = 0.2) +
  labs(x = NULL, y = "Number of sablefish") +
  facet_wrap(~station) +
  theme_bw()

# catch per skate/pots across all gear types and years
sab_catchrates %>% 
  ggplot(aes(x = gear2, y = catch)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2, width = 0.2) +
  labs(x = NULL, y = "Number of sablefish") +
  facet_grid(year~station) +
  theme_bw()

# now, plot slinky pots aggregated together
sab_catchrates %>% 
  ggplot(aes(x = gear, y = catch)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2, width = 0.2) +
  labs(x = NULL, y = "Number of sablefish") +
  facet_wrap(~station) +
  theme_bw()

# slinky aggregated together broken up by years
sab_catchrates %>% 
  ggplot(aes(x = gear, y = catch)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2, width = 0.2) +
  labs(x = NULL, y = "Number of sablefish") +
  facet_grid(year~station) +
  theme_bw()

