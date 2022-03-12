# 2021 Slinky pot and Hook-and-line Comparison Study on the AFSC Longline Survey
# July 2021, West Yakutat
# Contact: jane.sullivan@noaa.gov

# # Acknowledgements

# Thank you to the F/V Alaskan Leader for the use of their slinky pot gear and
# to Captain Dennis Black and his crew for their patience and expertise in
# making this pilot study a success. We are also grateful to Alexander Stubbs,
# who has generously shared his slinky pot knowledge and whose insights improved
# the design of this experiment.

libs <- c("tidyverse", "fs", "readxl", "viridis", "cowplot", "zoo", "knitr", "dgof", "lemon")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

theme_set(theme_minimal(base_size = 14))

dat_path <- "data/llsrv_exp_2021/clean_data"
res_path <- "results/2021"
dir.create(res_path)

# Pot size ----

# Get approximate pot volume:  pi * r^2 * height

# Large pots
(lgpot_d <- 33 * 0.0254) # diameter m
(lgpot_ht <- 55 * 0.0254) # height/length m
(lgpot_vol <- pi * (lgpot_d / 2)^2 * lgpot_ht)

# Small pots
(smpot_d <- 27 * 0.0254)
(smpot_ht <- 50 * 0.0254)
(smpot_vol <- pi * (smpot_d / 2)^2 * smpot_ht)

# Data ----
spp_lkup_lengths <- read_csv(paste0(dat_path, "/species_code_lengths_lkup.csv"))
spp_lkup_catch <- read_csv(paste0(dat_path, "/species_code_catch_lkup.csv"))

catch <- dir_ls(path = dat_path, regexp = "catch_haul_[0-9]{2,3}.csv") %>% 
  map(read_csv) %>% 
  bind_rows() 

lengths <- dir_ls(path = dat_path, regexp = "lengths_haul_[0-9]{2,3}.csv") %>% 
  map(read_csv) %>% 
  bind_rows() 

catch <- catch %>% 
  left_join(spp_lkup_catch) %>% 
  mutate(station = case_when(station == 201 ~ "Exp_Set1",
                             station == 202 ~ "Exp_Set2",
                             station == 203 ~ "Exp_Set3"))
lengths <- lengths %>% 
  left_join(spp_lkup_lengths) %>% 
  mutate(station = case_when(station == 201 ~ "Exp_Set1",
                             station == 202 ~ "Exp_Set2",
                             station == 203 ~ "Exp_Set3"))
# Reformat data ----

# the pot gear was entered using the Yuma unit, and so we had to use different
# codes to specify different things unique to pots
# EMPTY POT: species_code == 1 == 'Baited hook'
# POT DOOR OPEN: species_code == 2 == 'Ineffective hook'
# FISH STUCK IN ESCAPE RING: depredation == 1 (the count of these = the number of
# fish stuck in ring)
# Identifying BIG POTS: species_code == 19 == 'Big skate' (all other pots are small pots)

catchsum <- catch %>% 
  group_by(gear, station, haul, skate, species, depredation) %>% 
  summarize(catch = n())

catchsum <- catchsum %>% 
  ungroup() %>% 
  # Pot "baited hook" = empty pot
  mutate(species = ifelse(gear == "POT" & species == "Baited Hook",
                          "Empty Pot", species),
         catch = ifelse(gear == "POT" & species == "Empty Pot",
                        0, catch)) %>% 
  # Pot "Ineffective Hook" = trap door open
  mutate(species = ifelse(gear == "POT" & species == "Ineffective Hook",
                          "Trap Door Open", species),
         catch = ifelse(gear == "POT" & species == "Empty Pot",
                        0, catch)) %>% 
  # depredation = 1 for pots means the fish was stuck in the escape ring
  mutate(escape_ring = ifelse(gear == "POT" & depredation == 1, "Stuck in escape ring", NA))

# Large pots denotes with species = big skate
catchsum <- catchsum %>% 
  filter(!c(gear == "POT" & species == "Big Skate")) %>% 
  left_join(catchsum %>% 
              filter(gear == "POT" & species == "Big Skate") %>% 
              distinct(station, haul, skate) %>% 
              mutate(pot_size = "Large")) %>% 
  # Define small pots
  mutate(pot_size = case_when(pot_size == "Large" ~ "Large pots",
                              gear == "HAL" ~ "Hooks", 
                              gear != "HAL" & is.na(pot_size) ~ "Small pots"))  

# remove depredation flag for pots 
catchsum <- catchsum %>% 
  mutate(depredation = ifelse(gear == "POT", NA, depredation))

# Station table ----

# Start and end locations, number of pots or skates actually fished
station_tbl <- data.frame(Station = c("Exp_Set1", "", #Exp_Set1, 
                                      "Exp_Set2",  "", #Exp_Set2,
                                      "Exp_Set3",  ""), #Exp_Set3),
                          Date = c("07/24/2021",  "", #"07/24/2021",
                                   "07/28/2021",  "", #"07/28/2021",
                                   "07/29/2021",  ""), #"07/29/2021")
                          # start and end = start of set to start of haul (first buoy set to first buoy hauled)
                          start_time = c(630, 500,
                                         605, 500,
                                         625, 505),
                          end_time = c(930, 1405,
                                       917, 1457,
                                       930, 1400),
                          Gear = c("Hook-and-line", "Slinky pots",
                                   "Hook-and-line", "Slinky pots",
                                   "Hook-and-line", "Slinky pots"),
                          # Positions are the start and end locations during setting.
                          start_lat = c("5858.91 N", "5901.47 N",
                                        "5933.25 N", "5935.03 N",
                                        "5931.62 N", "5932.30 N"),
                          start_lon = c("14118.00 W", "14115.55 W",
                                        "14258.65 W", "14252.96 W",
                                        "14316.25 W", "14310.08 W"),
                          end_lat = c("5900.69 N", "5859.71 N",
                                      "5930.86 N", "5932.31 N",
                                      "5930.07 N", "5930.37 N"),
                          end_lon = c("14110.37 W", "14120.90 W",
                                      "14306.97 W", "14257.86 W",
                                      "14324.04 W", "14315.83 W")) %>% 
  mutate(soak = round((end_time - start_time) / 100, 1)) %>% 
  select(Station, Date, Gear, `Start Time` = start_time, `End Time` = end_time, 
         `Soak Time (hr)` = soak, `Start Latitude` = start_lat, `Start Longitude` = start_lon,
         `End Latitude` = end_lat, `End Longitude` = end_lon) %>% 
  bind_cols(catch %>% 
              rename(Station = station, Gear = gear) %>% 
              mutate(Gear = ifelse(Gear == "HAL", "Hook-and-line", "Slinky pots")) %>% 
              group_by(Station, Gear) %>% 
              summarize(`Number of hook skates or pots fished` = length(unique(skate))) %>% 
              ungroup() %>% 
              select(`Number of hook skates or pots fished`)) %>% 
  bind_cols(catch %>% 
              rename(Station = station, Gear = gear) %>% 
              mutate(Gear = ifelse(Gear == "HAL", "Hook-and-line", "Slinky pots")) %>% 
              filter(depth > 0) %>% 
              group_by(Station, Gear) %>% 
              summarize(mindep = min(depth),
                        maxdep = max(depth)) %>% 
              ungroup() %>% 
              select(`Minimum Depth (m)` = mindep,
                     `Maximum Depth (m)` = maxdep))

# n pots by size. The plan was to set 5 large, 80 small, and 5 large along the
# groundline, shallow to deep. crew errors resulted in deviations from this plan
n_pot_sizes <- catchsum %>% 
  filter(gear == 'POT')%>% 
  group_by(gear, station, pot_size) %>% 
  summarize(npots = length(unique(skate))) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(station, gear), names_from = pot_size, values_from = npots) %>% 
  mutate(gear = 'Slinky pots') %>% 
  rename(Station = station, Gear = gear) %>% 
  # use data/llsrv_exp_2021/clean_data/catch_haul_xxx.csvs for hauls 98, 106,
  # and 108. Filter species_code=19 (big skate), which is the unique identifier
  # for big pot. Count the skate numbers, which are unique pot ids.
  mutate(`Set Order (shallow to deep)` = c('5 large, 77 small, 5 large',
                                                        '5 large, 79 small, 6 large',
                                                        '5 large, 80 small, 6 large'))

n_pot_sizes
write_csv(n_pot_sizes, paste0(res_path, '/pot_size_set_order.csv'))

station_tbl #%>% kable()
write_csv(station_tbl, paste0(res_path, '/station_tbl.csv'))

# Sablefish catch rates ----

omit_pot <- catchsum %>% 
  filter(gear == "POT" & species == "Trap Door Open") %>% 
  distinct(station, haul, skate) %>% 
  mutate(skate_id = paste(station, haul, skate, sep = "_"))

omit_pot %>% 
  mutate(reason = 'trap door open') %>% 
  write_csv(paste0(res_path, '/pots_omitted_from_catchrate_analysis.csv'))

# Maybe for future studies consider omiting 'ineffective skates' using the same
# definition as in the RPN analysis (omit skates with more than 15 ineffective
# hooks) 
# catchsum %>% filter(gear == 'HAL' & grepl('Ineffective', species)) %>% filter(catch > 15)

catchsum 

catchrates <- catch %>% 
  mutate(skate_id = paste(station, haul, skate, sep = "_")) %>% 
  filter(!c(skate_id %in% omit_pot$skate_id)) %>%
  group_by(skate_id, station, gear, skate, .drop = FALSE) %>%
  summarize(n_sablefish = length(which(species == "Sablefish"))) %>% 
  ungroup()

# ID big pots

big_pots <- catch %>% 
  filter(species != "Trap Door Open") %>% 
  filter(gear == "POT" & species == "Big Skate") %>% 
  distinct(station, haul, skate) %>% 
  mutate(skate_id = paste(station, haul, skate, sep = "_")) %>% 
  mutate(pot_size = "Large") %>% 
  select(-haul)


catchrates <- catchrates %>% 
  left_join(big_pots) %>% 
  mutate(pot_size = ifelse(gear == 'POT' & is.na(pot_size), 'Small', pot_size)) %>% 
  mutate(gear2 = ifelse(is.na(pot_size), 'Hook-and-line',
                        ifelse(pot_size == 'Small', 'Small pots', 'Large pots')))

catchrates %>% 
  select(station, gear, skate_or_pot_unique_id = skate_id, skate_or_pot_number = skate, n_sablefish, detailed_gear = gear2) %>%
  write_csv(paste0(res_path, '/detailed_sablefish_catchrates.csv'))

catchrates %>% 
  ggplot(aes(x = gear2, y = n_sablefish)) +
  geom_boxplot() +
  labs(x = NULL, y = "Number of sablefish") +
  facet_wrap(~station)

catchrates %>% 
  ggplot(aes(col = gear2, x = n_sablefish)) +
  stat_ecdf() 

potsize <- catchrates %>% 
  filter(!is.na(pot_size))

potsize %>% filter(gear!='POT')


ttest_potsize_catchrates <- t.test(x = potsize %>% filter(pot_size == 'Small') %>% pull(n_sablefish),
                                   y = potsize %>% filter(pot_size == 'Large') %>% pull(n_sablefish))
ttest_potsize_catchrates # sablefish catch rates not significantly different by pot size
cat(capture.output(print(ttest_potsize_catchrates)), 
           file =paste0(res_path, "/ttest_potsize_catchrates.txt"),
    sep = "\n")

catchrates %>% 
  group_by(gear2) %>% 
  summarise(mean = mean(n_sablefish),
            se = sd(n_sablefish))

catchrates %>% 
  group_by(gear, station) %>% 
  summarise(mean = round(mean(n_sablefish),1))

catchrates %>% 
  group_by(gear) %>% 
  summarize(cv = round(sd(n_sablefish)/mean(n_sablefish),1))

# Not meaningful, omited.
# ttest_gear_catchrates <- t.test(x = catchrates %>% filter(gear == 'HAL') %>% pull(n_sablefish),
#        y = catchrates %>% filter(gear == 'POT') %>% pull(n_sablefish))
# ttest_gear_catchrates # sablefish catch rates significantly different by gear type
# cat(capture.output(print(ttest_gear_catchrates)), 
#     file =paste0(res_path, "/ttest_gear_catchrates.txt"),
#     sep = "\n")

# https://www.geeksforgeeks.org/kolmogorov-smirnov-test-in-r-programming/
# https://www.itl.nist.gov/div898/handbook/eda/section3/eda35g.htm
# res <- dgof::ks.test(x = potsize$n_sablefish[potsize$pot_size == "Large"],
#                      y = potsize$n_sablefish[potsize$pot_size == "Small"])

plot(ecdf(potsize$n_sablefish[potsize$pot_size == "Large"]));
plot(ecdf(potsize$n_sablefish[potsize$pot_size == "Small"]), col = 'red', add = TRUE);
plot(ecdf(catchrates$n_sablefish[catchrates$gear == "HAL"]), col = 'blue', add = TRUE)

p1 <- catchrates %>% 
  filter(gear == "HAL") %>% 
  ggplot(aes(x = station, y = n_sablefish)) +
  geom_boxplot() +
  # labs(x = NULL, y = NULL, title = "Sablefish per skate")
  labs(x = NULL, y = "Number per skate")

p2 <- catchrates %>% 
  filter(gear == "POT") %>% 
  ggplot(aes(x = station, y = n_sablefish)) +
  geom_boxplot() +
  # labs(x = NULL, y = NULL, title = "Sablefish per pot")
  labs(x = NULL, y = "Number per pot")

catch_plot <- plot_grid(p1, p2)
catch_plot
ggsave(plot = catch_plot, 
       filename = paste0(res_path, "/catch_rates_boxplot2.png"), 
       dpi = 400, units = "in", height = 4, width = 7)

# summary

crsum <- catchrates %>% 
  group_by(gear, station) %>% 
  summarize(mean = round(mean(n_sablefish),1),
            cv = round(sd(n_sablefish) / mean(n_sablefish), 1))
crsum2 <- catchrates %>% 
  group_by(gear) %>% 
  summarize(mean = round(mean(n_sablefish),1),
            cv = round(sd(n_sablefish) / mean(n_sablefish), 1))

crsum %>% 
  bind_rows(crsum2 %>% 
    mutate(station = 'All_sets_combined')) %>% 
  rename(mean_sablefish_catch_rate = mean,
         cv_sablefish_catch_rate = cv) %>% 
  arrange(gear) %>% 
  write_csv(paste0(res_path, '/mean_sablefish_catchrates.csv'))

# catch by depth 
depths <- catch %>% 
  distinct(station, haul, skate, depth) %>% 
  group_by(station, haul) %>%
  mutate(depth = ifelse(depth == 0, NA, depth),
         interp_depth = zoo::na.approx(depth, rule = 2))

catchrates <- catchrates %>% 
  left_join(depths) %>% 
  mutate(depth = plyr::round_any(interp_depth, 25, floor)) %>%
  arrange(haul, depth)  

dep_catchrate_plot <- catchrates %>% 
  group_by(gear, station, depth) %>% 
  summarize(n_sablefish = sum(n_sablefish)) %>% 
  ggplot(aes(x = depth, y = n_sablefish)) +
  geom_bar(stat = "identity") +
  facet_grid(gear ~ station, scales = "free_y") +
  labs(x = "Depth (m)", y = "Number of sablefish")

dep_catchrate_plot

ggsave(plot = dep_catchrate_plot,
       filename = paste0(res_path, "/depth_catch_rates.png"), 
       dpi = 400, units = "in", height = 4, width = 7)

# standardized catchrates by station and depth
std_sta_dep <- catchrates %>% 
  group_by(gear, station, depth) %>%
  summarize(n_sablefish = sum(n_sablefish)) %>%
  group_by(gear, station) %>%
  mutate(std = (n_sablefish - mean(n_sablefish)) / sd(n_sablefish)) 

cor_sta_dep <- std_sta_dep %>% 
  ungroup() %>% 
  arrange(gear, station, depth) %>% 
  select(-n_sablefish) %>% 
  pivot_wider(names_from = gear, values_from = std) %>% 
  filter(!is.na(HAL) & !is.na(POT)) %>% 
  group_by(station) %>%
  summarize(cor = cor(HAL, POT, method = "pearson"))

cor_sta_dep <- cor_sta_dep %>% 
  left_join(std_sta_dep %>% 
              group_by(station) %>%               
              summarize(x = 425, #min(depth) + 25,
                        y = 1.8)) %>% #max(std - 1))) %>% 
  mutate(corlab = paste0("Pearson's = ", round(cor, 2)))

cor_dep_p1 <- ggplot() +
  geom_point(data = std_sta_dep, 
             aes(x = depth, y = std, col = gear, shape = gear, lty = gear)) +
  geom_line(data = std_sta_dep, 
            aes(x = depth, y = std, col = gear, shape = gear, lty = gear)) +
  geom_text(data = cor_sta_dep, aes(x = x, 
                                    y = y,
                                    label = corlab),
            size = 3) +
  facet_wrap(~ station) + #, scales = "free_y"
  labs(x = "Depth (m)", y = "Standardized catch rates by gear",
       col = NULL, lty = NULL, shape = NULL)

cor_dep_p1

# ggsave(plot = cor_dep_p1,
#        filename = paste0(res_path, "/std_catchrates_station_depth.png"), 
#        dpi = 400, units = "in",
#        height = 3.5, width = 8)

# standardized catch rates by depths
std_dep <- catchrates %>% 
  group_by(gear,  depth) %>% 
  summarize(n_sablefish = sum(n_sablefish)) %>%
  group_by(gear) %>% 
  mutate(std = (n_sablefish - mean(n_sablefish)) / sd(n_sablefish))# %>% 

cor_dep <- std_dep %>% 
  ungroup() %>% 
  arrange(gear, depth) %>% 
  select(-n_sablefish) %>% 
  pivot_wider(names_from = gear, values_from = std) %>% 
  filter(!is.na(HAL) & !is.na(POT)) %>% 
  summarize(cor = cor(HAL, POT, method = "pearson"))

cor_dep_p2 <- std_dep %>% 
  mutate(gear = ifelse(gear == "HAL", "Hook-and-line", "Slinky pots")) %>% 
  ggplot(aes(x = depth, y = std, col = gear, shape = gear, lty = gear)) +
  geom_point() +
  geom_line() +
  scale_color_grey() +
  annotate(geom = "text", x = 450, y = 1.8, 
           label = paste0("Pearson's rho: ", round(cor_dep$cor, 2))) +
  labs(x = "Depth (m)", y = "Standardized catch rates by gear",
       col = NULL, lty = NULL, lty = NULL, shape = NULL)

cor_dep_p2

ggsave(plot = cor_dep_p2,
       filename = paste0(res_path, "/std_catchrates_depth.png"),
       dpi = 400, units = "in", height = 4, width = 6)

# SRRE split ----

# Sample sizes of catch and length samples by set

# catches are not recorded to species level for rougheye and shortraker. use
# lengths to split their catch
srre <- lengths %>% 
  filter(grepl("Shortraker|Rougheye", species)) %>% 
  
  group_by(gear, station, haul, species) %>% 
  summarise(nlen = n()) %>% 
  pivot_wider(names_from = species, values_from = nlen, values_fill = 0) %>% 
  left_join(catchsum %>% 
              filter(grepl("Shortraker|Rougheye", species)) %>% 
              group_by(gear, station, haul, species) %>% 
              summarise(catch = sum(catch))) %>% 
  # make sure lengths add up to catch - yes!
  mutate(testn = ifelse(`Shortraker Rockfish`+`Rougheye Rockfish` == catch, 1, 0)) 
unique(srre$testn) == 1 # should be TRUE

srre <- srre %>%
  select(-testn, -haul) %>% 
  select(gear, station, haul, `Rougheye Rockfish`, `Shortraker Rockfish`) %>%
  pivot_longer(cols = c(`Rougheye Rockfish`, `Shortraker Rockfish`),
               names_to = "species", values_to = "catch")

# Catch N ----

samplesizes <- lengths %>% 
  # mutate(gear2 = case_when(gear == "HAL" ~ "HAL",
  #                          pot_size == "Large" ~ "LPOT",
  #                          pot_size == "Small" ~ "SPOT")) %>% 
  group_by(gear, station, species) %>% 
  summarize(n_lengths = n()) %>% 
  full_join(catchsum %>% 
              bind_rows(srre) %>% 
              filter(species != "Rougheye/Shortraker Rockfish") %>% 
              group_by(gear, station, species) %>% 
              summarize(n_catch = sum(catch))) %>% 
  ungroup()

totn <- lengths %>% 
  filter(species == 'Sablefish') %>% 
  group_by(gear, pot_size) %>% 
  summarize(n_lengths = n()) %>% 
  bind_cols(catchsum %>% 
              filter(species == 'Sablefish') %>% 
              group_by(gear, pot_size) %>% 
              summarize(n_catch = sum(catch)) %>%
              ungroup() %>% 
              select(n_catch)) %>% 
  ungroup()
totn
write_csv(totn, paste0(res_path, "/totn_lengths_catch.csv"))

samplesizes <- samplesizes %>% 
  filter(!c(species %in%c("Baited Hook", "Empty Pot", "Ineffective Hook", "Trap Door Open"))) 

samplesize_tbl <- samplesizes %>% 
  mutate(n_lengths = replace_na(n_lengths , 0),
         n_catch = replace_na(n_catch, 0),
         n = paste0(formatC(n_lengths, format = "d", big.mark = ","), " / ", formatC(n_catch, format = "d", big.mark = ","))) %>% 
  arrange(station, desc(n_lengths)) %>% 
  select(-n_lengths, -n_catch) %>% 
  pivot_wider(names_from = gear, values_from = n, values_fill = "0 / 0") %>% 
  select(Station = station, Species = species, `Hook-and-line (# lengths / # catch)` = HAL, `Slinky pots (# lengths / # catch)` = POT)

samplesize_tbl
write_csv(samplesize_tbl, paste0(res_path, "/species_n_lengths_catch.csv"))

# Species comp ----
sppcomp <- catchsum %>% 
  bind_rows(srre) %>% 
  filter(!c(species %in% c("Rougheye/Shortraker Rockfish",
                           "Baited Hook", "Ineffective Hook",
                           "Trap Door Open", "Empty Pot",
                           "Lips or Jaws - Whale Depredation", #))) %>% #,
                           "Brittle Star", "Sea Whip",
                           "Sea Cucumber", "Brittle Starfish"))) %>%
  group_by(gear, station, species) %>% 
  summarize(n = sum(catch)) %>% 
  group_by(gear, station) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(Species = fct_reorder(species, prop, .desc = TRUE))

sppcomp %>% group_by(gear, station) %>% summarize(t=sum(prop)) %>% pull(t)    

sppcomp
write_csv(sppcomp, paste0(res_path, "/species_composition.csv"))

sppcomp_plot <- sppcomp %>% 
  ggplot(aes(x = gear, col = Species, fill = Species, y = prop)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~station) + #, scales = 'free_y') +
  scale_fill_viridis(discrete = TRUE, option = 'A', direction = -1) +
  scale_colour_viridis(discrete = TRUE, option = 'A', direction = -1) +
  labs(x = "Gear type", y = "Proportion by number", 
       title = "Hook vs. slinky pot catch composition",
       # subtitle = "2021 AFSC longline survey")
       subtitle = "(Fish and crustacean species only)")

sppcomp_plot

ggsave(plot = sppcomp_plot, filename = paste0(res_path, "/catch_comps.png"),
       dpi = 400, units = "in",
       height = 5, width = 8)

sppcomp %>% filter(species == "Sablefish")

# Lengths -----

# big vs. sm pots:

lengths <- lengths %>% 
  # (from SO): matches a location in the string right in between an uppercase letter and a
  # digit. The (?<=...) is a positive lookahead that requires the presence of
  # some pattern immediately to the left of the current location, whereas
  # (?=...) is a positive lookahead that requires the presence of its pattern
  # immediately to the right of the current location
  separate(length, into = c("sex", "length"), sep = "(?<=[A-Z])(?=[[0-9])") %>% 
  mutate(length = as.numeric(length),
         gear = ifelse(gear == "HAL", "Hook-and-line", "Slinky pots"))

sablelenpots <- lengths %>% 
  filter(gear == "Slinky pots" & species == "Sablefish")

lenres <- dgof::ks.test(x = sablelenpots$length[sablelenpots$pot_size == "Large"],
                  y = sablelenpots$length[sablelenpots$pot_size == "Small"])
lenres # length distributions statistically different between pot sizes
cat(capture.output(print(lenres)), 
    file =paste0(res_path, "/kstest_potsize_lengths.txt"),
    sep = "\n")

lenres <- t.test(x = sablelenpots$length[sablelenpots$pot_size == "Large"],
                  y = sablelenpots$length[sablelenpots$pot_size == "Small"])
lenres # mean lengths statistically different between pot sizes
cat(capture.output(print(lenres)), 
    file =paste0(res_path, "/ttest_potsize_lengths.txt"),
    sep = "\n")

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

sablelenpots <- sablelenpots %>% 
  group_by(pot_size) %>% 
  mutate(mode = getmode(length),
         mean = mean(length),
         median = median(length),
         n = n()) %>% 
  ungroup() %>% 
  mutate(pot_size = ifelse(pot_size == "Large", "Large slinky pots", "Small slinky pots"))

sablelenpots

p1 <- sablelenpots %>% 
  group_by(pot_size, stratum) %>% 
  mutate(mode = getmode(length),
         mean = mean(length),
         sd = sd(length),
         n = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = length, col = pot_size, fill = pot_size)) +
  geom_density(alpha = 0.2, adjust = 1.2) +
  geom_vline(data = sablelenpots, aes(xintercept = median, col = pot_size)) +
  labs(x = "Fork length (cm)", y = "Density", color = NULL, fill = NULL) +
  facet_wrap(~stratum, ncol = 1)
p1

# ggsave(plot = p1, filename = "results/2021/potsize_length_comps.png", dpi = 400, units = "in",
#        height = 4, width = 6)

sablelenpots %>% 
  filter(stratum != 4) %>% 
  group_by(pot_size, stratum) %>% 
  summarize(mean = mean(length),
         sd = sd(length),
         n = n())

sablelenpots

p1 <- sablelenpots %>% 
  ggplot(aes(x = length, col = pot_size, fill = pot_size)) +
  geom_density(alpha = 0.2, adjust = 1.2) +
  geom_vline(data = sablelenpots, aes(xintercept = median, col = pot_size)) +
  labs(x = "Fork length (cm)", y = "Density", color = NULL, fill = NULL)
p1
ggsave(plot = p1, filename = paste0(res_path, "/potsize_length_comps.png"),
       dpi = 400, units = "in",
       height = 4, width = 6)

# Lengths: pot vs hal
lenres2 <- dgof::ks.test(x = lengths$length[lengths$gear == "Slinky pots" &
                                        lengths$species == "Sablefish"],
                  y = lengths$length[lengths$gear == "Hook-and-line" & 
                                       lengths$species == "Sablefish"])
lenres2 # length distributions of combined pot and hal not statistically different
cat(capture.output(print(lenres2)), 
    file =paste0(res_path, "/kstest_gear_lengths.txt"),
    sep = "\n")

lenres2 <- t.test(x = lengths$length[lengths$gear == "Slinky pots" &
                                        lengths$species == "Sablefish"],
                   y = lengths$length[lengths$gear == "Hook-and-line" & 
                                        lengths$species == "Sablefish"])
lenres2 # mean lengths of combined pot and hal not statistically different
cat(capture.output(print(lenres2)), 
    file =paste0(res_path, "/ttest_gear_lengths.txt"),
    sep = "\n")


# Plot lengths
newlengths <- lengths %>%
         filter(species == "Sablefish") %>% # & c(is.na(pot_size) | pot_size == "Small")) %>% 
         mutate(gear = ifelse(is.na(pot_size), gear, 
                              ifelse(pot_size == "Small", "Small slinky pots",
                                     "Large slinky pots"))) 

newlengths %>% 
  distinct(station, gear, stratum) %>% 
  arrange(station, stratum)

lengthsum <- newlengths %>% 
         group_by(gear) %>% 
         summarize(mean = round(mean(length),1),
                   SD = round(sd(length),1),
                   n = n()) %>% 
         ungroup() %>% 
         mutate(gear2 = paste0(gear, ", n=", n, ", mean=", mean, " cm ", "SD=", SD))

lengthsum

# Exploring length by depth strata
ggplot(newlengths %>% 
         group_by(gear, depth_stratum = stratum) %>% 
         mutate(median = median(length),
                mean = round(mean(length),1),
                SD = round(sd(length),1),
                n = n()) %>% 
         ungroup() %>% 
         # mutate(gear = paste0(gear, ", n=", n, ", median=", median, " cm"))) +
         # mutate(gear = paste0(gear, ", n=", n, ", mean=", mean, " cm, SD=",SD))) +
         mutate(gear = paste0(depth_stratum, " ", gear, ", n=", n, ", mean=", mean, 
                              " cm, SD=",SD))) +
  geom_density(aes(x = length, col = gear, fill = gear),
               alpha = 0.2, adjust = 1.5) +
  geom_vline(aes(xintercept = mean, col = gear), lty = 2) +
  labs(x = "Fork length (cm)", y = "Density", color = NULL, fill = NULL) +
  facet_wrap(~depth_stratum)
# 
# ggsave(paste0(res_path, "/length_comps2.png"), dpi = 400, units = "in",
#        height = 4, width = 7)

ggplot(lengths %>% 
         filter(species == "Sablefish"), 
       aes(x = length, col = gear, fill = gear)) +
  geom_density(alpha = 0.2, adjust= 1.5) +
  facet_wrap(~station, ncol = 1) +
  labs(x = "Fork length (cm)", y = "Density", color = NULL, fill = NULL)

ggsave(paste0(res_path, "/length_comps_station.png"), dpi = 400, units = "in",
       height = 8, width = 5)

# station/sex lengths 
sslen <- lengths %>% 
  mutate(sex = ifelse(sex == 'F', 'Female', 'Male')) %>% 
  filter(species == "Sablefish")

lengthsum <- sslen %>% 
  group_by(gear,station,sex) %>% 
  summarize(mean = round(mean(length),1),
            SD = round(sd(length),1),
            n = n()) %>% 
  ungroup() %>% 
  mutate(gear2 = paste0(gear, ", n=", n, ", mean=", mean, " cm, ", "SD=", SD))

lengthsum
length_plot <- ggplot(sslen %>% 
                        left_join(lengthsum), 
                      aes(x = length, col = gear, fill = gear)) +
  geom_density(alpha = 0.2, adjust= 1.5) +
  geom_vline(aes(xintercept = mean, col = gear, lty = gear)) +
  # scale_colour_manual(values = c('darkgrey', 'black')) +
  # scale_fill_manual(values = c('grey', 'white')) +
  # scale_colour_grey() +
  # scale_fill_grey() +
  scale_linetype_manual(values = c(2,3))+
  # facet_grid(station~sex) +
  facet_grid(sex~station, scales = 'free_y') +
  labs(x = "Fork length (cm)", y = "Density", color = NULL, fill = NULL, lty = NULL) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom')
length_plot

ggsave(plot = length_plot, filename = paste0(res_path, "/length_comps_sex_station.png"),
       dpi = 400, units = "in",
       height = 6, width = 8)

lengthsum <- sslen %>% 
  group_by(gear) %>% 
  summarize(median = round(median(length),2),
            mean = round(mean(length),2),
            SD = round(sd(length),2),
            n = n()) %>% 
  ungroup() %>% 
  mutate(gear2 = paste0(gear, ", n=", n, ", mean=", mean, " cm, ", "SD=", SD))

lengthsum

# ecdf
ggplot(lengths %>% 
         mutate(sex = ifelse(sex == 'F', 'Female', 'Male')) %>% 
         filter(species == "Sablefish"& (is.na(pot_size) | pot_size == 'Small')), #%>% 
         # group_by(sex, station) %>% 
         # mutate(ecdf = ecdf(length)[1]), 
       aes(x = length, col = gear, fill = gear)) +
  stat_ecdf() +
  # geom_line(aes(y=cumsum(length)/sum(length)))+
  facet_grid(station~sex) +
  labs(x = "Fork length (cm)", y = "Empirical density function", color = NULL, fill = NULL)


hal <- lengths %>% 
  mutate(sex = ifelse(sex == 'F', 'Female', 'Male')) %>% 
  filter(species == 'Sablefish' & gear == 'Hook-and-line') 
pot <- lengths %>% 
  mutate(sex = ifelse(sex == 'F', 'Female', 'Male')) %>% 
  filter(species == 'Sablefish' & gear == 'Slinky pots') 
colors <- c("Hook-and-line" = "#69b3a2", "Slinky pots" = "#404080")

ggplot() +
  geom_histogram(data = hal, aes(x = length, y = ..density.., fill = 'Hook-and-line'), alpha=0.6) +
  geom_density(data = hal, aes(x = length, y = ..density.., color = 'Hook-and-line'), fill = NA) +
  # geom_label( aes(x=4.5, y=0.25, label="variable1"), color="#69b3a2") +
  geom_histogram(data = pot, aes(x = length, y = -..density.., fill = 'Slinky pots'), alpha=0.6) +
  geom_density(data = pot, aes(x = length, y = -..density.., color = 'Slinky pots'), fill = NA) +
  # geom_label( aes(x=4.5, y=-0.25, label="variable2"), color="#404080") +
  labs(x="Fork length (cm)",y='Density', color = NULL, fill = NULL) +
  facet_grid(station~sex, scales = 'free') +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)

ggsave(filename = paste0(res_path, "/length_comps_sex_station2.png"), dpi = 400, units = "in",
       height = 6, width = 8)

# sex ratios----

lengths %>% 
  filter(species == 'Sablefish') %>% 
  group_by(station, gear, sex) %>% 
  summarize(n = n()) %>% 
  group_by(station, gear) %>% 
  mutate(sex_ratio = n / sum(n)) %>% 
  ggplot(aes(x = gear, y = sex_ratio, col = sex, fill = sex)) +
  geom_bar(stat = 'identity', alpha = 0.3) +
  facet_wrap(~ station) +
  labs(x = NULL, y = "Sex ratio", col = "Sex", fill = "Sex")

length_tbl <- lengths %>% 
  filter(species == 'Sablefish') %>% 
  group_by(station, gear, sex) %>% 
  summarize(n = n()) %>% 
  group_by(station, gear) %>% 
  mutate(prop_f = n / sum(n)) %>% 
  filter(sex == "F") %>% 
  select(-c(sex, n)) %>% 
  left_join(lengths %>% 
              filter(species == 'Sablefish') %>% 
              group_by(station, gear) %>% 
              summarize(n = n(),
                        mean_length = mean(length),
                        se_length = sd(length) / sqrt(n))) %>% 
  mutate(sumlen = paste0(formatC(mean_length, digits = 1, format = "f"), 
                         " (", formatC(se_length, digits = 2, format = "f"), ")"),
         n = formatC(n, format = "d", big.mark = ","),
         prop_f = formatC(prop_f, digits = 2, format = "f")) %>% 
  select(Station = station, Gear = gear,  N = n, 
        `Proportion female` = prop_f,
        `Mean (SE) fork length (cm)` = sumlen)

length_tbl
write_csv(length_tbl, paste0(res_path, "/sablefish_length_sexratio_summary.csv"))

# Grenadier length comps ----
ggplot(lengths %>% 
         filter(species == "Giant Grenadier"), 
       aes(x = length, col = gear, fill = gear)) +
  geom_density(alpha = 0.2, adjust = 1.5) +
  labs(x = "Fork length (cm)", y = "Density", color = NULL, fill = NULL)

ggsave(paste0(res_path, "/giantgrenadier_length_comps.png"), dpi = 400, units = "in",
       height = 4, width = 6)

# Future considerations ----

catchrates_sum <- catchrates %>% 
  group_by(gear, station) %>% 
  summarize(mean_rate = mean(n_sablefish)) %>% 
  pivot_wider(names_from = gear, values_from = mean_rate) %>% 
  mutate(`HAL:POT` = HAL / POT) %>% 
  # average, all data combined
  bind_rows(catchrates %>% 
              group_by(gear) %>% 
              summarize(mean_rate = mean(n_sablefish)) %>% 
              pivot_wider(names_from = gear, values_from = mean_rate) %>% 
              mutate(`HAL:POT` = HAL / POT,
                     station = "Average")) %>% 
  # distances in m
  mutate(n_skates = 90,
         n_pot = 90,
         pot_space = 40 * 1.8288,
         pot_dist = n_pot * pot_space,
         n_pot = `HAL:POT` * n_skates,
         # new_pot_space = pot_dist / n_pot,
         skate_space = 100, # 55 * 1.8288,
         hook_dist = skate_space * n_skates,
         new_pot_space = hook_dist / n_pot,
         new_space_fm = new_pot_space / 1.8288) %>% 
  mutate(HAL = formatC(HAL, digits = 1, format = "f"),
         POT = formatC(POT, digits = 1, format = "f"),
         `HAL:POT` = formatC(`HAL:POT`, digits = 1, format = "f"),
         n_pot = paste0(round(n_pot, 0)),
         new_pot_space = paste0(round(new_pot_space, 0)),
         new_space_fm = paste0(round(new_space_fm, 0))) 

rec_tbl <- catchrates_sum %>% 
  select(Station = station, `Hook-and-line catch rate (# per skate)` = HAL,
         `Slinky pot catch rate (# per pot)` = POT, `Hook-and-line catch rate / Slinky pot catch rate` = `HAL:POT`,
         `Equivalent # of slinky pots` = n_pot, `Equivalent pot spacing (m)` = new_pot_space,
         `Equivalent pot spacing (fm)` = new_space_fm)# %>% 
  # kable()
rec_tbl
write_csv(rec_tbl, paste0(res_path, "/potspacing_recommendations.csv"))

# One key uncertainty when designing the pilot study was determining the number of slinky pots and pot spacing needed to provide an equivalent comparison to the standard set of 90 hook-and-line skates along a 9,000 m groundline. To inform future studies, we used the following equations to estimate the number of pots and pot spacing needed to equal a single set of the standard longline survey gear:
#   
#   *Number of pots = (Hook-and-line catch rate / Slinky pot catch rates) x (Number of hook-and-line skates)*
#   
#   *Pot spacing = Hook-and-line groundline / Number of pots*
#   
#   Results from this exercise suggest that an equivalent number of slinky pots to 90 hook-and-line skates ranges between 154 and 222 (average = 169) pots (Table 4). Interestingly, experimental sets 2 and 3 converged on roughly the same equivalency ratio of hook-and-line to slinky pots (1.7). Given that experimental set 1 was the first time this vessel had fished slinky pot gear, it's possible that the results from this set may be an overestimate of the equivalency ratio. Therefore, the recommended range of slinky pots for future studies is 155-170 pots with an associated pot spacing of 53-58 m (29-32 fm). 
# 
# Additional recommendations to date include the following: 
# 
# 1.  Use a single size of slinky pots with 8.9 cm escape rings; and   
# 2.  Eliminate the use of cannonballs between pots, which was found to be unnecessary.
# 
