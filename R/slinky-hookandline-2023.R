# 2023 Slinky pot and Hook-and-line Comparison Study on the AFSC Longline Survey
# July 27, 28, 29. 2023, West Yakutat
# Contact: jane.sullivan@noaa.gov

# # Acknowledgements

# Thank you to the F/V Alaskan Leader for the use of their slinky pot gear and
# to Captain Dean and his crew for their patience and expertise in
# making this pilot study a success. We are also grateful to Alexander Stubbs,
# who has generously shared his slinky pot knowledge and whose insights improved
# the design of this experiment.

# Set up ----

libs <- c("tidyverse", "fs", "readxl", "viridis", "cowplot", "zoo", "knitr", "dgof", "lemon")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

theme_set(theme_bw(base_size = 15) + 
          theme(panel.border = element_blank(), 
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(), 
                  axis.line = element_line(colour = "black")))

dat_path <- "data/llsrv_exp_2023/clean_data"
res_path <- "results/2023"
dir.create(res_path)

colors <- c('#69b3a2', '#404080')

# Get approximate pot volume:  pi * r^2 * height

# Small pots
(smpot_d <- 27 * 0.0254)
(smpot_ht <- 50 * 0.0254)
(smpot_vol <- pi * (smpot_d / 2)^2 * smpot_ht)

# Data ----
spp_lkup_lengths <- read_csv(paste0(dat_path, "/species_code_lengths_lkup.csv"))
spp_lkup_catch <- read_csv(paste0(dat_path, "/species_code_catch_lkup.csv"))

catch <- dir_ls(path = dat_path, regexp = "catch_haul_[0-9]{2,3}.csv") %>% 
  map(read_csv) %>% 
  bind_rows() %>% 
  mutate(gear = ifelse(gear == 'HAL', "Hooks", "Slinky pots"))

lengths <- dir_ls(path = dat_path, regexp = "lengths_haul_[0-9]{2,3}.csv") %>% 
  map(read_csv) %>% 
  bind_rows() %>% 
  mutate(gear = ifelse(gear == 'HAL', "Hooks", "Slinky pots"))

catch <- catch %>% 
  left_join(spp_lkup_catch) %>% 
  mutate(station = case_when(station == 201 ~ "Day 1",
                             station == 202 ~ "Day 2",
                             station == 203 ~ "Day 3"))
lengths <- lengths %>% 
  dplyr::rename(species_code2 = species_code) %>% 
  left_join(spp_lkup_lengths) %>% 
  mutate(station = case_when(station == 201 ~ "Day 1",
                             station == 202 ~ "Day 2",
                             station == 203 ~ "Day 3"))
# Reformat data ----

# the pot gear was entered using the Yuma unit, and so we had to use different
# codes to specify different things unique to pots
# EMPTY POT: species_code == 1 == 'Baited hook'
# POT DOOR OPEN: species_code == 2 == 'Ineffective hook'
# FISH STUCK IN MESH or ESCAPE RING: depredation == 1 (the count of these = the
# number of fish stuck in ring); note that the 2023 experiment we fished with
# the escape rings open (4 3.5" rings on ea pot)
# LOST POT == 172 == 'Alaskan false jingle'
# SNARLED POT = 87 == 'Blackfooted albatross'
# BROKEN OR LOST BAIT BAG == 181 == 'unsorted shark'
# EMPTY GANGION (used to identify gangions where a pot was not set) == 182 ==
# 'shark egg case'
catchsum <- catch %>% 
  group_by(gear, station, haul, skate, species, depredation) %>% 
  summarize(catch = n())

catchsum <- catchsum %>% 
  ungroup() %>% 
  # Pot "baited hook" = empty pot
  mutate(species = ifelse(gear == "Slinky pots" & species == "Baited Hook",
                          "Empty pot", species),
         catch = ifelse(gear == "Slinky pot" & species == "Empty pot",
                        0, catch)) %>% 
  # Pot "Ineffective Hook" = trap door open
  mutate(species = ifelse(gear == "Slinky pots" & species == "Ineffective Hook",
                          "Trap door open", species),
         catch = ifelse(gear == "Slinky pot" & species == "Trap door open",
                        0, catch)) %>% 
  # depredation = 1 for pots means the fish was stuck in the escape ring
  mutate(escape_ring = ifelse(gear == "Slinky pots" & depredation == 1, "Stuck in escape ring", NA)) %>% 
  # lost pot
  mutate(species = ifelse(gear == "Slinky pots" & species == "Alaskan False Jingle",
                          "Lost pot", species),
         catch = ifelse(gear == "Slinky pots" & species == "Lost pot",
                        0, catch)) %>% 
  # snarled pot
  mutate(species = ifelse(gear == "Slinky pots" & species == "Blackfooted Albatross",
                          "Snarled pot", species),
         catch = ifelse(gear == "Slinky pots" & species == "Snarled pot",
                        0, catch)) %>% 
  # bait bag broken or lost
  mutate(species = ifelse(gear == "Slinky pots" & species == "Unsorted sharks",
                          "Bait bag lost or damaged", species),
         catch = ifelse(gear == "Slinky pots" & species == "Bait bag lost or damaged",
                        0, catch)) %>% 
  # empty gangion
  mutate(species = ifelse(gear == "Slinky pots" & species == "Shark egg case",
                          "Empty gangion", species),
         catch = ifelse(gear == "Slinky pots" & species == "Empty gangion",
                        0, catch)) %>% 
  # remove the empty gangions for this analysis (only look at pots fished, not
  # gangions left intentionally empty)
  filter(species != "Empty gangion") %>% 
  # remove depredation flag for pots 
  mutate(depredation = ifelse(gear == "Slinky pots", NA, depredation))

catchsum %>% print(n=Inf)
catchsum %>% head
catchsum %>% filter(!is.na(escape_ring)) %>% summarise(sum(catch))
catchsum %>% filter(species == 'Sablefish') %>% summarise(sum(catch))
# Station table ----

# Start and end locations, number of pots or skates actually fished
station_tbl <- data.frame(Station = c("E1", "", 
                                      "E2",  "",
                                      "E3",  ""), 
                          Date = c("07/27/2023",  "", 
                                   "07/28/2023",  "", 
                                   "07/29/2023",  ""),
                          # start and end = start of set to start of haul (first buoy set to first buoy hauled)
                          start_time = c(642, 500,
                                         617, 500,
                                         607, 445),
                          end_time = c(938, 1405,
                                       915, 1342,
                                       905, 1318),
                          Gear = c("Hook-and-line", "Slinky pots",
                                   "Hook-and-line", "Slinky pots",
                                   "Hook-and-line", "Slinky pots"),
                          # Positions are the start and end locations during setting.
                          start_lat = c("5934.85 N", "5930.43 N",
                                        "5731.06 N", "5734.19 N",
                                        "5931.82 N", "5930.50 N"),
                          start_lon = c("14252.22 W", "14256.81 W",
                                        "14309.67 W", "14257.94 W",
                                        "14314.45 W", "14317.52 W"),
                          end_lat = c("5931.60 N", "5935.15 N",
                                      "5733.14 N", "5731.42 N",
                                      "5930.43 N", "5932.55 N"),
                          end_lon = c("14259.61 W", "14250.85 W",
                                      "14302.27 W", "14304.71 W",
                                      "14322.74 W", "14308.87 W")) %>% 
  mutate(soak = round((end_time - start_time) / 100, 1)) %>% 
  select(Station, Date, Gear, `Start Time` = start_time, `End Time` = end_time, 
         `Soak Time (hr)` = soak, `Start Latitude` = start_lat, `Start Longitude` = start_lon,
         `End Latitude` = end_lat, `End Longitude` = end_lon) %>% 
  bind_cols(catch %>% 
              rename(Station = station, Gear = gear) %>% 
              group_by(Station, Gear) %>% 
              summarize(`Number of hook skates or pots fished` = length(unique(skate))) %>% 
              ungroup() %>% 
              select(`Number of hook skates or pots fished`)) %>% 
  bind_cols(catch %>% 
              rename(Station = station, Gear = gear) %>% 
              filter(depth > 0) %>% 
              group_by(Station, Gear) %>% 
              summarize(mindep = min(depth),
                        maxdep = max(depth)) %>% 
              ungroup() %>% 
              select(`Minimum Depth (m)` = mindep,
                     `Maximum Depth (m)` = maxdep))

station_tbl #%>% kable()
write_csv(station_tbl, paste0(res_path, '/station_tbl.csv'))

# Sablefish catch rates ----

omit_pot <- catchsum %>% 
  filter(gear == "Slinky pots" & species %in% c("Trap door open", "Bait bag lost or damaged", "Lost pot")) %>%
  distinct(station, haul, skate, species) %>% 
  mutate(skate_id = paste(station, haul, skate, sep = "_"))

omit_pot %>%
  rename(pot = skate, reason = species) %>%
  write_csv(paste0(res_path, '/pots_omitted_from_catchrate_analysis.csv'))

# Maybe for future studies consider omiting 'ineffective skates' using the same
# definition as in the RPN analysis (omit skates with more than 15 ineffective
# hooks) 
catchsum %>% filter(gear == 'Hooks' & grepl('Ineffective', species)) %>% filter(catch > 15)

catchsum 

catchrates <- catch %>% 
  mutate(skate_id = paste(station, haul, skate, sep = "_")) %>% 
  filter(!c(skate_id %in% omit_pot$skate_id)) %>%
  group_by(skate_id, station, gear, skate, .drop = FALSE) %>%
  summarize(n_sablefish = length(which(species == "Sablefish"))) %>% 
  ungroup()

catchrates %>%
  select(station, gear, skate_or_pot_unique_id = skate_id, skate_or_pot_number = skate, n_sablefish) %>%
  write_csv(paste0(res_path, '/detailed_sablefish_catchrates.csv'))

catchrates %>% 
  ggplot(aes(x = gear, y = n_sablefish)) +
  geom_boxplot() +
  labs(x = NULL, y = "Number of sablefish") +
  facet_wrap(~station)

catchrates %>% 
  group_by(gear) %>% 
  summarise(mean = mean(n_sablefish),
            cv = round(sd(n_sablefish)/mean(n_sablefish),1))

catchrates %>% 
  group_by(gear, station) %>% 
  summarise(mean = round(mean(n_sablefish), 1),
            cv = round(sd(n_sablefish)/mean(n_sablefish), 1))

catchrates %>% 
  group_by(gear, station) %>% 
  summarize(n_sablefish = sum(n_sablefish))

catchrates %>% 
  group_by(gear) %>% 
  summarize(n_sablefish = sum(n_sablefish))

p1 <- catchrates %>% 
  filter(gear == "Hooks") %>% 
  ggplot(aes(x = station, y = n_sablefish)) +
  expand_limits(y = 0) +
  geom_boxplot() +
  labs(x = NULL, y = NULL, title = "Sablefish per skate")
  
p2 <- catchrates %>% 
  filter(gear == "Slinky pots") %>% 
  ggplot(aes(x = station, y = n_sablefish)) +
  geom_boxplot() +
  expand_limits(x = 0) +
  labs(x = NULL, y = NULL, title = "Sablefish per pot")
  
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

write_csv(catchrates, paste0(res_path, '/2023_catchrates_clean.csv'))

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

ggplot() +
  # geom_histogram(data = catchrates %>% 
  #                  filter(gear == 'Hooks') %>% 
  #                  group_by(gear, station, depth) %>% 
  #                  summarize(n_sablefish = sum(n_sablefish)), 
  #                  aes(x = depth, y = ..density.., fill = 'Hooks'), alpha = 0.6, stat = "identity") +
  # geom_histogram(data = catchrates %>% 
  #                  filter(gear == 'Slinky pots') %>% 
  #                  group_by(gear, station, depth) %>% 
  #                  summarize(n_sablefish = sum(n_sablefish)), 
  #                  aes(x = depth, y = -..density.., fill = 'Slinky pots'), alpha = 0.6, stat = "identity") +
  geom_bar(data = catchrates %>%
                   filter(gear == 'Hooks') %>%
                   group_by(gear, station, depth) %>%
                   summarize(n_sablefish = sum(n_sablefish)),
                   aes(x = depth, y = n_sablefish, fill = 'Hooks'), alpha = 0.6, stat = "identity") +
  geom_bar(data = catchrates %>%
                   filter(gear == 'Slinky pots') %>%
                   group_by(gear, station, depth) %>%
                   summarize(n_sablefish = sum(n_sablefish)),
                   aes(x = depth, y = -n_sablefish, fill = 'Slinky pots'), alpha = 0.6, stat = "identity") +
  # geom_density(data = hal, aes(x = length, y = ..density.., color = 'Hook-and-line'), fill = NA) +
  # geom_histogram(data = pot, aes(x = length, y = -..density.., fill = 'Slinky pots'), alpha=0.6) +
  # geom_density(data = pot, aes(x = length, y = -..density.., color = 'Slinky pots'), fill = NA) +
  labs(x="Depth (m)",y='Number of sablefish', color = NULL, fill = NULL) +
  facet_grid(~station, scales = 'free') +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)

ggsave(filename = paste0(res_path, "/catch_by_depth.png"), dpi = 400, units = "in",
       height = 4.5, width = 9)

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
  filter(!is.na(`Hooks`) & !is.na(`Slinky pots`)) %>% 
  group_by(station) %>%
  summarize(cor = cor(`Hooks`, `Slinky pots`, method = "pearson"))

cor_sta_dep <- cor_sta_dep %>% 
  left_join(std_sta_dep %>% 
              group_by(station) %>%               
              summarize(x = 600, #min(depth) + 25,
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
  scale_color_manual(values = colors) +
  facet_wrap(~ station) + #, scales = "free_y"
  labs(x = "Depth (m)", y = "Standardized catch rates by gear",
       col = NULL, lty = NULL, shape = NULL)

cor_dep_p1

ggsave(plot = cor_dep_p1,
       filename = paste0(res_path, "/std_catchrates_station_depth.png"),
       dpi = 400, units = "in",
       height = 3.5, width = 8)

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
  filter(!is.na(`Hooks`) & !is.na(`Slinky pots`)) %>% 
  summarize(cor = cor(`Hooks`, `Slinky pots`, method = "pearson"))

cor_dep_p2 <- std_dep %>% 
  ggplot(aes(x = depth, y = std, col = gear, shape = gear, lty = gear)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  # scale_color_grey() +
  # scale_color_manual(values = colors) +
  annotate(geom = "text", x = 600, y = 1.8, 
           label = paste0("Pearson's rho: ", round(cor_dep$cor, 2))) +
  labs(x = "Depth (m)", y = "Standardized catch rates",
       col = NULL, lty = NULL, lty = NULL, shape = NULL) +
  theme_bw(base_size = 15) + 
  scale_color_manual(values = colors) +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
cor_dep_p2

ggsave(plot = cor_dep_p2,
       filename = paste0(res_path, "/std_catchrates_depth.png"),
       dpi = 400, units = "in", height = 3, width = 6.5)

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
  group_by(gear) %>% 
  summarize(n_lengths = n()) %>% 
  bind_cols(catchsum %>% 
              filter(species == 'Sablefish') %>% 
              group_by(gear) %>% 
              summarize(n_catch = sum(catch)) %>%
              ungroup() %>% 
              select(n_catch)) %>% 
  ungroup()
totn
# write_csv(totn, paste0(res_path, "/totn_lengths_catch.csv"))

samplesizes <- samplesizes %>% 
  filter(!c(species %in%c("Baited Hook", "Empty pot", "Ineffective Hook", "Trap door open"))) 

samplesize_tbl <- samplesizes %>% 
  mutate(n_lengths = replace_na(n_lengths , 0),
         n_catch = replace_na(n_catch, 0),
         n = paste0(formatC(n_lengths, format = "d", big.mark = ","), " / ", formatC(n_catch, format = "d", big.mark = ","))) %>% 
  arrange(station, desc(n_lengths)) %>% 
  select(-n_lengths, -n_catch) %>% 
  pivot_wider(names_from = gear, values_from = n, values_fill = "0 / 0") %>% 
  select(Station = station, Species = species, `Hooks (# lengths / # catch)` = Hooks, `Slinky pots (# lengths / # catch)` = `Slinky pots`)

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
       subtitle = "(Fish and crustacean species only)")

sppcomp_plot

ggsave(plot = sppcomp_plot, filename = paste0(res_path, "/catch_comps.png"),
       dpi = 400, units = "in",
       height = 5, width = 8)

sppcomp %>% filter(species == "Sablefish")

# Lengths -----

lengths <- lengths %>% 
  # (from SO): matches a location in the string right in between an uppercase letter and a
  # digit. The (?<=...) is a positive lookahead that requires the presence of
  # some pattern immediately to the left of the current location, whereas
  # (?=...) is a positive lookahead that requires the presence of its pattern
  # immediately to the right of the current location
  separate(length, into = c("sex", "length"), sep = "(?<=[A-Z])(?=[[0-9])") %>% 
  mutate(length = as.numeric(length))

sablelenpots <- lengths %>% 
  filter(species == "Sablefish") %>% 
  mutate(stratum = ifelse(stratum == 5, '401-600 m', '601-800 m'))

sablelenpots %>% arrange(-length)
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

sablelenpots <- sablelenpots %>% 
  group_by(gear, station) %>% 
  mutate(mode = getmode(length),
         mean = mean(length),
         median = median(length),
         n = n()) %>% 
  ungroup() 
sablelenpots

p1 <- sablelenpots %>% 
  group_by(stratum, station) %>% 
  mutate(mode = getmode(length),
         mean = mean(length),
         sd = sd(length),
         n = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = length, col = gear, fill = gear)) +
  geom_density(alpha = 0.3, adjust = 1.2) +
  geom_vline(data = sablelenpots, 
             aes(xintercept = mean, col = gear, lty = gear), 
             size = 1) +
  labs(x = "Fork length (cm)", y = "Distribution", color = NULL, fill = NULL) +
  facet_grid(station ~ stratum) +
  guides(lty = FALSE) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) 
p1

ggsave(plot = p1, filename = paste0(res_path, '/potsize_length_comps.png'), dpi = 400, units = "in",
       height = 4, width = 6)

sablelenpots %>% 
  # filter(stratum != 4) %>% 
  group_by(gear, stratum) %>% 
  summarize(mean = mean(length),
         sd = sd(length),
         n = n())

sablelenpots

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
         filter(species == "Sablefish")

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
         filter(species == "Sablefish") %>% 
         group_by(gear, station) %>% 
         mutate(mean = round(mean(length),1)) %>% 
         ungroup(), 
       aes(x = length, col = gear, fill = gear, 
           lty = gear)) +
  geom_density(alpha = 0, adjust= 1.5, size = .8) +
  facet_wrap(~station) + #, ncol = 1
  geom_vline(aes(xintercept = mean, col = gear, lty = gear)) +
  labs(x = "Fork length (cm)", y = "Distribution", 
       color = NULL, fill = NULL, lty = NULL) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_bw(base_size = 15) + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave(paste0(res_path, "/length_comps_station.png"), dpi = 400, units = "in",
       # height = 8, width = 5)
       height = 3, width = 8)

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
  filter(species == 'Sablefish' & gear == 'Hooks') 
pot <- lengths %>% 
  mutate(sex = ifelse(sex == 'F', 'Female', 'Male')) %>% 
  filter(species == 'Sablefish' & gear == 'Slinky pots') 


ggplot() +
  geom_histogram(data = hal, aes(x = length, y = ..density.., fill = 'Hooks'), alpha=0.6) +
  geom_density(data = hal, aes(x = length, y = ..density.., color = 'Hooks'), fill = NA) +
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

# Standardize output ----

# only sablefish, giant grenadier, spiny dogfish, Pacific cod, and Greenland
# turbot are recorded by sex

lengths_clean <- lengths %>% 
  mutate(year = 2023,
         stratum = ifelse(stratum == 5, '401-600 m', '601-800 m'),
         sex = ifelse(species %in% c("Sablefish", "Giant Grenadier", "Spiny Dogfish"),
                  sex, "U"),
         date = as.Date(case_when(station == 'Day 1' ~ '2023/07/27',
                             station == 'Day 2' ~ '2023/07/28',
                             station == 'Day 3' ~ '2023/07/29')),
         station = case_when(station == 'Day 1' ~ 201,
                             station == 'Day 2' ~ 202,
                             station == 'Day 3' ~ 203),
         pot_size = ifelse(gear == 'Slinky pots', 'Small', NA),
         escape_ring = ifelse(gear == 'Slinky pots', 'None', NA)) %>% 
  mutate(sex = case_when(sex == 'M' ~ 'Male',
                         sex == 'F' ~ 'Female',
                         sex == 'U' ~ 'Unidentified')) %>% 
  select(year, date, gear, station, haul, stratum, species, sex, length, pot_size, escape_ring) %>% 
  arrange(date, station, haul)

catch_clean <- catchsum %>% 
  left_join(catch %>% 
              distinct(station, haul, skate, depth) %>% 
              group_by(station, haul, skate) %>% 
              # filter(depth == max(depth)) %>% 
              dplyr::summarise(depth = sum(depth)) %>% 
              group_by(station, haul) %>%
              mutate(depth = ifelse(depth == 0, NA, depth),
                     depth = zoo::na.approx(depth, rule = 2)) %>% 
              ungroup(),
            by = c("station", "haul", "skate"),
            multiple = "all") %>% 
  mutate(year = 2023,
         date = as.Date(case_when(station == 'Day 1' ~ '2023/07/28',
                                  station == 'Day 2' ~ '2023/07/29',
                                  station == 'Day 3' ~ '2023/07/30')),
         station = case_when(station == 'Day 1' ~ 201,
                             station == 'Day 2' ~ 202,
                             station == 'Day 3' ~ 203),
         pot_size = ifelse(gear == 'Slinky pots', 'Small', NA),
         escape_ring = ifelse(gear == 'Slinky pots', 'None', NA),
         escape_ring_notes = NA) %>% 
  select(year, date,  gear, station, haul, skate_or_pot = skate, depth, species, catch, depredation, pot_size, escape_ring, escape_ring_notes) %>% 
  arrange(date, station, haul)


lengths_clean
write_csv(lengths_clean, paste0("results/synthesis/clean_lengths_2023.csv"))

catch_clean
write_csv(catch_clean, paste0("results/synthesis/clean_catch_2023.csv"))

