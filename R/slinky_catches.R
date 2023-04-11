# Attempt to reconstruct slinky catches

libs <- c("tidyverse", "odbc", "keyring", "dbplyr")
libs
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

source('R/utils.R')
akfin <- connect(db = "akfin")

# slinky pot ----

aa <- dplyr::tbl(akfin, dplyr::sql('council.comprehensive_blend_ca')) %>% 
  dplyr::rename_with(tolower) %>% # colnames() %>% sort()
  dplyr::select(year, vessel_id, ves_akr_name, 
                ves_akr_adfg, ves_akr_length,
                #ves_akr_homeport_state,ves_owner_state,
                #adfg_stat_area_code,
                fmp_gear, trip_target_code,
                # fmp_area, 
                fmp_subarea, #reporting_area_code,
                # akr_state_federal_waters_code,
                catch_activity_date, week_end_date,
                weight_posted, retained_or_discarded,
                # monitoring_status, sampling_strata_name,
                # sampling_strata_selection_rate,
                management_program_code, #gf_harvest_sector, 
                # gf_processing_sector, cdq_flag,
                #price_ton, wholesale_value, exves_price_lb, exves_value,
                catch_report_source_pk, akfin_species_code)
                # agency_species_code)

bb <- dplyr::tbl(akfin, dplyr::sql('akr.v_ellr_report')) %>%
  dplyr::rename_with(tolower) %>% 
  dplyr::select(#landing_includes_ifq_sabl, landing_includes_ifq_hlbt,
                gear_modifier_description, catch_report_source_pk)

table <- aa %>% 
  dplyr::left_join(bb, by = 'catch_report_source_pk') %>% 
  dplyr::filter(akfin_species_code %in% c('SABL'),
                between(year, 2015, 2022),
                fmp_subarea %in% c('AI', 'BS', 'WG', 'CG', 'WY', 'SE'),
                management_program_code %in% c('IFQ','CDQ'))
# agency_species_code == '710',                 
# landing_includes_ifq_sabl == 'Y',
# landing_includes_ifq_hlbt == 'Y')

show_query(table)
slinky_full <- dplyr::collect(table)
readr::write_csv(slinky_full, 'data/confidential/sablefish_catch_20230309.csv')

slinky <- slinky_full %>%  
  # dplyr::filter(fmp_gear != 'JIG') %>% #& fmp_subarea %in% c('BS', 'AI') 
  dplyr::filter(!fmp_gear %in% c('JIG', 'TRW')) %>% 
  dplyr::mutate(#fmp_subarea = ifelse(fmp_subarea %in% c('BS', 'AI'), 'BSAI', fmp_subarea),
                week_end_date = lubridate::date(week_end_date),
                gear = case_when(fmp_gear == 'HAL' ~ 'HAL',
                                 fmp_gear == 'TRW' ~ 'TRW',
                                 # fmp_gear == 'JIG' ~ 'Jig',
                                 fmp_gear == 'POT' & gear_modifier_description  == 'Hard Pots' ~ 'POT_HARD',
                                 fmp_gear == 'POT' & gear_modifier_description == 'Slinky Pots' ~ 'POT_SLINKY',
                                 fmp_gear == 'POT' & is.na(gear_modifier_description) ~ 'POT_UNKN'),
                fmp_subarea = factor(fmp_subarea, levels = c('BS', 'AI', 'WG', 'CG',
                                                             'WY', 'SE'), ordered = TRUE))

# conservative confidentiality check
slinky %>% 
  dplyr::group_by(year, fmp_subarea) %>% 
  summarise(nvessel = length(unique(vessel_id)),
            ntrip = length(unique(catch_report_source_pk)),
            catch = sum(weight_posted)) %>% 
  filter(nvessel <= 6 | ntrip <= 6)

sum <- slinky %>% 
  dplyr::group_by(year, fmp_subarea, gear) %>% 
  dplyr::summarise(catch = sum(weight_posted)) %>%
  dplyr::group_by(year, fmp_subarea) %>% 
  dplyr::mutate(prop = catch / sum(catch))

sum %>% 
  ggplot(aes(year, catch, fill = gear)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~fmp_subarea)

sum %>% 
  ggplot(aes(year, prop, fill = gear)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~fmp_subarea, ncol = 2)


# gear_modifier_description started showing up in sept 2021:
# week_end_date == 2021-09-25
slinky %>% 
  dplyr::filter(year == 2021 & fmp_gear == 'POT') %>% 
  dplyr::group_by(week_end_date, gear_modifier_description) %>% 
  dplyr::summarise(catch = sum(weight_posted)) %>% 
  ggplot(aes(week_end_date, catch, fill = gear_modifier_description)) +
  geom_bar(stat = 'identity')

vessel_known <- slinky %>% 
  dplyr::filter(year %in% c(2021, 2022) & !is.na(gear_modifier_description)) %>% 
  dplyr::distinct(vessel_id, fmp_gear, gear) %>% 
  dplyr::rename(obsgear = gear)

dups <- vessel_known %>% 
  count(vessel_id) %>% 
  filter(n > 1) %>% 
  pull(vessel_id)

slinky %>% 
  dplyr::filter(vessel_id %in% dups & # & fmp_gear != 'HAL'
                  week_end_date >= 2021-09-25) %>% 
  dplyr::group_by(gear, vessel_id, ves_akr_name, ves_akr_length) %>% 
  dplyr::summarise(catch = sum(weight_posted)) %>% 
  tidyr::pivot_wider(id_cols = c(vessel_id, ves_akr_name, ves_akr_length), 
                     names_from = gear, values_from = catch,
                     values_fill = 0) %>% 
  dplyr::arrange(ves_akr_length)

idslinky <- slinky %>% 
  dplyr::filter(is.na(gear_modifier_description)) %>% 
  dplyr::left_join(vessel_known %>% 
                     dplyr::mutate(obsgear = ifelse(vessel_id %in% dups, 
                                                    'POT_UNKN_MIXED', 
                                                    as.character(obsgear))) %>% 
                     dplyr::distinct(),
                   by = join_by(vessel_id, fmp_gear)) %>% 
  bind_rows(slinky %>% 
              dplyr::filter(!is.na(gear_modifier_description))) %>% #) %>% mutate(obsgear = gear)
  # select(year, fmp_gear, gear, obsgear, gear_modifier_description)            
  mutate(newgear = case_when(fmp_gear == 'HAL' ~ gear,
                             fmp_gear == 'POT' & !is.na(obsgear) ~ obsgear,
                             fmp_gear == 'POT' & is.na(obsgear) ~ gear))

idslinky %>% filter(is.na(newgear)) 

sum <- idslinky %>% 
  filter(!fmp_subarea %in% c('BS', 'AI')) %>% 
  dplyr::group_by(year, fmp_subarea, newgear) %>% 
  dplyr::summarise(catch = sum(weight_posted),
                   landings = length(unique(catch_report_source_pk))) %>% 
  dplyr::group_by(year, fmp_subarea) %>% 
  dplyr::mutate(prop = catch / sum(catch),
                prop_landings = landings / sum(landings))

sum %>% 
  ggplot(aes(year, catch, fill = newgear)) +
  geom_bar(stat = 'identity') +
  ggthemes::scale_fill_colorblind() +
  facet_wrap(~fmp_subarea, scales = 'free_y')

sum %>% 
  ggplot(aes(year, prop, fill = newgear)) +
  geom_bar(stat = 'identity') +
  ggthemes::scale_fill_colorblind() +
  facet_wrap(~fmp_subarea, ncol = 2) + 
  labs(x = 'Year', y = 'Proportion', fill = 'Gear',
       title = 'Proportion of IFQ/CDQ sablefish catch by gear type')
ggsave(filename = paste0("results/synthesis/proportion_catch_by_gear.png"),
       dpi = 400, units = "in", height = 6, width = 8)

sum %>% 
  ggplot(aes(year, prop_landings, fill = newgear)) +
  geom_bar(stat = 'identity') +
  ggthemes::scale_fill_colorblind() +
  facet_wrap(~fmp_subarea, ncol = 2) + 
  labs(x = 'Year', y = 'Proportion', fill = 'Gear',
       title = 'Proportion of IFQ/CDQ sablefish landings by gear type')

sum %>% 
  # mutate(gear_description = case_when(newgear == 'HAL' ~ 'Hook-and-line',
  #                                     newgear == 'POT_HARD' ~ 'Hard/ridig pots',
  #                                     newgear == 'POT_SLINKY' ~ 'Slinky pots',
  #                                     newgear == 'POT_UNKN' ~ 'Unknown pot type',
  #                                     newgear == 'POT_UNKN_MIXED' ~ 'Unknown pot'))
  select(year, fmp_subarea, gear = newgear, catch_mt = catch,
         number_of_landings = landings, proportion_catch = prop,
         proportion_landings = prop_landings) %>% write_csv('results/synthesis/slinky_catch_summary.csv')

idslinky %>% 
  filter(newgear %in% c('POT_UNKN')) %>% 
  distinct(fmp_gear,ves_akr_name, ves_akr_length) %>% print(n=Inf)

# other queries in progress ----

# em 
dplyr::tbl(akfin, dplyr::sql('akfin_marts.comprehensive_obs_em')) %>%
  dplyr::rename_with(tolower) %>%
  colnames()

dplyr::tbl(akfin, dplyr::sql('akfin_marts.comprehensive_obs_em')) %>%
  dplyr::rename_with(tolower) %>%
  dplyr::filter(year > 2017) %>% 
  dplyr::count(year, fmp_area, agency_gear_code, em_gear_name) %>% 
  dplyr::arrange(year, agency_gear_code) %>% 
  print(n=100)

ftnames <- dplyr::tbl(akfin, dplyr::sql("council.comprehensive_ft")) %>% 
  dplyr::rename_with(tolower) %>%
  colnames()

ftnames[grepl('report_id|fish_ticket_number|catch_report_source_pk', ftnames)]

canames <- dplyr::tbl(akfin, dplyr::sql("council.comprehensive_blend_ca")) %>% 
  dplyr::rename_with(tolower) %>%
  colnames()

canames[grep('report_id|fish_ticket_number|catch_report_source_pk', canames)]
# can we link to observer data?