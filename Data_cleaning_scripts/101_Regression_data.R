# Regression data
#
# Date updated:   2023-10-02
# Auhtor:         Christian Vedel 
# Purpose:        Collects data 

# ==== Libraries ====
library(tidyverse)
library(foreach)
library(readxl)
source("Data_cleaning_scripts/000_Functions.R")

# ==== Load data ====
railways = read_csv2("Data/Panel_of_railways_in_parishes.csv", guess_max = 10000)
railways_StateTrunk = read_csv2("Data/Panels_by_type/type2_StateTrunk.csv")
railways_StateNonTrunk = read_csv2("Data/Panels_by_type/type2_StateNonTrunk.csv")
railways_Private = read_csv2("Data/Panels_by_type/type2_Private.csv")

Assembly_houses = read_csv2("Data/Panel_of_assembly_houses.csv", guess_max = 10000)
Assembly_houses_MA = read_csv2("Data/Panel_of_MA_assembly_houses.csv", guess_max = 10000)

Folk_high_schools = read_csv2("Data/Panel_of_folk_high_schools.csv", guess_max = 10000)
Folk_high_schools_MA = read_csv2("Data/Panel_of_MA_folk_high_schools.csv", guess_max = 10000)

census = read_csv2("Data/Census_data.csv", guess_max = 10000)  

geo = read_csv2("Data/Geo_info.csv", guess_max = 2000)

distance_to_nodes = read_excel("Data/distance_to_nodes.xlsx")

# ==== Join different raildata ====
railways_StateTrunk = railways_StateTrunk %>% select(-type2)
railways_StateNonTrunk = railways_StateNonTrunk %>% select(-type2)
railways_Private = railways_Private %>% select(-type2)

railways = railways %>% 
  left_join(
    railways_StateTrunk, by = c("Year", "GIS_ID"), suffix = c("","_StateTrunk")
  ) %>% 
  left_join(
    railways_StateNonTrunk, by = c("Year", "GIS_ID"), suffix = c("","_StateNonTrunk")
  ) %>% 
  left_join(
    railways_Private, by = c("Year", "GIS_ID"), suffix = c("","_Private")
  )

# ==== Load instrument ====
instrument = read_csv2("Data/Instruments/paramS_scrit_1.csv")

instrument = instrument %>% 
  rename(
    Connected_rail_instr = Connected_rail,
    Distance_to_nearest_railway_instr = Distance_to_nearest_railway
  )

# Forward imputation until 1920
tmp1901 = instrument %>% filter(Year == 1901)
years_to_add = 1902:1920
years_added = foreach(g = tmp1901$GIS_ID, .combine = "bind_rows") %do% {
  res = data.frame(
    Year = years_to_add,
    GIS_ID = g,
    Connected_rail_instr = tmp1901 %>% filter(GIS_ID == g) %>% pull(Connected_rail_instr),
    Distance_to_nearest_railway_instr = tmp1901 %>% filter(GIS_ID == g) %>% pull(Distance_to_nearest_railway_instr)
  )

  return(res)
}

instrument = instrument %>% bind_rows(years_added)

railways = railways %>% 
  left_join(instrument, by = c("GIS_ID", "Year"))

# ==== Misc small data juggling ====
pop1801 = census %>% 
  filter(Year == 1801) %>% 
  select(GIS_ID, Pop) %>% 
  rename(Pop1801 = Pop)

pop1787 = census %>% 
  filter(Year == 1787) %>% 
  select(GIS_ID, Pop) %>% 
  rename(Pop1787 = Pop)

# ==== Join assembly houses ====
railways_assembly_houses = railways %>% 
  left_join(
    Assembly_houses %>% mutate(GIS_ID = as.character(GIS_ID)), 
    by = c("GIS_ID", "Year")
  ) %>% 
  left_join(
    Assembly_houses_MA %>% mutate(GIS_ID = as.character(GIS_ID)), 
    by = c("GIS_ID", "Year")
  ) %>% 
  rename(MA_assembly = MA) %>% 
  select(-long, -lat) %>% 
  left_join(
    Folk_high_schools %>% mutate(GIS_ID = as.character(GIS_ID)), 
    by = c("GIS_ID", "Year")
  ) %>% 
  left_join(
    Folk_high_schools_MA %>% mutate(GIS_ID = as.character(GIS_ID)), 
    by = c("GIS_ID", "Year")
  ) %>% 
  rename(MA_folkhigh = MA) %>% 
  select(-long, -lat) %>% 
  select(-Parish) %>% 
  left_join(geo, by = "GIS_ID") %>% 
  left_join(pop1787, by = "GIS_ID") %>% 
  left_join(pop1801, by = "GIS_ID") %>%
  filter(Year <= 1920)

railways_census = railways_assembly_houses %>% 
  inner_join(census, by = c("GIS_ID", "Year"))

# ==== Rename again ====
# (Yes this is redundant, but it easier to implement this way than editing the above)
census = railways_census
grundtvig = railways_assembly_houses
rail_panel = railways

# ==== Clean census data ====
census = census %>%
  rename(
    # Rename and create new variables
    Population    = Pop,
    HISCAM_avg    = hiscam_avg,
    Migration     = Born_different_county,
    RailAccess    = Connected_rail,
    RailDist      = Distance_to_nearest_railway,
    LCPAccess     = Connected_rail_instr,
    LCPDist       = Distance_to_nearest_railway_instr
  ) %>%
  mutate(
    # Create logged variables
    lnPopulation       = log(Population),
    lnManufacturing    = log(Manufacturing_789 + 1),
    lnFarming          = log(Farming + 1),
    lnChild_women_ratio = log(Child_women_ratio + 1),
    lnHISCAM_avg       = log(HISCAM_avg),
    lnMigration        = log(Migration + 1),
    
    # Convert Year and GIS_ID for subsequent operations
    Year_num = as.numeric(as.character(Year)),
    GIS_ID_num = as.numeric(factor(GIS_ID)),
    
  ) %>%
  ungroup() %>%
  mutate(
    # Cut Boulder_clay_pct into quantiles
    Boulder_clay_decile = cut(Boulder_clay_pct, breaks = unique(quantile(Boulder_clay_pct, probs = seq(0, 1, by = 0.1), na.rm = TRUE)), include.lowest = TRUE),
    Boulder_clay_pct_year = paste(Boulder_clay_decile, Year, sep = "_"),

    # Cut Dist_hamb into quantiles
    Dist_hamb_decile = cut(dist_hmb, breaks = unique(quantile(dist_hmb, probs = seq(0, 1, by = 0.1), na.rm = TRUE)), include.lowest = TRUE),
    Dist_hamb_year = paste(Dist_hamb_decile, Year, sep = "_"),
    
    # Cut Dist_cph into quantiles
    Dist_cph_decile = cut(dist_cph, breaks = unique(quantile(dist_cph, probs = seq(0, 1, by = 0.1), na.rm = TRUE)), include.lowest = TRUE),
    Dist_cph_year = paste(Dist_cph_decile, Year, sep = "_"),
    
    # Cut Dist_cph into quantiles
    Dist_ox_decile = cut(DistOxRoad, breaks = unique(quantile(DistOxRoad, probs = seq(0, 1, by = 0.1), na.rm = TRUE)), include.lowest = TRUE),
    Dist_ox_year = paste(Dist_ox_decile, Year, sep = "_"),
    
    # Cut Dist_mt into quantiles
    Dist_mt_decile = cut(Distance_market_town, breaks = unique(quantile(Distance_market_town, probs = seq(0, 1, by = 0.1), na.rm = TRUE)), include.lowest = TRUE),
    Dist_mt_year = paste(Dist_mt_decile, Year, sep = "_"),

    # Cut pop 1801 into quantiles
    Pop1801_decile = cut(Pop1801, breaks = unique(quantile(Pop1801, probs = seq(0, 1, by = 0.1), na.rm = TRUE)), include.lowest = TRUE),
    Pop1801_year = paste(Pop1801_decile, Year, sep = "_"),

    # Area parish
    area_decile = cut(area_parish, breaks = unique(quantile(area_parish, probs = seq(0, 1, by = 0.1), na.rm = TRUE)), include.lowest = TRUE),
    area_parish_year = paste(area_decile, Year, sep = "_"),

    # County by year
    county_by_year = paste(County, Year, sep = "_")
  )

# Create Treat_year variable
census = census %>%
  group_by(GIS_ID) %>%
  mutate(
    Treat_year = min_treat_year(Year, RailAccess),
    Treat_year_instr = min_treat_year(Year, LCPAccess)
  ) %>%
  ungroup()

# Create Not agriculture
census = census %>% mutate(
  NotAgriculture = hisco_major0 + hisco_major1 + hisco_major2 + hisco_major3 + hisco_major4 + hisco_major5 + hisco_major7 + hisco_major8 + hisco_major9
) %>%
  mutate(
    lnNotAgriculture = log(1 + NotAgriculture)
  )

# === Delete duplicates Census ===

# Store the original number of observations
original_rows <- nrow(census)

# Remove duplicates and reassign to the same variable
census <- census %>%
  ungroup() %>%
  distinct()

# Calculate and display the number of observations removed
cat("Number of observations removed due to duplicate removal:", original_rows - nrow(census), "\n")

# ==== Clean Grundtvig data ====
grundtvig = grundtvig %>%
  rename(
    RailAccess = Connected_rail,
    LCPAccess  = Connected_rail_instr
  ) %>% 
  mutate(
    Year_num   = as.numeric(as.character(Year)),
    GIS_ID_num = as.numeric(factor(GIS_ID))
  ) %>%
  ungroup() %>%
  mutate(
    # Cut Boulder_clay_pct into quantiles
    Boulder_clay_decile = cut(Boulder_clay_pct, breaks = unique(quantile(Boulder_clay_pct, probs = seq(0, 1, by = 0.1), na.rm = TRUE)), include.lowest = TRUE),
    Boulder_clay_pct_year = paste(Boulder_clay_decile, Year, sep = "_"),
    
    # Cut Dist_hamb into quantiles
    Dist_hamb_decile = cut(dist_hmb, breaks = unique(quantile(dist_hmb, probs = seq(0, 1, by = 0.1), na.rm = TRUE)), include.lowest = TRUE),
    Dist_hamb_year = paste(Dist_hamb_decile, Year, sep = "_"),
    
    # Cut Dist_cph into quantiles
    Dist_cph_decile = cut(dist_cph, breaks = unique(quantile(dist_cph, probs = seq(0, 1, by = 0.1), na.rm = TRUE)), include.lowest = TRUE),
    Dist_cph_year = paste(Dist_cph_decile, Year, sep = "_"),
    
    # Cut Dist_cph into quantiles
    Dist_ox_decile = cut(DistOxRoad, breaks = unique(quantile(DistOxRoad, probs = seq(0, 1, by = 0.1), na.rm = TRUE)), include.lowest = TRUE),
    Dist_ox_year = paste(Dist_ox_decile, Year, sep = "_"),
    
    # Cut Dist_mt into quantiles
    Dist_mt_decile = cut(Distance_market_town, breaks = unique(quantile(Distance_market_town, probs = seq(0, 1, by = 0.1), na.rm = TRUE)), include.lowest = TRUE),
    Dist_mt_year = paste(Dist_mt_decile, Year, sep = "_"),
    
    # Cut pop 1801 into quantiles
    Pop1801_decile = cut(Pop1801, breaks = unique(quantile(Pop1801, probs = seq(0, 1, by = 0.1), na.rm = TRUE)), include.lowest = TRUE),
    Pop1801_year = paste(Pop1801_decile, Year, sep = "_"),
    
    # Area parish
    area_decile = cut(area_parish, breaks = unique(quantile(area_parish, probs = seq(0, 1, by = 0.1), na.rm = TRUE)), include.lowest = TRUE),
    area_parish_year = paste(area_decile, Year, sep = "_"),

    # County by year
    county_by_year = paste(County, Year, sep = "_")
  )


# Create Treat_year variable
grundtvig = grundtvig %>%
  group_by(GIS_ID) %>%
  mutate(
    Treat_year = min_treat_year(Year, RailAccess),
    Treat_year_instr = min_treat_year(Year, LCPAccess)
  ) %>%
  ungroup()

# Redefine Assembly_house and High School as a dummy
grundtvig = grundtvig %>%
  mutate(
    Assembly_house = ifelse(Assembly_house > 0, 1, Assembly_house),
    HighSchool = ifelse(HighSchool > 0, 1, HighSchool)
  ) %>%
  # NA in assembly house means 0 
  mutate(
    Assembly_house = replace_na(Assembly_house, 0),
  )

# Aline period
grundtvig = grundtvig %>%
  filter(Year <= 1920)

# === Delete duplicates Grundtvig ===

# Store the original number of observations
original_rows <- nrow(grundtvig)

# Remove duplicates and reassign to the same variable
grundtvig <- grundtvig %>%
  ungroup() %>%
  distinct()

# Calculate and display the number of observations removed
cat("Number of observations removed due to duplicate removal:", original_rows - nrow(grundtvig), "\n")

# ==== Add distance to nodes ==== 
distance_to_nodes = distance_to_nodes %>%
  mutate(GIS_ID = as.character(GIS_ID)) %>%
  mutate(
    away_from_node = ifelse(min_distance_to_node_km > 10, 1, 0)
  )

census = left_join(census, distance_to_nodes, by = "GIS_ID")
grundtvig = left_join(grundtvig, distance_to_nodes, by = "GIS_ID")

# ==== Add variable for invalid comparisons (i.e. connected after 1876) ====
later_connected_GIS_ID = rail_panel %>% 
  filter(Year > 1876) %>%
  filter(Year < 1920) %>%
  group_by(GIS_ID) %>%
  summarise(
    Connected_rail = max(Connected_rail)
  ) %>% 
  filter(Connected_rail == 1) %>%
  pull(GIS_ID)

earlier_connected_GIS_ID = rail_panel %>% 
  filter(Year <= 1876) %>%
  filter(Year < 1920) %>%
  group_by(GIS_ID) %>%
  summarise(
    Connected_rail = max(Connected_rail)
  ) %>% 
  filter(Connected_rail == 1) %>% 
  pull(GIS_ID)

invalid_comparison = data.frame(
  GIS_ID = rail_panel$GIS_ID %>% unique()
) %>%
  mutate(
    connected_later = 1*(GIS_ID %in% later_connected_GIS_ID),
    connected_earlier = 1*(GIS_ID %in% earlier_connected_GIS_ID)
  ) %>%
  mutate(
    never_connected = ifelse(connected_later == 0 & connected_earlier == 0, 1, 0)
  ) %>%
  mutate(
    only_connected_later = ifelse(connected_later == 1 & connected_earlier == 0, 1, 0),
  ) %>%
  mutate(
    invalid_comparison = ifelse(only_connected_later == 1, 1, 0)
  ) %>% 
  select(GIS_ID, invalid_comparison)

census = left_join(census, invalid_comparison, by = "GIS_ID")
grundtvig = left_join(grundtvig, invalid_comparison, by = "GIS_ID")

# ==== Save data ====
grundtvig %>% 
  write_csv2("Data/REGRESSION_DATA_Grundtvigianism.csv")

census %>% 
  write_csv2("Data/REGRESSION_DATA_Demography.csv")
