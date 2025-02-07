# Regression data
#
# Date updated:   2023-10-02
# Auhtor:         Christian Vedel 
# Purpose:        Collects data 

# ==== Libraries ====
library(tidyverse)
library(foreach)

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

# ==== Save data ====
railways_assembly_houses %>% 
  write_csv2("Data/REGRESSION_DATA_Grundtvigianism.csv")

railways_census %>% 
  write_csv2("Data/REGRESSION_DATA_Demography.csv")
