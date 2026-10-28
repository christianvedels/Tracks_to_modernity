# Railways
#
# Date updated:   2023-12-09
# Auhtor:         Christian Vedel 
# Purpose:        This script adds a functions to calculate stats for railways
#                 and parishes given two shape files: Parish shapes and railways
#                 shapes. 

# ==== Libraries ====
library(tidyverse)
source("Data_cleaning_scripts/000_Functions.R")
library(sf)
library(ggspatial)
library(foreach)

# ==== Read data ====
shape = read_sf("../Data not redistributable/Railways Fertner enriched/")
shape_parishes = read_sf("Data/sogne_shape/sogne.shp")

# test function
test = calc_rail( # Comes from functions
  shape,
  shape_parishes[1:10,],
  verbose = TRUE,
  plots = FALSE, # diagnostic maps not used in the paper
  id = "test",
  years = 1870:1871
)

# ==== Run it ====
railways_panel = calc_rail(
  shape,
  shape_parishes,
  verbose = TRUE,
  plots = FALSE, # diagnostic maps not used in the paper
  id = "actual_railways",
  years = 1846:2020
)

# ==== Simple summary stats ====
railways_panel %>% 
  group_by(Year) %>% 
  summarise(
    Connected_rail = mean(Connected_rail)
  ) %>% 
  ggplot(aes(Year, Connected_rail)) + geom_line() + 
  theme_bw()

# ==== Saving data ====
railways_panel %>% 
  write_csv2("Data/Panel_of_railways_in_parishes.csv")

# ==== Run it sepperately for line types ====
shape$type %>% unique()
shape$type2 %>% unique()
ggplot() +
  layer_spatial(data = shape, aes(col = type)) +
  theme_minimal()

shape = shape %>% # Convert NA to something else
  mutate(
    type = ifelse(is.na(type), "Unknown", type),
    type2 = ifelse(is.na(type2), "Unknown", type2)
  )

ggplot() +
  layer_spatial(data = shape, aes(col = type2)) +
  theme_minimal()

subset1 = foreach(i = unique(shape$type)) %do% {
  shape %>% filter(type == i)
}

subset2 = foreach(i = unique(shape$type2)) %do% {
  shape %>% filter(type2 == i)
}

foreach(shape_sub = subset1) %do% {
  type1 = unique(shape_sub$type)
  cat("\n:::::::: TYPE:", type1)
  if(length(type1)>1){
    stop("Number of types in subset should be no more than 1")
  }
  railways_panel_sub = calc_rail(
    shape_sub,
    shape_parishes,
    verbose = TRUE,
    plots = FALSE, # diagnostic maps not used in the paper
    id = paste0("By_type_type_", type1),
    years = 1846:2020
  )
  
  railways_panel_sub = railways_panel_sub %>% 
    mutate(type = type1)
  
  fname = paste0("Data/Panels_by_type/type_", type1, ".csv")
  railways_panel_sub %>% 
    write_csv2(fname)
}

foreach(shape_sub = subset2) %do% {
  type2 = unique(shape_sub$type2)
  cat("\n:::::::: TYPE:", type1)
  if(length(type2)>1){
    stop("Number of types in subset should be no more than 1")
  }
  railways_panel_sub = calc_rail(
    shape_sub,
    shape_parishes,
    verbose = TRUE,
    plots = FALSE, # diagnostic maps not used in the paper
    id = paste0("By_type_type2_", type2),
    years = 1846:2020
  )
  
  railways_panel_sub = railways_panel_sub %>% 
    mutate(type2 = type2)
  
  fname = paste0("Data/Panels_by_type/type2_", type2, ".csv")
  railways_panel_sub %>% 
    write_csv2(fname)
}




