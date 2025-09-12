# Instruments to panel data
#
# Date updated:   2023-12-08
# Auhtor:         Tom Görges, Christian Vedel
# Purpose:        Reads shape files of predicted railways based on 
#                 least cost paths


# ==== Libraries ====
library(tidyverse)
library(foreach)
library(ggspatial)
library(sf)
library(permute)
library(raster)
library(elevatr)

source("../Data_cleaning_scripts/000_Functions.R") # Contains calc_rail()

# ==== Load data ====
# Load shape files
shape_parishes = read_sf("../Data/sogne_shape/sogne.shp")

f_shapes = list.files("../../../Data not redistributable/Instrument_shapes/lcp_shape_files", pattern = ".shp")

shapes = foreach(f = f_shapes) %do% {
  f_full = paste0("../../../Data not redistributable/Instrument_shapes/lcp_shape_files/",f)
  shape_f = st_read(f_full) %>% st_transform(crs = 32632)
  return(shape_f)
}

### Add variable fake close. Necessary for function
for(i in seq_along(shapes)) {
  shapes[[i]]$fake_close = 3000
}


# ==== Run it ====
foreach(i = 1:length(shapes)) %do% {
  shape_i = shapes[[i]] # access list elements
  parameter = gsub("LCP_|.shp","", f_shapes[i])
  
  cat("\nComputing panel for:", parameter)
  
  railways_panel_i = calc_rail(
    shape_i,
    shape_parishes,
    verbose = TRUE,
    plots = TRUE,
    id = paste0("predicted_rail_paramS",i),
    years = 1846:1901
  )
  
  # Add parameter info 
  railways_panel_i = railways_panel_i %>% 
    mutate(parameter = parameter)
  
  railways_panel_i %>% 
    write_csv2(paste0("../Data/Instruments/paramS_",parameter, ".csv"))
  
  cat("\n===> FINISHED:",parameter)
}


























