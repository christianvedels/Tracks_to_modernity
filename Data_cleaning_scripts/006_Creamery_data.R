# Creamery data
#
# Date updated:   2025-02-05
# Auhtor:         Christian Vedel 
# Purpose:        Cleans creamery data

# ==== Libraries ====
library(tidyverse)
library(sf)
source("Data_cleaning_scripts/000_Functions.R")

# ==== Load data ====
load("../Data not redistributable/All_raw_data_for_project.Rdata")
shape_parishes = read_sf("Data/sogne_shape/sogne.shp") %>%
  st_make_valid()

shape_parishes = st_make_valid(shape_parishes)

# ==== Distinct coordinates ====
creameries_coord = creameries %>% 
    distinct(Creamery_ID, Longitude_new, Latitude_new) %>%
    rename(long = Longitude_new, lat = Latitude_new) %>%
    drop_na()


geo_tmp_sf = st_as_sf(
  creameries_coord, coords = c("long", "lat"),
  crs = st_crs(shape_parishes)
)

parish_coord_sf = st_as_sf(
  shape_parishes %>% data.frame(), coords = c("long", "lat"),
  crs = st_crs(shape_parishes)
)

# ==== Compute closest parish for each creamery ====
closest_parish = apply(
    st_distance(geo_tmp_sf, parish_coord_sf), 1, which.min
)

creameries_coord$GIS_ID = shape_parishes$GIS_ID[closest_parish]

# ==== Save data ====
creameries_coord %>% 
    write_csv("Data/creameries_parish.csv")
