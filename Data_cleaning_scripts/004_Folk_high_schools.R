# Folk High School (Højskoler)
#
# Date updated:   2023-10-02
# Auhtor:         Christian Vedel 
# Purpose:        Cleans data on high schools

# ==== Libraries ====
library(tidyverse)
library(sf)
library(ggspatial)
library(foreach)
source("000_Functions.R")

# ==== Load data ====
load("../Data not redistributable/All_raw_data_for_project.Rdata")
shape_parishes0 = read_sf("Data/sogne_shape/sogne.shp")
shape_parishes = shape_parishes0

# Load railways to be able to cast to this format
shape = read_sf("../Data not redistributable/Railways Fertner/jernbane_historisk_v050413/jernbane_historisk.shp")

# ==== High school data ====
geo_tmp = hoejskoler %>% 
  distinct(id, lat, long)

geo_tmp_sf = st_as_sf(
  geo_tmp, coords = c("long", "lat"),
  crs = st_crs(shape_parishes)
)

shape_parishes = st_transform(shape_parishes, st_crs(shape))
geo_tmp_sf = st_transform(geo_tmp_sf, st_crs(shape))

gis_id = st_intersects(geo_tmp_sf, shape_parishes) %>% 
  as.character()

geo_tmp$GIS_ID = gis_id

geo_key = geo_tmp %>% select(id, GIS_ID)

# Map of folk high schools: not used in the paper, commented out
# p1 = ggplot() + 
#   layer_spatial(
#     size = 0,
#     data = shape_parishes
#   ) + 
#   layer_spatial(
#     geo_tmp_sf
#   ) + 
#   theme_bw()

# p1
# w = 6
# ggsave("Plots/Hoejskoler.png", plot = p1, width = w, height = 0.7*w)

# ==== Construct panel ====
min_year = hoejskoler$Start %>% min()
max_year = hoejskoler$End %>% max()

highschool_panel = foreach(y = seq(min_year, max_year), .combine = "bind_rows") %do% {
  # Empty frame
  frame_y = shape_parishes %>% 
    as.data.frame() %>% 
    select(GIS_ID, long, lat)
  
  # High schools that existed in year 'y'
  schools_y = hoejskoler %>% 
    filter(Year == y) %>% 
    select(id) %>% 
    mutate(HighSchool = 1)
    
  res_y = schools_y %>% 
    left_join(geo_key, by = "id") %>% 
    # Aggregate count at parish level
    group_by(GIS_ID) %>% 
    summarise(
      HighSchool = sum(HighSchool)
    ) %>% 
    right_join(frame_y, by = "GIS_ID") %>% 
    mutate(
      HighSchool = ifelse(is.na(HighSchool), 0, HighSchool)
    ) %>% 
    mutate(Year = y) %>% 
    arrange(GIS_ID)
  
  cat(y, "           \r")
  
  return(res_y)
}

# Summary stats (plot not used in the paper, commented out)
# highschool_panel %>% 
#   group_by(Year) %>% 
#   summarise(HighSchools = sum(HighSchool)) %>% 
#   ggplot(aes(Year, HighSchools)) + geom_line()

# ==== Plot ==== (not used in the paper, commented out)
# foreach(y = seq(min(highschool_panel$Year), max(highschool_panel$Year))) %do% {
#   tmp_y = highschool_panel %>% 
#     filter(Year == y) %>% 
#     mutate(GIS_ID = as.character(GIS_ID))
  
#   shape_y = shape_parishes %>% left_join(tmp_y, by = "GIS_ID")
  
#   p1 = ggplot() +
#     layer_spatial(
#       aes(fill = HighSchool),
#       size = 0,
#       data = shape_y
#     ) + 
#     scale_fill_gradient(
#       low = "white",
#       high = "#DE7500"
#     ) + 
#     theme_bw()
  
  # p1 = p1 +
  #   expand_limits(fill = c(0, max(panel_assembly_houses$Assembly_house)))
  
#   fname_y = paste0("Plots/Folk_high_schools/","Y",y,".png")
#   w = 6
#   ggsave(fname_y, plot = p1, width = w, height = 0.7*w)
  
#   cat(y, "           \r")
# }

# ==== Compute market access ====
MA_highschools = MA(
  destination = shape_parishes0,
  origin = highschool_panel %>%
    filter(HighSchool > 0) %>% 
    mutate(w = HighSchool),
  verbose = TRUE
)

# ==== Plot MA ==== (not used in the paper, commented out)
# foreach(y = seq(min(MA_highschools$Year), max(MA_highschools$Year))) %do% {
#   tmp_y = MA_highschools %>% 
#     filter(Year == y) %>% 
#     mutate(GIS_ID = as.character(GIS_ID))
  
#   shape_y = shape_parishes %>% left_join(tmp_y, by = "GIS_ID")
  
#   p1 = ggplot() +
#     layer_spatial(
#       aes(fill = MA),
#       size = 0,
#       data = shape_y
#     ) + 
#     scale_fill_gradient(
#       low = "white",
#       high = "#DE7500"
#     ) + 
#     theme_bw()
  
  # p1 = p1 +
  #   expand_limits(fill = c(0, max(panel_assembly_houses$Assembly_house)))
  
#   fname_y = paste0("Plots/Folk_high_schools_MA/","Y",y,".png")
#   w = 6
#   ggsave(fname_y, plot = p1, width = w, height = 0.7*w)
  
#   cat("\n",y)
# }

# ==== Save data ====
highschool_panel %>% 
  write_csv2("Data/Panel_of_folk_high_schools.csv")

MA_highschools %>% 
  write_csv2("Data/Panel_of_MA_folk_high_schools.csv")
