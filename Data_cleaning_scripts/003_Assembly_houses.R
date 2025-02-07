# Assembly houses
#
# Date updated:   2023-10-02
# Auhtor:         Christian Vedel 
# Purpose:        Cleans data on assembly houses


# ==== Libraries ====
library(tidyverse)
library(sf)
library(ggspatial)
library(foreach)
source("000_Functions.R")

# ==== Load data ====
load("../Data not redistributable/All_raw_data_for_project.Rdata")
shape_parishes = read_sf("Data/sogne_shape/sogne.shp")

# ==== Creating key to use ====
tmp1 = assembly_houses %>% 
  distinct(Amt, Sogn, cluster) %>% 
  drop_na(Amt, Sogn, cluster) 

tmp2 = key %>% select(Cluster, GIS_ID, navn_digdag)

key0 = tmp1 %>% 
  full_join(tmp2, by = c("cluster"="Cluster")) %>% 
  distinct(cluster, GIS_ID)

# Problems
key0 %>% 
  group_by(cluster) %>% count() %>% filter(n>1) %>% 
  left_join(key0, by = "cluster")

key0 %>% 
  group_by(GIS_ID) %>% count() %>% filter(n>1) %>% 
  left_join(key0, by = "GIS_ID")

# Fix key
set.seed(20)
key0 = sample_frac(key0, 1)

key0 = key0 %>% 
  group_by(cluster) %>% 
  summarise(GIS_ID = GIS_ID[1])

key0 = key0 %>% 
  group_by(GIS_ID) %>% 
  summarise(cluster = cluster[1])

# Plot missing links
tmp = shape_parishes %>% 
  left_join(key0 %>% mutate(GIS_ID = as.character(GIS_ID)), by = "GIS_ID") %>% 
  mutate(
    missing = is.na(cluster)
  )

ggplot() + # Not important
  layer_spatial(
    aes(fill = missing, col = missing),
    size = 0,
    data = tmp
  )
  
tmp$missing %>% sum() # 32 is not linked

# ==== Create panel ====
panel_assembly_houses = assembly_houses %>% 
  select(Year, Sogn, Forsamlingshuse, cluster, long, lat) %>% 
  rename(
    Assembly_house = Forsamlingshuse,
    Parish = Sogn
  ) %>% 
  inner_join(key0, by = "cluster") %>% 
  filter(Year <= 1920) # First year of publicaitons of Trap 4

# ==== Plot ====
foreach(y = seq(min(panel_assembly_houses$Year), max(panel_assembly_houses$Year))) %do% {
  tmp_y = panel_assembly_houses %>% 
    filter(Year == y) %>% 
    mutate(GIS_ID = as.character(GIS_ID))
  
  shape_y = shape_parishes %>% left_join(tmp_y, by = "GIS_ID")
  
  p1 = ggplot() +
    layer_spatial(
      aes(fill = Assembly_house),
      size = 0,
      data = shape_y
    ) + 
    scale_fill_gradient(
      low = "white",
      high = "#DE7500"
    ) + 
    theme_bw()
  
  # p1 = p1 +
  #   expand_limits(fill = c(0, max(panel_assembly_houses$Assembly_house)))
    
  fname_y = paste0("Plots/Assembly_houses/","Y",y,".png")
  w = 6
  ggsave(fname_y, plot = p1, width = w, height = 0.7*w)
}

# ==== Compute market access ====
MA_assembly = MA(
  destination = shape_parishes,
  origin = assembly_houses %>% 
    filter(Forsamlingshuse > 0) %>% 
    filter(Year <= 1920) %>% 
    mutate(w = Forsamlingshuse),
  verbose = TRUE
)

# ==== Plot MA ====
foreach(y = seq(min(panel_assembly_houses$Year), max(panel_assembly_houses$Year))) %do% {
  tmp_y = MA_assembly %>% 
    filter(Year == y) %>% 
    mutate(GIS_ID = as.character(GIS_ID))
  
  shape_y = shape_parishes %>% left_join(tmp_y, by = "GIS_ID")
  
  p1 = ggplot() +
    layer_spatial(
      aes(fill = MA),
      size = 0,
      data = shape_y
    ) + 
    scale_fill_gradient(
      low = "white",
      high = "#DE7500"
    ) + 
    theme_bw()
  
  # p1 = p1 +
  #   expand_limits(fill = c(0, max(panel_assembly_houses$Assembly_house)))
  
  fname_y = paste0("Plots/Assembly_houses_MA/","Y",y,".png")
  w = 6
  ggsave(fname_y, plot = p1, width = w, height = 0.7*w)
  
  cat("\n",y)
}

# ==== Save data ====
panel_assembly_houses %>% 
  write_csv2("Data/Panel_of_assembly_houses.csv")

MA_assembly %>% 
  write_csv2("Data/Panel_of_MA_assembly_houses.csv")
