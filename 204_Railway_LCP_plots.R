# Railway and LCP maps
#
# Date updated:   2025-02-18
# Author:         Tom Görges
# Purpose:        Creates maps showing the evolution of the railways in Denmark and our LCP instrument.

# ==== Libraries ====
library(sf)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidygeocoder)
library(readxl)


source("Data_cleaning_scripts/000_Functions.R")


# ==== Read DK shape file ====

# Download shapefile from DAWA
url = "https://api.dataforsyningen.dk/kommuner?format=geojson"
geofile = tempfile()
download.file(url, geofile)
shape = st_read(geofile)

shape = shape %>% 
  filter(!navn %in% c("Tønder", "Haderslev", "Aabenraa", "Sønderborg"))

dk <- shape %>%
  st_make_valid() %>%   
  st_union()

# Read the other shapefiles
lcp <- st_read("../../Data not redistributable/Instrument_shapes/lcp_shape_files/LCP_scrit_2.shp")

rail <- st_read("../../Data not redistributable/Railways Fertner/jernbane_historisk_v050413/jernbane_historisk.shp") %>%
  st_transform(crs = 4326) %>%  # Ensure transformation to WGS 84
  mutate(id = c(1:n()))

schleswig_ids <- c(55, 74, 169, 1, 190, 73, 220, 221,
                   195, 196, 219, 152, 151, 222, 173,
                   174, 175, 172, 177, 166, 180, 179,
                   178, 223, 183, 182, 176, 208, 171)

# Exclude schleswig rails
rail <- rail %>% filter(!id %in% schleswig_ids)

# bbox without Bornholm
crop_extent <- st_bbox(c(xmin = 8.076389, 
                         ymin = 54.55903,
                         xmax = 12.6900061378, # cropped to exclude Bornholm
                         ymax = 57.75153),
                       crs = st_crs(dk))


# Exclude Bornholm
dk_cropped <- st_crop(dk, crop_extent)


st_crs(lcp) == st_crs(rail)
st_crs(lcp) == st_crs(dk_cropped)

# === Nodes ===
nodes <- read_excel("Data/nodes.xlsx")

# create sf
nodes_sf <- st_as_sf(nodes,
                     coords = c("long", "lat"),
                     crs = 4326)


# Exclude Bornholm from nodes
nodes_sf <- nodes_sf %>% filter(!Market_town == "Roenne")

# ==== Dynamic Plot Creation ====

years <- c(1847, 1850, 1860, 1876, 1880, 1901)


for (year in years) {
  rail_subset <- rail[rail$opened <= year, ]
  lcp_subset <- lcp[lcp$opened <= year, ]
  
  if (year == 1901) {
    rail_subset <- st_crop(rail_subset, crop_extent) # Exclude Bornholm from 1901 rails
  }
  
  p <- ggplot() +
    geom_sf(data = dk_cropped, fill = "grey90", color = "grey") +
    geom_sf(data = rail_subset, color = "black", linewidth = 1, alpha = 1, linetype = "solid") +
    geom_sf(data = lcp_subset, color = colours$red, linewidth = 1, linetype = "longdash", alpha = 1) +
    geom_sf(data = nodes_sf, size = 3, shape = 21, fill = "black") +
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = NA))
  print(p)
  ggsave(paste0("Plots/Maps_of_railways/Rails_and_LCP_", year, ".png"), p, width = 10, height = 8)
}

for (year in years) { # Without LCP
  rail_subset <- rail[rail$opened <= year, ]
  
  if (year == 1901) {
    rail_subset <- st_crop(rail_subset, crop_extent) # Exclude Bornholm from 1901 rails
  }
  
  p <- ggplot() +
    geom_sf(data = dk_cropped, fill = "grey90", color = "grey") +
    geom_sf(data = rail_subset, color = "black", linewidth = 1, alpha = 1, linetype = "solid") +
    geom_sf(data = nodes_sf, size = 3, shape = 21, fill = "black") +
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = NA))
  print(p)
  ggsave(paste0("Plots/Maps_of_railways/Rails", year, ".png"), p, width = 10, height = 8)
} 




