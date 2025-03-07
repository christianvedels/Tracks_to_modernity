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
lcp <- st_read("../Data not redistributable/Instrument_shapes/lcp_shape_files/LCP_scrit_1.shp")

rail <- st_read("../Data not redistributable/Railways Fertner/jernbane_historisk_v050413/jernbane_historisk.shp") %>%
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

# === Market towns ===

# Reading in market towns
market_towns = read_delim("Data/Market_towns.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE, locale =locale(encoding = "ISO-8859-1"))

# Clean Coord column in market towns

# Define a helper function
dms_to_dd <- function(dms) {
  # This pattern expects strings of the form "dd°mm?ss?D" or "dd°mm?D"
  pattern <- "([0-9]+)°([0-9]+)\\?(?:([0-9]+)\\?)?([NSEWØ])"
  parts <- str_match(dms, pattern)
  # Extract the components: group 1 = degrees, group 2 = minutes, group 3 = seconds (optional),
  # and group 4 = direction.
  deg <- as.numeric(parts[2])
  min <- as.numeric(parts[3])
  sec <- as.numeric(parts[4])
  if (is.na(sec)) sec <- 0
  # Convert to decimal degrees
  dd <- deg + min / 60 + sec / 3600
  # Adjust sign if the direction is South or West
  if (parts[5] %in% c("S", "W")) {
    dd <- -dd
  }
  return(dd)
}

# Clean the dataset:
market_towns <- market_towns %>%
  # Split Coord into two parts: lat_str and long_str
  separate(Coord, into = c("lat_str", "long_str"), sep = " ", remove = FALSE) %>%
  # Convert the DMS strings to decimal degrees for both latitude and longitude
  mutate(
    lat  = sapply(lat_str, dms_to_dd),
    long = sapply(long_str, dms_to_dd)
  )

# === Nodes ====
Q3_pop1801 <- quantile(market_towns$Pop1801, probs = 0.75, na.rm = TRUE)

# Subset the data frame, sort and select
market_towns <- market_towns %>% 
  filter(
    Pop1801 > Q3_pop1801
  ) %>% 
  dplyr::select(Market_town, Pop1801, lat, long)%>% 
  mutate(
    node_type = "75th percentile"
  )

# Take out nodes affected by German-Danish war
market_towns <- market_towns %>% 
  filter(!Market_town %in% c("Aabenraa", "Ribe", "Haderslev", "Toender", "Soenderborg"))

### Add additional nodes
additional_nodes = list(
  esbjerg = data.frame(Market_town = "Esbjerg", Pop1801 = NA),
  ringkobing = data.frame(Market_town = "Ringkobing", Pop1801 = NA),
  holstebro = data.frame(Market_town = "Holstebro", Pop1801 = NA),
  korsoer = data.frame(Market_town = "Korsør", Pop1801 = NA),
  middelfart = data.frame(Market_town = "Middelfart town", Pop1801 = NA),
  varde = data.frame(Market_town = "Varde", Pop1801 = NA)) %>%
  do.call("rbind", .)


# Geocode (Open Street Map)
additional_nodes <- additional_nodes %>% 
  geocode(
    Market_town, 
    method = 'osm', 
    full_results = F, 
    custom_query = list(countrycodes = 'dk')) 

# Add node type
additional_nodes <- additional_nodes %>%
  mutate(
    node_type = "Additional nodes"
  )

# bind together
nodes <- market_towns %>% bind_rows(additional_nodes)


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




