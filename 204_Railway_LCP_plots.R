# Railway and LCP maps
#
# Date updated:   2025-02-18
# Author:         Tom Görges
# Purpose:        Creates 3-4 maps in one plot showing the evolution of the railways and Denmark and our LCP instrument.

# ==== Libraries ====
library(sf)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidygeocoder)


source("Data_cleaning_scripts/000_Functions.R")

# ==== Load data ====

# Read the shapefiles
lcp <- st_read("../../Data not redistributable/Instrument_shapes/lcp_shape_files/LCP_scrit_1.shp")

rail <- st_read("../../Data not redistributable/Railways Fertner/jernbane_historisk_v050413/jernbane_historisk.shp") %>%
  st_transform(crs = 4326)  # Ensure transformation to WGS 84

dk <- st_read("Data/Denmark Outline ADM 0 Stanford/DNK_adm0.shp") %>%
  st_transform(crs = 4326)

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

# -----------
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



# --------------

# 1847 subset
rail47 <- rail[rail$opened <= 1847, ]
lcp47 <- lcp[lcp$opened <= 1847, ]

# 1876 subset
rail76 <- rail[rail$opened <= 1876, ]
lcp76 <- lcp

# 1901 subset
rail01 <- rail[rail$opened <= 1901, ]
lcp01 <- lcp

# Exclude Bornholm from 1901 rails
rail01 <- st_crop(rail01, crop_extent)

# Exclude Bornholm from nodes
nodes_sf <- nodes_sf %>% filter(!Market_town == "Roenne")

# === PLOTS === #

# 1847
p_1847 <- ggplot() +
  geom_sf(data = dk_cropped, fill = "grey90", color = "grey") +  # Denmark outline
  geom_sf(data = rail47, color = colours$red, linewidth = 1.25, alpha = 1) +  # Railways in 1847
  geom_sf(data = lcp47, color = colours$blue, linewidth = 1.25, linetype = "dashed", alpha = 1) +  # LCP in 1847
  geom_sf(data = nodes_sf, size = 2, shape = 21, fill = "black") + # Nodes as dots
  theme_void() +  # Removes grid and axis labels
  theme(panel.background = element_rect(fill = "white", color = NA),  # Set clean white background
        plot.title = element_text(size = 14, face = "bold"))

p_1847


ggsave("Plots/Rails_and_LCP_1847.png", p_1847, width = dims$width, height = dims$height)

# ----------------------

# 1876
p_1876 <- ggplot() +
  geom_sf(data = dk_cropped, fill = "grey90", color = "grey") +  # Denmark outline
  geom_sf(data = rail76, color = colours$red, linewidth = 1.25, alpha = 1) +  # Railways in 1847
  geom_sf(data = lcp76, color = colours$blue, linewidth = 1.25, linetype = "dashed", alpha = 1) +  # LCP in 1847
  geom_sf(data = nodes_sf, size = 2, shape = 21, fill = "black") + # Nodes as dots
  theme_void() +  # Removes grid and axis labels
  theme(panel.background = element_rect(fill = "white", color = NA),  # Set clean white background
        plot.title = element_text(size = 14, face = "bold"))

p_1876


ggsave("Plots/Rails_and_LCP_1876.png", p_1876, width = dims$width, height = dims$height)

# ----------------------

# 1901
p_1901 <- ggplot() +
  geom_sf(data = dk_cropped, fill = "grey90", color = "grey") +  # Denmark outline
  geom_sf(data = rail01, color = colours$red, linewidth = 1.25, alpha = 1) +  # Railways in 1847
  geom_sf(data = lcp01, color = colours$blue, linewidth = 1.25, linetype = "dashed", alpha = 1) +  # LCP in 1847
  geom_sf(data = nodes_sf, size = 2, shape = 21, fill = "black") + # Nodes as dots
  theme_void() +  # Removes grid and axis labels
  theme(panel.background = element_rect(fill = "white", color = NA),  # Set clean white background
        plot.title = element_text(size = 14, face = "bold"))

p_1901


ggsave("Plots/Rails_and_LCP_1901.png", p_1901, width = dims$width, height = dims$height)





