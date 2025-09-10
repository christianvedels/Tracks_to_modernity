# Railways: Generate instruments
#
# Date updated:   2025-09-09
# Author:         Tom Görges
# Purpose:        This script takes slope information and returns shape files
#                 of predicted railways based on least cost paths (Updated Version)


# ==== Libraries ====
library(sf)
library(tidyverse)
library(elevatr)
library(viridis)
library(raster)
library(gdistance)
library(sp)
library(ggrepel)
library(tidygeocoder)
library(leastcostpath)
library(terra)
library(writexl)


# ==== Parameters ====
# Define the range of crit_slope values
crit_slope_values = c(1:16) 

Market_town_naming_changes <- c(
  "Koebenhavn" = "Copenhagen",
  "Loegstoer" = "Løgstør",
  "Noerre Sundby" = "Norresundby",
  "Praestoe" = "Præstø",
  "Ringkoebing" = "Ringkøbing (2387702632)",
  "Roedby" = "Rødby",
  "Sakskoebing" = "Sakskøbing",
  "Middelfart" = "Middelfart town",
  "Skaelskoer" = "Skælskør",
  "Stubbekoebing" = "Stubbekøbing",
  "Aeroeskoebing" = "Ærøskøbing",
  "Hoersholm" = "Hørsholm",
  "Skive" = "Skive, Skive",
  "Soeborg" = "Søborg",
  "Nykoebing Mors" = "Glyngoere"
)

# ==== Load data (Railway shape data and Outline of Denmark) ====
shape_data = st_read("../../../Data not redistributable/Railways Fertner/jernbane_historisk_v050413/jernbane_historisk.shp") %>% st_transform(4326)
outline_dk = st_read("../Data/sogne_shape/") %>% st_transform(4326)

# Loading and preparing Market towns 
path = "https://raw.githubusercontent.com/christianvedels/A_perfect_storm_replication/main/Data/Market_towns.csv"
mt = read_csv2(path, guess_max = 2000) %>%
  mutate(Market_town = recode(Market_town, !!!Market_town_naming_changes)) %>%
  filter(!Market_town %in% c("Soenderborg", "Toender", "Haderslev", "Ribe", "Aabenraa", "Nordborg")) %>% # affected by German-Danish war
  filter(!Market_town %in% c("Roenne", "Neksoe", "Allinge-Sandvig", "Aakirkeby", "Hasle", "Svaneke")) %>% # On the island of Bornholm
  filter(Privilege_start < 1847 & Privilege_end > 1847) %>% # Subset to those towns that actually have privilege
  geocode(address = Market_town, method = "osm", custom_query = list(countrycodes = 'dk'), full_results = TRUE) %>%
  relocate(Market_town, display_name, lat, long) # reorder


# Add Esbjerg and Struer
manual_towns <- tibble(
  Market_town = c("Esbjerg", "Struer", "Skjern"),
  GIS_ID = c("1387", "1321", "180108")) %>%
  geocode(address = Market_town, method = "osm", custom_query = list(countrycodes = 'dk'), full_results = T)


# bind together
nodes <- bind_rows(mt, manual_towns) %>%
  filter(!Market_town %in% c("Mariager", "Ærøskøbing", "Stege")) #market towns not connected to rail (1916)

# -------------

# Obtain elevation raster (from OpenStreetMap)
denmark_elev = get_elev_raster(outline_dk, z = 5, source = "osm", clip = "locations") # z(oom) = 9 used by package "movecost", probably need zoom = 10 or higher but my computer breaks down at this resolution

# ==== Plot elev ====
plot(denmark_elev)
dnk = as(denmark_elev, "SpatRaster")
plot(dnk, main="Original Raster")

#############################################################################################
# === Create slope cost surface using pre-installed wheeled transport function (Herzog) === #
#############################################################################################

# Loop over the range of crit_slope values
for (crit_slope in crit_slope_values) {
  cat("crit_slope:", crit_slope, "\n")
  
  # Create slope cost surface with the current crit_slope
  slope_cs = create_slope_cs(
    dnk,
    cost_function = "wheeled transport",
    neighbours = 8,
    crit_slope = crit_slope, # Use current crit_slope value
    max_slope = NULL,
    exaggeration = FALSE
  )
  
  # Plot the resulting slope cost surface
  plot(slope_cs)
  
  # Construct the file name dynamically based on the crit_slope value
  slope_label = crit_slope
  
  # Construct the file path dynamically based on the crit_slope value
  file_path = paste0(
    "../../../Data not redistributable/Instrument_shapes/lcp_slope_cost_surfaces/slope_cs_crit_",
    slope_label,
    ".rds"
  )
  
  # Save the 'slope_cs' object to a .rds file with the current crit_slope in the filename
  write_rds(slope_cs, file_path)
}

# ==== Load slope cost surfaces ==== #

# Loop to load slope_cs_1 to slope_cs_16
for (i in crit_slope_values) {
  
  # Construct the file path for each crit_slope value
  file_path = paste0(
    "../../../Data not redistributable/Instrument_shapes/lcp_slope_cost_surfaces/slope_cs_crit_",
    i,
    ".rds"
  )
  
  # Dynamically assign the loaded rds file to a variable named slope_cs_1, slope_cs_2, etc.
  assign(paste0("slope_cs_", i), read_rds(file_path))
}


# create sf
nodes_sf <- st_as_sf(nodes,
                     coords = c("long", "lat"),
                     crs = 4326)


plot(dnk, main="Original Raster")
plot(nodes_sf$geometry, add = T, col = "yellow")

# === Creation of file that contains all unique GIS_IDs and their respective minimum distance to nodes

# Load shape files
shape_parishes <- read_sf("../../../Data not redistributable/DK parish shapefile/Parish1820Counting1837.shp")

# Ensure valid geometries
shape_parishes <- st_make_valid(shape_parishes)

# Check validity
shape_parishes$valid <- st_is_valid(shape_parishes, reason = TRUE)
table(shape_parishes$valid)

# Ensure consistent coordinate systems:
nodes_sf <- st_transform(nodes_sf, crs = st_crs(shape_parishes))

# Compute centroids of polygons
shape_parishes_centroids <- st_centroid(shape_parishes)

# compute distance matrix
dist_matrix <- st_distance(shape_parishes_centroids, nodes_sf)

# extract minimum distance
shape_parishes_centroids$min_distance_to_node_km <- apply(dist_matrix, 1, min) / 1000

# select needed vars
distance_to_nodes_df <- shape_parishes_centroids %>% 
  dplyr::select(GIS_ID, min_distance_to_node_km) %>%
  st_drop_geometry()

# safe
write_xlsx(distance_to_nodes_df, "../Data/distance_to_nodes.xlsx")

# ----------------------------------

### Make data frame a Spatial points df
coordinates(nodes) = ~long+lat
proj4string(nodes) = CRS("+proj=longlat +datum=WGS84")


# Define city pairs and opening years for the respective section
opening_years <- tribble(
  ~town1,              ~town2,                ~opened,
  "Esbjerg",           "Kolding",             1874,
  "Fredericia",        "Kolding",             1866,
  "Naestved",          "Vordingborg",         1870,
  "Naestved",          "Præstø",              1900,
  "Naestved",          "Skælskør",            1892,
  "Slagelse",          "Skælskør",            1892,
  "Naestved",          "Slagelse",            1892,
  "Koege",             "Naestved",            1870,
  "Koege",             "Præstø",              1879,
  "Koege",             "Store Heddinge",      1879,
  "Koege",             "Roskilde",            1870,
  "Copenhagen",        "Roskilde",            1847,
  "Copenhagen",        "Frederikssund",       1879,
  "Copenhagen",        "Slangerup",           1906,
  "Copenhagen",        "Hilleroed",           1864,
  "Helsingoer",        "Hilleroed",           1864,
  "Copenhagen",        "Hørsholm",            1895,
  "Helsingoer",        "Hørsholm",            1895,
  "Holbaek",           "Roskilde",            1874,
  "Holbaek",           "Kalundborg",          1874,
  "Holbaek",           "Nykoebing Sjaelland", 1899,
  "Slagelse",          "Kalundborg",          1898,
  "Slagelse",          "Korsoer",             1856,
  "Slagelse",          "Soroe",               1856,
  "Ringsted",          "Soroe",               1856,
  "Ringsted",          "Roskilde",            1856,
  "Svendborg",         "Odense",              1876,
  "Aalborg",           "Hjoerring",           1871,
  "Saeby",             "Hjoerring",           1913,
  "Saeby",             "Aalborg",             1899,
  "Saeby",             "Frederikshavn",       1899,
  "Hobro",             "Aalborg",             1869,
  "Hobro",             "Løgstør",             1893,
  "Hobro",             "Randers",             1869,
  "Løgstør",           "Viborg",              1893,
  "Grenaa",            "Randers",             1876,
  "Grenaa",            "Ebeltoft",            1901,
  "Aarhus",            "Skanderborg",         1868,
  "Aarhus",            "Randers",             1862,
  "Aarhus",            "Grenaa",              1877,
  "Viborg",            "Randers",             1863,
  "Viborg",            "Nibe",                1899,
  "Horsens",           "Skanderborg",         1868,
  "Horsens",           "Vejle",               1868,
  "Viborg",            "Skive, Skive",        1865,
  "Holstebro",         "Struer",              1866,
  "Skive, Skive",      "Struer",              1865,
  "Struer",            "Thisted",             1882,
  "Ringkøbing (2387702632)", "Lemvig",        1879,
  "Ringkøbing (2387702632)", "Skjern",        1875, 
  "Skjern",            "Varde",               1875,
  "Skjern",            "Skanderborg",         1882,
  "Esbjerg",           "Varde",               1874,
  "Aalborg",           "Thisted",             1904,
  "Glyngoere",         "Skive, Skive",        1884,
  "Fredericia",        "Vejle",               1868,
  "Frederikshavn",     "Skagen",              1890,
  "Frederikshavn",     "Hjoerring",           1871,
  "Faaborg",           "Odense",              1882,
  "Faaborg",           "Nyborg",              1897,
  "Aalborg",           "Randers",             1900,
  "Aalborg",           "Nibe",                1899,
  "Nyborg",            "Odense",              1865,
  "Nyborg",            "Svendborg",           1897,
  "Kerteminde",        "Odense",              1900,
  "Holstebro",         "Ringkøbing (2387702632)", 1875,
  "Bogense",           "Odense",              1882,
  "Assens",            "Odense",              1884,
  "Middelfart town",   "Odense",              1865,
  "Middelfart town",   "Bogense",             1911,
  "Nykoebing Falster", "Sakskøbing",          1874,
  "Nykoebing Falster", "Nysted",              1910,
  "Nykoebing Falster", "Stubbekøbing",        1911,
  "Maribo",            "Sakskøbing",          1874,
  "Maribo",            "Rødby",               1874,
  "Maribo",            "Nakskov",             1874
)



#####################


# ==== Compute Least Cost Paths between node pairs ====

# Function to calculate LCP between two towns
calculate_lcp <- function(cost_surface, town1_coords, town2_coords) {
  create_lcp(cost_surface,
             origin = town1_coords,
             destination = town2_coords,
             cost_distance = FALSE,
             check_locations = FALSE)
}

# Ensure we have coordinates for all nodes
nodes_sf <- st_transform(nodes_sf, 4326)
nodes_df <- nodes_sf %>%
  mutate(X = st_coordinates(.)[,1],
         Y = st_coordinates(.)[,2]) %>%
  st_drop_geometry()

# Prepare cost surfaces in a list
cost_surfaces <- list()
for (i in crit_slope_values) {
  cost_surfaces[[as.character(i)]] <- get(paste0("slope_cs_", i))
}

# Convert city_pairs (list) to tibble with id
town_pairs <- opening_years %>%
  mutate(id = row_number())

# Container for all slope results
lcp_sf_all_cost_surfaces <- list()

# Loop over cost surfaces
for (slope_label in names(cost_surfaces)) {
  slope_cs <- cost_surfaces[[slope_label]]
  lcp_sf_list <- list()
  
  for (i in seq_len(nrow(town_pairs))) {
    town1_name <- town_pairs$town1[i]
    town2_name <- town_pairs$town2[i]
    
    # Get coordinates
    town1_coords <- as.numeric(nodes_df[nodes_df$Market_town == town1_name, c("X","Y")])
    town2_coords <- as.numeric(nodes_df[nodes_df$Market_town == town2_name, c("X","Y")])
    
    if (length(town1_coords) == 2 & length(town2_coords) == 2) {
      lcp <- calculate_lcp(slope_cs, town1_coords, town2_coords)
      lcp_sf <- st_as_sf(lcp) %>%
        mutate(
          town_pair = paste0(town1_name, "_", town2_name),
          town1 = town1_name,
          town2 = town2_name,
          town_pair_id = town_pairs$id[i],
          crit_slope = slope_label,
          opened = town_pairs$opened[i]
        )
      lcp_sf_list[[paste0(town1_name, "_", town2_name)]] <- lcp_sf
    } else {
      message("Skipping pair: ", town1_name, " - ", town2_name, " (coords missing)")
    }
  }
  
  all_lcps_sf <- bind_rows(lcp_sf_list)
  
  # Store results
  lcp_sf_all_cost_surfaces[[slope_label]] <- all_lcps_sf
  
  # Save to shapefile
  out_path <- paste0("../../../Data not redistributable/Instrument_shapes/lcp_shape_files/LCP_scrit_", slope_label, ".shp")
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  st_write(all_lcps_sf, out_path, driver = "ESRI Shapefile", append = FALSE)
  
  message("Saved ", nrow(all_lcps_sf), " paths for slope crit=", slope_label)
}

########################
# === SANITY Check === #
########################

# === Plot year-by-year maps of actual railways + LCPs ===

# Pick the LCPs for your crit_slope (here: 12)
lcp_sf <- lcp_sf_all_cost_surfaces[["1"]]

# Path to PDF on Desktop
out_pdf <- "../../../Data not redistributable/instrument_shapes/railways_year_by_year.pdf"

pdf(out_pdf, width = 9, height = 9)

for (yr in sort(unique(town_pairs$opened))) {
  plot(st_geometry(outline_dk), 
       main = paste("Railways and LCPs up to", yr), 
       col = "grey90")
  
  # --- actual historical railways ---
  if ("opened" %in% names(shape_data)) {
    plot(st_geometry(shape_data[shape_data$opened <= yr, ]), 
         col = "black", lwd = 3, add = TRUE)   # thicker black lines
  } else if ("YEAR_OPEN" %in% names(shape_data)) {
    plot(st_geometry(shape_data[shape_data$YEAR_OPEN <= yr, ]), 
         col = "black", lwd = 3, add = TRUE)
  }
  
  # --- predicted LCPs ---
  plot(st_geometry(lcp_sf[lcp_sf$opened <= yr, ]), 
       col = "red", lwd = 3, add = TRUE)       # thicker red lines
  
  # --- towns (nodes) ---
  plot(st_geometry(nodes_sf),
       pch = 21,              # circle with border + fill
       bg = "yellow",         # fill color
       col = "black",         # border color
       cex = 1.2,             # size
       lwd = 2,
       add = TRUE)
}

dev.off()

message("Saved PDF to: ", out_pdf)






