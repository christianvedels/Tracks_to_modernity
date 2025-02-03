# Railways: Generate instruments
#
# Date updated:   2025-02-03
# Auhtor:         Tom Görges
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


# ==== Parameters ====
# Define the range of crit_slope values
crit_slope_values = c(1:16)

# ==== Load data (Railway shape data and Outline of Denmark) ====
shape_data = st_read("../../Data not redistributable/Railways Fertner/jernbane_historisk_v050413/jernbane_historisk.shp") %>% st_transform(4326)
outline_dk = st_read("../../Data not redistributable/Outline DK/DNK_adm0.shp") %>% st_transform(4326)

# Reading in market towns
market_towns = read_delim("../Data/Market_towns.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE, locale =locale(encoding = "ISO-8859-1"))

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

# -------------

# Obtain elevation raster (from OpenStreetMap)
denmark_elev = get_elev_raster(outline_dk, z = 8, source = "osm", clip = "locations") # z(oom) = 9 used by package "movecost", probably need zoom = 10 or higher but my computer breaks down at this resolution

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
    "../../Data not redistributable/Instrument_shapes/lcp_slope_cost_surfaces/slope_cs_crit_",
    slope_label,
    ".rds"
  )
  
  # Save the 'slope_cs' object to a .rds file with the current crit_slope in the filename
  write_rds(slope_cs, file_path)
}

# ---------------------------------------------------------------------------------------
# ==== Load slope cost surfaces ==== #

# Loop to load slope_cs_1 to slope_cs_16
for (i in crit_slope_values) {
  
  # Construct the file path for each crit_slope value
  file_path = paste0(
    "../../Data not redistributable/Instrument_shapes/lcp_slope_cost_surfaces/slope_cs_crit_",
    i,
    ".rds"
  )
  
  # Dynamically assign the loaded rds file to a variable named slope_cs_1, slope_cs_2, etc.
  assign(paste0("slope_cs_", i), read_rds(file_path))
}


# ----------------------------------------------------------------------------------------------------------

# === Nodes / Market towns ====
# Calculate the median of the Pop1801 column
median_pop1801 <- median(market_towns$Pop1801, na.rm = TRUE)
Q3_pop1801 <- quantile(market_towns$Pop1801, probs = 0.75, na.rm = TRUE)

# Subset the data frame, sort and select
subset_market_towns <- market_towns %>% 
  filter(
    Pop1801 > Q3_pop1801
  ) %>% 
  dplyr::select(Market_town, Pop1801, lat, long)%>% 
  mutate(
    node_type = "75th percentile"
  )

# Take out nodes affected by German-Danish war
subset_market_towns <- subset_market_towns %>% 
  filter(!Market_town %in% c("Aabenraa", "Ribe", "Haderslev", "Toender", "Soenderborg"))

# "3 years in advance it had been decided to build a railway to Strandby (Esbjerg) (lit. 167, p. 11), and
# by the Railway Act of 1868 and 1870, it was decided that a West Jutland railway should also be built from
# Esbjerg via Varde and Ringkøbing to Holstebro, where it was connected to the already existing railway network. (Aageesen, p. 58)

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
subset_market_towns <- subset_market_towns %>% bind_rows(additional_nodes)

# keep df
nodes_df <- subset_market_towns

# create sf
nodes_sf <- st_as_sf(subset_market_towns,
                         coords = c("long", "lat"),
                         crs = 4326)


plot(dnk, main="Original Raster")
plot(nodes_sf$geometry, add = T, col = "yellow")

# === Creation of file that contains all unique GIS_IDs and their respective minimum distance to nodes

# Load shape files
shape_parishes <- read_sf("../../Data not redistributable/DK parish shapefile/Parish1820Counting1837.shp")

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
library(writexl)

write_xlsx(distance_to_nodes_df, "../Data/distance_to_nodes.xlsx")

# ----------------------------------

### Make data frame a Spatial points df
coordinates(nodes_df) = ~long+lat
proj4string(nodes_df) = CRS("+proj=longlat +datum=WGS84")

# Define market town pairs (routes) node to node
town_pairs = matrix(c("Koebenhavn", "Roskilde",
                       "Roskilde", "Korsør",
                       "Aarhus", "Randers",
                       "Aarhus", "Viborg",
                       "Koebenhavn", "Helsingoer",
                       "Nyborg", "Odense", 
                       "Odense", "Middelfart town",
                       "Holstebro", "Viborg",
                       "Fredericia", "Horsens",
                       "Horsens", "Aarhus",
                       "Aalborg", "Randers",
                       "Naestved", "Roskilde",
                       "Esbjerg", "Fredericia",
                       "Esbjerg", "Varde",
                       "Varde", "Ringkobing",
                       "Ringkobing", "Holstebro",
                       "Svendborg", "Odense"),
                     ncol = 2, byrow = TRUE) %>% 
  data.frame() %>% 
  mutate(
    id = 1:n()
  )

# TODO: Derive "Data/Opened_pairs.csv" from data here


# Define a function to calculate LCP between two towns for any cost surface
calculate_lcp = function(cost_surface, town1_coords, town2_coords) {
  lcp = create_lcp(cost_surface, town1_coords, town2_coords, cost_distance = FALSE, check_locations = FALSE)
  return(lcp)
}

# Create an empty list to store the LCPs for each cost surface as sf objects
lcp_sf_all_cost_surfaces = list()

# Check if the dataset is an sf object and extract coordinates accordingly
if (inherits(nodes_sf, "sf")) {
  coords = st_coordinates(nodes_sf)
  nodes_df = cbind(nodes_sf, coords)
}

# Create a list of all cost surfaces
cost_surfaces = list() # HERE

# Add cost surfaces for slope_cs_1 to slope_cs_16
for (i in crit_slope_values) {
  cost_surfaces[[as.character(i)]] <- get(paste0("slope_cs_", i))
}

# Iterate over each cost surface
for (slope_label in names(cost_surfaces)) {
  # Get the current cost surface
  slope_cs = cost_surfaces[[slope_label]]

  # Create an empty list to store the LCPs for this particular cost surface
  lcp_sf_list = list()

  # Iterate over each town pair and calculate LCPs
  for (i in 1:nrow(town_pairs)) {
    town1_name = town_pairs[i, 1]
    town2_name = town_pairs[i, 2]

    # Get the coordinates of each town from the market towns dataset
    town1_coords = nodes_df[nodes_df$Market_town == town1_name, c("X", "Y")]
    town2_coords = nodes_df[nodes_df$Market_town == town2_name, c("X", "Y")]

    # Ensure there are valid coordinates
    if (nrow(town1_coords) > 0 & nrow(town2_coords) > 0) {
      # Calculate the least cost path
      lcp = calculate_lcp(slope_cs, town1_coords, town2_coords)

      # Convert LCP to an sf object and store in the list
      lcp_sf = st_as_sf(lcp)

      # Create a new column for start and end town names
      lcp_sf = lcp_sf %>% 
        mutate(
          town_pair = paste0(town1_name, "_", town2_name),
          town1_coords = town1_coords %>% st_drop_geometry(),
          town2_coords = town1_coords %>% st_drop_geometry(),
          town_pair_id = town_pairs$id[i]
        )

      # Store the LCP with the town pair names
      lcp_sf_list[[paste0(town1_name, "_", town2_name)]] = lcp_sf
    } else {
      message(paste("Coordinates for", town1_name, "or", town2_name, "not found. Skipping..."))
    }
  }
  
  # Combine all LCPs for the current cost surface into a single sf object
  all_lcps_sf = do.call("bind_rows", lcp_sf_list)
  
  # Add the "opened" column
  opened = read_csv("../Data/Opened_pairs.csv") %>% dplyr::select(town_pair_id, opened)
  # Test of 'opened' has expected dims
  test1 = !all(sort(all_lcps_sf$town_pair_id) == sort(opened$town_pair_id))
  test2 = !(NROW(opened) == NROW(all_lcps_sf))
  if(test1 | test2){
    stop("'Data/Opened_pairs.csv' did not have expected content")
  }
  
  # Join opened info
  all_lcps_sf = all_lcps_sf %>% 
    full_join(opened, by = "town_pair_id")
    
  
  # Store the result in the overall list, keyed by the cost surface label
  lcp_sf_all_cost_surfaces[[slope_label]] = all_lcps_sf
  
  # Plot the LCPs for this cost surface
  plot(st_geometry(outline_dk), main = paste("Least Cost Paths for Cost Surface:", slope_label))
  plot(st_geometry(all_lcps_sf), add = TRUE, col = "blue", lwd = 2)
}

# Save LCPs for each cost surface with town_pair in shapefile
for (slope_label in names(lcp_sf_all_cost_surfaces)) {
  st_write(lcp_sf_all_cost_surfaces[[slope_label]], 
           paste0("../../Data not redistributable/Instrument_shapes/lcp_shape_files/LCP_scrit_", slope_label, ".shp"), 
           driver = "ESRI Shapefile",
           append = F) # replace existing file
}


test = st_read("../../Data not redistributable/Instrument_shapes/lcp_shape_files/LCP_scrit_1.shp")
plot(test$geometry)


