# Functions
#
# Date updated:   2023-09-15
# Auhtor:         Christian Vedel 
# Purpose:        Functions used in the rest of the project

# ==== Colors ====
colours = list(
  black = "black",
  blue = "#273a8f",
  green = "#2c5c34",
  red = "#b33d3d",
  orange = "#DE7500"
)

dims = list(
  width = 8,
  height = 6
)

# ==== pretty_pct(x, digits = 4) ====
# Makes pretty pct string
pretty_pct = function(x, digits = 4){
  x = signif(x, digits = digits)
  x = x*100
  x = paste(x,"%",sep ="")
  return(x)
}

# ==== MA(destination, origin) ====
# Computes the Market Acces at 'destination' of the objects in origin 
# If 'Year' appears in 'origin' it will return a panel

# destination:  st object with destinations where MP should be estimated 
# origin:       The objects to calculate MA of. Should contain long and lat.
# theta:        Distance elasticity (default: theta = 1)
# verbose:      Should updates be printed?

MA = function(destination, origin, theta = 1, verbose = FALSE){
  require(foreach)
    # destination = shape_parishes
    # origin = assembly_houses
  
  # Extract points
  destination_points = destination %>% 
    as.data.frame() %>% 
    select(GIS_ID, long, lat) %>% 
    st_as_sf(
      ., coords = c("long", "lat"),
      crs = st_crs(destination)
    )
  
  # Add fake year if no year
  if(!"Year" %in% names(origin)){
    origin$Year = 9999
  }
  
  # Add equal weight if no weight
  if(!"w" %in% names(origin)){
    origin$w = 1
  }
  
  # Remove missing
  origin = origin %>% drop_na(lat, long)
  
  # Convert origin to spatial object
  origin_sp = st_as_sf(
    origin, coords = c("long", "lat"),
    crs = st_crs(destination)
  )
  
  # Compute distance in loop
  res = foreach(y = sort(unique(origin_sp$Year)), .combine = "bind_rows") %do% {
    origin_y = origin_sp %>% 
      filter(Year == y)
    
    # Compute distance matrix
    distM_y = st_distance(origin_y, destination_points)
    
    # Convert to proper matrix
    distM_y = matrix(as.numeric(distM_y), ncol = NCOL(distM_y))
    
    # Convert to km
    distM_y = distM_y/1000
    
    # Add one to avoid exploding
    distM_y = distM_y + 1
    
    # Inverted distance (inverted by theta)
    distM_y = distM_y^(-theta)
    
    # Compute MA
    MA_y = apply(distM_y, 2, function(x){
      sum(x*origin_y$w) # Multiplied by weight
    })
    
    # Res
    res_y = data.frame(
      Year = y,
      GIS_ID = destination_points$GIS_ID,
      MA = MA_y
    )
    
    if(verbose){
      cat("\nEstimated MA for",y)
    }
    
    return(res_y)
  }
  
  return(res)
}

# ==== first_occurence ====
# Registers first occurence of a dummy.
# e.g. (0, 0, 1, 1) --> (0, 0, 1, 0)

testNA = function(x){ # Handle NA in test
  ifelse(is.na(x), FALSE, x)
}

first_occurence = function(x){
  res = rep(0, length(x))
  for(i in seq(length(x))){
    if(testNA(x[i] == 1)){
      res[i] = 1
      break
    }
  }
  return(res)
}

# ==== Calculate distance ====
calculate_distance = function(parish_geometry, railways) {
  # Compute parish centroid
  suppressWarnings({ # to avoid 'st_centroid assumes attributes are constant over geometries'
    parish_centroid = st_centroid(parish_geometry)
  })
  # Compute distance
  distances = st_distance(parish_centroid, railways)
  min_distance = min(distances, na.rm = TRUE)
  return(min_distance)
}

# ==== Railway panel for parishes ====
# shape:          Shape of railways (or instrumented railways)
# shape_parishes: Shape of parishes 
# verbose:        Should updates be printed?
# plots:          Should plots of each instance be constructed?
# id:             A descriptive identifying string
# years:          Which years should this run for?

calc_rail = function(shape,
                     shape_parishes,
                     verbose = TRUE,
                     plots = TRUE,
                     id = "placeholder",
                     years = 1846:2020) {
  
  # Convert to same coordinate system
  shape_parishes = st_transform(shape_parishes, st_crs(shape))
  
  # Run loop
  railways_panel = foreach(y = years, .combine = "bind_rows") %do% {
    
    # Subset to period
    shape_y = shape %>% subset(y >= opened & y <= fake_close)
    
    # Initiatilize empty df
    if (NROW(shape_y) == 0) {
      return(data.frame(
        GIS_ID = shape_parishes$GIS_ID,
        Connected_rail = 0,
        Year = y
      ))
    }
    
    # Test conncection
    connected_y = shape_parishes %>%
      st_intersects(shape_y) %>%
      as.character() %>%
      data.frame(Connected_rail = .) %>%
      mutate(Connected_rail = ifelse(Connected_rail == "integer(0)", 0, 1)) %>%
      mutate(GIS_ID = shape_parishes$GIS_ID)
    
    # Clean result
    result_y = data.frame(GIS_ID = shape_parishes$GIS_ID) %>%
      left_join(connected_y, by = "GIS_ID") %>%
      mutate(Connected_rail = ifelse(is.na(Connected_rail), 0, Connected_rail))
    
    # Test validity
    if (any(!result_y$Connected_rail %in% c(0, 1))) {
      stop("This should not be possible")
    }
    
    # Calculate the distances to the nearest railway using foreach
    if(verbose){
      cat("\nCalculating distances\n")
    }
    
    distances = foreach(i = 1:NROW(shape_parishes), .combine = "c") %do% {
      dist_i = calculate_distance(shape_parishes[i, ], shape_y)
      dist_i = as.numeric(dist_i) / 1000 # Convert to km
      if(verbose){
        cat(paste0(i," is ", round(dist_i, 2), "m from rail in ", y, "                               \r"))
      }
      
      return(dist_i)
    }
    cat("\n")
    result_y$Distance_to_nearest_railway = distances
    
    parishes_covered = sum(result_y$Connected_rail)
    
    
    # Make plot
    if(plots){
      # Convert to 'yes' / 'no' to plot
      shape_parishes$tmp = ifelse(result_y$Connected_rail == 1, "Yes", "No")
      
      # Distance 
      shape_parishes$Dist = result_y$Distance_to_nearest_railway
      
      # Connected plots
      p1 = ggplot() +
        layer_spatial(aes(fill = tmp, col = tmp),
                      size = 0,
                      data = shape_parishes) +
        layer_spatial(data = shape_y) +
        theme_bw() +
        labs(
          title = paste0("Year ", y),
          subtitle = paste0(
            parishes_covered,
            " of ",
            NROW(shape_parishes),
            " parishes covered (",
            pretty_pct(parishes_covered / NROW(shape_parishes)),
            ")"
          ),
          caption = "Data from Fertner (2013)",
          col = "Parish connected:",
          fill = "Parish connected:"
        ) +
        theme(legend.position = "bottom")
      
      # Distance to rail
      p2 = ggplot() +
        layer_spatial(aes(fill = Dist, col = Dist),
                      size = 0,
                      data = shape_parishes) +
        layer_spatial(data = shape_y) +
        theme_bw() +
        labs(
          title = paste0("Year ", y),
          subtitle = paste0(
            parishes_covered,
            " of ",
            NROW(shape_parishes),
            " parishes covered (",
            pretty_pct(parishes_covered / NROW(shape_parishes)),
            ")"
          ),
          caption = "Data from Fertner (2013)",
          col = "Distance to rail (km):",
          fill = "Distance to rail (km):"
        ) +
        theme(legend.position = "bottom") + 
        scale_fill_gradient(
          low = "#273a8f",
          high = "#DE7500"
        ) + 
        scale_color_gradient(
          low = "#273a8f",
          high = "#DE7500"
        )
      
      # Save plots
      fname1 = paste0("Plots/Plots_parish_connected/", id ,"N_Y", y, ".png")
      fname2 = paste0("Plots/Plots_distance_to_rail/", id ,"N_Y", y, ".png")
      w = 6
      ggsave(fname1,
             plot = p1,
             width = w,
             height = 0.9 * w)
      ggsave(fname2,
             plot = p2,
             width = w,
             height = 0.9 * w)
      
    }
    
    cat(y, "          \r")
    
    
    # Results to df
    result_y = result_y %>%
      mutate(Year = y)
    return(result_y)
  }
  
  
  return(railways_panel)
}


# ==== Confusion matrix =====
# Computes confusion matrix for data_f in 009_Instruments_to_panel_data.R

confusion_matrix = function(data_f){
  param_f = data_f$parameter %>% unique()
  
  conf_mat = data_f %>% 
    summarise(
      Support_positive = sum(Connected_rail, na.rm = TRUE),
      Support_negative = sum(1-Connected_rail, na.rm = TRUE),
      True_negative = sum(Connected_rail_pred == 0 & Connected_rail == 0, na.rm = TRUE),
      True_positive = sum(Connected_rail_pred == 1 & Connected_rail == 1, na.rm = TRUE),
      False_negative = sum(Connected_rail_pred == 0 & Connected_rail == 1, na.rm = TRUE),
      False_positive = sum(Connected_rail_pred == 1 & Connected_rail == 0, na.rm = TRUE)
    )  %>%
    pivot_longer(cols = True_negative:False_positive, names_to = "Category", values_to = "Count") %>% 
    rowwise() %>% 
    mutate(
      Truth = strsplit(Category, "_")[[1]][1],
      Predicted = strsplit(Category, "_")[[1]][2]
    ) %>% 
    mutate(
      support = case_when(
        Predicted == "negative" ~ Support_negative,
        Predicted == "positive" ~ Support_positive
      )
    ) %>% 
    group_by(Predicted) %>% 
    mutate(
      Pct = Count/support
    ) %>% 
    mutate(
      label = paste0(
        Count,"\n",
        "(", signif(Pct, 5)*100, "%)"
      )
    ) %>% 
    mutate(
      Predicted = case_when(
        Category == "False_negative" ~ FALSE,
        Category == "True_negative" ~ FALSE,
        Category == "False_positive" ~ TRUE,
        Category == "True_positive" ~ TRUE,
      ),
      Truth = case_when(
        Category == "False_negative" ~ TRUE,
        Category == "True_negative" ~ FALSE,
        Category == "False_positive" ~ FALSE,
        Category == "True_positive" ~ TRUE,
      )
    ) %>% 
    mutate(
      parameter = param_f
    )
  
  return(conf_mat)
}

# ==== impute_it() ====
impute_it = function(x, min_year, max_year) {
  # Create a data frame with the full range of years
  full_years = data.frame(midpoint_year = min_year:max_year)
  
  # Join with the input data
  joined_data = left_join(full_years, x, by = "midpoint_year")
  
  # Initialize the result with joined_data
  result = joined_data
  
  # Loop over each column that starts with "emotion_score_"
  for (col_name in names(joined_data)[grepl("^emotion_score_", names(joined_data))]) {
    # Extract the column data
    col_data = joined_data[[col_name]]
    
    # Interpolate the data if not all values are NA
    if (sum(!is.na(col_data))>1) {
      interpolated_values = approx(
        x = joined_data$midpoint_year,
        y = col_data,
        xout = min_year:max_year,
        rule = 2
      )$y
    } else {
      interpolated_values = col_data
    }
    
    # Assign the interpolated values to the result
    result[[col_name]] = interpolated_values
    
    # Create imputation flags
    result[[paste0("flag_", col_name)]] = ifelse(is.na(joined_data[[col_name]]), 1, 0)
  }
  
  return(result)
}

# ==== Better as.numeric ====
as_numeric0 = function(x){
  if(is.factor(x)){
    x = as.character(x)
    x = as.numeric(x)
  } else {
    x = as.numeric(x)
  }
  return(x)
}

# ==== sub_scandi ====
# this substitutes scandinavian letters
sub_scandi = function(x){
  scandi_letters = c("Æ",
                     "æ",
                     "Ø",
                     "ø",
                     "Å",
                     "å")
  
  replacement = c("Ae",
                  "ae",
                  "Oe",
                  "oe",
                  "Aa",
                  "aa")
  
  for(i in 1:length(scandi_letters)){
    x = gsub(
      scandi_letters[i],
      replacement[i],
      x
    )
  }
  
  return(x)
  
}



# ==== Min treat year for CS estimator ====
min_treat_year = function(year, treat){
  if(mean(treat) == 0){
    return(0)
  }

  min_treat = year[treat == 1] %>% min()

  if(min_treat < 1700){
    stop("There is no way that the treatment is before 1700")
  }

  return(min_treat)
}


# ==== signif0 ====
signif0 = function(x, digits = 4){
  if(is.numeric(x)){
    x = signif(x, digits = digits)
  }
  return(x)
}


# ==== extract_res (from cs estimates) ====
extract_res = function(summary_results){
  
  extracted_res = lapply(
    summary_results, function(x){
      t = x$overall.att / x$overall.se
      p = pnorm(abs(t), lower.tail = FALSE)*2
      data.frame(
        "Estimate" = x$overall.att,
        "SE" = x$overall.se,
        "t" = t,
        "p" = p,
        n = x$DIDparams$n,
        n_parishes = length(unique(x$DIDparams$data$GIS_ID)),
        control_group = x$DIDparams$control_group,
        mean_outcome = mean(unlist(x$DIDparams$data[x$DIDparams$yname])),
        outcome = x$DIDparams$yname
      )
    }
  )

  extracted_res = do.call(bind_rows, extracted_res)


  return(extracted_res)
}
