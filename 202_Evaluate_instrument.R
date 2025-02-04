# Evaluating instrument performanc
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
library(foreach)
library(raster)
library(elevatr)
library(tidygeocoder)
library(ggrepel)


source("Data_cleaning_scripts/000_Functions.R") # Contains calc_rail()

# ==== Load and merge instruments + finding instrment with best fit (RMSE) ====
# Read files
real_rail = read_csv2("Data/Panel_of_railways_in_parishes.csv") %>%
  filter(Year < 1877) # filter because instrument only up to 1876

generated_rails = list.files("Data/Instruments")
instruments = foreach(f = generated_rails) %do% {
  data_f = read_csv2(paste0("Data/Instruments/", f)) %>%
    filter(Year < 1877) %>% 
    rename(
      Connected_rail_pred = Connected_rail,
      Distance_to_nearest_railway_pred = Distance_to_nearest_railway
    )
  
  # Join with real rail
  data_f = data_f %>% 
    left_join(real_rail, by = c("GIS_ID", "Year"))
  
  return(data_f)
}

# ==== Evaluate performance ====
# RMSE/Accuracy 
foreach(data_f = instruments, .combine = "bind_rows") %do% {
  data_f %>% 
    summarise(
      Accuracy = mean(Connected_rail_pred == Connected_rail, na.rm = TRUE),
      RMSE = sqrt(mean(Distance_to_nearest_railway_pred - Distance_to_nearest_railway, na.rm = TRUE)^2),
      RMSLE = sqrt(mean(log(Distance_to_nearest_railway_pred) - log(Distance_to_nearest_railway), na.rm = TRUE)^2),
      parameter = parameter[1]
    )
}

# ==== Confusion matrix ====
foreach(data_f = instruments) %do% {
  conf_mat = confusion_matrix(data_f)
  
  p1 = conf_mat %>% 
    mutate(
      Truth = relevel(factor(Truth), ref = "TRUE")
    ) %>% 
    ggplot(aes(x = Predicted, y = Truth, fill = Pct, label = label)) +
    geom_tile(color = "white") +
    geom_text(vjust = 1) +
    scale_fill_gradient(low = "white", high = "blue") +
    theme_bw() + 
    theme(
      legend.position = "bottom"
    ) + 
    labs(
      fill = "Pct of support", 
      title = "Confusion Matrix",
      subtitle = paste0("Parameter: ", unique(conf_mat$parameter)),
      x = "Predicted to be connected",
      y = "Actually connected"
    )
  
  f = paste0("../Plots/Instrument_confusion_matrix/Param_",unique(conf_mat$parameter), ".png")
  
  ggsave(f, plot = p1, width = 8, height = 6)
  
  return(1)
}

# Confusion matrix every year
foreach(i = seq(length(instruments))) %do% {
  data_f = instruments[[i]]
  # Create folder if it does not exist
  dir_for_plots = paste0("Plots/Instrument_confusion_matrix/By_year/", unique(data_f$parameter))
  if(!dir.exists(dir_for_plots)){
    dir.create(dir_for_plots)
  }
  
  foreach(y = unique(data_f$Year)) %do% {
    data_fy = data_f %>% filter(Year == y)
    conf_mat = confusion_matrix(data_fy)
    
    p1 = conf_mat %>% 
      mutate(
        Truth = relevel(factor(Truth), ref = "TRUE")
      ) %>% 
      ggplot(aes(x = Predicted, y = Truth, fill = Pct, label = label)) +
      geom_tile(color = "white") +
      geom_text(vjust = 1) +
      scale_fill_gradient(low = "white", high = "blue") +
      theme_bw() + 
      theme(
        legend.position = "bottom"
      ) + 
      labs(
        fill = "Pct of support", 
        title = paste("Confusion Matrix", y),
        subtitle = paste0("Parameter: ", unique(conf_mat$parameter)),
        x = "Predicted to be connected",
        y = "Actually connected"
      )
    
    f = paste0(dir_for_plots, "/Year", y ,"_",unique(conf_mat$parameter), ".png")
    
    ggsave(f, plot = p1, width = 8, height = 6)
    
    return(1)
  }
  
  return(1)
}

# ==== Predicted versus actual distance to railway ====
foreach(i = seq(length(instruments))) %do% {
  data_f = instruments[[i]]
  param_f = data_f$parameter %>% unique()
  
  # Create basic plot
  p1 = data_f %>% 
    ggplot(
      aes(Distance_to_nearest_railway_pred, Distance_to_nearest_railway)
    ) + 
    geom_point() + 
    theme_bw() +
    labs(
      y = "Distance to rail\n(Actual)",
      x = "Distance to rail\n(Predicted)"
    ) + 
    geom_smooth(method = "lm", se = FALSE)
  
  # RMSE
  RMSE = data_f %>% 
    summarise(
      RMSE = sqrt(mean(Distance_to_nearest_railway_pred - Distance_to_nearest_railway, na.rm = TRUE)^2)
    ) %>% unlist() %>% signif(3)
  
  # Rsq
  mod_tmp = lm(Distance_to_nearest_railway ~ Distance_to_nearest_railway_pred, data = data_f)
  Rsq = summary(mod_tmp)$r.squared %>% signif(3)
  
  # Add info to plot
  p1 = p1 + 
    labs(
      title = "Distance to rail versus predicted distance to rail",
      subtitle = paste0("Parameter: ", param_f, "   RMSE: ", RMSE, ";   R^2: ", Rsq)
    )
  
  f = paste0("Plots/Instrument_pred_vs_actual_dist_to_rail/Param_",param_f, ".png")
  
  ggsave(f, plot = p1, width = 8, height = 6)
}


