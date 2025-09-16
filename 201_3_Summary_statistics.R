# Regressions
#
# Date updated:   2025-02-13
# Author:         Christian Vedel, Tom Görges
# Purpose:        Runs regressions

# ==== Libraries ====
library(tidyverse)
library(fixest)
library(did)
library(kableExtra) # for latex tables
source("Data_cleaning_scripts/000_Functions.R")

# ==== Params ====
CONTROLS = "Dist_hamb_year + Dist_cph_year + Pop1801_year + county_by_year" # These are decile by year FE, now excluded: Dist_ox_year
NSIGNIF = 4 # Significant digits in all tables

# ==== Load data ====
census = read_csv2("Data/REGRESSION_DATA_Demography.csv", guess_max = 100000)
grundtvig = read_csv2("Data/REGRESSION_DATA_Grundtvigianism.csv", guess_max = 100000)
rail_panel = read_csv2("Data/Panel_of_railways_in_parishes.csv", guess_max = 100000)

# ==== Renaming =====
census = census %>% rename(
  Connected_railway = RailAccess,
  Connected_lcp = LCPAccess
)

grundtvig = grundtvig %>% rename(
  Connected_railway = RailAccess,
  Connected_lcp = LCPAccess
)

# Only same GIS_IDs
census = census %>% filter(GIS_ID %in% grundtvig$GIS_ID)
grundtvig = grundtvig %>% filter(GIS_ID %in% census$GIS_ID)

# Zeros are NAs in MA
grundtvig = grundtvig %>% mutate(
  MA_assembly = case_when(
    is.na(MA_assembly) ~ 0,
    TRUE ~ MA_assembly
  ),
  MA_folkhigh = case_when(
    is.na(MA_folkhigh) ~ 0,
    TRUE ~ MA_folkhigh
  )
)

# Outcome names 
outcomeNames = function(x){
  case_when(
    x == "lnPopulation" ~ "log(Population)",
    x == "lnpop1801" ~ "log(Population 1801)",	
    x == "lnChild_women_ratio" ~ "log(Child-women ratio + 1)",
    x == "lnManufacturing" ~ "log(Manufacturing + 1)",
    x == "lnNotAgriculture" ~ "log(Not agriculture + 1)",
    x == "HISCAM_avg" ~ "HISCAM_avg",
    x == "lnMigration" ~ "log(Migration)",
    x == "dist_hmb" ~ "Distance to Hamburg",
    x == "dist_cph" ~ "Distance to Copenhagen",
    x == "Boulder_clay_pct" ~ "Boulder clay (%)",
    x == "area_parish" ~ "Area of parish",
    x == "DistOxRoad" ~ "Distance to Oxroad",
    x == "Distance_market_town" ~ "Distance to market town",
    TRUE ~ x
  )
}

outcomeNames_grundtvig = function(x){
  case_when(
    x == "Assembly_house" ~ "Assembly house",
    x == "HighSchool" ~ "Folk high school",
    x == "MA_assembly" ~ "Density Assembly houses (MA)",
    x == "MA_folkhigh" ~ "Density Folk high schools (MA)",
    TRUE ~ x
  )
}

# ==== Create subsamples for IV and CS (IV reduced form) ====
# create census cs
census_cs = census %>% 
  filter(invalid_comparison == 0) %>%
  filter(away_from_node == 1)

# create grundtvig_cs
grundtvig_cs = grundtvig %>%
  filter(invalid_comparison == 0) %>%
  filter(away_from_node == 1)

# create census iv
census_iv = census %>% 
  filter(away_from_node == 1)

# create grundtvig_iv
grundtvig_iv = grundtvig %>%
  filter(away_from_node == 1)

# Grundtvig decade
grundtvig_decade = grundtvig %>%
  filter(Year < 1920) # Only one year included

# === Summary Statistics ===
summary_tables = function(){
  sum_table_census = census %>%
    select(
      Population,
      lnManufacturing,
      lnNotAgriculture,
      Child_women_ratio,
      HISCAM_avg,
      Migration,
      Connected_railway,
      Connected_lcp
    ) %>%
    pivot_longer(
      cols = c(
        Population,
        lnManufacturing,
        lnNotAgriculture,
        Child_women_ratio,
        HISCAM_avg,
        Migration,
        Connected_railway,
        Connected_lcp
      ),
      names_to = "var"
    ) %>%
    mutate(
      var = outcomeNames(var)
    ) %>%
    group_by(var) %>%
    summarise(
      n = sum(!is.na(value)),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE)
    ) %>%
    mutate_all(signif0, digits = NSIGNIF) %>%
    ungroup()
  
  sum_table_grundtvig = grundtvig %>%
    select(
      Assembly_house,
      HighSchool,
      MA_assembly,
      MA_folkhigh,
      Connected_railway,
      Connected_lcp
    ) %>%
    pivot_longer(
      cols = c(
        Assembly_house,
        HighSchool,
        MA_assembly,
        MA_folkhigh,
        Connected_railway,
        Connected_lcp
      ),
      names_to = "var"
    ) %>%
    mutate(
      var = case_when(
        var == "Assembly_house" ~ "Share of parishes with an assembly house",
        var == "HighSchool" ~ "Share of parishes with a folk high school",
        var == "MA_assembly" ~ "Local density of assembly houses (MA)",
        var == "MA_folkhigh" ~ "Local density of folk high schools (MA)",
        TRUE ~ var
      )
    ) %>%
    group_by(var) %>%
    summarise(
      n = sum(!is.na(value)),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE)
    ) %>%
    mutate_all(signif0, digits = NSIGNIF) %>%
    ungroup()
  
  summary_stats = bind_rows(
    sum_table_census,
    sum_table_grundtvig
  )
  
  # Create tex table
  sum_tex = summary_stats %>%
    kable(
      format = "latex",
      booktabs = TRUE,
      caption = "Summary Statistics",
      col.names = c("Variable", "N", "Mean", "SD", "Min", "Max"),
      align = "lcccccc"
    ) %>%
    kable_styling(
      latex_options = c("hold_position", "scale_down")
    )# %>%
  # group_rows("A. Economy", 1, NROW(sum_table_census)) %>%
  # group_rows("B. Grundtvig", NROW(sum_table_census) + 1, NROW(summary_stats))
  
  sink("Tables/Summary_Statistics.txt")
  print(sum_tex)
  sink()
  print(sum_tex) # To display in console when running the script
  
}

# ==== Densities ====
census_distributions = function(){
  tmp = census %>%
    group_by(GIS_ID) %>%
    mutate(Ever_rail = case_when(mean(Connected_railway) > 0 ~ "Yes", TRUE ~ "No")) %>%
    filter(Year == 1850)
  
  already_connected = tmp$Connected_railway %>% sum()
  total_parishes = nrow(tmp)
  sink("Tables/Note_connected.txt")
  cat("Number of parishes already connected to the railway in 1850: ", already_connected, "of", total_parishes, "\n")
  sink()
  cat("Number of parishes already connected to the railway in 1850: ", already_connected, "of", total_parishes, "\n")
  
  p1 = tmp %>%
    filter(Connected_railway == 0) %>%   # Exclude parishes with railways already 
    mutate(
      lnpop1801 = log(Pop1801)
    ) %>%
    select(
      Ever_rail, 
      lnPopulation, 
      lnChild_women_ratio, 
      lnManufacturing, 
      lnNotAgriculture, 
      HISCAM_avg, 
      lnMigration,
      dist_hmb,
      dist_cph,
      DistOxRoad,
      lnpop1801
    ) %>%
    pivot_longer(
      cols = c(
        lnPopulation, 
        lnChild_women_ratio, 
        lnManufacturing, 
        lnNotAgriculture, 
        HISCAM_avg, 
        lnMigration,
        dist_hmb,
        dist_cph,
        DistOxRoad,
        lnpop1801
      ), 
      names_to = "var"
    ) %>%
    mutate(
      var = outcomeNames(var)
    ) %>%
    ggplot(aes(x = value, fill = Ever_rail)) +
    geom_density(alpha = 0.5) + 
    facet_wrap(~var, scales = "free", ncol = 3) +  # columns layout
    scale_fill_manual(values = c("Yes" = colours$red, "No" = colours$black)) + # Better color contrast
    theme_minimal(base_size = 14) + 
    labs(fill = "Was it eventually connected to the railway?") +
    theme(
      legend.position = "bottom",
      legend.title = element_text(),
      strip.text = element_text(face = "bold", size = 12),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) + 
    theme_bw() +
    labs(
      x = "",
      y = ""
    ) + 
    theme(legend.position = "bottom")
  
  print(p1)
  
  ggsave("Plots/Densities_census.png", p1, width = dims$width, height = dims$height)
}

# Distributions comparing treated to not yet treated
census_distributions_by_year = function(){
  tmp = census %>%
    group_by(GIS_ID) %>%
    mutate(Ever_rail = case_when(mean(Connected_railway) > 0 ~ "Yes", TRUE ~ "No")) %>%
    filter(Year == 1850)
  
  p1 = tmp %>%
    filter(Connected_railway == 0) %>%   # Exclude parishes with railways already 
    mutate(
      lnpop1801 = log(Pop1801)
    ) %>%
    select(
      Treat_year,
      Ever_rail, 
      lnPopulation, 
      lnChild_women_ratio, 
      lnManufacturing, 
      lnNotAgriculture, 
      HISCAM_avg, 
      lnMigration,
      dist_hmb,
      dist_cph,
      DistOxRoad,
      lnpop1801
    ) %>%
    pivot_longer(
      cols = c(
        lnPopulation, 
        lnChild_women_ratio, 
        lnManufacturing, 
        lnNotAgriculture, 
        HISCAM_avg, 
        lnMigration,
        dist_hmb,
        dist_cph,
        DistOxRoad,
        lnpop1801
      ), 
      names_to = "var"
    ) %>%
    mutate(
      var = outcomeNames(var)
    ) %>%
    mutate(
      Treat_year = ifelse(
        Treat_year == 0,
        "Never",
        as.character(Treat_year)
      )
    ) %>%
    ggplot(aes(x = value, fill = Treat_year)) +
    geom_density(alpha = 0.25) + 
    facet_wrap(~var, scales = "free", ncol = 3) +  # columns layout
    scale_fill_manual(
      values = c(
        "Never" = colours$black, 
        "1850" = colours$red,
        "1860" = colours$blue,
        "1880" = colours$green,
        "1901" = colours$orange
      )
    ) +
    theme_minimal(base_size = 14) + 
    labs(fill = "When was it connected to the railway?") +
    theme(
      legend.position = "bottom",
      legend.title = element_text(),
      strip.text = element_text(face = "bold", size = 12),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) + 
    theme_bw() +
    labs(
      x = "",
      y = ""
    ) + 
    theme(legend.position = "bottom")
  
  print(p1)
  
  ggsave("Plots/Densities_census_treat_year.png", p1, width = dims$width, height = dims$height)
}


# Grundtvig
grundtvig_distributions_over_time = function(){
  p1 = grundtvig %>%
    group_by(GIS_ID) %>%
    mutate(Ever_rail = case_when(mean(Connected_railway) > 0 ~ "Yes", TRUE ~ "No")) %>%
    select(Year, Ever_rail, Assembly_house, HighSchool) %>%
    pivot_longer(
      cols = c(Assembly_house, HighSchool),
      names_to = "var"
    ) %>%
    mutate(
      var = case_when(
        var == "Assembly_house" ~ "Share of parishes with an assembly house",
        var == "HighSchool" ~ "Share of parishes with a folk high school"
      )
    ) %>%
    group_by(var, Year, Ever_rail) %>%
    summarise(
      share = mean(value, na.rm = TRUE)
    ) %>%
    ggplot(aes(x = Year, y = share, col = Ever_rail)) +
    geom_line() +
    geom_point() + 
    facet_wrap(~var, scales = "free", ncol = 1) + 
    theme_bw() + 
    scale_color_manual(values = c("Yes" = colours$red, "No" = colours$black)) + 
    labs(col = "Was it eventually connected to the railway?") + 
    theme(legend.position = "bottom") + 
    labs(y  = "")
  
  print(p1)
  
  ggsave("Plots/Grundtvig_over_time.png", p1, width = dims$width, height = 1.25*dims$height)
}