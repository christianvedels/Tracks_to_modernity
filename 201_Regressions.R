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
CONTROLS = "Dist_hamb_year + Dist_cph_year + Dist_ox_year + Pop1801_year + county_by_year" # These are decile by year FE
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

  ggsave("Plots/Densities_census.png", p1, width = dims$width, height = dims$height, create.dir = TRUE)
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

  ggsave("Plots/Densities_census_treat_year.png", p1, width = dims$width, height = dims$height, create.dir = TRUE)
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

  ggsave("Plots/Grundtvig_over_time.png", p1, width = dims$width, height = 1.25*dims$height, create.dir = TRUE)
}

# ==== Function to make plots of cs estimates ====
save_plots = function(plots, outcome_names, xformula, name = "Census", mult = 1, ylab = NA, xlab = NA, omit_legend = F){
  for(i in seq(length(plots))){
    p = plots[[i]]
    print(p)
    # Name based on controls or not
    if(xformula == 1){
      fname = paste("Plots/", name, "/", names(plots)[i], "_", outcome_names[i], ".png", sep = "")
    } else {
      fname = paste("Plots/", name, "_control/", names(plots)[i], "_", outcome_names[i], ".png", sep = "")
    }

    # Replace all " " with "_" in fname
    fname = gsub(" ", "_", fname)

    if(is.na(ylab)){
      ylab = outcomeNames(outcome_names[i])
    }

    if(is.na(xlab)){
      xlab = "Year"
    }

    p = p + 
      labs(
        y = ylab,
        x = xlab,
        title = NULL
      ) + 
      theme_bw() + 
      scale_x_continuous() + 
      theme(
        axis.text.x = element_text(angle = 90)
      ) + 
      scale_color_manual(values = c("1" = colours$red, "0" = colours$black), labels = c("0"="Not connected", "1"="Connected")) +
      theme(legend.position = "bottom")
      labs(
        color = "Connected to railway:"
      )

    if(omit_legend){
      p = p + theme(legend.position = "none")
    }

    # Create dir
    dir.create(dirname(fname), showWarnings = FALSE)
    ggsave(fname, p, width = mult*dims$width, height = mult*dims$height, limitsize = FALSE, create.dir = TRUE)
  }
}

# ==== TWFE regressions (Census data) ====
twfe_regressions_census = function(xformula = "1"){
  form1 = as.formula(paste("lnPopulation ~ Connected_railway | GIS_ID + Year +", xformula))
  twfe1 = feols(
    form1,
    data = census,
    cluster = ~ GIS_ID
  )

  form2 = as.formula(paste("lnChild_women_ratio ~ Connected_railway | GIS_ID + Year +", xformula))
  twfe2 = feols(
    form2,
    data = census,
    cluster = ~ GIS_ID
  )

  form3 = as.formula(paste("lnManufacturing ~ Connected_railway | GIS_ID + Year +", xformula))
  twfe3 = feols(
    form3,
    data = census,
    cluster = ~ GIS_ID
  )

  form4 = as.formula(paste("lnNotAgriculture ~ Connected_railway | GIS_ID + Year +", xformula))
  twfe4 = feols(
    form4,
    data = census,
    cluster = ~ GIS_ID
  )

  form5 = as.formula(paste("HISCAM_avg ~ Connected_railway | GIS_ID + Year +", xformula))
  twfe5 = feols(
    form5,
    data = census,
    cluster = ~ GIS_ID
  )

  form6 = as.formula(paste("lnMigration ~ Connected_railway | GIS_ID + Year +", xformula))
  twfe6 = feols(
    form6,
    data = census,
    cluster = ~ GIS_ID
  )

  res = etable(twfe1, twfe2, twfe3, twfe4, twfe5, twfe6,
       fitstat = ~ n + my,  # number of observations
       cluster = "GIS_ID", # Display the clustering
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1), # Custom significance codes
       tex = TRUE,
       keep = "Connected_railway" # Only include "Connected_railway"
       )
  
  # Tabel name depending on whether xformula is 1 or not
  if (xformula == "1") {
    table_name = "TWFE_regressions_census"
  } else {
    table_name = "TWFE_regressions_census_controls"
  }

  sink(paste("Tables/", table_name, ".txt", sep = ""))
  print(res)
  sink()

  print(res) # To display in console when running the script

}


# ==== TWFE regressions (Grundtvig data) ====
twfe_regressions_grundtvig = function(xformula = "1"){
  form1 = as.formula(paste("Assembly_house ~ Connected_railway | GIS_ID + Year +", xformula))
  twfe1 = feols(
    form1,
    data = grundtvig,
    cluster = ~ GIS_ID
  )

  form2 = as.formula(paste("HighSchool  ~ Connected_railway | GIS_ID + Year +", xformula))
  twfe2 = feols(
    form2,
    data = grundtvig,
    cluster = ~ GIS_ID
  )

  form3 = as.formula(paste("MA_assembly ~ Connected_railway | GIS_ID + Year +", xformula))
  twfe3 = feols(
    form3,
    data = grundtvig,
    cluster = ~ GIS_ID
  )

  form4 = as.formula(paste("MA_folkhigh ~ Connected_railway | GIS_ID + Year +", xformula))
  twfe4 = feols(
    form4,
    data = grundtvig,
    cluster = ~ GIS_ID
  )

  res = etable(twfe1, twfe2, twfe3, twfe4,
       fitstat = ~ n + my,  # Include number of observations
       cluster = "GIS_ID", # Display the clustering
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1), # Custom significance codes
       tex = TRUE,
       keep = "Connected_railway" # Only include "Connected_railway"
       )
  
  # Tabel name depending on whether xformula is 1 or not
  if (xformula == "1") {
    table_name = "TWFE_regressions_grundtvig"
  } else {
    table_name = "TWFE_regressions_grundtvig_controls"
  }

  sink(paste("Tables/", table_name, ".txt", sep = ""))
  print(res)
  sink()

  print(res) # To display in console when running the script

}

# ==== CS estimates (census) ====
cs_estimates_census = function(xformula = "1"){
  # log(Pop)
  cs_mod1 = att_gt(
    yname = "lnPopulation",    
    tname = "Year_num",        
    idname = "GIS_ID_num",     
    gname = "Treat_year",      
    xformla = as.formula(paste("~", xformula)), 
    data = census,        
    clustervars = "GIS_ID",
    control_group = "notyettreated"
  )

  # Child-woman ratio
  cs_mod2 = att_gt(
    yname = "lnChild_women_ratio",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year",       # First year of treatment
    xformla = as.formula(paste("~", xformula)),     
    data = census,              # Your dataset
    clustervars = "GIS_ID",      # Cluster variable
    control_group = "notyettreated"
  )

  # log Manufacturing
  cs_mod3 = att_gt(
    yname = "lnManufacturing",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year",       # First year of treatment
    xformla = as.formula(paste("~", xformula)),            
    data = census,              # Your dataset
    clustervars = "GIS_ID",      # Cluster variable
    control_group = "notyettreated"
  )

  # log Manufacturing
  cs_mod4 = att_gt(
    yname = "lnNotAgriculture",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year",       # First year of treatment
    xformla = as.formula(paste("~", xformula)),              
    data = census,              # Your dataset
    clustervars = "GIS_ID",      # Cluster variable
    control_group = "notyettreated"
  )

  # HISCAM_avg
  cs_mod5 = att_gt(
    yname = "HISCAM_avg",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year",       # First year of treatment
    xformla = as.formula(paste("~", xformula)),              
    data = census,              # Your dataset
    clustervars = "GIS_ID",      # Cluster variable
    control_group = "notyettreated"
  )

  # Migration
  cs_mod6 = att_gt(
    yname = "lnMigration",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year",       # First (observed) year of treatment
    xformla = as.formula(paste("~", xformula)),      
    data = census,              # Your dataset
    clustervars = "GIS_ID",      # Cluster variable
    control_group = "notyettreated"
  )

  # Aggregate the ATT
  simple_sum_res = list(
    agg_mod1 = aggte(cs_mod1, type = "simple", na.rm = T),
    agg_mod2 = aggte(cs_mod2, type = "simple", na.rm = T),
    agg_mod3 = aggte(cs_mod3, type = "simple", na.rm = T),
    agg_mod4 = aggte(cs_mod4, type = "simple", na.rm = T),
    agg_mod5 = aggte(cs_mod5, type = "simple", na.rm = T),
    agg_mod6 = aggte(cs_mod6, type = "simple", na.rm = T)
  )

  # Outcome names
  outcome_names = lapply(simple_sum_res, function(x){
    x$DIDparams$yname
  }) %>% unlist()

  # Construct table
  res = extract_res(simple_sum_res) %>%
    mutate(
      # Stars
      Estimate = stars(Estimate, p, NSIGNIF)
    ) %>%
    mutate(
      SE = paste0("(", signif0(SE, NSIGNIF), ")")
    ) %>%
    mutate_all(signif0, digits = NSIGNIF) %>%
    mutate_all(as.character) %>%
    pivot_longer(cols = c("Estimate", "SE", "t", "p", "n", "n_parishes", "mean_outcome"), names_to = "stat") %>% 
    pivot_wider(names_from = "outcome", values_from = "value") %>%
    filter(stat %in% c("Estimate", "SE", "n", "n_parishes", "mean_outcome")) %>%
    mutate(
      stat = case_when(
        stat == "Estimate" ~ "Connected railway",
        stat == "SE" ~ "",
        stat == "n" ~ "N Obs.",
        stat == "n_parishes" ~ "N parishes",
        stat == "mean_outcome" ~ "Mean of Outcome"
      )
    ) %>%
    select(-control_group) %>%
    rename(` ` = stat)
  
  
  # Tex table
  res = kable(
    res,
    booktabs = T, 
    caption = "Railways and Local Development", 
    row.names = F,
    align = "lcccccc",
    linesep = "",  # Suppress additional line spacing
    format = "latex",
    escape = FALSE
  )

  if(xformula == 1){
    table_name = "CS_estimates_census"
  } else {
    table_name = "CS_estimates_census_controls"
  }

  sink(paste("Tables/", table_name, ".txt", sep = ""))
  print(res)
  sink()
  print(res) # To display in console when running the script

  # Group table
  group_sum_res = list(
    agg_mod1 = aggte(cs_mod1, type = "group", na.rm = T),
    agg_mod2 = aggte(cs_mod2, type = "group", na.rm = T),
    agg_mod3 = aggte(cs_mod3, type = "group", na.rm = T),
    agg_mod4 = aggte(cs_mod4, type = "group", na.rm = T),
    agg_mod5 = aggte(cs_mod5, type = "group", na.rm = T),
    agg_mod6 = aggte(cs_mod6, type = "group", na.rm = T)
  )

  # Construct table
  stat_order = c("Estimate", "SE", "n", "n_parishes", "mean_outcome")
  res = extract_res(group_sum_res, grouped = TRUE) %>%
    mutate(
      # Stars
      Estimate = stars(Estimate, p, NSIGNIF)
    ) %>%
    mutate(
      SE = paste0("(", signif0(SE, NSIGNIF), ")")
    ) %>%
    mutate_all(signif0, digits = NSIGNIF) %>%
    mutate_all(as.character) %>%
    pivot_longer(cols = c("Estimate", "SE", "t", "p", "n", "n_parishes", "mean_outcome"), names_to = "stat") %>% 
    pivot_wider(names_from = "outcome", values_from = "value") %>%
    filter(stat %in% c("Estimate", "SE", "n", "n_parishes", "mean_outcome")) %>%
    mutate(stat = factor(stat, levels = stat_order)) %>%
    arrange(stat) %>%
    mutate(
      stat = case_when(
        stat == "Estimate" ~ "Connected railway",
        stat == "SE" ~ "",
        stat == "n" ~ "N Obs.",
        stat == "n_parishes" ~ "N parishes",
        stat == "mean_outcome" ~ "Mean of Outcome"
      )
    ) %>%
    mutate(
      stat = ifelse(stat == "Connected railway", paste("Connected railway", group), stat)
    ) %>%
    select(-control_group, -group) %>%
    rename(` ` = stat) %>%
    distinct()

  res = kable(
    res,
    booktabs = T, 
    caption = "Railways and Local Development", 
    row.names = F,
    align = "lcccccc",
    linesep = "",  # Suppress additional line spacing
    format = "latex",
    escape = FALSE
  )

  if(xformula == 1){
    table_name = "CS_estimates_census_grouped"
  } else {
    table_name = "CS_estimates_census_grouped_controls"
  }

  sink(paste("Tables/", table_name, ".txt", sep = ""))
  print(res)
  sink()
  print(res) # To display in console when running the script

  # Plots full
  plots_full = list(
    p1 = cs_mod1 %>% ggdid(),
    p2 = cs_mod2 %>% ggdid(),
    p3 = cs_mod3 %>% ggdid(),
    p4 = cs_mod4 %>% ggdid(),
    p5 = cs_mod5 %>% ggdid(),
    p6 = cs_mod6 %>% ggdid()
  )
  
  # Plots dynamic
  plots_dynamic = list(
    p1 = aggte(cs_mod1, type = "dynamic", na.rm = T) %>% ggdid(),
    p2 = aggte(cs_mod2, type = "dynamic", na.rm = T) %>% ggdid(),
    p3 = aggte(cs_mod3, type = "dynamic", na.rm = T) %>% ggdid(),
    p4 = aggte(cs_mod4, type = "dynamic", na.rm = T) %>% ggdid(),
    p5 = aggte(cs_mod5, type = "dynamic", na.rm = T) %>% ggdid(),
    p6 = aggte(cs_mod6, type = "dynamic", na.rm = T) %>% ggdid()
  )

  # Plots calendar
  plots_calendar = list(
    p1 = aggte(cs_mod1, type = "calendar", na.rm = T) %>% ggdid(),
    p2 = aggte(cs_mod2, type = "calendar", na.rm = T) %>% ggdid(),
    p3 = aggte(cs_mod3, type = "calendar", na.rm = T) %>% ggdid(),
    p4 = aggte(cs_mod4, type = "calendar", na.rm = T) %>% ggdid(),
    p5 = aggte(cs_mod5, type = "calendar", na.rm = T) %>% ggdid(),
    p6 = aggte(cs_mod6, type = "calendar", na.rm = T) %>% ggdid()
  )

  # Plots group
  plots_group = list(
    p1 = aggte(cs_mod1, type = "group", na.rm = T) %>% ggdid(),
    p2 = aggte(cs_mod2, type = "group", na.rm = T) %>% ggdid(),
    p3 = aggte(cs_mod3, type = "group", na.rm = T) %>% ggdid(),
    p4 = aggte(cs_mod4, type = "group", na.rm = T) %>% ggdid(),
    p5 = aggte(cs_mod5, type = "group", na.rm = T) %>% ggdid(),
    p6 = aggte(cs_mod6, type = "group", na.rm = T) %>% ggdid()
  )

  # Save plots
  save_plots(plots_full, outcome_names, xformula, name = "Census_full")
  save_plots(plots_dynamic, outcome_names, xformula, name = "Census_dynamic")
  save_plots(plots_calendar, outcome_names, xformula, name = "Census_calendar", mult = 0.3, ylab = "", omit_legend = T)
  save_plots(plots_group, outcome_names, xformula, name = "Census_group", mult = 0.3, ylab = "Group", xlab = "Effect", omit_legend = T)
}

# ==== CS estimates (Grundtvig data) ====
cs_estimates_grundtvig = function(xformula = "1"){
  # Assembly house
  cs_mod1 = att_gt(
    yname = "Assembly_house",          
    tname = "Year_num",             
    idname = "GIS_ID_num",          
    gname = "Treat_year",       
    xformla = as.formula(paste("~", xformula)),  
    data = grundtvig,              
    clustervars = "GIS_ID",      
    control_group = "notyettreated"
  )

  # HighSchool
  cs_mod2 = att_gt(
    yname = "HighSchool",          
    tname = "Year_num",            
    idname = "GIS_ID_num",         
    gname = "Treat_year",       
    xformla = as.formula(paste("~", xformula)), 
    data = grundtvig,
    clustervars = "GIS_ID",
    control_group = "notyettreated"
  )

  cs_mod3 = att_gt(
    yname = "MA_assembly",          
    tname = "Year_num",            
    idname = "GIS_ID_num",         
    gname = "Treat_year",       
    xformla = as.formula(paste("~", xformula)), 
    data = grundtvig,
    clustervars = "GIS_ID",
    control_group = "notyettreated"
  )

  cs_mod4 = att_gt(
    yname = "MA_folkhigh",          
    tname = "Year_num",            
    idname = "GIS_ID_num",         
    gname = "Treat_year",       
    xformla = as.formula(paste("~", xformula)), 
    data = grundtvig,
    clustervars = "GIS_ID",
    control_group = "notyettreated"
  )

  # Aggregate the ATT
  simple_sum_res = list(
    agg_mod1 = aggte(cs_mod1, type = "simple", na.rm = T),
    agg_mod2 = aggte(cs_mod2, type = "simple", na.rm = T),
    agg_mod3 = aggte(cs_mod3, type = "simple", na.rm = T),
    agg_mod4 = aggte(cs_mod4, type = "simple", na.rm = T)
  )

  # Outcome names
  outcome_names = lapply(simple_sum_res, function(x){
    x$DIDparams$yname
  }) %>% unlist() %>% outcomeNames_grundtvig

  # Construct table
  res = extract_res(simple_sum_res) %>%
    mutate(
      # Stars
      Estimate = stars(Estimate, p, NSIGNIF)
    ) %>%
    mutate(
      SE = paste0("(", signif0(SE, NSIGNIF), ")")
    ) %>%
    mutate_all(signif0, digits = NSIGNIF) %>%
    mutate_all(as.character) %>%
    pivot_longer(cols = c("Estimate", "SE", "t", "p", "n", "n_parishes", "mean_outcome"), names_to = "stat") %>% 
    mutate(outcome = outcomeNames_grundtvig(outcome)) %>%
    pivot_wider(names_from = "outcome", values_from = "value") %>% 
    filter(stat %in% c("Estimate", "SE", "n", "n_parishes", "mean_outcome")) %>% 
    mutate(
      stat = case_when(
        stat == "Estimate" ~ "Connected railway",
        stat == "SE" ~ "",
        stat == "n" ~ "N Obs.",
        stat == "n_parishes" ~ "N parishes",
        stat == "mean_outcome" ~ "Mean of Outcome"
      )
    ) %>% 
    select(-control_group) %>% 
    rename(` ` = stat)

  # Tex table
  res = kable(
    res,
    booktabs = T, 
    caption = "Railways and Grundtvigianism", 
    row.names = F,
    align = "lcccccc",
    linesep = "",  # Suppress additional line spacing
    format = "latex",
    escape = FALSE
  )

  if(xformula == 1){
    table_name = "CS_estimates_grundtvig"
  } else {
    table_name = "CS_estimates_grundtvig_controls"
  }

  sink(paste("Tables/", table_name, ".txt", sep = ""))
  print(res)
  sink()
  print(res) # To display in console when running the script

  # Group table
  group_sum_res = list(
    agg_mod1 = aggte(cs_mod1, type = "group", na.rm = T),
    agg_mod2 = aggte(cs_mod2, type = "group", na.rm = T),
    agg_mod3 = aggte(cs_mod3, type = "group", na.rm = T),
    agg_mod4 = aggte(cs_mod4, type = "group", na.rm = T)
  )

  # Construct table
  stat_order = c("Estimate", "SE", "n", "n_parishes", "mean_outcome")
  res = extract_res(group_sum_res, grouped = TRUE) %>%
    mutate(
      # Stars
      Estimate = stars(Estimate, p, NSIGNIF)
    ) %>%
    mutate(
      SE = paste0("(", signif0(SE, NSIGNIF), ")")
    ) %>%
    mutate_all(signif0, digits = NSIGNIF) %>%
    mutate_all(as.character) %>%
    pivot_longer(cols = c("Estimate", "SE", "t", "p", "n", "n_parishes", "mean_outcome"), names_to = "stat") %>% 
    pivot_wider(names_from = "outcome", values_from = "value") %>%
    filter(stat %in% c("Estimate", "SE", "n", "n_parishes", "mean_outcome")) %>%
    mutate(stat = factor(stat, levels = stat_order)) %>%
    arrange(stat) %>%
    mutate(
      stat = case_when(
        stat == "Estimate" ~ "Connected railway",
        stat == "SE" ~ "",
        stat == "n" ~ "N Obs.",
        stat == "n_parishes" ~ "N parishes",
        stat == "mean_outcome" ~ "Mean of Outcome"
      )
    ) %>%
    mutate(
      stat = ifelse(stat == "Connected railway", paste("Connected railway", group), stat)
    ) %>%
    select(-control_group, -group) %>%
    rename(` ` = stat) %>%
    distinct()

  res = kable(
    res,
    booktabs = T, 
    caption = "Railways and Local Development", 
    row.names = F,
    align = "lcccccc",
    linesep = "",  # Suppress additional line spacing
    format = "latex",
    escape = FALSE
  )

  if(xformula == 1){
    table_name = "CS_estimates_grundtvig_grouped"
  } else {
    table_name = "CS_estimates_grundtvig_grouped_controls"
  }

  sink(paste("Tables/", table_name, ".txt", sep = ""))
  print(res)
  sink()
  print(res) # To display in console when running the script

  # Plots full
  plots_full = list(
    p1 = cs_mod1 %>% ggdid(),
    p2 = cs_mod2 %>% ggdid(),
    p3 = cs_mod3 %>% ggdid(),
    p4 = cs_mod4 %>% ggdid()
  )
  
  # Plots dynamic
  plots_dynamic = list(
    p1 = aggte(cs_mod1, type = "dynamic", na.rm = T) %>% ggdid(),
    p2 = aggte(cs_mod2, type = "dynamic", na.rm = T) %>% ggdid(),
    p3 = aggte(cs_mod3, type = "dynamic", na.rm = T) %>% ggdid(),
    p4 = aggte(cs_mod4, type = "dynamic", na.rm = T) %>% ggdid()
  )

  # Plots calendar
  plots_calendar = list(
    p1 = aggte(cs_mod1, type = "calendar", na.rm = T) %>% ggdid(),
    p2 = aggte(cs_mod2, type = "calendar", na.rm = T) %>% ggdid(),
    p3 = aggte(cs_mod3, type = "calendar", na.rm = T) %>% ggdid(),
    p4 = aggte(cs_mod4, type = "calendar", na.rm = T) %>% ggdid()
  )

  # Plots group
  plots_group = list(
    p1 = aggte(cs_mod1, type = "group", na.rm = T) %>% ggdid(),
    p2 = aggte(cs_mod2, type = "group", na.rm = T) %>% ggdid(),
    p3 = aggte(cs_mod3, type = "group", na.rm = T) %>% ggdid(),
    p4 = aggte(cs_mod4, type = "group", na.rm = T) %>% ggdid()
  )

  # Save plots
  save_plots(plots_full, outcome_names, xformula, name = "Grundtvig_full", mult = 10)
  save_plots(plots_dynamic, outcome_names, xformula, name = "Grundtvig_dynamic")
  save_plots(plots_calendar, outcome_names, xformula, name = "Grundtvig_calendar", mult = 0.5, ylab = "", omit_legend = T)
  save_plots(plots_group, outcome_names, xformula, name = "Grundtvig_group", mult = 0.3, ylab = "Group", xlab = "Effect", omit_legend = T)
  
  
}

# ==== cs estimates by decade (grundtvig) ====
cs_estimates_grundtvig_decade = function(xformula = "1"){
  # Assembly house
  cs_mod1 = att_gt(
    yname = "Assembly_house",
    tname = "decade", # MA_folkhigh  
    idname = "GIS_ID_num",      
    gname = "Treat_year_broad",       
    xformla = as.formula(paste("~", xformula)),
    data = grundtvig_decade,
    clustervars = "GIS_ID",
    panel = FALSE, # Repeated cross seciton of ~10 observations per parish per decade
    control_group = "notyettreated"
  )

  # HighSchool
  cs_mod2 = att_gt(
    yname = "HighSchool", 
    tname = "decade",             
    idname = "GIS_ID_num",          
    gname = "Treat_year_broad",       
    xformla = as.formula(paste("~", xformula)),               
    data = grundtvig_decade,              
    clustervars = "GIS_ID",
    panel = FALSE, # Repeated cross seciton of ~10 observations per parish per decade
    control_group = "notyettreated"
  )

  # Assembly house
  cs_mod3 = att_gt(
    yname = "MA_assembly", 
    tname = "decade", # MA_folkhigh  
    idname = "GIS_ID_num",      
    gname = "Treat_year_broad",       
    xformla = as.formula(paste("~", xformula)),
    data = grundtvig_decade,
    clustervars = "GIS_ID",
    panel = FALSE, # Repeated cross seciton of ~10 observations per parish per decade
    control_group = "notyettreated"
  )

  # HighSchool
  cs_mod4 = att_gt(
    yname = "MA_folkhigh", 
    tname = "decade",             
    idname = "GIS_ID_num",          
    gname = "Treat_year_broad",       
    xformla = as.formula(paste("~", xformula)),               
    data = grundtvig_decade,              
    clustervars = "GIS_ID",
    panel = FALSE, # Repeated cross seciton of ~10 observations per parish per decade
    control_group = "notyettreated"
  )

  # Aggregate the ATT
  simple_sum_res = list(
    agg_mod1 = aggte(cs_mod1, type = "simple", na.rm = T),
    agg_mod2 = aggte(cs_mod2, type = "simple", na.rm = T),
    agg_mod3 = aggte(cs_mod3, type = "simple", na.rm = T),
    agg_mod4 = aggte(cs_mod4, type = "simple", na.rm = T)
  )

  # Outcome names
  outcome_names = lapply(simple_sum_res, function(x){
    x$DIDparams$yname
  }) %>% unlist()
  
  # Construct table
  res = extract_res(simple_sum_res) %>%
    mutate(
      # Stars
      Estimate = stars(Estimate, p, NSIGNIF)
    ) %>%
    mutate(
      SE = paste0("(", signif0(SE, NSIGNIF), ")")
    ) %>%
    mutate_all(signif0, digits = NSIGNIF) %>%
    mutate_all(as.character) %>%
    pivot_longer(cols = c("Estimate", "SE", "t", "p", "n", "n_parishes", "mean_outcome"), names_to = "stat") %>% 
    pivot_wider(names_from = "outcome", values_from = "value") %>% 
    filter(stat %in% c("Estimate", "SE", "n", "n_parishes", "mean_outcome")) %>% 
    mutate(
      stat = case_when(
        stat == "Estimate" ~ "Connected railway",
        stat == "SE" ~ "",
        stat == "n" ~ "N Obs.",
        stat == "n_parishes" ~ "N parishes",
        stat == "mean_outcome" ~ "Mean of Outcome"
      )
    ) %>% 
    select(-control_group) %>% 
    rename(` ` = stat)

  # Tex table
  res = kable(
    res,
    booktabs = T, 
    caption = "Railways and Grundtvigianism", 
    row.names = F,
    align = "lcccccc",
    linesep = "",  # Suppress additional line spacing
    format = "latex",
    escape = FALSE
  )

  if(xformula == 1){
    table_name = "CS_estimates_grundtvig_decade"
  } else {
    table_name = "CS_estimates_grundtvig_decade_controls"
  }

  sink(paste("Tables/", table_name, ".txt", sep = ""))
  print(res)
  sink()
  print(res) # To display in console when running the script

  # Group table
  group_sum_res = list(
    agg_mod1 = aggte(cs_mod1, type = "group", na.rm = T),
    agg_mod2 = aggte(cs_mod2, type = "group", na.rm = T),
    agg_mod3 = aggte(cs_mod3, type = "group", na.rm = T),
    agg_mod4 = aggte(cs_mod4, type = "group", na.rm = T)
  )

  # Construct table
  stat_order = c("Estimate", "SE", "n", "n_parishes", "mean_outcome")
  res = extract_res(group_sum_res, grouped = TRUE) %>%
    mutate(
      # Stars
      Estimate = stars(Estimate, p, NSIGNIF)
    ) %>%
    mutate(
      SE = paste0("(", signif0(SE, NSIGNIF), ")")
    ) %>%
    mutate_all(signif0, digits = NSIGNIF) %>%
    mutate_all(as.character) %>%
    pivot_longer(cols = c("Estimate", "SE", "t", "p", "n", "n_parishes", "mean_outcome"), names_to = "stat") %>% 
    pivot_wider(names_from = "outcome", values_from = "value") %>%
    filter(stat %in% c("Estimate", "SE", "n", "n_parishes", "mean_outcome")) %>%
    mutate(stat = factor(stat, levels = stat_order)) %>%
    arrange(stat, group) %>%
    mutate(
      stat = case_when(
        stat == "Estimate" ~ "Connected railway",
        stat == "SE" ~ "",
        stat == "n" ~ "N Obs.",
        stat == "n_parishes" ~ "N parishes",
        stat == "mean_outcome" ~ "Mean of Outcome"
      )
    ) %>%
    mutate(
      stat = ifelse(stat == "Connected railway", paste("Connected railway", group), stat)
    ) %>%
    select(-control_group, -group) %>%
    rename(` ` = stat) %>%
    distinct()

  res = kable(
    res,
    booktabs = T, 
    caption = "Railways and Local Development", 
    row.names = F,
    align = "lcccccc",
    linesep = "",  # Suppress additional line spacing
    format = "latex",
    escape = FALSE
  )

  if(xformula == 1){
    table_name = "CS_estimates_grundtvig_decade_grouped"
  } else {
    table_name = "CS_estimates_grundtvig_decade_grouped_controls"
  }

  sink(paste("Tables/", table_name, ".txt", sep = ""))
  print(res)
  sink()
  print(res) # To display in console when running the script

  # Plots full
  plots_full = list(
    p1 = cs_mod1 %>% ggdid(),
    p2 = cs_mod2 %>% ggdid(),
    p3 = cs_mod3 %>% ggdid(),
    p4 = cs_mod4 %>% ggdid()
  )
  
  # Plots dynamic
  plots_dynamic = list(
    p1 = aggte(cs_mod1, type = "dynamic", na.rm = T) %>% ggdid(),
    p2 = aggte(cs_mod2, type = "dynamic", na.rm = T) %>% ggdid(),
    p3 = aggte(cs_mod3, type = "dynamic", na.rm = T) %>% ggdid(),
    p4 = aggte(cs_mod4, type = "dynamic", na.rm = T) %>% ggdid()
  )

  # Plots calendar
  plots_calendar = list(
    p1 = aggte(cs_mod1, type = "calendar", na.rm = T) %>% ggdid(),
    p2 = aggte(cs_mod2, type = "calendar", na.rm = T) %>% ggdid(),
    p3 = aggte(cs_mod3, type = "calendar", na.rm = T) %>% ggdid(),
    p4 = aggte(cs_mod4, type = "calendar", na.rm = T) %>% ggdid()
  )

  # Plots group
  plots_group = list(
    p1 = aggte(cs_mod1, type = "group", na.rm = T) %>% ggdid(),
    p2 = aggte(cs_mod2, type = "group", na.rm = T) %>% ggdid(),
    p3 = aggte(cs_mod3, type = "group", na.rm = T) %>% ggdid(),
    p4 = aggte(cs_mod4, type = "group", na.rm = T) %>% ggdid()
  )

  # Save plots
  save_plots(plots_full, outcome_names, xformula, name = "Grundtvig_decade_full", mult = 1)
  save_plots(plots_dynamic, outcome_names, xformula, name = "Grundtvig_decade_dynamic")
  save_plots(plots_calendar, outcome_names, xformula, name = "Grundtvig_decade_calendar", mult = 0.5, ylab = "", omit_legend = T)
  save_plots(plots_group, outcome_names, xformula, name = "Grundtvig_decade_group", mult = 0.3, ylab = "Group", xlab = "Effect", omit_legend = T)
}

# ==== TSLS regressions (Census data) ====
tsls_regressions_census = function(xformula = "1"){
  form1 = as.formula(paste("lnPopulation ~ 1 | GIS_ID + Year + ", xformula, "| Connected_railway ~ Connected_lcp"))
  tsls1 = feols(
    form1,  # Full formula including covariates, fixed effects, and IV
    data = census_iv,
    cluster = ~ GIS_ID  # Clustering
  )

  form2 = as.formula(paste("lnChild_women_ratio ~ 1 | GIS_ID + Year + ", xformula, "| Connected_railway ~ Connected_lcp"))
  tsls2 = feols(
    form2,
    data = census_iv,
    cluster = ~ GIS_ID
  )

  form3 = as.formula(paste("lnManufacturing ~ 1 | GIS_ID + Year + ", xformula, "| Connected_railway ~ Connected_lcp"))
  tsls3 = feols(
    form3,
    data = census_iv,
    cluster = ~ GIS_ID
  )

  form4 = as.formula(paste("lnNotAgriculture ~ 1 | GIS_ID + Year + ", xformula, "| Connected_railway ~ Connected_lcp"))
  tsls4 = feols(
    form4,
    data = census_iv,
    cluster = ~ GIS_ID
  )

  form5 = as.formula(paste("HISCAM_avg ~ 1 | GIS_ID + Year + ", xformula, "| Connected_railway ~ Connected_lcp"))
  tsls5 = feols(
    form5,
    data = census_iv,
    cluster = ~ GIS_ID
  )

  form6 = as.formula(paste("lnMigration ~ 1 | GIS_ID + Year + ", xformula, "| Connected_railway ~ Connected_lcp"))
  tsls6 = feols(
    form6,
    data = census_iv,
    cluster = ~ GIS_ID
  )

  res = etable(tsls1, tsls2, tsls3, tsls4, tsls5, tsls6,
       fitstat = ~ n + my,  # Number of observations
       cluster = "GIS_ID", # Display the clustering
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1), # Custom significance codes
       tex = TRUE,
       keep = "Connected_railway" # Only include "Connected_railway"
       )

  res_1st_stage = tsls1$iv_first_stage %>%
    etable(
      fitstat = ~ ivf,
      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
      tex = TRUE
    )

  # Tabel name depending on whether xformula is 1 or not
  if (xformula == "1") {
    table_name = "TSLS_regressions_census"
  } else {
    table_name = "TSLS_regressions_census_controls"
  }

  sink(paste("Tables/", table_name, ".txt", sep = ""))
  print(res)
  print(res_1st_stage)
  list(tsls1, tsls2, tsls3, tsls4, tsls5, tsls6) %>%
    lapply(function(x) print(x$iv_first_stage))
  sink()

  print(res) # To display in console when running the script  
}

# ==== TSLS regressions (Grundtvig data) ====
tsls_regressions_grundtvig = function(xformula = "1"){
  form1 = as.formula(paste("Assembly_house ~ 1 | GIS_ID + Year + ", xformula, "| Connected_railway ~ Connected_lcp"))
  tsls1 = feols(
    form1,
    data = grundtvig_iv,
    cluster = ~ GIS_ID
  )

  form2 = as.formula(paste("HighSchool ~ 1 | GIS_ID + Year + ", xformula, "| Connected_railway ~ Connected_lcp"))
  tsls2 = feols(
    form2,
    data = grundtvig_iv,
    cluster = ~ GIS_ID
  )

  form3 = as.formula(paste("MA_assembly ~ 1 | GIS_ID + Year + ", xformula, "| Connected_railway ~ Connected_lcp"))
  tsls3 = feols(
    form3,
    data = grundtvig_iv,
    cluster = ~ GIS_ID
  )

  form4 = as.formula(paste("MA_folkhigh ~ 1 | GIS_ID + Year + ", xformula, "| Connected_railway ~ Connected_lcp"))
  tsls4 = feols(
    form4,
    data = grundtvig_iv,
    cluster = ~ GIS_ID
  )

  res = etable(tsls1, tsls2, tsls3, tsls4,
       fitstat = ~ n + my,  # Number of observations
       cluster = "GIS_ID", # Display the clustering
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1), # Custom significance codes
       tex = TRUE,
       keep = "Connected_railway" # Only include "Connected_railway"
       )

  res_1st_stage = tsls1$iv_first_stage %>%
    etable(
      fitstat = ~ ivf,
      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
      tex = TRUE
    )

  # Tabel name depending on whether xformula is 1 or not
  if (xformula == "1") {
    table_name = "TSLS_regressions_grundtvig"
  } else {
    table_name = "TSLS_regressions_grundtvig_controls"
  }

  sink(paste("Tables/", table_name, ".txt", sep = ""))
  print(res)
  print(res_1st_stage)
  list(tsls1, tsls2) %>%
    lapply(function(x) print(x$iv_first_stage))
  sink()

  print(res) # To display in console when running the script
}


# ==== Reduced form regressions (Census data) ====
reduced_form_regressions_census = function(xformula = "1"){
  form1 = as.formula(paste("lnPopulation ~ Connected_lcp | GIS_ID + Year +", xformula))
  twfe1_red = feols(
    form1,
    data = census_cs,
    cluster = ~ GIS_ID
  )

  form2 = as.formula(paste("lnChild_women_ratio ~ Connected_lcp | GIS_ID + Year +", xformula))
  twfe2_red = feols(
    form2,
    data = census_cs,
    cluster = ~ GIS_ID
  )

  form3 = as.formula(paste("lnManufacturing ~ Connected_lcp | GIS_ID + Year +", xformula))
  twfe3_red = feols(
    form3,
    data = census_cs,
    cluster = ~ GIS_ID
  )

  form4 = as.formula(paste("lnNotAgriculture ~ Connected_lcp | GIS_ID + Year +", xformula))
  twfe4_red = feols(
    form4,
    data = census_cs,
    cluster = ~ GIS_ID
  )

  form5 = as.formula(paste("HISCAM_avg ~ Connected_lcp | GIS_ID + Year +", xformula))
  twfe5_red = feols(
    form5,
    data = census_cs,
    cluster = ~ GIS_ID
  )

  form6 = as.formula(paste("lnMigration ~ Connected_lcp | GIS_ID + Year +", xformula))
  twfe6_red = feols(
    form6,
    data = census_cs,
    cluster = ~ GIS_ID
  )

  res = etable(twfe1_red, twfe2_red, twfe3_red, twfe4_red, twfe5_red, twfe6_red,
       fitstat = ~ n + my,  # Number of observations
       cluster = "GIS_ID", # Display the clustering
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1), # Custom significance codes
       tex = TRUE,
       keep = "Connected_lcp" # Only include "Connected_lcp"
       )

  # Tabel name depending on whether xformula is 1 or not
  if (xformula == "1") {
    table_name = "Reduced_form_regressions_census"
  } else {
    table_name = "Reduced_form_regressions_census_controls"
  }

  sink(paste("Tables/", table_name, ".txt", sep = ""))
  print(res)
  sink()

  print(res) # To display in console when running the script
}

# ==== Reduced form regressions (Census data) ====
reduced_form_regressions_grundtvig = function(xformula = "1"){
  form1 = as.formula(paste("Assembly_house ~ Connected_lcp | GIS_ID + Year + ", xformula))
  twfe1_red = feols(
    form1,
    data = grundtvig_cs,
    cluster = ~ GIS_ID
  )

  form2 = as.formula(paste("HighSchool ~ Connected_lcp | GIS_ID + Year + ", xformula))
  twfe2_red = feols(
    form2,
    data = grundtvig_cs,
    cluster = ~ GIS_ID
  )

  form3 = as.formula(paste("MA_assembly ~ Connected_lcp | GIS_ID + Year + ", xformula))
  twfe3_red = feols(
    form3,
    data = grundtvig_cs,
    cluster = ~ GIS_ID
  )

  form4 = as.formula(paste("MA_folkhigh ~ Connected_lcp | GIS_ID + Year + ", xformula))
  twfe4_red = feols(
    form4,
    data = grundtvig_cs,
    cluster = ~ GIS_ID
  )
  
  res = etable(twfe1_red, twfe2_red, twfe3_red, twfe4_red,
       fitstat = ~ n + my,  # Number of observations
       cluster = "GIS_ID", # Display the clustering
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1), # Custom significance codes
       tex = TRUE,
       keep = "Connected_lcp" # Only include "Connected_lcp"
       )

  # Tabel name depending on whether xformula is 1 or not
  if (xformula == "1") {
    table_name = "Reduced_form_regressions_grundtvig"
  } else {
    table_name = "Reduced_form_regressions_grundtvig_controls"
  }

  sink(paste("Tables/", table_name, ".txt", sep = ""))
  print(res)
  sink()

  print(res) # To display in console when running the script
}

# ==== Reduced form regressions cs (Census data) ====
cs_reduced_form_regressions_census = function(xformula = "1"){
  cs_mod1 = att_gt(
    yname = "lnPopulation",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year_instr",       # First year of treatment
    xformla = as.formula(paste("~", xformula)),              # No covariates 
    data = census_cs,              # Your dataset
    clustervars = "GIS_ID",      # Cluster variable
    control_group = "notyettreated"
  )

  cs_mod2 = att_gt(
    yname = "lnChild_women_ratio",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year_instr",       # First year of treatment
    xformla = as.formula(paste("~", xformula)),              # No covariates (consistent with TWFE)
    data = census_cs,              # Your dataset
    clustervars = "GIS_ID",      # Cluster variable
    control_group = "notyettreated"
  )

  cs_mod3 = att_gt(
    yname = "lnManufacturing",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year_instr",       # First year of treatment
    xformla = as.formula(paste("~", xformula)),               # No covariates (consistent with TWFE)
    data = census_cs,              # Your dataset
    clustervars = "GIS_ID",      # Cluster variable
    control_group = "notyettreated"
  )

  cs_mod4 = att_gt(
    yname = "lnNotAgriculture",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year_instr",       # First year of treatment
    xformla = as.formula(paste("~", xformula)),              # No covariates (consistent with TWFE)
    data = census_cs,              # Your dataset
    clustervars = "GIS_ID",      # Cluster variable
    control_group = "notyettreated"
  )

  cs_mod5 = att_gt(
    yname = "HISCAM_avg",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year_instr",       # First year of treatment
    xformla = as.formula(paste("~", xformula)),             # No covariates (consistent with TWFE)
    data = census_cs,              # Your dataset
    clustervars = "GIS_ID",      # Cluster variable
    control_group = "notyettreated"
  )

  cs_mod6 = att_gt(
    yname = "lnMigration",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year_instr",       # First (observed) year of treatment
    xformla = as.formula(paste("~", xformula)),              # No covariates (consistent with TWFE)
    data = census_cs,              # Your dataset
    clustervars = "GIS_ID",      # Cluster variable
    control_group = "notyettreated"
  )

  simple_sum_res = list(
    agg_mod1 = aggte(cs_mod1, type = "simple", na.rm = T),
    agg_mod2 = aggte(cs_mod2, type = "simple", na.rm = T),
    agg_mod3 = aggte(cs_mod3, type = "simple", na.rm = T),
    agg_mod4 = aggte(cs_mod4, type = "simple", na.rm = T),
    agg_mod5 = aggte(cs_mod5, type = "simple", na.rm = T),
    agg_mod6 = aggte(cs_mod6, type = "simple", na.rm = T)
  )

  outcome_names = lapply(simple_sum_res, function(x){
    x$DIDparams$yname
  }) %>% unlist()

  res = extract_res(simple_sum_res) %>%
    mutate(
      # Stars
      Estimate = stars(Estimate, p, NSIGNIF)
    ) %>%
    mutate(
      SE = paste0("(", signif0(SE, NSIGNIF), ")")
    ) %>%
    mutate_all(signif0, digits = NSIGNIF) %>%
    mutate_all(as.character) %>%
    pivot_longer(cols = c("Estimate", "SE", "t", "p", "n", "n_parishes", "mean_outcome"), names_to = "stat") %>% 
    pivot_wider(names_from = "outcome", values_from = "value") %>%
    filter(stat %in% c("Estimate", "SE", "n", "n_parishes", "mean_outcome")) %>%
    mutate(
      stat = case_when(
        stat == "Estimate" ~ "Connected railway",
        stat == "SE" ~ "",
        stat == "n" ~ "N Obs.",
        stat == "n_parishes" ~ "N parishes",
        stat == "mean_outcome" ~ "Mean of Outcome"
      )
    ) %>%
    select(-control_group) %>%
    rename(` ` = stat)

  res = kable(
    res,
    booktabs = T, 
    caption = "Railways and Local Development", 
    row.names = F,
    align = "lcccccc",
    linesep = "",  # Suppress additional line spacing
    format = "latex",
    escape = FALSE
  )

  if(xformula == 1){
    table_name = "CS_reduced_form_estimates_census"
  } else {
    table_name = "CS_reduced_form_estimates_census_controls"
  }

  sink(paste("Tables/", table_name, ".txt", sep = ""))
  print(res)
  sink()

  print(res) # To display in console when running the script

  # Plots full
  plots_full = list(
    p1 = cs_mod1 %>% ggdid(),
    p2 = cs_mod2 %>% ggdid(),
    p3 = cs_mod3 %>% ggdid(),
    p4 = cs_mod4 %>% ggdid(),
    p5 = cs_mod5 %>% ggdid(),
    p6 = cs_mod6 %>% ggdid()
  )

  # Plots dynamic
  plots_dynamic = list(
    p1 = aggte(cs_mod1, type = "dynamic", na.rm = T) %>% ggdid(),
    p2 = aggte(cs_mod2, type = "dynamic", na.rm = T) %>% ggdid(),
    p3 = aggte(cs_mod3, type = "dynamic", na.rm = T) %>% ggdid(),
    p4 = aggte(cs_mod4, type = "dynamic", na.rm = T) %>% ggdid(),
    p5 = aggte(cs_mod5, type = "dynamic", na.rm = T) %>% ggdid(),
    p6 = aggte(cs_mod6, type = "dynamic", na.rm = T) %>% ggdid()
  )

  # Plots calendar
  plots_calendar = list(
    p1 = aggte(cs_mod1, type = "calendar", na.rm = T) %>% ggdid(),
    p2 = aggte(cs_mod2, type = "calendar", na.rm = T) %>% ggdid(),
    p3 = aggte(cs_mod3, type = "calendar", na.rm = T) %>% ggdid(),
    p4 = aggte(cs_mod4, type = "calendar", na.rm = T) %>% ggdid(),
    p5 = aggte(cs_mod5, type = "calendar", na.rm = T) %>% ggdid(),
    p6 = aggte(cs_mod6, type = "calendar", na.rm = T) %>% ggdid()
  )

  # Plots group
  plots_group = list(
    p1 = aggte(cs_mod1, type = "group", na.rm = T) %>% ggdid(),
    p2 = aggte(cs_mod2, type = "group", na.rm = T) %>% ggdid(),
    p3 = aggte(cs_mod3, type = "group", na.rm = T) %>% ggdid(),
    p4 = aggte(cs_mod4, type = "group", na.rm = T) %>% ggdid(),
    p5 = aggte(cs_mod5, type = "group", na.rm = T) %>% ggdid(),
    p6 = aggte(cs_mod6, type = "group", na.rm = T) %>% ggdid()
  )

  # Save plots
  save_plots(plots_full, outcome_names, xformula, name = "Census_reduced_form_full", mult = 10)
  save_plots(plots_dynamic, outcome_names, xformula, name = "Census_reduced_form_dynamic")
  save_plots(plots_calendar, outcome_names, xformula, name = "Census_reduced_form_calendar", mult = 0.3, ylab = "", omit_legend = T)
  save_plots(plots_group, outcome_names, xformula, name = "Census_reduced_form_group", mult = 0.3, ylab = "Group", xlab = "Effect", omit_legend = T)
  
}

# ==== Reduced form regressions cs (Grundtvig data) ====
cs_reduced_form_regressions_grundtvig = function(xformula = "1"){
  cs_mod1 = att_gt(
    yname = "Assembly_house",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year_instr",       # First year of treatment
    xformla = as.formula(paste("~", xformula)),
    data = grundtvig_cs,              # Your dataset
    clustervars = "GIS_ID",      # Cluster variable
    control_group = "notyettreated"
  )

  cs_mod2 = att_gt(
    yname = "HighSchool",         
    tname = "Year_num",           
    idname = "GIS_ID_num",        
    gname = "Treat_year_instr",   
    xformla = as.formula(paste("~", xformula)),
    data = grundtvig_cs,          
    clustervars = "GIS_ID",      
    control_group = "notyettreated"
  )

  cs_mod3 = att_gt(
    yname = "MA_assembly",          
    tname = "Year_num",             
    idname = "GIS_ID_num",          
    gname = "Treat_year_instr",     
    xformla = as.formula(paste("~", xformula)),
    data = grundtvig_cs,            
    clustervars = "GIS_ID",      
    control_group = "notyettreated"
  )

  cs_mod4 = att_gt(
    yname = "MA_folkhigh",          
    tname = "Year_num",             
    idname = "GIS_ID_num",          
    gname = "Treat_year_instr",     
    xformla = as.formula(paste("~", xformula)),
    data = grundtvig_cs,            
    clustervars = "GIS_ID",      
    control_group = "notyettreated"
  )

  simple_sum_res = list(
    agg_mod1 = aggte(cs_mod1, type = "simple", na.rm = T),
    agg_mod2 = aggte(cs_mod2, type = "simple", na.rm = T),
    agg_mod3 = aggte(cs_mod3, type = "simple", na.rm = T),
    agg_mod4 = aggte(cs_mod4, type = "simple", na.rm = T)
  )

  outcome_names = lapply(simple_sum_res, function(x){
    x$DIDparams$yname
  }) %>% unlist()

  res = extract_res(simple_sum_res) %>%
    mutate(
      # Stars
      Estimate = stars(Estimate, p, NSIGNIF)
    ) %>%
    mutate(
      SE = paste0("(", signif0(SE, NSIGNIF), ")")
    ) %>%
    mutate_all(signif0, digits = NSIGNIF) %>%
    mutate_all(as.character) %>%
    pivot_longer(cols = c("Estimate", "SE", "t", "p", "n", "n_parishes", "mean_outcome"), names_to = "stat") %>% 
    pivot_wider(names_from = "outcome", values_from = "value") %>%
    filter(stat %in% c("Estimate", "SE", "n", "n_parishes", "mean_outcome")) %>%
    mutate(
      stat = case_when(
        stat == "Estimate" ~ "Connected railway",
        stat == "SE" ~ "",
        stat == "n" ~ "N Obs.",
        stat == "n_parishes" ~ "N parishes",
        stat == "mean_outcome" ~ "Mean of Outcome"
      )
    ) %>%
    select(-control_group) %>%
    rename(` ` = stat)

  res = kable(
    res,
    booktabs = T, 
    caption = "Railways and Grundtvigianism", 
    row.names = F,
    align = "lcccccc",
    linesep = "",  # Suppress additional line spacing
    format = "latex",
    escape = FALSE
  )

  if(xformula == 1){
    table_name = "CS_reduced_form_estimates_grundtvig"
  } else {
    table_name = "CS_reduced_form_estimates_grundtvig_controls"
  }

  sink(paste("Tables/", table_name, ".txt", sep = ""))
  print(res)
  sink()

  print(res) # To display in console when running the script

  # Plots full
  plots_full = list(
    p1 = cs_mod1 %>% ggdid(),
    p2 = cs_mod2 %>% ggdid(),
    p3 = cs_mod3 %>% ggdid(),
    p4 = cs_mod4 %>% ggdid()
  )

  # Plots dynamic
  plots_dynamic = list(
    p1 = aggte(cs_mod1, type = "dynamic", na.rm = T) %>% ggdid(),
    p2 = aggte(cs_mod2, type = "dynamic", na.rm = T) %>% ggdid(),
    p3 = aggte(cs_mod3, type = "dynamic", na.rm = T) %>% ggdid(),
    p4 = aggte(cs_mod4, type = "dynamic", na.rm = T) %>% ggdid()
  )

  # Plots calendar
  plots_calendar = list(
    p1 = aggte(cs_mod1, type = "calendar", na.rm = T) %>% ggdid(),
    p2 = aggte(cs_mod2, type = "calendar", na.rm = T) %>% ggdid(),
    p3 = aggte(cs_mod3, type = "calendar", na.rm = T) %>% ggdid(),
    p4 = aggte(cs_mod4, type = "calendar", na.rm = T) %>% ggdid()
  )

  # Plots group
  plots_group = list(
    p1 = aggte(cs_mod1, type = "group", na.rm = T) %>% ggdid(),
    p2 = aggte(cs_mod2, type = "group", na.rm = T) %>% ggdid(),
    p3 = aggte(cs_mod3, type = "group", na.rm = T) %>% ggdid(),
    p4 = aggte(cs_mod4, type = "group", na.rm = T) %>% ggdid()
  )

  # Save plots
  save_plots(plots_full, outcome_names, xformula, name = "Grundtvig_reduced_form_full")
  save_plots(plots_dynamic, outcome_names, xformula, name = "Grundtvig_reduced_form_dynamic")
  save_plots(plots_calendar, outcome_names, xformula, name = "Grundtvig_reduced_form_calendar", mult = 0.5, ylab = "", omit_legend = T)
  save_plots(plots_group, outcome_names, xformula, name = "Grundtvig_reduced_form_group", mult = 0.5, ylab = "Group", xlab = "Effect", omit_legend = T)
}

# ===== main ==== 
main = function(){
  start_time = Sys.time()

  # Descipritve statistics:
  summary_tables()
  census_distributions()
  census_distributions_by_year()
  grundtvig_distributions_over_time()

  time_descriptive = time_passed(start_time, "Descriptive: ")

  # TWFE estimates:
  twfe_regressions_census()
  twfe_regressions_grundtvig()
  twfe_regressions_census(CONTROLS)
  twfe_regressions_grundtvig(CONTROLS)

  time_twfe = time_passed(time_descriptive, "TWFE: ")
  
  # CS estimates
  cs_estimates_census()
  cs_estimates_grundtvig()
  cs_estimates_census(CONTROLS)
  cs_estimates_grundtvig(CONTROLS)
  cs_estimates_grundtvig_decade()
  # cs_estimates_grundtvig_decade(CONTROLS) # Does not converge

  time_cs = time_passed(time_twfe, "CS: ")

  # TSLS estimates
  tsls_regressions_census()
  tsls_regressions_grundtvig()
  tsls_regressions_census(CONTROLS)
  tsls_regressions_grundtvig(CONTROLS)

  time_tsls = time_passed(time_cs, "TSLS: ")

  # Reduced form OLS
  reduced_form_regressions_census()
  reduced_form_regressions_grundtvig()
  reduced_form_regressions_census(CONTROLS)
  reduced_form_regressions_grundtvig(CONTROLS)

  time_reduced_form = time_passed(time_tsls, "Reduced form: ")

  # Reduced form CS
  cs_reduced_form_regressions_census()
  cs_reduced_form_regressions_grundtvig()
  cs_reduced_form_regressions_census(CONTROLS)
  cs_reduced_form_regressions_grundtvig(CONTROLS)

  time_reduced_form_cs = time_passed(time_reduced_form, "Reduced form CS: ")

  time_passed(start_time, "Total time: ")
}

main()

