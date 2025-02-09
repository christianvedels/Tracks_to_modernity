# Regressions
#
# Date updated:   2025-01-21
# Author:         Christian Vedel, Tom Görges
# Purpose:        Runs regressions

# ==== Libraries ====
library(tidyverse)
library(fixest)
library(did)
library(kableExtra) # for latex tables
source("Data_cleaning_scripts/000_Functions.R")

# ==== Params ====
CONTROLS = "Boulder_clay_pct_year + Dist_hamb_year + Dist_cph_year + Pop1801_year + county_by_year"
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
      Connected_railway,
      Connected_lcp
    ) %>%
    pivot_longer(
      cols = c(
        Assembly_house,
        HighSchool,
        Connected_railway,
        Connected_lcp
      ),
      names_to = "var"
    ) %>%
    mutate(
      var = case_when(
        var == "Assembly_house" ~ "Share of parishes with an assembly house",
        var == "HighSchool" ~ "Share of parishes with a folk high school",
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
    sum_table_census %>% mutate(dataset = "Census"),
    sum_table_grundtvig %>% mutate(dataset = "Grundtvig")
  )

  # Create tex table
  sum_tex = summary_stats %>%
    kable(
      format = "latex",
      booktabs = TRUE,
      caption = "Summary Statistics",
      col.names = c("Variable", "Dataset", "N", "Mean", "SD", "Min", "Max"),
      align = "lcccccc"
    ) %>%
    kable_styling(
      latex_options = c("hold_position", "scale_down")
    ) %>%
    group_rows("A. Economy", 1, NROW(sum_table_census)) %>%
    group_rows("B. Grundtvig", NROW(sum_table_census) + 1, NROW(summary_stats))

  sink("Tables/Summary_Statistics.txt")
  print(sum_tex)
  sink()
  print(sum_tex) # To display in console when running the script

}

# ==== Densities ====
census_distributions = function(){
  p1 = census %>%
    group_by(GIS_ID) %>%
    mutate(Ever_rail = case_when(mean(Connected_railway) > 0 ~ "Yes", TRUE ~ "No")) %>%
    filter(Year == 1850, Connected_railway == 0) %>%  # Exclude parishes with railways already
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
      Boulder_clay_pct,
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
        Boulder_clay_pct,
        area_parish, 
        DistOxRoad,
        Distance_market_town,
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
        var == "HighSchool" ~ "Share of parishes with a folk high school",
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

  ggsave("Plots/Grundtvig_over_time.png", p1, width = 0.75*dims$width, height = 1.5*dims$height)
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
       fitstat = ~ ar2 + n,  # Include R-squared and number of observations
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

  res = etable(twfe1, twfe2,
       fitstat = ~ ar2 + n,  # Include R-squared and number of observations
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
    yname = "lnPopulation",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year",       # First year of treatment
    xformla = as.formula(paste("~", xformula)),  # Any covariates 
    data = census,              # Your dataset
    clustervars = "GIS_ID"      # Cluster variable
  )

  # Child-woman ratio
  cs_mod2 = att_gt(
    yname = "lnChild_women_ratio",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year",       # First year of treatment
    xformla = as.formula(paste("~", xformula)),     
    data = census,              # Your dataset
    clustervars = "GIS_ID"      # Cluster variable
  )

  # log Manufacturing
  cs_mod3 = att_gt(
    yname = "lnManufacturing",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year",       # First year of treatment
    xformla = as.formula(paste("~", xformula)),            
    data = census,              # Your dataset
    clustervars = "GIS_ID"      # Cluster variable
  )

  # log Manufacturing
  cs_mod4 = att_gt(
    yname = "lnNotAgriculture",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year",       # First year of treatment
    xformla = as.formula(paste("~", xformula)),              
    data = census,              # Your dataset
    clustervars = "GIS_ID"      # Cluster variable
  )

  # HISCAM_avg
  cs_mod5 = att_gt(
    yname = "HISCAM_avg",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year",       # First year of treatment
    xformla = as.formula(paste("~", xformula)),              
    data = census,              # Your dataset
    clustervars = "GIS_ID"      # Cluster variable
  )

  # Migration
  cs_mod6 = att_gt(
    yname = "lnMigration",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year",       # First (observed) year of treatment
    xformla = as.formula(paste("~", xformula)),      
    data = census,              # Your dataset
    clustervars = "GIS_ID"      # Cluster variable
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
      Estimate = case_when(
        p < 0.01 ~ paste0(signif0(Estimate, NSIGNIF), "***"),
        p < 0.05 ~ paste0(signif0(Estimate, NSIGNIF), "**"),
        p < 0.1 ~ paste0(signif0(Estimate, NSIGNIF), "*"),
        TRUE ~ paste0(signif0(Estimate, NSIGNIF))
      )
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
    format = "latex"
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

  # Function to save plots list
  save_plots = function(plots, outcome_names, xformula, name = "Census"){
    for(i in seq(length(plots))){
      p = plots[[i]]
      print(p)
      # Name based on controls or not
      if(xformula == 1){
        fname = paste("Plots/", name, "/", names(plots)[i], "_", outcome_names[i], ".png", sep = "")
      } else {
        fname = paste("Plots/", name, "_control/", names(plots)[i], "_", outcome_names[i], ".png", sep = "")
      }

      p = p + 
        labs(
          y = outcomeNames(outcome_names[i]),
          x = "Year",
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

      ggsave(fname, p, width = dims$width, height = dims$height)

      ggsave(fname, p, width = dims$width, height = dims$height)
  }
  }

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

  # Save plots
  save_plots(plots_full, outcome_names, xformula, name = "Census_full")
  save_plots(plots_dynamic, outcome_names, xformula, name = "Census_dynamic")
  save_plots(plots_calendar, outcome_names, xformula, name = "Census_calendar")
}

# ==== CS estimates (Grundtvig data) ====
cs_estimates_grundtvig = function(xformula = "1"){
  # Assembly house
  cs_mod1 = att_gt(
    yname = "Assembly_house",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year",       # First year of treatment
    xformla = as.formula(paste("~", xformula)),  # Any covariates 
    data = grundtvig,              # Your dataset
    clustervars = "GIS_ID"      # Cluster variable
  )

  # HighSchool
  cs_mod2 = att_gt(
    yname = "HighSchool",          # Outcome variable
    tname = "Year_num",             # Time variable
    idname = "GIS_ID_num",          # Unit identifier
    gname = "Treat_year",       # First year of treatment
    xformla = as.formula(paste("~", xformula)),               # No covariates (consistent with TWFE)
    data = grundtvig,              # Your dataset
    clustervars = "GIS_ID"      # Cluster variable
  )

  # Aggregate the ATT
  simple_sum_res = list(
    agg_mod1 = aggte(cs_mod1, type = "simple", na.rm = T),
    agg_mod2 = aggte(cs_mod2, type = "simple", na.rm = T)
  )

  # Outcome names
  outcome_names = lapply(simple_sum_res, function(x){
    x$DIDparams$yname
  }) %>% unlist()
  
  # Construct table
  res = extract_res(simple_sum_res) %>%
    mutate(
      # Stars
      Estimate = case_when(
        p < 0.01 ~ paste0(signif0(Estimate, NSIGNIF), "***"),
        p < 0.05 ~ paste0(signif0(Estimate, NSIGNIF), "**"),
        p < 0.1 ~ paste0(signif0(Estimate, NSIGNIF), "*"),
        TRUE ~ paste0(signif0(Estimate, NSIGNIF))
      )
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
    format = "latex"
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

  # Function to save plots list
  save_plots = function(plots, outcome_names, xformula, name = "Grundtvig"){
    for(i in seq(length(plots))){
      p = plots[[i]]
      # print(p)
      # Name based on controls or not
      if(xformula == 1){
        fname = paste("Plots/", name, "/", names(plots)[i], "_", outcome_names[i], ".png", sep = "")
      } else {
        fname = paste("Plots/", name, "_control/", names(plots)[i], "_", outcome_names[i], ".png", sep = "")
      }
      p = p + 
        labs(
          y = outcomeNames(outcome_names[i]),
          x = "Year",
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
          col = "Connected to railway:"
        )

      ggsave(fname, p, width = dims$width, height = dims$height)
  }
  }

  # Plots full
  plots_full = list(
    p1 = cs_mod1 %>% ggdid(),
    p2 = cs_mod2 %>% ggdid()
  )
  
  # Plots dynamic
  plots_dynamic = list(
    p1 = aggte(cs_mod1, type = "dynamic", na.rm = T) %>% ggdid(),
    p2 = aggte(cs_mod2, type = "dynamic", na.rm = T) %>% ggdid()
  )

  # Plots calendar
  plots_calendar = list(
    p1 = aggte(cs_mod1, type = "calendar", na.rm = T) %>% ggdid(),
    p2 = aggte(cs_mod2, type = "calendar", na.rm = T) %>% ggdid()
  )

  # Save plots
  save_plots(plots_full, outcome_names, xformula, name = "Grundtvig_full")
  save_plots(plots_dynamic, outcome_names, xformula, name = "Grundtvig_dynamic")
  save_plots(plots_calendar, outcome_names, xformula, name = "Grundtvig_calendar")
}

# # ==== Instrumental Variable Approach ====

# # ==== TSLS regressions (Census data) ====
# form1 <- as.formula(paste("lnPopulation ~", xformula, "| GIS_ID + Year | Connected_railway ~ Connected_lcp"))
# tsls1 = feols(
#   form1,  # Full formula including covariates, fixed effects, and IV
#   data = census_iv,
#   cluster = ~ GIS_ID  # Clustering
# )

# form2 <- as.formula(paste("lnChild_women_ratio ~", xformula, "| GIS_ID + Year | Connected_railway ~ Connected_lcp"))
# tsls2 = feols(
#   form2,
#   data = census_iv,
#   cluster = ~ GIS_ID
# )

# form3 <- as.formula(paste("lnManufacturing ~", xformula, "| GIS_ID + Year | Connected_railway ~ Connected_lcp"))
# tsls3 = feols(
#   form3,
#   data = census_iv,
#   cluster = ~ GIS_ID
# )

# form4 <- as.formula(paste("lnNotAgriculture ~", xformula, "| GIS_ID + Year | Connected_railway ~ Connected_lcp"))
# tsls4 = feols(
#   form4,
#   data = census_iv,
#   cluster = ~ GIS_ID
# )

# form5 <- as.formula(paste("HISCAM_avg ~", xformula, "| GIS_ID + Year | Connected_railway ~ Connected_lcp"))
# tsls5 = feols(
#   form5,
#   data = census_iv,
#   cluster = ~ GIS_ID
# )

# form6 <- as.formula(paste("lnMigration ~", xformula, "| GIS_ID + Year | Connected_railway ~ Connected_lcp"))
# tsls6 = feols(
#   form6,
#   data = census_iv,
#   cluster = ~ GIS_ID
# )

# # Define the controls row dynamically
# controls <- if (xformula == 1) {
#   rep("No", 6)  # If xformula is 1, set all models to "No"
# } else if (xformula == "Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year") {
#   rep("Yes", 6)  # If xformula matches the specified string, set all to "Yes"
# } else {
#   rep("Mixed", 6)  # Default case if xformula does not match the predefined conditions
# }

# nodes_dropped <- rep("Yes", 6)

# # second stages output table
# etable(
#   list(tsls1, tsls2, tsls3, tsls4, tsls5, tsls6),
#   signif.code = c("*" = 0.10, "**" = 0.05, "***" = 0.01),  
#   fitstat = ~ n + ivf,
#   tex = T,
#   digits = 3,
#   keep = "Connected_railway",
#   title = "Railways and local development (TSLS estimates)",
#   extralines = list(
#     "__Nodes dropped (10km)" = nodes_dropped,
#     "__Controls" = controls
#   )
# )


# # === Reduced form TWFE with Instrument ===
# form1 = as.formula(paste("lnPopulation ~ Connected_lcp +", xformula, "| GIS_ID + Year"))
# twfe1_red = feols(
#   form1,
#   data = census_cs,
#   cluster = ~ GIS_ID
# )

# form2 = as.formula(paste("lnChild_women_ratio ~ Connected_lcp +", xformula, "| GIS_ID + Year"))
# twfe2_red = feols(
#   form2,
#   data = census_cs,
#   cluster = ~ GIS_ID
# )

# form3 = as.formula(paste("lnManufacturing ~ Connected_lcp +", xformula, "| GIS_ID + Year"))
# twfe3_red = feols(
#   form3,
#   data = census_cs,
#   cluster = ~ GIS_ID
# )

# form4 = as.formula(paste("lnNotAgriculture ~ Connected_lcp +", xformula, "| GIS_ID + Year"))
# twfe4_red = feols(
#   form4,
#   data = census_cs,
#   cluster = ~ GIS_ID
# )

# form5 = as.formula(paste("HISCAM_avg ~ Connected_lcp +", xformula, "| GIS_ID + Year"))
# twfe5_red = feols(
#   form5,
#   data = census_cs,
#   cluster = ~ GIS_ID
# )

# form6 = as.formula(paste("lnMigration ~ Connected_lcp +", xformula, "| GIS_ID + Year"))
# twfe6_red = feols(
#   form6,
#   data = census_cs,
#   cluster = ~ GIS_ID
# )

# etable(
#   list(twfe1_red, twfe2_red, twfe3_red, twfe4_red, twfe5_red, twfe6_red)
# )

# ### Reduced form CS

# # log(Pop)
# cs_mod1 <- att_gt(
#   yname = "lnPopulation",          # Outcome variable
#   tname = "Year_num",             # Time variable
#   idname = "GIS_ID_num",          # Unit identifier
#   gname = "Treat_year_instr",       # First year of treatment
#   xformla = as.formula(paste("~", xformula)),              # No covariates 
#   data = census_cs,              # Your dataset
#   clustervars = "GIS_ID"      # Cluster variable
# )

# # Child-woman ratio
# cs_mod2 <- att_gt(
#   yname = "lnChild_women_ratio",          # Outcome variable
#   tname = "Year_num",             # Time variable
#   idname = "GIS_ID_num",          # Unit identifier
#   gname = "Treat_year_instr",       # First year of treatment
#   xformla = as.formula(paste("~", xformula)),              # No covariates (consistent with TWFE)
#   data = census_cs,              # Your dataset
#   clustervars = "GIS_ID"      # Cluster variable
# )

# # log Manufacturing
# cs_mod3 <- att_gt(
#   yname = "lnManufacturing",          # Outcome variable
#   tname = "Year_num",             # Time variable
#   idname = "GIS_ID_num",          # Unit identifier
#   gname = "Treat_year_instr",       # First year of treatment
#   xformla = as.formula(paste("~", xformula)),               # No covariates (consistent with TWFE)
#   data = census_cs,              # Your dataset
#   clustervars = "GIS_ID"      # Cluster variable
# )

# # log Not agriculture
# cs_mod4 <- att_gt(
#   yname = "lnNotAgriculture",          # Outcome variable
#   tname = "Year_num",             # Time variable
#   idname = "GIS_ID_num",          # Unit identifier
#   gname = "Treat_year_instr",       # First year of treatment
#   xformla = as.formula(paste("~", xformula)),              # No covariates (consistent with TWFE)
#   data = census_cs,              # Your dataset
#   clustervars = "GIS_ID"      # Cluster variable
# )

# # HISCAM_avg
# cs_mod5 <- att_gt(
#   yname = "HISCAM_avg",          # Outcome variable
#   tname = "Year_num",             # Time variable
#   idname = "GIS_ID_num",          # Unit identifier
#   gname = "Treat_year_instr",       # First year of treatment
#   xformla = as.formula(paste("~", xformula)),             # No covariates (consistent with TWFE)
#   data = census_cs,              # Your dataset
#   clustervars = "GIS_ID"      # Cluster variable
# )

# # Migration
# cs_mod6 <- att_gt(
#   yname = "lnMigration",          # Outcome variable
#   tname = "Year_num",             # Time variable
#   idname = "GIS_ID_num",          # Unit identifier
#   gname = "Treat_year_instr",       # First (observed) year of treatment
#   xformla = as.formula(paste("~", xformula)),              # No covariates (consistent with TWFE)
#   data = census_cs,              # Your dataset
#   clustervars = "GIS_ID"      # Cluster variable
# )

# # Aggregate the ATT
# agg_mod1 <- aggte(cs_mod1, type = "simple", na.rm = T)
# agg_mod2 <- aggte(cs_mod2, type = "simple", na.rm = T)
# agg_mod3 <- aggte(cs_mod3, type = "simple", na.rm = T)
# agg_mod4 <- aggte(cs_mod4, type = "simple", na.rm = T)
# agg_mod5 <- aggte(cs_mod5, type = "simple", na.rm = T)
# agg_mod6 <- aggte(cs_mod6, type = "simple", na.rm = T)

# # Summary
# summary(agg_mod1) # Pop
# summary(agg_mod2) # Child w ratio
# summary(agg_mod3) # Manufacturing
# summary(agg_mod4) # Not agriculture
# summary(agg_mod5) # HISCAM
# summary(agg_mod6) # Migration


# # === Create Latex output table ===

# # Store p-values for significance ***

# # Define models for each set
# twfe_iv_models <- list(tsls1, tsls2, tsls3, tsls4, tsls5, tsls6)
# twfe_reduced_models <- list(twfe1_red, twfe2_red, twfe3_red, twfe4_red, twfe5_red, twfe6_red)
# did_reduced_models <- list(agg_mod1, agg_mod2, agg_mod3, agg_mod4, agg_mod5, agg_mod6)

# # Compute p-values for TWFE models
# twfe_iv_p <- unlist(lapply(twfe_iv_models, function(model) {
#   pnorm(abs(model$coefficients / model$se), lower.tail = FALSE) * 2
# }))

# # ... for reduced twfe
# twfe_reduced_p <- unlist(lapply(twfe_reduced_models, function(model) {
#   pnorm(abs(model$coefficients / model$se), lower.tail = FALSE) * 2
# }))

# # ... for DID models
# did_reduced_p <- unlist(lapply(did_reduced_models, function(model) {
#   pnorm(abs(model$overall.att / model$overall.se), lower.tail = FALSE) * 2
# }))


# # Dynamically create the Outcomes header mapping
# outcomes_twfe_iv <- c(
#   all.vars(formula(tsls1))[1], 
#   all.vars(formula(tsls2))[1], 
#   all.vars(formula(tsls3))[1], 
#   all.vars(formula(tsls4))[1],
#   all.vars(formula(tsls5))[1],
#   all.vars(formula(tsls6))[1]
# )

# outcomes_twfe_reduced <- c(
#   all.vars(formula(twfe1_red))[1], 
#   all.vars(formula(twfe2_red))[1], 
#   all.vars(formula(twfe3_red))[1], 
#   all.vars(formula(twfe4_red))[1],
#   all.vars(formula(twfe5_red))[1],
#   all.vars(formula(twfe6_red))[1]
# )

# outcomes_cs_reduced <- c(
#   agg_mod1$DIDparams$yname,
#   agg_mod2$DIDparams$yname,
#   agg_mod3$DIDparams$yname,
#   agg_mod4$DIDparams$yname,
#   agg_mod5$DIDparams$yname,
#   agg_mod6$DIDparams$yname)


# # Check if headers align
# if (!identical(outcomes_twfe_reduced, outcomes_cs_reduced) || 
#     !identical(outcomes_twfe_reduced, outcomes_twfe_iv)) {
#   stop("Models don't align!")
# }


# # Create the named vector for add_header_above if they align
# outcomes_header <- c(" " = 1, setNames(rep(1, length(outcomes_twfe_reduced)), outcomes_twfe_reduced))


# twfe_iv_coef <- c("fit_Connected_railway", 
#                   sprintf("%.3f", tsls1$coefficients[1]),
#                   sprintf("%.3f", tsls2$coefficients[1]),
#                   sprintf("%.3f", tsls3$coefficients[1]),
#                   sprintf("%.3f", tsls4$coefficients[1]),
#                   sprintf("%.3f", tsls5$coefficients[1]),
#                   sprintf("%.3f", tsls6$coefficients[1]))

# # Add ***
# twfe_iv_coef[2:7] <- sapply(2:7, function(i) {
#   coef_value <- as.numeric(twfe_iv_coef[i])
#   p_value <- as.numeric(twfe_iv_p[i-1])
#   if (!is.na(p_value)) {
#     if (p_value < 0.01) {
#       sprintf("%.3f***", coef_value)
#     } else if (p_value < 0.05) {
#       sprintf("%.3f**", coef_value)
#     } else if (p_value < 0.1) {
#       sprintf("%.3f*", coef_value)
#     } else {
#       sprintf("%.3f", coef_value)
#     }
#   } else {
#     sprintf("%.3f", coef_value)
#   }
# })

# # built output table row by row
# twfe_iv_se <- c("", 
#                 sprintf("(%.3f)", tsls1$se[1]),
#                 sprintf("(%.3f)", tsls2$se[1]),
#                 sprintf("(%.3f)", tsls3$se[1]),
#                 sprintf("(%.3f)", tsls4$se[1]),
#                 sprintf("(%.3f)", tsls5$se[1]),
#                 sprintf("(%.3f)", tsls6$se[1]))

# parish_fe <- c("Parish FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
# year_fe <- c("Year FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")

# twfe_red_coef <- c("Connected_lcp", 
#                    sprintf("%.3f", twfe1_red$coefficients[1]),
#                    sprintf("%.3f", twfe2_red$coefficients[1]),
#                    sprintf("%.3f", twfe3_red$coefficients[1]),
#                    sprintf("%.3f", twfe4_red$coefficients[1]),
#                    sprintf("%.3f", twfe5_red$coefficients[1]),
#                    sprintf("%.3f", twfe6_red$coefficients[1]))

# # Add ***
# twfe_red_coef[2:7] <- sapply(2:7, function(i) {
#   coef_value <- as.numeric(twfe_red_coef[i])
#   p_value <- as.numeric(twfe_reduced_p[i-1])
#   if (!is.na(p_value)) {
#     if (p_value < 0.01) {
#       sprintf("%.3f***", coef_value)
#     } else if (p_value < 0.05) {
#       sprintf("%.3f**", coef_value)
#     } else if (p_value < 0.1) {
#       sprintf("%.3f*", coef_value)
#     } else {
#       sprintf("%.3f", coef_value)
#     }
#   } else {
#     sprintf("%.3f", coef_value)
#   }
# })

# # SE's
# twfe_red_se <- c("", 
#                  sprintf("(%.3f)", twfe1_red$se[1]),
#                  sprintf("(%.3f)", twfe2_red$se[1]),
#                  sprintf("(%.3f)", twfe3_red$se[1]),
#                  sprintf("(%.3f)", twfe4_red$se[1]),
#                  sprintf("(%.3f)", twfe5_red$se[1]),
#                  sprintf("(%.3f)", twfe6_red$se[1]))

# # did coeffcicients reduced form
# did_coef_red <- c("Connected_lcp", 
#                   sprintf("%.3f", agg_mod1$overall.att),
#                   sprintf("%.3f", agg_mod2$overall.att),
#                   sprintf("%.3f", agg_mod3$overall.att),
#                   sprintf("%.3f", agg_mod4$overall.att),
#                   sprintf("%.3f", agg_mod5$overall.att),
#                   sprintf("%.3f", agg_mod6$overall.att))

# # Add significance stars to DID coefficients based on p-values
# did_coef_red[2:7] <- sapply(2:7, function(i) {
#   coef_value <- as.numeric(did_coef_red[i])
#   p_value <- as.numeric(did_reduced_p[i-1])
#   if (!is.na(p_value)) {
#     if (p_value < 0.01) {
#       sprintf("%.3f***", coef_value)
#     } else if (p_value < 0.05) {
#       sprintf("%.3f**", coef_value)
#     } else if (p_value < 0.1) {
#       sprintf("%.3f*", coef_value)
#     } else {
#       sprintf("%.3f", coef_value)
#     }
#   } else {
#     sprintf("%.3f", coef_value)
#   }
# })

# # Standard error
# did_se_red <- c("", 
#                 sprintf("(%.3f)", agg_mod1$overall.se),
#                 sprintf("(%.3f)", agg_mod2$overall.se),
#                 sprintf("(%.3f)", agg_mod3$overall.se),
#                 sprintf("(%.3f)", agg_mod4$overall.se),
#                 sprintf("(%.3f)", agg_mod5$overall.se),
#                 sprintf("(%.3f)", agg_mod6$overall.se))

# # Observations
# twfe_iv_obs <- c("Obs.", tsls1$nobs, tsls2$nobs, tsls3$nobs, tsls4$nobs, tsls5$nobs, tsls6$nobs)
# twfe_reduced_obs <- c("Obs.", twfe1_red$nobs, twfe2_red$nobs, twfe3_red$nobs, twfe4_red$nobs, twfe5_red$nobs, twfe6_red$nobs)
# did_reduced_obs <- c("Obs.", 
#                      agg_mod1$DIDparams$n * agg_mod1$DIDparams$nT,
#                      agg_mod2$DIDparams$n * agg_mod2$DIDparams$nT,
#                      agg_mod3$DIDparams$n * agg_mod3$DIDparams$nT,
#                      agg_mod4$DIDparams$n * agg_mod4$DIDparams$nT,
#                      agg_mod5$DIDparams$n * agg_mod5$DIDparams$nT,
#                      agg_mod6$DIDparams$n * agg_mod6$DIDparams$nT)

# # Create mean Outcome

# # Dynamically calculate the means for the specified columns
# mean_outcome <- c(
#   "Mean of Outcome",
#   sapply(outcomes_twfe_reduced, function(col) sprintf("%.3f", mean(census[[col]], na.rm = TRUE)))
# )

# controls <- c("Controls", 
#               if (xformula == 1) rep("No", 6) 
#               else if (xformula == "Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year") rep("Yes", 6))


# n_parishes <- c("Parishes", 
#                 length(unique(agg_mod1$DIDparams$data$GIS_ID)),
#                 length(unique(agg_mod2$DIDparams$data$GIS_ID)),
#                 length(unique(agg_mod3$DIDparams$data$GIS_ID)),
#                 length(unique(agg_mod4$DIDparams$data$GIS_ID)),
#                 length(unique(agg_mod5$DIDparams$data$GIS_ID)),
#                 length(unique(agg_mod6$DIDparams$data$GIS_ID)))

# nodes_dropped <- c("Nodes dropped (10km)", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
  
# # first stage f-statistic
# firstF <- c("F-test (1st stage)", 
#             round(as.numeric(fitstat(tsls1, "ivf")$ivf[1]), 2),
#             round(as.numeric(fitstat(tsls2, "ivf")$ivf[1]), 2),
#             round(as.numeric(fitstat(tsls3, "ivf")$ivf[1]), 2),
#             round(as.numeric(fitstat(tsls4, "ivf")$ivf[1]), 2),
#             round(as.numeric(fitstat(tsls5, "ivf")$ivf[1]), 2),
#             round(as.numeric(fitstat(tsls6, "ivf")$ivf[1]), 2)
#             )

# # bind together for output table
# results <- rbind(twfe_iv_coef, twfe_iv_se, parish_fe, year_fe, twfe_iv_obs, firstF,
#                  twfe_red_coef, twfe_red_se, parish_fe, year_fe, twfe_reduced_obs,
#                  did_coef_red, did_se_red, n_parishes, did_reduced_obs, mean_outcome, nodes_dropped, controls)

# colnames(results) <- c(" ", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)")

# # Create Output latex table
# kbl(results, 
#     booktabs = T, 
#     caption = "TSLS vs Reduced form: Railways and Local Development", 
#     row.names = F,
#     align = "lcccccc",
#     linesep = "",  # Suppress additional line spacing
#     format = "latex") %>%
#   kable_styling(
#     latex_options = c("hold_position", "scale_down") # Ensures [!h] placement
#   ) %>%
#   add_header_above(outcomes_header) %>%
#   add_header_above(c(" " = 1, "Dependent variable:" = 6), escape = F) %>%
#   group_rows("Panel A: 2SLS", 1, 6) %>%
#   group_rows("Panel B: TWFE Reduced Form", 7, 11) %>%
#   group_rows("Panel C: CS Reduced Form", 12, 15)

# #############################
# # === First-stage table === #
# #############################
# first1 = feols(
#   lnPopulation ~ 1 | GIS_ID + Year | Connected_railway ~ Connected_lcp, 
#   data = census,
#   cluster = ~ GIS_ID  # Clustering
# )

# first2 = feols(
#   lnPopulation ~ 1 | GIS_ID + Year | Connected_railway ~ Connected_lcp, 
#   data = census_iv,
#   cluster = ~ GIS_ID  # Clustering
# )

# first3 = feols(
#   lnPopulation ~ 1 | GIS_ID + Year | Connected_railway ~ Connected_lcp, 
#   data = census_cs,
#   cluster = ~ GIS_ID  # Clustering
# )

# # now same with covariats

# first4 = feols(
#   lnPopulation ~ Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year 
#   | GIS_ID + Year 
#   | Connected_railway ~ Connected_lcp, 
#   data = census,
#   cluster = ~ GIS_ID  # Clustering
# )

# first5 = feols(
#   lnPopulation ~ Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year 
#   | GIS_ID + Year 
#   | Connected_railway ~ Connected_lcp, 
#   data = census_iv,
#   cluster = ~ GIS_ID  # Clustering
# )

# first6 = feols(
#   lnPopulation ~ Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year 
#   | GIS_ID + Year 
#   | Connected_railway ~ Connected_lcp, 
#   data = census_cs,
#   cluster = ~ GIS_ID  # Clustering
# )

# # create table
# etable(first1, first2, first3, first4, first5, first6,
#        keep = "Connected_lcp",
#        stage = 1,
#        tex = T,
#        signif.code = c("*" = 0.10, "**" = 0.05, "***" = 0.01),  
#        fitstat = ~ n + ivf,
#        extralines = list(
#          "__Nodes dropped (10km)" = c("No", "Yes", "Yes", "No", "Yes", "Yes"),
#          "__Controls" = c("No", "No", "No", "Yes", "Yes", "Yes"),
#          "__Invalid comparisons dropped" = c("No", "No", "Yes", "No", "No", "Yes")
#        ))



# # ==== Doubly Robust DID: Grundtvig ====

# # Assembly house
# out1 = att_gt(
#   yname = "Assembly_house",
#   tname = "Year_num",
#   gname = "Treat_year",
#   idname = "GIS_ID_num",
#   xformla = as.formula(paste("~", xformula)),
#   data = grundtvig,
# )

# # Folk high school
# out2 = att_gt(
#   yname = "HighSchool",
#   tname = "Year_num",
#   gname = "Treat_year",
#   idname = "GIS_ID_num",
#   xformla = as.formula(paste("~", xformula)),
#   data = grundtvig,
# )

# # Aggregate the ATT
# agg_mod1 <- aggte(out1, type = "simple", na.rm = T)
# agg_mod2 <- aggte(out2, type = "simple", na.rm = T)

# # Summary
# summary(agg_mod1)
# summary(agg_mod2)

# # Dynamic summary
# #agg_mod1_dyn = aggte(out1, type = "dynamic")
# #agg_mod2_dyn = aggte(out2, type = "dynamic")
# #agg_mod1_dyn %>% ggdid()
# #agg_mod2_dyn %>% ggdid()

# # === Create Latex output table ===

# # Store p-values for significance ***

# # Define models for each set
# twfe_models <- list(mod1, mod2)
# did_models <- list(agg_mod1, agg_mod2)

# # Compute p-values for TWFE models
# twfe_p <- unlist(lapply(twfe_models, function(model) {
#   pnorm(abs(model$coefficients / model$se), lower.tail = FALSE) * 2
# }))

# # Compute p-values for DID models
# did_p <- unlist(lapply(did_models, function(model) {
#   pnorm(abs(model$overall.att / model$overall.se), lower.tail = FALSE) * 2
# }))


# # Dynamically create the Outcomes header mapping
# outcomes_twfe <- c(as.character(formula(mod1)[2]), 
#                    as.character(formula(mod2)[2]))

# outcomes_cs <- c(agg_mod1$DIDparams$yname,
#                  agg_mod2$DIDparams$yname)


# # Check if headers align
# if (!identical(outcomes_twfe, outcomes_cs)) {
#   stop("Models don't align!")
# }

# # Create the named vector for add_header_above if they align
# outcomes_header <- c(" " = 1, setNames(rep(1, length(outcomes_twfe)), outcomes_twfe))

# # Built output table row by row

# # TWFE Coefficients
# twfe_coef <- c("Connected_railway", 
#                sprintf("%.3f", mod1$coefficients[1]),
#                sprintf("%.3f", mod2$coefficients[1]))

# # Add ***
# twfe_coef[2:3] <- sapply(2:3, function(i) {
#   coef_value <- as.numeric(twfe_coef[i])
#   p_value <- as.numeric(twfe_p[i-1])
#   if (!is.na(p_value)) {
#     if (p_value < 0.01) {
#       sprintf("%.3f***", coef_value)
#     } else if (p_value < 0.05) {
#       sprintf("%.3f**", coef_value)
#     } else if (p_value < 0.1) {
#       sprintf("%.3f*", coef_value)
#     } else {
#       sprintf("%.3f", coef_value)
#     }
#   } else {
#     sprintf("%.3f", coef_value)
#   }
# })

# # TWFE SEs
# twfe_se <- c("", 
#              sprintf("(%.3f)", mod1$se[1]),
#              sprintf("(%.3f)", mod2$se[1]))

# # Fixed Effects
# parish_fe <- c("Parish FE", "Yes", "Yes")
# year_fe <- c("Year FE", "Yes", "Yes")

# # nObs
# twfe_obs <- c("Obs.", mod1$nobs, mod2$nobs)
# did_obs <- c("Obs.", 
#              agg_mod1$DIDparams$n * agg_mod1$DIDparams$nT,
#              agg_mod2$DIDparams$n * agg_mod1$DIDparams$nT)

# # CS DiD coefficient
# did_coef <- c("Connected_railway", 
#               sprintf("%.3f", agg_mod1$overall.att),
#               sprintf("%.3f", agg_mod2$overall.att))

# # Add significance stars to DID coefficients based on p-values
# did_coef[2:3] <- sapply(2:3, function(i) {
#   coef_value <- as.numeric(did_coef[i])
#   p_value <- as.numeric(did_p[i-1])
#   if (!is.na(p_value)) {
#     if (p_value < 0.01) {
#       sprintf("%.3f***", coef_value)
#     } else if (p_value < 0.05) {
#       sprintf("%.3f**", coef_value)
#     } else if (p_value < 0.1) {
#       sprintf("%.3f*", coef_value)
#     } else {
#       sprintf("%.3f", coef_value)
#     }
#   } else {
#     sprintf("%.3f", coef_value)
#   }
# })


# did_se <- c("", 
#             sprintf("(%.3f)", agg_mod1$overall.se),
#             sprintf("(%.3f)", agg_mod2$overall.se))


# # Create mean Outcome
# mean_outcome <- c(
#   "Mean of Outcome",
#   sapply(outcomes_cs, function(col) sprintf("%.3f", mean(census[[col]], na.rm = TRUE)))
# )

# # #Parishes
# did_n_parishes <- c("Parishes", length(unique(agg_mod1$DIDparams$data$GIS_ID)), length(unique(agg_mod2$DIDparams$data$GIS_ID)))

# controls <- c("Controls", 
#               if (xformula == 1) rep("No", 2) 
#               else if (xformula == "Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year") rep("Yes", 2))


# # bind together
# results <- rbind(twfe_coef, twfe_se, parish_fe, year_fe, twfe_obs, did_coef, did_se, did_n_parishes, did_obs, mean_outcome, controls)
# colnames(results) <- c(" ", "(1)", "(2)")

# # Create Output latex table
# kbl(results, 
#     booktabs = T, 
#     caption = "Railways and Grundtvigianism", 
#     row.names = F,
#     align = "lcc",
#     linesep = "",  # Suppress additional line spacing
#     format = "latex") %>%
#   kable_styling(
#     latex_options = c("hold_position", "scale_down") # Ensures [!h] placement
#   ) %>%
#   add_header_above(outcomes_header) %>%
#   add_header_above(c(" " = 1, "Dependent variable:" = 2), escape = F) %>%
#   group_rows("Panel A: TWFE", 1, 5) %>%
#   group_rows("Panel B: Callaway and Sant'Anna", 6, 9)

# # With Instrument ##########################################################################

# # ==== TSLS regressions (Census data) ====
# form1 <- as.formula(paste("Assembly_house ~", xformula, "| GIS_ID + Year | Connected_railway ~ Connected_lcp"))
# tsls1 = feols(
#   form1,
#   data = grundtvig_iv,
#   cluster = ~ GIS_ID
# )

# form2 <- as.formula(paste("HighSchool ~", xformula, "| GIS_ID + Year | Connected_railway ~ Connected_lcp"))
# tsls2 = feols(
#   form2,
#   data = grundtvig_iv,
#   cluster = ~ GIS_ID
# )

# # Define the controls row dynamically
# controls <- if (xformula == 1) {
#   rep("No", 2)  # If xformula is 1, set all models to "No"
# } else if (xformula == "Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year") {
#   rep("Yes", 2)  # If xformula matches the specified string, set all to "Yes"
# } else {
#   rep("?", 2)  # Default case if xformula does not match the predefined conditions
# }

# nodes_dropped <- rep("Yes", 2)

# # second stages output table
# etable(
#   list(tsls1, tsls2),
#   signif.code = c("*" = 0.10, "**" = 0.05, "***" = 0.01),  
#   fitstat = ~ n + ivf,
#   tex = T,
#   digits = 3,
#   keep = "Connected_railway",
#   title = "Railways and Grundtvigianism (TSLS estimates)",
#   extralines = list(
#     "__Nodes dropped (10km)" = nodes_dropped,
#     "__Controls" = controls
#   )
# )

# #####

# # REDUCED FORM
# form1 = as.formula(paste("Assembly_house ~ Connected_lcp +", xformula, "| GIS_ID + Year"))
# twfe1_red <- feols(
#   form1,
#   data = grundtvig_cs,
#   cluster = ~ GIS_ID
# )

# form2 = as.formula(paste("HighSchool ~ Connected_lcp +", xformula, "| GIS_ID + Year"))
# twfe2_red <- feols(
#   form2,
#   data = grundtvig_cs,
#   cluster = ~ GIS_ID
# )

# out1 = att_gt(
#   yname = "Assembly_house",
#   tname = "Year_num",
#   gname = "Treat_year_instr",
#   idname = "GIS_ID_num",
#   xformla = as.formula(paste("~", xformula)),
#   data = grundtvig_cs,
# )

# out2 = att_gt(
#   yname = "HighSchool",
#   tname = "Year_num",
#   gname = "Treat_year_instr",
#   idname = "GIS_ID_num",
#   xformla = as.formula(paste("~", xformula)),
#   data = grundtvig_cs,
# )

# # Aggregate the ATT
# agg_mod1 <- aggte(out1, type = "simple", na.rm = T) # When using covariats: Fehler in max(t) : ungültiger 'type' (list) des Argumentes
# agg_mod2 <- aggte(out2, type = "simple", na.rm = T)

# # Summary
# summary(agg_mod1)
# summary(agg_mod2)

# # === Create Latex output table ===

# # Store p-values for significance ***

# # Define models for each set
# twfe_iv_models <- list(tsls1, tsls2)
# twfe_reduced_models <- list(twfe1_red, twfe2_red)
# did_reduced_models <- list(agg_mod1, agg_mod2)

# # Compute p-values for TWFE models
# twfe_iv_p <- unlist(lapply(twfe_iv_models, function(model) {
#   pnorm(abs(model$coefficients / model$se), lower.tail = FALSE) * 2
# }))

# # ... for reduced twfe
# twfe_reduced_p <- unlist(lapply(twfe_reduced_models, function(model) {
#   pnorm(abs(model$coefficients / model$se), lower.tail = FALSE) * 2
# }))

# # ... for DID models
# did_reduced_p <- unlist(lapply(did_reduced_models, function(model) {
#   pnorm(abs(model$overall.att / model$overall.se), lower.tail = FALSE) * 2
# }))


# # Dynamically create the Outcomes header mapping
# outcomes_twfe_iv <- c(
#   all.vars(formula(tsls1))[1], 
#   all.vars(formula(tsls2))[1]
# )

# outcomes_twfe_reduced <- c(
#   all.vars(formula(twfe1_red))[1], 
#   all.vars(formula(twfe2_red))[1]
# )

# outcomes_cs_reduced <- c(agg_mod1$DIDparams$yname,
#                          agg_mod2$DIDparams$yname
# )


# # Check if headers align
# if (!identical(outcomes_twfe_reduced, outcomes_cs_reduced) || 
#     !identical(outcomes_twfe_reduced, outcomes_twfe_iv)) {
#   stop("Models don't align!")
# }


# # Create the named vector for add_header_above if they align
# outcomes_header <- c(" " = 1, setNames(rep(1, length(outcomes_twfe_reduced)), outcomes_twfe_reduced))

# # coefficients
# twfe_iv_coef <- c("fit_Connected_railway", 
#                   sprintf("%.3f", tsls1$coefficients[1]), sprintf("%.3f", tsls2$coefficients[1]))

# # Add ***
# twfe_iv_coef[2:3] <- sapply(2:3, function(i) {
#   coef_value <- as.numeric(twfe_iv_coef[i])
#   p_value <- as.numeric(twfe_iv_p[i-1])
#   if (!is.na(p_value)) {
#     if (p_value < 0.01) {
#       sprintf("%.3f***", coef_value)
#     } else if (p_value < 0.05) {
#       sprintf("%.3f**", coef_value)
#     } else if (p_value < 0.1) {
#       sprintf("%.3f*", coef_value)
#     } else {
#       sprintf("%.3f", coef_value)
#     }
#   } else {
#     sprintf("%.3f", coef_value)
#   }
# })

# # built output table row by row
# twfe_iv_se <- c("", 
#                 sprintf("(%.3f)", tsls1$se[1]), 
#                 sprintf("(%.3f)", tsls2$se[1]))

# parish_fe <- c("Parish FE", "Yes", "Yes")
# year_fe <- c("Year FE", "Yes", "Yes")

# twfe_red_coef <- c("Connected_lcp", 
#                    sprintf("%.3f", twfe1_red$coefficients[1]), 
#                    sprintf("%.3f", twfe2_red$coefficients[1]))

# # Add ***
# twfe_red_coef[2:3] <- sapply(2:3, function(i) {
#   coef_value <- as.numeric(twfe_red_coef[i])
#   p_value <- as.numeric(twfe_reduced_p[i-1])
#   if (!is.na(p_value)) {
#     if (p_value < 0.01) {
#       sprintf("%.3f***", coef_value)
#     } else if (p_value < 0.05) {
#       sprintf("%.3f**", coef_value)
#     } else if (p_value < 0.1) {
#       sprintf("%.3f*", coef_value)
#     } else {
#       sprintf("%.3f", coef_value)
#     }
#   } else {
#     sprintf("%.3f", coef_value)
#   }
# })

# # SE's
# twfe_red_se <- c("", 
#                  sprintf("(%.3f)", twfe1_red$se[1]),
#                  sprintf("(%.3f)", twfe2_red$se[1]))

# # did coefficicients reduced form
# did_coef_red <- c("Connected_lcp", sprintf("%.3f", agg_mod1$overall.att), sprintf("%.3f",agg_mod2$overall.att))

# # Add significance stars to DID coefficients based on p-values
# did_coef_red[2:3] <- sapply(2:3, function(i) {
#   coef_value <- as.numeric(did_coef_red[i])
#   p_value <- as.numeric(did_reduced_p[i-1])
#   if (!is.na(p_value)) {
#     if (p_value < 0.01) {
#       sprintf("%.3f***", coef_value)
#     } else if (p_value < 0.05) {
#       sprintf("%.3f**", coef_value)
#     } else if (p_value < 0.1) {
#       sprintf("%.3f*", coef_value)
#     } else {
#       sprintf("%.3f", coef_value)
#     }
#   } else {
#     sprintf("%.3f", coef_value)
#   }
# })

# # Standard error
# did_se_red <- c("", sprintf("(%.3f)", agg_mod1$overall.se), sprintf("(%.3f)", agg_mod2$overall.se))

# # Observations
# twfe_iv_obs <- c("Obs.", tsls1$nobs, tsls2$nobs)
# twfe_reduced_obs <- c("Obs.", twfe1_red$nobs, twfe2_red$nobs)
# did_reduced_obs <- c("Obs.", agg_mod1$DIDparams$n*agg_mod1$DIDparams$nT, agg_mod2$DIDparams$n*agg_mod2$DIDparams$nT)

# # Create mean Outcome

# # Dynamically calculate the means for the specified columns
# mean_outcome <- c(
#   "Mean of Outcome",
#   sapply(outcomes_twfe_reduced, function(col) sprintf("%.3f", mean(census[[col]], na.rm = TRUE)))
# )

# n_parishes <- c("Parishes", length(unique(agg_mod1$DIDparams$data$GIS_ID)), length(unique(agg_mod2$DIDparams$data$GIS_ID)))

# nodes_dropped <- c("Nodes dropped (< 10 km)", "Yes", "Yes")

# # first stage f-statistic
# firstF <- c("F-test (1st stage)", 
#             round(as.numeric(fitstat(tsls1, "ivf")$ivf[1]), 2),
#             round(as.numeric(fitstat(tsls2, "ivf")$ivf[1]), 2)
# )

# controls <- c("Controls", 
#               if (xformula == 1) rep("No", 2) 
#               else if (xformula == "Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year") rep("Yes", 2))


# # bind together for output table
# results <- rbind(twfe_iv_coef, twfe_iv_se, parish_fe, year_fe, twfe_iv_obs, firstF,
#                  twfe_red_coef, twfe_red_se, parish_fe, year_fe, twfe_reduced_obs,
#                  did_coef_red, did_se_red, n_parishes, did_reduced_obs, mean_outcome, nodes_dropped, controls)

# colnames(results) <- c(" ", "(1)", "(2)")

# results

# # Create Output latex table
# kbl(results, 
#     booktabs = T, 
#     caption = "TSLS: Railways and Grundtvig", 
#     row.names = F,
#     align = "lcc",
#     linesep = "",  # Suppress additional line spacing
#     format = "latex") %>%
#   kable_styling(
#     latex_options = c("hold_position", "scale_down") # Ensures [!h] placement
#   ) %>%
#   add_header_above(outcomes_header) %>%
#   add_header_above(c(" " = 1, "Dependent variable:" = 2), escape = F) %>%
#   group_rows("Panel A: 2SLS", 1, 6) %>%
#   group_rows("Panel B: TWFE Reduced Form", 7, 11) %>%
#   group_rows("Panel C: CS Reduced Form", 12, 15) 

# #######################################
# # === First stage table Grundtvig === #
# #######################################

# first1 = feols(
#   Assembly_house ~ 1 | GIS_ID + Year | Connected_railway ~ Connected_lcp, 
#   data = grundtvig,
#   cluster = ~ GIS_ID  # Clustering
# )

# first2 = feols(
#   Assembly_house ~ 1 | GIS_ID + Year | Connected_railway ~ Connected_lcp, 
#   data = grundtvig_iv,
#   cluster = ~ GIS_ID  # Clustering
# )

# first3 = feols(
#   Assembly_house ~ 1 | GIS_ID + Year | Connected_railway ~ Connected_lcp, 
#   data = grundtvig_cs,
#   cluster = ~ GIS_ID  # Clustering
# )

# # now same with covariats

# first4 = feols(
#   Assembly_house ~ Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year 
#   | GIS_ID + Year 
#   | Connected_railway ~ Connected_lcp, 
#   data = grundtvig,
#   cluster = ~ GIS_ID  # Clustering
# )

# first5 = feols(
#   Assembly_house ~ Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year 
#   | GIS_ID + Year 
#   | Connected_railway ~ Connected_lcp, 
#   data = grundtvig_iv,
#   cluster = ~ GIS_ID  # Clustering
# )

# first6 = feols(
#   Assembly_house ~ Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year 
#   | GIS_ID + Year 
#   | Connected_railway ~ Connected_lcp, 
#   data = grundtvig_cs,
#   cluster = ~ GIS_ID  # Clustering
# )

# # create table
# etable(first1, first2, first3, first4, first5, first6,
#        keep = "Connected_lcp",
#        stage = 1,
#        tex = T,
#        signif.code = c("*" = 0.10, "**" = 0.05, "***" = 0.01),  
#        fitstat = ~ n + ivf,
#        extralines = list(
#          "__Nodes dropped (10km)" = c("No", "Yes", "Yes", "No", "Yes", "Yes"),
#          "__Controls" = c("No", "No", "No", "Yes", "Yes", "Yes"),
#          "__Invalid comparisons dropped" = c("No", "No", "Yes", "No", "No", "Yes")
#        ))


# ===== main ==== 
main = function(){
  # Descipritve statistics:
  summary_tables()
  census_distributions()
  grundtvig_distributions_over_time()

  # Regressions:
  twfe_regressions_census()
  twfe_regressions_census(CONTROLS)
  twfe_regressions_grundtvig()
  twfe_regressions_grundtvig(CONTROLS)
  cs_estimates_census()
  cs_estimates_grundtvig()

}

# main()
