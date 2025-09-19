# Descriptive statistics only
#
# Date updated:   2025-02-13
# Author:         Christian Vedel, Tom Görges
# Purpose:        Summary stats + distributions

# ==== Libraries ====
library(tidyverse)
library(kableExtra) # for latex tables
source("Data_cleaning_scripts/000_Functions.R")

# ==== Params ====
NSIGNIF = 4 # Significant digits in all tables

# ==== Load data ====
census = read_csv2("Data/REGRESSION_DATA_Demography.csv", guess_max = 100000)
grundtvig = read_csv2("Data/REGRESSION_DATA_Grundtvigianism.csv", guess_max = 100000)

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
  MA_assembly = ifelse(is.na(MA_assembly), 0, MA_assembly),
  MA_folkhigh = ifelse(is.na(MA_folkhigh), 0, MA_folkhigh)
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

# === Summary Statistics ===
summary_tables = function() {
  
  # Helper: create summary stats for a dataset
  make_summary = function(df, vars, labeller) {
    df %>%
      summarise(across(
        all_of(vars),
        list(
          n = ~sum(!is.na(.)),
          mean = ~mean(., na.rm = TRUE),
          sd = ~sd(., na.rm = TRUE),
          min = ~min(., na.rm = TRUE),
          max = ~max(., na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )) %>%
      pivot_longer(
        everything(),
        names_to   = c("var", ".value"),
        names_pattern = "^(.*)_(n|mean|sd|min|max)$"
      ) %>%
      mutate(var = labeller(var)) %>%
      mutate(across(where(is.numeric), \(x) signif0(x, digits = NSIGNIF)))
  }
  
  # Census table
  sum_table_census = make_summary(
    census,
    vars = c("Population", "lnManufacturing", "lnNotAgriculture",
             "Child_women_ratio", "HISCAM_avg", "Migration",
             "Connected_railway", "Connected_lcp"),
    labeller = outcomeNames
  )
  
  # Grundtvig table
  sum_table_grundtvig = make_summary(
    grundtvig,
    vars = c("Assembly_house", "HighSchool", "MA_assembly", "MA_folkhigh",
             "Connected_railway", "Connected_lcp"),
    labeller = outcomeNames_grundtvig
  )
  
  # Combine
  summary_stats = bind_rows(sum_table_census, sum_table_grundtvig)
  
  # Counts for grouping
  n_census    = nrow(sum_table_census)
  n_grundtvig = nrow(sum_table_grundtvig)
  
  # Create tex table with section headers
  sum_tex = summary_stats %>%
    kable(
      format = "latex",
      booktabs = TRUE,
      caption = "Summary Statistics",
      col.names = c("Variable", "N", "Mean", "SD", "Min", "Max"),
      align = "lccccc"
    ) %>%
    kable_styling(
      latex_options = c("hold_position", "scale_down")
    ) %>%
    group_rows("A. Census", 1, n_census) %>%
    group_rows("B. Grundtvig", n_census + 1, n_census + n_grundtvig)
  
  # Export + print
  sink("Tables/Summary_Statistics.txt")
  print(sum_tex)
  sink()
  print(sum_tex)
}




summary_tables()

# ==== Densities ====
census_distributions = function(){
  tmp = census %>%
    group_by(GIS_ID) %>%
    mutate(Ever_rail = ifelse(mean(Connected_railway) > 0, "Yes", "No")) %>%
    filter(Year == 1850)
  
  p1 = tmp %>%
    filter(Connected_railway == 0) %>%
    mutate(lnpop1801 = log(Pop1801)) %>%
    select(Ever_rail, lnPopulation, lnChild_women_ratio, lnManufacturing, 
           lnNotAgriculture, HISCAM_avg, lnMigration, dist_hmb, dist_cph, 
           DistOxRoad, lnpop1801) %>%
    pivot_longer(
      cols = c(lnPopulation, lnChild_women_ratio, lnManufacturing, 
               lnNotAgriculture, HISCAM_avg, lnMigration, dist_hmb, 
               dist_cph, DistOxRoad, lnpop1801),
      names_to = "var"
    ) %>%
    mutate(var = outcomeNames(var)) %>%
    ggplot(aes(x = value, fill = Ever_rail)) +
    geom_density(alpha = 0.5) + 
    facet_wrap(~var, scales = "free", ncol = 3) +
    theme_bw() +
    labs(fill = "Eventually connected to railway?", x = "", y = "") +
    scale_fill_manual(values = c("No" = colours$black, "Yes" = colours$red)) +
    theme(legend.position = "bottom")
  
  print(p1)
  ggsave("Plots/Densities_census.png", p1, width = dims$width, height = dims$height)
}

census_distributions_by_year = function(){
  tmp = census %>%
    group_by(GIS_ID) %>%
    mutate(Ever_rail = ifelse(mean(Connected_railway) > 0, "Yes", "No")) %>%
    filter(Year == 1850)
  
  tmp = tmp %>%
    filter(Connected_railway == 0) %>%
    mutate(
      lnpop1801 = log(Pop1801),
      Treat_year = ifelse(Treat_year == 0, "Never", as.character(Treat_year))
    )
  
  # Unique years except "Never"
  years <- sort(unique(tmp$Treat_year[tmp$Treat_year != "Never"]))
  
  # Assign black for "Never" and distinct colors for each year
  palette_years <- RColorBrewer::brewer.pal(min(length(years), 8), "Set1")
  colors <- c("Never" = "black", setNames(palette_years, years))
  
  p1 = tmp %>%
    select(Treat_year, lnPopulation, lnChild_women_ratio, lnManufacturing, 
           lnNotAgriculture, HISCAM_avg, lnMigration, dist_hmb, dist_cph, 
           DistOxRoad, lnpop1801) %>%
    pivot_longer(
      cols = c(lnPopulation, lnChild_women_ratio, lnManufacturing, 
               lnNotAgriculture, HISCAM_avg, lnMigration, dist_hmb, 
               dist_cph, DistOxRoad, lnpop1801),
      names_to = "var"
    ) %>%
    mutate(var = outcomeNames(var)) %>%
    ggplot(aes(x = value, fill = Treat_year)) +
    geom_density(alpha = 0.4) + 
    facet_wrap(~var, scales = "free", ncol = 3) +
    theme_bw() +
    labs(fill = "Connection year", x = "", y = "") +
    scale_fill_manual(values = colors) +
    theme(legend.position = "bottom")
  
  print(p1)
  ggsave("Plots/Densities_census_treat_year.png", p1, width = dims$width, height = dims$height)
}


# ==== Grundtvig over time ====
grundtvig_distributions_over_time = function(){
  p1 = grundtvig %>%
    group_by(GIS_ID) %>%
    mutate(Ever_rail = ifelse(mean(Connected_railway) > 0, "Yes", "No")) %>%
    select(Year, Ever_rail, Assembly_house, HighSchool) %>%
    pivot_longer(c(Assembly_house, HighSchool), names_to = "var") %>%
    mutate(var = recode(var,
                        "Assembly_house" = "Share of parishes with an assembly house",
                        "HighSchool" = "Share of parishes with a folk high school")) %>%
    group_by(var, Year, Ever_rail) %>%
    summarise(share = mean(value, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = Year, y = share, col = Ever_rail)) +
    geom_line() +
    geom_point() +
    facet_wrap(~var, scales = "free", ncol = 1) +
    theme_bw() +
    labs(col = "Eventually connected to railway?", y = "") +
    scale_color_manual(values = c("No" = colours$black, "Yes" = colours$red)) +
    theme(legend.position = "bottom")
  
  print(p1)
  ggsave("Plots/Grundtvig_over_time.png", p1, width = dims$width, height = 1.25*dims$height)
}


# ===== main ==== 
main = function(){
  summary_tables()
  census_distributions()
  census_distributions_by_year()
  grundtvig_distributions_over_time()
}

main()
