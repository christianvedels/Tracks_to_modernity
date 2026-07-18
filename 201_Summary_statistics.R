# Descriptive statistics only
#
# Date updated:   2025-10-29
# Author:         Christian Vedel, Tom Görges
# Purpose:        Summary stats + distributions

# ==== Libraries ====
library(tidyverse)
library(kableExtra) # for latex tables

source("Data_cleaning_scripts/000_Functions.R")


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
    x == "Child_women_ratio" ~ "Child-women ratio",
    x == "industry_share" ~ "Manufacturing",
    x == "non_agricultural_share" ~ "Not agriculture",
    x == "HISCAM_avg" ~ "HISCAM avg",
    x == "lnMigration" ~ "log(Migration)",
    x == "Connected_railway" ~ "Connected railroad",
    x == "Connected_lcp" ~ "Connected LCP",
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
    x == "Connected_railway" ~ "Connected railroad",
    x == "Connected_lcp" ~ "Connected LCP",
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
      mutate(across(where(is.numeric), ~ round(.x, 3)))
  }
  
  # Census table
  sum_table_census = make_summary(
    census,
    vars = c("Population", "industry_share", "non_agricultural_share",
             "Child_women_ratio", "HISCAM_avg", "Migration",
             "Connected_railway"),
    labeller = outcomeNames
  )
  
  # Grundtvig table
  sum_table_grundtvig = make_summary(
    grundtvig,
    vars = c("Assembly_house", "HighSchool", "MA_assembly", "MA_folkhigh",
             "Connected_railway"),
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
    select(Ever_rail, lnPopulation, Child_women_ratio, industry_share, 
           non_agricultural_share, HISCAM_avg, lnMigration, dist_hmb, dist_cph, 
           DistOxRoad, lnpop1801) %>%
    pivot_longer(
      cols = c(lnPopulation, Child_women_ratio, industry_share, 
               non_agricultural_share, HISCAM_avg, lnMigration, dist_hmb, 
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
    select(Treat_year, lnPopulation, Child_women_ratio, industry_share,
           non_agricultural_share, HISCAM_avg, lnMigration, dist_hmb, dist_cph, 
           DistOxRoad, lnpop1801) %>%
    pivot_longer(
      cols = c(lnPopulation, Child_women_ratio, industry_share, 
               non_agricultural_share, HISCAM_avg, lnMigration, dist_hmb, 
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

# ==== Kolmogorov-Smirnov tests ====
ks_tests_ever_treated = function(){
  tmp = census %>%
    group_by(GIS_ID) %>%
    mutate(Ever_rail = ifelse(mean(Connected_railway) > 0, "Yes", "No")) %>%
    filter(Year == 1850)
  
  tmp = tmp %>%
    filter(Connected_railway == 0) %>%
    mutate(
      lnpop1801 = log(Pop1801),
      Ever_rail_binary = ifelse(Ever_rail == "Yes", 1, 0)
    )
  
  # Variables to test
  vars = c("lnPopulation", "Child_women_ratio", "industry_share",
            "non_agricultural_share", "HISCAM_avg", "lnMigration", 
            "dist_hmb", "dist_cph", "DistOxRoad", "lnpop1801")
  
  # 0. Compute mean and standard deviation
  mean_sd_results = lapply(vars, function(var) {
    vals = tmp %>% 
      pull(!!sym(var)) %>% 
      na.omit()
    
    data.frame(
      Variable = var,
      Mean = mean(vals, na.rm = TRUE),
      SD = sd(vals, na.rm = TRUE)
    )
  }) %>%
    bind_rows()
  
  # 1. Compute regressions (instead of t-tests)
  reg_results = lapply(vars, function(var) {
    formula_str = paste0(var, " ~ Ever_rail_binary")
    reg = tryCatch(lm(as.formula(formula_str), data = tmp), error = function(e) NULL)
    
    if (!is.null(reg)) {
      coef_summary = summary(reg)$coefficients
      if(nrow(coef_summary) >= 2) {
        data.frame(
          Variable     = var,
          Coefficient  = coef_summary[2, "Estimate"],
          Std_Error    = coef_summary[2, "Std. Error"],
          t_statistic  = coef_summary[2, "t value"],
          p_value      = coef_summary[2, "Pr(>|t|)"],
          N            = nobs(reg),
          R_squared    = summary(reg)$r.squared
        )
      } else {
        data.frame(
          Variable = var, Coefficient = NA, Std_Error = NA,
          t_statistic = NA, p_value = NA, N = NA, R_squared = NA
        )
      }
    } else {
      data.frame(
        Variable = var, Coefficient = NA, Std_Error = NA,
        t_statistic = NA, p_value = NA, N = NA, R_squared = NA
      )
    }
  }) %>%
    bind_rows()
  
  # 2. Compute KS tests
  ks_results = lapply(vars, function(var) {
    no_vals = tmp %>% 
      filter(Ever_rail == "No") %>% 
      pull(!!sym(var)) %>% 
      na.omit()
    
    yes_vals = tmp %>% 
      filter(Ever_rail == "Yes") %>% 
      pull(!!sym(var)) %>% 
      na.omit()
    
    if(length(no_vals) > 0 & length(yes_vals) > 0) {
      ks_result = ks.test(no_vals, yes_vals)
      data.frame(
        Variable = var,
        D_statistic = ks_result$statistic,
        ks_pvalue = ks_result$p.value
      )
    } else {
      data.frame(
        Variable = var,
        D_statistic = NA,
        ks_pvalue = NA
      )
    }
  }) %>%
    bind_rows()
  
  # 3. Construct regression table with KS test column
  combined_results = reg_results %>%
    left_join(ks_results, by = "Variable") %>%
    left_join(mean_sd_results, by = "Variable") %>%
    mutate(
      Variable = outcomeNames(Variable),
      Mean = round(Mean, 3),
      SD = round(SD, 3),
      Coefficient = round(Coefficient, 3),
      Std_Error = round(Std_Error, 3),
      p_sig = case_when(
        p_value < 0.01 ~ "***",
        p_value < 0.05 ~ "**",
        p_value < 0.10 ~ "*",
        TRUE ~ ""
      ),
      coef_str = paste0(Coefficient, p_sig),
      se_str = paste0("(", Std_Error, ")"),
      D_statistic = round(D_statistic, 3),
      ks_pvalue = round(ks_pvalue, 3),
      ks_sig = case_when(
        ks_pvalue < 0.01 ~ "***",
        ks_pvalue < 0.05 ~ "**",
        ks_pvalue < 0.10 ~ "*",
        TRUE ~ ""
      ),
      ks_str = paste0(D_statistic, ks_sig)
    )
  
  # Create table with coefficients, std errors in parentheses, and KS column
  # Need to create long format with interleaved coefficient and SE rows
  table_rows = lapply(1:nrow(combined_results), function(i) {
    data.frame(
      Variable = c(combined_results$Variable[i], ""),
      Mean = c(combined_results$Mean[i], ""),
      SD = c(combined_results$SD[i], ""),
      `Ever Connected` = c(combined_results$coef_str[i], combined_results$se_str[i]),
      `KS D-stat` = c(combined_results$ks_str[i], ""),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }) %>% bind_rows()
  
  combined_tex = table_rows %>%
    kable(
      format = "latex",
      booktabs = TRUE,
      caption = "Regression Tests: Ever Connected vs Never Connected",
      align = "lcccc",
      escape = FALSE
    ) %>%
    kable_styling(
      latex_options = c("hold_position")
    ) %>%
    footnote(
      general = "Mean and standard deviation of each variable. OLS regressions of each variable on Ever Connected indicator (1=eventually connected, 0=never connected). Standard errors in parentheses. KS D-stat shows Kolmogorov-Smirnov test statistic. Significance: * p<0.10, ** p<0.05, *** p<0.01",
      threeparttable = TRUE
    )
  
  # 4. Save table
  sink("Tables/Distribution_tests_ever_treated.txt")
  print(combined_tex)
  sink()
  print(combined_tex)
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

# ==== Figure: Census outcomes over time by railroad connection ====
census_outcomes_over_time = function(){
  # Parishes already connected in 1850 (8 of 1589), excluded as in Figure 4
  already_connected_1850 = census %>%
    filter(Year == 1850, Connected_railway == 1) %>%
    distinct(GIS_ID) %>%
    pull(GIS_ID)
  
  p1 = census %>%
    filter(!GIS_ID %in% already_connected_1850) %>%
    group_by(GIS_ID) %>%
    mutate(Ever_rail = ifelse(mean(Connected_railway) > 0, "Yes", "No")) %>%
    ungroup() %>%
    select(Year_num, Ever_rail, lnPopulation, Child_women_ratio, industry_share,
           non_agricultural_share, HISCAM_avg, lnMigration) %>%
    pivot_longer(
      cols = c(lnPopulation, Child_women_ratio, industry_share,
               non_agricultural_share, HISCAM_avg, lnMigration),
      names_to = "var"
    ) %>%
    mutate(var = outcomeNames(var)) %>%
    group_by(var, Year_num, Ever_rail) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = Year_num, y = mean_value, col = Ever_rail)) +
    geom_line() +
    geom_point() +
    facet_wrap(~var, scales = "free", ncol = 3) +
    scale_x_continuous(breaks = c(1850, 1860, 1880, 1901), minor_breaks = NULL) +
    theme_bw() +
    labs(col = "Eventually connected to railway?", x = "Year", y = "") +
    scale_color_manual(values = c("No" = colours$black, "Yes" = colours$red)) +
    theme(legend.position = "bottom")
  
  print(p1)
  ggsave("Plots/Census_outcomes_over_time.png", p1, width = dims$width, height = dims$height)
}


# ===== main ==== 
main = function(){
  summary_tables()
  census_distributions()
  census_distributions_by_year()
  ks_tests_ever_treated()
  grundtvig_distributions_over_time()
  census_outcomes_over_time()
}

main()








