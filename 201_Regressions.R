# Regressions
#
# Date updated:   2025-01-21
# Author:         Christian Vedel, Tom Görges
# Purpose:        Runs regressions

# ==== Libraries ====
library(tidyverse)
library(fixest)
library(did)
library(readxl)
library(psych) # for summary stats
library(kableExtra) # for latex tables
source("Data_cleaning_scripts/000_Functions.R")

# ==== Params ====
xformula = "Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year"
# xformula = "1"

# var == "lnPopulation" ~ "log(Population 1850)",
#       var == "lnpop1801" ~ "log(Population 1801)",	
#       var == "lnChild_women_ratio" ~ "log(Child-women ratio + 1)",
#       var == "lnManufacturing" ~ "log(Manufacturing + 1)",
#       var == "lnNotAgriculture" ~ "log(Not agriculture + 1)",
#       var == "HISCAM_avg" ~ "HISCAM_avg",
#       var == "lnMigration" ~ "log(Migration)",
#       var == "dist_hmb" ~ "Distance to Hamburg",
#       var == "dist_cph" ~ "Distance to Copenhagen",
#       var == "Boulder_clay_pct" ~ "Boulder clay (%)",
#       var == "area_parish" ~ "Area of parish",
#       var == "DistOxRoad" ~ "Distance to Oxroad",
#       var == "Distance_market_town" ~ "Distance to market town",

# ==== Load data ====
census = read_csv2("Data/REGRESSION_DATA_Demography.csv", guess_max = 100000)
grundtvig = read_csv2("Data/REGRESSION_DATA_Grundtvigianism.csv", guess_max = 100000)
rail_panel = read_csv2("Data/Panel_of_railways_in_parishes.csv", guess_max = 100000)

# ==== Clean census data ====
census = census %>%
  rename(
    # Rename and create new variables
    Population    = Pop,
    HISCAM_avg        = hiscam_avg,
    Migration     = Born_different_county,
    RailAccess    = Connected_rail,
    RailDist      = Distance_to_nearest_railway,
    LCPAccess     = Connected_rail_instr,
    LCPDist       = Distance_to_nearest_railway_instr
  ) %>%
  mutate(
    # Create logged variables
    lnPopulation       = log(Population),
    lnManufacturing    = log(Manufacturing_789 + 1),
    lnFarming          = log(Farming + 1),
    lnChild_women_ratio = log(Child_women_ratio + 1),
    lnHISCAM_avg       = log(HISCAM_avg),
    lnMigration        = log(Migration + 1),
    
    # Convert Year and GIS_ID for subsequent operations
    Year_num = as.numeric(as.character(Year)),
    GIS_ID_num = as.numeric(factor(GIS_ID)),
    
  ) %>%
  ungroup() %>%
  mutate(
    # Cut Boulder_clay_pct into quantiles
    Boulder_clay_decile = cut(Boulder_clay_pct, breaks = unique(quantile(Boulder_clay_pct, probs = seq(0, 1, by = 0.1), na.rm = TRUE)), include.lowest = TRUE),
    Boulder_clay_pct_year = paste(Boulder_clay_decile, Year, sep = "_"),

    # Cut Dist_hamb into quantiles
    Dist_hamb_decile = cut(dist_hmb, breaks = unique(quantile(dist_hmb, probs = seq(0, 1, by = 0.1), na.rm = TRUE)), include.lowest = TRUE),
    Dist_hamb_year = paste(Dist_hamb_decile, Year, sep = "_"),

    # Cut pop 1801 into quantiles
    Pop1801_decile = cut(Pop1801, breaks = unique(quantile(Pop1801, probs = seq(0, 1, by = 0.1), na.rm = TRUE)), include.lowest = TRUE),
    Pop1801_year = paste(Pop1801_decile, Year, sep = "_"),

    # Area parish
    area_decile = cut(area_parish, breaks = unique(quantile(area_parish, probs = seq(0, 1, by = 0.1), na.rm = TRUE)), include.lowest = TRUE),
    area_parish_year = paste(area_decile, Year, sep = "_")
  )

# Create Treat_year variable
census = census %>%
  group_by(GIS_ID) %>%
  mutate(
    Treat_year = min_treat_year(Year, RailAccess),
    Treat_year_instr = min_treat_year(Year, LCPAccess)
  ) %>%
  ungroup()

# Create Not agriculture
census = census %>% mutate(
  NotAgriculture = hisco_major0 + hisco_major1 + hisco_major2 + hisco_major3 + hisco_major4 + hisco_major5 + hisco_major7 + hisco_major8 + hisco_major9
) %>%
  mutate(
    lnNotAgriculture = log(1 + NotAgriculture)
  )


# === Delete duplicates Census ===

# Store the original number of observations
original_rows <- nrow(census)

# Remove duplicates and reassign to the same variable
census <- census %>%
  ungroup() %>%
  distinct()

# Calculate and display the number of observations removed
cat("Number of observations removed due to duplicate removal:", original_rows - nrow(census), "\n")

# ==== Clean Grundtvig data ====
grundtvig = grundtvig %>%
  rename(
    RailAccess = Connected_rail,
    LCPAccess  = Connected_rail_instr
  ) %>% 
  mutate(
    Year_num   = as.numeric(as.character(Year)),
    GIS_ID_num = as.numeric(factor(GIS_ID))
  )

# Create Treat_year variable
grundtvig = grundtvig %>%
  group_by(GIS_ID) %>%
  mutate(
    Treat_year = min_treat_year(Year, RailAccess),
    Treat_year_instr = min_treat_year(Year, LCPAccess)
  ) %>%
  ungroup()

# Redefine Assembly_house as a dummy
grundtvig = grundtvig %>%
  mutate(
    Assembly_house = ifelse(Assembly_house > 0, 1, Assembly_house),
    HighSchool = ifelse(HighSchool > 0, 1, HighSchool)
  ) %>%
  # NA in assembly house means 0 
  mutate(
    Assembly_house = replace_na(Assembly_house, 0),
  )

# Aline to same period as census data
grundtvig = grundtvig %>%
  filter(Year <= 1920)

# === Delete duplicates Grundtvig ===

# Store the original number of observations
original_rows <- nrow(grundtvig)

# Remove duplicates and reassign to the same variable
grundtvig <- grundtvig %>%
  ungroup() %>%
  distinct()

# Calculate and display the number of observations removed
cat("Number of observations removed due to duplicate removal:", original_rows - nrow(grundtvig), "\n")

# === Summary Statistics ===
# 1) Census data
stats_census <- describe(census[, c("Population", 
                           "lnManufacturing",
                           "lnNotAgriculture",
                           "Child_women_ratio", 
                           "HISCAM_avg", 
                           "Migration", 
                           "RailAccess",
                           "LCPAccess")])


# Select only relevant columns
stats_selected <- stats_census[, c("n", "mean", "sd", "min", "max")]

stats_selected <- stats_selected %>%
  mutate(across(c(mean, sd, min, max), ~ round(.x, 3)))


# Create summary statistics latex table
kbl(stats_selected, 
    booktabs = T, 
    caption = "Summary Statistics: Railways and Local Development", 
    row.names = T,
    align = "cccccc",
    linesep = "",  # Suppress additional line spacing
    format = "latex") %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down") # Ensures [!h] placement and that it firs the page
  ) %>%
  group_rows("Economy", 1, 6) %>%
  group_rows("Infrastructure", 7, 8) %>%
  footnote(general = "Here is a general comments of the table. ")


# 2) Grundtvig data
stats_grundtvig <- describe(grundtvig[, c("Assembly_house", 
                                    "HighSchool",
                                    "RailAccess",
                                    "LCPAccess")])


# Select only relevant columns
stats_selected <- stats_grundtvig[, c("n", "mean", "sd", "min", "max")]

stats_selected <- stats_selected %>%
  mutate(across(c(mean, sd, min, max), ~ round(.x, 3)))


# Create summary statistics latex table
kbl(stats_selected, 
    booktabs = T, 
    caption = "Summary Statistics: Railways and Grundtvigianism", 
    row.names = T,
    align = "ccccc",
    linesep = "",  # Suppress additional line spacing
    format = "latex") %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down") # Ensures [!h] placement
  ) %>%
  group_rows("Grundtvig", 1, 2) %>%
  group_rows("Infrastructure", 3, 4)  %>%
  footnote(general = "Here is a general comments of the table. ")

# ==== Densities ====
p1 = census %>%
  group_by(GIS_ID) %>%
  mutate(Ever_rail = case_when(mean(RailAccess) > 0 ~ "Yes", TRUE ~ "No")) %>%
  filter(Year == 1850, RailAccess == 0) %>%  # Exclude parishes with railways already
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
    area_parish, 
    DistOxRoad,
    Distance_market_town,
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
    var = case_when(
      var == "lnPopulation" ~ "log(Population 1850)",
      var == "lnpop1801" ~ "log(Population 1801)",	
      var == "lnChild_women_ratio" ~ "log(Child-women ratio + 1)",
      var == "lnManufacturing" ~ "log(Manufacturing + 1)",
      var == "lnNotAgriculture" ~ "log(Not agriculture + 1)",
      var == "HISCAM_avg" ~ "HISCAM_avg",
      var == "lnMigration" ~ "log(Migration)",
      var == "dist_hmb" ~ "Distance to Hamburg",
      var == "dist_cph" ~ "Distance to Copenhagen",
      var == "Boulder_clay_pct" ~ "Boulder clay (%)",
      var == "area_parish" ~ "Area of parish",
      var == "DistOxRoad" ~ "Distance to Oxroad",
      var == "Distance_market_town" ~ "Distance to market town",
      TRUE ~ var
    )
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

p1

ggsave("Plots/Densities_census.png", p1, width = dims$width, height = dims$height)

# Grundtvig
p1 = grundtvig %>%
  group_by(GIS_ID) %>%
  mutate(Ever_rail = case_when(mean(RailAccess) > 0 ~ "Yes", TRUE ~ "No")) %>%
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

p1
ggsave("Plots/Grundtvig_over_time.png", p1, width = 0.75*dims$width, height = 1.5*dims$height)

###############################
# === Filter out IV nodes === #
###############################

# load nodes_distance data
distance_to_nodes = read_excel("Data/distance_to_nodes.xlsx")

distance_to_nodes = distance_to_nodes %>%
  mutate(GIS_ID = as.character(GIS_ID)) %>%
  mutate(
    away_from_node = ifelse(min_distance_to_node_km > 10, 1, 0)
  )

census = left_join(census, distance_to_nodes, by = "GIS_ID")
grundtvig = left_join(grundtvig, distance_to_nodes, by = "GIS_ID")

####################################################################################################
# === Filter for parishes that are connected later === #
####################################################################################################
later_connected_GIS_ID = rail_panel %>% 
  filter(Year > 1876) %>%
  filter(Year < 1920) %>%
  group_by(GIS_ID) %>%
  summarise(
    Connected_rail = max(Connected_rail)
  ) %>% 
  filter(Connected_rail == 1) %>%
  pull(GIS_ID)

earlier_connected_GIS_ID = rail_panel %>% 
  filter(Year <= 1876) %>%
  filter(Year < 1920) %>%
  group_by(GIS_ID) %>%
  summarise(
    Connected_rail = max(Connected_rail)
  ) %>% 
  filter(Connected_rail == 1) %>% 
  pull(GIS_ID)

invalid_comparison = data.frame(
  GIS_ID = rail_panel$GIS_ID %>% unique()
) %>%
  mutate(
    connected_later = 1*(GIS_ID %in% later_connected_GIS_ID),
    connected_earlier = 1*(GIS_ID %in% earlier_connected_GIS_ID)
  ) %>%
  mutate(
    never_connected = ifelse(connected_later == 0 & connected_earlier == 0, 1, 0)
  ) %>%
  mutate(
    only_connected_later = ifelse(connected_later == 1 & connected_earlier == 0, 1, 0),
  ) %>%
  mutate(
    invalid_comparison = ifelse(only_connected_later == 1, 1, 0)
  ) %>% 
  select(GIS_ID, invalid_comparison)

census = left_join(census, invalid_comparison, by = "GIS_ID")
grundtvig = left_join(grundtvig, invalid_comparison, by = "GIS_ID")

census %>% distinct(Treat_year)
grundtvig %>% distinct(Treat_year)

census_cs = census %>% 
  filter(invalid_comparison == 0) %>%
  filter(away_from_node == 1)

grundtvig_cs = grundtvig %>%
  filter(invalid_comparison == 0) %>%
  filter(away_from_node == 1)

census_cs %>% distinct(Treat_year)
grundtvig_cs %>% distinct(Treat_year)


#######################
# === Regressions === #
#######################

# ==== TWFE regressions (Census data) ====

form1 = as.formula(paste("lnPopulation ~ RailAccess |", xformula, "+ GIS_ID + Year"))
twfe1 = feols(
  form1,
  data = census,
  cluster = ~ GIS_ID
)

form2 = as.formula(paste("lnChild_women_ratio ~ RailAccess |", xformula, "+ GIS_ID + Year"))
twfe2 = feols(
  form2,
  data = census,
  cluster = ~ GIS_ID
)

form3 = as.formula(paste("lnManufacturing ~ RailAccess |", xformula, "+ GIS_ID + Year"))
twfe3 = feols(
  form3,
  data = census,
  cluster = ~ GIS_ID
)

form4 = as.formula(paste("lnNotAgriculture ~ RailAccess |", xformula, "+ GIS_ID + Year"))
twfe4 = feols(
  form4,
  data = census,
  cluster = ~ GIS_ID
)

form5 = as.formula(paste("HISCAM_avg ~ RailAccess |", xformula, "+ GIS_ID + Year"))
twfe5 = feols(
  form5,
  data = census,
  cluster = ~ GIS_ID
)

form6 = as.formula(paste("lnMigration ~ RailAccess |", xformula, "+ GIS_ID + Year"))
twfe6 = feols(
  form6,
  data = census,
  cluster = ~ GIS_ID
)

# View results
etable(twfe1, twfe2, twfe3, twfe4, twfe5, twfe6,
       fitstat = ~ ar2 + n,  # Include R-squared and number of observations
       cluster = "GIS_ID", # Display the clustering
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1)) # Custom significance codes

etable(twfe1, twfe2, twfe3, twfe4, twfe5, twfe6,
       fitstat = ~ ar2 + n,  # Include R-squared and number of observations
       cluster = "GIS_ID", # Display the clustering
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1), # Custom significance codes
       tex = TRUE
       ) 

# ==== Doubly Robust DID: Development ====

# log(Pop)
cs_mod1 <- att_gt(
  yname = "lnPopulation",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First year of treatment
  xformla = as.formula(paste("~", xformula)),  # Any covariates 
  data = census,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# Child-woman ratio
cs_mod2 <- att_gt(
  yname = "lnChild_women_ratio",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# log Manufacturing
cs_mod3 <- att_gt(
  yname = "lnManufacturing",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# log Manufacturing
cs_mod4 <- att_gt(
  yname = "lnNotAgriculture",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# HISCAM_avg
cs_mod5 <- att_gt(
  yname = "HISCAM_avg",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# Migration
cs_mod6 <- att_gt(
  yname = "lnMigration",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First (observed) year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# Aggregate the ATT
agg_mod1 <- aggte(cs_mod1, type = "simple")
agg_mod2 <- aggte(cs_mod2, type = "simple")
agg_mod3 <- aggte(cs_mod3, type = "simple")
agg_mod4 <- aggte(cs_mod4, type = "simple")
agg_mod5 <- aggte(cs_mod5, type = "simple")
agg_mod6 <- aggte(cs_mod6, type = "simple")

# Summary
summary(agg_mod1) # Pop
summary(agg_mod2) # Child w ratio
summary(agg_mod3) # Manufacturing
summary(agg_mod4) # Not agriculture
summary(agg_mod5) # HISCAM
summary(agg_mod6) # Migration

# Aggregate the ATT
agg_mod1_dyn <- aggte(cs_mod1, type = "calendar")
agg_mod2_dyn <- aggte(cs_mod2, type = "calendar")
agg_mod3_dyn <- aggte(cs_mod3, type = "calendar")
agg_mod4_dyn <- aggte(cs_mod4, type = "calendar")
agg_mod5_dyn <- aggte(cs_mod5, type = "calendar")
agg_mod6_dyn <- aggte(cs_mod6, type = "calendar")

# Plots 
agg_mod1_dyn %>% ggdid()
agg_mod2_dyn %>% ggdid()
agg_mod3_dyn %>% ggdid()
agg_mod4_dyn %>% ggdid()
agg_mod5_dyn %>% ggdid()
agg_mod6_dyn %>% ggdid()

# === Create Latex output table ===

# Store p-values for significance ***

# Define models for each set
twfe_models <- list(twfe1, twfe2, twfe3, twfe4, twfe5, twfe6)
did_models <- list(agg_mod1, agg_mod2, agg_mod3, agg_mod4, agg_mod5, agg_mod6)

# Compute p-values for TWFE models
twfe_p <- unlist(lapply(twfe_models, function(model) {
  pnorm(abs(model$coefficients / model$se), lower.tail = FALSE) * 2
}))

# Compute p-values for DID models
did_p <- unlist(lapply(did_models, function(model) {
  pnorm(abs(model$overall.att / model$overall.se), lower.tail = FALSE) * 2
}))


# Dynamically create the Outcomes header mapping
outcomes_twfe <- c(as.character(formula(twfe1)[2]), 
            as.character(formula(twfe2)[2]), 
            as.character(formula(twfe3)[2]), 
            as.character(formula(twfe4)[2]),
            as.character(formula(twfe5)[2]), 
            as.character(formula(twfe6)[2]))

outcomes_cs <- c(agg_mod1$DIDparams$yname,
                 agg_mod2$DIDparams$yname,
                 agg_mod3$DIDparams$yname,
                 agg_mod4$DIDparams$yname,
                 agg_mod5$DIDparams$yname,
                 agg_mod6$DIDparams$yname)


# Check if headers align
if (!identical(outcomes_twfe, outcomes_cs)) {
  stop("Models don't align!")
}

# Create the named vector for add_header_above if they align
outcomes_header <- c(" " = 1, setNames(rep(1, length(outcomes_twfe)), outcomes_twfe))


twfe_coef <- c("RailAccess", 
               sprintf("%.3f", twfe1$coefficients),
               sprintf("%.3f", twfe2$coefficients), 
               sprintf("%.3f", twfe3$coefficients), 
               sprintf("%.3f", twfe4$coefficients), 
               sprintf("%.3f", twfe5$coefficients), 
               sprintf("%.3f", twfe6$coefficients))

# Add ***
twfe_coef[2:7] <- sapply(2:7, function(i) {
  coef_value <- as.numeric(twfe_coef[i])
  p_value <- as.numeric(twfe_p[i-1])
  if (!is.na(p_value)) {
    if (p_value < 0.01) {
      sprintf("%.3f***", coef_value)
    } else if (p_value < 0.05) {
      sprintf("%.3f**", coef_value)
    } else if (p_value < 0.1) {
      sprintf("%.3f*", coef_value)
    } else {
      sprintf("%.3f", coef_value)
    }
  } else {
    sprintf("%.3f", coef_value)
  }
})


# built output table row by row
twfe_se <- c("", 
             sprintf("(%.3f)", twfe1$se),
             sprintf("(%.3f)", twfe2$se), 
             sprintf("(%.3f)", twfe3$se),
             sprintf("(%.3f)", twfe4$se),
             sprintf("(%.3f)", twfe5$se),
             sprintf("(%.3f)", twfe6$se))

parish_fe <- c("Parish FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")

year_fe <- c("Year FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")

twfe_obs <- c("Obs.", twfe1$nobs, twfe2$nobs, twfe3$nobs, twfe4$nobs, twfe5$nobs, twfe6$nobs)

did_coef <- c("RailAccess", 
              sprintf("%.3f",agg_mod1$overall.att),
              sprintf("%.3f",agg_mod2$overall.att),
              sprintf("%.3f",agg_mod3$overall.att),
              sprintf("%.3f", agg_mod4$overall.att),
              sprintf("%.3f", agg_mod5$overall.att),
              sprintf("%.3f",agg_mod6$overall.att))

# Add significance stars to DID coefficients based on p-values
did_coef[2:7] <- sapply(2:7, function(i) {
  coef_value <- as.numeric(did_coef[i])
  p_value <- as.numeric(did_p[i-1])
  if (!is.na(p_value)) {
    if (p_value < 0.01) {
      sprintf("%.3f***", coef_value)
    } else if (p_value < 0.05) {
      sprintf("%.3f**", coef_value)
    } else if (p_value < 0.1) {
      sprintf("%.3f*", coef_value)
    } else {
      sprintf("%.3f", coef_value)
    }
  } else {
    sprintf("%.3f", coef_value)
  }
})

did_se <- c("", 
            sprintf("(%.3f)", agg_mod1$overall.se),
            sprintf("(%.3f)", agg_mod2$overall.se),
            sprintf("(%.3f)", agg_mod3$overall.se),
            sprintf("(%.3f)", agg_mod4$overall.se),
            sprintf("(%.3f)", agg_mod5$overall.se),
            sprintf("(%.3f)", agg_mod6$overall.se))

did_obs <- c("Obs.", 
             agg_mod1$DIDparams$n * agg_mod1$DIDparams$nT,
             agg_mod2$DIDparams$n * agg_mod2$DIDparams$nT,
             agg_mod3$DIDparams$n * agg_mod3$DIDparams$nT,
             agg_mod4$DIDparams$n * agg_mod4$DIDparams$nT,
             agg_mod5$DIDparams$n * agg_mod5$DIDparams$nT,
             agg_mod6$DIDparams$n * agg_mod6$DIDparams$nT)

# Create mean Outcome

# Dynamically calculate the means for the specified columns
mean_outcome <- c(
  "Mean of Outcome",
  sapply(outcomes_cs, function(col) sprintf("%.3f", mean(census[[col]], na.rm = TRUE)))
)

did_n_parishes <- c("Parishes", 
                    length(unique(agg_mod1$DIDparams$data$GIS_ID)),
                    length(unique(agg_mod2$DIDparams$data$GIS_ID)),
                    length(unique(agg_mod3$DIDparams$data$GIS_ID)),
                    length(unique(agg_mod4$DIDparams$data$GIS_ID)),
                    length(unique(agg_mod5$DIDparams$data$GIS_ID)),
                    length(unique(agg_mod6$DIDparams$data$GIS_ID)))


# bind together for output table
results <- rbind(twfe_coef, twfe_se, parish_fe, year_fe, twfe_obs, did_coef, did_se, did_obs, did_n_parishes, mean_outcome)
colnames(results) <- c(" ", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)")


# Create Output latex table
kbl(results, 
    booktabs = T, 
    caption = "Railways and Local Development", 
    row.names = F,
    align = "lcccccc",
    linesep = "",  # Suppress additional line spacing
    format = "latex") %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down") # Ensures [!h] placement
  ) %>%
  add_header_above(outcomes_header) %>%
  add_header_above(c(" " = 1, "Dependent variable:" = 6), escape = F) %>%
  # group_rows("Panel A: TWFE", 1, 5) %>%
  # group_rows("Panel B: Callaway and Sant'Anna", 6, 9) %>%
  # footnote(
  #   general = "Notes: This table...",
  #   number = c("Clustered (Parish) Standard errors in parentheses."),
  #   symbol = c("Significance levels: * p<0.10, ** p<0.05, *** p<0.01"),
  #   threeparttable = TRUE, # Ensures LaTeX handles the footnotes correctly
  #   escape = FALSE          # Allows LaTeX symbols in the notes
  # )
  print()


##########################################
# === Instrumental Variable Approach === #
##########################################

# ==== TSLS regressions (Census data) ====
tsls1 = feols(
  lnPopulation ~ 1 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess,
  data = census,
  cluster = ~ GIS_ID
)

tsls2 = feols(
  lnChild_women_ratio ~ 1 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess,
  data = census,
  cluster = ~ GIS_ID
)

tsls3 = feols(
  lnManufacturing ~ 1 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess,
  data = census,
  cluster = ~ GIS_ID
)

tsls4 = feols(
  lnNotAgriculture ~ 1 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess,
  data = census,
  cluster = ~ GIS_ID
)

tsls5 = feols(
  HISCAM_avg ~ 1 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess,
  data = census,
  cluster = ~ GIS_ID
)

tsls6 = feols(
  lnMigration ~ 1 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess,
  data = census,
  cluster = ~ GIS_ID
)

# first stages
etable(tsls1$iv_first_stage[[1]], tsls2$iv_first_stage[[1]], tsls3$iv_first_stage[[1]],
       tsls4$iv_first_stage[[1]], tsls5$iv_first_stage[[1]], tsls6$iv_first_stage[[1]],
       tex = TRUE,
       title = "First-Stage Estimates (With Nodes)")

# second stage
etable(
  list(tsls1, tsls2, tsls3, tsls4, tsls5, tsls6)
)

# Reduced form TWFE with Instrument
twfe1_red = feols(
  lnPopulation ~ LCPAccess | GIS_ID + Year,
  data = census_cs,
  cluster = ~ GIS_ID
)

twfe2_red = feols(
  lnChild_women_ratio ~ LCPAccess | GIS_ID + Year,
  data = census_cs,
  cluster = ~ GIS_ID
)

twfe3_red = feols(
  lnManufacturing ~ LCPAccess | GIS_ID + Year,
  data = census_cs,
  cluster = ~ GIS_ID
)

twfe4_red = feols(
  lnNotAgriculture ~ LCPAccess | GIS_ID + Year,
  data = census_cs,
  cluster = ~ GIS_ID
)

twfe5_red = feols(
  HISCAM_avg ~ LCPAccess | GIS_ID + Year,
  data = census_cs,
  cluster = ~ GIS_ID
)

twfe6_red = feols(
  lnMigration ~ LCPAccess | GIS_ID + Year,
  data = census_cs,
  cluster = ~ GIS_ID
)

etable(
  list(twfe1_red, twfe2_red, twfe3_red, twfe4_red, twfe5_red, twfe6_red)
)

### Reduced form CS

# log(Pop)
cs_mod1 <- att_gt(
  yname = "lnPopulation",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year_instr",       # First year of treatment
  xformla = ~1,               # No covariates 
  data = census_cs,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# Child-woman ratio
cs_mod2 <- att_gt(
  yname = "lnChild_women_ratio",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year_instr",       # First year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census_cs,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# log Manufacturing
cs_mod3 <- att_gt(
  yname = "lnManufacturing",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year_instr",       # First year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census_cs,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# log Not agriculture
cs_mod4 <- att_gt(
  yname = "lnNotAgriculture",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year_instr",       # First year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census_cs,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# HISCAM_avg
cs_mod5 <- att_gt(
  yname = "HISCAM_avg",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year_instr",       # First year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census_cs,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# Migration
cs_mod6 <- att_gt(
  yname = "lnMigration",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year_instr",       # First (observed) year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census_cs,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# Aggregate the ATT
agg_mod1 <- aggte(cs_mod1, type = "simple")
agg_mod2 <- aggte(cs_mod2, type = "simple")
agg_mod3 <- aggte(cs_mod3, type = "simple")
agg_mod4 <- aggte(cs_mod4, type = "simple")
agg_mod5 <- aggte(cs_mod5, type = "simple")
agg_mod6 <- aggte(cs_mod6, type = "simple")

# Summary
summary(agg_mod1) # Pop
summary(agg_mod2) # Child w ratio
summary(agg_mod3) # Manufacturing
summary(agg_mod4) # Not agriculture
summary(agg_mod5) # HISCAM
summary(agg_mod6) # Migration


# === Create Latex output table ===

# Store p-values for significance ***

# Define models for each set
twfe_iv_models <- list(tsls1, tsls2, tsls3, tsls4, tsls5, tsls6)
twfe_reduced_models <- list(twfe1_red, twfe2_red, twfe3_red, twfe4_red, twfe5_red, twfe6_red)
did_reduced_models <- list(agg_mod1, agg_mod2, agg_mod3, agg_mod4, agg_mod5, agg_mod6)

# Compute p-values for TWFE models
twfe_iv_p <- unlist(lapply(twfe_iv_models, function(model) {
  pnorm(abs(model$coefficients / model$se), lower.tail = FALSE) * 2
}))

# ... for reduced twfe
twfe_reduced_p <- unlist(lapply(twfe_reduced_models, function(model) {
  pnorm(abs(model$coefficients / model$se), lower.tail = FALSE) * 2
}))

# ... for DID models
did_reduced_p <- unlist(lapply(did_reduced_models, function(model) {
  pnorm(abs(model$overall.att / model$overall.se), lower.tail = FALSE) * 2
}))


# Dynamically create the Outcomes header mapping
outcomes_twfe_iv <- c(
  all.vars(formula(tsls1))[1], 
  all.vars(formula(tsls2))[1], 
  all.vars(formula(tsls3))[1], 
  all.vars(formula(tsls4))[1],
  all.vars(formula(tsls5))[1],
  all.vars(formula(tsls6))[1]
)

outcomes_twfe_reduced <- c(
  all.vars(formula(twfe1_red))[1], 
  all.vars(formula(twfe2_red))[1], 
  all.vars(formula(twfe3_red))[1], 
  all.vars(formula(twfe4_red))[1],
  all.vars(formula(twfe5_red))[1],
  all.vars(formula(twfe6_red))[1]
)

outcomes_cs_reduced <- c(
  agg_mod1$DIDparams$yname,
  agg_mod2$DIDparams$yname,
  agg_mod3$DIDparams$yname,
  agg_mod4$DIDparams$yname,
  agg_mod5$DIDparams$yname,
  agg_mod6$DIDparams$yname)


# Check if headers align
if (!identical(outcomes_twfe_reduced, outcomes_cs_reduced) || 
    !identical(outcomes_twfe_reduced, outcomes_twfe_iv)) {
  stop("Models don't align!")
}


# Create the named vector for add_header_above if they align
outcomes_header <- c(" " = 1, setNames(rep(1, length(outcomes_twfe_reduced)), outcomes_twfe_reduced))


twfe_iv_coef <- c("fit_RailAccess", 
                  sprintf("%.3f", tsls1$coefficients),
                  sprintf("%.3f", tsls2$coefficients),
                  sprintf("%.3f", tsls3$coefficients),
                  sprintf("%.3f", tsls4$coefficients),
                  sprintf("%.3f", tsls5$coefficients),
                  sprintf("%.3f", tsls6$coefficients))

# Add ***
twfe_iv_coef[2:7] <- sapply(2:7, function(i) {
  coef_value <- as.numeric(twfe_iv_coef[i])
  p_value <- as.numeric(twfe_iv_p[i-1])
  if (!is.na(p_value)) {
    if (p_value < 0.01) {
      sprintf("%.3f***", coef_value)
    } else if (p_value < 0.05) {
      sprintf("%.3f**", coef_value)
    } else if (p_value < 0.1) {
      sprintf("%.3f*", coef_value)
    } else {
      sprintf("%.3f", coef_value)
    }
  } else {
    sprintf("%.3f", coef_value)
  }
})

# built output table row by row
twfe_iv_se <- c("", 
                sprintf("(%.3f)", tsls1$se),
                sprintf("(%.3f)", tsls2$se),
                sprintf("(%.3f)", tsls3$se),
                sprintf("(%.3f)", tsls4$se),
                sprintf("(%.3f)", tsls5$se),
                sprintf("(%.3f)", tsls6$se))

parish_fe <- c("Parish FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
year_fe <- c("Year FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")

twfe_red_coef <- c("LCPAccess", 
                   sprintf("%.3f", twfe1_red$coefficients),
                   sprintf("%.3f", twfe2_red$coefficients),
                   sprintf("%.3f", twfe3_red$coefficients),
                   sprintf("%.3f", twfe4_red$coefficients),
                   sprintf("%.3f", twfe5_red$coefficients),
                   sprintf("%.3f", twfe6_red$coefficients))

# Add ***
twfe_red_coef[2:7] <- sapply(2:7, function(i) {
  coef_value <- as.numeric(twfe_red_coef[i])
  p_value <- as.numeric(twfe_reduced_p[i-1])
  if (!is.na(p_value)) {
    if (p_value < 0.01) {
      sprintf("%.3f***", coef_value)
    } else if (p_value < 0.05) {
      sprintf("%.3f**", coef_value)
    } else if (p_value < 0.1) {
      sprintf("%.3f*", coef_value)
    } else {
      sprintf("%.3f", coef_value)
    }
  } else {
    sprintf("%.3f", coef_value)
  }
})

# SE's
twfe_red_se <- c("", 
                 sprintf("(%.3f)", twfe1_red$se),
                 sprintf("(%.3f)", twfe2_red$se),
                 sprintf("(%.3f)", twfe3_red$se),
                 sprintf("(%.3f)", twfe4_red$se),
                 sprintf("(%.3f)", twfe5_red$se),
                 sprintf("(%.3f)", twfe6_red$se))

# did coeffcicients reduced form
did_coef_red <- c("LCPAccess", 
                  sprintf("%.3f", agg_mod1$overall.att),
                  sprintf("%.3f", agg_mod2$overall.att),
                  sprintf("%.3f", agg_mod3$overall.att),
                  sprintf("%.3f", agg_mod4$overall.att),
                  sprintf("%.3f", agg_mod5$overall.att),
                  sprintf("%.3f", agg_mod6$overall.att))

# Add significance stars to DID coefficients based on p-values
did_coef_red[2:7] <- sapply(2:7, function(i) {
  coef_value <- as.numeric(did_coef_red[i])
  p_value <- as.numeric(did_reduced_p[i-1])
  if (!is.na(p_value)) {
    if (p_value < 0.01) {
      sprintf("%.3f***", coef_value)
    } else if (p_value < 0.05) {
      sprintf("%.3f**", coef_value)
    } else if (p_value < 0.1) {
      sprintf("%.3f*", coef_value)
    } else {
      sprintf("%.3f", coef_value)
    }
  } else {
    sprintf("%.3f", coef_value)
  }
})

# Standard error
did_se_red <- c("", 
                sprintf("(%.3f)", agg_mod1$overall.se),
                sprintf("(%.3f)", agg_mod2$overall.se),
                sprintf("(%.3f)", agg_mod3$overall.se),
                sprintf("(%.3f)", agg_mod4$overall.se),
                sprintf("(%.3f)", agg_mod5$overall.se),
                sprintf("(%.3f)", agg_mod6$overall.se))

# Observations
twfe_iv_obs <- c("Obs.", tsls1$nobs, tsls2$nobs, tsls3$nobs, tsls4$nobs, tsls5$nobs, tsls6$nobs)
twfe_reduced_obs <- c("Obs.", twfe1_red$nobs, twfe2_red$nobs, twfe3_red$nobs, twfe4_red$nobs, twfe5_red$nobs, twfe6_red$nobs)
did_reduced_obs <- c("Obs.", 
                     agg_mod1$DIDparams$n * agg_mod1$DIDparams$nT,
                     agg_mod2$DIDparams$n * agg_mod2$DIDparams$nT,
                     agg_mod3$DIDparams$n * agg_mod3$DIDparams$nT,
                     agg_mod4$DIDparams$n * agg_mod4$DIDparams$nT,
                     agg_mod5$DIDparams$n * agg_mod5$DIDparams$nT,
                     agg_mod6$DIDparams$n * agg_mod6$DIDparams$nT)

# Create mean Outcome

# Dynamically calculate the means for the specified columns
mean_outcome <- c(
  "Mean of Outcome",
  sapply(outcomes_twfe_reduced, function(col) sprintf("%.3f", mean(census[[col]], na.rm = TRUE)))
)

n_parishes <- c("Parishes", 
                length(unique(agg_mod1$DIDparams$data$GIS_ID)),
                length(unique(agg_mod2$DIDparams$data$GIS_ID)),
                length(unique(agg_mod3$DIDparams$data$GIS_ID)),
                length(unique(agg_mod4$DIDparams$data$GIS_ID)),
                length(unique(agg_mod5$DIDparams$data$GIS_ID)),
                length(unique(agg_mod6$DIDparams$data$GIS_ID)))

nodes_dropped <- c("Nodes dropped (<5km)", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
  
# first stage f-statistic
firstF <- c("F-test (1st stage)", 
            round(as.numeric(fitstat(tsls1, "ivf")$ivf[1]), 2),
            round(as.numeric(fitstat(tsls2, "ivf")$ivf[1]), 2),
            round(as.numeric(fitstat(tsls3, "ivf")$ivf[1]), 2),
            round(as.numeric(fitstat(tsls4, "ivf")$ivf[1]), 2),
            round(as.numeric(fitstat(tsls5, "ivf")$ivf[1]), 2),
            round(as.numeric(fitstat(tsls6, "ivf")$ivf[1]), 2)
            )



# bind together for output table
results <- rbind(twfe_iv_coef, twfe_iv_se, parish_fe, year_fe, twfe_iv_obs, firstF,
                 twfe_red_coef, twfe_red_se, parish_fe, year_fe, twfe_reduced_obs,
                 did_coef_red, did_se_red, n_parishes, did_reduced_obs, mean_outcome, nodes_dropped)

colnames(results) <- c(" ", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)")

# Create Output latex table
kbl(results, 
    booktabs = T, 
    caption = "TSLS: Railways and Local Development", 
    row.names = F,
    align = "lcccccc",
    linesep = "",  # Suppress additional line spacing
    format = "latex") %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down") # Ensures [!h] placement
  ) %>%
  add_header_above(outcomes_header) %>%
  add_header_above(c(" " = 1, "Dependent variable:" = 6), escape = F) %>%
  # group_rows("Panel A: 2SLS", 1, 6) %>%
  # group_rows("Panel B: TWFE Reduced Form", 7, 11) %>%
  # group_rows("Panel C: CS Reduced Form", 12, 15) %>%
  # footnote(
  #   general = "Notes: This table...",
  #   number = c("Clustered (Parish) Standard errors in parentheses."),
  #   symbol = c("Significance levels: * p<0.10, ** p<0.05, *** p<0.01"),
  #   threeparttable = TRUE, # Ensures LaTeX handles the footnotes correctly
  #   escape = FALSE          # Allows LaTeX symbols in the notes
  # )
  print()

######################
# ==== Grundtvig === #
######################

# TWFE
mod1 = feols(
  Assembly_house ~ RailAccess | GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)

mod2 = feols(
  HighSchool ~ RailAccess | GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)

# View results
etable(mod1, mod2)

# ==== Doubly Robust DID: Grundtvig ====

# No covariates
out1 = att_gt(
  yname = "Assembly_house",
  tname = "Year_num",
  gname = "Treat_year",
  idname = "GIS_ID_num",
  data = grundtvig,
)

out2 = att_gt(
  yname = "HighSchool",
  tname = "Year_num",
  gname = "Treat_year",
  idname = "GIS_ID_num",
  data = grundtvig,
)

# Aggregate the ATT
agg_mod1 <- aggte(out1, type = "simple")
agg_mod2 <- aggte(out2, type = "simple")

# Summary
summary(agg_mod1)
summary(agg_mod2)

# Dynamic summary
agg_mod1_dyn = aggte(out1, type = "dynamic")
agg_mod2_dyn = aggte(out2, type = "dynamic")
agg_mod1_dyn %>% ggdid()
agg_mod2_dyn %>% ggdid()

# === Create Latex output table ===

# Store p-values for significance ***

# Define models for each set
twfe_models <- list(mod1, mod2)
did_models <- list(agg_mod1, agg_mod2)

# Compute p-values for TWFE models
twfe_p <- unlist(lapply(twfe_models, function(model) {
  pnorm(abs(model$coefficients / model$se), lower.tail = FALSE) * 2
}))

# Compute p-values for DID models
did_p <- unlist(lapply(did_models, function(model) {
  pnorm(abs(model$overall.att / model$overall.se), lower.tail = FALSE) * 2
}))


# Dynamically create the Outcomes header mapping
outcomes_twfe <- c(as.character(formula(mod1)[2]), 
                   as.character(formula(mod2)[2]))

outcomes_cs <- c(agg_mod1$DIDparams$yname,
                 agg_mod2$DIDparams$yname)


# Check if headers align
if (!identical(outcomes_twfe, outcomes_cs)) {
  stop("Models don't align!")
}

# Create the named vector for add_header_above if they align
outcomes_header <- c(" " = 1, setNames(rep(1, length(outcomes_twfe)), outcomes_twfe))

# Built output table row by row

# TWFE Coefficients
twfe_coef <- c("RailAccess", sprintf("%.3f", mod1$coefficients), sprintf("%.3f", mod2$coefficients))

# Add ***
twfe_coef[2:3] <- sapply(2:3, function(i) {
  coef_value <- as.numeric(twfe_coef[i])
  p_value <- as.numeric(twfe_p[i-1])
  if (!is.na(p_value)) {
    if (p_value < 0.01) {
      sprintf("%.3f***", coef_value)
    } else if (p_value < 0.05) {
      sprintf("%.3f**", coef_value)
    } else if (p_value < 0.1) {
      sprintf("%.3f*", coef_value)
    } else {
      sprintf("%.3f", coef_value)
    }
  } else {
    sprintf("%.3f", coef_value)
  }
})

# TWFE SEs
twfe_se <- c("", sprintf("(%.3f)", mod1$se), sprintf("(%.3f)",mod2$se))

# Fixed Effects
parish_fe <- c("Parish FE", "Yes", "Yes")
year_fe <- c("Year FE", "Yes", "Yes")

# nObs
twfe_obs <- c("Obs.", mod1$nobs, mod2$nobs)
did_obs <- c("Obs.", 
             agg_mod1$DIDparams$n * agg_mod1$DIDparams$nT,
             agg_mod2$DIDparams$n * agg_mod1$DIDparams$nT)

# CS DiD coefficient
did_coef <- c("RailAccess", sprintf("%.3f",agg_mod1$overall.att), sprintf("%.3f",agg_mod2$overall.att))

# Add significance stars to DID coefficients based on p-values
did_coef[2:3] <- sapply(2:3, function(i) {
  coef_value <- as.numeric(did_coef[i])
  p_value <- as.numeric(did_p[i-1])
  if (!is.na(p_value)) {
    if (p_value < 0.01) {
      sprintf("%.3f***", coef_value)
    } else if (p_value < 0.05) {
      sprintf("%.3f**", coef_value)
    } else if (p_value < 0.1) {
      sprintf("%.3f*", coef_value)
    } else {
      sprintf("%.3f", coef_value)
    }
  } else {
    sprintf("%.3f", coef_value)
  }
})


did_se <- c("", sprintf("(%.3f)", agg_mod1$overall.se), sprintf("(%.3f)", agg_mod2$overall.se))


# Create mean Outcome
mean_outcome <- c(
  "Mean of Outcome",
  sapply(outcomes_cs, function(col) sprintf("%.3f", mean(census[[col]], na.rm = TRUE)))
)

# #Parishes
did_n_parishes <- c("Parishes", length(unique(agg_mod1$DIDparams$data$GIS_ID)), length(unique(agg_mod2$DIDparams$data$GIS_ID)))

# bind together for output table

# bind together
results <- rbind(twfe_coef, twfe_se, parish_fe, year_fe, twfe_obs, did_coef, did_se, did_n_parishes, did_obs, mean_outcome)
colnames(results) <- c(" ", "(1)", "(2)")

# Create Output latex table
kbl(results, 
    booktabs = T, 
    caption = "Railways and Grundtvigianism", 
    row.names = F,
    align = "lcc",
    linesep = "",  # Suppress additional line spacing
    format = "latex") %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down") # Ensures [!h] placement
  ) %>%
  add_header_above(outcomes_header) %>%
  add_header_above(c(" " = 1, "Dependent variable:" = 2), escape = F) %>%
  # group_rows("Panel A: TWFE", 1, 5) %>%
  # group_rows("Panel B: Callaway and Sant'Anna", 6, 9) %>% 
  footnote(
    general = "Notes: This table...",
    number = c("Clustered (Parish) Standard errors in parentheses."),
    symbol = c("Significance levels: * p<0.10, ** p<0.05, *** p<0.01"),
    threeparttable = TRUE, # Ensures LaTeX handles the footnotes correctly
    escape = FALSE          # Allows LaTeX symbols in the notes
  )

# With Instrument ##########################################################################

tsls1 = feols(
  Assembly_house ~ 1 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess,
  data = grundtvig,
  cluster = ~ GIS_ID
)

tsls2 = feols(
  HighSchool ~ 1 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess,
  data = grundtvig,
  cluster = ~ GIS_ID
)

# first stages
etable(tsls1$iv_first_stage[[1]], tsls2$iv_first_stage[[1]],
       tex = TRUE,
       title = "First-Stage Estimates (With Nodes)")

# second stage
etable(tsls1, tsls2,
       tex = TRUE,
       title = "First-Stage Estimates (With Nodes)")

twfe1_red <- feols(
  Assembly_house ~ LCPAccess 
  | GIS_ID + Year,
  data = grundtvig_cs,
  cluster = ~ GIS_ID
)

twfe2_red <- feols(
  HighSchool ~ LCPAccess 
  | GIS_ID + Year,
  data = grundtvig_cs,
  cluster = ~ GIS_ID
)



out1 = att_gt(
  yname = "Assembly_house",
  tname = "Year_num",
  gname = "Treat_year_instr",
  idname = "GIS_ID_num",
  data = grundtvig_cs,
)

out2 = att_gt(
  yname = "HighSchool",
  tname = "Year_num",
  gname = "Treat_year_instr",
  idname = "GIS_ID_num",
  data = grundtvig_cs,
)

# Aggregate the ATT
agg_mod1 <- aggte(out1, type = "simple", na.rm = T)
agg_mod2 <- aggte(out2, type = "simple", na.rm = T)

# Summary
summary(agg_mod1)
summary(agg_mod2)

# === Create Latex output table ===

# Store p-values for significance ***

# Define models for each set
twfe_iv_models <- list(tsls1, tsls2)
twfe_reduced_models <- list(twfe1_red, twfe2_red)
did_reduced_models <- list(agg_mod1, agg_mod2)

# Compute p-values for TWFE models
twfe_iv_p <- unlist(lapply(twfe_iv_models, function(model) {
  pnorm(abs(model$coefficients / model$se), lower.tail = FALSE) * 2
}))

# ... for reduced twfe
twfe_reduced_p <- unlist(lapply(twfe_reduced_models, function(model) {
  pnorm(abs(model$coefficients / model$se), lower.tail = FALSE) * 2
}))

# ... for DID models
did_reduced_p <- unlist(lapply(did_reduced_models, function(model) {
  pnorm(abs(model$overall.att / model$overall.se), lower.tail = FALSE) * 2
}))


# Dynamically create the Outcomes header mapping
outcomes_twfe_iv <- c(
  all.vars(formula(tsls1))[1], 
  all.vars(formula(tsls2))[1]
)

outcomes_twfe_reduced <- c(
  all.vars(formula(twfe1_red))[1], 
  all.vars(formula(twfe2_red))[1]
)

outcomes_cs_reduced <- c(agg_mod1$DIDparams$yname,
                         agg_mod2$DIDparams$yname
)


# Check if headers align
if (!identical(outcomes_twfe_reduced, outcomes_cs_reduced) || 
    !identical(outcomes_twfe_reduced, outcomes_twfe_iv)) {
  stop("Models don't align!")
}


# Create the named vector for add_header_above if they align
outcomes_header <- c(" " = 1, setNames(rep(1, length(outcomes_twfe_reduced)), outcomes_twfe_reduced))

# coefficients
twfe_iv_coef <- c("fit_RailAccess", sprintf("%.3f", tsls1$coefficients), sprintf("%.3f", tsls2$coefficients))

# Add ***
twfe_iv_coef[2:3] <- sapply(2:3, function(i) {
  coef_value <- as.numeric(twfe_iv_coef[i])
  p_value <- as.numeric(twfe_iv_p[i-1])
  if (!is.na(p_value)) {
    if (p_value < 0.01) {
      sprintf("%.3f***", coef_value)
    } else if (p_value < 0.05) {
      sprintf("%.3f**", coef_value)
    } else if (p_value < 0.1) {
      sprintf("%.3f*", coef_value)
    } else {
      sprintf("%.3f", coef_value)
    }
  } else {
    sprintf("%.3f", coef_value)
  }
})

# built output table row by row
twfe_iv_se <- c("", sprintf("(%.3f)", tsls1$se), sprintf("(%.3f)",tsls2$se))
parish_fe <- c("Parish FE", "Yes", "Yes")
year_fe <- c("Year FE", "Yes", "Yes")

twfe_red_coef <- c("LCPAccess", sprintf("%.3f", twfe1_red$coefficients), sprintf("%.3f", twfe2_red$coefficients))

# Add ***
twfe_red_coef[2:3] <- sapply(2:3, function(i) {
  coef_value <- as.numeric(twfe_red_coef[i])
  p_value <- as.numeric(twfe_reduced_p[i-1])
  if (!is.na(p_value)) {
    if (p_value < 0.01) {
      sprintf("%.3f***", coef_value)
    } else if (p_value < 0.05) {
      sprintf("%.3f**", coef_value)
    } else if (p_value < 0.1) {
      sprintf("%.3f*", coef_value)
    } else {
      sprintf("%.3f", coef_value)
    }
  } else {
    sprintf("%.3f", coef_value)
  }
})

# SE's
twfe_red_se <- c("", sprintf("(%.3f)", twfe1_red$se), sprintf("(%.3f)", twfe2_red$se))

# did coefficicients reduced form
did_coef_red <- c("LCPAccess", sprintf("%.3f", agg_mod1$overall.att), sprintf("%.3f",agg_mod2$overall.att))

# Add significance stars to DID coefficients based on p-values
did_coef_red[2:3] <- sapply(2:3, function(i) {
  coef_value <- as.numeric(did_coef_red[i])
  p_value <- as.numeric(did_reduced_p[i-1])
  if (!is.na(p_value)) {
    if (p_value < 0.01) {
      sprintf("%.3f***", coef_value)
    } else if (p_value < 0.05) {
      sprintf("%.3f**", coef_value)
    } else if (p_value < 0.1) {
      sprintf("%.3f*", coef_value)
    } else {
      sprintf("%.3f", coef_value)
    }
  } else {
    sprintf("%.3f", coef_value)
  }
})

# Standard error
did_se_red <- c("", sprintf("(%.3f)", agg_mod1$overall.se), sprintf("(%.3f)", agg_mod2$overall.se))

# Observations
twfe_iv_obs <- c("Obs.", tsls1$nobs, tsls2$nobs)
twfe_reduced_obs <- c("Obs.", twfe1_red$nobs, twfe2_red$nobs)
did_reduced_obs <- c("Obs.", agg_mod1$DIDparams$n*agg_mod1$DIDparams$nT, agg_mod2$DIDparams$n*agg_mod2$DIDparams$nT)

# Create mean Outcome

# Dynamically calculate the means for the specified columns
mean_outcome <- c(
  "Mean of Outcome",
  sapply(outcomes_twfe_reduced, function(col) sprintf("%.3f", mean(census[[col]], na.rm = TRUE)))
)

n_parishes <- c("Parishes", length(unique(agg_mod1$DIDparams$data$GIS_ID)), length(unique(agg_mod2$DIDparams$data$GIS_ID)))

nodes_dropped <- c("Nodes dropped (< 10 km)", "Yes", "Yes")
# first stage f-statistic
firstF <- c("F-test (1st stage)", 
            round(as.numeric(fitstat(tsls1, "ivf")$ivf[1]), 2),
            round(as.numeric(fitstat(tsls2, "ivf")$ivf[1]), 2)
)

# bind together for output table
results <- rbind(twfe_iv_coef, twfe_iv_se, parish_fe, year_fe, twfe_iv_obs, firstF,
                 twfe_red_coef, twfe_red_se, parish_fe, year_fe, twfe_reduced_obs,
                 did_coef_red, did_se_red, n_parishes, did_reduced_obs, mean_outcome, nodes_dropped)

colnames(results) <- c(" ", "(1)", "(2)")

results

# Create Output latex table
kbl(results, 
    booktabs = T, 
    caption = "TSLS: Railways and Grundtvig", 
    row.names = F,
    align = "lcc",
    linesep = "",  # Suppress additional line spacing
    format = "latex") %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down") # Ensures [!h] placement
  ) %>%
  add_header_above(outcomes_header) %>%
  add_header_above(c(" " = 1, "Dependent variable:" = 2), escape = F) %>%
  group_rows("Panel A: 2SLS", 1, 6) %>%
  group_rows("Panel B: TWFE Reduced Form", 7, 11) %>%
  group_rows("Panel C: CS Reduced Form", 12, 15) %>%
  footnote(
    general = "Notes: This table...",
    number = c("Clustered (Parish) Standard errors in parentheses."),
    symbol = c("Significance levels: * p<0.10, ** p<0.05, *** p<0.01"),
    threeparttable = TRUE, # Ensures LaTeX handles the footnotes correctly
    escape = FALSE          # Allows LaTeX symbols in the notes
  )











