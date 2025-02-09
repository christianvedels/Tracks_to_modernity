# Regressions
#
# Date updated:   2025-01-21
# Author:         Christian Vedel, Tom Görges
# Purpose:        Runs regressions

# ==== Libraries ====
library(tidyverse)
library(fixest)
library(did)
library(psych) # for summary stats
library(kableExtra) # for latex tables
source("Data_cleaning_scripts/000_Functions.R")

# ==== Params ====
xformula = "Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year"
# xformula = "1"

# ==== Load data ====
census = read_csv2("Data/REGRESSION_DATA_Demography.csv", guess_max = 100000)
grundtvig = read_csv2("Data/REGRESSION_DATA_Grundtvigianism.csv", guess_max = 100000)
rail_panel = read_csv2("Data/Panel_of_railways_in_parishes.csv", guess_max = 100000)

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

# ==== Regressions ====

# ==== TWFE regressions (Census data) ====
form1 = as.formula(paste("lnPopulation ~ RailAccess +", xformula, "| GIS_ID + Year"))
twfe1 = feols(
  form1,
  data = census,
  cluster = ~ GIS_ID
)

form2 = as.formula(paste("lnChild_women_ratio ~ RailAccess +", xformula, "| GIS_ID + Year"))
twfe2 = feols(
  form2,
  data = census,
  cluster = ~ GIS_ID
)

form3 = as.formula(paste("lnManufacturing ~ RailAccess +", xformula, "| GIS_ID + Year"))
twfe3 = feols(
  form3,
  data = census,
  cluster = ~ GIS_ID
)

form4 = as.formula(paste("lnNotAgriculture ~ RailAccess +", xformula, "| GIS_ID + Year"))
twfe4 = feols(
  form4,
  data = census,
  cluster = ~ GIS_ID
)

form5 = as.formula(paste("HISCAM_avg ~ RailAccess +", xformula, "| GIS_ID + Year"))
twfe5 = feols(
  form5,
  data = census,
  cluster = ~ GIS_ID
)

form6 = as.formula(paste("lnMigration ~ RailAccess +", xformula, "| GIS_ID + Year"))
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
  xformla = as.formula(paste("~", xformula)),               # No covariates (consistent with TWFE)
  data = census,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# log Manufacturing
cs_mod3 <- att_gt(
  yname = "lnManufacturing",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First year of treatment
  xformla = as.formula(paste("~", xformula)),               # No covariates (consistent with TWFE)
  data = census,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# log Manufacturing
cs_mod4 <- att_gt(
  yname = "lnNotAgriculture",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First year of treatment
  xformla = as.formula(paste("~", xformula)),               # No covariates (consistent with TWFE)
  data = census,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# HISCAM_avg
cs_mod5 <- att_gt(
  yname = "HISCAM_avg",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First year of treatment
  xformla = as.formula(paste("~", xformula)),               # No covariates (consistent with TWFE)
  data = census,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# Migration
cs_mod6 <- att_gt(
  yname = "lnMigration",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First (observed) year of treatment
  xformla = as.formula(paste("~", xformula)),               # No covariates (consistent with TWFE)
  data = census,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# Aggregate the ATT
agg_mod1 <- aggte(cs_mod1, type = "simple", na.rm = T)
agg_mod2 <- aggte(cs_mod2, type = "simple", na.rm = T)
agg_mod3 <- aggte(cs_mod3, type = "simple", na.rm = T)
agg_mod4 <- aggte(cs_mod4, type = "simple", na.rm = T)
agg_mod5 <- aggte(cs_mod5, type = "simple", na.rm = T)
agg_mod6 <- aggte(cs_mod6, type = "simple", na.rm = T)

# Summary
summary(agg_mod1) # Pop
summary(agg_mod2) # Child w ratio
summary(agg_mod3) # Manufacturing
summary(agg_mod4) # Not agriculture
summary(agg_mod5) # HISCAM
summary(agg_mod6) # Migration

# Aggregate the ATT
agg_mod1_dyn <- aggte(cs_mod1, type = "calendar", na.rm = T)
agg_mod2_dyn <- aggte(cs_mod2, type = "calendar", na.rm = T)
agg_mod3_dyn <- aggte(cs_mod3, type = "calendar", na.rm = T)
agg_mod4_dyn <- aggte(cs_mod4, type = "calendar", na.rm = T)
agg_mod5_dyn <- aggte(cs_mod5, type = "calendar", na.rm = T)
agg_mod6_dyn <- aggte(cs_mod6, type = "calendar", na.rm = T)

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
               sprintf("%.3f", twfe1$coefficients[1]),
               sprintf("%.3f", twfe2$coefficients[1]), 
               sprintf("%.3f", twfe3$coefficients[1]), 
               sprintf("%.3f", twfe4$coefficients[1]), 
               sprintf("%.3f", twfe5$coefficients[1]), 
               sprintf("%.3f", twfe6$coefficients[1]))

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
             sprintf("(%.3f)", twfe1$se[1]),
             sprintf("(%.3f)", twfe2$se[1]), 
             sprintf("(%.3f)", twfe3$se[1]),
             sprintf("(%.3f)", twfe4$se[1]),
             sprintf("(%.3f)", twfe5$se[1]),
             sprintf("(%.3f)", twfe6$se[1]))

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

controls <- c("Controls", 
              if (xformula == 1) rep("No", 6) else if (xformula == "Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year") rep("Yes", 6))


# bind together for output table
results <- rbind(twfe_coef, twfe_se, parish_fe, year_fe, twfe_obs, did_coef, did_se, did_obs, did_n_parishes, mean_outcome, controls)
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
  group_rows("Panel A: TWFE", 1, 5) %>%
  group_rows("Panel B: Callaway and Sant'Anna", 6, 9) 

# ==== Instrumental Variable Approach ====

# ==== TSLS regressions (Census data) ====
form1 <- as.formula(paste("lnPopulation ~", xformula, "| GIS_ID + Year | RailAccess ~ LCPAccess"))
tsls1 = feols(
  form1,  # Full formula including covariates, fixed effects, and IV
  data = census_iv,
  cluster = ~ GIS_ID  # Clustering
)

form2 <- as.formula(paste("lnChild_women_ratio ~", xformula, "| GIS_ID + Year | RailAccess ~ LCPAccess"))
tsls2 = feols(
  form2,
  data = census_iv,
  cluster = ~ GIS_ID
)

form3 <- as.formula(paste("lnManufacturing ~", xformula, "| GIS_ID + Year | RailAccess ~ LCPAccess"))
tsls3 = feols(
  form3,
  data = census_iv,
  cluster = ~ GIS_ID
)

form4 <- as.formula(paste("lnNotAgriculture ~", xformula, "| GIS_ID + Year | RailAccess ~ LCPAccess"))
tsls4 = feols(
  form4,
  data = census_iv,
  cluster = ~ GIS_ID
)

form5 <- as.formula(paste("HISCAM_avg ~", xformula, "| GIS_ID + Year | RailAccess ~ LCPAccess"))
tsls5 = feols(
  form5,
  data = census_iv,
  cluster = ~ GIS_ID
)

form6 <- as.formula(paste("lnMigration ~", xformula, "| GIS_ID + Year | RailAccess ~ LCPAccess"))
tsls6 = feols(
  form6,
  data = census_iv,
  cluster = ~ GIS_ID
)

# Define the controls row dynamically
controls <- if (xformula == 1) {
  rep("No", 6)  # If xformula is 1, set all models to "No"
} else if (xformula == "Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year") {
  rep("Yes", 6)  # If xformula matches the specified string, set all to "Yes"
} else {
  rep("Mixed", 6)  # Default case if xformula does not match the predefined conditions
}

nodes_dropped <- rep("Yes", 6)

# second stages output table
etable(
  list(tsls1, tsls2, tsls3, tsls4, tsls5, tsls6),
  signif.code = c("*" = 0.10, "**" = 0.05, "***" = 0.01),  
  fitstat = ~ n + ivf,
  tex = T,
  digits = 3,
  keep = "RailAccess",
  title = "Railways and local development (TSLS estimates)",
  extralines = list(
    "__Nodes dropped (10km)" = nodes_dropped,
    "__Controls" = controls
  )
)


# === Reduced form TWFE with Instrument ===
form1 = as.formula(paste("lnPopulation ~ LCPAccess +", xformula, "| GIS_ID + Year"))
twfe1_red = feols(
  form1,
  data = census_cs,
  cluster = ~ GIS_ID
)

form2 = as.formula(paste("lnChild_women_ratio ~ LCPAccess +", xformula, "| GIS_ID + Year"))
twfe2_red = feols(
  form2,
  data = census_cs,
  cluster = ~ GIS_ID
)

form3 = as.formula(paste("lnManufacturing ~ LCPAccess +", xformula, "| GIS_ID + Year"))
twfe3_red = feols(
  form3,
  data = census_cs,
  cluster = ~ GIS_ID
)

form4 = as.formula(paste("lnNotAgriculture ~ LCPAccess +", xformula, "| GIS_ID + Year"))
twfe4_red = feols(
  form4,
  data = census_cs,
  cluster = ~ GIS_ID
)

form5 = as.formula(paste("HISCAM_avg ~ LCPAccess +", xformula, "| GIS_ID + Year"))
twfe5_red = feols(
  form5,
  data = census_cs,
  cluster = ~ GIS_ID
)

form6 = as.formula(paste("lnMigration ~ LCPAccess +", xformula, "| GIS_ID + Year"))
twfe6_red = feols(
  form6,
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
  xformla = as.formula(paste("~", xformula)),              # No covariates 
  data = census_cs,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# Child-woman ratio
cs_mod2 <- att_gt(
  yname = "lnChild_women_ratio",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year_instr",       # First year of treatment
  xformla = as.formula(paste("~", xformula)),              # No covariates (consistent with TWFE)
  data = census_cs,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# log Manufacturing
cs_mod3 <- att_gt(
  yname = "lnManufacturing",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year_instr",       # First year of treatment
  xformla = as.formula(paste("~", xformula)),               # No covariates (consistent with TWFE)
  data = census_cs,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# log Not agriculture
cs_mod4 <- att_gt(
  yname = "lnNotAgriculture",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year_instr",       # First year of treatment
  xformla = as.formula(paste("~", xformula)),              # No covariates (consistent with TWFE)
  data = census_cs,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# HISCAM_avg
cs_mod5 <- att_gt(
  yname = "HISCAM_avg",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year_instr",       # First year of treatment
  xformla = as.formula(paste("~", xformula)),             # No covariates (consistent with TWFE)
  data = census_cs,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# Migration
cs_mod6 <- att_gt(
  yname = "lnMigration",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year_instr",       # First (observed) year of treatment
  xformla = as.formula(paste("~", xformula)),              # No covariates (consistent with TWFE)
  data = census_cs,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# Aggregate the ATT
agg_mod1 <- aggte(cs_mod1, type = "simple", na.rm = T)
agg_mod2 <- aggte(cs_mod2, type = "simple", na.rm = T)
agg_mod3 <- aggte(cs_mod3, type = "simple", na.rm = T)
agg_mod4 <- aggte(cs_mod4, type = "simple", na.rm = T)
agg_mod5 <- aggte(cs_mod5, type = "simple", na.rm = T)
agg_mod6 <- aggte(cs_mod6, type = "simple", na.rm = T)

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
                  sprintf("%.3f", tsls1$coefficients[1]),
                  sprintf("%.3f", tsls2$coefficients[1]),
                  sprintf("%.3f", tsls3$coefficients[1]),
                  sprintf("%.3f", tsls4$coefficients[1]),
                  sprintf("%.3f", tsls5$coefficients[1]),
                  sprintf("%.3f", tsls6$coefficients[1]))

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
                sprintf("(%.3f)", tsls1$se[1]),
                sprintf("(%.3f)", tsls2$se[1]),
                sprintf("(%.3f)", tsls3$se[1]),
                sprintf("(%.3f)", tsls4$se[1]),
                sprintf("(%.3f)", tsls5$se[1]),
                sprintf("(%.3f)", tsls6$se[1]))

parish_fe <- c("Parish FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
year_fe <- c("Year FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")

twfe_red_coef <- c("LCPAccess", 
                   sprintf("%.3f", twfe1_red$coefficients[1]),
                   sprintf("%.3f", twfe2_red$coefficients[1]),
                   sprintf("%.3f", twfe3_red$coefficients[1]),
                   sprintf("%.3f", twfe4_red$coefficients[1]),
                   sprintf("%.3f", twfe5_red$coefficients[1]),
                   sprintf("%.3f", twfe6_red$coefficients[1]))

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
                 sprintf("(%.3f)", twfe1_red$se[1]),
                 sprintf("(%.3f)", twfe2_red$se[1]),
                 sprintf("(%.3f)", twfe3_red$se[1]),
                 sprintf("(%.3f)", twfe4_red$se[1]),
                 sprintf("(%.3f)", twfe5_red$se[1]),
                 sprintf("(%.3f)", twfe6_red$se[1]))

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

controls <- c("Controls", 
              if (xformula == 1) rep("No", 6) 
              else if (xformula == "Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year") rep("Yes", 6))


n_parishes <- c("Parishes", 
                length(unique(agg_mod1$DIDparams$data$GIS_ID)),
                length(unique(agg_mod2$DIDparams$data$GIS_ID)),
                length(unique(agg_mod3$DIDparams$data$GIS_ID)),
                length(unique(agg_mod4$DIDparams$data$GIS_ID)),
                length(unique(agg_mod5$DIDparams$data$GIS_ID)),
                length(unique(agg_mod6$DIDparams$data$GIS_ID)))

nodes_dropped <- c("Nodes dropped (10km)", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
  
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
                 did_coef_red, did_se_red, n_parishes, did_reduced_obs, mean_outcome, nodes_dropped, controls)

colnames(results) <- c(" ", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)")

# Create Output latex table
kbl(results, 
    booktabs = T, 
    caption = "TSLS vs Reduced form: Railways and Local Development", 
    row.names = F,
    align = "lcccccc",
    linesep = "",  # Suppress additional line spacing
    format = "latex") %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down") # Ensures [!h] placement
  ) %>%
  add_header_above(outcomes_header) %>%
  add_header_above(c(" " = 1, "Dependent variable:" = 6), escape = F) %>%
  group_rows("Panel A: 2SLS", 1, 6) %>%
  group_rows("Panel B: TWFE Reduced Form", 7, 11) %>%
  group_rows("Panel C: CS Reduced Form", 12, 15)

#############################
# === First-stage table === #
#############################
first1 = feols(
  lnPopulation ~ 1 | GIS_ID + Year | RailAccess ~ LCPAccess, 
  data = census,
  cluster = ~ GIS_ID  # Clustering
)

first2 = feols(
  lnPopulation ~ 1 | GIS_ID + Year | RailAccess ~ LCPAccess, 
  data = census_iv,
  cluster = ~ GIS_ID  # Clustering
)

first3 = feols(
  lnPopulation ~ 1 | GIS_ID + Year | RailAccess ~ LCPAccess, 
  data = census_cs,
  cluster = ~ GIS_ID  # Clustering
)

# now same with covariats

first4 = feols(
  lnPopulation ~ Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess, 
  data = census,
  cluster = ~ GIS_ID  # Clustering
)

first5 = feols(
  lnPopulation ~ Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess, 
  data = census_iv,
  cluster = ~ GIS_ID  # Clustering
)

first6 = feols(
  lnPopulation ~ Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess, 
  data = census_cs,
  cluster = ~ GIS_ID  # Clustering
)

# create table
etable(first1, first2, first3, first4, first5, first6,
       keep = "LCPAccess",
       stage = 1,
       tex = T,
       signif.code = c("*" = 0.10, "**" = 0.05, "***" = 0.01),  
       fitstat = ~ n + ivf,
       extralines = list(
         "__Nodes dropped (10km)" = c("No", "Yes", "Yes", "No", "Yes", "Yes"),
         "__Controls" = c("No", "No", "No", "Yes", "Yes", "Yes"),
         "__Invalid comparisons dropped" = c("No", "No", "Yes", "No", "No", "Yes")
       ))


######################
# ==== Grundtvig === #
######################


# TWFE
form1 = as.formula(paste("Assembly_house ~ RailAccess +", xformula, "| GIS_ID + Year"))
mod1 = feols(
  form1,
  data = grundtvig,
  cluster = ~ GIS_ID
)

form2 = as.formula(paste("HighSchool ~ RailAccess +", xformula, "| GIS_ID + Year"))
mod2 = feols(
  form2,
  data = grundtvig,
  cluster = ~ GIS_ID
)

# View results
etable(mod1, mod2,
       keep = "RailAccess")

# ==== Doubly Robust DID: Grundtvig ====

# Assembly house
out1 = att_gt(
  yname = "Assembly_house",
  tname = "Year_num",
  gname = "Treat_year",
  idname = "GIS_ID_num",
  xformla = as.formula(paste("~", xformula)),
  data = grundtvig,
)

# Folk high school
out2 = att_gt(
  yname = "HighSchool",
  tname = "Year_num",
  gname = "Treat_year",
  idname = "GIS_ID_num",
  xformla = as.formula(paste("~", xformula)),
  data = grundtvig,
)

# Aggregate the ATT
agg_mod1 <- aggte(out1, type = "simple", na.rm = T)
agg_mod2 <- aggte(out2, type = "simple", na.rm = T)

# Summary
summary(agg_mod1)
summary(agg_mod2)

# Dynamic summary
#agg_mod1_dyn = aggte(out1, type = "dynamic")
#agg_mod2_dyn = aggte(out2, type = "dynamic")
#agg_mod1_dyn %>% ggdid()
#agg_mod2_dyn %>% ggdid()

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
twfe_coef <- c("RailAccess", 
               sprintf("%.3f", mod1$coefficients[1]),
               sprintf("%.3f", mod2$coefficients[1]))

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
twfe_se <- c("", 
             sprintf("(%.3f)", mod1$se[1]),
             sprintf("(%.3f)", mod2$se[1]))

# Fixed Effects
parish_fe <- c("Parish FE", "Yes", "Yes")
year_fe <- c("Year FE", "Yes", "Yes")

# nObs
twfe_obs <- c("Obs.", mod1$nobs, mod2$nobs)
did_obs <- c("Obs.", 
             agg_mod1$DIDparams$n * agg_mod1$DIDparams$nT,
             agg_mod2$DIDparams$n * agg_mod1$DIDparams$nT)

# CS DiD coefficient
did_coef <- c("RailAccess", 
              sprintf("%.3f", agg_mod1$overall.att),
              sprintf("%.3f", agg_mod2$overall.att))

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


did_se <- c("", 
            sprintf("(%.3f)", agg_mod1$overall.se),
            sprintf("(%.3f)", agg_mod2$overall.se))


# Create mean Outcome
mean_outcome <- c(
  "Mean of Outcome",
  sapply(outcomes_cs, function(col) sprintf("%.3f", mean(census[[col]], na.rm = TRUE)))
)

# #Parishes
did_n_parishes <- c("Parishes", length(unique(agg_mod1$DIDparams$data$GIS_ID)), length(unique(agg_mod2$DIDparams$data$GIS_ID)))

controls <- c("Controls", 
              if (xformula == 1) rep("No", 2) 
              else if (xformula == "Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year") rep("Yes", 2))


# bind together
results <- rbind(twfe_coef, twfe_se, parish_fe, year_fe, twfe_obs, did_coef, did_se, did_n_parishes, did_obs, mean_outcome, controls)
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
  group_rows("Panel A: TWFE", 1, 5) %>%
  group_rows("Panel B: Callaway and Sant'Anna", 6, 9)

# With Instrument ##########################################################################

# ==== TSLS regressions (Census data) ====
form1 <- as.formula(paste("Assembly_house ~", xformula, "| GIS_ID + Year | RailAccess ~ LCPAccess"))
tsls1 = feols(
  form1,
  data = grundtvig_iv,
  cluster = ~ GIS_ID
)

form2 <- as.formula(paste("HighSchool ~", xformula, "| GIS_ID + Year | RailAccess ~ LCPAccess"))
tsls2 = feols(
  form2,
  data = grundtvig_iv,
  cluster = ~ GIS_ID
)

# Define the controls row dynamically
controls <- if (xformula == 1) {
  rep("No", 2)  # If xformula is 1, set all models to "No"
} else if (xformula == "Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year") {
  rep("Yes", 2)  # If xformula matches the specified string, set all to "Yes"
} else {
  rep("?", 2)  # Default case if xformula does not match the predefined conditions
}

nodes_dropped <- rep("Yes", 2)

# second stages output table
etable(
  list(tsls1, tsls2),
  signif.code = c("*" = 0.10, "**" = 0.05, "***" = 0.01),  
  fitstat = ~ n + ivf,
  tex = T,
  digits = 3,
  keep = "RailAccess",
  title = "Railways and Grundtvigianism (TSLS estimates)",
  extralines = list(
    "__Nodes dropped (10km)" = nodes_dropped,
    "__Controls" = controls
  )
)

#####

# REDUCED FORM
form1 = as.formula(paste("Assembly_house ~ LCPAccess +", xformula, "| GIS_ID + Year"))
twfe1_red <- feols(
  form1,
  data = grundtvig_cs,
  cluster = ~ GIS_ID
)

form2 = as.formula(paste("HighSchool ~ LCPAccess +", xformula, "| GIS_ID + Year"))
twfe2_red <- feols(
  form2,
  data = grundtvig_cs,
  cluster = ~ GIS_ID
)

out1 = att_gt(
  yname = "Assembly_house",
  tname = "Year_num",
  gname = "Treat_year_instr",
  idname = "GIS_ID_num",
  xformla = as.formula(paste("~", xformula)),
  data = grundtvig_cs,
)

out2 = att_gt(
  yname = "HighSchool",
  tname = "Year_num",
  gname = "Treat_year_instr",
  idname = "GIS_ID_num",
  xformla = as.formula(paste("~", xformula)),
  data = grundtvig_cs,
)

# Aggregate the ATT
agg_mod1 <- aggte(out1, type = "simple", na.rm = T) # When using covariats: Fehler in max(t) : ungültiger 'type' (list) des Argumentes
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
twfe_iv_coef <- c("fit_RailAccess", 
                  sprintf("%.3f", tsls1$coefficients[1]), sprintf("%.3f", tsls2$coefficients[1]))

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
twfe_iv_se <- c("", 
                sprintf("(%.3f)", tsls1$se[1]), 
                sprintf("(%.3f)", tsls2$se[1]))

parish_fe <- c("Parish FE", "Yes", "Yes")
year_fe <- c("Year FE", "Yes", "Yes")

twfe_red_coef <- c("LCPAccess", 
                   sprintf("%.3f", twfe1_red$coefficients[1]), 
                   sprintf("%.3f", twfe2_red$coefficients[1]))

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
twfe_red_se <- c("", 
                 sprintf("(%.3f)", twfe1_red$se[1]),
                 sprintf("(%.3f)", twfe2_red$se[1]))

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

controls <- c("Controls", 
              if (xformula == 1) rep("No", 2) 
              else if (xformula == "Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year") rep("Yes", 2))


# bind together for output table
results <- rbind(twfe_iv_coef, twfe_iv_se, parish_fe, year_fe, twfe_iv_obs, firstF,
                 twfe_red_coef, twfe_red_se, parish_fe, year_fe, twfe_reduced_obs,
                 did_coef_red, did_se_red, n_parishes, did_reduced_obs, mean_outcome, nodes_dropped, controls)

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
  group_rows("Panel C: CS Reduced Form", 12, 15) 

#######################################
# === First stage table Grundtvig === #
#######################################

first1 = feols(
  Assembly_house ~ 1 | GIS_ID + Year | RailAccess ~ LCPAccess, 
  data = grundtvig,
  cluster = ~ GIS_ID  # Clustering
)

first2 = feols(
  Assembly_house ~ 1 | GIS_ID + Year | RailAccess ~ LCPAccess, 
  data = grundtvig_iv,
  cluster = ~ GIS_ID  # Clustering
)

first3 = feols(
  Assembly_house ~ 1 | GIS_ID + Year | RailAccess ~ LCPAccess, 
  data = grundtvig_cs,
  cluster = ~ GIS_ID  # Clustering
)

# now same with covariats

first4 = feols(
  Assembly_house ~ Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess, 
  data = grundtvig,
  cluster = ~ GIS_ID  # Clustering
)

first5 = feols(
  Assembly_house ~ Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess, 
  data = grundtvig_iv,
  cluster = ~ GIS_ID  # Clustering
)

first6 = feols(
  Assembly_house ~ Boulder_clay_pct_year + Dist_hamb_year + Pop1801_year + area_parish_year + Dist_mt_year + Dist_cph_year + Dist_ox_year 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess, 
  data = grundtvig_cs,
  cluster = ~ GIS_ID  # Clustering
)

# create table
etable(first1, first2, first3, first4, first5, first6,
       keep = "LCPAccess",
       stage = 1,
       tex = T,
       signif.code = c("*" = 0.10, "**" = 0.05, "***" = 0.01),  
       fitstat = ~ n + ivf,
       extralines = list(
         "__Nodes dropped (10km)" = c("No", "Yes", "Yes", "No", "Yes", "Yes"),
         "__Controls" = c("No", "No", "No", "Yes", "Yes", "Yes"),
         "__Invalid comparisons dropped" = c("No", "No", "Yes", "No", "No", "Yes")
       ))









