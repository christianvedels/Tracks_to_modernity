# Simple regressions
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

# clear workspace
rm(list = ls())

# ==== Load and prepare Census data ====
census <- read_csv2("Data/REGRESSION_DATA_Demography.csv") %>%
  mutate(
    # Rename and create new variables
    Population    = Pop,
    HISCAM        = hiscam_avg,
    Migration     = Born_different_county,
    RailAccess    = Connected_rail,
    RailDist      = Distance_to_nearest_railway,
    LCPAccess     = Connected_rail_instr,
    LCPDist       = Distance_to_nearest_railway_instr,
    
    # Create logged variables
    lnPopulation       = log(Pop),
    lnManufacturing    = log(Manufacturing_789 + 1),
    lnFarming          = log(Farming + 1),
    lnChild_women_ratio = log(Child_women_ratio + 1),
    lnHISCAM           = log(HISCAM),
    lnMigration        = log(Migration + 1),
    
    # Convert Year and GIS_ID for subsequent operations
    Year_num = as.numeric(as.character(Year)),
    GIS_ID_num = as.numeric(factor(GIS_ID))
  ) %>%
  group_by(GIS_ID) %>%
  mutate(
    # Identify the first year with Connected_rail equal to 1;
    # if none exist, set Treat_year to 0
    Treat_year = ifelse(any(Connected_rail == 1), min(Year[Connected_rail == 1]), 0),
    # Ensure Treat_year is consistent across groups (i.e., only positive values are kept)
    Treat_year = ifelse(Treat_year > 0, Treat_year, 0),
    Treat_year_instr = ifelse(any(Connected_rail_instr == 1), min(Year[Connected_rail_instr == 1]), 0),
    Treat_year_instr = ifelse(Treat_year_instr > 0, Treat_year_instr, 0)
  ) %>%
  ungroup()

# create Share_Manufacturing
census$Pop_work <- rowSums(census[, grep("^hisco_major[0-9]$", names(census))], na.rm = TRUE)

# Create Share Manufacturing
census$Share_Manufacturing <- (census$Manufacturing_789 / census$Pop_work)*100


# === Delete duplicates Census ===

# Store the original number of observations
original_rows <- nrow(census)

# Remove duplicates and reassign to the same variable
census <- census %>%
  ungroup() %>%
  distinct()

# Calculate and display the number of observations removed
cat("Number of observations removed due to duplicate removal:", original_rows - nrow(census), "\n")

# ==== Load and prepare Grundtvig data ====
grundtvig <- read_csv2("Data/REGRESSION_DATA_Grundtvigianism.csv") %>%
  mutate(
    RailAccess = Connected_rail,
    RailDist   = Distance_to_nearest_railway,
    LCPAccess  = Connected_rail_instr,
    LCPDist    = Distance_to_nearest_railway_instr,
    Year_num   = as.numeric(as.character(Year)),
    GIS_ID_num = as.numeric(factor(GIS_ID))
  ) %>%
  group_by(GIS_ID) %>%
  mutate(
    # Identify the first year when Connected_rail is 1
    Treat_year = ifelse(any(Connected_rail == 1), min(Year[Connected_rail == 1]), 0),
    # Ensure Treat_year is consistent across groups
    Treat_year = ifelse(Treat_year > 0, Treat_year, 0)
  ) %>%
  ungroup()

# Compute Treat_year_instr
grundtvig <- grundtvig %>%
  group_by(GIS_ID) %>%
  mutate(Treat_year_instr = ifelse(any(LCPAccess == 1, na.rm = TRUE), 
                                   min(Year_num[LCPAccess == 1], na.rm = TRUE), 
                                   0)) %>%
  ungroup()


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
                           "Share_Manufacturing", 
                           "Child_women_ratio", 
                           "HISCAM", 
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
    align = "ccccc",
    linesep = "",  # Suppress additional line spacing
    format = "latex") %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down") # Ensures [!h] placement and that it firs the page
  ) %>%
  group_rows("Panel A: Economy", 1, 5) %>%
  group_rows("Panel B: Infrastructure", 6, 7) %>%
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
  group_rows("Panel A: Grundtvig", 1, 2) %>%
  group_rows("Panel B: Infrastructure", 3, 4)  %>%
  footnote(general = "Here is a general comments of the table. ")

# ==== Densities ====
p1 = census %>% 
  group_by(GIS_ID) %>%
  mutate(Ever_rail = ifelse(mean(RailAccess) > 0, "Yes", "No")) %>%
  filter(Year == 1850) %>%
  filter(RailAccess == 0) %>% # Exclude parishes with railways already
  select(Ever_rail, Population, Share_Manufacturing) %>%
  pivot_longer(c(Population, Share_Manufacturing), names_to = "var") %>%
  ggplot(aes(x = value, fill = Ever_rail)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~var, scales = "free") + 
  theme_bw()

ggsave("Plots/Densities.png", p1, width = 8, height = 4)

###############################
# === Filter out IV nodes === #
###############################

# load nodes_distance data
distance_to_nodes <- read_excel("Data/distance_to_nodes.xlsx")

distance_to_nodes <- distance_to_nodes %>%
  mutate(GIS_ID = as.character(GIS_ID))

census <- left_join(census, distance_to_nodes, by = "GIS_ID")
grundtvig <- left_join(grundtvig, distance_to_nodes, by = "GIS_ID")


# filter 10 km
census_iv <- census %>% filter(min_distance_to_node_km > 10)
grundtvig_iv <- grundtvig %>% filter(min_distance_to_node_km > 10)


# ==== TWFE regressions (Census data) ====
twfe1 = feols(
  lnPopulation ~ RailAccess | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

twfe2 = feols(
  lnChild_women_ratio ~ RailAccess | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

twfe3 = feols(
  Share_Manufacturing ~ RailAccess | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

twfe4 = feols(
  HISCAM ~ RailAccess | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

twfe5 = feols(
  lnMigration ~ RailAccess | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

# View results
etable(twfe1, twfe2, twfe3, twfe4, twfe5,
       fitstat = ~ ar2 + n,  # Include R-squared and number of observations
       cluster = "GIS_ID", # Display the clustering
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1)) # Custom significance codes


# ==== Doubly Robust DID: Development ====

# log(Pop)
cs_mod1 <- att_gt(
  yname = "lnPopulation",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First year of treatment
  xformla = ~1,               # No covariates 
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
  yname = "Share_Manufacturing",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# HISCAM
cs_mod4 <- att_gt(
  yname = "HISCAM",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# Migration
cs_mod5 <- att_gt(
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

# Summary
summary(agg_mod1)
summary(agg_mod2)
summary(agg_mod3)
summary(agg_mod4)
summary(agg_mod5)


# === Create Latex output table ===

# Store p-values for significance ***

# Define models for each set
twfe_models <- list(twfe1, twfe2, twfe3, twfe4, twfe5)
did_models <- list(agg_mod1, agg_mod2, agg_mod3, agg_mod4, agg_mod5)

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
            as.character(formula(twfe5)[2]))

outcomes_cs <- c(agg_mod1$DIDparams$yname,
                        agg_mod2$DIDparams$yname,
                        agg_mod3$DIDparams$yname,
                        agg_mod4$DIDparams$yname,
                        agg_mod5$DIDparams$yname)


# Check if headers align
if (!identical(outcomes_twfe, outcomes_cs)) {
  stop("Models don't align!")
}

# Create the named vector for add_header_above if they align
outcomes_header <- c(" " = 1, setNames(rep(1, length(outcomes_twfe)), outcomes_twfe))


twfe_coef <- c("RailAccess", sprintf("%.3f", twfe1$coefficients), sprintf("%.3f", twfe2$coefficients), sprintf("%.3f", twfe3$coefficients), sprintf("%.3f", twfe4$coefficients), sprintf("%.3f", twfe5$coefficients))

# Add ***
twfe_coef[2:6] <- sapply(2:6, function(i) {
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
twfe_se <- c("", sprintf("(%.3f)", twfe1$se), sprintf("(%.3f)",twfe2$se), sprintf("(%.3f)",twfe3$se), sprintf("(%.3f)",twfe4$se), sprintf("(%.3f)",twfe5$se))
parish_fe <- c("Parish FE", "Yes", "Yes", "Yes", "Yes", "Yes")
year_fe <- c("Year FE", "Yes", "Yes", "Yes", "Yes", "Yes")
twfe_obs <- c("Obs.", twfe1$nobs, twfe2$nobs, twfe3$nobs, twfe4$nobs, twfe5$nobs)
did_coef <- c("RailAccess", sprintf("%.3f",agg_mod1$overall.att), sprintf("%.3f",agg_mod2$overall.att), sprintf("%.3f",agg_mod3$overall.att), sprintf("%.3f", agg_mod4$overall.att), sprintf("%.3f",agg_mod5$overall.att))

# Add significance stars to DID coefficients based on p-values
did_coef[2:6] <- sapply(2:6, function(i) {
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


did_se <- c("", sprintf("(%.3f)", agg_mod1$overall.se), sprintf("(%.3f)", agg_mod2$overall.se), sprintf("(%.3f)", agg_mod3$overall.se), sprintf("(%.3f)", agg_mod4$overall.se), sprintf("(%.3f)", agg_mod5$overall.se))

did_obs <- c("Obs.", agg_mod1$DIDparams$n * agg_mod1$DIDparams$nT, agg_mod2$DIDparams$n* agg_mod2$DIDparams$nT, agg_mod3$DIDparams$n* agg_mod3$DIDparams$nT, agg_mod4$DIDparams$n* agg_mod4$DIDparams$nT, agg_mod5$DIDparams$n* agg_mod5$DIDparams$nT)

# Create mean Outcome

# Dynamically calculate the means for the specified columns
mean_outcome <- c(
  "Mean of Outcome",
  sapply(outcomes_cs, function(col) sprintf("%.3f", mean(census[[col]], na.rm = TRUE)))
)

did_n_parishes <- c("Parishes", length(unique(agg_mod1$DIDparams$data$GIS_ID)), length(unique(agg_mod2$DIDparams$data$GIS_ID)), length(unique(agg_mod3$DIDparams$data$GIS_ID)), length(unique(agg_mod4$DIDparams$data$GIS_ID)), length(unique(agg_mod5$DIDparams$data$GIS_ID)))


# bind together for output table
results <- rbind(twfe_coef, twfe_se, parish_fe, year_fe, twfe_obs, did_coef, did_se, did_obs, did_n_parishes, mean_outcome)
colnames(results) <- c(" ", "(1)", "(2)", "(3)", "(4)", "(5)")


# Create Output latex table
kbl(results, 
    booktabs = T, 
    caption = "Railways and Local Development", 
    row.names = F,
    align = "lccccc",
    linesep = "",  # Suppress additional line spacing
    format = "latex") %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down") # Ensures [!h] placement
  ) %>%
  add_header_above(outcomes_header) %>%
  add_header_above(c(" " = 1, "Dependent variable:" = 5), escape = F) %>%
  group_rows("Panel A: TWFE", 1, 5) %>%
  group_rows("Panel B: Callaway and Sant'Anna", 6, 9) %>%
  footnote(
    general = "Notes: This table...",
    number = c("Clustered (Parish) Standard errors in parentheses."),
    symbol = c("Significance levels: * p<0.10, ** p<0.05, *** p<0.01"),
    threeparttable = TRUE, # Ensures LaTeX handles the footnotes correctly
    escape = FALSE          # Allows LaTeX symbols in the notes
  )


##########################################
# === Instrumental Variable Approach === #
##########################################

# ==== TSLS regressions (Census data) ====
tsls1 = feols(
  lnPopulation ~ 1 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess,
  data = census_iv,
  cluster = ~ GIS_ID
)


tsls2 = feols(
  lnChild_women_ratio ~ 1 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess,
  data = census_iv,
  cluster = ~ GIS_ID
)

tsls3 = feols(
  Share_Manufacturing ~ 1 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess,
  data = census_iv,
  cluster = ~ GIS_ID
)

tsls4 = feols(
  HISCAM ~ 1 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess,
  data = census_iv,
  cluster = ~ GIS_ID
)

tsls5 = feols(
  lnMigration ~ 1 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess,
  data = census_iv,
  cluster = ~ GIS_ID
)

# first stages
etable(tsls1$iv_first_stage[[1]], tsls2$iv_first_stage[[1]], tsls3$iv_first_stage[[1]],
       tsls4$iv_first_stage[[1]], tsls5$iv_first_stage[[1]], 
       tex = TRUE,
       title = "First-Stage Estimates (With Nodes)")

# Reduced form TWFE with Instrument
twfe1_red = feols(
  lnPopulation ~ LCPAccess | GIS_ID + Year,
  data = census_iv,
  cluster = ~ GIS_ID
)

twfe2_red = feols(
  lnChild_women_ratio ~ LCPAccess | GIS_ID + Year,
  data = census_iv,
  cluster = ~ GIS_ID
)

twfe3_red = feols(
  Share_Manufacturing ~ LCPAccess | GIS_ID + Year,
  data = census_iv,
  cluster = ~ GIS_ID
)

twfe4_red = feols(
  HISCAM ~ LCPAccess | GIS_ID + Year,
  data = census_iv,
  cluster = ~ GIS_ID
)

twfe5_red = feols(
  lnMigration ~ LCPAccess | GIS_ID + Year,
  data = census_iv,
  cluster = ~ GIS_ID
)

### Reduced form CS

# log(Pop)
cs_mod1 <- att_gt(
  yname = "lnPopulation",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year_instr",       # First year of treatment
  xformla = ~1,               # No covariates 
  data = census_iv,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# Child-woman ratio
cs_mod2 <- att_gt(
  yname = "lnChild_women_ratio",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year_instr",       # First year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census_iv,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# log Manufacturing
cs_mod3 <- att_gt(
  yname = "Share_Manufacturing",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year_instr",       # First year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census_iv,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# HISCAM
cs_mod4 <- att_gt(
  yname = "HISCAM",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year_instr",       # First year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census_iv,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# Migration
cs_mod5 <- att_gt(
  yname = "lnMigration",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year_instr",       # First (observed) year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census_iv,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# Aggregate the ATT
agg_mod1 <- aggte(cs_mod1, type = "simple")
agg_mod2 <- aggte(cs_mod2, type = "simple")
agg_mod3 <- aggte(cs_mod3, type = "simple")
agg_mod4 <- aggte(cs_mod4, type = "simple")
agg_mod5 <- aggte(cs_mod5, type = "simple")

# Summary
summary(agg_mod1)
summary(agg_mod2)
summary(agg_mod3)
summary(agg_mod4)
summary(agg_mod5)

# === Create Latex output table ===

# Store p-values for significance ***

# Define models for each set
twfe_iv_models <- list(tsls1, tsls2, tsls3, tsls4, tsls5)
twfe_reduced_models <- list(twfe1_red, twfe2_red, twfe3_red, twfe4_red, twfe5_red)
did_reduced_models <- list(agg_mod1, agg_mod2, agg_mod3, agg_mod4, agg_mod5)

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
  all.vars(formula(tsls5))[1]
)

outcomes_twfe_reduced <- c(
  all.vars(formula(twfe1_red))[1], 
  all.vars(formula(twfe2_red))[1], 
  all.vars(formula(twfe3_red))[1], 
  all.vars(formula(twfe4_red))[1], 
  all.vars(formula(twfe5_red))[1]
)

outcomes_cs_reduced <- c(agg_mod1$DIDparams$yname,
                 agg_mod2$DIDparams$yname,
                 agg_mod3$DIDparams$yname,
                 agg_mod4$DIDparams$yname,
                 agg_mod5$DIDparams$yname)


# Check if headers align
if (!identical(outcomes_twfe_reduced, outcomes_cs_reduced) || 
    !identical(outcomes_twfe_reduced, outcomes_twfe_iv)) {
  stop("Models don't align!")
}


# Create the named vector for add_header_above if they align
outcomes_header <- c(" " = 1, setNames(rep(1, length(outcomes_twfe_reduced)), outcomes_twfe_reduced))


twfe_iv_coef <- c("fit_RailAccess", sprintf("%.3f", tsls1$coefficients), sprintf("%.3f", tsls2$coefficients), sprintf("%.3f", tsls3$coefficients), sprintf("%.3f", tsls4$coefficients), sprintf("%.3f", tsls5$coefficients))

# Add ***
twfe_iv_coef[2:6] <- sapply(2:6, function(i) {
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
twfe_iv_se <- c("", sprintf("(%.3f)", tsls1$se), sprintf("(%.3f)",tsls2$se), sprintf("(%.3f)",tsls3$se), sprintf("(%.3f)",tsls4$se), sprintf("(%.3f)",tsls5$se))
parish_fe <- c("Parish FE", "Yes", "Yes", "Yes", "Yes", "Yes")
year_fe <- c("Year FE", "Yes", "Yes", "Yes", "Yes", "Yes")

twfe_red_coef <- c("LCPAccess", sprintf("%.3f", twfe1_red$coefficients), sprintf("%.3f", twfe2_red$coefficients), sprintf("%.3f", twfe3_red$coefficients), sprintf("%.3f", twfe4_red$coefficients), sprintf("%.3f", twfe5_red$coefficients))

# Add ***
twfe_red_coef[2:6] <- sapply(2:6, function(i) {
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
twfe_red_se <- c("", sprintf("(%.3f)", twfe1_red$se), sprintf("(%.3f)", twfe2_red$se), sprintf("(%.3f)", twfe3_red$se), sprintf("(%.3f)",twfe4_red$se), sprintf("(%.3f)", twfe5_red$se))

# did coeffcicients reduced form
did_coef_red <- c("LCPAccess", sprintf("%.3f", agg_mod1$overall.att), sprintf("%.3f",agg_mod2$overall.att), sprintf("%.3f",agg_mod3$overall.att), sprintf("%.3f", agg_mod4$overall.att), sprintf("%.3f",agg_mod5$overall.att))

# Add significance stars to DID coefficients based on p-values
did_coef_red[2:6] <- sapply(2:6, function(i) {
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
did_se_red <- c("", sprintf("(%.3f)", agg_mod1$overall.se), sprintf("(%.3f)", agg_mod2$overall.se), sprintf("(%.3f)", agg_mod3$overall.se), sprintf("(%.3f)", agg_mod4$overall.se), sprintf("(%.3f)", agg_mod5$overall.se))

# Observations
twfe_iv_obs <- c("Obs.", tsls1$nobs, tsls2$nobs, tsls3$nobs, tsls4$nobs, tsls5$nobs)
twfe_reduced_obs <- c("Obs.", twfe1_red$nobs, twfe2_red$nobs, twfe3_red$nobs, twfe4_red$nobs, twfe5_red$nobs)
did_reduced_obs <- c("Obs.", agg_mod1$DIDparams$n * agg_mod1$DIDparams$nT, agg_mod2$DIDparams$n* agg_mod2$DIDparams$nT, agg_mod3$DIDparams$n* agg_mod3$DIDparams$nT, agg_mod4$DIDparams$n* agg_mod4$DIDparams$nT, agg_mod5$DIDparams$n* agg_mod5$DIDparams$nT)

# Create mean Outcome

# Dynamically calculate the means for the specified columns
mean_outcome <- c(
  "Mean of Outcome",
  sapply(outcomes_twfe_reduced, function(col) sprintf("%.3f", mean(census[[col]], na.rm = TRUE)))
)

n_parishes <- c("Parishes", length(unique(agg_mod1$DIDparams$data$GIS_ID)), length(unique(agg_mod2$DIDparams$data$GIS_ID)), length(unique(agg_mod3$DIDparams$data$GIS_ID)), length(unique(agg_mod4$DIDparams$data$GIS_ID)), length(unique(agg_mod5$DIDparams$data$GIS_ID)))

nodes_dropped <- c("Nodes dropped (<10km)", "No", "No", "No", "No", "No")
  
# first stage f-statistic
firstF <- c("F-test (1st stage)", 
            round(as.numeric(fitstat(tsls1, "ivf")$ivf[1]), 2),
            round(as.numeric(fitstat(tsls2, "ivf")$ivf[1]), 2),
            round(as.numeric(fitstat(tsls3, "ivf")$ivf[1]), 2),
            round(as.numeric(fitstat(tsls4, "ivf")$ivf[1]), 2),
            round(as.numeric(fitstat(tsls5, "ivf")$ivf[1]), 2)
            )



# bind together for output table
results <- rbind(twfe_iv_coef, twfe_iv_se, parish_fe, year_fe, twfe_iv_obs, firstF,
                 twfe_red_coef, twfe_red_se, parish_fe, year_fe, twfe_reduced_obs,
                 did_coef_red, did_se_red, n_parishes, did_reduced_obs, mean_outcome, nodes_dropped)

colnames(results) <- c(" ", "(1)", "(2)", "(3)", "(4)", "(5)")

# Create Output latex table
kbl(results, 
    booktabs = T, 
    caption = "TSLS: Railways and Local Development", 
    row.names = F,
    align = "lccccc",
    linesep = "",  # Suppress additional line spacing
    format = "latex") %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down") # Ensures [!h] placement
  ) %>%
  add_header_above(outcomes_header) %>%
  add_header_above(c(" " = 1, "Dependent variable:" = 5), escape = F) %>%
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

######################
# ==== Grundtvig === #
######################

# TWFE
mod1 = feols(
  Assembly_house ~ Connected_rail | GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)

mod2 = feols(
  HighSchool ~ Connected_rail | GIS_ID + Year,
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
agg_mod1 <- aggte(out1, type = "simple", na.rm = T)
agg_mod2 <- aggte(out2, type = "simple", na.rm = T)

# Summary
summary(agg_mod1)
summary(agg_mod2)


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
did_obs <- c("Obs.", agg_mod1$DIDparams$n, agg_mod2$DIDparams$n)

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
  group_rows("Panel A: TWFE", 1, 5) %>%
  group_rows("Panel B: Callaway and Sant'Anna", 6, 9) %>% 
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
  data = grundtvig_iv,
  cluster = ~ GIS_ID
)

tsls2 = feols(
  HighSchool ~ 1 
  | GIS_ID + Year 
  | RailAccess ~ LCPAccess,
  data = grundtvig_iv,
  cluster = ~ GIS_ID
)

# first stages
etable(tsls1$iv_first_stage[[1]], tsls2$iv_first_stage[[1]],
       tex = TRUE,
       title = "First-Stage Estimates (With Nodes)")


twfe1_red <- feols(
  Assembly_house ~ LCPAccess 
  | GIS_ID + Year,
  data = grundtvig_iv,
  cluster = ~ GIS_ID
)

twfe2_red <- feols(
  HighSchool ~ LCPAccess 
  | GIS_ID + Year,
  data = grundtvig_iv,
  cluster = ~ GIS_ID
)


out1 = att_gt(
  yname = "Assembly_house",
  tname = "Year_num",
  gname = "Treat_year_instr",
  idname = "GIS_ID_num",
  data = grundtvig_iv,
)

out2 = att_gt(
  yname = "HighSchool",
  tname = "Year_num",
  gname = "Treat_year_instr",
  idname = "GIS_ID_num",
  data = grundtvig_iv,
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

nodes_dropped <- c("Nodes dropped (< 10 km)", "No", "No")
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











