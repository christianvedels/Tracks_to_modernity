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


# ==== Load data ====
grundtvig <- read_csv2("Data/REGRESSION_DATA_Grundtvigianism.csv") %>%
  mutate(
    RailAccess = Connected_rail,
    RailDist = Distance_to_nearest_railway,
    LCPAccess = Connected_rail_instr,
    LCPDist = Distance_to_nearest_railway_instr
  )

# Create variable treat_year:
grundtvig <- grundtvig %>% 
  mutate(
    Year_num = as.numeric(as.character(Year)),
    GIS_ID_num = as.numeric(factor(GIS_ID))
  ) %>% 
  group_by(GIS_ID) %>% 
  mutate(
    # Identify the first year when Connected_rail is 1
    Treat_year = ifelse(any(Connected_rail == 1), min(Year[Connected_rail == 1]), 0)
  ) %>% 
  mutate(
    # Ensure Treat_year is consistent across groups
    Treat_year = ifelse(Treat_year > 0, Treat_year, 0)
  ) 

census <- read_csv2("Data/REGRESSION_DATA_Demography.csv") %>%
  mutate(
    Population = Pop,
    CWR = Child_women_ratio,
    Manufacturing = Manufacturing_789,
    HISCAM = hiscam_avg,
    Migration = Born_different_county,
    RailAccess = Connected_rail,
    RailDist = Distance_to_nearest_railway,
    LCPAccess = Connected_rail_instr,
    LCPDist = Distance_to_nearest_railway_instr) %>% 
  mutate(
    lnPopulation = log(Pop),
    lnManufacturing = log(Manufacturing_789 + 1),
    lnFarming = log(Farming + 1),
    lnCWR = log(CWR + 1),
    lnHISCAM = log(HISCAM),
    lnMigration = log(Migration + 1)
  )

# Create variable treat_year:
census <- census %>% 
  mutate(
    Year_num = as.numeric(as.character(Year)),
    GIS_ID_num = as.numeric(factor(GIS_ID))
  ) %>% 
  group_by(GIS_ID) %>% 
  mutate(
    # Identify the first year when Connected_rail is 1
    Treat_year = ifelse(any(Connected_rail == 1), min(Year[Connected_rail == 1]), 0)
  ) %>% 
  mutate(
    # Ensure Treat_year is consistent across groups
    Treat_year = ifelse(Treat_year > 0, Treat_year, 0)
  ) 

# Why duplicates?
#duplicated_rows <- census %>% filter(duplicated(.))
#nrow(duplicated_rows)

#duplicated_rows <- census %>% filter(duplicated(.))
#print(duplicated_rows)

# === Summary Statistics ===

# 1) Census data
stats_census <- describe(census[, c("Population", 
                           "Manufacturing", 
                           "CWR", 
                           "HISCAM", 
                           "Migration", 
                           "RailAccess",
                           "RailDist",
                           "LCPAccess",
                           "LCPDist")])


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
  group_rows("Panel B: Infrastructure", 6, 9) %>%
  footnote(general = "Here is a general comments of the table. ")


# 2) Grundtvig data
stats_grundtvig <- describe(grundtvig[, c("Assembly_house", 
                                    "HighSchool",
                                    "RailAccess",
                                    "RailDist",
                                    "LCPAccess",
                                    "LCPDist")])


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
  group_rows("Panel B: Infrastructure", 3, 6)  %>%
  footnote(general = "Here is a general comments of the table. ")


# ==== TWFE regressions (Demographics / Economy) ====
twfe1 = feols(
  lnPopulation ~ RailAccess | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

twfe2 = feols(
  lnCWR ~ RailAccess | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

twfe3 = feols(
  lnManufacturing ~ RailAccess | GIS_ID + Year,
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
  yname = "lnCWR",          # Outcome variable
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


# Create mean Outcome

# Dynamically calculate the means for the specified columns
mean_outcome <- c(
  "Mean of Outcome",
  sapply(outcomes_cs, function(col) sprintf("%.3f", mean(census[[col]], na.rm = TRUE)))
)

did_n_parishes <- c("Parishes", length(unique(agg_mod1$DIDparams$data$GIS_ID)), length(unique(agg_mod2$DIDparams$data$GIS_ID)), length(unique(agg_mod3$DIDparams$data$GIS_ID)), length(unique(agg_mod4$DIDparams$data$GIS_ID)), length(unique(agg_mod5$DIDparams$data$GIS_ID)))

# bind together for output table

results <- rbind(twfe_coef, twfe_se, parish_fe, year_fe, twfe_obs, did_coef, did_se, mean_outcome, did_n_parishes)
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


# ==== TWFE: Grundtvig ===
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
results <- rbind(twfe_coef, twfe_se, parish_fe, year_fe, twfe_obs, did_coef, did_se, mean_outcome, did_n_parishes)
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







