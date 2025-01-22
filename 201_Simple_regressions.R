# Simple regressions
#
# Date updated:   2025-01-21
# Author:         Christian Vedel, Tom Görges
# Purpose:        Runs regressions

# ==== Libraries ====
library(tidyverse)
library(fixest)
library(did)
source("Data_cleaning_scripts/000_Functions.R")

# clear workspace
rm(list = ls())


# ==== Load data ====
grundtvig = read_csv2("Data/REGRESSION_DATA_Grundtvigianism.csv")

census = read_csv2("Data/REGRESSION_DATA_Demography.csv") %>%
  mutate(
    Population = Pop,
    CWR = Child_women_ratio,
    Manufacturing = Manufacturing_789,
    HISCAM = hiscam_avg,
    Migration = Born_different_county,
    Railway = Connected_rail)


# some preparation before implementation of staggered DiD
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
  ) %>% 
  mutate(
    lnPopulation = log(Pop),
    lnManufacturing = log(Manufacturing_789 + 1),
    lnFarming = log(Farming + 1),
    lnCWR = log(CWR + 1),
    lnHISCAM = log(HISCAM),
    lnMigration = log(Migration + 1)
  )

# SUMMARY STATISTICS
library(psych) # for summary stats

# SUMMARY STATS
stats <- describe(census[, c("Population", 
                           "Manufacturing", 
                           "CWR", 
                           "HISCAM", 
                           "Migration", 
                           "Railway")])


# Select only relevant columns
stats_selected <- stats[, c("n", "mean", "sd", "min", "max")]

stats_selected <- stats_selected %>%
  mutate(across(c(mean, sd, min, max), ~ round(.x, 3)))


# Create summary statistics latex table
library(kableExtra)

kbl(stats_selected, booktabs = T, caption = "Caption", row.names = T, format = "latex")


# ==== TWFE regressions (Demographics / Economy) ====
twfe1 = feols(
  lnPopulation ~ Railway | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

twfe2 = feols(
  lnCWR ~ Railway | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

twfe3 = feols(
  lnManufacturing ~ Railway | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

twfe4 = feols(
  HISCAM ~ Railway | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

twfe5 = feols(
  lnMigration ~ Railway | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

# Generate output table
etable(twfe1, twfe2, twfe3, twfe4, twfe5,
       fitstat = ~ ar2 + n,  # Include R-squared and number of observations
       cluster = "GIS_ID", # Display the clustering
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1)) # Custom significance codes


############################################################
### === Callaway and St Anna: Demographics / Economy === ###
############################################################

# Why duplicates?
#duplicated_rows <- census0 %>% filter(duplicated(.))
#nrow(duplicated_rows)

#duplicated_rows <- census0 %>% filter(duplicated(.))
#print(duplicated_rows)

######################################################################################
# ==== Need treat_year variable (rail opened) for each GIS_ID for staggered DiD: === #
######################################################################################



###########################################################################################


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




###################################################################################################

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


# Output table


# Initialize an empty dataframe
results <- data.frame("Dependent variable:", as.character(formula(twfe1)[2]), as.character(formula(twfe2)[2]), as.character(formula(twfe3)[2]), as.character(formula(twfe4)[2]), as.character(formula(twfe5)[2]))

# Built table row by row
Panel_A <- c("Panel A. TWFE", "(1)", "(2)", "(3)", "(4)", "(5)")

twfe_coef <- c("Railway", sprintf("%.3f", twfe1$coefficients), sprintf("%.3f", twfe2$coefficients), sprintf("%.3f", twfe3$coefficients), sprintf("%.3f", twfe4$coefficients), sprintf("%.3f", twfe5$coefficients))

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

twfe_se <- c("", sprintf("(%.3f)", twfe1$se), sprintf("(%.3f)",twfe2$se), sprintf("(%.3f)",twfe3$se), sprintf("(%.3f)",twfe4$se), sprintf("(%.3f)",twfe5$se))

parish_fe <- c("Parish FE", "Yes", "Yes", "Yes", "Yes", "Yes")
year_fe <- c("Year FE", "Yes", "Yes", "Yes", "Yes", "Yes")

twfe_obs <- c("Obs.", twfe1$nobs, twfe2$nobs, twfe3$nobs, twfe4$nobs, twfe5$nobs)



Panel_B <- c("Panel B. SDiD", "(1)", "(2)", "(3)", "(4)", "(5)")
did_ynames <- c("Dependent variable:", agg_mod1$DIDparams$yname, agg_mod2$DIDparams$yname, agg_mod3$DIDparams$yname, agg_mod4$DIDparams$yname, agg_mod5$DIDparams$yname)
did_coef <- c("Railway", sprintf("%.3f",agg_mod1$overall.att), sprintf("%.3f",agg_mod2$overall.att), sprintf("%.3f",agg_mod3$overall.att), sprintf("%.3f", agg_mod4$overall.att), sprintf("%.3f",agg_mod5$overall.att))

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
# Subset the column names from did_ynames (elements 2 to 6)
columns_to_use <- did_ynames[2:6]

# Dynamically calculate the means for the specified columns
mean_outcome <- c(
  "Mean of Outcome",
  sapply(columns_to_use, function(col) sprintf("%.3f", mean(census[[col]], na.rm = TRUE)))
)

did_n_parishes <- c("Parishes", length(unique(agg_mod1$DIDparams$data$GIS_ID)), length(unique(agg_mod2$DIDparams$data$GIS_ID)), length(unique(agg_mod3$DIDparams$data$GIS_ID)), length(unique(agg_mod4$DIDparams$data$GIS_ID)), length(unique(agg_mod5$DIDparams$data$GIS_ID)))

# bind together for output table

results <- rbind(results, Panel_A, twfe_coef, twfe_se, parish_fe, year_fe, twfe_obs,
                 did_ynames, Panel_B, did_coef, did_se, mean_outcome, did_n_parishes)

# Use the first row as the header
colnames(results) <- results[1, ] # Set the first row as column names

# Identify rows that are identical to the header, should be the one with the did outcomes if models align
matching_rows <- apply(results, 1, function(row) all(as.character(row) == colnames(results)))

# Remove all matching rows
results <- results[!matching_rows, ]

print(results)









# Create output table
kbl(results, booktabs = T, caption = "Caption", row.names = F, format = "latex")

###################################################################################################



# ==== TWFE regressions (Grundtvigianism) ====
mod1 = feols(
  Assembly_house ~ Connected_rail | GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)


mod2 = feols(
  log(MA_assembly) ~ Connected_rail | GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)

mod3 = feols(
  HighSchool ~ Connected_rail | GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)

mod4 = feols(
  log(MA_folkhigh) ~ Connected_rail | GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)


etable(mod1, mod2, mod3, mod4) %>% 
  knitr::kable()



# ==== Doubly Robust DID: Gr. ====

set.seed(20)
grundtvig0 = grundtvig %>% 
  mutate(
    Year_num = as.numeric(as.character(Year)),
    GIS_ID_num = as.numeric(factor(GIS_ID))
  ) %>% 
  group_by(GIS_ID) %>% 
  mutate( # Treat year
    Treat_year = ifelse(first_occurence(Connected_rail)==1, Year, 0),
    Treat_year_instr = ifelse(first_occurence(Connected_rail_instr)==1, Year, 0)
  ) %>% 
  mutate( # Treat year
    Treat_year = ifelse(any(Year!=0), max(Treat_year), 0),
    Treat_year_instr = ifelse(any(Year!=0), max(Treat_year), 0)
  )

# No covariates
out1 = att_gt(
  yname = "Assembly_house",
  tname = "Year_num",
  gname = "Treat_year",
  idname = "GIS_ID_num",
  data = grundtvig0,
)



out2 = att_gt(
  yname = "HighSchool",
  tname = "Year_num",
  gname = "Treat_year",
  idname = "GIS_ID_num",
  data = grundtvig0,
)



# Event studies
es1 = aggte(out1, "dynamic", na.rm = TRUE)
p1_did = ggdid(es1)

p1_did = p1_did + 
  labs(title = "Outcome: Assembly house") + 
  xlim(c(-20, 35)) + 
  ylim(c(-0.135, 0.2)) + 
  geom_vline(xintercept = -0.5) + 
  geom_hline(yintercept = 0)

p1_did
#ggsave("Plots/DID_assembly.png", plot = p1_did, width = 6, height = 4)

es2 = aggte(out2, "dynamic", na.rm = TRUE)
p2_did = ggdid(es2)

p2_did = p2_did + 
  labs(title = "Outcome: High School") + 
  xlim(c(-20, 35)) + 
  ylim(c(-0.025, 0.05)) +
  geom_vline(xintercept = -0.5) + 
  geom_hline(yintercept = 0)

p2_did
#ggsave("Plots/DID_HighSchool.png", plot = p2_did, width = 6, height = 4)





