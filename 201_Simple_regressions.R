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
census = read_csv2("Data/REGRESSION_DATA_Demography.csv")


# ==== TWFE regressions (Demographics / Economy) ====
twfe1 = feols(
  log(Pop) ~ Connected_rail | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

twfe2 = feols(
  log(Child_women_ratio) ~ Connected_rail | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

twfe3 = feols(
  log(Manufacturing_789) ~ Connected_rail | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

twfe4 = feols(
  log(hiscam_avg) ~ Connected_rail | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

twfe5 = feols(
  log(Born_different_county) ~ Connected_rail | GIS_ID + Year,
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

# change treat year to first occurence in observed year

# actually treated in 1878 but treat as treated in 1880 because this is where we observe it

##############################

# some preparation before implementation of staggered DiD
census0 <- census %>% 
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
    lPop = log(Pop),
    lManu = log(Manufacturing_789 + 1),
    lAgri = log(hisco_major6 + 1),
    lCWratio = log(Child_women_ratio + 1),
    lHISCAM_avg = log(hiscam_avg),
    lBorn_different_county = log(Born_different_county + 1)
  )


###########################################################################################


# log(Pop)
cs_mod1 <- att_gt(
  yname = "lPop",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First year of treatment
  xformla = ~1,               # No covariates 
  data = census0,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# Child-woman ratio
cs_mod2 <- att_gt(
  yname = "lCWratio",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census0,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# log Manufacturing
cs_mod3 <- att_gt(
  yname = "lManu",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census0,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# HISCAM
cs_mod4 <- att_gt(
  yname = "lHISCAM_avg",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census0,              # Your dataset
  clustervars = "GIS_ID"      # Cluster variable
)

# Migration
cs_mod5 <- att_gt(
  yname = "lBorn_different_county",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First (observed) year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
  data = census0,              # Your dataset
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
# Make nice output table

# Extract results
results <- data.frame(
  Model = c("TWFE_1", "TWFE_2", "TWFE_3", "TWFE_4", "TWFE_5", 
            "CS_1", "CS_2", "CS_3", "CS_4", "CS_5"),
  Outcome = c(as.character(formula(twfe1)[2]), as.character(formula(twfe2)[2]), as.character(formula(twfe3)[2]), as.character(formula(twfe4)[2]), as.character(formula(twfe5)[2]),
    agg_mod1$DIDparams$yname, agg_mod2$DIDparams$yname, agg_mod3$DIDparams$yname, agg_mod4$DIDparams$yname, agg_mod5$DIDparams$yname),
  coefficient = c(twfe1$coefficients, twfe2$coefficients, twfe3$coefficients, twfe4$coefficients, twfe5$coefficients,
                  agg_mod1$overall.att, agg_mod2$overall.att, agg_mod3$overall.att, agg_mod4$overall.att, agg_mod5$overall.att
                  ),
  se = c(twfe1$se, twfe2$se, twfe3$se, twfe4$se, twfe5$se, 
         agg_mod1$overall.se, agg_mod2$overall.se, agg_mod3$overall.se, agg_mod4$overall.se, agg_mod5$overall.se
         )
)

# Calculate the t-statistic after creating the data frame
results$t <- results$coefficient / results$se

results$p <- pnorm(abs(results$t), lower.tail = FALSE)*2

results$significance <- ifelse(
  results$p < 0.01, "***",
  ifelse(results$p < 0.05, "**",
         ifelse(results$p < 0.1, "*", "")
  )
)



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





