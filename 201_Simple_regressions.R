# Simple regressions
#
# Date updated:   2023-10-02
# Auhtor:         Christian Vedel 
# Purpose:        Runs a few simple regressions

# ==== Libraries ====
library(tidyverse)
library(fixest)
library(did)
source("Data_cleaning_scripts/000_Functions.R")

# ==== Load data ====
grundtvig = read_csv2("Data/REGRESSION_DATA_Grundtvigianism.csv")
census = read_csv2("Data/REGRESSION_DATA_Demography.csv")



# ==== TWFE regressions (Demographics / Economy) ====
mod1 = feols(
  log(Pop) ~ Connected_rail | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod2 = feols(
  log(Child_women_ratio) ~ Connected_rail | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod3 = feols(
  log(Manufacturing_789) ~ Connected_rail | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod4 = feols(
  log(hiscam_avg) ~ Connected_rail | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod5 = feols(
  log(Born_different_county) ~ Connected_rail | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

etable(mod1, mod2, mod3, mod4, mod5) %>% 
  knitr::kable()

etable(mod1, mod2, mod3, mod4, mod5)

############################################################
### === Callaway and St Anna: Demographics / Economy === ###
############################################################

# Why duplicates?
duplicated_rows <- census0 %>% filter(duplicated(.))
nrow(duplicated_rows) # Number of duplicate rows

duplicated_rows <- census0 %>% filter(duplicated(.))
print(duplicated_rows)

######################################################################################
# ==== Need treat_year variable (rail opened) for each GIS_ID for staggered DiD: === #
######################################################################################

rail <- read.csv2("./Data/Panel_of_railways_in_parishes.csv")

treat_year <- rail %>%
  group_by(GIS_ID) %>% 
  summarise(Treat_year = ifelse(any(Connected_rail == 1), min(Year[Connected_rail == 1]), 0))


census <- left_join(census, treat_year, by = "GIS_ID")
##############################


census0 <- census %>%
  mutate(
    Year_num = as.numeric(as.character(Year)),
    GIS_ID_num = as.numeric(factor(GIS_ID))
  ) %>%
  mutate(
    # Transform other variables for further analysis
    lPop = log(Pop),
    lManu = log(Manufacturing_789 + 1),
    lAgri = log(hisco_major6 + 1),
    lCWratio = log(Child_women_ratio + 1),
    lHISCAM_avg = log(hiscam_avg),
    lBorn_different_county = log(Born_different_county + 1)
  )

# log(Pop)
cs_mod1 <- att_gt(
  yname = "lPop",          # Outcome variable
  tname = "Year_num",             # Time variable
  idname = "GIS_ID_num",          # Unit identifier
  gname = "Treat_year",       # First year of treatment
  xformla = ~1,               # No covariates (consistent with TWFE)
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
  gname = "Treat_year",       # First year of treatment
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

# Pop
agg.es1 <- aggte(cs_mod1, type = "group") # "The Overall ATT averages the group-specific treatment effects 
                                          #  across groups. In our view, this parameter is a leading choice as an overall
                                          #  summary effect of participating in the treatment. It is the average effect of
                                          #  participating in the treatment that was experienced across all units that 
                                          #  participate in the treatment in any period. In this sense, it has a similar 
                                          #  interpretation to the ATT in the textbook case where there are exactly two periods
                                          #  and two groups."

summary(agg.es1)
ggdid(agg.es1)

# child-woman ratio
agg.es2 <- aggte(cs_mod2, type = "group")
summary(agg.es2)
ggdid(agg.es2)

# Manufacturing
agg.es3 <- aggte(cs_mod3, type = "group")
summary(agg.es3)
ggdid(agg.es3)

# HISCAM
agg.es4 <- aggte(cs_mod4, type = "group")
summary(agg.es4)
ggdid(agg.es4)

# Migration
agg.es5 <- aggte(cs_mod5, type = "group")
summary(agg.es5)
ggdid(agg.es5)
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





