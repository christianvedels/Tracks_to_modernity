# Regressions
#
# Date updated:   2025-09-11
# Author:         Tom Görges
# Purpose:        Runs regressions

rm(list = ls())


# ==== Libraries ====
library(tidyverse)
library(fixest)
library(did)
library(kableExtra) # for latex tables
source("Data_cleaning_scripts/000_Functions.R")

# ==== Params ====
CONTROLS = "Dist_hamb_year + Dist_cph_year + Dist_ox_year + Pop1801_year + county_by_year" # These are decile by year FE
NSIGNIF = 4 # Significant digits in all tables

# ==== Load data ====
census = read_csv2("Data/REGRESSION_DATA_Demography.csv", guess_max = 100000)
grundtvig = read_csv2("Data/REGRESSION_DATA_Grundtvigianism.csv", guess_max = 100000)

# ==== Renaming =====
census = census %>% rename(
  Connected_railway = RailAccess,
  Connected_lcp = LCPAccess
)

### TWFE CENSUS No controls ===============================================================

mod1 <- feols(
  lnPopulation ~ Connected_railway  |
    GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod2 <- feols(
  lnChild_women_ratio ~ Connected_railway  |
    GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod3 <- feols(
  lnManufacturing ~ Connected_railway  |
    GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod4 <- feols(
  lnNotAgriculture ~ Connected_railway  |
    GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod5 <- feols(
  HISCAM_avg ~ Connected_railway  |
    GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod6 <- feols(
  lnMigration ~ Connected_railway  |
    GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)


etable(mod1, mod2, mod3, mod4, mod5, mod6,
       fitstat = ~ n + my)


### CENSUS Callaway and ST Anna

cs_mod1 = att_gt(
  yname = "lnPopulation",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year",      
  xformla = ~1, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple1 = aggte(cs_mod1, type = "simple")


#########################################
cs_mod2 = att_gt(
  yname = "lnChild_women_ratio",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year",      
  xformla = ~1, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple2 = aggte(cs_mod2, type = "simple")

#########################################
cs_mod3 = att_gt(
  yname = "lnManufacturing",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year",      
  xformla = ~1, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple3 = aggte(cs_mod3, type = "simple")



#########################################
cs_mod4 = att_gt(
  yname = "lnNotAgriculture",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year",      
  xformla = ~1, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple4 = aggte(cs_mod4, type = "simple")


#########################################
cs_mod5 = att_gt(
  yname = "HISCAM_avg",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year",      
  xformla = ~1, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple5 = aggte(cs_mod5, type = "simple")


#########################################
cs_mod6 = att_gt(
  yname = "lnMigration",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year",      
  xformla = ~1, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple6 = aggte(cs_mod6, type = "simple")


###################

summary(agg_simple1)
summary(agg_simple2)
summary(agg_simple3)
summary(agg_simple4)
summary(agg_simple5)
summary(agg_simple6)

# Dynamic effects
dyn1 = aggte(cs_mod1, type = "dynamic")
dyn2 = aggte(cs_mod2, type = "dynamic")
dyn3 = aggte(cs_mod3, type = "dynamic")
dyn4 = aggte(cs_mod4, type = "dynamic")
dyn5 = aggte(cs_mod5, type = "dynamic")
dyn6 = aggte(cs_mod6, type = "dynamic")

# Plots
p1 = ggdid(dyn1) + ggtitle("lnPopulation")
p2 = ggdid(dyn2) + ggtitle("lnChild_women_ratio")
p3 = ggdid(dyn3) + ggtitle("lnManufacturing")
p4 = ggdid(dyn4) + ggtitle("lnNotAgriculture")
p5 = ggdid(dyn5) + ggtitle("HISCAM_avg")
p6 = ggdid(dyn6) + ggtitle("lnMigration")

# Display one by one
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)

# Or, if you want them combined in a grid:
library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)


#########################
# === WITH controls === #
#########################

mod1 <- feols(
  lnPopulation ~ Connected_railway + Dist_hamb_year + Dist_cph_year  + Pop1801_year + county_by_year  |
    GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod2 <- feols(
  lnChild_women_ratio ~ Connected_railway + Dist_hamb_year + Dist_cph_year  + Pop1801_year + county_by_year   |
    GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod3 <- feols(
  lnManufacturing ~ Connected_railway + Dist_hamb_year + Dist_cph_year  + Pop1801_year + county_by_year   |
    GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod4 <- feols(
  lnNotAgriculture ~ Connected_railway + Dist_hamb_year + Dist_cph_year  + Pop1801_year + county_by_year  |
    GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod5 <- feols(
  HISCAM_avg ~ Connected_railway + Dist_hamb_year + Dist_cph_year  + Pop1801_year + county_by_year   |
    GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod6 <- feols(
  lnMigration ~ Connected_railway + Dist_hamb_year + Dist_cph_year  + Pop1801_year + county_by_year  |
    GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)


etable(mod1, mod2, mod3, mod4, mod5, mod6,
       fitstat = ~ n + my)


### CENSUS Callaway and ST Anna with controls
cs_mod1 = att_gt(
  yname = "lnPopulation",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year",      
  xformla = ~ Dist_hamb_year + Dist_cph_year  + Pop1801_year + county_by_year, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple1 = aggte(cs_mod1, type = "simple", na.rm = TRUE)


#########################################
cs_mod2 = att_gt(
  yname = "lnChild_women_ratio",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year",      
  xformla = ~ Dist_hamb_year + Dist_cph_year + Pop1801_year + county_by_year, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple2 = aggte(cs_mod2, type = "simple", na.rm = TRUE)

#########################################
cs_mod3 = att_gt(
  yname = "lnManufacturing",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year",      
  xformla = ~ Dist_hamb_year + Dist_cph_year + Pop1801_year + county_by_year, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple3 = aggte(cs_mod3, type = "simple", na.rm = TRUE)



#########################################
cs_mod4 = att_gt(
  yname = "lnNotAgriculture",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year",      
  xformla = ~ Dist_hamb_year + Dist_cph_year  + Pop1801_year + county_by_year, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple4 = aggte(cs_mod4, type = "simple", na.rm = TRUE)


#########################################
cs_mod5 = att_gt(
  yname = "HISCAM_avg",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year",      
  xformla = ~ Dist_hamb_year + Dist_cph_year  + Pop1801_year + county_by_year, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple5 = aggte(cs_mod5, type = "simple", na.rm = TRUE)


#########################################
cs_mod6 = att_gt(
  yname = "lnMigration",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year",      
  xformla = ~ Dist_hamb_year + Dist_cph_year + Pop1801_year + county_by_year, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple6 = aggte(cs_mod6, type = "simple", na.rm = TRUE)


###################

summary(agg_simple1)
summary(agg_simple2)
summary(agg_simple3)
summary(agg_simple4)
summary(agg_simple5)
summary(agg_simple6)

######################################################################
######################################################################
######################################################################


# No controls
base = att_gt(
  yname = "lnPopulation",
  tname = "Year_num",
  idname = "GIS_ID_num",
  gname = "Treat_year",
  xformla = ~ 1,
  data = census,
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Add each control separately
c1 = update(base, xformla = ~ Dist_hamb_year)
c2 = update(base, xformla = ~ Dist_cph_year)
c3 = update(base, xformla = ~ Dist_ox_year)
c4 = update(base, xformla = ~ Pop1801_year)
c5 = update(base, xformla = ~ county_by_year)

# Aggregate and summarise
lapply(list(c1, c2, c3, c4, c5), function(m) summary(aggte(m, type="simple", na.rm=TRUE)))


###################################################################
###################################################################
###################################################################




##########################
# === IV regressions === #
##########################

census_iv <- census %>%
  filter(away_from_node == 1)

# estimate the models
mod1_iv <- feols(
  lnPopulation ~ 1 | 
    GIS_ID + Year | 
    Connected_railway ~ Connected_lcp,
  data = census_iv,
  cluster = ~GIS_ID
)

# estimate the models
mod2_iv <- feols(
  lnChild_women_ratio ~ 1 | 
    GIS_ID + Year | 
    Connected_railway ~ Connected_lcp,
  data = census_iv,
  cluster = ~GIS_ID
)

# estimate the models
mod3_iv <- feols(
  lnManufacturing ~ 1 | 
    GIS_ID + Year | 
    Connected_railway ~ Connected_lcp,
  data = census_iv,
  cluster = ~GIS_ID
)

# estimate the models
mod4_iv <- feols(
  lnNotAgriculture ~ 1 | 
    GIS_ID + Year | 
    Connected_railway ~ Connected_lcp,
  data = census_iv,
  cluster = ~GIS_ID
)

# estimate the models
mod5_iv <- feols(
  HISCAM_avg ~ 1 | 
    GIS_ID + Year | 
    Connected_railway ~ Connected_lcp,
  data = census_iv,
  cluster = ~GIS_ID
)

# estimate the models
mod6_iv <- feols(
  lnMigration ~ 1 | 
    GIS_ID + Year | 
    Connected_railway ~ Connected_lcp,
  data = census_iv,
  cluster = ~GIS_ID
)



################################
# === Second Stage Results === #
################################
etable(mod1_iv, mod2_iv, mod3_iv, mod4_iv, mod5_iv, mod6_iv,
       stage = 2,
       fitstat = ~n + ivf)



# IV Callaway and ST Anna #########################################################################


cs_mod1 = att_gt(
  yname = "lnPopulation",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year_instr",      
  xformla = ~1, 
  data = census_iv,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple1 = aggte(cs_mod1, type = "simple")


#########################################
cs_mod2 = att_gt(
  yname = "lnChild_women_ratio",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year_instr",      
  xformla = ~1, 
  data = census_iv,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple2 = aggte(cs_mod2, type = "simple")

#########################################
cs_mod3 = att_gt(
  yname = "lnManufacturing",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year_instr",      
  xformla = ~1, 
  data = census_iv,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple3 = aggte(cs_mod3, type = "simple")



#########################################
cs_mod4 = att_gt(
  yname = "lnNotAgriculture",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year_instr",      
  xformla = ~1, 
  data = census_iv,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple4 = aggte(cs_mod4, type = "simple")


#########################################
cs_mod5 = att_gt(
  yname = "HISCAM_avg",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year_instr",      
  xformla = ~1, 
  data = census_iv,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple5 = aggte(cs_mod5, type = "simple")


#########################################
cs_mod6 = att_gt(
  yname = "lnMigration",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year_instr",      
  xformla = ~1, 
  data = census_iv,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple6 = aggte(cs_mod6, type = "simple")


###################

summary(agg_simple1)
summary(agg_simple2)
summary(agg_simple3)
summary(agg_simple4)
summary(agg_simple5)
summary(agg_simple6)




cor(census$Treat_year, census$Treat_year_instr)



