# Regressions
#
# Date updated:   2025-09-16
# Author:         Tom Görges
# Purpose:        Runs Grundtvig regressions

rm(list = ls())


# ==== Libraries ====
library(tidyverse)
library(fixest)
library(did)
library(kableExtra) # for latex tables
source("Data_cleaning_scripts/000_Functions.R")

# ==== Load data ====
grundtvig = read_csv2("Data/REGRESSION_DATA_Grundtvigianism.csv", guess_max = 100000)

# ==== Renaming =====
grundtvig = grundtvig %>% rename(
  Connected_railway = RailAccess,
  Connected_lcp = LCPAccess
)

# Only same GIS_IDs
#census = census %>% filter(GIS_ID %in% grundtvig$GIS_ID)
#grundtvig = grundtvig %>% filter(GIS_ID %in% census$GIS_ID)

# Zeros are NAs in MA
grundtvig = grundtvig %>% mutate(
  MA_assembly = case_when(
    is.na(MA_assembly) ~ 0,
    TRUE ~ MA_assembly
  ),
  MA_folkhigh = case_when(
    is.na(MA_folkhigh) ~ 0,
    TRUE ~ MA_folkhigh
  )
)


#########################################
# === TWFE Regressions, No controls === #
#########################################

dep_vars <- c("Assembly_house", "HighSchool", "MA_assembly", "MA_folkhigh")

twfe_models <- lapply(dep_vars, \(y) feols(
  as.formula(paste0(y, " ~ Connected_railway | GIS_ID + Year")),
  data = census, cluster = ~ GIS_ID
))

# Have a look at results
etable(twfe_models, fitstat = ~ n + my)


# ---------------------------------------------------
# ---------------------------------------------------
# ---------------------------------------------------


# ==== TWFE regressions (Grundtvig data) ====
mod1 <- feols(
  Assembly_house ~ Connected_railway  |
    GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)

mod2 <- feols(
  HighSchool ~ Connected_railway  |
    GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)

mod3 <- feols(
  MA_assembly ~ Connected_railway  |
    GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)

mod4 <- feols(
  MA_folkhigh ~ Connected_railway  |
    GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)


etable(mod1, mod2, mod3, mod4,
       fitstat = ~ n + my)


# ==== CS estimates (Grundtvig data) ====
cs_mod1 = att_gt(
  yname = "Assembly_house",    
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

#################################################################

cs_mod2 = att_gt(
  yname = "HighSchool",    
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

#################################################################

cs_mod3 = att_gt(
  yname = "MA_assembly",    
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

#################################################################

cs_mod4 = att_gt(
  yname = "MA_folkhigh",    
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


summary(agg_simple1)
summary(agg_simple2)
summary(agg_simple3)
summary(agg_simple4)


# ==== WITH Controls: TWFE regressions (Grundtvig data) ====

mod1 <- feols(
  Assembly_house ~ Connected_railway + Dist_hamb_year + Dist_cph_year  + Pop1801_year + county_by_year  |
    GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)

mod2 <- feols(
  HighSchool ~ Connected_railway + Dist_hamb_year + Dist_cph_year  + Pop1801_year + county_by_year  |
    GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)

mod3 <- feols(
  MA_assembly ~ Connected_railway + Dist_hamb_year + Dist_cph_year  + Pop1801_year + county_by_year  |
    GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)

mod4 <- feols(
  MA_folkhigh ~ Connected_railway + Dist_hamb_year + Dist_cph_year  + Pop1801_year + county_by_year  |
    GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)


etable(mod1, mod2, mod3, mod4,
       fitstat = ~ n + my,
       keep = "Connected_railway")


# ==== WITH Controls: CS estimates (Grundtvig data) ====

cs_mod1 = att_gt(
  yname = "Assembly_house",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year",      
  xformla = ~ Dist_hamb_year + Dist_cph_year + Pop1801_year + county_by_year, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple1 = aggte(cs_mod1, type = "simple")

#################################################################

cs_mod2 = att_gt(
  yname = "HighSchool",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year",      
  xformla = ~ Dist_hamb_year + Dist_cph_year + Pop1801_year + county_by_year, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple2 = aggte(cs_mod2, type = "simple")

#################################################################

cs_mod3 = att_gt(
  yname = "MA_assembly",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year",      
  xformla = ~ Dist_hamb_year + Dist_cph_year + Pop1801_year + county_by_year, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple3 = aggte(cs_mod3, type = "simple")

#################################################################

cs_mod4 = att_gt(
  yname = "MA_folkhigh",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year",      
  xformla = ~Dist_hamb_year + Dist_cph_year + Pop1801_year + county_by_year, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple4 = aggte(cs_mod4, type = "simple")


summary(agg_simple1)
summary(agg_simple2)
summary(agg_simple3)
summary(agg_simple4)

##########################
# === IV regressions === #
##########################

grundtvig_iv <- grundtvig %>%
  filter(away_from_node == 1)

# estimate the models
mod1_iv <- feols(
  Assembly_house ~ 1 | 
    GIS_ID + Year | 
    Connected_railway ~ Connected_lcp,
  data = grundtvig_iv,
  cluster = ~GIS_ID
)

mod2_iv <- feols(
  HighSchool ~ 1 | 
    GIS_ID + Year | 
    Connected_railway ~ Connected_lcp,
  data = grundtvig_iv,
  cluster = ~GIS_ID
)

mod3_iv <- feols(
  MA_assembly ~ 1 | 
    GIS_ID + Year | 
    Connected_railway ~ Connected_lcp,
  data = grundtvig_iv,
  cluster = ~GIS_ID
)

mod4_iv <- feols(
  MA_folkhigh ~ 1 | 
    GIS_ID + Year | 
    Connected_railway ~ Connected_lcp,
  data = grundtvig_iv,
  cluster = ~GIS_ID
)

################################
# === Second Stage Results === #
################################
etable(mod1_iv, mod2_iv, mod3_iv, mod4_iv,
       stage = 2,
       fitstat = ~n + ivf)


# Instrument: Callaway and St Anna

cs_mod1 = att_gt(
  yname = "Assembly_house",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year_instr",      
  xformla = ~1, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple1 = aggte(cs_mod1, type = "simple")

#################################################################

cs_mod2 = att_gt(
  yname = "HighSchool",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year_instr",      
  xformla = ~1, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple2 = aggte(cs_mod2, type = "simple")

#################################################################

cs_mod3 = att_gt(
  yname = "MA_assembly",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year_instr",      
  xformla = ~1, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple3 = aggte(cs_mod3, type = "simple")

#################################################################

cs_mod4 = att_gt(
  yname = "MA_folkhigh",    
  tname = "Year_num",        
  idname = "GIS_ID_num",     
  gname = "Treat_year_instr",      
  xformla = ~1, 
  data = census,        
  clustervars = "GIS_ID",
  control_group = "notyettreated"
)

# Aggregate into one overall ATT
agg_simple4 = aggte(cs_mod4, type = "simple")


summary(agg_simple1)
summary(agg_simple2)
summary(agg_simple3)
summary(agg_simple4)








