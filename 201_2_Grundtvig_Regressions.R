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
census = read_csv2("Data/REGRESSION_DATA_Demography.csv", guess_max = 100000)


# ==== Renaming =====
grundtvig = grundtvig %>% rename(
  Connected_railway = RailAccess,
  Connected_lcp = LCPAccess
)

# Only same GIS_IDs
common_ids <- intersect(census$GIS_ID, grundtvig$GIS_ID)

census    <- census %>% filter(GIS_ID %in% common_ids)
grundtvig <- grundtvig %>% filter(GIS_ID %in% common_ids)


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

# Dependent variables
dep_vars <- c("Assembly_house", "HighSchool", "MA_assembly", "MA_folkhigh")

#########################################
# === TWFE Regressions, No controls === #
#########################################

twfe_models <- lapply(dep_vars, \(y) feols(
  as.formula(paste0(y, " ~ Connected_railway | GIS_ID + Year")),
  data = grundtvig, cluster = ~ GIS_ID
))

# Have a look at results
etable(twfe_models, fitstat = ~ n + my)

###########################################################
# === Callaway and Sant’Anna Regressions, no controls === #
###########################################################

# Estimate all models
cs_models <- lapply(dep_vars, \(y) att_gt(
  yname   = y,    
  tname   = "Year_num",        
  idname  = "GIS_ID_num",     
  gname   = "Treat_year",      
  xformla = ~1, 
  data    = grundtvig,        
  clustervars   = "GIS_ID",
  control_group = "notyettreated"
))

# Aggregate into overall ATTs
cs_aggs <- lapply(cs_models, \(m) aggte(m, type = "simple"))

# Name the lists for easy reference
names(cs_models) <- dep_vars
names(cs_aggs)   <- dep_vars


# Print all summaries one by one
for (nm in names(cs_aggs)) {
  cat("\n=== ", nm, " ===\n")
  print(summary(cs_aggs[[nm]]))
}

################################
# === Prepare Output Table === #
################################

# Extract results from Callaway and St Anna
cs_results <- extract_res(cs_aggs, grouped = FALSE)

# Prepare TWFE results
twfe_tidy   <- lapply(twfe_models, tidy)
twfe_glance <- lapply(twfe_models, glance)

# store mean of outcome TWFE
my_twfe <- sapply(twfe_models, function(m) unname(fitstat(m, "my")))

# create output table
table_vals <- data.frame(
  outcome   = c("Assembly house", "Folk high school", "Density Assembly houses (MA)",
                "Density Folk High Schools (MA)"),
  twfe_coef = mapply(starify, sapply(twfe_tidy, \(x) x$estimate[1]),
                     sapply(twfe_tidy, \(x) x$p.value[1])),
  twfe_se   = sapply(twfe_tidy, \(x) sprintf("(%.4f)", x$std.error[1])),
  cs_coef   = mapply(starify, cs_results$Estimate, cs_results$p),
  cs_se     = sprintf("(%.4f)", cs_results$SE),
  obs_twfe  = sapply(twfe_glance, \(x) x$nobs),
  my_twfe   = sprintf("%.4f", my_twfe),                # TWFE means
  my_cs     = sprintf("%.4f", cs_results$mean_outcome),# CS means
  obs_cs    = cs_results$n
)

# create and store latex table
sink("../../Apps/Overleaf/Tracks to Modernity/Tables/railways_and_grundtvig.tex")

cat("\\begin{tabular}{lcccc}\n")
cat("  \\toprule\n")
cat("  Outcome: & Assembly house & Folk high school & Density Assembly houses (MA) & Density Folk High Schools (MA) \\\\\n")
cat("           & (1) & (2) & (3) & (4)  \\\\\n")
cat("  \\midrule\n")
cat("  \\multicolumn{7}{l}{\\textbf{A. TWFE estimates}}\\\\\n")
cat("  Connected railway & ",
    paste(table_vals$twfe_coef, collapse=" & "),
    " \\\\\n")
cat("                    & ",
    paste(table_vals$twfe_se, collapse=" & "),
    " \\\\\n")
cat("  \\cmidrule(lr){2-5}\n")
cat("  Observations      & ",
    paste(table_vals$obs_twfe, collapse=" & "),
    " \\\\\n")
cat("  Mean of outcome   & ",
    paste(table_vals$my_twfe, collapse=" & "),
    " \\\\\n")   # <-- TWFE means here
cat("  \\midrule\n")
cat("  \\multicolumn{7}{l}{\\textbf{B. Callaway and Sant'Anna estimates}}\\\\\n")
cat("  Connected railway & ",
    paste(table_vals$cs_coef, collapse=" & "),
    " \\\\\n")
cat("                    & ",
    paste(table_vals$cs_se, collapse=" & "),
    " \\\\\n")
cat("  \\cmidrule(lr){2-5}\n")
cat("  Observations      & ",
    paste(table_vals$obs_cs, collapse=" & "),
    " \\\\\n")
cat("  Mean of outcome   & ",
    paste(table_vals$my_cs, collapse=" & "),
    " \\\\\n")   # <-- CS means here
cat("  \\bottomrule\n")
cat("\\end{tabular}\n")
sink()

# ---------------------------------------------------
# ---------------------------------------------------
# ---------------------------------------------------





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








