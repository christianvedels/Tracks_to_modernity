# Regressions
#
# Date updated:   2025-09-11
# Author:         Tom Görges
# Purpose:        Runs Census regressions

rm(list = ls())


# ==== Libraries ====
library(tidyverse)
library(fixest)
library(did)
library(kableExtra) # for latex tables
library(broom)

source("Data_cleaning_scripts/000_Functions.R")

# ==== Load data ====
census = read_csv2("Data/REGRESSION_DATA_Demography.csv", guess_max = 100000)

# ==== Renaming =====
census = census %>% rename(
  Connected_railway = RailAccess,
  Connected_lcp = LCPAccess
)

# function to add stars
starify <- function(est, pval){
  stars <- ifelse(pval < 0.01, "***",
                  ifelse(pval < 0.05, "**",
                         ifelse(pval < 0.1, "*", "")))
  sprintf("%.4f$^{%s}$", est, stars)
}

#########################################
# === TWFE Regressions, No controls === #
#########################################

dep_vars <- c("lnPopulation", "lnChild_women_ratio", "lnManufacturing",
              "lnNotAgriculture", "HISCAM_avg", "lnMigration")

twfe_models <- lapply(dep_vars, \(y) feols(
  as.formula(paste0(y, " ~ Connected_railway | GIS_ID + Year")),
  data = census, cluster = ~ GIS_ID
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
  data    = census,        
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
  outcome   = c("log(Pop.)","log(Child women ratio)","log(Manufacturing+1)",
                "log(Not Agriculture+1)","HISCAM avg","log(Migration)"),
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
sink("../../Apps/Overleaf/Tracks to Modernity/Tables/railways_and_development.tex")

cat("\\begin{tabular}{lcccccc}\n")
cat("  \\toprule\n")
cat("  Outcome: & log(Pop.) & log(Child women ratio) & log(Manufacturing+1) & log(Not Agriculture+1) & HISCAM avg & log(Migration) \\\\\n")
cat("           & (1) & (2) & (3) & (4) & (5) & (6) \\\\\n")
cat("  \\midrule\n")
cat("  \\multicolumn{7}{l}{\\textbf{A. TWFE estimates}}\\\\\n")
cat("  Connected railway & ",
    paste(table_vals$twfe_coef, collapse=" & "),
    " \\\\\n")
cat("                    & ",
    paste(table_vals$twfe_se, collapse=" & "),
    " \\\\\n")
cat("  \\cmidrule(lr){2-7}\n")
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
cat("  \\cmidrule(lr){2-7}\n")
cat("  Observations      & ",
    paste(table_vals$obs_cs, collapse=" & "),
    " \\\\\n")
cat("  Mean of outcome   & ",
    paste(table_vals$my_cs, collapse=" & "),
    " \\\\\n")   # <-- CS means here
cat("  \\bottomrule\n")
cat("\\end{tabular}\n")
sink()


# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

###########################################
# === TWFE Regressions, With controls === #
###########################################

twfe_models <- lapply(dep_vars, \(y) feols(
  as.formula(paste0(y, " ~ Connected_railway + Dist_hamb_year + Dist_cph_year + Pop1801_year + county_by_year | GIS_ID + Year")),
  data = census, cluster = ~ GIS_ID
))

# Have a look
etable(twfe_models, 
       fitstat = ~ n + my, 
       keep = "Connected_railway")


#############################################################
# === Callaway and Sant’Anna Regressions, With controls === #
#############################################################

# Estimate models
cs_models <- lapply(dep_vars, \(y) att_gt(
  yname   = y,    
  tname   = "Year_num",        
  idname  = "GIS_ID_num",     
  gname   = "Treat_year",      
  xformla = ~ Dist_hamb_year + Dist_cph_year + Pop1801_year + county_by_year, 
  data    = census,        
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
  outcome   = c("log(Pop.)","log(Child women ratio)","log(Manufacturing+1)",
                "log(Not Agriculture+1)","HISCAM avg","log(Migration)"),
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
sink("../../Apps/Overleaf/Tracks to Modernity/Tables/railways_and_development_controls.tex")

cat("\\begin{tabular}{lcccccc}\n")
cat("  \\toprule\n")
cat("  Outcome: & log(Pop.) & log(Child women ratio) & log(Manufacturing+1) & log(Not Agriculture+1) & HISCAM avg & log(Migration) \\\\\n")
cat("           & (1) & (2) & (3) & (4) & (5) & (6) \\\\\n")
cat("  \\midrule\n")
cat("  \\multicolumn{7}{l}{\\textbf{A. TWFE estimates}}\\\\\n")
cat("  Connected railway & ",
    paste(table_vals$twfe_coef, collapse=" & "),
    " \\\\\n")
cat("                    & ",
    paste(table_vals$twfe_se, collapse=" & "),
    " \\\\\n")
cat("  \\cmidrule(lr){2-7}\n")
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
cat("  \\cmidrule(lr){2-7}\n")
cat("  Observations      & ",
    paste(table_vals$obs_cs, collapse=" & "),
    " \\\\\n")
cat("  Mean of outcome   & ",
    paste(table_vals$my_cs, collapse=" & "),
    " \\\\\n")   # <-- CS means here
cat("  \\bottomrule\n")
cat("\\end{tabular}\n")
sink()


# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

##########################################
# === TWFE Regressions, Instrumented === #
##########################################

# exclude nodes
census_iv <- census %>%
  filter(away_from_node == 1)

twfe_models <- lapply(dep_vars, \(y) feols(
  as.formula(paste0(y, " ~ 1 | GIS_ID + Year | Connected_railway ~ Connected_lcp")),
  data = census_iv, cluster = ~ GIS_ID
))

# Have a look at results
etable(twfe_models, 
       fitstat = ~ n + my, 
       keep = "Connected_railway")

#######################
# === First stage === #
#######################
etable(
  twfe_models[[1]],
  stage   = 1,
  fitstat = ~ ivf,
  dict = c(
    "Connected_railway" = "Connected railway",
    "Connected_lcp"     = "Connected LCP",
    "GIS_ID" = "Parish"
  ),
  tex = T
  #file = "../../Apps/Overleaf/Tracks to Modernity/Tables/first_stage_census.tex",
  #replace = T
)


###########################################################################
# === Callaway and Sant’Anna Regressions, Instrumented (reduced form) === #
###########################################################################

# Estimate models
cs_models <- lapply(dep_vars, \(y) att_gt(
  yname   = y,    
  tname   = "Year_num",        
  idname  = "GIS_ID_num",     
  gname   = "Treat_year_instr",      
  xformla = ~1, 
  data    = census_iv,        
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
  outcome   = c("log(Pop.)","log(Child women ratio)","log(Manufacturing+1)",
                "log(Not Agriculture+1)","HISCAM avg","log(Migration)"),
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
sink("../../Apps/Overleaf/Tracks to Modernity/Tables/tsls_railways_and_development.tex")

cat("\\begin{tabular}{lcccccc}\n")
cat("  \\toprule\n")
cat("  Outcome: & log(Pop.) & log(Child women ratio) & log(Manufacturing+1) & log(Not Agriculture+1) & HISCAM avg & log(Migration) \\\\\n")
cat("           & (1) & (2) & (3) & (4) & (5) & (6) \\\\\n")
cat("  \\midrule\n")
cat("  \\multicolumn{7}{l}{\\textbf{A. TWFE estimates}}\\\\\n")
cat("  Connected railway & ",
    paste(table_vals$twfe_coef, collapse=" & "),
    " \\\\\n")
cat("                    & ",
    paste(table_vals$twfe_se, collapse=" & "),
    " \\\\\n")
cat("  \\cmidrule(lr){2-7}\n")
cat("  Observations      & ",
    paste(table_vals$obs_twfe, collapse=" & "),
    " \\\\\n")
cat("  Mean of outcome   & ",
    paste(table_vals$my_twfe, collapse=" & "),
    " \\\\\n")   # <-- TWFE means here
cat("  \\midrule\n")
cat("  \\multicolumn{7}{l}{\\textbf{B. Callaway and Sant'Anna estimates (reduced form)}}\\\\\n")
cat("  Connected railway & ",
    paste(table_vals$cs_coef, collapse=" & "),
    " \\\\\n")
cat("                    & ",
    paste(table_vals$cs_se, collapse=" & "),
    " \\\\\n")
cat("  \\cmidrule(lr){2-7}\n")
cat("  Observations      & ",
    paste(table_vals$obs_cs, collapse=" & "),
    " \\\\\n")
cat("  Mean of outcome   & ",
    paste(table_vals$my_cs, collapse=" & "),
    " \\\\\n")   # <-- CS means here
cat("  \\bottomrule\n")
cat("\\end{tabular}\n")
sink()


# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------


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

# and other decompositions



