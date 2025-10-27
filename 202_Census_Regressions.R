# Regressions
#
# Date updated:   2025-10-27
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

# === Prepare data ===
census = census %>% rename(
  Connected_railway = RailAccess,
  Connected_lcp = LCPAccess
)

# harmonize census intervals
census <- census %>%
  mutate(
    Year_num = ifelse(Year_num == 1901, 1900, Year_num),
    Treat_year = ifelse(Treat_year == 1901, 1900, Treat_year)
  )

# Define dependent variables
dep_vars <- c("lnPopulation", "lnChild_women_ratio", "lnManufacturing",
              "lnNotAgriculture", "HISCAM_avg", "lnMigration")


############################
# === TWFE Regressions === #
############################
twfe_models <- lapply(dep_vars, \(y) feols(
  as.formula(paste0(y, " ~ Connected_railway +
                    Dist_hamb_year +
                    Dist_cph_year + 
                    Pop1801_year + 
                    county_by_year + 
                    Dist_ox_year | GIS_ID + Year")),
  data = census, cluster = ~ GIS_ID
))

# Have a look
etable(twfe_models, 
       fitstat = ~ n + my, 
       keep = "Connected_railway")


##############################################
# === Callaway and Sant’Anna Regressions === #
##############################################

# Estimate models
cs_models <- lapply(dep_vars, \(y) att_gt(
  yname   = y,    
  tname   = "Year_num",        
  idname  = "GIS_ID_num",     
  gname   = "Treat_year",      
  xformla = ~ dist_hmb + dist_cph + Pop1801 + county_by_year + DistOxRoad, 
  data    = census,        
  clustervars   = "GIS_ID",
  control_group = "nevertreated"
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

##########################################
# === Create and Export Output Table === #
##########################################

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
sink("Tables/railways_and_development_controls.tex")

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

####################################################
# === Decompositions: Group, Calendar, Dynamic === #
####################################################

# Run all decompositions for each outcome
cs_decomp <- lapply(cs_models, function(m) {
  list(
    group    = aggte(m, type = "group"),
    calendar = aggte(m, type = "calendar"),
    dynamic = aggte(m, type = "dynamic")
  )
})

# Name lists
names(cs_decomp) <- dep_vars

#########################
# === Dynamic plots === #
#########################

# create plots
plots <- lapply(names(cs_decomp), function(v) {
  base_plot <- ggdid(cs_decomp[[v]]$dynamic)
  dat <- layer_data(base_plot)  # extract coefficients + CI
  
  ggplot(dat, aes(x = x, y = y, color = factor(group))) +
    geom_point(size = 8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", size = 2) +  # dashed line at 0
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 4, size = 2) +
    scale_color_manual(values = c("1" = colours$black, "2" = colours$red)) +
    scale_x_continuous(
      limits = c(-45, 45),
      breaks = seq(-40, 40, by = 20)
    ) +
    theme_minimal(base_size = 30) +
    labs(
      x = "Years since treatment",
      y = NULL,
      title = NULL,
      color = NULL,
      fill  = "Confidence Interval"
    ) +
    theme(legend.position = "none")
})

# View the first one
plots[[1]]

# Save plots with names p1_varname, p2_varname, ...
for (i in seq_along(plots)) {
  varname <- dep_vars[i]
  filename <- paste0("p", i, "_", varname, ".png")
  ggsave(
    filename = file.path("Plots/decomposition_census_dynamic_controls", filename),
    plot = plots[[i]],
    width = dims$width, height = dims$height, dpi = 300
  )
}

##########################
# === Calendar plots === #
##########################

# create plots
plots <- lapply(names(cs_decomp), function(v) {
  base_plot <- ggdid(cs_decomp[[v]]$calendar)
  dat <- layer_data(base_plot)  # extract coefficients + CI
  
  ggplot(dat, aes(x = x, y = y, color = factor(group))) +
    geom_point(size = 8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", size = 2) +  # dashed line at 0
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 3, size = 2) +
    scale_color_manual(values = c("1" = colours$red, "2" = colours$black)) +
    theme_minimal(base_size = 30) +
    labs(
      x = "Year",
      y = NULL,
      title = NULL,
      color = NULL,
      fill  = "Confidence Interval"
    ) +
    theme(legend.position = "none")
})

# View the first one
plots[[1]]


# Save plots with names p1_varname, p2_varname, ...
for (i in seq_along(plots)) {
  varname <- dep_vars[i]
  filename <- paste0("p", i, "_", varname, ".png")
  ggsave(
    filename = file.path("Plots/decomposition_census_calendar_controls", filename),
    plot = plots[[i]],
    width = dims$width, height = dims$height, dpi = 300
  )
}

#######################
# === Group plots === #
#######################


# === Group plots === #
plots <- lapply(names(cs_decomp), function(v) {
  gobj <- cs_decomp[[v]]$group
  dat <- data.frame(
    group = gobj$egt,
    att   = gobj$att.egt,
    se    = gobj$se.egt
  )
  
  ggplot(dat, aes(x = att, y = factor(group), color = factor(group))) +
    geom_point(size = 8, color = colours$red) +
    geom_errorbarh(aes(xmin = att - 1.96*se, xmax = att + 1.96*se),
                   height = 0.4, size = 2, color = colours$red) +
    geom_vline(xintercept = 0, linetype = "dashed",  # ← swapped
               color = "grey40", size = 2) +
    theme_minimal(base_size = 30) +
    labs(
      x = "Effect",
      y = "Group",
      color = NULL
    ) +
    theme(legend.position = "none")
})


# View the first one
plots[[1]]


# Save plots with names p1_varname, p2_varname, ...
for (i in seq_along(plots)) {
  varname <- dep_vars[i]
  filename <- paste0("p", i, "_", varname, ".png")
  ggsave(
    filename = file.path("Plots/decomposition_census_group_controls", filename),
    plot = plots[[i]],
    width = dims$width, height = dims$height, dpi = 300
  )
}

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
  control_group = "nevertreated"
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
sink("Tables/tsls_railways_and_development.tex")

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



#########################################
# === TWFE Regressions, no controls === #
#########################################

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
  control_group = "nevertreated"
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
sink("Tables/railways_and_development.tex")

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


