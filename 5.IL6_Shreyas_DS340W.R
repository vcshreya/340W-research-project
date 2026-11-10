# ========================================================================
# DS 340W Research Project
# Adapted for: Shreya Chandanshive
# Study: Effect of Proximity to Failure in Resistance Training on
#        Circulating Levels of Neuroprotective Biomarkers
# Analysis: Interleukin-6 (IL-6)
#
# IMPORTANT:
# - The original study's IL-6 model formulas, priors, seed, chains,
#   iterations, warmup, max_treedepth, effect extraction, and plotting
#   logic are preserved.
# - Only project setup, file checks, CmdStanR loading, and a plotting-theme
#   fallback were adapted for Shreya's DS 340W R project.
# - Run this script from the root of the "340W project" RStudio Project.
# - Run 1.Functions_Shreyas_DS340W.R first.
# ========================================================================

# Create/confirm folders expected by the original analysis workflow
required_dirs <- c("Data Frames", "Functions", "RData", "Models", "Plots", "Tables")
for (folder in required_dirs) {
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
}

# Confirm that 1.Functions has prepared the required objects
if (length(list.files("Data Frames", pattern = "\\.RData$")) == 0) {
  stop(
    "No prepared data files were found in 'Data Frames/'. ",
    "Run 1.Functions_Shreyas_DS340W.R first."
  )
}

if (length(list.files("Functions", pattern = "\\.RData$")) == 0) {
  stop(
    "No saved analysis functions were found in 'Functions/'. ",
    "Run 1.Functions_Shreyas_DS340W.R first."
  )
}

# Recreate a compatible theme only if the original THEME.RData file is absent.
# This affects figure appearance only; it does not change any statistical result.
if (!file.exists(file.path("RData", "THEME.RData"))) {
  suppressPackageStartupMessages(library(ggplot2))

  THEME <- theme_gray(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      strip.text = element_text(face = "bold", color = "black"),
      strip.background = element_rect(fill = "grey90", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
      legend.position = "none"
    )

  save(THEME, file = file.path("RData", "THEME.RData"))
  message("Created compatible RData/THEME.RData for Shreya's project.")
}

# ========================================================================
# ORIGINAL IL-6 ANALYSIS WORKFLOW
# ========================================================================

# LOAD --------------------------------------------------------------------

## load packages
pacman::p_load(brms, tidybayes, marginaleffects, tidyverse, SimplyAgree,
               patchwork, readxl, lme4, easystats, emmeans, sjPlot)

# CmdStanR is not installed from ordinary CRAN repositories.
# Load it separately so pacman does not repeatedly try to install it from CRAN.
if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  stop(
    paste0(
      "Package 'cmdstanr' is not installed. Install it first with:\n",
      "install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))\n",
      "Then run cmdstanr::check_cmdstan_toolchain() and cmdstanr::install_cmdstan(cores = 4)."
    )
  )
}
library(cmdstanr)

## load data frames
data.frame.list = list.files(path = "Data Frames/", pattern = "\\.RData$")

for(i in data.frame.list){load(file = paste0("Data Frames/",i))}

rm(data.frame.list, i)

## load functions
function.list = list.files(path = "Functions/", pattern = "\\.RData$")

for(i in function.list){load(file = paste0("Functions/",i))}

rm(function.list, i)

## load asethetics
load(file = "RData/THEME.RData")

# ##### RELIABILITY ##### -------------------------------------------------

# SEM ---------------------------------------------------------------------

get.sem(df.reliability, marker = "il6")

# PLOT --------------------------------------------------------------------

plot.reliability(data.frame = df.reliability,
                 marker = "il6")

# ##### ACUTE ##### -------------------------------------------------------

# EXPLORE LINEARITY -------------------------------------------------------

plot.linearity(data = df.acute.wide.time.minus,
               marker = "il6",
               type = "acute") # linearity of RPE/RIR looks sufficient

# FIT MODEL ---------------------------------------------------------------

il6.acute.model = brm(
  data = df.acute.long.minus,
  family = gaussian(),
  standardize(il6) ~ week * session * standardize(avg.rpe) + (week * session | id),
  prior = c(set_prior(prior = "normal(0,1)",class = "Intercept"),
            set_prior(prior = "normal(0,1)",class = "b"),
            set_prior(prior = "exponential(1)",class = "sd"),
            set_prior(prior = "exponential(1)",class = "sigma"),
            set_prior(prior = "lkj(4)",class = "cor")),
  save_pars = save_pars(all = TRUE),
  seed = 123,
  chains = 4,
  cores = 4,
  backend = "cmdstanr",
  iter = 4000,
  warmup = 1000,
  control = list(max_treedepth=20))

save(il6.acute.model, file = "Models/il6.acute.model.RData")

# EXTRACT EFFECTS ---------------------------------------------------------

get.acute.effects(model = il6.acute.model,
                  data.frame = df.acute.long.minus,
                  marker = "il6")

# PLOT --------------------------------------------------------------------

plot.acute.effects(draws = il6.acute.plot.draws,
                   data.frame = df.acute.long.minus,
                   marker = "il6")

# #################### ----------------------------------------------------

# ##### CHRONIC ##### -----------------------------------------------------

# EXPLORE LINEARITY -------------------------------------------------------

plot.linearity(data = df.resting.wide.time.minus,
               marker = "il6",
               type = "chronic") # linearity of RPE/RIR looks sufficient

# FIT MODEL ---------------------------------------------------------------

il6.chronic.model = brm(
  data = df.resting.long.minus,
  family = gaussian(),
  standardize(il6) ~ time + time:standardize(avg.rpe) + (time | id),
  prior = c(set_prior(prior = "normal(0,1)",class = "Intercept"),
            set_prior(prior = "normal(0,1)",class = "b"),
            set_prior(prior = "exponential(1)",class = "sd"),
            set_prior(prior = "exponential(1)",class = "sigma"),
            set_prior(prior = "lkj(4)",class = "cor")),
  save_pars = save_pars(all = TRUE),
  seed = 123,
  chains = 4,
  cores = 4,
  backend = "cmdstanr",
  iter = 4000,
  warmup = 1000)

save(il6.chronic.model, file = "Models/il6.chronic.model.RData")

# EXTRACT EFFECTS ---------------------------------------------------------

get.chronic.effects(model = il6.chronic.model,
                    data.frame = df.resting.long.minus,
                    marker = "il6")

# PLOT --------------------------------------------------------------------

plot.chronic.effects(draws = il6.chronic.plot.draws,
                     data.frame = df.resting.long.minus,
                     marker = "il6")

# #################### ----------------------------------------------------
# ##### COMBINED ##### ----------------------------------------------------

# PLOT --------------------------------------------------------------------

plot.combined.effects(draws.acute = il6.acute.plot.draws,
                      draws.chronic = il6.chronic.plot.draws,
                      data.frame.acute = df.acute.long.minus,
                      data.frame.chronic = df.resting.long.minus,
                      marker = "il6")

# TABLE -------------------------------------------------------------------

get.combined.effects.table(acute.effects = il6.acute.estimates.df,
                           chronic.effects = il6.chronic.estimates.df,
                           marker = "il6")

# #################### ----------------------------------------------------
# ##### END OF ANALYSIS ##### ---------------------------------------------


