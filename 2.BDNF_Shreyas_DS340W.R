# ========================================================================
# DS 340W Research Project
# Adapted for: Shreya Chandanshive
# Study: Effect of Proximity to Failure in Resistance Training on
#        Circulating Levels of Neuroprotective Biomarkers
# Analysis: BDNF
#
# IMPORTANT:
# - The original study's BDNF statistical model, priors, seed, sampling
#   settings, effect extraction, and plotting logic are preserved.
# - This adaptation only adds project checks/setup for Shreya's R project.
# - Run this script from the root of the "340W project" RStudio Project.
# - Run 1.Functions.R first so the required data frames and functions exist.
# ========================================================================

# Check that this script is being run from the project root
required_dirs <- c("Data Frames", "Functions", "RData", "Models", "Plots", "Tables")
for (folder in required_dirs) {
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
}

# Confirm that the prepared data and saved functions from 1.Functions.R exist
if (length(list.files("Data Frames", pattern = "\\.RData$")) == 0) {
  stop(
    "No prepared data files were found in 'Data Frames/'. ",
    "Run 1.Functions.R first."
  )
}

if (length(list.files("Functions", pattern = "\\.RData$")) == 0) {
  stop(
    "No saved analysis functions were found in 'Functions/'. ",
    "Run 1.Functions.R first."
  )
}

# # The original plotting workflow expects this theme object.
# if (!file.exists(file.path("RData", "THEME.RData"))) {
#   warning(
#     "RData/THEME.RData was not found. ",
#     "The original plot functions require it. ",
#     "Model fitting can still be reviewed, but plotting will fail until ",
#     "THEME.RData is available."
#   )
# }

# If the original THEME.RData file is unavailable,
# recreate a compatible plotting theme for this project
if (!file.exists(file.path("RData", "THEME.RData"))) {
  
  THEME <- theme_gray(base_size = 12) +
    theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      ),
      
      axis.title = element_text(
        face = "bold"
      ),
      
      axis.text = element_text(
        color = "black"
      ),
      
      strip.text = element_text(
        face = "bold",
        color = "black"
      ),
      
      strip.background = element_rect(
        fill = "grey90",
        color = NA
      ),
      
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.7
      ),
      
      legend.position = "none"
    )
  
  save(
    THEME,
    file = "RData/THEME.RData"
  )
  
  message(
    "Original THEME.RData was unavailable. ",
    "A compatible theme was created and saved to RData/THEME.RData."
  )
}

# ========================================================================
# ORIGINAL BDNF ANALYSIS WORKFLOW
# ========================================================================

# LOAD --------------------------------------------------------------------

## load packages
pacman::p_load(brms,tidybayes,marginaleffects,tidyverse,SimplyAgree,
               patchwork,readxl,lme4,easystats,emmeans,cmdstanr,sjPlot)

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

get.sem(df.reliability, marker = "bdnf")

# PLOT --------------------------------------------------------------------

plot.reliability(data.frame = df.reliability,
                 marker = "bdnf")

# ##### ACUTE ##### -------------------------------------------------------

# EXPLORE LINEARITY -------------------------------------------------------

plot.linearity(data = df.acute.wide.time.minus,
               marker = "bdnf",
               type = "acute") # linearity of RPE/RIR looks sufficient

# FIT MODEL ---------------------------------------------------------------

bdnf.acute.model = brm(
  data = df.acute.long.minus,
  family = gaussian(),
  standardize(bdnf) ~ week * session * standardize(avg.rpe) + (week * session | id),
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

save(bdnf.acute.model, file = "Models/bdnf.acute.model.RData")

# EXTRACT EFFECTS ---------------------------------------------------------

get.acute.effects(model = bdnf.acute.model,
                  data.frame = df.acute.long.minus,
                  marker = "bdnf")

# PLOT --------------------------------------------------------------------

plot.acute.effects(draws = bdnf.acute.plot.draws,
                   data.frame = df.acute.long.minus,
                   marker = "bdnf")

# #################### ----------------------------------------------------

# ##### CHRONIC ##### -----------------------------------------------------

# EXPLORE LINEARITY -------------------------------------------------------

plot.linearity(data = df.resting.wide.time.minus,
               marker = "bdnf",
               type = "chronic") # linearity of RPE/RIR looks sufficient

# FIT MODEL ---------------------------------------------------------------

bdnf.chronic.model = brm(
  data = df.resting.long.minus,
  family = gaussian(),
  standardize(bdnf) ~ time + time:standardize(avg.rpe) + (time | id),
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

save(bdnf.chronic.model, file = "Models/bdnf.chronic.model.RData")

# EXTRACT EFFECTS ---------------------------------------------------------

get.chronic.effects(model = bdnf.chronic.model,
                  data.frame = df.resting.long.minus,
                  marker = "bdnf")

# PLOT --------------------------------------------------------------------

plot.chronic.effects(draws = bdnf.chronic.plot.draws,
                   data.frame = df.resting.long.minus,
                   marker = "bdnf")

# #################### ----------------------------------------------------
# ##### COMBINED ##### ----------------------------------------------------

# PLOT --------------------------------------------------------------------

plot.combined.effects(draws.acute = bdnf.acute.plot.draws,
                      draws.chronic = bdnf.chronic.plot.draws,
                      data.frame.acute = df.acute.long.minus,
                      data.frame.chronic = df.resting.long.minus,
                      marker = "bdnf")

# TABLE -------------------------------------------------------------------

get.combined.effects.table(acute.effects = bdnf.acute.estimates.df,
                           chronic.effects = bdnf.chronic.estimates.df,
                           marker = "bdnf")

# #################### ----------------------------------------------------
# ##### END OF ANALYSIS ##### ---------------------------------------------


