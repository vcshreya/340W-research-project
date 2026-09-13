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


