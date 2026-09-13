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

get.sem(df.reliability, marker = "igf1")

# PLOT --------------------------------------------------------------------

plot.reliability(data.frame = df.reliability,
                 marker = "igf1")

# ##### ACUTE ##### -------------------------------------------------------

# EXPLORE LINEARITY -------------------------------------------------------

plot.linearity(data = df.acute.wide.time.minus,
               marker = "igf1",
               type = "acute") # linearity of RPE/RIR looks sufficient

# FIT MODEL ---------------------------------------------------------------

igf1.acute.model = brm(
  data = df.acute.long.minus,
  family = gaussian(),
  standardize(igf1) ~ week * session * standardize(avg.rpe) + (week * session | id),
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

save(igf1.acute.model, file = "Models/igf1.acute.model.RData")

# EXTRACT EFFECTS ---------------------------------------------------------

get.acute.effects(model = igf1.acute.model,
                  data.frame = df.acute.long.minus,
                  marker = "igf1")

# PLOT --------------------------------------------------------------------

plot.acute.effects(draws = igf1.acute.plot.draws,
                   data.frame = df.acute.long.minus,
                   marker = "igf1")

# #################### ----------------------------------------------------

# ##### CHRONIC ##### -----------------------------------------------------

# EXPLORE LINEARITY -------------------------------------------------------

plot.linearity(data = df.resting.wide.time.minus,
               marker = "igf1",
               type = "chronic") # linearity of RPE/RIR looks sufficient

# FIT MODEL ---------------------------------------------------------------

igf1.chronic.model = brm(
  data = df.resting.long.minus,
  family = gaussian(),
  standardize(igf1) ~ time + time:standardize(avg.rpe) + (time | id),
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

save(igf1.chronic.model, file = "Models/igf1.chronic.model.RData")

# EXTRACT EFFECTS ---------------------------------------------------------

get.chronic.effects(model = igf1.chronic.model,
                    data.frame = df.resting.long.minus,
                    marker = "igf1")

# PLOT --------------------------------------------------------------------

plot.chronic.effects(draws = igf1.chronic.plot.draws,
                     data.frame = df.resting.long.minus,
                     marker = "igf1")

# #################### ----------------------------------------------------
# ##### COMBINED ##### ----------------------------------------------------

# PLOT --------------------------------------------------------------------

plot.combined.effects(draws.acute = igf1.acute.plot.draws,
                      draws.chronic = igf1.chronic.plot.draws,
                      data.frame.acute = df.acute.long.minus,
                      data.frame.chronic = df.resting.long.minus,
                      marker = "igf1")

# TABLE -------------------------------------------------------------------

get.combined.effects.table(acute.effects = igf1.acute.estimates.df,
                           chronic.effects = igf1.chronic.estimates.df,
                           marker = "igf1")

# #################### ----------------------------------------------------
# ##### END OF ANALYSIS ##### ---------------------------------------------


