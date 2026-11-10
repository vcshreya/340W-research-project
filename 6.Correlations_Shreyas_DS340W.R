# ========================================================================
# DS 340W Research Project
# Adapted for: Shreya Chandanshive
# Study: Effect of Proximity to Failure in Resistance Training on
#        Circulating Levels of Neuroprotective Biomarkers
# Analysis: Acute and Chronic Biomarker Correlations
#
# IMPORTANT:
# - The original study's acute/chronic data transformations, Bayesian
#   correlation settings, table construction, and correlation plotting
#   logic are preserved.
# - Only project setup, file checks, package loading, and file-path handling
#   were adapted for Shreya's DS 340W R project.
# - Run this script from the root of the "340W project" RStudio Project.
# - Run 1.Functions_Shreyas_DS340W.R first so the required data frames exist.
# ========================================================================

# Create/confirm folders expected by the original workflow
required_dirs <- c("Data Frames", "Functions", "RData", "Models", "Plots", "Tables")
for (folder in required_dirs) {
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
}

# Confirm that the prepared data from 1.Functions are available
if (length(list.files("Data Frames", pattern = "\\.RData$")) == 0) {
  stop(
    "No prepared data files were found in 'Data Frames/'. ",
    "Run 1.Functions_Shreyas_DS340W.R first."
  )
}

# ========================================================================
# ORIGINAL CORRELATIONS ANALYSIS WORKFLOW
# ========================================================================

# LOAD --------------------------------------------------------------------

## load packages
pacman::p_load(brms, tidybayes, marginaleffects, tidyverse, SimplyAgree,
               patchwork, readxl, lme4, easystats, emmeans, sjPlot)

## load data frames
data.frame.list = list.files(path = "Data Frames/", pattern = "\\.RData$")

for(i in data.frame.list){load(file = paste0("Data Frames/",i))}

rm(data.frame.list, i)

## load functions
function.list = list.files(path = "Functions/", pattern = "\\.RData$")

for(i in function.list){load(file = paste0("Functions/",i))}

rm(function.list, i)

## load aesthetics if available
# The original script loaded THEME.RData, although the correlation plot below
# uses theme_classic() directly. Loading it is therefore optional here.
if (file.exists(file.path("RData", "THEME.RData"))) {
  load(file = file.path("RData", "THEME.RData"))
}

# ##### EDIT DATA FRAMES ##### --------------------------------------------

acute.df=df.acute.long.minus%>%
  select(-timepoint)%>%
  pivot_wider(names_from = "session",
              names_sep = "_",
              values_from = c("bdnf",
                              "igf1",
                              "catb",
                              "il6"))%>%
  group_by(id,week)%>%
  summarise(bdnf_0=mean(bdnf_0,na.rm=TRUE),
            bdnf_1=mean(bdnf_1,na.rm=TRUE),
            igf1_0=mean(igf1_0,na.rm=TRUE),
            igf1_1=mean(igf1_1,na.rm=TRUE),
            catb_0=mean(catb_0,na.rm=TRUE),
            catb_1=mean(catb_1,na.rm=TRUE),
            il6_0=mean(il6_0,na.rm=TRUE),
            il6_1=mean(il6_1,na.rm=TRUE))%>%
  ungroup()%>%
  mutate(BDNF=bdnf_1-bdnf_0,
         `IGF-1`=igf1_1-igf1_0,
         `CatB`=catb_1-catb_0,
         `IL-6`=il6_1-il6_0,
         id=as.factor(id))%>%
  group_by(id)%>%
  summarise(BDNF = mean(BDNF, na.rm = TRUE),
            `IGF-1` = mean(`IGF-1`, na.rm = TRUE),
            `CatB` = mean(`CatB`, na.rm = TRUE),
            `IL-6` = mean(`IL-6`, na.rm = TRUE))%>%
  ungroup()%>%
  select(id,BDNF,
         `IGF-1`,
         `CatB`,
         `IL-6`)


resting.df=df.resting.wide.time.minus%>%
  group_by(id)%>%
  summarise(bdnf_0=mean(bdnf_0,na.rm=TRUE),
            bdnf_1=mean(bdnf_1,na.rm=TRUE),
            igf1_0=mean(igf1_0,na.rm=TRUE),
            igf1_1=mean(igf1_1,na.rm=TRUE),
            catb_0=mean(catb_0,na.rm=TRUE),
            catb_1=mean(catb_1,na.rm=TRUE),
            il6_0=mean(il6_0,na.rm=TRUE),
            il6_1=mean(il6_1,na.rm=TRUE))%>%
  ungroup()%>%
  mutate(BDNF=bdnf_1-bdnf_0,
         `IGF-1`=igf1_1-igf1_0,
         `CatB`=catb_1-catb_0,
         `IL-6`=il6_1-il6_0)%>%
  select(BDNF,
         `IGF-1`,
         `CatB`,
         `IL-6`)


# ###### FIT CORRELATIONS ###### ------------------------------------------

acute.tab=correlation(acute.df,
            bayesian = TRUE,
            bayesian_test = "pd",
            bayesian_prior = "medium.narrow")

chronic.tab=correlation(resting.df,
              bayesian = TRUE,
              bayesian_test = "pd",
              bayesian_prior = "medium.narrow")

# ##### TABLE ##### -------------------------------------------------------

combined.tab=rbind(acute.tab,
                   chronic.tab)%>%
  mutate(Model=c(rep("Acute",6),
                 rep("Chronic",6)))%>%
  rename(X=Parameter1,
         Y=Parameter2,
         r=rho,
         `Lower_HDI`=CI_low,
         `Upper_HDI`=CI_high)%>%
  select(-c(CI,
            Prior_Distribution,
           Prior_Location,
           Prior_Scale,
           pd,
           BF,
           Method,
           n_Obs))%>%
  relocate(Model)%>%
  as.data.frame()

tab_df(combined.tab,
       file = "correlation.table.doc",
       title = "Acute and Chronic Correlations Between Neuroprotective Biomarkers")

# ##### PLOT ###### -------------------------------------------------------

final.corr.plot=acute.tab%>%
  summary()%>%
    plot()+
    theme_classic()+
    labs(title=bquote(bold("A. Acute Effects")))+
  
chronic.tab%>%
  summary()%>%
  plot()+
  theme_classic()+
  labs(title=bquote(bold("B. Chronic Effects")))+
  plot_layout(guides = "collect",
              axes = "collect")

ggsave(final.corr.plot,
       device = "pdf",
       filename = paste0("Plots/final.corr.plot.pdf"),
       width = 10,
       height = 5)


# ##### END OF ANALYSIS ##### ---------------------------------------------


