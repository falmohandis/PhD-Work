# Load necessary libraries
library(ARTool)
library(readxl)
library(dplyr)
library(stringr)
library(broom)
library(openxlsx)
library(rstatix)
library(tidyverse)
library(nlme)
library(ggpubr)
library(broom.mixed)
library(tidyr)
library(car)
library(lme4)
library(lmerTest)
library(emmeans)

# library(effectsize)



# Robustly get script directory or fallback to working directory
if ("rstudioapi" %in% rownames(installed.packages())) {
  library(rstudioapi)
  if (isAvailable()) {
    script_dir <- dirname(getActiveDocumentContext()$path)
  } else {
    script_dir <- getwd()
  }
} else {
  script_dir <- getwd()
}

# Load data
file_path <- "Fractional_Increase_0.xlsx"
full_file_path <- file.path(script_dir, file_path)
df <- read_excel(full_file_path)

# Clean column names
colnames(df) <- colnames(df) %>%
  str_replace_all("[ \\-\\(\\)]", "") %>%
  make.names(unique = TRUE)

# Variable transformation
df <- df %>%
  mutate(
    SubjectID = as.factor(ParentFolder),
    TimePoint = as.numeric(TimePoint),
    OcclusionGroup = factor(OcclusionGroup, levels = c("No Occlusion", "Partial Occlusion", "Full Occlusion")),
    OcclusionGroup = as.numeric(OcclusionGroup) - 1,
    HemorrhageLevel = as.factor((HemorrhageLevel / 10) - 1)
  )

df$OcclusionGroup <- factor(df$OcclusionGroup, levels = 0:2, labels = c("None", "Partial", "Full"))
df$HemorrhageLevel <- factor(df$HemorrhageLevel, levels = 0:2, labels = c("10", "20", "30"))

# Select relevant columns
columns_to_keep <- c("SubjectID", "TimePoint", "HemorrhageLevel", "OcclusionGroup",
                     "Gender", "Weightkg", "ProbeLocation", "Avg",
                     "CumulativeVasopressin", "Plasmalyte", "Norepi", "UrineOutput")

# Define common timepoints
selected_timepoints <- c(0, 30, 60, 65, 75, 85, 120, 180, 240)

# Data prep function
prepare_flow_data <- function(df, location) {
  df %>%
    filter(ProbeLocation == location, TimePoint %in% selected_timepoints) %>%
    select(all_of(columns_to_keep)) %>%
    mutate(
      TimePoint = as.factor(TimePoint),
      HemorrhageLevel = as.factor(HemorrhageLevel),
      OcclusionGroup = as.factor(OcclusionGroup),
      SubjectID = as.factor(SubjectID),
      Plasmalyte = as.numeric(Plasmalyte),
      Norepi = as.numeric(Norepi),
      CumulativeVasopressin = as.numeric(CumulativeVasopressin),
      Gender = as.factor(Gender),
    )
}

# Prepare renal flow data
renal_flow <- prepare_flow_data(df, "Renal Flow")

# ---------------------------------------------------
# ANALYSIS at TimePoint = 30
# ---------------------------------------------------

# Subset to TimePoint 30
renal_30 <- renal_flow %>% filter(TimePoint == 30)

# Ensure factors are correct
renal_30 <- renal_30 %>%
  mutate(
    HemorrhageGroup = as.factor(HemorrhageLevel),
    Gender = as.factor(Gender)
  )


####################################################################################################################################################################################################
####################################################################################################################################################################################################
# 1. Compare Avg across Hemorrhage Groups at TimePoint = 30

# # --------------------------------
# # A. VASOPRESSIN as DV - NOT NEEDED AS AT THIS POINT THERE IS NO VASOPRESSIN USE
# # --------------------------------
# vaso_model <- aov(CumulativeVasopressin ~ HemorrhageGroup + Gender + Weightkg, data = renal_30)
# summary(vaso_model)
# 
# # Check assumptions
# shapiro.test(residuals(vaso_model))         # Normality of residuals
# leveneTest(CumulativeVasopressin ~ HemorrhageGroup, data = renal_30)  # Homogeneity of variances
# 
# # Post-hoc
# TukeyHSD(vaso_model, "HemorrhageGroup")
# 
# # Effect size
# eta_squared(vaso_model)

# --------------------------------
# B. PLASMALYTE as DV
# --------------------------------
plasma_model <- aov(Plasmalyte ~ HemorrhageGroup + Gender + Weightkg, data = renal_30)
summary(plasma_model)

# Check assumptions
shapiro.test(residuals(plasma_model))
leveneTest(Plasmalyte ~ HemorrhageGroup, data = renal_30)

# Post-hoc
TukeyHSD(plasma_model, "HemorrhageGroup")

# Effect size
eta_squared(plasma_model)

# --------------------------------
# C. NOREPINEPHRINE as DV
# --------------------------------
norepi_model <- aov(Norepi ~ HemorrhageGroup + Gender + Weightkg, data = renal_30)
summary(norepi_model)

# Check assumptions
shapiro.test(residuals(norepi_model))
leveneTest(Norepi ~ HemorrhageGroup, data = renal_30)

# Post-hoc
TukeyHSD(norepi_model, "HemorrhageGroup")

# Effect size
eta_squared(norepi_model)

# ---------------------------------------------------
# OPTIONAL: Non-parametric fallback using ART if needed
# ---------------------------------------------------

# Example: Vasopressin non-parametric (if residuals not normal)
art_vaso <- art(CumulativeVasopressin ~ HemorrhageGroup , data = renal_30)
art_emm_vaso <- artlm(art_vaso, "HemorrhageGroup")
pairs(emmeans(art_emm_vaso, ~HemorrhageGroup), adjust = "tukey")

# Plasmalyte ART
art_plasma <- art(Plasmalyte ~ HemorrhageLevel, data = renal_30)
art_emm_plasma <- artlm(art_plasma, "HemorrhageLevel")
pairs(emmeans(art_emm_plasma, ~HemorrhageLevel), adjust = "tukey")

# Norepinephrine ART
art_norepi <- art(Norepi ~ HemorrhageLevel, data = renal_30)
art_emm_norepi <- artlm(art_norepi, "HemorrhageLevel")
pairs(emmeans(art_emm_norepi, ~HemorrhageLevel), adjust = "tukey")


####################################################################################################################################################################################################
####################################################################################################################################################################################################
# 2. Compare Avg across HemorrhageGroup AND OcclusionGroup at TimePoint = 60

# =======================================
# ANALYSIS AT TIMEPOINT 60 – DRUG VARIABLES
# =======================================

# Filter data at TimePoint 60
renal_60 <- renal_flow %>% filter(TimePoint == 60)

# Ensure factor conversion
renal_60$HemorrhageLevel <- as.factor(renal_60$HemorrhageLevel)
renal_60$OcclusionGroup <- as.factor(renal_60$OcclusionGroup)
renal_60$Gender <- as.factor(renal_60$Gender)

# --------------------------------
# A. VASOPRESSIN as DV
# # --------------------------------
# vaso_model <- aov(CumulativeVasopressin ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg, data = renal_60)
# summary(vaso_model)
# 
# # Check assumptions
# shapiro.test(residuals(vaso_model))         
# leveneTest(CumulativeVasopressin ~ HemorrhageLevel * OcclusionGroup, data = renal_60)
# 
# # Post-hoc
# TukeyHSD(vaso_model, "HemorrhageLevel")
# TukeyHSD(vaso_model, "OcclusionGroup")
# TukeyHSD(vaso_model, "HemorrhageLevel:OcclusionGroup")
# 
# # Effect size
# eta_squared(vaso_model)

# --------------------------------
# B. PLASMALYTE as DV
# --------------------------------
plasma_model <- aov(Plasmalyte ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg, data = renal_60)
summary(plasma_model)

# Check assumptions
shapiro.test(residuals(plasma_model))
leveneTest(Plasmalyte ~ HemorrhageLevel * OcclusionGroup, data = renal_60)

# Post-hoc
TukeyHSD(plasma_model, "HemorrhageLevel")
TukeyHSD(plasma_model, "OcclusionGroup")
TukeyHSD(plasma_model, "HemorrhageLevel:OcclusionGroup")

# Effect size
eta_squared(plasma_model)

# --------------------------------
# C. NOREPINEPHRINE as DV
# --------------------------------
norepi_model <- aov(Norepi ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg, data = renal_60)
summary(norepi_model)

# Check assumptions
shapiro.test(residuals(norepi_model))
leveneTest(Norepi ~ HemorrhageLevel * OcclusionGroup, data = renal_60)

# Post-hoc
TukeyHSD(norepi_model, "HemorrhageLevel")
TukeyHSD(norepi_model, "OcclusionGroup")
TukeyHSD(norepi_model, "HemorrhageLevel:OcclusionGroup")

# Effect size
eta_squared(norepi_model)

# ---------------------------------------------------
# OPTIONAL: Non-parametric fallback using ART if needed
# ---------------------------------------------------

# Vasopressin ART
art_vaso <- art(CumulativeVasopressin ~ HemorrhageLevel * OcclusionGroup, data = renal_60)
art_emm_vaso <- artlm(art_vaso, "HemorrhageLevel:OcclusionGroup")
pairs(emmeans(art_emm_vaso, ~HemorrhageLevel * OcclusionGroup), adjust = "tukey")

# Plasmalyte ART
art_plasma <- art(Plasmalyte ~ HemorrhageLevel * OcclusionGroup, data = renal_60)
art_emm_plasma <- artlm(art_plasma, "HemorrhageLevel:OcclusionGroup")
pairs(emmeans(art_emm_plasma, ~HemorrhageLevel * OcclusionGroup), adjust = "tukey")

# Norepinephrine ART
art_norepi <- art(Norepi ~ HemorrhageLevel * OcclusionGroup, data = renal_60)
art_emm_norepi <- artlm(art_norepi, "HemorrhageLevel:OcclusionGroup")
pairs(emmeans(art_emm_norepi, ~HemorrhageLevel * OcclusionGroup), adjust = "tukey")


####################################################################################################################################################################################################
####################################################################################################################################################################################################
# 3. Compare the change from TimePoint 60 to 65 across HemorrhageGroup AND OcclusionGroup

# =======================================
# DRUG EFFECTS ACROSS TIMEPOINT 60 → 65
# =======================================

# Filter data for TimePoints 60 and 65
renal_60_65 <- renal_flow %>% filter(TimePoint %in% c(60, 65))

# Ensure factors
renal_60_65$HemorrhageLevel <- as.factor(renal_60_65$HemorrhageLevel)
renal_60_65$OcclusionGroup <- as.factor(renal_60_65$OcclusionGroup)
renal_60_65$Gender <- as.factor(renal_60_65$Gender)


# # --------------------------------
# # A. VASOPRESSIN as DV
# # --------------------------------
# vaso_lmm <- lmer(
#   CumulativeVasopressin ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg + (1 | SubjectID),
#   data = renal_60_65
# )
# summary(vaso_lmm)
# 
# # Assumption check
# shapiro.test(residuals(vaso_lmm))
# 
# # Estimated marginal means & pairwise comparisons
# emm_vaso <- emmeans(vaso_lmm, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
# pairs(emm_vaso, adjust = "tukey")
# 
# # Effect size
# eta_squared(vaso_lmm)

# --------------------------------
# B. PLASMALYTE as DV
# --------------------------------
plasma_lmm <- lmer(
  Plasmalyte ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg + (1 | SubjectID),
  data = renal_60_65
)
summary(plasma_lmm)

# Assumption check
shapiro.test(residuals(plasma_lmm))

# Estimated marginal means & pairwise comparisons
emm_plasma <- emmeans(plasma_lmm, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
pairs(emm_plasma, adjust = "tukey")

# Effect size
eta_squared(plasma_lmm)

# --------------------------------
# C. NOREPINEPHRINE as DV
# --------------------------------
norepi_lmm <- lmer(
  Norepi ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg + (1 | SubjectID),
  data = renal_60_65
)
summary(norepi_lmm)

# Assumption check
shapiro.test(residuals(norepi_lmm))

# Estimated marginal means & pairwise comparisons
emm_norepi <- emmeans(norepi_lmm, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
pairs(emm_norepi, adjust = "tukey")

# Effect size
eta_squared(norepi_lmm)

# ---------------------------------------------------
# OPTIONAL: Non-parametric fallback using ART if needed
# ---------------------------------------------------
# 
# # VASOPRESSIN ART
# art_vaso <- art(CumulativeVasopressin ~ TimePoint * HemorrhageLevel * OcclusionGroup, data = renal_60_65)
# art_emm_vaso <- artlm(art_vaso, "TimePoint:HemorrhageLevel:OcclusionGroup")
# pairs(emmeans(art_emm_vaso, ~ TimePoint * HemorrhageLevel * OcclusionGroup), adjust = "tukey")
# 
# # PLASMALYTE ART
# art_plasma <- art(Plasmalyte ~ TimePoint * HemorrhageLevel * OcclusionGroup, data = renal_60_65)
# art_emm_plasma <- artlm(art_plasma, "TimePoint:HemorrhageLevel:OcclusionGroup")
# pairs(emmeans(art_emm_plasma, ~ TimePoint * HemorrhageLevel * OcclusionGroup), adjust = "tukey")
# 
# # NOREPINEPHRINE ART
# art_norepi <- art(Norepi ~ TimePoint * HemorrhageLevel * OcclusionGroup, data = renal_60_65)
# art_emm_norepi <- artlm(art_norepi, "TimePoint:HemorrhageLevel:OcclusionGroup")
# pairs(emmeans(art_emm_norepi, ~ TimePoint * HemorrhageLevel * OcclusionGroup), adjust = "tukey")
# 

####################################################################################################################################################################################################
####################################################################################################################################################################################################
# 4. Compare change over ALL TimePoints across HemorrhageLevel AND OcclusionGroup
# =======================================
# DRUG EFFECTS ACROSS ALL TIMEPOINTS
# =======================================

# Ensure categorical variables are treated as factors
renal_flow$HemorrhageLevel <- as.factor(renal_flow$HemorrhageLevel)
renal_flow$OcclusionGroup <- as.factor(renal_flow$OcclusionGroup)
renal_flow$Gender <- as.factor(renal_flow$Gender)


# --------------------------------
# A. VASOPRESSIN as DV
# --------------------------------
vaso_all <- lmer(
  CumulativeVasopressin ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg + (1 | SubjectID),
  data = renal_flow
)
summary(vaso_all)

# Assumption check
shapiro.test(residuals(vaso_all))

# Estimated marginal means and comparisons
emm_vaso_all <- emmeans(vaso_all, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
pairs(emm_vaso_all, adjust = "tukey")

# Effect size
eta_squared(vaso_all)

# --------------------------------
# B. PLASMALYTE as DV
# --------------------------------
plasma_all <- lmer(
  Plasmalyte ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg + (1 | SubjectID),
  data = renal_flow
)
summary(plasma_all)

# Assumption check
shapiro.test(residuals(plasma_all))

# Estimated marginal means and comparisons
emm_plasma_all <- emmeans(plasma_all, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
pairs(emm_plasma_all, adjust = "tukey")

# Effect size
eta_squared(plasma_all)

# --------------------------------
# C. NOREPINEPHRINE as DV
# --------------------------------
norepi_all <- lmer(
  Norepi ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg + (1 | SubjectID),
  data = renal_flow
)
summary(norepi_all)

# Assumption check
shapiro.test(residuals(norepi_all))

# Estimated marginal means and comparisons
emm_norepi_all <- emmeans(norepi_all, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
pairs(emm_norepi_all, adjust = "tukey")

# Effect size
eta_squared(norepi_all)

# ---------------------------------------------------
# OPTIONAL: Non-parametric fallback using ART if needed
# ---------------------------------------------------

# # VASOPRESSIN ART
# art_vaso_all <- art(CumulativeVasopressin ~ TimePoint * HemorrhageLevel * OcclusionGroup, data = renal_flow)
# art_emm_vaso_all <- artlm(art_vaso_all, "TimePoint:HemorrhageLevel:OcclusionGroup")
# pairs(emmeans(art_emm_vaso_all, ~ TimePoint * HemorrhageLevel * OcclusionGroup), adjust = "tukey")

# PLASMALYTE ART
art_plasma_all <- art(Plasmalyte ~ TimePoint * HemorrhageLevel * OcclusionGroup, data = renal_flow)
art_emm_plasma_all <- artlm(art_plasma_all, "TimePoint:HemorrhageLevel:OcclusionGroup")
pairs(emmeans(art_emm_plasma_all, ~ TimePoint * HemorrhageLevel * OcclusionGroup), adjust = "tukey")

# NOREPINEPHRINE ART
art_norepi_all <- art(Norepi ~ TimePoint * HemorrhageLevel * OcclusionGroup, data = renal_flow)
art_emm_norepi_all <- artlm(art_norepi_all, "TimePoint:HemorrhageLevel:OcclusionGroup")
pairs(emmeans(art_emm_norepi_all, ~ TimePoint * HemorrhageLevel * OcclusionGroup), adjust = "tukey")

