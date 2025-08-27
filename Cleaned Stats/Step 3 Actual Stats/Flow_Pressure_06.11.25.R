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
                     "CumulativeVasopressin",	"Plasmalyte",	"Norepi",'UrineOutput'
                    )

# Define common timepoints
selected_timepoints <- c(0,30,
                         # 31,
                         60,
                         # 61,
                         65,
                         # 66,71,
                         75,
                         # 76,81,
                         85,
                         # 86,
                         120,
                         180,
                         240)

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
      CumulativeVasopressin = as.numeric(CumulativeVasopressin)
    )
}



# Prepare renal flow data
renal_flow <- prepare_flow_data(df, "Renal Flow")

# Summary stats and plots for renal flow
renal_flow %>%
  group_by(TimePoint, HemorrhageLevel, OcclusionGroup) %>%
  get_summary_stats(Avg, type = "mean_sd") %>%
  print(n = 100)

ggboxplot(renal_flow, x = "TimePoint", y = "Avg",
          color = "OcclusionGroup", palette = "jco",
          facet.by = "HemorrhageLevel") 

ggboxplot(renal_flow, x = "TimePoint", y = "Avg",
          color = "HemorrhageLevel", palette = "jco",
          facet.by = "OcclusionGroup")

renal_flow %>%
  group_by(TimePoint, HemorrhageLevel, OcclusionGroup) %>%
  identify_outliers(Avg) %>%
  print(n = 100)

ggqqplot(renal_flow, "Avg", ggtheme = theme_bw()) +
  facet_grid(HemorrhageLevel + OcclusionGroup ~ TimePoint, labeller = "label_both")

# Prepare carotid flow data
carotid_flow <- prepare_flow_data(df, "Carotid Flow")

# Summary stats and plots for carotid flow
carotid_flow %>%
  group_by(TimePoint, HemorrhageLevel, OcclusionGroup) %>%
  get_summary_stats(Avg, type = "mean_sd") %>%
  print(n = 100)

ggboxplot(carotid_flow, x = "TimePoint", y = "Avg",
          color = "OcclusionGroup", palette = "jco",
          facet.by = "HemorrhageLevel")

ggboxplot(carotid_flow, x = "TimePoint", y = "Avg",
          color = "HemorrhageLevel", palette = "jco",
          facet.by = "OcclusionGroup")

carotid_flow %>%
  group_by(TimePoint, HemorrhageLevel, OcclusionGroup) %>%
  identify_outliers(Avg) %>%
  print(n = 100)

ggqqplot(carotid_flow, "Avg", ggtheme = theme_bw()) +
  facet_grid(HemorrhageLevel + OcclusionGroup ~ TimePoint, labeller = "label_both")


# Code to view outliers in each dataset and then remove anything with greater than 3 st.devs from the group mean
# renal_outliers <- renal_flow %>%
#   group_by(HemorrhageLevel, OcclusionGroup) %>%
#   mutate(
#     group_mean = mean(Avg, na.rm = TRUE),
#     group_sd   = sd(Avg, na.rm = TRUE),
#     z_score    = (Avg - group_mean) / group_sd,
#     is_outlier = abs(z_score) > 3
#   ) %>%
#   filter(is_outlier)
# 
# carotid_outliers <- carotid_flow %>%
#   group_by(HemorrhageLevel, OcclusionGroup) %>%
#   mutate(
#     group_mean = mean(Avg, na.rm = TRUE),
#     group_sd   = sd(Avg, na.rm = TRUE),
#     z_score    = (Avg - group_mean) / group_sd,
#     is_outlier = abs(z_score) > 3
#   ) %>%
#   filter(is_outlier)
# 
# print(renal_outliers, n = 100)
# print(carotid_outliers, n = 100)

# Function to remove outliers beyond 3 SD from mean per eachgroup
remove_outliers_by_group <- function(data) {
  data %>%
    group_by(HemorrhageLevel, OcclusionGroup) %>%
    mutate(
      group_mean = mean(Avg, na.rm = TRUE),
      group_sd   = sd(Avg, na.rm = TRUE),
      z_score    = (Avg - group_mean) / group_sd,
      is_outlier = abs(z_score) > 3
    ) %>%
    ungroup() %>%
    filter(!is_outlier) %>%
    select(-group_mean, -group_sd, -z_score, -is_outlier)
}

# Clean both datasets
renal_flow <- remove_outliers_by_group(renal_flow)
carotid_flow <- remove_outliers_by_group(carotid_flow)


# # For renal_flow
# renal_flow$Plasmalyte <- as.numeric(renal_flow$Plasmalyte)
# renal_flow$Norepi <- as.numeric(renal_flow$Norepi)
# renal_flow$CumulativeVasopressin <- as.numeric(renal_flow$CumulativeVasopressin)
# 
# # For carotid_flow 
# carotid_flow$Plasmalyte <- as.numeric(carotid_flow$Plasmalyte)
# carotid_flow$Norepi <- as.numeric(carotid_flow$Norepi)
# carotid_flow$CumulativeVasopressin <- as.numeric(carotid_flow$CumulativeVasopressin)




# Things to add: hemorrhage/balloon difference at each time point vs the fluid/drug
                      # ============================
                      # RENAL FLOW ANALYSIS
                      # ============================

####################################################################################################################################################################################################
####################################################################################################################################################################################################
# 1. Compare Avg across Hemorrhage Groups at TimePoint = 30

# Filter data at TimePoint 30
renal_30 <- renal_flow %>% filter(TimePoint == 30)

# Test the normality of the data before running the test (for sanity)
renal_30 %>%
  group_by(HemorrhageLevel) %>%
  shapiro_test(Avg)

# Ensure HemorrhageLevel and Gender are factors
renal_30$HemorrhageLevel <- as.factor(renal_30$HemorrhageLevel)
renal_30$Gender <- as.factor(renal_30$Gender)


# Generate the actual ANOVA
anova_30 <- aov(Avg ~ HemorrhageLevel + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = renal_30)
summary(anova_30)

# Tukey Post-Hoc Analysis
tukey_30 <- TukeyHSD(anova_30)
print(tukey_30)

# Normality check for ANOVA residuals at TimePoint 30
shapiro.test(residuals(anova_30))  # Shapiro-Wilk test

# Effect sizes with 95% CI
eta_squared(anova_30)

# --------------------------------------------------------
# NO TRANSFORMATION LED TO NORMAL RESIDUALS — ORIGINAL DATA KEPT AS IS
# The following transformations were tested and did not help:

# Log transform
# renal_30 <- renal_30 %>% mutate(Avg_log = log(Avg + 1))  # +1 to avoid log(0)
# anova_30_log <- aov(Avg_log ~ HemorrhageLevel + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = renal_30)
# shapiro.test(residuals(anova_30_log))

# Square root transform - Produced a p-value = 0.0326: however this is because NaN's were produced
# renal_30 <- renal_30 %>% mutate(Avg_sqrt = sqrt(Avg))
# anova_30_sqrt <- aov(Avg_sqrt ~ HemorrhageLevel + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = renal_30)
# shapiro.test(residuals(anova_30_sqrt))

# Square root of absolute value transform - just bad
# renal_30 <- renal_30 %>% mutate(Avg_sqrt = sqrt(sqrt(Avg^2)))
# anova_30_sqrt <- aov(Avg_sqrt ~ HemorrhageLevel + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = renal_30)
# shapiro.test(residuals(anova_30_sqrt))

# Cube root transform
# renal_30 <- renal_30 %>% mutate(Avg_cbrt = sign(Avg) * abs(Avg)^(1/3))
# anova_30_cbrt <- aov(Avg_cbrt ~ HemorrhageLevel + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = renal_30)
# shapiro.test(residuals(anova_30_cbrt))

# Reciprocal transform (handle zeros with care)
# renal_30 <- renal_30 %>% mutate(Avg_recip = ifelse(Avg == 0, NA, 1 / Avg))
# anova_30_recip <- aov(Avg_recip ~ HemorrhageLevel + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = renal_30)
# shapiro.test(residuals(anova_30_recip))

# --------------------------------------------------------
# NON-PARAMETRIC ALTERNATIVE METHOD — Aligned Rank Transform

# Fit the ART model
art_model <- art(Avg ~ HemorrhageLevel, data = renal_30)

# Use artlm() to create the model for EMMs
art_emm_model <- artlm(art_model, "HemorrhageLevel")

# Estimated marginal means
emm <- emmeans(art_emm_model, ~ HemorrhageLevel)

# Pairwise comparisons with Tukey adjustment
pairs(emm, adjust = "tukey")

####################################################################################################################################################################################################
####################################################################################################################################################################################################
# 2. Compare Avg across HemorrhageGroup AND OcclusionGroup at TimePoint = 60

# Filter data at TimePoint 60
renal_60 <- renal_flow %>% filter(TimePoint == 60)

# --------------------------------------------------------
# Optional: Normality test by group (commented out for now)
# renal_60 %>%
#   group_by(HemorrhageLevel, OcclusionGroup) %>%
#   shapiro_test(Avg)

# Ensure factor conversion
renal_60$HemorrhageLevel <- as.factor(renal_60$HemorrhageLevel)
renal_60$Gender <- as.factor(renal_60$Gender)

# Run ANOVA with interaction and covariates
anova_60 <- aov(Avg ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg + 
                  CumulativeVasopressin + Plasmalyte + Norepi, data = renal_60)
summary(anova_60)

# Tukey HSD post-hoc tests
TukeyHSD(anova_60, "OcclusionGroup")
TukeyHSD(anova_60, "HemorrhageLevel")
TukeyHSD(anova_60, "HemorrhageLevel:OcclusionGroup")

# --------------------------------------------------------
# Optional estimated marginal means with pairwise comparisons (commented)
# library(emmeans)
# emm_60 <- emmeans(anova_60, ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg)
# tukey_60 <- pairs(emm_60, adjust = "tukey")
# as.data.frame(tukey_60)

# --------------------------------------------------------
# Normality check for ANOVA residuals
shapiro.test(residuals(anova_60))  # Shapiro-Wilk test

# --------------------------------------------------------
# Transformation attempts

# Log transform- p-value = 0.0003662
renal_60 <- renal_60 %>% mutate(Avg_log = log(Avg + 1))  # +1 to avoid log(0)
anova_60_log <- aov(Avg_log ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg + 
                      CumulativeVasopressin + Plasmalyte + Norepi, data = renal_60)
shapiro.test(residuals(anova_60_log))

# Square root transform-  p-value = 0.001653, but NaN was produced due to negatives
renal_60 <- renal_60 %>% mutate(Avg_sqrt = sqrt(Avg))
anova_60_sqrt <- aov(Avg_sqrt ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg + 
                       CumulativeVasopressin + Plasmalyte + Norepi, data = renal_60)
shapiro.test(residuals(anova_60_sqrt))

# Cube root transform- p-value = 0.001925
renal_60 <- renal_60 %>% mutate(Avg_cbrt = sign(Avg) * abs(Avg)^(1/3))
anova_60_cbrt <- aov(Avg_cbrt ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg + 
                       CumulativeVasopressin + Plasmalyte + Norepi, data = renal_60)
shapiro.test(residuals(anova_60_cbrt))

# Reciprocal transform (handle zeros with care)
renal_60 <- renal_60 %>% mutate(Avg_recip = ifelse(Avg == 0, NA, 1 / Avg))
anova_60_recip <- aov(Avg_recip ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg + 
                        CumulativeVasopressin + Plasmalyte + Norepi, data = renal_60)
shapiro.test(residuals(anova_60_recip))

# --------------------------------------------------------
# NON-PARAMETRIC METHODS — Aligned Rank Transform

# Initial failed attempt including covariates (commented)
# art_model <- art(Avg ~ (HemorrhageLevel + OcclusionGroup + Gender + Weightkg + 
#                         CumulativeVasopressin + Plasmalyte + Norepi)^3, data = renal_60)

# Alternative model (commented out because it has too many factors)
# art_model <- art(Avg ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg + 
#                  CumulativeVasopressin + Plasmalyte + Norepi, data = renal_60)
# anova(art_model)

# Final ART model: HemorrhageLevel * OcclusionGroup only
art_model <- art(Avg ~ HemorrhageLevel * OcclusionGroup, data = renal_60)

# Create the model for estimated marginal means
art_emm_model <- artlm(art_model, "HemorrhageLevel:OcclusionGroup")

# Estimated marginal means
emm <- emmeans(art_emm_model, ~ HemorrhageLevel * OcclusionGroup)

# Pairwise comparisons with Tukey adjustment
pairs(emm, adjust = "tukey")

####################################################################################################################################################################################################
####################################################################################################################################################################################################
# 3. Compare the change from TimePoint 60 to 65 across HemorrhageGroup AND OcclusionGroup

# Filter data for TimePoints 60 and 65
renal_60_65 <- renal_flow %>% filter(TimePoint %in% c(60, 65))

# --------------------------------------------------------
# Optional: Normality tests by group at each time point (commented)
# renal_flow %>%
#   filter(TimePoint == 60) %>%
#   group_by(HemorrhageLevel, OcclusionGroup) %>%
#   shapiro_test(Avg)
#
# renal_flow %>%
#   filter(TimePoint == 65) %>%
#   group_by(HemorrhageLevel, OcclusionGroup) %>%
#   shapiro_test(Avg)

# --------------------------------------------------------
# Mixed effects model (LMM) setup
library(lme4)

# Ensure factors
renal_60_65$HemorrhageLevel <- as.factor(renal_60_65$HemorrhageLevel)
renal_60_65$Gender <- as.factor(renal_60_65$Gender)

# Linear mixed effects model
model_60_65 <- lmer(
  Avg ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg +
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID),
  data = renal_60_65
)
summary(model_60_65)

# Estimated marginal means and Tukey-adjusted pairwise comparisons
emm_60_65 <- emmeans(model_60_65, ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg)
tukey_60_65 <- pairs(emm_60_65, adjust = "tukey")
as.data.frame(tukey_60_65)

# # Experimental DO NOT USE YET
# 
# # Estimated marginal means by TimePoint, HemorrhageLevel, and OcclusionGroup with cofactors:
# emm_60_65 <- emmeans(model_60_65, ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg)
# 
# # 1) Pairwise comparisons within each TimePoint (comparing HemorrhageLevel:OcclusionGroup)
# pairs_within_timepoint <- pairs(emm_60_65, by = "TimePoint", adjust = "tukey")
# print(as.data.frame(pairs_within_timepoint))
# 
# # 2) Pairwise comparisons within each HemorrhageLevel (comparing TimePoint:OcclusionGroup)
# pairs_within_hemorrhage <- pairs(emm_60_65, by = "HemorrhageLevel", adjust = "tukey")
# print(as.data.frame(pairs_within_hemorrhage))
# 
# # 3) Pairwise comparisons within each OcclusionGroup (comparing TimePoint:HemorrhageLevel)
# pairs_within_occlusion <- pairs(emm_60_65, by = "OcclusionGroup", adjust = "tukey")
# print(as.data.frame(pairs_within_occlusion))

# --------------------------------------------------------
# Normality check for LMER residuals
shapiro.test(residuals(model_60_65))  # Shapiro-Wilk test

# --------------------------------------------------------
# Transformation attempts

# Log transform
renal_60_65 <- renal_60_65 %>% mutate(Avg_log = log(Avg + 1))
model_60_65_log <- lmer(
  Avg_log ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg +
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID),
  data = renal_60_65
)
shapiro.test(residuals(model_60_65_log))

# Square root transform
renal_60_65 <- renal_60_65 %>% mutate(Avg_sqrt = sqrt(Avg))
model_60_65_sqrt <- lmer(
  Avg_sqrt ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg +
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID),
  data = renal_60_65
)
shapiro.test(residuals(model_60_65_sqrt))

# Cube root transform
renal_60_65 <- renal_60_65 %>% mutate(Avg_cbrt = sign(Avg) * abs(Avg)^(1/3))
model_60_65_cbrt <- lmer(
  Avg_cbrt ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg +
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID),
  data = renal_60_65
)
shapiro.test(residuals(model_60_65_cbrt))

# Reciprocal transform
renal_60_65 <- renal_60_65 %>% mutate(Avg_recip = ifelse(Avg == 0, NA, 1 / Avg))
model_60_65_recip <- lmer(
  Avg_recip ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg +
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID),
  data = renal_60_65
)
shapiro.test(residuals(model_60_65_recip))

# --------------------------------------------------------
# Non-parametric method — Aligned Rank Transform (ART)

# Fit ART model with 3-way interaction
art_model <- art(Avg ~ TimePoint * HemorrhageLevel * OcclusionGroup, data = renal_60_65)

# Create ART-compatible model for EMMs
art_emm_model <- artlm(art_model, "TimePoint:HemorrhageLevel:OcclusionGroup")

# Compute estimated marginal means
emm <- emmeans(art_emm_model, ~ TimePoint * HemorrhageLevel * OcclusionGroup)

# Pairwise comparisons with Tukey adjustment
pairs(emm, adjust = "tukey")



####################################################################################################################################################################################################
####################################################################################################################################################################################################
# 4. Compare change over ALL TimePoints across HemorrhageLevel AND OcclusionGroup

# --------------------------------------------------------
# Linear mixed effects model across all timepoints
model_all_renal <- lmer(
  Avg ~ TimePoint * HemorrhageLevel * OcclusionGroup + 
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID), 
  data = renal_flow
)
summary(model_all_renal)

# Estimated marginal means and Tukey-adjusted comparisons
emm_all_renal <- emmeans(model_all_renal, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_all_renal <- pairs(emm_all_renal, adjust = "tukey")
as.data.frame(tukey_all_renal)

# --------------------------------------------------------
# Normality check of residuals
shapiro.test(residuals(model_all_renal))  # Shapiro-Wilk test

# --------------------------------------------------------
# Transformation attempts

# Log transform (+1 to avoid log(0))
renal_flow <- renal_flow %>% mutate(Avg_log = log(Avg + 1))
model_all_renal_log <- lmer(
  Avg_log ~ TimePoint * HemorrhageLevel * OcclusionGroup + 
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID), 
  data = renal_flow
)
shapiro.test(residuals(model_all_renal_log))

# Square root transform
renal_flow <- renal_flow %>% mutate(Avg_sqrt = sqrt(Avg))
model_all_renal_sqrt <- lmer(
  Avg_sqrt ~ TimePoint * HemorrhageLevel * OcclusionGroup + 
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID), 
  data = renal_flow
)
shapiro.test(residuals(model_all_renal_sqrt))

# Cube root transform
renal_flow <- renal_flow %>% mutate(Avg_cbrt = sign(Avg) * abs(Avg)^(1/3))
model_all_renal_cbrt <- lmer(
  Avg_cbrt ~ TimePoint * HemorrhageLevel * OcclusionGroup + 
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID), 
  data = renal_flow
)
shapiro.test(residuals(model_all_renal_cbrt))

# Reciprocal transform (handle zeros carefully)
renal_flow <- renal_flow %>% mutate(Avg_recip = ifelse(Avg == 0, NA, 1 / Avg))
model_all_renal_recip <- lmer(
  Avg_recip ~ TimePoint * HemorrhageLevel * OcclusionGroup + 
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID), 
  data = renal_flow
)
shapiro.test(residuals(model_all_renal_recip))

# --------------------------------------------------------
# Non-parametric method — Aligned Rank Transform (ART)

# Fit ART model with full interaction
art_model_all <- art(Avg ~ TimePoint * HemorrhageLevel * OcclusionGroup, data = renal_flow)

# ART-compatible model for highest-order interaction
art_emm_model_all <- artlm(art_model_all, "TimePoint:HemorrhageLevel:OcclusionGroup")

# Estimated marginal means and Tukey comparisons for ART model
emm_art_all <- emmeans(art_emm_model_all, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
pairs(emm_art_all, adjust = "tukey")

####################################################################################################################################################################################################
####################################################################################################################################################################################################


                      # ============================
                      # CAROTID FLOW ANALYSIS
                      # ============================

####################################################################################################################################################################################################
####################################################################################################################################################################################################
# 1. Compare Avg across Hemorrhage Groups at TimePoint = 30

# Filter data at TimePoint 30
carotid_30 <- carotid_flow %>% filter(TimePoint == 30)

# Test the normality of the data before running the test (for sanity)
carotid_30 %>%
  group_by(HemorrhageLevel) %>%
  shapiro_test(Avg)

# Ensure HemorrhageLevel and Gender are factors
carotid_30$HemorrhageLevel <- as.factor(carotid_30$HemorrhageLevel)
carotid_30$Gender <- as.factor(carotid_30$Gender)

# Generate the actual ANOVA
anova_30 <- aov(Avg ~ HemorrhageLevel + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = carotid_30)
summary(anova_30)

# Tukey Post-Hoc Analysis
tukey_30 <- TukeyHSD(anova_30)
print(tukey_30)

# Normality check for ANOVA residuals at TimePoint 30
shapiro.test(residuals(anova_30))  # Shapiro-Wilk test

# Effect sizes with 95% CI
eta_squared(anova_30)

# --------------------------------------------------------
# NO TRANSFORMATION LED TO NORMAL RESIDUALS — ORIGINAL DATA KEPT AS IS
# The following transformations were tested and did not help:

# Log transform
# carotid_30 <- carotid_30 %>% mutate(Avg_log = log(Avg + 1))  # +1 to avoid log(0)
# anova_30_log <- aov(Avg_log ~ HemorrhageLevel + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = carotid_30)
# shapiro.test(residuals(anova_30_log))

# Square root transform
# carotid_30 <- carotid_30 %>% mutate(Avg_sqrt = sqrt(Avg))
# anova_30_sqrt <- aov(Avg_sqrt ~ HemorrhageLevel + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = carotid_30)
# shapiro.test(residuals(anova_30_sqrt))

# Cube root transform
# carotid_30 <- carotid_30 %>% mutate(Avg_cbrt = sign(Avg) * abs(Avg)^(1/3))
# anova_30_cbrt <- aov(Avg_cbrt ~ HemorrhageLevel + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = carotid_30)
# shapiro.test(residuals(anova_30_cbrt))

# Reciprocal transform (handle zeros with care)
# carotid_30 <- carotid_30 %>% mutate(Avg_recip = ifelse(Avg == 0, NA, 1 / Avg))
# anova_30_recip <- aov(Avg_recip ~ HemorrhageLevel + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = carotid_30)
# shapiro.test(residuals(anova_30_recip))

# --------------------------------------------------------
# NON-PARAMETRIC ALTERNATIVE METHOD — Aligned Rank Transform

# Fit the ART model
art_model <- art(Avg ~ HemorrhageLevel, data = carotid_30)

# Use artlm() to create the model for EMMs
art_emm_model <- artlm(art_model, "HemorrhageLevel")

# Estimated marginal means
emm <- emmeans(art_emm_model, ~ HemorrhageLevel)

# Pairwise comparisons with Tukey adjustment
pairs(emm, adjust = "tukey")

####################################################################################################################################################################################################
####################################################################################################################################################################################################
# 2. Compare Avg across HemorrhageGroup AND OcclusionGroup at TimePoint = 60

# Filter data at TimePoint 60
carotid_60 <- carotid_flow %>% filter(TimePoint == 60)

# --------------------------------------------------------
# Optional: Normality test by group (commented out for now)
# carotid_60 %>%
#   group_by(HemorrhageLevel, OcclusionGroup) %>%
#   shapiro_test(Avg)

# Ensure factor conversion
carotid_60$HemorrhageLevel <- as.factor(carotid_60$HemorrhageLevel)
carotid_60$Gender <- as.factor(carotid_60$Gender)

# Run ANOVA with interaction and covariates
anova_60 <- aov(Avg ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg + 
                  CumulativeVasopressin + Plasmalyte + Norepi, data = carotid_60)
summary(anova_60)

# Tukey HSD post-hoc tests
TukeyHSD(anova_60, "OcclusionGroup")
TukeyHSD(anova_60, "HemorrhageLevel")
TukeyHSD(anova_60, "HemorrhageLevel:OcclusionGroup")

# --------------------------------------------------------
# Optional estimated marginal means with pairwise comparisons (commented)
# library(emmeans)
# emm_60 <- emmeans(anova_60, ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg)
# tukey_60 <- pairs(emm_60, adjust = "tukey")
# as.data.frame(tukey_60)

# --------------------------------------------------------
# Normality check for ANOVA residuals
shapiro.test(residuals(anova_60))  # Shapiro-Wilk test

# --------------------------------------------------------
# Transformation attempts

# Log transform
carotid_60 <- carotid_60 %>% mutate(Avg_log = log(Avg + 1))  # +1 to avoid log(0)
anova_60_log <- aov(Avg_log ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg + 
                      CumulativeVasopressin + Plasmalyte + Norepi, data = carotid_60)
shapiro.test(residuals(anova_60_log))

# Square root transform
carotid_60 <- carotid_60 %>% mutate(Avg_sqrt = sqrt(Avg))
anova_60_sqrt <- aov(Avg_sqrt ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg + 
                       CumulativeVasopressin + Plasmalyte + Norepi, data = carotid_60)
shapiro.test(residuals(anova_60_sqrt))

# Cube root transform
carotid_60 <- carotid_60 %>% mutate(Avg_cbrt = sign(Avg) * abs(Avg)^(1/3))
anova_60_cbrt <- aov(Avg_cbrt ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg + 
                       CumulativeVasopressin + Plasmalyte + Norepi, data = carotid_60)
shapiro.test(residuals(anova_60_cbrt))

# Reciprocal transform (handle zeros with care)
carotid_60 <- carotid_60 %>% mutate(Avg_recip = ifelse(Avg == 0, NA, 1 / Avg))
anova_60_recip <- aov(Avg_recip ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg + 
                        CumulativeVasopressin + Plasmalyte + Norepi, data = carotid_60)
shapiro.test(residuals(anova_60_recip))

# --------------------------------------------------------
# NON-PARAMETRIC METHODS — Aligned Rank Transform

# Initial failed attempt including covariates (commented)
# art_model <- art(Avg ~ (HemorrhageLevel + OcclusionGroup + Gender + Weightkg + 
#                         CumulativeVasopressin + Plasmalyte + Norepi)^3, data = carotid_60)

# Alternative model (commented out because it has too many factors)
# art_model <- art(Avg ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg + 
#                  CumulativeVasopressin + Plasmalyte + Norepi, data = carotid_60)
# anova(art_model)

# Final ART model: HemorrhageLevel * OcclusionGroup only
art_model <- art(Avg ~ HemorrhageLevel * OcclusionGroup, data = carotid_60)

# Create the model for estimated marginal means
art_emm_model <- artlm(art_model, "HemorrhageLevel:OcclusionGroup")

# Estimated marginal means
emm <- emmeans(art_emm_model, ~ HemorrhageLevel * OcclusionGroup)

# Pairwise comparisons with Tukey adjustment
pairs(emm, adjust = "tukey")

####################################################################################################################################################################################################
####################################################################################################################################################################################################
# 3. Compare the change from TimePoint 60 to 65 across HemorrhageGroup AND OcclusionGroup

# Filter data for TimePoints 60 and 65
carotid_60_65 <- carotid_flow %>% filter(TimePoint %in% c(60, 65))

# --------------------------------------------------------
# Optional: Normality tests by group at each time point (commented)
# carotid_flow %>%
#   filter(TimePoint == 60) %>%
#   group_by(HemorrhageLevel, OcclusionGroup) %>%
#   shapiro_test(Avg)
#
# carotid_flow %>%
#   filter(TimePoint == 65) %>%
#   group_by(HemorrhageLevel, OcclusionGroup) %>%
#   shapiro_test(Avg)

# --------------------------------------------------------
# Mixed effects model (LMM) setup
library(lme4)

# Ensure factors
carotid_60_65$HemorrhageLevel <- as.factor(carotid_60_65$HemorrhageLevel)
carotid_60_65$Gender <- as.factor(carotid_60_65$Gender)

# Linear mixed effects model
model_60_65 <- lmer(
  Avg ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg +
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID),
  data = carotid_60_65
)
summary(model_60_65)

# Estimated marginal means and Tukey-adjusted pairwise comparisons
emm_60_65 <- emmeans(model_60_65, ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg)
tukey_60_65 <- pairs(emm_60_65, adjust = "tukey")
as.data.frame(tukey_60_65)

# # Experimental DO NOT USE YET
# 
# # Estimated marginal means by TimePoint, HemorrhageLevel, and OcclusionGroup with cofactors:
# emm_60_65 <- emmeans(model_60_65, ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg)
# 
# # 1) Pairwise comparisons within each TimePoint (comparing HemorrhageLevel:OcclusionGroup)
# pairs_within_timepoint <- pairs(emm_60_65, by = "TimePoint", adjust = "tukey")
# print(as.data.frame(pairs_within_timepoint))
# 
# # 2) Pairwise comparisons within each HemorrhageLevel (comparing TimePoint:OcclusionGroup)
# pairs_within_hemorrhage <- pairs(emm_60_65, by = "HemorrhageLevel", adjust = "tukey")
# print(as.data.frame(pairs_within_hemorrhage))
# 
# # 3) Pairwise comparisons within each OcclusionGroup (comparing TimePoint:HemorrhageLevel)
# pairs_within_occlusion <- pairs(emm_60_65, by = "OcclusionGroup", adjust = "tukey")
# print(as.data.frame(pairs_within_occlusion))

# --------------------------------------------------------
# Normality check for LMER residuals
shapiro.test(residuals(model_60_65))  # Shapiro-Wilk test

# --------------------------------------------------------
# Transformation attempts

# Log transform
carotid_60_65 <- carotid_60_65 %>% mutate(Avg_log = log(Avg + 1))
model_60_65_log <- lmer(
  Avg_log ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg +
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID),
  data = carotid_60_65
)
shapiro.test(residuals(model_60_65_log))

# Square root transform
carotid_60_65 <- carotid_60_65 %>% mutate(Avg_sqrt = sqrt(Avg))
model_60_65_sqrt <- lmer(
  Avg_sqrt ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg +
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID),
  data = carotid_60_65
)
shapiro.test(residuals(model_60_65_sqrt))

# Cube root transform
carotid_60_65 <- carotid_60_65 %>% mutate(Avg_cbrt = sign(Avg) * abs(Avg)^(1/3))
model_60_65_cbrt <- lmer(
  Avg_cbrt ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg +
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID),
  data = carotid_60_65
)
shapiro.test(residuals(model_60_65_cbrt))

# Reciprocal transform
carotid_60_65 <- carotid_60_65 %>% mutate(Avg_recip = ifelse(Avg == 0, NA, 1 / Avg))
model_60_65_recip <- lmer(
  Avg_recip ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg +
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID),
  data = carotid_60_65
)
shapiro.test(residuals(model_60_65_recip))

# --------------------------------------------------------
# Non-parametric method — Aligned Rank Transform (ART)

# Fit ART model with 3-way interaction
art_model <- art(Avg ~ TimePoint * HemorrhageLevel * OcclusionGroup, data = carotid_60_65)

# Create ART-compatible model for EMMs
art_emm_model <- artlm(art_model, "TimePoint:HemorrhageLevel:OcclusionGroup")

# Compute estimated marginal means
emm <- emmeans(art_emm_model, ~ TimePoint + HemorrhageLevel + OcclusionGroup)

# Pairwise comparisons with Tukey adjustment
pairs(emm, adjust = "tukey")



####################################################################################################################################################################################################
####################################################################################################################################################################################################
# 4. Compare change over ALL TimePoints across HemorrhageLevel AND OcclusionGroup

# --------------------------------------------------------
# Linear mixed effects model across all timepoints
model_all_carotid <- lmer(
  Avg ~ TimePoint * HemorrhageLevel * OcclusionGroup + 
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID), 
  data = carotid_flow
)
summary(model_all_carotid)

# Estimated marginal means and Tukey-adjusted comparisons
emm_all_carotid <- emmeans(model_all_carotid, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_all_carotid <- pairs(emm_all_carotid, adjust = "tukey")
as.data.frame(tukey_all_carotid)

# --------------------------------------------------------
# Normality check of residuals
shapiro.test(residuals(model_all_carotid))  # Shapiro-Wilk test

# --------------------------------------------------------
# Transformation attempts

# Log transform (+1 to avoid log(0))
carotid_flow <- carotid_flow %>% mutate(Avg_log = log(Avg + 1))
model_all_carotid_log <- lmer(
  Avg_log ~ TimePoint * HemorrhageLevel * OcclusionGroup + 
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID), 
  data = carotid_flow
)
shapiro.test(residuals(model_all_carotid_log))

# Square root transform
carotid_flow <- carotid_flow %>% mutate(Avg_sqrt = sqrt(Avg))
model_all_carotid_sqrt <- lmer(
  Avg_sqrt ~ TimePoint * HemorrhageLevel * OcclusionGroup + 
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID), 
  data = carotid_flow
)
shapiro.test(residuals(model_all_carotid_sqrt))

# Cube root transform
carotid_flow <- carotid_flow %>% mutate(Avg_cbrt = sign(Avg) * abs(Avg)^(1/3))
model_all_carotid_cbrt <- lmer(
  Avg_cbrt ~ TimePoint * HemorrhageLevel * OcclusionGroup + 
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID), 
  data = carotid_flow
)
shapiro.test(residuals(model_all_carotid_cbrt))

# Reciprocal transform (handle zeros carefully)
carotid_flow <- carotid_flow %>% mutate(Avg_recip = ifelse(Avg == 0, NA, 1 / Avg))
model_all_carotid_recip <- lmer(
  Avg_recip ~ TimePoint * HemorrhageLevel * OcclusionGroup + 
    CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID), 
  data = carotid_flow
)
shapiro.test(residuals(model_all_carotid_recip))

# --------------------------------------------------------
# Non-parametric method — Aligned Rank Transform (ART)

# Fit ART model with full interaction
art_model_all <- art(Avg ~ TimePoint * HemorrhageLevel * OcclusionGroup, data = carotid_flow)

# ART-compatible model for highest-order interaction
art_emm_model_all <- artlm(art_model_all, "TimePoint:HemorrhageLevel:OcclusionGroup")

# Estimated marginal means and Tukey comparisons for ART model
emm_art_all <- emmeans(art_emm_model_all, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
pairs(emm_art_all, adjust = "tukey")

