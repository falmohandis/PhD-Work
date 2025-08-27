# Load necessary libraries
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
                     "CumulativeVasopressin",	"Plasmalyte",	"Norepi"
                    )

# Define common timepoints
# selected_timepoints <- c(0, 30, 60,61, 65, 75, 85, 120, 180, 240)

selected_timepoints <- c(0,
                         # 6,
                         # 11,
                         # 16,
                         # 21,
                         # 26,
                         30,
                         31,
                         # 36,
                         # 41,
                         # 46,
                         # 51,
                         # 56,
                         60,
                         61,
                         65,
                         66,
                         71,
                         75,
                         76,
                         81,
                         85,
                         86,
                         # 91,
                         # 121,
                         # 181,
                         240)

# Function to filter and prepare data by ProbeLocation
prepare_flow_data <- function(df, location) {
  df %>%
    filter(ProbeLocation == location, TimePoint %in% selected_timepoints) %>%
    select(all_of(columns_to_keep)) %>%
    mutate(
      TimePoint = as.factor(TimePoint),
      HemorrhageLevel = as.factor(HemorrhageLevel),
      OcclusionGroup = as.factor(OcclusionGroup),
      SubjectID = as.factor(SubjectID)
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




# ============================
# RENAL FLOW ANALYSIS
# ============================


# 1. Compare Avg across Hemorrhage Groups at TimePoint = 30
renal_30 <- renal_flow %>% filter(TimePoint == 30)


# # Test the normality of the data before even running the test (not necessary, but for sanity)
# renal_30 %>%
#   group_by(HemorrhageLevel) %>%
#   shapiro_test(Avg)



renal_30$HemorrhageLevel <- as.factor(renal_30$HemorrhageLevel)
renal_30$Gender <- as.factor(renal_30$Gender)

anova_30 <- aov(Avg ~ HemorrhageLevel+ Gender + Weightkg+CumulativeVasopressin+Plasmalyte+ Norepi, data = renal_30)
summary(anova_30)


tukey_30 <- TukeyHSD(anova_30)
print(tukey_30)


# NO TRANSFORMATION LED TO NORMAL RESIDUALS SO I WILL KEEP THE ORIGINALS AS IS

# Normality check for ANOVA residuals at TimePoint 30
shapiro.test(residuals(anova_30))  # Shapiro-Wilk test
#
# renal_30 <- renal_30 %>% mutate(Avg_log = log(Avg + 1))  # +1 to avoid log(0)
#
# anova_30_log <- aov(Avg_log ~ HemorrhageLevel + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = renal_30)
# shapiro.test(residuals(anova_30_log))
#
#
# # Square root transform
# renal_30 <- renal_30 %>% mutate(Avg_sqrt = sqrt(Avg))
# anova_30_sqrt <- aov(Avg_sqrt ~ HemorrhageLevel + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = renal_30)
# shapiro.test(residuals(anova_30_sqrt))
#
# # Cube root transform
# renal_30 <- renal_30 %>% mutate(Avg_cbrt = sign(Avg) * abs(Avg)^(1/3))
# anova_30_cbrt <- aov(Avg_cbrt ~ HemorrhageLevel + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = renal_30)
# shapiro.test(residuals(anova_30_cbrt))
#
# # Reciprocal transform (handle zeros with care)
# renal_30 <- renal_30 %>% mutate(Avg_recip = ifelse(Avg == 0, NA, 1 / Avg))
# anova_30_recip <- aov(Avg_recip ~ HemorrhageLevel + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = renal_30)
# shapiro.test(residuals(anova_30_recip))

# NON PARAMETRIC METHODS
# library(ARTool)
#
# # Non-parametric version of your ANOVA
# art_model <- art(Avg ~ HemorrhageLevel + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = renal_30)
#
# # Run ANOVA on aligned ranks
# anova(art_model)




# 2. Compare Avg across HemorrhageGroup AND OcclusionGroup at TimePoint = 60
renal_60 <- renal_flow %>% filter(TimePoint == 60)



# # Test the normality of the data before even running the test (not necessary, but for sanity)
# renal_60 %>%
#   group_by(HemorrhageLevel, OcclusionGroup) %>%
#   shapiro_test(Avg)

renal_60$HemorrhageLevel <- as.factor(renal_60$HemorrhageLevel)
renal_60$Gender <- as.factor(renal_60$Gender)

anova_60 <- aov(Avg ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg+CumulativeVasopressin+Plasmalyte+ Norepi, data = renal_60)
summary(anova_60)

TukeyHSD(anova_60, "OcclusionGroup")
TukeyHSD(anova_60, "HemorrhageLevel")
TukeyHSD(anova_60, "HemorrhageLevel:OcclusionGroup")


# library(emmeans)
# emm_60 <- emmeans(anova_60, ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg)
# tukey_60 <- pairs(emm_60, adjust = "tukey")
# as.data.frame(tukey_60)




# Normality check for ANOVA residuals at TimePoint 60
shapiro.test(residuals(anova_60))  # Shapiro-Wilk test
#
renal_60 <- renal_60 %>% mutate(Avg_log = log(Avg + 1))  # +1 to avoid log(0)

anova_60_log <- aov(Avg_log ~ HemorrhageLevel*OcclusionGroup + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = renal_60)
shapiro.test(residuals(anova_60_log))


# Square root transform
renal_60 <- renal_60 %>% mutate(Avg_sqrt = sqrt(Avg))
anova_60_sqrt <- aov(Avg_sqrt ~ HemorrhageLevel*OcclusionGroup + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = renal_60)
shapiro.test(residuals(anova_60_sqrt))

# Cube root transform
renal_60 <- renal_60 %>% mutate(Avg_cbrt = sign(Avg) * abs(Avg)^(1/3))
anova_60_cbrt <- aov(Avg_cbrt ~ HemorrhageLevel*OcclusionGroup + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = renal_60)
shapiro.test(residuals(anova_60_cbrt))

# Reciprocal transform (handle zeros with care)
renal_60 <- renal_60 %>% mutate(Avg_recip = ifelse(Avg == 0, NA, 1 / Avg))
anova_60_recip <- aov(Avg_recip ~ HemorrhageLevel*OcclusionGroup + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = renal_60)
shapiro.test(residuals(anova_60_recip))

# NON PARAMETRIC METHODS
# library(ARTool)
#
# # Non-parametric version of your ANOVA
# art_model <- art(Avg ~ HemorrhageLevel*OcclusionGroup + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = renal_60)
#
# # Run ANOVA on aligned ranks
# anova(art_model)





# 3. Compare the change from TimePoint 60 to 65 across HemorrhageGroup AND OcclusionGroup
renal_60_65 <- renal_flow %>% filter(TimePoint %in% c(60, 65))


# # Test the normality of the data before even running the test (not necessary, but for sanity)
# renal_flow %>%
#   filter(TimePoint == 60) %>%
#   group_by(HemorrhageLevel, OcclusionGroup) %>%
#   shapiro_test(Avg)
# 
# renal_flow %>%
#   filter(TimePoint == 65) %>%
#   group_by(HemorrhageLevel, OcclusionGroup) %>%
#   shapiro_test(Avg)


library(lme4)
renal_60_65$HemorrhageLevel <- as.factor(renal_60_65$HemorrhageLevel)
renal_60_65$Gender <- as.factor(renal_60_65$Gender)

model_60_65 <- lmer(Avg ~ TimePoint * HemorrhageLevel * OcclusionGroup  + Gender + Weightkg +CumulativeVasopressin+Plasmalyte+ Norepi+(1 | SubjectID), data = renal_60_65)
summary(model_60_65)

emm_60_65 <- emmeans(model_60_65, ~ TimePoint * HemorrhageLevel * OcclusionGroup  + Gender + Weightkg)
tukey_60_65 <- pairs(emm_60_65, adjust = "tukey")
as.data.frame(tukey_60_65)



# 4. Compare change over ALL TimePoints across HemorrhageGroup AND OcclusionGroup
model_all_renal <- lmer(Avg ~ TimePoint * HemorrhageLevel * OcclusionGroup +CumulativeVasopressin+Plasmalyte+ Norepi+ (1 | SubjectID), data = renal_flow)
summary(model_all_renal)

emm_all_renal <- emmeans(model_all_renal, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_all_renal <- pairs(emm_all_renal, adjust = "tukey")
as.data.frame(tukey_all_renal)




# ============================
# CAROTID FLOW ANALYSIS
# ============================

# 1. Compare Avg across Hemorrhage Groups at TimePoint = 30
carotid_30 <- carotid_flow %>% filter(TimePoint == 30)


# Test the normality of the data before even running the test (not necessary, but for sanity)
carotid_30 %>%
  group_by(HemorrhageLevel) %>%
  shapiro_test(Avg)


carotid_30$HemorrhageLevel <- as.factor(carotid_30$HemorrhageLevel)
carotid_30$Gender <- as.factor(carotid_30$Gender)

anova_30 <- aov(Avg ~ HemorrhageLevel+ Gender + Weightkg+CumulativeVasopressin+Plasmalyte+ Norepi, data = carotid_30)
summary(anova_30)


tukey_30 <- TukeyHSD(anova_30)
print(tukey_30)



# Normality check for ANOVA residuals at TimePoint 30
shapiro.test(residuals(anova_30))  # Shapiro-Wilk test

carotid_30 <- carotid_30 %>% mutate(Avg_log = log(Avg + 1))  # +1 to avoid log(0)

anova_30_log <- aov(Avg_log ~ HemorrhageLevel + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = carotid_30)
shapiro.test(residuals(anova_30_log))


# 2. Compare Avg across HemorrhageGroup AND OcclusionGroup at TimePoint = 60
carotid_60 <- carotid_flow %>% filter(TimePoint == 60)


# 
# # Test the normality of the data before even running the test (not necessary, but for sanity)
# carotid_60 %>%
#   group_by(HemorrhageLevel,OcclusionGroup) %>%
#   shapiro_test(Avg)


carotid_60$HemorrhageLevel <- as.factor(carotid_60$HemorrhageLevel)
carotid_60$Gender <- as.factor(carotid_60$Gender)

anova_60 <- aov(Avg ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg+CumulativeVasopressin+Plasmalyte+ Norepi, data = carotid_60)
summary(anova_60)

TukeyHSD(anova_60, "OcclusionGroup")
TukeyHSD(anova_60, "HemorrhageLevel")
TukeyHSD(anova_60, "HemorrhageLevel:OcclusionGroup")


# library(emmeans)
# emm_60 <- emmeans(anova_60, ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg)
# tukey_60 <- pairs(emm_60, adjust = "tukey")
# as.data.frame(tukey_60)

# Put the tests of the normality in the residuals here!!!*******
# Normality check for ANOVA residuals at TimePoint 30
shapiro.test(residuals(anova_30))  # Shapiro-Wilk test

# carotid_60 <- carotid_60 %>% mutate(Avg_log = log(Avg + 1))  # +1 to avoid log(0)
# 
# anova_60_log <- aov(Avg_log ~ HemorrhageLevel + Gender + Weightkg + CumulativeVasopressin + Plasmalyte + Norepi, data = carotid_30)
# shapiro.test(residuals(anova_60_log))

# 3. Compare the change from TimePoint 60 to 65 across HemorrhageGroup AND OcclusionGroup
carotid_60_65 <- carotid_flow %>% filter(TimePoint %in% c(60, 65))

# 
# # Test the normality of the data before even running the test (not necessary, but for sanity)
# carotid_flow %>%
#   filter(TimePoint == 60) %>%
#   group_by(HemorrhageLevel, OcclusionGroup) %>%
#   shapiro_test(Avg)
# 
# carotid_flow %>%
#   filter(TimePoint == 65) %>%
#   group_by(HemorrhageLevel, OcclusionGroup) %>%
#   shapiro_test(Avg)


library(lme4)
carotid_60_65$HemorrhageLevel <- as.factor(carotid_60_65$HemorrhageLevel)
carotid_60_65$Gender <- as.factor(carotid_60_65$Gender)

model_60_65 <- lmer(Avg ~ TimePoint * HemorrhageLevel * OcclusionGroup  + Gender + Weightkg +CumulativeVasopressin+Plasmalyte+ Norepi +(1 | SubjectID), data = carotid_60_65)
summary(model_60_65)

emm_60_65 <- emmeans(model_60_65, ~ TimePoint * HemorrhageLevel * OcclusionGroup  + Gender + Weightkg)
tukey_60_65 <- pairs(emm_60_65, adjust = "tukey")
as.data.frame(tukey_60_65)

shapiro.test(residuals(model_60_65))

# Normality check for LMER residuals at TimePoint 60–65
shapiro.test(residuals(model_60_65))  # Shapiro-Wilk test

# Log transform
carotid_60_65 <- carotid_60_65 %>% mutate(Avg_log = log(Avg + 1))  # +1 to avoid log(0)
model_60_65_log <- lmer(Avg_log ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg + 
                          CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID),
                        data = carotid_60_65)
shapiro.test(residuals(model_60_65_log))

# Square root transform
carotid_60_65 <- carotid_60_65 %>% mutate(Avg_sqrt = sqrt(Avg))
model_60_65_sqrt <- lmer(Avg_sqrt ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg + 
                           CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID),
                         data = carotid_60_65)
shapiro.test(residuals(model_60_65_sqrt))

# Cube root transform
carotid_60_65 <- carotid_60_65 %>% mutate(Avg_cbrt = sign(Avg) * abs(Avg)^(1/3))
model_60_65_cbrt <- lmer(Avg_cbrt ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg + 
                           CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID),
                         data = carotid_60_65)
shapiro.test(residuals(model_60_65_cbrt))

# Reciprocal transform (handle zeros with care)
carotid_60_65 <- carotid_60_65 %>% mutate(Avg_recip = ifelse(Avg == 0, NA, 1 / Avg))
model_60_65_recip <- lmer(Avg_recip ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg + 
                            CumulativeVasopressin + Plasmalyte + Norepi + (1 | SubjectID),
                          data = carotid_60_65)
shapiro.test(residuals(model_60_65_recip))


# 4. Compare change over ALL TimePoints across HemorrhageGroup AND OcclusionGroup
model_all_carotid <- lmer(Avg ~ TimePoint * HemorrhageLevel * OcclusionGroup +CumulativeVasopressin+Plasmalyte+ Norepi+ (1 | SubjectID), data = carotid_flow)
summary(model_all_carotid)

emm_all_carotid <- emmeans(model_all_carotid, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_all_carotid <- pairs(emm_all_carotid, adjust = "tukey")
as.data.frame(tukey_all_carotid)
