# Load necessary libraries

library(lme4)
library(lmerTest)
library(broom.mixed)
library(emmeans)
library(dplyr)
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


# # Custom Code to Robustly get current script directory or fallback to working directory
# # I made this so that it saves the package automatically into your list of packages. 
# # You just have to run this once and then you are good.


# if ("rstudioapi" %in% rownames(installed.packages())) {
#   library(rstudioapi)
#   if (isAvailable()) {
#     script_dir <- dirname(getActiveDocumentContext()$path)
#   } else {
#     script_dir <- getwd()
#   }
# } else {
#   script_dir <- getwd()
# }


script_dir <- dirname(getActiveDocumentContext()$path)
file_path= "ERNE_TEG_cleaned.xlsx"

full_file_path = file.path(script_dir, file_path)


df = read_excel(full_file_path)


colnames(df) <- colnames(df) %>%
  str_replace_all("[ \\-\\(\\)]", "") %>%  
  make.names(unique = TRUE)  

df$ParentFolder <- as.factor(df$ParentFolder)
df$SubjectID <- as.factor(df$ParentFolder)
df$TimePoint <- as.numeric(df$TimePoint)
df$OcclusionGroup <- as.factor(df$OcclusionGroup)


head(df,4)


# Convert OcclusionGroup to a factor with levels in the correct order
df$OcclusionGroup <- factor(df$OcclusionGroup, 
                            levels = c("No Occlusion", "Partial Occlusion", "Full Occlusion"))





# 
# # Now convert it to numeric (0, 1, 2)
# df$OcclusionGroup <- as.numeric(df$OcclusionGroup) - 1  # to make it 0, 1, 2 instead of 1, 2, 3
# 
# # Convert Hemorrhage Level to values of 0, 1, 2
# df$HemorrhageLevel <- (df$HemorrhageLevel / 10) - 1
# df$HemorrhageLevel <- as.factor(df$HemorrhageLevel)


# Convert OcclusionGroup to a factor with levels in the correct order and numeric codes
df$OcclusionGroup <- factor(df$OcclusionGroup, 
                            levels = c("No Occlusion", "Partial Occlusion", "Full Occlusion"))

# Convert to numeric (0, 1, 2)
df$OcclusionGroup <- as.numeric(df$OcclusionGroup) - 1

# Label as factor with desired labels
df$OcclusionGroup <- factor(df$OcclusionGroup, levels = 0:2, labels = c("None", "Partial", "Full"))

# Convert HemorrhageLevel from 10, 20, 30 to 0, 1, 2
df$HemorrhageLevel <- (df$HemorrhageLevel / 10) - 1

# Then relabel as factor
df$HemorrhageLevel <- factor(df$HemorrhageLevel, levels = 0:2, labels = c("10", "20", "30"))










columns_to_keep <- c(
  'ParentFolder',
  'TimePoint', 
  'HemorrhageLevel', 
  'OcclusionGroup', 
  'R_time', 
  'K_time', 
  'Angle', 
  'MA', 
  'G', 
  'LY30',
  'Gender',
  'Weightkg',
  'SubjectID'
)


df_filtered <- df[, columns_to_keep, drop = FALSE]


#
library(rstatix)  # ensure this is loaded for identify_outliers()
library(dplyr)
library(modeest)  # for mode calculation

# Function to compute mode safely (returns first if multiple modes)
get_mode <- function(x) {
  mfv <- mfv(x, method = "mfv", na.rm = TRUE)
  if (length(mfv) > 1) return(mfv[1]) else return(mfv)
}

# Enhanced reporting function
report_outliers <- function(data, var_name) {
  var_sym <- sym(var_name)
  
  # Summary stats
  stats <- data %>%
    summarise(
      Mean   = mean(!!var_sym, na.rm = TRUE),
      SD     = sd(!!var_sym, na.rm = TRUE),
      Median = median(!!var_sym, na.rm = TRUE),
      Mode   = get_mode(!!var_sym),
      Min    = min(!!var_sym, na.rm = TRUE),
      Max    = max(!!var_sym, na.rm = TRUE)
    )
  
  cat(paste0("\n==== Summary Statistics for ", var_name, " ====\n"))
  print(stats)
  
  # Identify and show outliers
  outliers <- data %>%
    group_by(TimePoint, HemorrhageLevel, OcclusionGroup) %>%
    identify_outliers(!!var_sym) %>%
    filter(is.outlier) %>%
    select(ParentFolder, TimePoint, HemorrhageLevel, OcclusionGroup, all_of(var_name)) %>%
    slice_head(n = 50)
  
  cat(paste0("\n==== Outliers in ", var_name, " (Top 50) ====\n"))
  print(outliers, n = 50)
}

# Run it for each variable
report_outliers(df_filtered, "R_time")
report_outliers(df_filtered, "K_time")
report_outliers(df_filtered, "Angle")
report_outliers(df_filtered, "MA")
report_outliers(df_filtered, "G")
report_outliers(df_filtered, "LY30")







# ------------------- LY30 -------------------
cat("\n===== LY30 =====\n")

anova_ly <- aov(LY30 ~ HemorrhageLevel * OcclusionGroup * TimePoint + Gender + Weightkg, data = df_filtered)
summary(anova_ly)

glmm_ly <- lmer(LY30 ~ HemorrhageLevel * OcclusionGroup * TimePoint + Gender + Weightkg + (1 | ParentFolder), data = df_filtered)
summary(glmm_ly)

emmeans_ly <- emmeans(glmm_ly, ~ HemorrhageLevel | OcclusionGroup)
pairs(emmeans_ly)

emmeans_ly <- emmeans(glmm_ly, ~ OcclusionGroup  | HemorrhageLevel)
pairs(emmeans_ly)

emmeans_ly <- emmeans(glmm_ly, ~ HemorrhageLevel * OcclusionGroup | TimePoint)
pairs(emmeans_ly)


cat("\n===== LY30 (analyzed per TimePoint) =====\n")

# Get unique time points
time_points <- unique(df_filtered$TimePoint)

# Loop through each time point
for (tp in time_points) {
  cat("\n=============================\n")
  cat(paste("TimePoint:", tp, "\n"))
  cat("=============================\n")
  
  # Subset data
  df_tp <- df_filtered %>% filter(TimePoint == tp)
  
  # Check observation and group counts
  n_obs <- nrow(df_tp)
  n_groups <- length(unique(df_tp$ParentFolder))
  
  # Choose model type based on data availability
  if (n_groups < n_obs) {
    cat("Using lmer (with random effect)\n")
    model_tp <- lmer(LY30 ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg + (1 | ParentFolder), data = df_tp)
  } else {
    cat("Using lm (no random effect)\n")
    model_tp <- lm(LY30 ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg, data = df_tp)
  }
  
  # Show model summary
  print(summary(model_tp))
  
  # Estimated marginal means and pairwise contrasts
  emmeans_tp <- emmeans(model_tp, ~ HemorrhageLevel * OcclusionGroup)
  pairwise_tp <- contrast(emmeans_tp, method = "pairwise")
  print(summary(pairwise_tp, infer = TRUE))
}



# Filter data to just TimePoint 0 and 240
df_compare <- df_filtered %>% filter(TimePoint %in% c(0, 240))

# Convert TimePoint to a factor for interaction modeling
df_compare$TimePoint <- factor(df_compare$TimePoint, levels = c(0, 240), labels = c("T0", "T240"))

# Fit the mixed model
model_compare <- lmer(
  LY30 ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg + (1 | ParentFolder),
  data = df_compare
)

# Summary of model
summary(model_compare)
# Compare TimePoint within each H × O group
emm_tp <- emmeans(model_compare, ~ TimePoint | HemorrhageLevel * OcclusionGroup)
pairs(emm_tp, adjust = "fdr")  # Compare T0 vs T240 per group

# HemorrhageLevel comparisons within each TimePoint × OcclusionGroup
emm_hemo <- emmeans(model_compare, ~ HemorrhageLevel | TimePoint * OcclusionGroup)
pairs(emm_hemo, adjust = "tukey")  # Tukey HSD for HemorrhageLevel

# OcclusionGroup comparisons within each TimePoint × HemorrhageLevel
emm_occ <- emmeans(model_compare, ~ OcclusionGroup | TimePoint * HemorrhageLevel)
pairs(emm_occ, adjust = "tukey")  # Tukey HSD for OcclusionGroup







# Filter to only T0 and T240
df_compare <- df_filtered %>% filter(TimePoint %in% c(0, 240))

# Convert TimePoint to factor for interaction modeling
df_compare$TimePoint <- factor(df_compare$TimePoint, levels = c(0, 240), labels = c("T0", "T240"))

# Fit the mixed model
model_compare <- lmer(
  LY30 ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg + (1 | ParentFolder),
  data = df_compare
)

# Compare TimePoint within each H × O group
emm_tp <- emmeans(model_compare, ~ TimePoint | HemorrhageLevel * OcclusionGroup)
pairs(emm_tp, adjust = "fdr")  # Compare T0 vs T240 per group

# HemorrhageLevel comparisons within each TimePoint × OcclusionGroup
emm_hemo <- emmeans(model_compare, ~ HemorrhageLevel | TimePoint * OcclusionGroup)
pairs(emm_hemo, adjust = "tukey")  # Tukey HSD for HemorrhageLevel

# OcclusionGroup comparisons within each TimePoint × HemorrhageLevel
emm_occ <- emmeans(model_compare, ~ OcclusionGroup | TimePoint * HemorrhageLevel)
pairs(emm_occ, adjust = "tukey")  # Tukey HSD for OcclusionGroup





####### COMPARING ALL TIME POINTS #############

# Get all time points (excluding 0)
all_tps <- sort(unique(df_filtered$TimePoint))
comparison_tps <- setdiff(all_tps, 0)

# Loop through each TimePoint and compare it with T0
for (tp in comparison_tps) {
  
  cat("\n============================================\n")
  cat(paste("⏱ Comparing TimePoint 0 vs", tp, "\n"))
  cat("============================================\n")
  
  # Filter to just T0 and current tp
  df_sub <- df_filtered %>% filter(TimePoint %in% c(0, tp))
  
  # Convert TimePoint to a factor (T0, Txxx)
  df_sub$TimePoint <- factor(df_sub$TimePoint, levels = c(0, tp), labels = c("T0", paste0("T", tp)))
  
  # Fit the mixed model
  model <- lmer(
    LY30 ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg + (1 | ParentFolder),
    data = df_sub
  )
  
  # Print model summary
  print(summary(model))
  
  # ==== TimePoint pairwise comparison ====
  cat("\n➡️  Comparing T0 vs T", tp, " per H × O combo\n")
  emm_tp <- emmeans(model, ~ TimePoint | HemorrhageLevel * OcclusionGroup)
  print(pairs(emm_tp, adjust = "fdr"))
  
  # ==== Tukey for HemorrhageLevel ====
  cat("\n🧪 Tukey HSD: HemorrhageLevel at TimePoint × OcclusionGroup\n")
  emm_hemo <- emmeans(model, ~ HemorrhageLevel | TimePoint * OcclusionGroup)
  print(pairs(emm_hemo, adjust = "tukey"))
  
  # ==== Tukey for OcclusionGroup ====
  cat("\n🧪 Tukey HSD: OcclusionGroup at TimePoint × HemorrhageLevel\n")
  emm_occ <- emmeans(model, ~ OcclusionGroup | TimePoint * HemorrhageLevel)
  print(pairs(emm_occ, adjust = "tukey"))
}













# ------------------- R_time -------------------
cat("\n===== R_time =====\n")

anova_ly <- aov(R_time ~ HemorrhageLevel * OcclusionGroup * TimePoint + Gender + Weightkg, data = df_filtered)
summary(anova_ly)

glmm_ly <- lmer(R_time ~ HemorrhageLevel * OcclusionGroup * TimePoint + Gender + Weightkg + (1 | ParentFolder), data = df_filtered)
summary(glmm_ly)

emmeans_ly <- emmeans(glmm_ly, ~ HemorrhageLevel | OcclusionGroup)
pairs(emmeans_ly)

emmeans_ly <- emmeans(glmm_ly, ~ OcclusionGroup  | HemorrhageLevel)
pairs(emmeans_ly)

emmeans_ly <- emmeans(glmm_ly, ~ HemorrhageLevel * OcclusionGroup | TimePoint)
pairs(emmeans_ly)


# Loop through each time point
for (tp in time_points) {
  cat("\n=============================\n")
  cat(paste("TimePoint:", tp, "\n"))
  cat("=============================\n")
  
  # Subset data
  df_tp <- df_filtered %>% filter(TimePoint == tp)
  
  # Check levels
  n_obs <- nrow(df_tp)
  n_groups <- length(unique(df_tp$ParentFolder))
  
  # Choose model based on whether random effect is valid
  if (n_groups < n_obs) {
    cat("Using lmer (with random effect)\n")
    model_tp <- lmer(R_time ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg + (1 | ParentFolder), data = df_tp)
  } else {
    cat("Using lm (no random effect)\n")
    model_tp <- lm(R_time ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg, data = df_tp)
  }
  
  print(summary(model_tp))
  
  # Estimated marginal means and contrasts
  emmeans_tp <- emmeans(model_tp, ~ HemorrhageLevel * OcclusionGroup)
  pairwise_tp <- contrast(emmeans_tp, method = "pairwise")
  print(summary(pairwise_tp, infer = TRUE))
}





# Filter to only T0 and T240
df_compare <- df_filtered %>% filter(TimePoint %in% c(0, 240))

# Convert TimePoint to factor for interaction modeling
df_compare$TimePoint <- factor(df_compare$TimePoint, levels = c(0, 240), labels = c("T0", "T240"))

# Fit the mixed model
model_compare <- lmer(
  R_time ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg + (1 | ParentFolder),
  data = df_compare
)

# Compare TimePoint within each H × O group
emm_tp <- emmeans(model_compare, ~ TimePoint | HemorrhageLevel * OcclusionGroup)
pairs(emm_tp, adjust = "fdr")  # Compare T0 vs T240 per group

# HemorrhageLevel comparisons within each TimePoint × OcclusionGroup
emm_hemo <- emmeans(model_compare, ~ HemorrhageLevel | TimePoint * OcclusionGroup)
pairs(emm_hemo, adjust = "tukey")  # Tukey HSD for HemorrhageLevel

# OcclusionGroup comparisons within each TimePoint × HemorrhageLevel
emm_occ <- emmeans(model_compare, ~ OcclusionGroup | TimePoint * HemorrhageLevel)
pairs(emm_occ, adjust = "tukey")  # Tukey HSD for OcclusionGroup





####### COMPARING ALL TIME POINTS #############

# Get all time points (excluding 0)
all_tps <- sort(unique(df_filtered$TimePoint))
comparison_tps <- setdiff(all_tps, 0)

# Loop through each TimePoint and compare it with T0
for (tp in comparison_tps) {
  
  cat("\n============================================\n")
  cat(paste("⏱ Comparing TimePoint 0 vs", tp, "\n"))
  cat("============================================\n")
  
  # Filter to just T0 and current tp
  df_sub <- df_filtered %>% filter(TimePoint %in% c(0, tp))
  
  # Convert TimePoint to a factor (T0, Txxx)
  df_sub$TimePoint <- factor(df_sub$TimePoint, levels = c(0, tp), labels = c("T0", paste0("T", tp)))
  
  # Fit the mixed model
  model <- lmer(
    R_time ~ TimePoint * HemorrhageLevel * OcclusionGroup + Gender + Weightkg + (1 | ParentFolder),
    data = df_sub
  )
  
  # Print model summary
  print(summary(model))
  
  # ==== TimePoint pairwise comparison ====
  cat("\n➡️  Comparing T0 vs T", tp, " per H × O combo\n")
  emm_tp <- emmeans(model, ~ TimePoint | HemorrhageLevel * OcclusionGroup)
  print(pairs(emm_tp, adjust = "fdr"))
  
  # ==== Tukey for HemorrhageLevel ====
  cat("\n🧪 Tukey HSD: HemorrhageLevel at TimePoint × OcclusionGroup\n")
  emm_hemo <- emmeans(model, ~ HemorrhageLevel | TimePoint * OcclusionGroup)
  print(pairs(emm_hemo, adjust = "tukey"))
  
  # ==== Tukey for OcclusionGroup ====
  cat("\n🧪 Tukey HSD: OcclusionGroup at TimePoint × HemorrhageLevel\n")
  emm_occ <- emmeans(model, ~ OcclusionGroup | TimePoint * HemorrhageLevel)
  print(pairs(emm_occ, adjust = "tukey"))
}













###################################################################

# Remainder of the code is in files V6, V7 and V8.