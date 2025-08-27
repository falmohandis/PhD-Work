# Load libraries
library(readxl)
library(dplyr)

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

# Define the Excel file name
file_path <- "MAP.xlsx"
full_file_path <- file.path(script_dir, file_path)

# Read the Excel file
df <- read_excel(full_file_path)

# Clean column names
names(df) <- gsub("[\r\n\t]+", "", names(df))
names(df) <- trimws(names(df))

# Convert grouping factors to factor type
df$`Hemorrhage Level` <- as.factor(df$`Hemorrhage Level`)
df$`Time Point` <- as.factor(df$`Time Point`)

# --- Summary Statistics Function ---
print_summary_stats <- function(timepoint_val) {
  cat("\n=====================================\n")
  cat("Summary Statistics for Time Point =", timepoint_val, "\n")
  
  df_sub <- df %>% filter(`Time Point` == timepoint_val)
  
  if (!"MAP" %in% names(df_sub)) {
    cat("MAP column not found.\n")
    return()
  }
  
  summary_stats <- df_sub %>%
    group_by(`Hemorrhage Level`) %>%
    summarise(
      Mean = mean(MAP, na.rm = TRUE),
      SD = sd(MAP, na.rm = TRUE),
      N = sum(!is.na(MAP))
    )
  print(summary_stats)
}

# Summary stats for Time Point 0 and 30
print_summary_stats("0")
print_summary_stats("30")



# --- Summary Statistics Function (No Hemorrhage Level Split) ---
print_summary_stats <- function(timepoint_val) {
  cat("\n=====================================\n")
  cat("Summary Statistics for Time Point =", timepoint_val, "\n")
  
  df_sub <- df %>% filter(`Time Point` == timepoint_val)
  
  if (!"MAP" %in% names(df_sub)) {
    cat("MAP column not found.\n")
    return()
  }
  
  summary_stats <- df_sub %>%
    summarise(
      Mean = mean(MAP, na.rm = TRUE),
      SD = sd(MAP, na.rm = TRUE),
      N = sum(!is.na(MAP))
    )
  print(summary_stats)
}

# Summary stats for Time Point 0 and 30
print_summary_stats("0")





# ------------------ Hard-coded ANOVA & Kruskal-Wallis Tests ------------------

cat("\n===== TIME POINT = 0 =====\n")

# ANOVA for MAP at Time Point 0
model_map_0 <- aov(MAP ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "0"))
cat("\n--- ANOVA: MAP at Time Point 0 ---\n")
print(summary(model_map_0))

# Shapiro-Wilk test for normality of residuals
sw_map_0 <- shapiro.test(residuals(model_map_0))
cat("\nShapiro-Wilk test:\n")
print(sw_map_0)

# If not normal, run Kruskal-Wallis
if (sw_map_0$p.value < 0.05) {
  cat("\nNon-normal residuals, running Kruskal-Wallis test...\n")
  print(kruskal.test(MAP ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "0")))
}


cat("\n===== TIME POINT = 30 =====\n")

# ANOVA for MAP at Time Point 30
model_map_30 <- aov(MAP ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "30"))
cat("\n--- ANOVA: MAP at Time Point 30 ---\n")
print(summary(model_map_30))

# Shapiro-Wilk test for normality of residuals
sw_map_30 <- shapiro.test(residuals(model_map_30))
cat("\nShapiro-Wilk test:\n")
print(sw_map_30)

# If not normal, run Kruskal-Wallis
if (sw_map_30$p.value < 0.05) {
  cat("\nNon-normal residuals, running Kruskal-Wallis test...\n")
  print(kruskal.test(MAP ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "30")))
}




# ------------------ Two-Way ANOVA ------------------

cat("\n===== TWO-WAY ANOVA: Hemorrhage Level × Time Point on MAP =====\n")

# Fit two-way ANOVA model with interaction
model_map_2way <- aov(MAP ~ `Hemorrhage Level` * `Time Point`, data = df)

# Output ANOVA summary
cat("\n--- ANOVA Summary ---\n")
print(summary(model_map_2way))

# Shapiro-Wilk test on residuals
cat("\n--- Shapiro-Wilk Normality Test on Residuals ---\n")
sw_map_2way <- shapiro.test(residuals(model_map_2way))
print(sw_map_2way)

# If residuals are non-normal, suggest non-parametric alternatives
if (sw_map_2way$p.value < 0.05) {
  cat("\nResiduals are not normally distributed.\n")
  cat("For non-parametric testing, consider stratified Kruskal-Wallis tests for each time point.\n")
}