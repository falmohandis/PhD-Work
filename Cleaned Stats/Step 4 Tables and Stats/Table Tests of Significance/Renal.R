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
file_path <- "Renal.xlsx"
full_file_path <- file.path(script_dir, file_path)

# Read the Excel file
df <- read_excel(full_file_path)

# Clean column names
names(df) <- gsub("[\r\n\t]+", "", names(df))
names(df) <- trimws(names(df))

# Convert grouping variables to factor
df$`Hemorrhage Level` <- as.factor(df$`Hemorrhage Level`)
df$`Time Point` <- as.factor(df$`Time Point`)

# --- Summary Statistics Function ---
print_summary_stats <- function(timepoint_val) {
  cat("\n=====================================\n")
  cat("Summary Statistics for Time Point =", timepoint_val, "\n")
  
  df_sub <- df %>% filter(`Time Point` == timepoint_val)
  
  if (!"Avg" %in% names(df_sub)) {
    cat("Avg column not found.\n")
    return()
  }
  
  summary_stats <- df_sub %>%
    group_by(`Hemorrhage Level`) %>%
    summarise(
      Mean = mean(Avg, na.rm = TRUE),
      SD = sd(Avg, na.rm = TRUE),
      N = sum(!is.na(Avg))
    )
  print(summary_stats)
}

# Summary stats for Time Point 0 and 30
print_summary_stats("0")
print_summary_stats("30")

# ------------------ Hard-coded ANOVA & Kruskal-Wallis Tests ------------------

cat("\n===== TIME POINT = 0 =====\n")

# ANOVA for Avg at Time Point 0
model_avg_0 <- aov(Avg ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "0"))
cat("\n--- ANOVA: Avg at Time Point 0 ---\n")
print(summary(model_avg_0))

# Shapiro-Wilk test
sw_avg_0 <- shapiro.test(residuals(model_avg_0))
cat("\nShapiro-Wilk test:\n")
print(sw_avg_0)

# If not normal, run Kruskal-Wallis
if (sw_avg_0$p.value < 0.05) {
  cat("\nNon-normal residuals, running Kruskal-Wallis test...\n")
  print(kruskal.test(Avg ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "0")))
}


cat("\n===== TIME POINT = 30 =====\n")

# ANOVA for Avg at Time Point 30
model_avg_30 <- aov(Avg ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "30"))
cat("\n--- ANOVA: Avg at Time Point 30 ---\n")
print(summary(model_avg_30))

# Shapiro-Wilk test
sw_avg_30 <- shapiro.test(residuals(model_avg_30))
cat("\nShapiro-Wilk test:\n")
print(sw_avg_30)

# If not normal, run Kruskal-Wallis
if (sw_avg_30$p.value < 0.05) {
  cat("\nNon-normal residuals, running Kruskal-Wallis test...\n")
  print(kruskal.test(Avg ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "30")))
}




cat("\n===== TWO-WAY ANOVA: Hemorrhage Level × Time Point on Avg =====\n")

# Fit two-way ANOVA model with interaction
model_avg_2way <- aov(Avg ~ `Hemorrhage Level` * `Time Point`, data = df)

# Output ANOVA summary
cat("\n--- ANOVA Summary ---\n")
print(summary(model_avg_2way))

# Shapiro-Wilk test on residuals
cat("\n--- Shapiro-Wilk Normality Test on Residuals ---\n")
sw_avg_2way <- shapiro.test(residuals(model_avg_2way))
print(sw_avg_2way)

# If residuals are non-normal, consider Kruskal-Wallis (note: not valid for interaction)
if (sw_avg_2way$p.value < 0.05) {
  cat("\nResiduals not normal — consider non-parametric approaches (e.g., stratified Kruskal-Wallis).\n")
}
