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

# Define the Excel file name with extension
file_path <- "PV.xlsx"
full_file_path <- file.path(script_dir, file_path)

# Read the Excel file
df <- read_excel(full_file_path)

# Clean column names
names(df) <- gsub("[\r\n\t]+", "", names(df))
names(df) <- trimws(names(df))

# Convert grouping factors to factor type
df$`Hemorrhage Level` <- as.factor(df$`Hemorrhage Level`)
df$`Time Point` <- as.factor(df$`Time Point`)

# Variables of interest
variables <- c("Heart Rate", "Cardiac Output", "ESP", "EDP")

# Function to print summary statistics by Hemorrhage Level and Time Point
print_summary_stats <- function(timepoint_val) {
  cat("\n=====================================\n")
  cat("Summary Statistics for Time Point =", timepoint_val, "\n")
  
  df_sub <- df %>% filter(`Time Point` == timepoint_val)
  
  for (var in variables) {
    cat("\n---------------------------------\n")
    cat("Variable:", var, "\n")
    
    if (!var %in% names(df_sub)) {
      cat("Column not found.\n")
      next
    }
    
    summary_stats <- df_sub %>%
      group_by(`Hemorrhage Level`) %>%
      summarise(
        Mean = mean(.data[[var]], na.rm = TRUE),
        SD = sd(.data[[var]], na.rm = TRUE),
        N = sum(!is.na(.data[[var]]))
      )
    print(summary_stats)
  }
}

# Print summary stats for Time Point 0 and 30
print_summary_stats("0")
print_summary_stats("30")













# --------------- HARD-CODED TESTS FOR TIME POINT 0 ------------------

cat("\n===== Time Point = 0 =====\n")

# Heart Rate
cat("\n--- Heart Rate ---\n")
model_hr_0 <- aov(`Heart Rate` ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "0"))
print(summary(model_hr_0))
sw_hr_0 <- shapiro.test(residuals(model_hr_0))
cat("\nShapiro-Wilk test:\n")
print(sw_hr_0)
if (sw_hr_0$p.value < 0.05) {
  cat("\nNon-normal residuals, running Kruskal-Wallis test...\n")
  print(kruskal.test(`Heart Rate` ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "0")))
}

# Cardiac Output
cat("\n--- Cardiac Output ---\n")
model_co_0 <- aov(`Cardiac Output` ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "0"))
print(summary(model_co_0))
sw_co_0 <- shapiro.test(residuals(model_co_0))
cat("\nShapiro-Wilk test:\n")
print(sw_co_0)
if (sw_co_0$p.value < 0.05) {
  cat("\nNon-normal residuals, running Kruskal-Wallis test...\n")
  print(kruskal.test(`Cardiac Output` ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "0")))
}

# ESP
cat("\n--- ESP ---\n")
model_esp_0 <- aov(`ESP` ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "0"))
print(summary(model_esp_0))
sw_esp_0 <- shapiro.test(residuals(model_esp_0))
cat("\nShapiro-Wilk test:\n")
print(sw_esp_0)
if (sw_esp_0$p.value < 0.05) {
  cat("\nNon-normal residuals, running Kruskal-Wallis test...\n")
  print(kruskal.test(`ESP` ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "0")))
}

# EDP
cat("\n--- EDP ---\n")
model_edp_0 <- aov(`EDP` ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "0"))
print(summary(model_edp_0))
sw_edp_0 <- shapiro.test(residuals(model_edp_0))
cat("\nShapiro-Wilk test:\n")
print(sw_edp_0)
if (sw_edp_0$p.value < 0.05) {
  cat("\nNon-normal residuals, running Kruskal-Wallis test...\n")
  print(kruskal.test(`EDP` ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "0")))
}


# --------------- HARD-CODED TESTS FOR TIME POINT 30 ------------------

cat("\n===== Time Point = 30 =====\n")

# Heart Rate
cat("\n--- Heart Rate ---\n")
model_hr_30 <- aov(`Heart Rate` ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "30"))
print(summary(model_hr_30))
sw_hr_30 <- shapiro.test(residuals(model_hr_30))
cat("\nShapiro-Wilk test:\n")
print(sw_hr_30)
if (sw_hr_30$p.value < 0.05) {
  cat("\nNon-normal residuals, running Kruskal-Wallis test...\n")
  print(kruskal.test(`Heart Rate` ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "30")))
}

# Cardiac Output
cat("\n--- Cardiac Output ---\n")
model_co_30 <- aov(`Cardiac Output` ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "30"))
print(summary(model_co_30))
sw_co_30 <- shapiro.test(residuals(model_co_30))
cat("\nShapiro-Wilk test:\n")
print(sw_co_30)
if (sw_co_30$p.value < 0.05) {
  cat("\nNon-normal residuals, running Kruskal-Wallis test...\n")
  print(kruskal.test(`Cardiac Output` ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "30")))
}

# ESP
cat("\n--- ESP ---\n")
model_esp_30 <- aov(`ESP` ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "30"))
print(summary(model_esp_30))
sw_esp_30 <- shapiro.test(residuals(model_esp_30))
cat("\nShapiro-Wilk test:\n")
print(sw_esp_30)
if (sw_esp_30$p.value < 0.05) {
  cat("\nNon-normal residuals, running Kruskal-Wallis test...\n")
  print(kruskal.test(`ESP` ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "30")))
}

# EDP
cat("\n--- EDP ---\n")
model_edp_30 <- aov(`EDP` ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "30"))
print(summary(model_edp_30))
sw_edp_30 <- shapiro.test(residuals(model_edp_30))
cat("\nShapiro-Wilk test:\n")
print(sw_edp_30)
if (sw_edp_30$p.value < 0.05) {
  cat("\nNon-normal residuals, running Kruskal-Wallis test...\n")
  print(kruskal.test(`EDP` ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "30")))
}



cat("\n========== TWO-WAY ANOVA: Heart Rate ==========\n")
model_hr <- aov(`Heart Rate` ~ `Hemorrhage Level` * `Time Point`, data = df)
print(summary(model_hr))
cat("\nShapiro-Wilk Test:\n")
print(shapiro.test(residuals(model_hr)))

cat("\n========== TWO-WAY ANOVA: Cardiac Output ==========\n")
model_co <- aov(`Cardiac Output` ~ `Hemorrhage Level` * `Time Point`, data = df)
print(summary(model_co))
cat("\nShapiro-Wilk Test:\n")
print(shapiro.test(residuals(model_co)))

cat("\n========== TWO-WAY ANOVA: ESP ==========\n")
model_esp <- aov(`ESP` ~ `Hemorrhage Level` * `Time Point`, data = df)
print(summary(model_esp))
cat("\nShapiro-Wilk Test:\n")
print(shapiro.test(residuals(model_esp)))

cat("\n========== TWO-WAY ANOVA: EDP ==========\n")
model_edp <- aov(`EDP` ~ `Hemorrhage Level` * `Time Point`, data = df)
print(summary(model_edp))
cat("\nShapiro-Wilk Test:\n")
print(shapiro.test(residuals(model_edp)))





##################### NORMALIZATION ATTEMPTS ##################### 


library(MASS)  # For boxcox

# Helper function to attempt transformations and test residuals
check_normality_with_transforms <- function(model, variable_name, df) {
  residuals_orig <- residuals(model)
  cat("\n--- Shapiro-Wilk Test (Original) ---\n")
  print(shapiro.test(residuals_orig))
  
  # Try log transform
  if (all(df[[variable_name]] > 0, na.rm = TRUE)) {
    cat("\n--- Log Transformation ---\n")
    model_log <- aov(log(df[[variable_name]]) ~ df$`Hemorrhage Level` * df$`Time Point`)
    print(shapiro.test(residuals(model_log)))
  } else {
    cat("\n--- Log Transformation skipped (non-positive values) ---\n")
  }
  
  # Try square root transform
  if (all(df[[variable_name]] >= 0, na.rm = TRUE)) {
    cat("\n--- Square Root Transformation ---\n")
    model_sqrt <- aov(sqrt(df[[variable_name]]) ~ df$`Hemorrhage Level` * df$`Time Point`)
    print(shapiro.test(residuals(model_sqrt)))
  } else {
    cat("\n--- Square Root Transformation skipped (negative values) ---\n")
  }
  
  # Try Box-Cox transformation
  cat("\n--- Box-Cox Lambda Estimate (may require complete cases) ---\n")
  model_lm <- lm(df[[variable_name]] ~ df$`Hemorrhage Level` * df$`Time Point`)
  df_complete <- df[complete.cases(df[[variable_name]], df$`Hemorrhage Level`, df$`Time Point`), ]
  tryCatch({
    boxcox(model_lm)
  }, error = function(e) {
    cat("Box-Cox transformation failed or not applicable.\n")
  })
}

# ---- Heart Rate ----
cat("\n========== TWO-WAY ANOVA: Heart Rate ==========\n")
model_hr <- aov(`Heart Rate` ~ `Hemorrhage Level` * `Time Point`, data = df)
print(summary(model_hr))
check_normality_with_transforms(model_hr, "Heart Rate", df)

# ---- Cardiac Output ----
cat("\n========== TWO-WAY ANOVA: Cardiac Output ==========\n")
model_co <- aov(`Cardiac Output` ~ `Hemorrhage Level` * `Time Point`, data = df)
print(summary(model_co))
check_normality_with_transforms(model_co, "Cardiac Output", df)

# ---- ESP ----
cat("\n========== TWO-WAY ANOVA: ESP ==========\n")
model_esp <- aov(`ESP` ~ `Hemorrhage Level` * `Time Point`, data = df)
print(summary(model_esp))
check_normality_with_transforms(model_esp, "ESP", df)

# ---- EDP ----
cat("\n========== TWO-WAY ANOVA: EDP ==========\n")
model_edp <- aov(`EDP` ~ `Hemorrhage Level` * `Time Point`, data = df)
print(summary(model_edp))
check_normality_with_transforms(model_edp, "EDP", df)











############################# TRANSFORMS


# Helper function to attempt transformations and test residuals
check_normality_with_transforms <- function(model, variable_name, df) {
  residuals_orig <- residuals(model)
  cat("\n--- Shapiro-Wilk Test (Original) ---\n")
  print(shapiro.test(residuals_orig))
  
  # Try log transform
  if (all(df[[variable_name]] > 0, na.rm = TRUE)) {
    cat("\n--- Log Transformation ---\n")
    model_log <- aov(log(df[[variable_name]]) ~ df$`Hemorrhage Level` * df$`Time Point`)
    print(shapiro.test(residuals(model_log)))
  } else {
    cat("\n--- Log Transformation skipped (non-positive values) ---\n")
  }
  
  # Try square root transform
  if (all(df[[variable_name]] >= 0, na.rm = TRUE)) {
    cat("\n--- Square Root Transformation ---\n")
    model_sqrt <- aov(sqrt(df[[variable_name]]) ~ df$`Hemorrhage Level` * df$`Time Point`)
    print(shapiro.test(residuals(model_sqrt)))
  } else {
    cat("\n--- Square Root Transformation skipped (negative values) ---\n")
  }
  
  # Try Box-Cox transformation
  cat("\n--- Box-Cox Lambda Estimate (may require complete cases) ---\n")
  model_lm <- lm(df[[variable_name]] ~ df$`Hemorrhage Level` * df$`Time Point`)
  df_complete <- df[complete.cases(df[[variable_name]], df$`Hemorrhage Level`, df$`Time Point`), ]
  tryCatch({
    boxcox(model_lm)
  }, error = function(e) {
    cat("Box-Cox transformation failed or not applicable.\n")
  })
}

# ---- Heart Rate ----
cat("\n========== TWO-WAY ANOVA: Heart Rate ==========\n")
model_hr <- aov(`Heart Rate` ~ `Hemorrhage Level` * `Time Point`, data = df)
print(summary(model_hr))
check_normality_with_transforms(model_hr, "Heart Rate", df)

# ---- Cardiac Output ----
cat("\n========== TWO-WAY ANOVA: Cardiac Output ==========\n")
model_co <- aov(`Cardiac Output` ~ `Hemorrhage Level` * `Time Point`, data = df)
print(summary(model_co))
check_normality_with_transforms(model_co, "Cardiac Output", df)

# ---- ESP ----
cat("\n========== TWO-WAY ANOVA: ESP ==========\n")
model_esp <- aov(`ESP` ~ `Hemorrhage Level` * `Time Point`, data = df)
print(summary(model_esp))
check_normality_with_transforms(model_esp, "ESP", df)

# ---- EDP ----
cat("\n========== TWO-WAY ANOVA: EDP ==========\n")
model_edp <- aov(`EDP` ~ `Hemorrhage Level` * `Time Point`, data = df)
print(summary(model_edp))
check_normality_with_transforms(model_edp, "EDP", df)







# ------------------------- TRANSFORMED TWO-WAY ANOVA -------------------------

cat("\n========== TRANSFORMED TWO-WAY ANOVA (Sqrt) ==========\n")

# ---- Heart Rate (sqrt) ----
cat("\n--- Heart Rate (Square Root Transformed) ---\n")
model_hr_sqrt <- aov(sqrt(`Heart Rate`) ~ `Hemorrhage Level` * `Time Point`, data = df)
print(summary(model_hr_sqrt))
cat("\nShapiro-Wilk Test on residuals:\n")
print(shapiro.test(residuals(model_hr_sqrt)))

# ---- EDP (sqrt) ----
cat("\n--- EDP (Square Root Transformed) ---\n")
model_edp_sqrt <- aov(sqrt(`EDP`) ~ `Hemorrhage Level` * `Time Point`, data = df)
print(summary(model_edp_sqrt))
cat("\nShapiro-Wilk Test on residuals:\n")
print(shapiro.test(residuals(model_edp_sqrt)))
