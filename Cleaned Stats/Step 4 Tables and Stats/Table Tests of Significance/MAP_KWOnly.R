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




# --- Summary Statistics Function ---
print_summary_stats <- function(timepoint_vals) {
  df_sub <- df %>% 
    filter(`Time Point` %in% timepoint_vals)
  
  if (!"MAP" %in% names(df_sub)) {
    cat("MAP column not found.\n")
    return(invisible(NULL))
  }
  
  cat("\n=====================================\n")
  cat("Summary Statistics by Time Point\n")
  
  summary_stats <- df_sub %>%
    group_by(`Time Point`) %>%
    summarise(
      Mean = mean(MAP, na.rm = TRUE),
      SD   = sd(MAP, na.rm = TRUE),
      N    = sum(!is.na(MAP)),
      .groups = "drop"
    )
  
  print(summary_stats)
}

# Summary stats for Time Points 0 and 30
print_summary_stats(c("0", "30"))






# --- Kruskal-Wallis Tests Only ---
run_kw_test <- function(timepoint_val) {
  cat("\n===== TIME POINT =", timepoint_val, "=====\n")
  
  df_sub <- df %>% filter(`Time Point` == timepoint_val)
  
  if (!"MAP" %in% names(df_sub)) {
    cat("MAP column not found.\n")
    return()
  }
  
  cat("\n--- Kruskal-Wallis: MAP by Hemorrhage Level ---\n")
  print(kruskal.test(MAP ~ `Hemorrhage Level`, data = df_sub))
}

# Run Kruskal-Wallis tests
run_kw_test("0")
run_kw_test("30")
