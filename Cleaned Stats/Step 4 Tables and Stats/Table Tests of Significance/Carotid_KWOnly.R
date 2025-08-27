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
file_path <- "Carotid.xlsx"
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



# --- Summary Statistics Function (No Hemorrhage Level Breakdown) ---
print_summary_stats_overall <- function(timepoint_val) {
  cat("\n=====================================\n")
  cat("Overall Summary Statistics for Time Point =", timepoint_val, "\n")
  
  df_sub <- df %>% filter(`Time Point` == timepoint_val)
  
  if (!"Avg" %in% names(df_sub)) {
    cat("Avg column not found.\n")
    return()
  }
  
  summary_stats <- df_sub %>%
    summarise(
      Mean = mean(Avg, na.rm = TRUE),
      SD = sd(Avg, na.rm = TRUE),
      N = sum(!is.na(Avg))
    )
  
  print(summary_stats)
}
print_summary_stats_overall("0")






# ------------------ Kruskal-Wallis Tests Only ------------------

cat("\n===== TIME POINT = 0 =====\n")
cat("\n--- Kruskal-Wallis: Avg by Hemorrhage Level ---\n")
print(kruskal.test(Avg ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "0")))

cat("\n===== TIME POINT = 30 =====\n")
cat("\n--- Kruskal-Wallis: Avg by Hemorrhage Level ---\n")
print(kruskal.test(Avg ~ `Hemorrhage Level`, data = df, subset = (`Time Point` == "30")))
