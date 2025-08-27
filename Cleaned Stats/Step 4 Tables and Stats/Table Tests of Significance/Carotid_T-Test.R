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




# --- T-Tests at Time Point 0 ---

df_time0 <- df %>% filter(`Time Point` == "0")

# Filter df_time0 to only include Vendor == 0 or Vendor == 1
df_time0 <- df_time0 %>% filter(Vendor %in% c(0, 1))

# Convert Vendor to labeled factor after filtering
df_time0$Vendor <- factor(df_time0$Vendor, levels = c(0, 1), labels = c("Palladium", "Oak Hill"))

# Now continue with your grouping variables as factors
df_time0$`Hemorrhage Level` <- as.factor(df_time0$`Hemorrhage Level`)
df_time0$`Occlusion Group` <- as.factor(df_time0$`Occlusion Group`)  # confirm column name

cat("\n===== T-TESTS AT TIME POINT 0 =====\n")

# Helper function to check valid group sizes
has_valid_group_sizes <- function(df_sub) {
  vendor_counts <- df_sub %>%
    group_by(Vendor) %>%
    summarise(n = sum(!is.na(Avg)), .groups = "drop")
  
  all(c("Palladium", "Oak Hill") %in% vendor_counts$Vendor) &&
    all(vendor_counts$n[vendor_counts$Vendor == "Palladium"] >= 2,
        vendor_counts$n[vendor_counts$Vendor == "Oak Hill"] >= 2)
}

# 1. Compare Vendors within each Hemorrhage Level
cat("\n--- T-Tests: Vendor Differences within Each Hemorrhage Level ---\n")
hemorrhage_levels <- unique(df_time0$`Hemorrhage Level`)
for (level in hemorrhage_levels) {
  df_sub <- df_time0 %>% filter(`Hemorrhage Level` == level)
  cat(paste0("\nHemorrhage Level: ", level, "\n"))
  
  if (has_valid_group_sizes(df_sub)) {
    print(t.test(Avg ~ Vendor, data = df_sub))
  } else {
    cat("Only 1 vendor group exists or too few observations — skipping.\n")
  }
}

# 2. Compare Vendors within each Hemorrhage × Occlusion group
cat("\n--- T-Tests: Vendor Differences within Each Hemorrhage × Occlusion Group ---\n")
combos <- df_time0 %>%
  distinct(`Hemorrhage Level`, `Occlusion Group`) %>%
  arrange(`Hemorrhage Level`, `Occlusion Group`)

for (i in 1:nrow(combos)) {
  h <- combos$`Hemorrhage Level`[i]
  o <- combos$`Occlusion Group`[i]
  df_sub <- df_time0 %>%
    filter(`Hemorrhage Level` == h, `Occlusion Group` == o)
  
  cat(paste0("\nHemorrhage Level: ", h, " | Occlusion Group: ", o, "\n"))
  
  if (has_valid_group_sizes(df_sub)) {
    print(t.test(Avg ~ Vendor, data = df_sub))
  } else {
    cat("Only 1 vendor group exists or too few observations — skipping.\n")
  }
}

# 3. Compare Vendors Overall (ignore groups)
cat("\n--- T-Test: Overall Vendor Comparison (ignoring groups) ---\n")
if (has_valid_group_sizes(df_time0)) {
  print(t.test(Avg ~ Vendor, data = df_time0))
} else {
  cat("Only 1 vendor group exists or too few observations — skipping overall t-test.\n")
}
