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

# Clean column names: remove control chars, trim, replace spaces with underscores
names(df) <- gsub("[\r\n\t]+", "", names(df))
names(df) <- trimws(names(df))
names(df) <- gsub(" ", "_", names(df))  # Replace spaces with underscores

# Convert grouping variables to factors (adjusted names with underscores)
df$Hemorrhage_Level <- as.factor(df$Hemorrhage_Level)
df$Time_Point <- as.factor(df$Time_Point)

# Filter to Time Point 0 and Vendor == 0 or 1 only
df_time0 <- df %>%
  filter(Time_Point == "0", Vendor %in% c(0,1))

# Factorize Vendor with labels
df_time0$Vendor <- factor(df_time0$Vendor, levels = c(0,1), labels = c("Palladium", "Oak_Hill"))

# Factorize Occlusion_Group if it exists
if ("Occlusion_Group" %in% names(df_time0)) {
  df_time0$Occlusion_Group <- as.factor(df_time0$Occlusion_Group)
}

# Variables of interest with underscores
variables <- c("Heart_Rate", "Cardiac_Output", "ESP", "EDP")

# Helper function to check valid group sizes for a variable
has_valid_group_sizes <- function(df_sub, var) {
  vendor_counts <- df_sub %>%
    group_by(Vendor) %>%
    summarise(n = sum(!is.na(.data[[var]])), .groups = "drop")

  all(c("Palladium", "Oak_Hill") %in% vendor_counts$Vendor) &&
    all(vendor_counts$n[vendor_counts$Vendor == "Palladium"] >= 2,
        vendor_counts$n[vendor_counts$Vendor == "Oak_Hill"] >= 2)
}

cat("\n===== T-TESTS AT TIME POINT 0 =====\n")

for (var in variables) {
  cat(paste0("\n\n=== Variable: ", var, " ===\n"))

  # 1. Compare Vendors within each Hemorrhage Level
  cat("\n--- T-Tests: Vendor Differences within Each Hemorrhage Level ---\n")
  hemorrhage_levels <- unique(df_time0$Hemorrhage_Level)
  for (level in hemorrhage_levels) {
    df_sub <- df_time0 %>% filter(Hemorrhage_Level == level)
    cat(paste0("\nHemorrhage Level: ", level, "\n"))

    if (has_valid_group_sizes(df_sub, var)) {
      formula <- as.formula(paste(var, "~ Vendor"))
      print(t.test(formula, data = df_sub))
    } else {
      cat("Only 1 vendor group exists or too few observations — skipping.\n")
    }
  }
# 
#   # 2. Compare Vendors within each Hemorrhage × Occlusion Group (if Occlusion Group exists)
#   if ("Occlusion_Group" %in% names(df_time0)) {
#     cat("\n--- T-Tests: Vendor Differences within Each Hemorrhage × Occlusion Group ---\n")
#     combos <- df_time0 %>%
#       distinct(Hemorrhage_Level, Occlusion_Group) %>%
#       arrange(Hemorrhage_Level, Occlusion_Group)
# 
#     for (i in 1:nrow(combos)) {
#       h <- combos$Hemorrhage_Level[i]
#       o <- combos$Occlusion_Group[i]
#       df_sub <- df_time0 %>%
#         filter(Hemorrhage_Level == h, Occlusion_Group == o)
# 
#       cat(paste0("\nHemorrhage Level: ", h, " | Occlusion Group: ", o, "\n"))
# 
#       if (has_valid_group_sizes(df_sub, var)) {
#         formula <- as.formula(paste(var, "~ Vendor"))
#         print(t.test(formula, data = df_sub))
#       } else {
#         cat("Only 1 vendor group exists or too few observations — skipping.\n")
#       }
#     }
#   }

  
  # 2. Compare Vendors within each Hemorrhage × Occlusion Group (if Occlusion Group exists)
  if ("Occlusion_Group" %in% names(df_time0)) {
    cat("\n--- T-Tests: Vendor Differences within Each Hemorrhage × Occlusion Group ---\n")
    
    # Hardcoded combinations (replace these with actual combos you want to test)
    # Example combos:
    combos_to_test <- list(
      list(Hemorrhage_Level = "10", Occlusion_Group = "Full"),
      list(Hemorrhage_Level = "30", Occlusion_Group = "No"),
      list(Hemorrhage_Level = "30", Occlusion_Group = "Full")
      # add more if needed
    )
    
    for (combo in combos_to_test) {
      h <- combo$Hemorrhage_Level
      o <- combo$Occlusion_Group
      df_sub <- df_time0 %>%
        filter(Hemorrhage_Level == h, Occlusion_Group == o)
      
      cat(paste0("\nHemorrhage Level: ", h, " | Occlusion Group: ", o, "\n"))
      
      if (has_valid_group_sizes(df_sub, var)) {
        formula <- as.formula(paste(var, "~ Vendor"))
        print(t.test(formula, data = df_sub))
      } else {
        cat("Only 1 vendor group exists or too few observations — skipping.\n")
      }
    }
  }
  
  # 3. Compare Vendors Overall (ignoring groups)
  cat("\n--- T-Test: Overall Vendor Comparison (ignoring groups) ---\n")
  if (has_valid_group_sizes(df_time0, var)) {
    formula <- as.formula(paste(var, "~ Vendor"))
    print(t.test(formula, data = df_time0))
  } else {
    cat("Only 1 vendor group exists or too few observations — skipping overall t-test.\n")
  }
}




