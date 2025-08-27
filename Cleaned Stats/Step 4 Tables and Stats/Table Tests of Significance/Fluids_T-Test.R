# Load necessary libraries
library(tidyverse)
library(readxl)

# Get script directory or fallback
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

# Construct file path
file_path <- file.path(script_dir, "8-isoprostane.xlsx")

# Read the Excel file (assumes data is in the first sheet)
df <- read_excel(file_path)

# Convert character columns to appropriate formats
df$Time <- as.factor(df$Time)
df$Group <- as.factor(df$Group)
df$Subject <- as.factor(df$Subject)

# Convert Gender to numeric: 0 = Male, 1 = Female
df$Gender <- ifelse(df$Gender == "Male", 0,
                    ifelse(df$Gender == "Female", 1, NA))

# Ensure Weight is numeric
df$Weight <- as.numeric(df$Weight)

# Run two-way ANOVA with covariates
anova_result <- aov(Value ~ Time * Group + Gender + Weight + Error(Subject), data = df)

# View summary
summary(anova_result)
