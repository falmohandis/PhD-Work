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

# Define the Excel file name with extension
file_path <- "Fluids.xlsx"
full_file_path <- file.path(script_dir, file_path)

# Read the Excel file
df <- read_excel(full_file_path)

# Clean column names
names(df) <- gsub("[\r\n\t]+", "", names(df))
names(df) <- trimws(names(df))

# Ensure Hemorrhage is treated as a factor
df$`Hemorrhage (%)` <- as.factor(df$`Hemorrhage (%)`)
# df$`Time Point` <- as.factor(df$`Time Point`)


# List of variables for summary stats
variables <- c(
  "Instrumentation (min)", 
  "Avg. Inhaled Isoflurane (%)", 
  "Norepinephrine Inst. (mcg)",
  "Crystalloids Inst. (mL)", 
  "Plasmalyte/Time", 
  "Crystalloids Exp. (mL)", 
  "Heparin (mL)",
  "Norepi/weight",
  "Weight (kg)",
  "Heparin/wt (mL/kg)"
)

# Loop to print summary stats by Hemorrhage (%)
for (var in variables) {
  cat("\n=====================================\n")
  cat("Variable:", var, "\n")
  
  if (!var %in% names(df)) {
    cat("Column not found.\n")
    next
  }
  
  summary_stats <- df %>%
    group_by(`Hemorrhage (%)`) %>%
    summarise(
      Mean = mean(.data[[var]], na.rm = TRUE),
      SD = sd(.data[[var]], na.rm = TRUE),
      N = sum(!is.na(.data[[var]]))
    )
  
  print(summary_stats)
}

# Loop to print overall summary stats (not by Hemorrhage level)
for (var in variables) {
  cat("\n=====================================\n")
  cat("Variable:", var, "\n")
  
  if (!var %in% names(df)) {
    cat("Column not found.\n")
    next
  }
  
  summary_stats <- df %>%
    summarise(
      Mean = mean(.data[[var]], na.rm = TRUE),
      SD = sd(.data[[var]], na.rm = TRUE),
      N = sum(!is.na(.data[[var]]))
    )
  
  print(summary_stats)
}



# -------------------- MANUAL KRUSKAL-WALLIS TESTS ONLY --------------------

cat("\n========== Instrumentation (min) ==========\n")
print(kruskal.test(`Instrumentation (min)` ~ `Hemorrhage (%)`, data = df))

cat("\n========== Avg. Inhaled Isoflurane (%) ==========\n")
print(kruskal.test(`Avg. Inhaled Isoflurane (%)` ~ `Hemorrhage (%)`, data = df))

cat("\n========== Norepinephrine Inst. (mcg) ==========\n")
print(kruskal.test(`Norepinephrine Inst. (mcg)` ~ `Hemorrhage (%)`, data = df))

cat("\n========== Crystalloids Inst. (mL) ==========\n")
print(kruskal.test(`Crystalloids Inst. (mL)` ~ `Hemorrhage (%)`, data = df))

cat("\n========== Plasmalyte/Time ==========\n")
print(kruskal.test(`Plasmalyte/Time` ~ `Hemorrhage (%)`, data = df))

cat("\n========== Crystalloids Exp. (mL) ==========\n")
print(kruskal.test(`Crystalloids Exp. (mL)` ~ `Hemorrhage (%)`, data = df))

cat("\n========== Heparin (mL) ==========\n")
print(kruskal.test(`Heparin (mL)` ~ `Hemorrhage (%)`, data = df))

cat("\n========== Weight (kg) ==========\n")
print(kruskal.test(`Weight (kg)` ~ `Hemorrhage (%)`, data = df))

cat("\n========== Norepi/weight ==========\n")
print(kruskal.test(`Norepi/weight` ~ `Hemorrhage (%)`, data = df))

cat("\n========== Heparin/wt (mL/kg)  ==========\n")
print(kruskal.test(`Heparin/wt (mL/kg)` ~ `Hemorrhage (%)`, data = df))







# -------------------- MANUAL ANOVAS + SHAPIRO-WILK --------------------

cat("\n========== Instrumentation (min) ==========\n")
model1 <- aov(`Instrumentation (min)` ~ `Hemorrhage (%)`, data = df)
print(summary(model1))
cat("\nShapiro-Wilk Test:\n")
print(shapiro.test(residuals(model1)))

cat("\n========== Avg. Inhaled Isoflurane (%) ==========\n")
model2 <- aov(`Avg. Inhaled Isoflurane (%)` ~ `Hemorrhage (%)`, data = df)
print(summary(model2))
cat("\nShapiro-Wilk Test:\n")
print(shapiro.test(residuals(model2)))

cat("\n========== Norepinephrine Inst. (mcg) ==========\n")
model3 <- aov(`Norepinephrine Inst. (mcg)` ~ `Hemorrhage (%)`, data = df)
print(summary(model3))
cat("\nShapiro-Wilk Test:\n")
print(shapiro.test(residuals(model3)))

cat("\n========== Crystalloids Inst. (mL) ==========\n")
model4 <- aov(`Crystalloids Inst. (mL)` ~ `Hemorrhage (%)`, data = df)
print(summary(model4))
cat("\nShapiro-Wilk Test:\n")
print(shapiro.test(residuals(model4)))

cat("\n========== Plasmalyte/Time ==========\n")
model5 <- aov(`Plasmalyte/Time` ~ `Hemorrhage (%)`, data = df)
print(summary(model5))
cat("\nShapiro-Wilk Test:\n")
print(shapiro.test(residuals(model5)))

cat("\n========== Crystalloids Exp. (mL) ==========\n")
model6 <- aov(`Crystalloids Exp. (mL)` ~ `Hemorrhage (%)`, data = df)
print(summary(model6))
cat("\nShapiro-Wilk Test:\n")
print(shapiro.test(residuals(model6)))

cat("\n========== Heparin (mL) ==========\n")
model7 <- aov(`Heparin (mL)` ~ `Hemorrhage (%)`, data = df)
print(summary(model7))
cat("\nShapiro-Wilk Test:\n")
print(shapiro.test(residuals(model7)))

# ANOVA for Weight (kg)
cat("\n========== Weight (kg) ==========\n")
model_weight <- aov(`Weight (kg)` ~ `Hemorrhage (%)`, data = df)
print(summary(model_weight))

# Shapiro-Wilk test on residuals
shapiro_res <- shapiro.test(residuals(model_weight))
cat("\nShapiro-Wilk Test for Normality of Residuals:\n")
print(shapiro_res)

# -------------------- KRUSKAL-WALLIS --------------------



cat("\n========== Kruskal-Wallis: Instrumentation (min) ==========\n")
print(kruskal.test(`Instrumentation (min)` ~ `Hemorrhage (%)`, data = df))

cat("\n========== Kruskal-Wallis: Plasmalyte/Time ==========\n")
print(kruskal.test(`Plasmalyte/Time` ~ `Hemorrhage (%)`, data = df))

cat("\n========== Kruskal-Wallis: Crystalloids Exp. (mL) ==========\n")
print(kruskal.test(`Crystalloids Exp. (mL)` ~ `Hemorrhage (%)`, data = df))

cat("\n========== Kruskal-Wallis: Heparin (mL) ==========\n")
print(kruskal.test(`Heparin (mL)` ~ `Hemorrhage (%)`, data = df))



# 
# # Ensure Time Point is a factor
# df$`Time Point` <- as.factor(df$`Time Point`)
# 
# cat("\n========== TWO-WAY ANOVA: Instrumentation (min) ==========\n")
# model1 <- aov(`Instrumentation (min)` ~ `Hemorrhage (%)` * `Time Point`, data = df)
# print(summary(model1))
# cat("\nShapiro-Wilk Test:\n")
# print(shapiro.test(residuals(model1)))
# 
# cat("\n========== TWO-WAY ANOVA: Avg. Inhaled Isoflurane (%) ==========\n")
# model2 <- aov(`Avg. Inhaled Isoflurane (%)` ~ `Hemorrhage (%)` * `Time Point`, data = df)
# print(summary(model2))
# cat("\nShapiro-Wilk Test:\n")
# print(shapiro.test(residuals(model2)))
# 
# cat("\n========== TWO-WAY ANOVA: Norepinephrine Inst. (mcg) ==========\n")
# model3 <- aov(`Norepinephrine Inst. (mcg)` ~ `Hemorrhage (%)` * `Time Point`, data = df)
# print(summary(model3))
# cat("\nShapiro-Wilk Test:\n")
# print(shapiro.test(residuals(model3)))
# 
# cat("\n========== TWO-WAY ANOVA: Crystalloids Inst. (mL) ==========\n")
# model4 <- aov(`Crystalloids Inst. (mL)` ~ `Hemorrhage (%)` * `Time Point`, data = df)
# print(summary(model4))
# cat("\nShapiro-Wilk Test:\n")
# print(shapiro.test(residuals(model4)))
# 
# cat("\n========== TWO-WAY ANOVA: Plasmalyte/Time ==========\n")
# model5 <- aov(`Plasmalyte/Time` ~ `Hemorrhage (%)` * `Time Point`, data = df)
# print(summary(model5))
# cat("\nShapiro-Wilk Test:\n")
# print(shapiro.test(residuals(model5)))
# 
# cat("\n========== TWO-WAY ANOVA: Crystalloids Exp. (mL) ==========\n")
# model6 <- aov(`Crystalloids Exp. (mL)` ~ `Hemorrhage (%)` * `Time Point`, data = df)
# print(summary(model6))
# cat("\nShapiro-Wilk Test:\n")
# print(shapiro.test(residuals(model6)))
# 
# cat("\n========== TWO-WAY ANOVA: Heparin (mL) ==========\n")
# model7 <- aov(`Heparin (mL)` ~ `Hemorrhage (%)` * `Time Point`, data = df)
# print(summary(model7))
# cat("\nShapiro-Wilk Test:\n")
# print(shapiro.test(residuals(model7)))
