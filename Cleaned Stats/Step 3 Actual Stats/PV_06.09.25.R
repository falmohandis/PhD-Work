# Load necessary libraries
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

# Load data
file_path <- "PV_Fractional_Increase_T0-1.xlsx"
full_file_path <- file.path(script_dir, file_path)
df <- read_excel(full_file_path)

# Clean column names
colnames(df) <- colnames(df) %>%
  str_replace_all("[ \\-\\(\\)]", "") %>%
  make.names(unique = TRUE)

# Define hard-coded mapping from TimePoint to numeric
timepoint_map <- c(
  "T0-1" = 0,
  "T5-6" = 5,
  "T10-11" = 10,
  "T15-16" = 15,
  "T20-21" = 20,
  "T25-26" = 25,
  "T29-30" = 30,
  "T30-T31" = 31,
  "T35-36" = 35,
  "T40-41" = 40,
  "T45-46" = 45,
  "T50-51" = 50,
  "T55-56" = 55,
  "T59-60" = 59,
  "T60-61" = 60,
  "T64-65" = 64,
  "T65-66" = 65,
  "T70-71" = 70,
  "T74-75" = 75,
  "T75-76" = 76,
  "T80-81" = 80,
  "T120-121" = 120,
  "T180-181" = 180,
  "T239-240" = 240
)


# Extract the text part after the percentage and space
df$OcclusionGroup <- sub("^\\d+%\\s+", "", df$TreatmentGroup)

# Apply mapping to create a new numeric TimePoint column
df$TimePoint <- timepoint_map[df$OGTimePoint]

# Variable transformation
df <- df %>%
  mutate(
    SubjectID = as.factor(ParentFolder),
    TimePoint = as.factor(TimePoint),
    OcclusionGroup = factor(OcclusionGroup, levels = c("None", "Partial", "Full")),
    OcclusionGroup = as.numeric(OcclusionGroup) - 1,
    HemorrhageLevel = as.factor((HemorrhageLevel / 10) - 1)
  )

df$OcclusionGroup <- factor(df$OcclusionGroup, levels = 0:2, labels = c("None", "Partial", "Full"))
df$HemorrhageLevel <- factor(df$HemorrhageLevel, levels = 0:2, labels = c("10", "20", "30"))


# Select relevant columns
reference_columns <- c("SubjectID", "TimePoint", "HemorrhageLevel", "OcclusionGroup",
                     "Gender", "Weightkg",
                     "CumulativeVasopressin",	"Plasmalyte",	"Norepi"
                     )


# Use reference_columns in all selections
strokework <- df %>% select(all_of(reference_columns), StrokeWork)
strokevolume <- df %>% select(all_of(reference_columns), StrokeVolume)
ejectionfraction <- df %>% select(all_of(reference_columns), EjectionFraction)
heartrate <- df %>% select(all_of(reference_columns), HeartRate)
cardiacoutput <- df %>% select(all_of(reference_columns), CardiacOutput)
elastance <- df %>% select(all_of(reference_columns), Elastance)

esv <- df %>% select(all_of(reference_columns), ESV)
esp <- df %>% select(all_of(reference_columns), ESP)
edv <- df %>% select(all_of(reference_columns), EDV)
edp <- df %>% select(all_of(reference_columns), EDP)



######################## DIASTOLE ########################



########### EDV ###########
# ----- Test 1: EDV at TimePoint 30 -----
edv_30 <- edv %>% filter(TimePoint == 30)
edv_30$HemorrhageLevel <- as.factor(edv_30$HemorrhageLevel)
edv_30$Gender <- as.factor(edv_30$Gender)

anova_edv_30 <- aov(EDV ~ HemorrhageLevel + Gender + Weightkg+CumulativeVasopressin+Plasmalyte+ Norepi, data = edv_30)
summary(anova_edv_30)
TukeyHSD(anova_edv_30, "HemorrhageLevel")

# ----- Test 2: EDV at TimePoint 60 -----
edv_60 <- edv %>% filter(TimePoint == 60)
edv_60$HemorrhageLevel <- as.factor(edv_60$HemorrhageLevel)
edv_60$Gender <- as.factor(edv_60$Gender)

anova_edv_60 <- aov(EDV ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg+CumulativeVasopressin+Plasmalyte+ Norepi, data = edv_60)
summary(anova_edv_60)
TukeyHSD(anova_edv_60, "OcclusionGroup")
TukeyHSD(anova_edv_60, "HemorrhageLevel")
TukeyHSD(anova_edv_60, "HemorrhageLevel:OcclusionGroup")

# ----- Test 3: EDV from TimePoint 60 to 65 -----
edv_60_65 <- edv %>% filter(TimePoint %in% c(60, 65))
edv_60_65$HemorrhageLevel <- as.factor(edv_60_65$HemorrhageLevel)
edv_60_65$Gender <- as.factor(edv_60_65$Gender)

model_edv_60_65 <- lmer(EDV ~ TimePoint * HemorrhageLevel * OcclusionGroup+CumulativeVasopressin+Plasmalyte+ Norepi + (1 | SubjectID), data = edv_60_65)
summary(model_edv_60_65)

emm_edv_60_65 <- emmeans(model_edv_60_65, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_edv_60_65 <- pairs(emm_edv_60_65, adjust = "tukey")
as.data.frame(tukey_edv_60_65)

# ----- Test 4: EDV across all TimePoints -----
model_edv_all <- lmer(EDV ~ TimePoint * HemorrhageLevel * OcclusionGroup +CumulativeVasopressin+Plasmalyte+ Norepi+ (1 | SubjectID), data = edv)
summary(model_edv_all)

emm_edv_all <- emmeans(model_edv_all, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_edv_all <- pairs(emm_edv_all, adjust = "tukey")
as.data.frame(tukey_edv_all)







########### EDP ###########
# ----- Test 1: EDP at TimePoint 30 -----
edp_30 <- edp %>% filter(TimePoint == 30)
edp_30$HemorrhageLevel <- as.factor(edp_30$HemorrhageLevel)
edp_30$Gender <- as.factor(edp_30$Gender)

anova_edp_30 <- aov(EDP ~ HemorrhageLevel + Gender + Weightkg+CumulativeVasopressin+Plasmalyte+ Norepi, data = edp_30)
summary(anova_edp_30)
TukeyHSD(anova_edp_30, "HemorrhageLevel")

# ----- Test 2: EDP at TimePoint 60 -----
edp_60 <- edp %>% filter(TimePoint == 60)
edp_60$HemorrhageLevel <- as.factor(edp_60$HemorrhageLevel)
edp_60$Gender <- as.factor(edp_60$Gender)

anova_edp_60 <- aov(EDP ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg+CumulativeVasopressin+Plasmalyte+ Norepi, data = edp_60)
summary(anova_edp_60)
TukeyHSD(anova_edp_60, "OcclusionGroup")
TukeyHSD(anova_edp_60, "HemorrhageLevel")
TukeyHSD(anova_edp_60, "HemorrhageLevel:OcclusionGroup")

# ----- Test 3: EDP from TimePoint 60 to 65 -----
edp_60_65 <- edp %>% filter(TimePoint %in% c(60, 65))
edp_60_65$HemorrhageLevel <- as.factor(edp_60_65$HemorrhageLevel)
edp_60_65$Gender <- as.factor(edp_60_65$Gender)

model_edp_60_65 <- lmer(EDP ~ TimePoint * HemorrhageLevel * OcclusionGroup+CumulativeVasopressin+Plasmalyte+ Norepi + (1 | SubjectID), data = edp_60_65)
summary(model_edp_60_65)

emm_edp_60_65 <- emmeans(model_edp_60_65, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_edp_60_65 <- pairs(emm_edp_60_65, adjust = "tukey")
as.data.frame(tukey_edp_60_65)

# ----- Test 4: EDP across all TimePoints -----
model_edp_all <- lmer(EDP ~ TimePoint * HemorrhageLevel * OcclusionGroup +CumulativeVasopressin+Plasmalyte+ Norepi+ (1 | SubjectID), data = edp)
summary(model_edp_all)

emm_edp_all <- emmeans(model_edp_all, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_edp_all <- pairs(emm_edp_all, adjust = "tukey")
as.data.frame(tukey_edp_all)






########### HeartRate ###########
# ----- Test 1: HeartRate at TimePoint 30 -----
heartrate_30 <- heartrate %>% filter(TimePoint == 30)
heartrate_30$HemorrhageLevel <- as.factor(heartrate_30$HemorrhageLevel)
heartrate_30$Gender <- as.factor(heartrate_30$Gender)

anova_hr_30 <- aov(HeartRate ~ HemorrhageLevel + Gender + Weightkg+CumulativeVasopressin+Plasmalyte+ Norepi, data = heartrate_30)
summary(anova_hr_30)
TukeyHSD(anova_hr_30, "HemorrhageLevel")

# ----- Test 2: HeartRate at TimePoint 60 -----
heartrate_60 <- heartrate %>% filter(TimePoint == 60)
heartrate_60$HemorrhageLevel <- as.factor(heartrate_60$HemorrhageLevel)
heartrate_60$Gender <- as.factor(heartrate_60$Gender)

anova_hr_60 <- aov(HeartRate ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg+CumulativeVasopressin+Plasmalyte+ Norepi, data = heartrate_60)
summary(anova_hr_60)
TukeyHSD(anova_hr_60, "OcclusionGroup")
TukeyHSD(anova_hr_60, "HemorrhageLevel")
TukeyHSD(anova_hr_60, "HemorrhageLevel:OcclusionGroup")

# ----- Test 3: HeartRate from TimePoint 60 to 65 -----
heartrate_60_65 <- heartrate %>% filter(TimePoint %in% c(60, 65))
heartrate_60_65$HemorrhageLevel <- as.factor(heartrate_60_65$HemorrhageLevel)
heartrate_60_65$Gender <- as.factor(heartrate_60_65$Gender)

model_hr_60_65 <- lmer(HeartRate ~ TimePoint * HemorrhageLevel * OcclusionGroup+CumulativeVasopressin+Plasmalyte+ Norepi + (1 | SubjectID), data = heartrate_60_65)
summary(model_hr_60_65)

emm_hr_60_65 <- emmeans(model_hr_60_65, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_hr_60_65 <- pairs(emm_hr_60_65, adjust = "tukey")
as.data.frame(tukey_hr_60_65)

# ----- Test 4: HeartRate across all TimePoints -----
model_hr_all <- lmer(HeartRate ~ TimePoint * HemorrhageLevel * OcclusionGroup +CumulativeVasopressin+Plasmalyte+ Norepi+ (1 | SubjectID), data = heartrate)
summary(model_hr_all)

emm_hr_all <- emmeans(model_hr_all, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_hr_all <- pairs(emm_hr_all, adjust = "tukey")
as.data.frame(tukey_hr_all)







######################## SYSTOLE ########################



########### StrokeWork ########### 
# ----- Test 1: StrokeWork at TimePoint 30 -----
strokework_30 <- strokework %>% filter(TimePoint == 30)
strokework_30$HemorrhageLevel <- as.factor(strokework_30$HemorrhageLevel)
strokework_30$Gender <- as.factor(strokework_30$Gender)

anova_sw_30 <- aov(StrokeWork ~ HemorrhageLevel + Gender + Weightkg +CumulativeVasopressin+Plasmalyte+ Norepi, data = strokework_30)
summary(anova_sw_30)
# TukeyHSD(anova_sw_30)
TukeyHSD(anova_sw_30, "HemorrhageLevel")

# ----- Test 2: StrokeWork at TimePoint 60 -----
strokework_60 <- strokework %>% filter(TimePoint == 60)
strokework_60$HemorrhageLevel <- as.factor(strokework_60$HemorrhageLevel)
strokework_60$Gender <- as.factor(strokework_60$Gender)

anova_sw_60 <- aov(StrokeWork ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg+CumulativeVasopressin+Plasmalyte+ Norepi, data = strokework_60)
summary(anova_sw_60)
TukeyHSD(anova_sw_60, "OcclusionGroup")
TukeyHSD(anova_sw_60, "HemorrhageLevel")
TukeyHSD(anova_sw_60, "HemorrhageLevel:OcclusionGroup")

# ----- Test 3: StrokeWork from TimePoint 61 to 65 -----
strokework_60_65 <- strokework %>% filter(TimePoint %in% c(60, 65))
strokework_60_65$HemorrhageLevel <- as.factor(strokework_60_65$HemorrhageLevel)
strokework_60_65$Gender <- as.factor(strokework_60_65$Gender)

model_sw_60_65 <- lmer(StrokeWork ~ TimePoint * HemorrhageLevel * OcclusionGroup 
                       + Gender + Weightkg +CumulativeVasopressin+Plasmalyte+ Norepi
                       + (1 | SubjectID), data = strokework_60_65
                       )
summary(model_sw_60_65)

emm_sw_60_65 <- emmeans(model_sw_60_65, ~ TimePoint * HemorrhageLevel * OcclusionGroup 
                        + Gender + Weightkg 
                        )
tukey_sw_60_65 <- pairs(emm_sw_60_65, adjust = "tukey")
as.data.frame(tukey_sw_60_65)

# ----- Test 4: StrokeWork across all TimePoints -----
model_sw_all <- lmer(StrokeWork ~ TimePoint * HemorrhageLevel * OcclusionGroup
                     + Gender + Weightkg +CumulativeVasopressin+Plasmalyte+ Norepi
                     + (1 | SubjectID), data = strokework)
summary(model_sw_all)

emm_sw_all <- emmeans(model_sw_all, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_sw_all <- pairs(emm_sw_all, adjust = "tukey")
as.data.frame(tukey_sw_all)








########### StrokeVolume ########### 

# ----- Test 1: StrokeVolume at TimePoint 30 -----
strokevolume_30 <- strokevolume %>% filter(TimePoint == 30)
strokevolume_30$HemorrhageLevel <- as.factor(strokevolume_30$HemorrhageLevel)
strokevolume_30$Gender <- as.factor(strokevolume_30$Gender)

anova_sv_30 <- aov(StrokeVolume ~ HemorrhageLevel + Gender + Weightkg +CumulativeVasopressin+Plasmalyte+ Norepi, data = strokevolume_30)
summary(anova_sv_30)
TukeyHSD(anova_sv_30, "HemorrhageLevel")

# ----- Test 2: StrokeVolume at TimePoint 60 -----
strokevolume_60 <- strokevolume %>% filter(TimePoint == 60)
strokevolume_60$HemorrhageLevel <- as.factor(strokevolume_60$HemorrhageLevel)
strokevolume_60$Gender <- as.factor(strokevolume_60$Gender)

anova_sv_60 <- aov(StrokeVolume ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg +CumulativeVasopressin+Plasmalyte+ Norepi, data = strokevolume_60)
summary(anova_sv_60)
TukeyHSD(anova_sv_60, "OcclusionGroup")
TukeyHSD(anova_sv_60, "HemorrhageLevel")
TukeyHSD(anova_sv_60, "HemorrhageLevel:OcclusionGroup")

# ----- Test 3: StrokeVolume from TimePoint 60 to 65 -----
strokevolume_60_65 <- strokevolume %>% filter(TimePoint %in% c(60, 65))
strokevolume_60_65$HemorrhageLevel <- as.factor(strokevolume_60_65$HemorrhageLevel)
strokevolume_60_65$Gender <- as.factor(strokevolume_60_65$Gender)

model_sv_60_65 <- lmer(StrokeVolume ~ TimePoint * HemorrhageLevel * OcclusionGroup +CumulativeVasopressin+Plasmalyte+ Norepi+ (1 | SubjectID), data = strokevolume_60_65)
summary(model_sv_60_65)

emm_sv_60_65 <- emmeans(model_sv_60_65, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_sv_60_65 <- pairs(emm_sv_60_65, adjust = "tukey")
as.data.frame(tukey_sv_60_65)

# ----- Test 4: StrokeVolume across all TimePoints -----
model_sv_all <- lmer(StrokeVolume ~ TimePoint * HemorrhageLevel * OcclusionGroup +CumulativeVasopressin+Plasmalyte+ Norepi + (1 | SubjectID), data = strokevolume)
summary(model_sv_all)

emm_sv_all <- emmeans(model_sv_all, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_sv_all <- pairs(emm_sv_all, adjust = "tukey")
as.data.frame(tukey_sv_all)











########### EjectionFraction ###########
# ----- Test 1: EjectionFraction at TimePoint 30 -----
ejectionfraction_30 <- ejectionfraction %>% filter(TimePoint == 30)
ejectionfraction_30$HemorrhageLevel <- as.factor(ejectionfraction_30$HemorrhageLevel)
ejectionfraction_30$Gender <- as.factor(ejectionfraction_30$Gender)

anova_ef_30 <- aov(EjectionFraction ~ HemorrhageLevel + Gender + Weightkg+CumulativeVasopressin+Plasmalyte+ Norepi, data = ejectionfraction_30)
summary(anova_ef_30)
TukeyHSD(anova_ef_30, "HemorrhageLevel")

# ----- Test 2: EjectionFraction at TimePoint 60 -----
ejectionfraction_60 <- ejectionfraction %>% filter(TimePoint == 60)
ejectionfraction_60$HemorrhageLevel <- as.factor(ejectionfraction_60$HemorrhageLevel)
ejectionfraction_60$Gender <- as.factor(ejectionfraction_60$Gender)

anova_ef_60 <- aov(EjectionFraction ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg+CumulativeVasopressin+Plasmalyte+ Norepi, data = ejectionfraction_60)
summary(anova_ef_60)
TukeyHSD(anova_ef_60, "OcclusionGroup")
TukeyHSD(anova_ef_60, "HemorrhageLevel")
TukeyHSD(anova_ef_60, "HemorrhageLevel:OcclusionGroup")

# ----- Test 3: EjectionFraction from TimePoint 60 to 65 -----
ejectionfraction_60_65 <- ejectionfraction %>% filter(TimePoint %in% c(60, 65))
ejectionfraction_60_65$HemorrhageLevel <- as.factor(ejectionfraction_60_65$HemorrhageLevel)
ejectionfraction_60_65$Gender <- as.factor(ejectionfraction_60_65$Gender)

model_ef_60_65 <- lmer(EjectionFraction ~ TimePoint * HemorrhageLevel * OcclusionGroup +CumulativeVasopressin+Plasmalyte+ Norepi+ (1 | SubjectID), data = ejectionfraction_60_65)
summary(model_ef_60_65)

emm_ef_60_65 <- emmeans(model_ef_60_65, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_ef_60_65 <- pairs(emm_ef_60_65, adjust = "tukey")
as.data.frame(tukey_ef_60_65)

# ----- Test 4: EjectionFraction across all TimePoints -----
model_ef_all <- lmer(EjectionFraction ~ TimePoint * HemorrhageLevel * OcclusionGroup + (1 | SubjectID), data = ejectionfraction)
summary(model_ef_all)

emm_ef_all <- emmeans(model_ef_all, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_ef_all <- pairs(emm_ef_all, adjust = "tukey")
as.data.frame(tukey_ef_all)







########### CardiacOutput ###########
# ----- Test 1: CardiacOutput at TimePoint 30 -----
cardiacoutput_30 <- cardiacoutput %>% filter(TimePoint == 30)
cardiacoutput_30$HemorrhageLevel <- as.factor(cardiacoutput_30$HemorrhageLevel)
cardiacoutput_30$Gender <- as.factor(cardiacoutput_30$Gender)

anova_co_30 <- aov(CardiacOutput ~ HemorrhageLevel + Gender + Weightkg, data = cardiacoutput_30)
summary(anova_co_30)
TukeyHSD(anova_co_30, "HemorrhageLevel")

# ----- Test 2: CardiacOutput at TimePoint 60 -----
cardiacoutput_60 <- cardiacoutput %>% filter(TimePoint == 60)
cardiacoutput_60$HemorrhageLevel <- as.factor(cardiacoutput_60$HemorrhageLevel)
cardiacoutput_60$Gender <- as.factor(cardiacoutput_60$Gender)

anova_co_60 <- aov(CardiacOutput ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg, data = cardiacoutput_60)
summary(anova_co_60)
TukeyHSD(anova_co_60, "OcclusionGroup")
TukeyHSD(anova_co_60, "HemorrhageLevel")
TukeyHSD(anova_co_60, "HemorrhageLevel:OcclusionGroup")

# ----- Test 3: CardiacOutput from TimePoint 60 to 65 -----
cardiacoutput_60_65 <- cardiacoutput %>% filter(TimePoint %in% c(60, 65))
cardiacoutput_60_65$HemorrhageLevel <- as.factor(cardiacoutput_60_65$HemorrhageLevel)
cardiacoutput_60_65$Gender <- as.factor(cardiacoutput_60_65$Gender)

model_co_60_65 <- lmer(CardiacOutput ~ TimePoint * HemorrhageLevel * OcclusionGroup + (1 | SubjectID), data = cardiacoutput_60_65)
summary(model_co_60_65)

emm_co_60_65 <- emmeans(model_co_60_65, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_co_60_65 <- pairs(emm_co_60_65, adjust = "tukey")
as.data.frame(tukey_co_60_65)

# ----- Test 4: CardiacOutput across all TimePoints -----
model_co_all <- lmer(CardiacOutput ~ TimePoint * HemorrhageLevel * OcclusionGroup + (1 | SubjectID), data = cardiacoutput)
summary(model_co_all)

emm_co_all <- emmeans(model_co_all, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_co_all <- pairs(emm_co_all, adjust = "tukey")
as.data.frame(tukey_co_all)





########### Elastance ###########
# ----- Test 1: Elastance at TimePoint 30 -----
elastance_30 <- elastance %>% filter(TimePoint == 30)
elastance_30$HemorrhageLevel <- as.factor(elastance_30$HemorrhageLevel)
elastance_30$Gender <- as.factor(elastance_30$Gender)

anova_el_30 <- aov(Elastance ~ HemorrhageLevel + Gender + Weightkg, data = elastance_30)
summary(anova_el_30)
TukeyHSD(anova_el_30, "HemorrhageLevel")

# ----- Test 2: Elastance at TimePoint 60 -----
elastance_60 <- elastance %>% filter(TimePoint == 60)
elastance_60$HemorrhageLevel <- as.factor(elastance_60$HemorrhageLevel)
elastance_60$Gender <- as.factor(elastance_60$Gender)

anova_el_60 <- aov(Elastance ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg, data = elastance_60)
summary(anova_el_60)
TukeyHSD(anova_el_60, "OcclusionGroup")
TukeyHSD(anova_el_60, "HemorrhageLevel")
TukeyHSD(anova_el_60, "HemorrhageLevel:OcclusionGroup")

# ----- Test 3: Elastance from TimePoint 60 to 65 -----
elastance_60_65 <- elastance %>% filter(TimePoint %in% c(60, 65))
elastance_60_65$HemorrhageLevel <- as.factor(elastance_60_65$HemorrhageLevel)
elastance_60_65$Gender <- as.factor(elastance_60_65$Gender)

model_el_60_65 <- lmer(Elastance ~ TimePoint * HemorrhageLevel * OcclusionGroup + (1 | SubjectID), data = elastance_60_65)
summary(model_el_60_65)

emm_el_60_65 <- emmeans(model_el_60_65, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_el_60_65 <- pairs(emm_el_60_65, adjust = "tukey")
as.data.frame(tukey_el_60_65)

# ----- Test 4: Elastance across all TimePoints -----
model_el_all <- lmer(Elastance ~ TimePoint * HemorrhageLevel * OcclusionGroup + (1 | SubjectID), data = elastance)
summary(model_el_all)

emm_el_all <- emmeans(model_el_all, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_el_all <- pairs(emm_el_all, adjust = "tukey")
as.data.frame(tukey_el_all)


########### ESV ###########
# ----- Test 1: ESV at TimePoint 30 -----
esv_30 <- esv %>% filter(TimePoint == 30)
esv_30$HemorrhageLevel <- as.factor(esv_30$HemorrhageLevel)
esv_30$Gender <- as.factor(esv_30$Gender)

anova_esv_30 <- aov(ESV ~ HemorrhageLevel + Gender + Weightkg, data = esv_30)
summary(anova_esv_30)
TukeyHSD(anova_esv_30, "HemorrhageLevel")

# ----- Test 2: ESV at TimePoint 60 -----
esv_60 <- esv %>% filter(TimePoint == 60)
esv_60$HemorrhageLevel <- as.factor(esv_60$HemorrhageLevel)
esv_60$Gender <- as.factor(esv_60$Gender)

anova_esv_60 <- aov(ESV ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg, data = esv_60)
summary(anova_esv_60)
TukeyHSD(anova_esv_60, "OcclusionGroup")
TukeyHSD(anova_esv_60, "HemorrhageLevel")
TukeyHSD(anova_esv_60, "HemorrhageLevel:OcclusionGroup")

# ----- Test 3: ESV from TimePoint 60 to 65 -----
esv_60_65 <- esv %>% filter(TimePoint %in% c(60, 65))
esv_60_65$HemorrhageLevel <- as.factor(esv_60_65$HemorrhageLevel)
esv_60_65$Gender <- as.factor(esv_60_65$Gender)

model_esv_60_65 <- lmer(ESV ~ TimePoint * HemorrhageLevel * OcclusionGroup + (1 | SubjectID), data = esv_60_65)
summary(model_esv_60_65)

emm_esv_60_65 <- emmeans(model_esv_60_65, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_esv_60_65 <- pairs(emm_esv_60_65, adjust = "tukey")
as.data.frame(tukey_esv_60_65)

# ----- Test 4: ESV across all TimePoints -----
model_esv_all <- lmer(ESV ~ TimePoint * HemorrhageLevel * OcclusionGroup + (1 | SubjectID), data = esv)
summary(model_esv_all)

emm_esv_all <- emmeans(model_esv_all, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_esv_all <- pairs(emm_esv_all, adjust = "tukey")
as.data.frame(tukey_esv_all)




########### ESP ###########
# ----- Test 1: ESP at TimePoint 30 -----
esp_30 <- esp %>% filter(TimePoint == 30)
esp_30$HemorrhageLevel <- as.factor(esp_30$HemorrhageLevel)
esp_30$Gender <- as.factor(esp_30$Gender)

anova_esp_30 <- aov(ESP ~ HemorrhageLevel + Gender + Weightkg, data = esp_30)
summary(anova_esp_30)
TukeyHSD(anova_esp_30, "HemorrhageLevel")

# ----- Test 2: ESP at TimePoint 60 -----
esp_60 <- esp %>% filter(TimePoint == 60)
esp_60$HemorrhageLevel <- as.factor(esp_60$HemorrhageLevel)
esp_60$Gender <- as.factor(esp_60$Gender)

anova_esp_60 <- aov(ESP ~ HemorrhageLevel * OcclusionGroup + Gender + Weightkg, data = esp_60)
summary(anova_esp_60)
TukeyHSD(anova_esp_60, "OcclusionGroup")
TukeyHSD(anova_esp_60, "HemorrhageLevel")
TukeyHSD(anova_esp_60, "HemorrhageLevel:OcclusionGroup")

# ----- Test 3: ESP from TimePoint 60 to 65 -----
esp_60_65 <- esp %>% filter(TimePoint %in% c(60, 65))
esp_60_65$HemorrhageLevel <- as.factor(esp_60_65$HemorrhageLevel)
esp_60_65$Gender <- as.factor(esp_60_65$Gender)

model_esp_60_65 <- lmer(ESP ~ TimePoint * HemorrhageLevel * OcclusionGroup + (1 | SubjectID), data = esp_60_65)
summary(model_esp_60_65)

emm_esp_60_65 <- emmeans(model_esp_60_65, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_esp_60_65 <- pairs(emm_esp_60_65, adjust = "tukey")
as.data.frame(tukey_esp_60_65)

# ----- Test 4: ESP across all TimePoints -----
model_esp_all <- lmer(ESP ~ TimePoint * HemorrhageLevel * OcclusionGroup + (1 | SubjectID), data = esp)
summary(model_esp_all)

emm_esp_all <- emmeans(model_esp_all, ~ TimePoint * HemorrhageLevel * OcclusionGroup)
tukey_esp_all <- pairs(emm_esp_all, adjust = "tukey")
as.data.frame(tukey_esp_all)



