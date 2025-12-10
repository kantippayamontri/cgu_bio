# TODO: change smoking_status and Gene_X_SNP1 to factor

# install.packages("openxlsx2") # library for reading excel file

# Config parts
patients_biomarkers_dataset_path <- "./datasets/patients_biomarkers.xlsx"
patients_biomarkers_dataset_path_sheet_name <- "student_phenotypes"
patients_outputs_dataset_path <- "./datasets/patients_outcomes.xlsx"
patients_outputs_dataset_path_sheet_name <- "patients_outcomes"
breast_cancer_dataset_path <- "./datasets/Breast_Cancer.xlsx"
breast_cancer_dataset_path_sheet_name <- "Breast_Cancer"
final_clean_data <- "./cleaned_datasets.xlsx"

# IMPORTANT: 1. construcing and initial cohort -> merge patients_biomarker and patients_outcomes together
# load patients_biomarkers.xlsx file with have "student_phenotypes" and "code_book" sheet name

library(openxlsx2)
# HEADER1: read patients_biomarkers file with sheet name
patients_biomarkers <- read_xlsx(patients_biomarkers_dataset_path, patients_biomarkers_dataset_path_sheet_name)

head(patients_biomarkers)
str(patients_biomarkers)
summary(patients_biomarkers)

# HEADER1: read patients_outcomes file with sheet name
patients_outcomes <- read_xlsx(patients_outputs_dataset_path, patients_outputs_dataset_path_sheet_name)
head(patients_outcomes)
str(patients_outcomes)

# HEADER1: find the duplicated patient_id in both patients_biomarkers and patients_outcomes

# HEADER2: Number of rows
nrow(patients_biomarkers) # found 4024

# HEADER2: Number of duplicate patient IDs (IDs that appear more than once)
sum(duplicated(patients_biomarkers$Patient_ID))

# HEADER2: Number of rows
nrow(patients_outcomes) # found 4019

# HEADER2: Number of unique patient IDs
sum(duplicated(patients_outcomes$Patient_ID)) # found 5

# NOTE: Checkpoint Question 1: How many duplicate Patient_ID entries did you find in the  <U+201C>patients_outcomes<U+201D> file before any merging or cleaning?
# ANS: 5

# HEADER2: deal with duplicate patient IDs
patients_outcomes_dup_row <- patients_outcomes$Patient_ID[duplicated(patients_outcomes$Patient_ID)]
# All rows with duplicated Patient_IDs
patients_outcomes[patients_outcomes$Patient_ID %in% patients_outcomes_dup_row, ] # we found that it is just the
# Remove rows that are identical across all columns
patients_outcomes <- patients_outcomes[!duplicated(patients_outcomes), ]
nrow(patients_outcomes) # we will found 4014
sum(duplicated(patients_outcomes$Patient_ID)) # found 0


# HEADER1: merge 2 files together
merged_data <- merge(patients_biomarkers, patients_outcomes, by = "Patient_ID", all.x = TRUE)
head(merged_data)
nrow(merged_data) # found 4024 -> use left join, NOTE: if use left join I will get 4014 (10 unmatched)
sum(is.na(merged_data$event)) # 10 the number of patient_id that found on patients_biomarkers and not found in patients_outcomes
sum(is.na(merged_data$Survival.Months)) # 10 the number of patient_id that found on patients_biomarkers and not found in patients_outcomes

# IMPORTANT: 2. quality control and plausibility assesments
# Using the <U+201C>code book<U+201D> dictionaries, perform a thorough plausibility check on all  variables in your merged dataset. Identify all data points that violate the  provided normal ranges or logical consistency.
# For each anomaly, describe your corrective action (e.g., setting to NA) and justify  your choice.

# HEADER1: check BMI
range(merged_data$BMI) # min=11.25304 max=70.000 -> the Plausible Range [15.0 - 50.0]

# HEADER2: Check how many values are out of range
sum(merged_data$BMI < 15 | merged_data$BMI > 50, na.rm = TRUE) # found 26 -> 26 of 4019 is only 0.64 percent -> remove the

# HEADER2: View the problematic rows
merged_data[merged_data$BMI < 15 | merged_data$BMI > 50, ]$BMI

# HEADER2: set out-of-range BMI to NA
merged_data$BMI[merged_data$BMI < 15 | merged_data$BMI > 50] <- NA
sum(is.na(merged_data$BMI)) # 26 the same number as number of out-of-range

# HEADER1: check Smoking_Status
unique(merged_data$Smoking_Status) # found "Smoker", "Non-Smoker", "NA", "smoker", " Yes", "Smokerr"
length(unique(merged_data$Smoking_Status)) # 6 classes unique
nrow(merged_data[merged_data$Smoking_Status == "NA", ]) # we found 80 records that contains NA in term of sting "NA"
sum(is.na(merged_data$Smoking_Status)) # we not found NA in Smoking_Status

# HEADER2: First, trim whitespace and convert to lowercase for consistency
merged_data$Smoking_Status_clean <- tolower(trimws(merged_data$Smoking_Status))
unique(merged_data$Smoking_Status_clean)
length(unique(merged_data$Smoking_Status_clean))

# HEADER2: Handle the string "NA" and typos
merged_data$Smoking_Status_clean[merged_data$Smoking_Status_clean == "na"] <- NA
merged_data$Smoking_Status_clean[merged_data$Smoking_Status_clean == "smokerr"] <- "smoker"
merged_data$Smoking_Status_clean[merged_data$Smoking_Status_clean == "yes"] <- "smoker"

# count the number of NA, smoker and non-smoker
table(merged_data$Smoking_Status_clean, useNA = "ifany")
# non-smoker     smoker       <NA>
#       2705       1239         80

# HEADER2: Convert to factor instead of boolean
merged_data$Smoking_Status <- factor(
    ifelse(merged_data$Smoking_Status_clean == "smoker", "smoker",
        ifelse(merged_data$Smoking_Status_clean == "non-smoker", "non-smoker", NA)
    ),
    levels = c("non-smoker", "smoker")
)

# Verify conversion
str(merged_data$Smoking_Status)
table(merged_data$Smoking_Status, useNA = "ifany")
# non-smoker     smoker       <NA> 
#       2705       1239         80 

# HEADER2: drop the unused columns
merged_data$Smoking_Status_clean <- NULL
merged_data$Is_Smoker <- NULL # Remove the boolean version

# HEADER1: check Gene_X_SNP1
unique(merged_data$Gene_X_SNP1) # [1] "Allele_B (Risk)" "Allele_A" "Allele_B (risk)"
# Convert to lowercase for consistency
merged_data$Gene_X_SNP1_clean <- tolower(merged_data$Gene_X_SNP1)
unique(merged_data$Gene_X_SNP1_clean) # [1] "allele_b (risk)" "allele_a"

table(merged_data$Gene_X_SNP1_clean)
#    allele_a allele_b (risk)
#        2792            1232

# # HEADER2: Convert to Boolean (1 = risk allele, 0 = non-risk allele)
# merged_data$Has_Risk_Allele <- ifelse(merged_data$Gene_X_SNP1_clean == "allele_b (risk)", 1, 0)

# HEADER2: Convert to factor (allele_a as reference level = non-risk)
merged_data$Gene_X_SNP1 <- factor(merged_data$Gene_X_SNP1_clean,
    levels = c("allele_a", "allele_b (risk)")
)
# NOTE: make Gene_X_SNP1 as factor type
merged_data$Gene_X_SNP1_clean <- NULL # drop the intermediate cleaning column

# Factor levels (internal storage):
#
# "allele_a" = level 1 (stored internally as integer 1)
# "allele_b (risk)" = level 2 (stored internally as integer 2)

table(merged_data$Gene_X_SNP1)

# Verify conversion
str(merged_data$Gene_X_SNP1)
table(merged_data$Gene_X_SNP1)
# allele_a allele_b (risk)
#            2792            1232

# HEADER1: check Biomarker_A
range(merged_data$Biomarker_A) # -117.5337  195.2368
# we need to make the Biomarker_A which value < 0 to NA because it is not possible

# HEADER2: Check how many values are negative
sum(merged_data$Biomarker_A < 0, na.rm = TRUE) # 3

# HEADER2: View the problematic values
merged_data$Biomarker_A[merged_data$Biomarker_A < 0] # -117.53370 -115.52958  -72.85441

# HEADER2: Set negative values to NA
merged_data$Biomarker_A[merged_data$Biomarker_A < 0] <- NA

# HEADER2: Verify the correction
sum(is.na(merged_data$Biomarker_A)) # 3
range(merged_data$Biomarker_A, na.rm = TRUE) # Should show min >= 0
# [1]  15.43171 195.23680

# HEADER1: check Biomarker_B

range(merged_data$Biomarker_B, na.rm = TRUE) # [1] -551.8600  170.1654

# HEADER2: Check how many values are negative
sum(merged_data$Biomarker_B < 0, na.rm = TRUE) # 96

# HEADER2: View the problematic values
merged_data$Biomarker_B[merged_data$Biomarker_B < 0] #

# HEADER2: Set negative values to NA
merged_data$Biomarker_B[merged_data$Biomarker_B < 0] <- NA

# HEADER2: Verify the correction
sum(is.na(merged_data$Biomarker_B))
range(merged_data$Biomarker_B, na.rm = TRUE) # Should show min >= 0
# [1]   0.7097661 170.1653846

# HEADER1: check Lab_Value_Y
range(merged_data$Lab_Value_Y, na.rm = TRUE) # [1] "1.57591033468796" "NA" -> shows that Lab_Value_Y is stored as a character/string type instead of numeric. The "NA" is a string, not an actual missing value, and all numbers are text.

nrow(merged_data[merged_data$Lab_Value_Y == "NA", ]) # 120 of "NA"

# Convert to numeric (string "NA" will become actual NA)
merged_data$Lab_Value_Y <- as.numeric(merged_data$Lab_Value_Y)

# HEADER2: Check how many values are negative
sum(merged_data$Lab_Value_Y < 0, na.rm = TRUE) # 0 -> no value of data that < 0

# # HEADER2: Set negative values to NA
# merged_data$Lab_Value_Y[merged_data$Lab_Value_Y < 0] <- NA

# HEADER2: Verify the correction
sum(is.na(merged_data$Lab_Value_Y))
range(merged_data$Lab_Value_Y, na.rm = TRUE) # Should show min >= 0

# HEADER1: check Biomarker_C1
range(merged_data$Biomarker_C1, na.rm = TRUE) # [1]  -8.428749 489.188374

# HEADER2: Check how many values are negative
sum(merged_data$Biomarker_C1 < 0, na.rm = TRUE) # 3

# HEADER2: View the problematic values
merged_data$Biomarker_C1[merged_data$Biomarker_C1 < 0] # [1] -0.902186 -3.726261 -8.428749

# HEADER2: Set negative values to NA
merged_data$Biomarker_C1[merged_data$Biomarker_C1 < 0] <- NA

# HEADER2: Verify the correction
sum(is.na(merged_data$Biomarker_C1)) # 3
range(merged_data$Biomarker_C1, na.rm = TRUE) # Should show min >= 0 -> [1]   1.700193 489.188374

# HEADER1: check Biomarker_C2
range(merged_data$Biomarker_C2, na.rm = TRUE) # [1]  15.68146 312.50811
sum(is.na(merged_data$Biomarker_C2)) # 0 -> no NA row data

# HEADER1: Blood_Pressure_Systolic
range(merged_data$Blood_Pressure_Systolic) # [1]  90.26483 174.39127
sum(is.na(merged_data$Blood_Pressure_Systolic)) # [1] 0

# HEADER1: Metabolic_Marker_Z
range(merged_data$Metabolic_Marker_Z) # [1] 0.2699379 1.7015720
sum(is.na(merged_data$Metabolic_Marker_Z)) # 0

str(merged_data)
# 'data.frame':   4024 obs. of  13 variables:
#  $ Patient_ID             : num  1 2 3 4 5 6 7 8 9 10 ...
#  $ BMI                    : num  31 22.9 27.6 27.8 26.8 ...
#  $ Biomarker_A            : num  56 89.4 109.8 73.7 92.8 ...
#  $ Biomarker_B            : num  91.7 121 102.9 100.2 75 ...
#  $ Lab_Value_Y            : num  17.47 9.41 27.15 9.69 10.78 ...
#  $ Biomarker_C1           : num  60.4 87.3 111.9 37.5 32.5 ...
#  $ Biomarker_C2           : num  105.8 116 101.6 69.9 51.5 ...
#  $ Blood_Pressure_Systolic: num  144 110 140 130 132 ...
#  $ Metabolic_Marker_Z     : num  1.176 0.688 0.904 0.921 1.192 ...
#  $ Survival.Months        : num  60 62 75 84 50 89 54 14 70 92 ...
#  $ event                  : num  0 0 0 0 0 0 0 1 0 0 ...
#  $ Is_Smoker              : num  1 0 1 1 0 1 1 1 0 0 ...
#  $ Has_Risk_Allele        : num  1 0 0 0 0 0 0 1 1 1 ...

# HEADER1: Min/Max summary table
summary_df <- data.frame(
    Variable = names(merged_data)[-1], # Exclude Patient_ID
    Min = sapply(merged_data[-1], min, na.rm = TRUE),
    Max = sapply(merged_data[-1], max, na.rm = TRUE)
)
print(summary_df)
#                                        Variable        Min        Max
# BMI                                         BMI 15.0441980  39.788638
# Biomarker_A                         Biomarker_A 15.4317137 195.236803
# Biomarker_B                         Biomarker_B  0.7097661 170.165385
# Lab_Value_Y                         Lab_Value_Y  1.5759103 231.195945
# Biomarker_C1                       Biomarker_C1  1.7001932 489.188374
# Biomarker_C2                       Biomarker_C2 15.6814609 312.508109
# Blood_Pressure_Systolic Blood_Pressure_Systolic 90.2648289 174.391266
# Metabolic_Marker_Z           Metabolic_Marker_Z  0.2699379   1.701572
# Survival.Months                 Survival.Months  1.0000000 107.000000
# event                                     event  0.0000000   1.000000
# Is_Smoker                             Is_Smoker  0.0000000   1.000000
# Has_Risk_Allele                 Has_Risk_Allele  0.0000000   1.000000

# IMPORTANT: 3. harmonizing inconsistent data

# HEADER1: check Breast_Cancer.xlsx
breast_cancer <- read_xlsx(breast_cancer_dataset_path, breast_cancer_dataset_path_sheet_name)
str(breast_cancer)
# 'data.frame':   4024 obs. of  15 variables:
#  $ Patient_ID            : num  1 2 3 4 5 6 7 8 9 10 ...
#  $ Age                   : num  68 50 58 58 47 51 51 40 40 69 ...
#  $ Race                  : chr  "White" "White" "White" "White" ...
#  $ Marital Status        : chr  "Married" "Married" "Divorced" "Married" ...
#  $ T Stage               : chr  "T1" "T2" "T3" "T1" ...
#  $ N Stage               : chr  "N1" "N2" "N3" "N1" ...
#  $ 6th Stage             : chr  "IIA" "IIIA" "IIIC" "IIA" ...
#  $ differentiate         : chr  "Poorly differentiated" "Moderately differentiated" "Moderately differentiated" "Poorly differentiated" ...
#  $ Grade                 : chr  "3" "2" "2" "3" ...
#  $ A Stage               : chr  "Regional" "Regional" "Regional" "Regional" ...
#  $ Tumor Size            : num  4 35 63 18 41 20 8 30 103 32 ...
#  $ Estrogen Status       : chr  "Positive" "Positive" "Positive" "Positive" ...
#  $ Progesterone Status   : chr  "Positive" "Positive" "Positive" "Positive" ...
#  $ Regional Node Examined: num  24 14 14 2 3 18 11 9 20 21 ...
#  $ Reginol Node Positive : num  1 5 7 1 1 2 1 1 18 12 ...
# HEADER1: check Age
# Check if Age contains only whole numbers
all(breast_cancer$Age == floor(breast_cancer$Age)) # TRUE
range(breast_cancer$Age) # 30-69
str(breast_cancer$Age) # num [1:4024] 68 50 58 58 47 51 51 40 40 69 ... -> num is a float -> need to change to integer
breast_cancer$Age <- as.integer(breast_cancer$Age)
str(breast_cancer$Age)

# HEADER1: check Race - > change to factor
unique(breast_cancer$Race) # [1] "White" "Black" "Other"
# Standardize to lowercase
breast_cancer$Race <- tolower(breast_cancer$Race)
unique(breast_cancer$Race) # [1] "white" "black" "other"
# Convert to factor
breast_cancer$Race <- factor(breast_cancer$Race,
    levels = c("white", "black", "other")
)
# Verify
str(breast_cancer$Race)
table(breast_cancer$Race)

# HEADER1: check Marital.Status - >change to factors
unique(breast_cancer$`Marital Status`) # [1] "Married"   "Divorced"  "Single "   "Widowed"   "Separated" -> single have white space

# Trim whitespace first (notice "Single " has trailing space)
breast_cancer$`Marital Status` <- trimws(breast_cancer$`Marital Status`)

breast_cancer$`Marital Status` <- tolower(breast_cancer$`Marital Status`)
unique(breast_cancer$`Marital Status`) # [1] "married"   "divorced"  "single "   "widowed"   "separated"

# Convert to factor
breast_cancer$`Marital Status` <- factor(breast_cancer$`Marital Status`,
    levels = c("married", "divorced", "single", "separated", "widowed")
)

# Verify
str(breast_cancer$`Marital Status`)
table(breast_cancer$`Marital Status`)

# HEADER1: check T.Stage -> orinal
str(breast_cancer$`T Stage`) # chr type
unique(breast_cancer$`T Stage`) # [1] "T1" "T2" "T3" "T4"

# Standardize to lowercase
breast_cancer$`T Stage` <- tolower(trimws(breast_cancer$`T Stage`))
unique(breast_cancer$`T Stage`)

# Convert to ORDERED factor (T1 < T2 < T3 < T4)
breast_cancer$`T Stage` <- factor(breast_cancer$`T Stage`,
    levels = c("t1", "t2", "t3", "t4"),
    ordered = TRUE # ordinal data with meaningful sequence
)

# Verify
str(breast_cancer$`T Stage`)
table(breast_cancer$`T Stage`)

# HEADER1: check N.Stage -> orinal
colnames(breast_cancer)
str(breast_cancer$`N Stage`) # chr type
unique(breast_cancer$`N Stage`)

# HEADER2: Standardize to lowercase
breast_cancer$`N Stage` <- tolower(trimws(breast_cancer$`N Stage`))
unique(breast_cancer$`N Stage`)

# Convert to ORDERED factor (N1 < N2 < N3)
breast_cancer$`N Stage` <- factor(breast_cancer$`N Stage`,
    levels = c("n1", "n2", "n3"),
    ordered = TRUE # ordinal data with meaningful sequence
)

# Verify
str(breast_cancer$`N Stage`)
table(breast_cancer$`N Stage`)

# HEADER1: check X6th.Stage field (6th Stage)
colnames(breast_cancer)
unique(breast_cancer$`6th Stage`) # [1] "IIA"  "IIIA" "IIIC" "IIB"  "IIIB"

# Standardize to lowercase
breast_cancer$`6th Stage` <- tolower(trimws(breast_cancer$`6th Stage`))
unique(breast_cancer$`6th Stage`)

# Convert to ORDERED factor (IIA < IIB < IIIA < IIIB < IIIC)
breast_cancer$`6th Stage` <- factor(breast_cancer$`6th Stage`,
    levels = c("iia", "iib", "iiia", "iiib", "iiic"),
    ordered = TRUE # ordinal data with meaningful sequence
)

# Verify
str(breast_cancer$`6th Stage`)
table(breast_cancer$`6th Stage`)

# FIXME: how to order the ordinal
# HEADER1: check differentiate field
unique(breast_cancer$differentiate)
# [1] "Poorly differentiated"     "Moderately differentiated"
# [3] "Well differentiated"       "Undifferentiated"

# Standardize to lowercase
breast_cancer$differentiate <- tolower(trimws(breast_cancer$differentiate))
unique(breast_cancer$differentiate)

# Convert to ORDERED factor (Well differentiated = best to Undifferentiated = worst)
breast_cancer$differentiate <- factor(breast_cancer$differentiate,
    levels = c("well differentiated", "moderately differentiated", "poorly differentiated", "undifferentiated"),
    ordered = TRUE # ordinal data with meaningful sequence
)

# Verify
str(breast_cancer$differentiate)
table(breast_cancer$differentiate)

# HEADER1: check Grade field
unique(breast_cancer$Grade)
# [1] "3"                     "2"                     "1"
# [4] " anaplastic; Grade IV"

# Standardize: trim whitespace and fix the anaplastic entry
breast_cancer$Grade <- trimws(breast_cancer$Grade)
breast_cancer$Grade[breast_cancer$Grade == "anaplastic; Grade IV"] <- "4"
unique(breast_cancer$Grade)

# Convert to ordered factor (1 = best to 4 = worst)
breast_cancer$Grade <- factor(breast_cancer$Grade,
    levels = c("1", "2", "3", "4"),
    ordered = TRUE # ordinal data with meaningful sequence
)

# Verify
str(breast_cancer$Grade)
table(breast_cancer$Grade)

# HEADER1: check A.Stage field
unique(breast_cancer$`A Stage`) # [1] "Regional" "Distant"

# Standardize to lowercase
breast_cancer$`A Stage` <- tolower(trimws(breast_cancer$`A Stage`))
unique(breast_cancer$`A Stage`)

# Convert to ORDERED factor (Regional = better prognosis < Distant = worse/metastasis)
breast_cancer$`A Stage` <- factor(breast_cancer$`A Stage`,
    levels = c("regional", "distant"),
    ordered = TRUE # ordinal data: regional is better than distant
)

# need to explain for ordered factor
# Regional = cancer has spread to nearby lymph nodes (better prognosis)
# Distant = metastasis to distant organs (worse prognosis)
# This is an ordinal variable with a clear progression: Regional < Distant.

# Verify
str(breast_cancer$`A Stage`)
table(breast_cancer$`A Stage`)

# HEADER1: check Tumor.Size field
range(breast_cancer$`Tumor Size`) # [1]   1 140
sum(is.na(breast_cancer$`Tumor Size`)) # 0

# HEADER1: check Estrogen.Status field
unique(breast_cancer$`Estrogen Status`) # [1] "Positive" "Negative" -> change to factor type without order cuz binary variable is not ordinal

# Standardize to lowercase
breast_cancer$`Estrogen Status` <- tolower(trimws(breast_cancer$`Estrogen Status`))
unique(breast_cancer$`Estrogen Status`)

# Convert to factor (Positive as reference level)
breast_cancer$`Estrogen Status` <- factor(breast_cancer$`Estrogen Status`,
    levels = c("positive", "negative")
)

# Verify
str(breast_cancer$`Estrogen Status`)
table(breast_cancer$`Estrogen Status`)
# positive negative
#     3755      269

# HEADER1: check Progesterone.Status field
unique(breast_cancer$`Progesterone Status`) # [1] "Positive" "Negative"

# Standardize to lowercase
breast_cancer$`Progesterone Status` <- tolower(trimws(breast_cancer$`Progesterone Status`))
unique(breast_cancer$`Progesterone Status`)

# Convert to factor (Positive as reference level)
breast_cancer$`Progesterone Status` <- factor(breast_cancer$`Progesterone Status`,
    levels = c("positive", "negative")
)

# Verify
str(breast_cancer$`Progesterone Status`)
table(breast_cancer$`Progesterone Status`)

# HEADER1: check Regional.Node.Examined field
colnames(breast_cancer)
range(breast_cancer$`Regional Node Examined`) # [1]  1 61
str(breast_cancer$`Regional Node Examined`) # num [1:4024] 24 14 14 2 3 18 11 9 20 21 ...

# Convert to integer
breast_cancer$`Regional Node Examined` <- as.integer(breast_cancer$`Regional Node Examined`)

# Verify
str(breast_cancer$`Regional Node Examined`)

# HEADER1: check Reginol.Node.Positive field
colnames(breast_cancer)
range(breast_cancer$`Reginol Node Positive`) # [1]  1 46
str(breast_cancer$`Reginol Node Positive`) # num [1:4024] 1 5 7 1 1 2 1 1 18 12 ...

# Convert to integer
breast_cancer$`Reginol Node Positive` <- as.integer(breast_cancer$`Reginol Node Positive`)

# Verify
str(breast_cancer$`Reginol Node Positive`)

colnames(breast_cancer)

# HEADER1: Quality control for lymph node data consistency
# HEADER2: Check logical consistency between examined and positive nodes
# Count how many records violate the rule (positive > examined)
sum(breast_cancer$`Reginol Node Positive` > breast_cancer$`Regional Node Examined`, na.rm = TRUE)

# HEADER2: Identify illogical rows
illogical_rows <- breast_cancer$`Reginol Node Positive` > breast_cancer$`Regional Node Examined`
illogical_rows[is.na(illogical_rows)] <- FALSE  # Handle NA values

# HEADER2: View the problematic cases
breast_cancer[illogical_rows, c("Patient_ID", "Regional Node Examined", "Reginol Node Positive")]

# HEADER2: Set both values to NA for illogical cases
# Rationale: If the data is inconsistent, we cannot trust either value
breast_cancer$`Regional Node Examined`[illogical_rows] <- NA
breast_cancer$`Reginol Node Positive`[illogical_rows] <- NA

# HEADER2: Verify correction
sum(breast_cancer$`Reginol Node Positive` > breast_cancer$`Regional Node Examined`, na.rm = TRUE)  # Should be 0

# IMPORTANT: 4. creating the final analytical cohort
colnames(merged_data)

# HEADER1: Merge merged_data and breast_cancer datasets
final_cohort <- merge(merged_data, breast_cancer, by = "Patient_ID", all = FALSE)

# Verify the merge
nrow(final_cohort) # [1] 4024
nrow(merged_data) # [1] 4024
nrow(breast_cancer) # [1] 4024

# Check structure
str(final_cohort)
colnames(final_cohort)

# Check for any unmatched records
sum(!merged_data$Patient_ID %in% breast_cancer$Patient_ID)  # Records in merged_data not in breast_cancer
sum(!breast_cancer$Patient_ID %in% merged_data$Patient_ID)  # Records in breast_cancer not in merged_data

# HEADER1: Complete-case analysis - remove rows with any missing values
# HEADER2: Check how many rows have missing values before removal
sum(!complete.cases(final_cohort))

# HEADER2: Identify which columns have missing values and how many
colSums(is.na(final_cohort))

# HEADER2: Remove rows with any NA values
final_cohort <- final_cohort[complete.cases(final_cohort), ]

# HEADER2: Verify removal
nrow(final_cohort) # 3694
sum(!complete.cases(final_cohort))  # Should be 0
colSums(is.na(final_cohort))  # All should be 0

# HEADER1: Save the final cleaned cohort to Excel file
write_xlsx(final_cohort, final_clean_data)

# Verify the file was created
cat("Final cleaned dataset saved to:", final_clean_data, "\n")
cat("Number of observations:", nrow(final_cohort), "\n")
cat("Number of variables:", ncol(final_cohort), "\n")
