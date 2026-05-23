# libraries ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, writexl, dplyr, tidyverse, gridExtra, 
               openxlsx, ggsignif, lubridate, psych, 
               corrplot, janitor, purrr, flextable, officer)

# load --------------------------------------------------------------------
fname2 <- "C:\\Users\\galor\\OneDrive - University of Haifa\\תואר שני\\תזה\\clean files\\BGU_dataset_2024-07-23.xlsx"
narci_master <- read_excel(fname2, sheet = 2)
narci_srs_sch <- read_excel(fname2, sheet = 9)
narci_srs_pres <- read_excel(fname2, sheet = 10)

# Process SRS data --------------------------------------------------------
  # Convert date and age for preschool data
  narci_srs_pres$date_of_evaluation_srs_pr <- convertToDate(narci_srs_pres$date_of_evaluation_srs_pr, origin = "1900-01-01")
narci_srs_pres$age_at_ans_srs_pr <- as.numeric(narci_srs_pres$age_at_ans_srs_pr)

narci_srs_pres$type <- 1
narci_srs_sch$type <- 2

# Merge SRS data
narci_srs <- full_join(narci_srs_pres, narci_srs_sch,
                       by = c("PK", "redcap_repeat_instance",
                              "examiners_name_srs_pr" = "examiners_name_srs_sch",
                              "date_of_evaluation_srs_pr" = "date_of_evaluation_srs_sc",
                              "type", "age_at_ans_srs_pr" = "age_at_ans_srs_sc",
                              "tot_t_srs_pr" = "tot_t_srs_srs_sch")) %>%
  rename(examiners_name_srs = examiners_name_srs_pr,
         age_at_ans_srs = age_at_ans_srs_pr,
         test_date = date_of_evaluation_srs_pr,
         tot_t_srs = tot_t_srs_pr) %>%
  select(PK, type, age_at_ans_srs, examiners_name_srs, test_date, tot_t_srs, )

# Clean data
narci_srs <- narci_srs %>%
  mutate(across(where(is.character), ~na_if(., "NULL")),
         tot_t_srs = as.numeric(tot_t_srs),
         test_date = as.Date(ymd(test_date)))

# Add sex from master
master_sex <- narci_master %>%
  select(PK...1, sex) %>%
  distinct(PK...1, .keep_all = TRUE)

narci_srs <- narci_srs %>%
  left_join(master_sex, by = c("PK" = "PK...1"))

# DIAGNOSTIC CHECKS BEFORE FILTERING =====================================
cat("=== DIAGNOSTIC 1: EXAMINER VALUES BEFORE ANY FILTERING ===\n")
examiner_counts_raw <- narci_srs %>%
  count(examiners_name_srs, sort = TRUE)
print(examiner_counts_raw)

cat("\n=== DIAGNOSTIC 2: EXAMINER VALUES WITH LABELS ===\n")
examiner_labeled <- narci_srs %>%
  mutate(examiner_label = case_when(
    examiners_name_srs == 1 ~ "Mother",
    examiners_name_srs == 2 ~ "Father", 
    examiners_name_srs == 3 ~ "Teacher/Other",
    is.na(examiners_name_srs) ~ "Missing",
    TRUE ~ paste("Unknown:", examiners_name_srs)
  )) %>%
  count(examiners_name_srs, examiner_label, sort = TRUE)
print(examiner_labeled)

# Remove duplicates BEFORE mothers filter to see full picture
narci_srs_clean_pre_filter <- narci_srs %>%
  arrange(PK, test_date) %>%
  distinct(PK, .keep_all = TRUE)

cat("\n=== DIAGNOSTIC 3: EXAMINER DISTRIBUTION AFTER REMOVING DUPLICATES ===\n")
examiner_after_dedup <- narci_srs_clean_pre_filter %>%
  mutate(examiner_label = case_when(
    examiners_name_srs == 1 ~ "Mother",
    examiners_name_srs == 2 ~ "Father", 
    examiners_name_srs == 3 ~ "Teacher/Other",
    is.na(examiners_name_srs) ~ "Missing",
    TRUE ~ paste("Unknown:", examiners_name_srs)
  )) %>%
  count(examiners_name_srs, examiner_label, sort = TRUE) %>%
  mutate(percentage = round(n/sum(n)*100, 1))
print(examiner_after_dedup)

cat("\n\n=== DEMOGRAPHIC SUMMARY OF SRS DATABASE (VALID AGE RANGE) ===\n")

# Create a temporary dataframe from the clean, pre-filtered data.
# This includes both mothers and fathers but applies the same age criteria
# as the final sample to ensure the counts are comparable.
demographics_srs <- narci_srs_clean_pre_filter %>%
  filter(
    (type == 1 & age_at_ans_srs >= 0 & age_at_ans_srs <= 60) |      # Preschool criteria
      (type == 2 & age_at_ans_srs > 60 & age_at_ans_srs <= 228)       # School-age criteria
  ) %>%
  mutate(
    parent = factor(case_when(
      examiners_name_srs == 1 ~ "Mother",
      examiners_name_srs == 2 ~ "Father",
      TRUE ~ "Other"
    ), levels = c("Mother", "Father", "Other")),
    child_gender = factor(case_when(
      sex == 1 ~ "Boy",
      sex == 2 ~ "Girl",
      TRUE ~ "Unknown"
    ), levels = c("Boy", "Girl", "Unknown")),
    age_group = factor(case_when(
      type == 1 ~ "Preschool",
      type == 2 ~ "School-age"
    ), levels = c("Preschool", "School-age"))
  ) %>%
  filter(parent %in% c("Mother", "Father"))

# --- 1. Overall Counts ---
cat("\n--- Total Participants by Role ---\n")
print(demographics_srs %>% count(parent, name = "Total_Count"))
print(demographics_srs %>% count(child_gender, name = "Total_Count"))

# --- 2. Breakdown by Age Group ---
cat("\n--- Breakdown of Participants by Age Group ---\n")
age_breakdown_summary <- demographics_srs %>%
  group_by(age_group) %>%
  summarise(
    Mothers = sum(parent == "Mother"),
    Fathers = sum(parent == "Father"),
    Boys = sum(child_gender == "Boy"),
    Girls = sum(child_gender == "Girl"),
    .groups = 'drop'
  )
print(age_breakdown_summary)

# --- 3. Breakdown of Parents by Child Gender ---
cat("\n--- Crosstab: Parent x Child Gender ---\n")
parent_child_crosstab <- demographics_srs %>%
  count(parent, child_gender, name = "count") %>%
  pivot_wider(
    names_from = child_gender,
    values_from = count,
    values_fill = 0
  )
print(parent_child_crosstab)

# --- 4. Full Breakdown by Age Group, Parent, and Child Gender ---
cat("\n--- Crosstab: Age Group x Parent x Child Gender ---\n")
full_crosstab <- demographics_srs %>%
  group_by(age_group, parent, child_gender) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(
    names_from = child_gender,
    values_from = count,
    values_fill = 0
  )
print(full_crosstab)

cat("\n=== END OF DEMOGRAPHIC SUMMARY ===\n\n")

# --- 4. Full Breakdown by Age Group, Parent, and Child Gender (with Totals) ---
cat("\n--- Crosstab: Age Group x Parent x Child Gender (with Totals) ---\n")

final_crosstab_with_totals <- demographics_srs %>%
  # Create the initial counts and name the new column "count"
  count(age_group, parent, child_gender, name = "count") %>% # <-- THE FIX IS HERE
  # Pivot the child genders into columns
  pivot_wider(
    names_from = child_gender,
    values_from = count, # This now correctly finds the 'count' column
    values_fill = 0
  ) %>%
  # Group by age_group to create subtotals for each group
  group_by(age_group) %>%
  # nest() creates a mini-dataframe for each age_group
  nest() %>%
  # Use map() to apply the totaling functions to each mini-dataframe
  mutate(data = map(data, ~.x %>%
                      # Add a 'Total' column (sums Boys + Girls)
                      adorn_totals("col", name = "Total") %>%
                      # Add a 'Subtotal' row
                      adorn_totals("row", name = "Subtotal"))) %>%
  # Unnest brings the mini-dataframes back into one big table
  unnest(cols = data) %>%
  # Add a 'Grand Total' row at the very end
  adorn_totals("row", name = "Grand Total", fill = "-", na.rm = TRUE)

# Print the final table, ensuring all rows are shown
print(final_crosstab_with_totals, n = Inf)

cat("\n=== END OF DEMOGRAPHIC SUMMARY ===\n\n")


# NOW APPLY MOTHERS FILTER
narci_srs_clean <- narci_srs_clean_pre_filter %>%
  filter(examiners_name_srs == 1)  # Keep only mothers

cat("\n=== DIAGNOSTIC 4: VERIFICATION OF MOTHERS-ONLY FILTER ===\n")
mothers_check <- narci_srs_clean %>%
  count(examiners_name_srs)
cat("Records after mothers-only filter:", nrow(narci_srs_clean), "\n")
cat("All examiner values should be 1 (mothers):\n")
print(mothers_check)

# Check if filter worked correctly
if(all(narci_srs_clean$examiners_name_srs == 1, na.rm = TRUE)) {
  cat("✓ FILTER SUCCESSFUL: All records are from mothers\n")
} else {
  cat("✗ FILTER FAILED: Non-mother records still present\n")
  cat("Non-mother values found:\n")
  print(unique(narci_srs_clean$examiners_name_srs[narci_srs_clean$examiners_name_srs != 1]))
}

# Apply age criteria
narci_age_criteria <- narci_srs_clean %>%
  filter(
    (type == 1 & age_at_ans_srs >= 0 & age_at_ans_srs <= 60) |  # Preschool: 0-60 months
      (type == 2 & age_at_ans_srs > 60 & age_at_ans_srs <= 228)   # School-age: >60-228 months (19 years)
  )

cat("\n=== DIAGNOSTIC 5: AGE CRITERIA APPLICATION ===\n")
cat("Records before age criteria:", nrow(narci_srs_clean), "\n")
cat("Records after age criteria:", nrow(narci_age_criteria), "\n")
cat("Records excluded by age criteria:", nrow(narci_srs_clean) - nrow(narci_age_criteria), "\n")

# Check age distribution by type
age_check <- narci_srs_clean %>%
  mutate(
    age_group = case_when(
      type == 1 & age_at_ans_srs >= 0 & age_at_ans_srs <= 60 ~ "Preschool (valid)",
      type == 1 & (age_at_ans_srs < 0 | age_at_ans_srs > 60) ~ "Preschool (invalid age)",
      type == 2 & age_at_ans_srs > 60 & age_at_ans_srs <= 228 ~ "School-age (valid)",
      type == 2 & (age_at_ans_srs <= 60 | age_at_ans_srs > 228) ~ "School-age (invalid age)",
      is.na(age_at_ans_srs) ~ "Missing age",
      TRUE ~ "Other"
    )
  ) %>%
  count(age_group, sort = TRUE)

cat("\nAge distribution check:\n")
print(age_check)

# Final verification
cat("\n=== DIAGNOSTIC 6: FINAL SAMPLE VERIFICATION ===\n")
final_check <- narci_age_criteria %>%
  mutate(examiner_label = case_when(
    examiners_name_srs == 1 ~ "Mother",
    examiners_name_srs == 2 ~ "Father", 
    examiners_name_srs == 3 ~ "Teacher/Other",
    is.na(examiners_name_srs) ~ "Missing",
    TRUE ~ paste("Unknown:", examiners_name_srs)
  )) %>%
  count(examiner_label)

cat("Final sample composition:\n")
print(final_check)

if(nrow(final_check) == 1 && final_check$examiner_label[1] == "Mother") {
  cat("✓ FINAL VERIFICATION PASSED: Only mothers in final sample\n")
} else {
  cat("✗ FINAL VERIFICATION FAILED: Non-mothers found in final sample\n")
}

cat("\n=== SUMMARY ===\n")
cat("Original records (before deduplication):", nrow(narci_srs), "\n")
cat("After removing duplicates:", nrow(narci_srs_clean_pre_filter), "\n")
cat("After mothers-only filter:", nrow(narci_srs_clean), "\n")
cat("Final sample (after age criteria):", nrow(narci_age_criteria), "\n")

# Visualization -----------------------------------------------------------
# Gender distribution plot
ggplot(gender_dist, aes(x = examiner, y = n, fill = child_gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(0.9), vjust = -0.5) +
  facet_wrap(~ age_group) +
  labs(title = "NARCI: Child Gender Distribution by Parent and Age Group",
       x = "Parent Examiner", y = "Count", fill = "Child Gender") +
  scale_fill_manual(values = c("Male" = "#89CFF0", "Female" = "#F4C2C2")) +
  theme_minimal()

#




# Statistical Analysis ----------------------------------------------------
# Load required packages
pacman::p_load(tidyverse, lme4, lmerTest, emmeans, coin, 
               boot, BayesFactor, pwr, simr, WRS2, 
               ggplot2, car, effsize)

# Prepare data with proper variable names
narci_complete <- narci_age_criteria %>%
  mutate(
    examiner = case_when(
      examiners_name_srs == 1 ~ "Mother",
      examiners_name_srs == 2 ~ "Father",
      TRUE ~ "Other"
    ),
    age_group = ifelse(type == 1, "Preschool", "School-age"),
    age_years = age_at_ans_srs / 12,
    tot_t_srs = as.numeric(tot_t_srs)
  ) %>%
  filter(examiner %in% c("Mother", "Father"), !is.na(tot_t_srs))

# Add child gender and site from master file
narci_complete <- narci_complete %>%
  left_join(narci_master %>% 
              select(PK = PK...1, sex, site = place_of_treatment) %>%
              distinct(PK, .keep_all = TRUE), 
            by = "PK")

narci_complete <- narci_complete |> 
  mutate(child_gender = ifelse(sex.x == 1, "Male", "Female"))

names(narci_complete)
# Verify sample sizes
sample_summary <- narci_complete %>%
  group_by(examiner, age_group) %>%
  summarise(n = n(), .groups = "drop")

# Check normality assumptions
shapiro_results <- narci_complete %>%
  group_by(examiner) %>%
  summarise(
    n = n(),
    shapiro_p = ifelse(n() >= 3, shapiro.test(tot_t_srs)$p.value, NA),
    mean = mean(tot_t_srs, na.rm = TRUE),
    sd = sd(tot_t_srs, na.rm = TRUE)
  )

# Test homogeneity of variance
levene_result <- leveneTest(tot_t_srs ~ examiner, data = narci_complete)


# Welch's t-test for total score
t_test_total <- t.test(tot_t_srs ~ examiner, 
                       data = narci_complete,
                       var.equal = FALSE)

# Non-parametric Mann-Whitney U test
mw_test_total <- wilcox.test(tot_t_srs ~ examiner, 
                             data = narci_complete,
                             exact = FALSE,
                             correct = TRUE)

# Effect sizes
# Cohen's d with Hedges' correction for small samples
d_total <- cohen.d(
  narci_complete$tot_t_srs[narci_complete$examiner == "Mother"],
  narci_complete$tot_t_srs[narci_complete$examiner == "Father"],
  pooled = FALSE,
  hedges.correction = TRUE
)

# Cliff's delta (non-parametric effect size)
delta_total <- cliff.delta(
  narci_complete$tot_t_srs[narci_complete$examiner == "Mother"],
  narci_complete$tot_t_srs[narci_complete$examiner == "Father"]
)

# Summary of initial results
initial_results <- data.frame(
  Test = c("Welch's t-test", "Mann-Whitney U"),
  Statistic = c(t_test_total$statistic, mw_test_total$statistic),
  p_value = c(t_test_total$p.value, mw_test_total$p.value),
  Effect_Size = c(d_total$estimate, delta_total$estimate),
  ES_Type = c("Hedges' g", "Cliff's delta"),
  ES_CI_lower = c(d_total$conf.int[1], delta_total$conf.int[1]),
  ES_CI_upper = c(d_total$conf.int[2], delta_total$conf.int[2])
)


# ----new---------------------------------
file_path <- "C:/Users/galor/Downloads/ABIDEII_Composite_Phenotypic.csv" 

# Load required libraries
library(dplyr)
library(tidyr)

# --- 1. Load and Prepare Data ---

# Set the path to your data file.
# This should be the composite phenotypic file for ABIDE.

# Check if the file exists before attempting to read it.
if (!file.exists(file_path)) {
  stop("Error: The file was not found at the specified path: ", file_path)
}

# Read the raw data.
ABIDE_raw <- read.csv(file_path)

# Create the final analysis sample by filtering and adding descriptive columns.
analysis_data <- ABIDE_raw %>%
  # Select only the necessary columns to start.
  select(SUB_ID, SEX, AGE_AT_SCAN, SRS_INFORMANT, SRS_EDITION, SRS_TOTAL_T, ADOS_G_TOTAL, ADOS_2_TOTAL) %>%
  # Apply all filtering criteria.
  filter(
    SRS_EDITION == 1,                        # Filter for SRS-2 only
    SRS_INFORMANT %in% c(1, 2),              # 1=Mother, 2=Father
    !is.na(SRS_TOTAL_T),                     # Must have a valid SRS score
    !is.na(ADOS_G_TOTAL) | !is.na(ADOS_2_TOTAL), # Must have a valid ADOS score
    !is.na(AGE_AT_SCAN)                      # Must have valid age information
  ) %>%
  # Create new, more readable factor variables.
  mutate(
    informant = factor(case_when(
      SRS_INFORMANT == 1 ~ "Mother",
      SRS_INFORMANT == 2 ~ "Father"
    )),
    child_gender = factor(case_when(
      SEX == 1 ~ "Boy",
      SEX == 2 ~ "Girl"
    )),
    # NOTE: AGE_AT_SCAN is in years. Using a cutoff of 6 years for Preschool.
    # This creates two age groups for analysis.
    age_group = factor(case_when(
      AGE_AT_SCAN < 6 ~ "Preschool",
      AGE_AT_SCAN >= 6 ~ "School-age"
    ))
  )

# --- 2. Function to Generate Summary Statistics ---

# This function calculates all required statistics for a given dataset
# and formats them into a clean row for the final table.
calculate_summary <- function(df, group_name) {
  # Handle cases where a group might have no participants.
  if (nrow(df) == 0) {
    return(tibble(
      Group = group_name,
      `Total SRS` = 0, `# Mothers` = 0, `# Fathers` = 0,
      `# Boys` = 0, `# Girls` = 0,
      `Mothers (B/G)` = "0 / 0", `Fathers (B/G)` = "0 / 0"
    ))
  }
  
  # Create a crosstab of informants by child gender.
  breakdown <- df %>%
    count(informant, child_gender, .drop = FALSE) %>%
    pivot_wider(names_from = child_gender, values_from = n, values_fill = 0)
  
  # Extract counts for mothers and fathers.
  mothers_row <- breakdown %>% filter(informant == "Mother")
  fathers_row <- breakdown %>% filter(informant == "Father")
  
  # Build the final summary tibble for the group.
  tibble(
    Group = group_name,
    `Total SRS` = nrow(df),
    `# Mothers` = sum(df$informant == "Mother"),
    `# Fathers` = sum(df$informant == "Father"),
    `# Boys` = sum(df$child_gender == "Boy"),
    `# Girls` = sum(df$child_gender == "Girl"),
    `Mothers (B/G)` = paste(mothers_row$Boy, "/", mothers_row$Girl),
    `Fathers (B/G)` = paste(fathers_row$Boy, "/", fathers_row$Girl)
  )
}

# --- 3. Generate and Display the Final Summary Table ---

# Calculate summaries for each group (Total, Preschool, School-age).
summary_total <- calculate_summary(analysis_data, "Total")
summary_preschool <- analysis_data %>% filter(age_group == "Preschool") %>% calculate_summary("Preschool")
summary_school_age <- analysis_data %>% filter(age_group == "School-age") %>% calculate_summary("School-age")

# Combine all summaries into a single table.
final_summary_table <- bind_rows(summary_total, summary_preschool, summary_school_age)

# Print the final table to the console.
cat("=== ABIDE II Demographic Summary Table (SRS-2 Only) ===\n\n")
print(final_summary_table, row.names = FALSE)
cat("\n'Mothers (B/G)' and 'Fathers (B/G)' columns show the breakdown of Boys / Girls.\n")

