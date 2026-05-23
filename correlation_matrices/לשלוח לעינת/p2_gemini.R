# -------------------------------------------------------------------------
# Phase 3: Master Profile Plots (Pattern Visualisation)
# -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, patchwork)

# 1. Load & Prep Data -----------------------------------------------------
files <- c(
  MIND_Preschool_Boys  = "MIND_Preschool_Boys.rds",
  MIND_Preschool_Girls = "MIND_Preschool_Girls.rds",
  MIND_Schoolage_Boys  = "MIND_Schoolage_Boys.rds",
  MIND_Schoolage_Girls = "MIND_Schoolage_Girls.rds",
  SSC_Preschool_Boys   = "SSC_Preschool_Boys.rds",
  SSC_Preschool_Girls  = "SSC_Preschool_Girls.rds",
  SSC_Schoolage_Boys   = "SSC_Schoolage_Boys.rds",
  SSC_Schoolage_Girls  = "SSC_Schoolage_Girls.rds"
)
raw_list <- map(files, readRDS)

# Extract Means
extract_stats <- function(obj, filename) {
  parts <- str_split(filename, "_")[[1]]
  sex   <- tail(parts, 1)
  cohort <- paste(head(parts, -1), collapse = "_") 
  
  obj$descriptives %>%
    select(measure, mean, sd) %>%
    mutate(sex = sex, cohort = cohort)
}

big_data <- imap_dfr(raw_list, \(obj, name) extract_stats(obj, name))

# --- CRITICAL STEP: Z-Score Normalization ---
# This ensures ADOS (scale 0-10) and SRS (scale 40-90) can be plotted on one line.
# We group by Measure to normalize each variable to itself.
big_data_z <- big_data %>%
  group_by(measure) %>%
  mutate(
    z_score = scale(mean), # Converts to std deviations
    # Create a clean label for the plot
    label = paste0(round(mean, 1)) 
  ) %>%
  ungroup()

# 2. View A: Compare SEXES (Within one Cohort) ----------------------------
# "Do Boys and Girls have different symptom shapes?"
plot_profile_sex <- function(data, target_cohort, use_z = F) {
  
  # Switch between Raw scores and Z-scores
  y_var <- if(use_z) "z_score" else "mean"
  y_lab <- if(use_z) "Z-Score (Standardized Pattern)" else "Raw Mean Score"
  
  sub <- data %>% filter(cohort == target_cohort)
  
  ggplot(sub, aes(x = measure, y = .data[[y_var]], group = sex, color = sex)) +
    geom_hline(yintercept = 0, color = "grey80") + # Reference line
    geom_line(size = 1.2, alpha = 0.8) +
    geom_point(size = 3) +
    # Optional: Add labels to points so you can see the real numbers
    geom_text(aes(label = label), vjust = -1, size = 3, show.legend = FALSE) +
    scale_color_manual(values = c("Boys" = "blue3", "Girls" = "red3")) +
    labs(title = paste("Clinical Profile:", target_cohort),
         subtitle = "Lines show the 'shape' of the phenotype",
         y = y_lab, x = NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# 3. View B: Compare COHORTS (Longitudinal Trend) -------------------------
# "Does the pattern change as they get older?" (e.g. Preschool vs Schoolage)
plot_profile_cohort <- function(data, target_sex, use_z = FALSE) {
  
  y_var <- if(use_z) "z_score" else "mean"
  
  sub <- data %>% filter(sex == target_sex)
  
  ggplot(sub, aes(x = measure, y = .data[[y_var]], group = cohort, color = cohort)) +
    geom_line(size = 1, alpha = 0.8) +
    geom_point(size = 2) +
    labs(title = paste("Developmental Trajectory:", target_sex),
         subtitle = "Comparing cohorts across measures",
         y = "Score", x = NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# 4. View C: The Master Grid ----------------------------------------------
# All cohorts, Boys vs Girls side-by-side
plot_profile_master <- function(data) {
  ggplot(data, aes(x = measure, y = z_score, group = sex, color = sex)) +
    geom_hline(yintercept = 0, color = "grey80") +
    geom_line(size = 1) +
    geom_point(size = 1.5) +
    facet_wrap(~cohort, ncol = 2) +
    scale_color_manual(values = c("Boys" = "blue3", "Girls" = "red3")) +
    labs(title = "Master Profile Overview (Z-Scored)",
         y = "Standard Deviation from Mean", x = NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
}


# 5. EXECUTION ------------------------------------------------------------

# --- A. List of Sex Comparisons (Within Cohort) ---
# Set use_z = TRUE to see the PATTERN (recommended).
# Set use_z = FALSE to see the RAW numbers (lines will look jumpy).
unique_cohorts <- unique(big_data_z$cohort)
list_profiles <- map(unique_cohorts, ~plot_profile_sex(big_data_z, .x, use_z = TRUE))
names(list_profiles) <- unique_cohorts

# --- B. List of Cohort Comparisons (Longitudinal) ---
list_trends <- list(
  Boys  = plot_profile_cohort(big_data_z, "Boys"),
  Girls = plot_profile_cohort(big_data_z, "Girls")
)

# --- C. Master Overview ---
p_master_profile <- plot_profile_master(big_data_z)


# 6. DISPLAY --------------------------------------------------------------

# Example 1: The Profile for MIND Preschool
# Look for: Do lines cross? (Interaction effect)
list_profiles$MIND_Preschool
list_profiles$MIND_Schoolage
list_profiles$SSC_Preschool
list_profiles$SSC_Schoolage

# Example 2: How do Boys change from Preschool to Schoolage?
# Look for: Are the lines parallel (stable) or do they separate (developmental shift)?
list_trends$Boys
list_trends$Girls
# Example 3: The Big Picture
p_master_profile




# -------------------------------------------------------------------------
# Phase 3: Raw Profile Plots (Absolute Means Only)
# -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, patchwork)

# 1. Load Data ------------------------------------------------------------
files <- c(
  MIND_Preschool_Boys  = "MIND_Preschool_Boys.rds",
  MIND_Preschool_Girls = "MIND_Preschool_Girls.rds",
  MIND_Schoolage_Boys  = "MIND_Schoolage_Boys.rds",
  MIND_Schoolage_Girls = "MIND_Schoolage_Girls.rds",
  SSC_Preschool_Boys   = "SSC_Preschool_Boys.rds",
  SSC_Preschool_Girls  = "SSC_Preschool_Girls.rds",
  SSC_Schoolage_Boys   = "SSC_Schoolage_Boys.rds",
  SSC_Schoolage_Girls  = "SSC_Schoolage_Girls.rds"
)
raw_list <- map(files, readRDS)

# 2. Extract Raw Means ----------------------------------------------------
extract_raw_stats <- function(obj, filename) {
  parts <- str_split(filename, "_")[[1]]
  sex   <- tail(parts, 1)
  cohort <- paste(head(parts, -1), collapse = "_") 
  
  obj$descriptives %>%
    select(measure, mean, sd) %>%
    mutate(
      sex = sex, 
      cohort = cohort,
      # Create a label for the points (e.g. "72.4")
      label = as.character(round(mean, 1))
    )
}

big_data_raw <- imap_dfr(raw_list, \(obj, name) extract_raw_stats(obj, name))

# 3. View A: Compare SEXES (Within Cohort) --------------------------------
plot_raw_profile_sex <- function(data, target_cohort) {
  
  sub <- data %>% filter(cohort == target_cohort)
  
  ggplot(sub, aes(x = measure, y = mean, group = sex, color = sex)) +
    # The Lines
    geom_line(size = 1.2, alpha = 0.8) +
    # The Points
    geom_point(size = 3) +
    # The Labels (So you can read the actual score on the graph)
    geom_text(aes(label = label), vjust = -1, size = 3.5, show.legend = FALSE) +
    
    scale_color_manual(values = c("Boys" = "blue3", "Girls" = "red3")) +
    labs(title = paste("Raw Clinical Profile:", target_cohort),
         subtitle = "Actual Mean Scores (No Transformation)",
         y = "Mean Score", x = NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          panel.grid.minor = element_blank())
}

# 4. View B: Compare COHORTS (Longitudinal Trend) -------------------------
plot_raw_profile_trend <- function(data, target_sex) {
  
  sub <- data %>% filter(sex == target_sex)
  
  ggplot(sub, aes(x = measure, y = mean, group = cohort, color = cohort)) +
    geom_line(size = 1, alpha = 0.8) +
    geom_point(size = 2) +
    labs(title = paste("Developmental Trend:", target_sex),
         subtitle = "Comparing Raw Scores Across Cohorts",
         y = "Mean Score", x = NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# 5. View C: Master Grid (Everything) -------------------------------------
plot_raw_master <- function(data) {
  ggplot(data, aes(x = measure, y = mean, group = sex, color = sex)) +
    geom_line(size = 1) +
    geom_point(size = 1.5) +
    facet_wrap(~cohort, ncol = 2) +
    scale_color_manual(values = c("Boys" = "blue3", "Girls" = "red3")) +
    labs(title = "Master Overview (Raw Data)",
         y = "Mean Score", x = NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}


# 6. EXECUTION ------------------------------------------------------------

# --- A. Generate List of Sex Profiles ---
unique_cohorts <- unique(big_data_raw$cohort)
list_raw_profiles <- map(unique_cohorts, ~plot_raw_profile_sex(big_data_raw, .x))
names(list_raw_profiles) <- unique_cohorts

# --- B. Generate List of Trends ---
list_raw_trends <- list(
  Boys  = plot_raw_profile_trend(big_data_raw, "Boys"),
  Girls = plot_raw_profile_trend(big_data_raw, "Girls")
)

# --- C. Master Plot ---
p_raw_master <- plot_raw_master(big_data_raw)


# 7. DISPLAY EXAMPLES -----------------------------------------------------

# View 1: MIND Preschool (Raw scores)
# Note: SRS will be high up (70s), ADOS will be low down (7s).

list_raw_profiles$MIND_Preschool
list_raw_profiles$MIND_Schoolage
list_raw_profiles$SSC_Preschool
list_raw_profiles$SSC_Schoolage
# View 2: SSC Schoolage
list_raw_trends$Boys
list_raw_trends$Girls
# View 3: Master Overview
p_raw_master



if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, patchwork)

# 1. Load Data & Create "Master Dataframe" --------------------------------
files <- c(
  MIND_Preschool_Boys  = "MIND_Preschool_Boys.rds",
  MIND_Preschool_Girls = "MIND_Preschool_Girls.rds",
  MIND_Schoolage_Boys  = "MIND_Schoolage_Boys.rds",
  MIND_Schoolage_Girls = "MIND_Schoolage_Girls.rds",
  SSC_Preschool_Boys   = "SSC_Preschool_Boys.rds",
  SSC_Preschool_Girls  = "SSC_Preschool_Girls.rds",
  SSC_Schoolage_Boys   = "SSC_Schoolage_Boys.rds",
  SSC_Schoolage_Girls  = "SSC_Schoolage_Girls.rds"
)
raw_list <- map(files, readRDS)

# Helper to extract and label data
extract_stats <- function(obj, filename) {
  # Parse filename to get Cohort and Sex
  parts <- str_split(filename, "_")[[1]]
  sex   <- tail(parts, 1) # Last part is usually "Boys" or "Girls"
  # Everything before the sex is the Cohort name
  cohort <- paste(head(parts, -1), collapse = "_") 
  
  obj$descriptives %>%
    select(measure, mean, sd, n_valid) %>%
    mutate(sex = sex, cohort = cohort)
}

# Create one big dataframe
big_data <- imap_dfr(raw_list, \(obj, name) extract_stats(obj, name))

# 2. Strategy A: Plot WITHIN Cohorts (The "Pairs" View) -------------------
# This answers: "In MIND Preschool, how do Boys compare to Girls?"

plot_within_cohort <- function(data, target_cohort) {
  p <- data %>%
    filter(cohort == target_cohort) %>%
    ggplot(aes(x = sex, y = mean, fill = sex)) +
    geom_col(position = position_dodge(), alpha = 0.8, width = 0.7) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
    facet_wrap(~measure, scales = "free_y") + # Essential for different scales
    scale_fill_manual(values = c("Boys" = "blue3", "Girls" = "red3")) +
    labs(title = paste("Within-Cohort:", target_cohort),
         y = "Mean Score (+/- SD)", x = NULL) +
    theme_bw() +
    theme(legend.position = "none")
  return(p)
}

# 3. Strategy B: Plot BETWEEN Cohorts (The "Trend" View) ------------------
# This answers: "How does ADOS Severity look across ALL cohorts?"

plot_between_cohorts <- function(data, target_measure) {
  p <- data %>%
    filter(measure == target_measure) %>%
    ggplot(aes(x = cohort, y = mean, fill = sex)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), 
                  position = position_dodge(width = 0.8), width = 0.25) +
    scale_fill_manual(values = c("Boys" = "blue3", "Girls" = "red3")) +
    labs(title = paste("Comparison Across Cohorts:", target_measure),
         y = "Mean Score", x = NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(p)
}

# 4. Strategy C: The "Everything" Plot (Overview) -------------------------
# This plots Cohorts as Rows and Measures as Columns (or vice versa)

plot_everything <- function(data) {
  ggplot(data, aes(x = cohort, y = mean, fill = sex)) +
    geom_col(position = position_dodge(width=0.8), width=0.7) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), 
                  position = position_dodge(width=0.8), width=0.2) +
    facet_wrap(~measure, scales = "free_y", ncol = 3) + 
    scale_fill_manual(values = c("Boys" = "blue3", "Girls" = "red3")) +
    labs(title = "Master Overview: All Cohorts, All Measures",
         y = "Mean Score", x = NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          strip.background = element_rect(fill="grey95"))
}

# 5. EXECUTION: Generate the Plots ----------------------------------------

# --- A. Generate one plot per Cohort ---
# Get list of unique cohorts
unique_cohorts <- unique(big_data$cohort)
list_within <- map(unique_cohorts, ~plot_within_cohort(big_data, .x))
names(list_within) <- unique_cohorts

# --- B. Generate one plot per Measure ---
# Get list of unique measures
unique_measures <- unique(big_data$measure)
list_between <- map(unique_measures, ~plot_between_cohorts(big_data, .x))
names(list_between) <- unique_measures

# --- C. Generate the Master Plot ---
p_master <- plot_everything(big_data)


# 6. DISPLAY EXAMPLES -----------------------------------------------------

# View 1: Show MIND Preschool (Boys vs Girls)
list_within$MIND_Preschool
list_within$MIND_Schoolage
list_within$SSC_Preschool
list_within$SSC_Preschool

# View 2: Show "ADOS_Severity" across ALL cohorts
list_between$ADOS_Severity
list_between$ADOS_SA
list_between$ADOS_RRB
list_between$ADI_Total
list_between$ADI_Social_Com
list_between$ADI_RRB
list_between$SRS_Total_Parent
list_between$SRS_Social_Parent
list_between$SRS_RRB_Parent
list_between$RBSR_Total

# View 3: The Master Plot (Requires a large window/screen)
p_master

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, patchwork)

# 1. Load Data ------------------------------------------------------------
files <- c(
  MIND_Preschool_Boys  = "MIND_Preschool_Boys.rds",
  MIND_Preschool_Girls = "MIND_Preschool_Girls.rds",
  MIND_Schoolage_Boys  = "MIND_Schoolage_Boys.rds",
  MIND_Schoolage_Girls = "MIND_Schoolage_Girls.rds",
  SSC_Preschool_Boys   = "SSC_Preschool_Boys.rds",
  SSC_Preschool_Girls  = "SSC_Preschool_Girls.rds",
  SSC_Schoolage_Boys   = "SSC_Schoolage_Boys.rds",
  SSC_Schoolage_Girls  = "SSC_Schoolage_Girls.rds"
)
data_list <- map(files, readRDS)

# 2. Extract Raw Descriptive Data (Long Format) ---------------------------
get_raw_means <- function(b_obj, g_obj, cohort_label) {
  
  # Extract Boys
  df_b <- b_obj$descriptives %>% 
    select(measure, mean, sd) %>% 
    mutate(sex = "Boys", cohort = cohort_label)
  
  # Extract Girls
  df_g <- g_obj$descriptives %>% 
    select(measure, mean, sd) %>% 
    mutate(sex = "Girls", cohort = cohort_label)
  
  bind_rows(df_b, df_g)
}

# 3. Extract Raw Correlation Data (Long Format) ---------------------------
get_raw_corrs <- function(b_obj, g_obj, cohort_label) {
  
  # Helper to melt a matrix
  melt_mat <- function(mat, sex_label) {
    as_tibble(mat, rownames = "var1") %>% 
      pivot_longer(-var1, names_to = "var2", values_to = "r") %>%
      mutate(sex = sex_label, cohort = cohort_label)
  }
  
  bind_rows(
    melt_mat(b_obj$corr, "Boys"),
    melt_mat(g_obj$corr, "Girls")
  )
}

# 4. Generate the Dataframes ----------------------------------------------
cohorts <- list(
  MIND_Preschool = c("MIND_Preschool_Boys","MIND_Preschool_Girls"),
  MIND_Schoolage = c("MIND_Schoolage_Boys","MIND_Schoolage_Girls"),
  SSC_Preschool  = c("SSC_Preschool_Boys","SSC_Preschool_Girls"),
  SSC_Schoolage  = c("SSC_Schoolage_Boys","SSC_Schoolage_Girls")
)

means_df <- imap_dfr(cohorts, \(fg, lab) get_raw_means(data_list[[fg[1]]], data_list[[fg[2]]], lab))
corrs_df <- imap_dfr(cohorts, \(fg, lab) get_raw_corrs(data_list[[fg[1]]], data_list[[fg[2]]], lab))


# 5. Plotting Functions ---------------------------------------------------

# (A) Side-by-Side Bars (Means + SD)
plot_bars <- function(df, specific_cohort) {
  df %>%
    filter(cohort == specific_cohort) %>%
    ggplot(aes(x = measure, y = mean, fill = sex)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
    # Error Bars (Mean +/- SD)
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), 
                  position = position_dodge(width = 0.8), width = 0.25) +
    facet_wrap(~measure, scales = "free") + # scales="free" is CRITICAL here
    scale_fill_manual(values = c("Boys" = "blue3", "Girls" = "red3")) +
    labs(title = paste("Descriptive Stats:", specific_cohort),
         subtitle = "Bars = Mean, Lines = Standard Deviation",
         x = NULL, y = "Score") +
    theme_bw() +
    theme(axis.text.x = element_blank(), # Hide x text (redundant with facet label)
          axis.ticks.x = element_blank())
}

# (B) The "Profile" Line Plot
plot_profile <- function(df, specific_cohort) {
  df %>%
    filter(cohort == specific_cohort) %>%
    ggplot(aes(x = measure, y = mean, color = sex, group = sex)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    scale_color_manual(values = c("Boys" = "blue3", "Girls" = "red3")) +
    theme_bw() +
    labs(title = paste("Clinical Profile:", specific_cohort),
         y = "Mean Score", x = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# (C) Dual Heatmaps (Raw Correlations)
plot_dual_heat <- function(df, specific_cohort) {
  df %>% 
    filter(cohort == specific_cohort) %>%
    ggplot(aes(var1, var2, fill = r)) +
    geom_tile() +
    facet_wrap(~sex) + # This creates the side-by-side view
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", limit = c(-1,1)) +
    labs(title = paste("Correlation Structure:", specific_cohort),
         x = NULL, y = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
          axis.text.y = element_text(size = 6),
          panel.grid = element_blank()) +
    coord_fixed()
}

# 6. Run & Display --------------------------------------------------------

# Example: Bar Chart (Best for Mean comparison)
plot_bars(means_df, "MIND_Preschool")
plot_bars(means_df, "MIND_Schoolage")
plot_bars(means_df, "SSC_Preschool")
plot_bars(means_df, "SSC_Schoolage")

# Example: Profile Plot (Best for Pattern comparison)
# Note: This looks weird if variables have vastly different scales (e.g. 7 vs 70)

# Example: Dual Heatmaps (Best for Correlation comparison)





# -------------------------------------------------------------------------
# 7. SAVE TO PDF (Multi-Page Report)
# -------------------------------------------------------------------------

# Export:
# -------------------------------------------------------------------------
# 8. SAVE TO PDF (Multi-Page Report)
# -------------------------------------------------------------------------

# 1. Open the PDF device
pdf("Profile_Plots_Report.pdf", width = 12, height = 8)

# --- SECTION 1: Z-SCORE PLOTS (PATTERNS) ---

# A. Loop through all specific Cohort Profiles (Z-Score)
# We use iwalk to print the plot and give it a console message
iwalk(list_profiles, \(plot, name) {
  message(paste("Printing Z-Score Profile:", name))
  print(plot)
})

# B. Print Trends (Z-Score)
message("Printing Z-Score Trends...")
print(list_trends$Boys)
print(list_trends$Girls)

# C. Print Master Overview (Z-Score)
message("Printing Z-Score Master Overview...")
print(p_master_profile)


# --- SECTION 2: RAW PLOTS (ABSOLUTE MEANS) ---

# A. Loop through all specific Cohort Profiles (Raw)
iwalk(list_raw_profiles, \(plot, name) {
  message(paste("Printing Raw Profile:", name))
  print(plot)
})

# B. Print Trends (Raw)
message("Printing Raw Trends...")
print(list_raw_trends$Boys)
print(list_raw_trends$Girls)

# C. Print Master Overview (Raw)
message("Printing Raw Master Overview...")
print(p_raw_master)

plot_bars(means_df, "MIND_Preschool")
plot_bars(means_df, "MIND_Schoolage")
plot_bars(means_df, "SSC_Preschool")
plot_bars(means_df, "SSC_Schoolage")
# 2. Close the PDF device (This actually saves the file)

# 6. DISPLAY EXAMPLES -----------------------------------------------------

# View 1: Show MIND Preschool (Boys vs Girls)
list_within$MIND_Preschool
list_within$MIND_Schoolage
list_within$SSC_Preschool
list_within$SSC_Preschool

# View 2: Show "ADOS_Severity" across ALL cohorts
list_between$ADOS_Severity
list_between$ADOS_SA
list_between$ADOS_RRB
list_between$ADI_Total
list_between$ADI_Social_Com
list_between$ADI_RRB
list_between$SRS_Total_Parent
list_between$SRS_Social_Parent
list_between$SRS_RRB_Parent
list_between$RBSR_Total

# View 3: The Master Plot (Requires a large window/screen)
p_master

dev.off()

message("Done! File saved as 'Profile_Plots_Report.pdf'")


SEM-ALL AT ONCE -CORR MATRIX
COVARIANTS AND CORRLATIONS, DIFFERENT FOR BOYS AND GIRLS
CHOOSE MOST IMPORTANT ONES

CREAT A FACTOR MODEL (FACTOR ANALYSIS)
CORRELATIONS NOT AT THE VARIABLE LEVEL AND THE FACTOR LEVEL.
