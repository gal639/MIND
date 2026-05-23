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

# ==============================================================================
# 1. SETUP & LIBRARIES
# ==============================================================================
if(!require(corrplot)) install.packages("corrplot")
if(!require(tools)) install.packages("tools")

library(corrplot)
library(tools)

# --- PATH CONFIGURATION ---
input_dir  <- "C:/Users/galor/OneDrive/Desktop/MIND/correlation_matrices"
output_dir <- file.path(input_dir, "plots")

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  message(paste("Created output directory:", output_dir))
}

# ==============================================================================
# 2. HELPER: Calculate P-values
# ==============================================================================
calculate_p_matrix <- function(r_mat, n_mat) {
  # Avoid division by zero
  r_mat <- ifelse(abs(r_mat) == 1, 0.999999, r_mat)
  
  # t-statistic formula & p-value
  t_stat <- r_mat * sqrt((n_mat - 2) / (1 - r_mat^2))
  p_mat <- 2 * pt(-abs(t_stat), df = n_mat - 2)
  
  diag(p_mat) <- 0
  return(p_mat)
}

# ==============================================================================
# 3. STYLING FUNCTION (Updated for Larger Text & No Diagonal)
# ==============================================================================
create_styled_corrplot_rds <- function(rds_obj, title) {
  
  # Check for required data slots
  if (!all(c("corr", "n") %in% names(rds_obj))) {
    stop("Error: RDS file is missing '$corr' or '$n' slots.")
  }
  
  correlation_matrix <- rds_obj$corr
  n_matrix <- rds_obj$n
  
  # Calculate P-values
  p_matrix <- calculate_p_matrix(correlation_matrix, n_matrix)
  
  # Handle NAs
  correlation_matrix[is.na(correlation_matrix)] <- 0
  
  # Save graphics parameters
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  
  # Set margins
  par(mar = c(5, 4, 4, 2) + 0.1)
  
  # --- DYNAMIC SCALING UPDATE ---
  n_vars <- ncol(correlation_matrix)
  
  # INCREASED TEXT SIZE:
  # I increased the intercept from 1.55 to 1.85 to make numbers larger overall
  number_cex <- 1.85 - 0.08 * n_vars 
  
  # Asterisk size
  pch_cex <- 2.15 - 0.1 * n_vars
  
  # Generate Plot
  corrplot(correlation_matrix,
           method = "shade",
           type = "lower",
           p.mat = p_matrix,
           order = "original",
           addrect = 3,
           col = colorRampPalette(c("blue", "white", "red"))(200),
           insig = "label_sig",
           sig.level = c(.001, .01, .05),
           pch.col = "orange",
           pch.cex = pch_cex,
           tl.col = "black",
           
           # UPDATED PARAMETERS:
           tl.cex = 1.2,           # Increased Label text size (was 0.9)
           addCoef.col = "black",
           tl.srt = 45,
           diag = FALSE,           # REMOVED DIAGONAL (was TRUE)
           number.cex = number_cex*1.2,# Larger numbers based on new formula above
           outline = TRUE,
           mar = c(0,0,2,0))
  
  # Dynamic Title Adjustment
  title_length <- nchar(title)
  if (title_length <= 3) {
    adj_value <- 0.15
  } else if (title_length >= 18) {
    adj_value <- 0.03
  } else {
    adj_value <- 0.15 - (title_length - 3) * 0.008
  }
  
  title(main = title, line = 2.5, adj = adj_value)
}

# ==============================================================================
# 4. EXECUTION LOOP (With Specific Filtering)
# ==============================================================================
file_list <- list.files(path = input_dir, pattern = "\\.rds$", full.names = TRUE)

message(paste("Found", length(file_list), "RDS files."))


for (file_path in file_list) {
  
  file_base <- file_path_sans_ext(basename(file_path))
  rds_data <- readRDS(file_path)
  
  # Determine Title
  group_name <- if(!is.null(rds_data$group)) rds_data$group else file_base
  
  message(paste("Processing:", group_name))
  
  # --- SPECIAL FILTERING RULE ---
  # If the group is Schoolage Boys or Girls, remove ADI variables
  if (group_name %in% c("MIND_Schoolage_Boys", "MIND_Schoolage_Girls") || 
      file_base  %in% c("MIND_Schoolage_Boys", "MIND_Schoolage_Girls")) {
    
    message("   -> Filtering out ADI variables for this group...")
    
    # Identify columns that DO NOT contain "ADI" (case insensitive)
    # This keeps ADOS, SRS, RBSR, etc., but drops ADI_Total, ADI_Social, etc.
    keep_cols <- !grepl("ADI", colnames(rds_data$corr), ignore.case = TRUE)
    
    # Subset the Correlation and N matrices
    rds_data$corr <- rds_data$corr[keep_cols, keep_cols]
    rds_data$n    <- rds_data$n[keep_cols, keep_cols]
  }
  
  # Save Plot
  png_filename <- file.path(output_dir, paste0(file_base, "_styled.png"))
  png(filename = png_filename, width = 1000, height = 1000, res = 100)
  
  tryCatch({
    create_styled_corrplot_rds(rds_data, group_name)
  }, error = function(e) {
    message(paste("Error processing", file_base, ":", e$message))
  })
  
  dev.off()
}

message(paste("Done! Images saved to:", output_dir))
