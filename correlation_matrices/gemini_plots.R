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
