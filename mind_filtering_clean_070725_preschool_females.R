# boot --------------------------------------------------------------------

boot <- if (!require("pacman")) install.packages("pacman")
pacman::p_load(qdapRegex, patchwork,coin, ggside, pacman, readxl, writexl, dplyr, tidyverse, gridExtra, openxlsx, ggsignif, lubridate, psych, corrplot, janitor, purrr)

# functions ---------------------------------------------------------------
run_lines <- function(x) {
  editor <- rstudioapi::getSourceEditorContext()
  start <- rstudioapi::document_position(min(x), 1)
  end <- rstudioapi::document_position(max(x) + 1, 1)
  range <- rstudioapi::document_range(start, end)
  rstudioapi::setSelectionRanges(range, editor$id)
  selection <- rstudioapi::primary_selection(
    rstudioapi::getSourceEditorContext())$text
  source(textConnection(selection))
}


create_styled_corrplot <- function(data, title) {
  # Save current graphics parameters and restore on exit
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  
  # Define a custom plotting region that leaves extra space at the top.
  par(fig = c(0, 1, 0, 0.95))
  
  # Compute correlation and significance matrices using only the passed data subset.
  correlation_matrix <- cor(data, use = "pairwise.complete.obs")
  pmat <- cor.mtest(data, use = "pairwise.complete.obs")
  correlation_matrix[is.na(correlation_matrix)] <- 0
  
  # Count only the columns in the subset (not the entire adi_ados_plot)
  n_vars <- ncol(data)
  
  # Create a linear function for dynamic scaling:
  # For 3 variables, we want number_cex = 1.3 and for 10 variables, number_cex = 0.7.
  # Solve for f(n) = a * n + b:
  #   f(3) = 1.3  --> 3a + b = 1.3
  #   f(10) = 0.7 --> 10a + b = 0.7
  # Subtracting gives: 7a = -0.6  ->  a = -0.0857 (approx)
  # Then, b = 1.3 - 3*(-0.0857) ≈ 1.5571
  number_cex <- 1.5571 - 0.0857 * n_vars
  # Similarly, you can adjust pch_cex if desired. Here’s an example scaling:
  pch_cex <- 1.5571 - 0.0857 * n_vars  # Adjust or compute dynamically as needed
  
  # Create the corrplot using dynamic cex values.
  corrplot(correlation_matrix,
           method = "shade",
           type = "lower",
           p.mat = pmat$p,
           order = "original",
           addrect = 3,
           col = colorRampPalette(c("blue", "white", "red"))(200),
           insig = "label_sig",
           sig.level = c(.001, .01, .05),
           pch.cex = pch_cex,
           pch.col = "orange",
           tl.col = "black",
           tl.cex = 1,
           addCoef.col = "black",
           tl.srt = 45,
           diag = TRUE,
           number.cex = number_cex,
           outline = TRUE)
  
  # Add title above the plot region.
  title(title)
}


find_rows_below_zero <- function(df) {
  
  rows_below_zero <- df %>%
    rowwise() %>%
    filter(any(c_across(where(is.numeric)) < 0))
  
  return(rows_below_zero)
}

get_descriptives <- function(df) {
  df %>%
    gather(key = "variable", value = "value") %>%
    group_by(variable) %>%
    summarise(
      count = n(),
      mean = mean(as.numeric(value), na.rm = TRUE),
      sd = sd(as.numeric(value), na.rm = TRUE),
      median = median(as.numeric(value), na.rm = TRUE),
      min = min(as.numeric(value), na.rm = TRUE),
      max = max(as.numeric(value), na.rm = TRUE)
    )
}

# Plotting
plot_all_distributions <- function(df, plots_per_page = 9) {
  plot_list <- lapply(names(df), function(var) {
    if (is.numeric(df[[var]])) {
      p <- ggplot(df, aes(x = !!sym(var))) +
        geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
        labs(title = var, x = var, y = "Density") +
        theme_minimal()
      
      mean_val <- mean(df[[var]], na.rm = TRUE)
      sd_val <- sd(df[[var]], na.rm = TRUE)
      
      p + stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), color = "red", linewidth = 1)
    } else {
      p <- ggplot(df, aes(x = !!sym(var))) +
        geom_bar(fill = "blue", color = "black", alpha = 0.7) +
        labs(title = var, x = var, y = "Count") +
        theme_minimal()
      p
    }
  })
  
  num_plots <- length(plot_list)
  num_pages <- ceiling(num_plots / plots_per_page)
  
  for (page in 1:num_pages) {
    start <- (page - 1) * plots_per_page + 1
    end <- min(page * plots_per_page, num_plots)
    grid.arrange(grobs = plot_list[start:end], ncol = 3)
  }
}

plot_selected_distributions <- function(df, variables, plots_per_page = 9) {
  plot_list <- lapply(variables, function(var) {
    if (is.numeric(df[[var]])) {
      p <- ggplot(df, aes(x = !!sym(var))) +
        geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
        labs(title = var, x = var, y = "Density") +
        theme_minimal()
      
      mean_val <- mean(df[[var]], na.rm = TRUE)
      sd_val <- sd(df[[var]], na.rm = TRUE)
      
      p + stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), color = "red", linewidth = 1)
    } else {
      p <- ggplot(df, aes(x = !!sym(var))) +
        geom_bar(fill = "blue", color = "black", alpha = 0.7) +
        labs(title = var, x = var, y = "Count") +
        theme_minimal()
      p
    }
  })
  
  num_plots <- length(plot_list)
  num_pages <- ceiling(num_plots / plots_per_page)
  
  for (page in 1:num_pages) {
    start <- (page - 1) * plots_per_page + 1
    end <- min(page * plots_per_page, num_plots)
    grid.arrange(grobs = plot_list[start:end], ncol = 3)
  }
}

# loading files -----------------------------------------------------------
# library(readxl)
# library(dplyr)
# library(tidyverse)
# library(gridExtra)
# library(ggsignif)
# library(psych)

# fname2 <- choose.files() #Load MIND_dataset_2024-05-29
# fname <- choose.files() #Load MIND_ADOS_item_061924.xlsx
# fname2 <- "C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Analyzing data\\MIND\\Preschool - All Correlation analysis\\11.5.24\\MIND_dataset_2024-05-29.xlsx"
# fname <- "C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Analyzing data\\MIND\\Preschool - All Correlation analysis\\11.5.24\\MIND_ADOS_item_061924.xlsx"
fname <- "C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/MIND/MIND_ADOS_item_061924.xlsx"
fname2 <- "C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/MIND/MIND_dataset_2024-05-29.xlsx"

ad2_mod1 <- read_excel(fname, sheet = 1)
ad2_mod2 <- read_excel(fname, sheet = 2)
ad2_mod3 <- read_excel(fname, sheet = 3)
adg_mod1 <- read_excel(fname, sheet = 4)
adg_mod2 <- read_excel(fname, sheet = 5)
adg_mod3 <- read_excel(fname, sheet = 6)
ad2_mod1 <- ad2_mod1 %>% mutate(origin = "ad2_mod1")
ad2_mod2 <- ad2_mod2 %>% mutate(origin = "ad2_mod2")
ad2_mod3 <- ad2_mod3 %>% mutate(origin = "ad2_mod3")
adg_mod1 <- adg_mod1 %>% mutate(origin = "adg_mod1")
adg_mod2 <- adg_mod2 %>% mutate(origin = "adg_mod2")
adg_mod3 <- adg_mod3 %>% mutate(origin = "adg_mod3")

combined_df <- bind_rows(ad2_mod1, ad2_mod2, ad2_mod3,adg_mod1, adg_mod2, adg_mod3) |>
  relocate(origin, .after = id)

duplicates <- combined_df %>% #There are adg dupes
  group_by(id, visit) %>%
  filter(n() > 1)

combined_df <- combined_df %>%  #every ados module
  distinct(id, visit, .keep_all = TRUE)

min_visits <- combined_df %>%
  group_by(id) %>%
  summarize(min_visit = min(visit))

visit_df <- combined_df %>%
  inner_join(min_visits, by = c("id" = "id", "visit" = "min_visit"))

filter_and_replace <- function(df, blacklist_cols = NULL) {
  # Identify columns to check by excluding the blacklist columns
  cols_to_check <- setdiff(names(df), blacklist_cols)
  
  df %>%
    mutate(across(all_of(cols_to_check), ~ {
      if (is.numeric(.)) {
        case_when(
          . %in% c(7, 8, 9) | . < 0 ~ NA_real_,  # Replace 7, 8, 9, or any negative number with NA
          . %in% c(3, 4) ~ 2,  # Replace 3 or 4 with 2
          TRUE ~ as.numeric(.)  # Preserve other numeric values
        )
      } else if (is.character(.)) {
        case_when(
          . %in% c("7", "8", "9") ~ NA_character_,  # Replace "7", "8", "9" with NA
          . %in% c("3", "4") ~ "2",  # Replace "3" or "4" with "2"
          TRUE ~ .  # Preserve other character values
        )
      } else {
        .  # For other types, preserve original values
      }
    }))
}


blacklist_cols <- c("id","origin","visit",
                    "ad2_mod1_date", "ad2_mod1_examiner","ad2_mod1_algorithm", "ad2_mod1_class", "ad2_mod1_class",
                    "ad2_mod2_date", "ad2_mod2_examiner","ad2_mod2_algorithm", "ad2_mod2_class", "ad2_mod2_class",
                    "ad2_mod3_date", "ad2_mod3_examiner","ad2_mod3_algorithm", "ad2_mod3_class", "ad2_mod3_class",
                    "ad2_mod1_sa_total","ad2_mod1_rb_total","ad2_mod1_sarb_total","ad2_mod1_comp_score",
                    "ad2_mod2_sa_total","ad2_mod2_rb_total","ad2_mod2_sarb_total","ad2_mod2_comp_score",
                    "ad2_mod3_sa_total","ad2_mod3_rb_total","ad2_mod3_sarb_total","ad2_mod3_comp_score",
                    "ad1_sa_total","ad1_rb_total","ad1_sarb_total",
                    "ad2_sa_total","ad2_rb_total","ad2_sarb_total",
                    "ad3_sa_total","ad3_rb_total","ad3_sarb_total")

visit_df <- filter_and_replace(visit_df, blacklist_cols)

visit_df_processed <- visit_df |>
  group_by(origin) |>
  mutate(
    JA = case_when(
      origin == "ad2_mod1" & ad2_mod1_algorithm == "No words" ~ rowSums(data.frame(ad2_mod1_a7, ad2_mod1_a8, ad2_mod1_b1, ad2_mod1_b9, ad2_mod1_b10, ad2_mod1_b11), na.rm = TRUE),
      origin == "ad2_mod1" & ad2_mod1_algorithm == "Some Words" ~ rowSums(data.frame(ad2_mod1_a7, ad2_mod1_a8, ad2_mod1_b1, ad2_mod1_b9, ad2_mod1_b10), na.rm = TRUE),
      origin == "ad2_mod2" & ad2_mod2_algorithm == "Younger Than Age 5" ~ rowSums(data.frame(ad2_mod2_a6, ad2_mod2_a7, ad2_mod2_b5, ad2_mod2_b6, ad2_mod2_b1, ad2_mod2_b7), na.rm = TRUE),
      origin == "ad2_mod2" & ad2_mod2_algorithm == "Age 5 Years Or Older" ~ rowSums(data.frame(ad2_mod2_a6, ad2_mod2_a7, ad2_mod2_b5, ad2_mod2_b6, ad2_mod2_b1, ad2_mod2_b7), na.rm = TRUE)
    ),
    sumA = case_when(
      origin == "ad2_mod1" ~ rowSums(data.frame(ad2_mod1_a1, ad2_mod1_a2, ad2_mod1_a3, ad2_mod1_a4, ad2_mod1_a5, ad2_mod1_a6, ad2_mod1_a7, ad2_mod1_a8), na.rm = TRUE),
      origin == "ad2_mod2" ~ rowSums(data.frame(ad2_mod2_a1, ad2_mod2_a2, ad2_mod2_a3, ad2_mod2_a4, ad2_mod2_a5, ad2_mod2_a6, ad2_mod2_a7), na.rm = TRUE),
      origin == "ad2_mod3" ~ rowSums(data.frame(ad2_mod3_a1, ad2_mod3_a2, ad2_mod3_a3, ad2_mod3_a4, ad2_mod3_a5, ad2_mod3_a6, ad2_mod3_a7, ad2_mod3_a8, ad2_mod3_a9), na.rm = TRUE),
      origin == "adg_mod1" ~ rowSums(data.frame(ad1_a1, ad1_a2, ad1_a3, ad1_a4, ad1_a5, ad1_a6, ad1_a7, ad1_a8), na.rm = TRUE),
      origin == "adg_mod2" ~ rowSums(data.frame(ad2_a1, ad2_a2, ad2_a3, ad2_a4, ad2_a5, ad2_a6, ad2_a7), na.rm = TRUE),
      origin == "adg_mod3" ~ rowSums(data.frame(ad3_a1, ad3_a2, ad3_a3, ad3_a4, ad3_a5, ad3_a6, ad3_a7, ad3_a8, ad3_a9), na.rm = TRUE)
      
    ),
    sumB = case_when(
      origin == "ad2_mod1" ~ rowSums(data.frame(ad2_mod1_b1, ad2_mod1_b2, ad2_mod1_b3, ad2_mod1_b4, ad2_mod1_b5, ad2_mod1_b6, ad2_mod1_b7, ad2_mod1_b8, ad2_mod1_b9, ad2_mod1_b10, ad2_mod1_b11, ad2_mod1_b12, ad2_mod1_b13a, ad2_mod1_b13b, ad2_mod1_b14, ad2_mod1_b15, ad2_mod1_b16), na.rm = TRUE),
      origin == "ad2_mod2" ~ rowSums(data.frame(ad2_mod2_b1, ad2_mod2_b2, ad2_mod2_b3, ad2_mod2_b4, ad2_mod2_b5, ad2_mod2_b6, ad2_mod2_b7, ad2_mod2_b8, ad2_mod2_b9a, ad2_mod2_9b, ad2_mod2_b10, ad2_mod2_b11, ad2_mod2_b12), na.rm = TRUE),
      origin == "ad2_mod3" ~ rowSums(data.frame(ad2_mod3_b1, ad2_mod3_b2, ad2_mod3_b3, ad2_mod3_b4, ad2_mod3_b5, ad2_mod3_b6, ad2_mod3_b7, ad2_mod3_b8, ad2_mod3_b9, ad2_mod3_b10, ad2_mod3_b11), na.rm = TRUE),
      origin == "adg_mod1" ~ rowSums(data.frame(ad1_b1, ad1_b2, ad1_b3, ad1_b4, ad1_b5, ad1_b6, ad1_b7, ad1_b8, ad1_b9, ad1_b10, ad1_b11, ad1_b12), na.rm = TRUE),
      origin == "adg_mod2" ~ rowSums(data.frame(ad2_b1, ad2_b2, ad2_b3, ad2_b4, ad2_b5, ad2_b6, ad2_b7, ad2_b8, ad2_b9, ad2_b11), na.rm = TRUE),
      origin == "adg_mod3" ~ rowSums(data.frame(ad3_b1, ad3_b2, ad3_b3, ad3_b4, ad3_b5, ad3_b6, ad3_b7, ad3_b9, ad3_10), na.rm = TRUE)
    ),
    sumAalgorithm = case_when(
      origin == "ad2_mod1" ~ rowSums(data.frame(ad2_mod1_a2, ad2_mod1_a7, ad2_mod1_a8), na.rm = TRUE),
      origin == "ad2_mod2" ~ rowSums(data.frame(ad2_mod2_a6, ad2_mod2_a7), na.rm = TRUE),
      origin == "ad2_mod3" ~ rowSums(data.frame(ad2_mod3_a7, ad2_mod3_a8, ad2_mod3_a9), na.rm = TRUE),
      origin == "adg_mod1" ~ rowSums(data.frame(ad1_a2, ad1_a7, ad1_a8), na.rm = TRUE),
      origin == "adg_mod2" ~ rowSums(data.frame(ad2_a6, ad2_a7), na.rm = TRUE),
      origin == "adg_mod3" ~ rowSums(data.frame(ad3_a7, ad3_a8, ad3_a9), na.rm = TRUE)
      
    ),
    sumBalgorithm = case_when(
      origin == "ad2_mod1" ~ rowSums(data.frame(ad2_mod1_b1, ad2_mod1_b3, ad2_mod1_b4, ad2_mod1_b5, ad2_mod1_b9, ad2_mod1_b10, ad2_mod1_b11, ad2_mod1_b12), na.rm = TRUE),
      origin == "ad2_mod2" ~ rowSums(data.frame(ad2_mod2_b1, ad2_mod2_b2, ad2_mod2_b3, ad2_mod2_b5, ad2_mod2_b6, ad2_mod2_b8, ad2_mod2_b11, ad2_mod2_b12), na.rm = TRUE),
      origin == "ad2_mod3" ~ rowSums(data.frame(ad2_mod3_b1, ad2_mod3_b2, ad2_mod3_b4, ad2_mod3_b7, ad2_mod3_b9, ad2_mod3_b10, ad2_mod3_b11), na.rm = TRUE),
      origin == "adg_mod1" ~ rowSums(data.frame(ad1_b1, ad1_b3, ad1_b4, ad1_b5, ad1_b9, ad1_b10, ad1_b11, ad1_b12), na.rm = TRUE),
      origin == "adg_mod2" ~ rowSums(data.frame(ad2_b1, ad2_b2, ad2_b3, ad2_b4, ad2_b5, ad2_b6, ad2_b8, ad2_b11, ad2_b12), na.rm = TRUE),
      origin == "adg_mod3" ~ rowSums(data.frame(ad3_b1, ad3_b2, ad3_b4, ad3_b7, ad3_b9, ad3_10, ad3_b11), na.rm = TRUE)
    )
    
  ) |>
  relocate(JA, .after = origin) |>
  relocate(ad2_mod1_algorithm, .after = JA) |>
  relocate(ad2_mod2_algorithm, .after = ad2_mod1_algorithm) |>
  relocate(c("sumA", "sumB"), .after = JA)

# merge -------------------------------------------------------------------

mastermind <- read_excel(fname2)

mastermind <- rename(mastermind, id = subj_id)
merged_df <- merge(mastermind, visit_df_processed[, c("id", "JA", "sumA", "sumB", "sumAalgorithm","sumBalgorithm", "origin")], by = "id", all.x = F)

merged_df <- merged_df %>%
  mutate(
    ados_done = ifelse(!is.na(ados_severity), 1, 0),
    adi_done = ifelse(!is.na(adi_totalA), 1, 0),
    rbs_done = ifelse(!is.na(rbs_6_factor_total), 1, 0),
    srs_done = ifelse(!is.na(srs_total_t), 1, 0)
  ) %>%
  mutate(
    total_done = rowSums(select(., ados_done:srs_done)))

# Only consider rows with ados_age < 60 when computing the max per id
max_scores <- merged_df %>%
  filter(ados_age < 60) %>%
  group_by(id) %>%
  summarize(max_total_done = max(total_done), .groups = 'drop')

# Join back the max_total_done and mark best_instance only if ados_age is below 60
merged_df_best <- merged_df %>%
  left_join(max_scores, by = "id") %>%
  mutate(best_instance = ifelse(ados_age < 60 & total_done == max_total_done, "Yes", "No"))

merged_df <- merged_df_best %>%
  filter(best_instance == "Yes") %>%
  distinct(id, visit, .keep_all = TRUE)

min_visits2 <- merged_df %>%
  group_by(id) %>%
  summarize(min_visit = min(visit))

visit_df2 <- merged_df %>%
  inner_join(min_visits2, by = c("id" = "id", "visit" = "min_visit"))
nrow(visit_df2)


visit_df2 <- visit_df2 |> 
  relocate(JA, .after = visit) |>
  relocate(c("sumA", "sumB", "sumAalgorithm", "sumBalgorithm","ados_age"), .after = JA)

#Visit_df2 is the mixed data of every ID. we take columns from this dataframe.

# selecting only interesting variables ------------------------------------

interest <- visit_df2 |>
  select(id, visit, origin, sex, ados_age, JA, sumAalgorithm, sumBalgorithm, ados_severity, ados_rrb_severity, ados_sa_severity, adi_totalA, adi_totalVB, adi_totalNVB, adi_totalC, adi_missing_items_count, rbs_6_factor_compulsive, rbs_6_factor_restricted, rbs_6_factor_stereotypy, rbs_6_factor_self_injurious, rbs_6_factor_ritualistic, rbs_6_factor_sameness, rbs_6_factor_total, srs_awr_t, srs_cog_t, srs_com_t, srs_mot_t, srs_rrb_t, srs_sci_t, srs_total_t, best_instance, total_done, adi_done, srs_done, rbs_done, ados_done, adi_age, rbs_age, srs_age, mean_age, median_age, missing_measure_list) |>
  mutate(srs_total_social_t = srs_sci_t)

interest <- interest %>% filter(sex == "Female")

describe(interest)
print(get_descriptives(interest), n=length(interest))
# plot_all_distributions(interest)  #This might take some time


# ADI - calculations ---------------------------------------------------------------------
adi_r <- "C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/MIND/‏‏MIND_ADI_R_2003_062824.xlsx"
# adi_r <- "C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Analyzing data\\MIND\\Preschool - All Correlation analysis\\11.5.24\\��MIND_ADI_R_2003_062824.xlsx"

adi_r_1 <- read_excel(adi_r, sheet = 2)
adi_col <- read_excel(adi_r, sheet = 1)
adi_r_1 <- adi_r_1 %>% mutate(origin = "adi_r_1")
adi_col <- adi_col |> mutate(origin = "adi_col")
names(adi_r_1) <- names(adi_col)

adi_algorithm <- function(df, blacklist_cols = NULL) {
  # Identify columns to check by excluding the blacklist columns
  cols_to_check <- setdiff(names(df), blacklist_cols)
  
  df %>%
    mutate(across(all_of(cols_to_check), ~ {
      if (is.numeric(.)) {
        case_when(
          . %in% c(7, 8, 9) | . < 0 ~ NA_real_,  # Replace 7, 8, 9, or any negative number with NA
          . == 3 ~ 2,  # Replace 3 with 2
          TRUE ~ as.numeric(.)  # Preserve other numeric values
        )
      } else if (is.character(.)) {
        case_when(
          . %in% c("7", "8", "9") ~ NA_character_,  # Replace "7", "8", "9" with NA
          . == "3" ~ "2",  # Replace "3" with "2"
          TRUE ~ .  # Preserve other character values
        )
      } else {
        .  # For other types, preserve original values
      }
    }))
}

adi_r_1 <- adi_algorithm(adi_r_1, blacklist_cols)


adi_r_1_filter <- adi_r_1 %>% 
  inner_join(interest %>% select(id, visit), by = c("id", "visit"))



# lord -------------------------------------------------------------

adi_r_1_calc <- adi_r_1_filter %>%
  mutate(
    is.verbal = case_when(
      ADI_30_overallang == 0 ~ 1,  # Verbal
      ADI_30_overallang %in% c(1, 2) ~ 0,  # Nonverbal
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    # Calculate Social communication_lord (15 items)
    soc_com_lord = rowSums(select(., ADI_31_usebody, ADI_42_point, ADI_43_nodding,
                                  ADI_44_headshake, ADI_45_conventgest, ADI_51_socsmile,
                                  ADI_52_showing, ADI_53_sharing, ADI_54_sharedenjoy,
                                  ADI_55_offercomfort, ADI_56_qual_social, ADI_57_rangefacialexp,
                                  ADI_58_inappfacialexp, ADI_59_socialresp, ADI_62_intchild),
                           na.rm = TRUE),
    
    # Calculate rrb_lord (sum of repetitive sensory motor and insistence on sameness)
    rrb_lord = rowSums(select(., ADI_67_preoccupy, ADI_69_repobj, ADI_71_unusualsense,
                              ADI_77_handfingerman, ADI_78_othermanner, ADI_70_compulsions,
                              ADI_72_noisesense, ADI_73_negrespstimuli, ADI_74_difficultchange),
                       na.rm = TRUE),
  ) %>%
  mutate(
    # Subscores and A, B, C, D totals
    A1 = rowSums(select(., ADI_50_directgaze, ADI_51_socsmile, ADI_57_rangefacialexp), na.rm = TRUE),
    A2 = rowSums(select(., ADI_49_imagplay_peers, ADI_62_intchild, ADI_63_respotherchild, ADI_64_grpplay, ADI_65_friends), na.rm = TRUE),
    A3 = rowSums(select(., ADI_52_showing, ADI_53_sharing, ADI_54_sharedenjoy), na.rm = TRUE),
    A4 = rowSums(select(., ADI_31_usebody, ADI_55_offercomfort, ADI_56_qual_social, ADI_58_inappfacialexp, ADI_59_socialresp), na.rm = TRUE),
    B1 = rowSums(select(., ADI_42_point, ADI_43_nodding, ADI_44_headshake, ADI_45_conventgest), na.rm = TRUE),
    B4 = rowSums(select(., ADI_47_spontimitation, ADI_48_imagplay, ADI_61_imitplay), na.rm = TRUE),
    B2V = case_when(
      is.verbal == 1 ~ rowSums(select(., ADI_34_socchat, ADI_35_recipcon), na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    B3V = case_when(
      is.verbal == 1 ~ rowSums(select(., ADI_33_stereo, ADI_36_innappq, ADI_37_pronounrev, ADI_38_neologisms), na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    C1 = rowSums(select(., ADI_67_preoccupy, ADI_68_circuminterest), na.rm = TRUE),
    C2 = rowSums(select(., ADI_39_verbrituals, ADI_70_compulsions), na.rm = TRUE),
    C3 = pmax(ADI_77_handfingerman, ADI_78_othermanner, na.rm = TRUE),
    C4 = pmax(ADI_69_repobj, ADI_71_unusualsense, na.rm = TRUE)
  ) %>%
  mutate(
    Total_A = rowSums(select(., A1, A2, A3, A4), na.rm = TRUE),
    Total_BV = case_when(
      is.verbal == 1 ~ rowSums(select(., B1, B4, B2V, B3V), na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    Total_NVB = case_when(
      is.verbal == 0 ~ rowSums(select(., B1, B4), na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    Total_C = rowSums(select(., C1, C2, C3, C4), na.rm = TRUE),
    ADI_2_score = if_else(ADI_2_sympt_onset < 36, 1, 0),
    ADI_9_score = if_else(ADI_9_age_singlewords > 24, 1, 0),
    ADI_10_score = if_else(ADI_10_age_firstphrases > 33, 1, 0),
    ADI_86_score = if_else(ADI_86_resp_ageabn %in% c(3, 4), 1, 0),
    ADI_87_score = if_else(ADI_87_clin_ageabn > 36, 1, 0),
    Total_D = ADI_2_score + ADI_9_score + ADI_10_score + ADI_86_score + ADI_87_score
  ) %>%
  mutate(
    # Add combined score for Social Communication and RRB
    Total_LORD = soc_com_lord + rrb_lord
  )

# 19.05.2025 Dsm ----------------------------------------------------------

adi_r_1_calc <- adi_r_1_calc %>%
  mutate(
    # DSM-5 Social Communication Domain (SC)
    sc_dsm5 = case_when(
      is.verbal == 1 ~ rowSums(select(., 
                                      # Ever scores for items 34-35
                                      ADI_34a_socchat_ever, ADI_35a_recipcon_ever,
                                      # Most Abnormal 4-5 scores for items 42-64
                                      ADI_42a_point_mostabn, ADI_43a_nodding_mostabn, 
                                      ADI_44a_headshake_mostabn, ADI_45a_conventgest_mostabn,
                                      ADI_46a_attendvoice_mostabn, ADI_47a_spontimitation_mostabn,
                                      ADI_49a_imagplay_peers_mostabn, ADI_50a_directgaze_mostabn,
                                      ADI_51a_socsmile_mostabn, ADI_52a_showing_mostabn,
                                      ADI_53a_sharing_mostabn, ADI_54a_sharedenjoy_mostabn,
                                      ADI_55a_offercomfort_mostabn, ADI_56a_qual_social_mostabn,
                                      ADI_57a_rangefacialexp_mostabn, ADI_59a_socialresp_mostabn,
                                      ADI_61a_imitplay_mostabn, ADI_62a_intchild_mostabn,
                                      ADI_63a_respotherchild_mostabn, ADI_64a_grpplay_mostabn
      ), na.rm = TRUE),
      
      is.verbal == 0 ~ rowSums(select(., 
                                      # Most Abnormal 4-5 scores for items 42-64 (excluding verbal-only items)
                                      ADI_42a_point_mostabn, ADI_43a_nodding_mostabn, 
                                      ADI_44a_headshake_mostabn, ADI_45a_conventgest_mostabn,
                                      ADI_46a_attendvoice_mostabn, ADI_47a_spontimitation_mostabn,
                                      ADI_49a_imagplay_peers_mostabn, ADI_50a_directgaze_mostabn,
                                      ADI_51a_socsmile_mostabn, ADI_52a_showing_mostabn,
                                      ADI_53a_sharing_mostabn, ADI_54a_sharedenjoy_mostabn,
                                      ADI_55a_offercomfort_mostabn, ADI_56a_qual_social_mostabn,
                                      ADI_57a_rangefacialexp_mostabn, ADI_59a_socialresp_mostabn,
                                      ADI_61a_imitplay_mostabn, ADI_62a_intchild_mostabn,
                                      ADI_63a_respotherchild_mostabn, ADI_64a_grpplay_mostabn
      ), na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    
    # DSM-5 Restricted and Repetitive Behaviors Domain (RRB)
    rrb_dsm5 = case_when(
      is.verbal == 1 ~ rowSums(select(., 
                                      # Ever scores for all RRB items
                                      ADI_33a_stereo_ever, ADI_39a_verbrituals_ever,
                                      ADI_67a_preoccupy_ever, ADI_68a_circuminterest_ever,
                                      ADI_69a_repobj_ever, ADI_70a_compulsions_ever,
                                      ADI_71a_unusualsense_ever, ADI_72a_noisesense_ever,
                                      ADI_73a_negrespstimuli_ever, ADI_74a_difficultchange_ever,
                                      ADI_77a_handfingerman_ever, ADI_78a_othermanner_ever
      ), na.rm = TRUE),
      
      is.verbal == 0 ~ rowSums(select(., 
                                      # Ever scores for RRB items (excluding verbal-only items)
                                      ADI_67a_preoccupy_ever, ADI_68a_circuminterest_ever,
                                      ADI_69a_repobj_ever, ADI_70a_compulsions_ever,
                                      ADI_71a_unusualsense_ever, ADI_72a_noisesense_ever,
                                      ADI_73a_negrespstimuli_ever, ADI_74a_difficultchange_ever,
                                      ADI_77a_handfingerman_ever, ADI_78a_othermanner_ever
      ), na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    
    # DSM-5 Total Score
    total_dsm5 = sc_dsm5 + rrb_dsm5,
    
    # DSM-5 Classification
    dsm5_classification = case_when(
      is.verbal == 1 & total_dsm5 > 28 ~ "ASD",
      is.verbal == 0 & total_dsm5 > 34 ~ "ASD",
      !is.na(total_dsm5) ~ "Non-ASD",
      TRUE ~ NA_character_
    )
  ) %>%
  # Relocate the new variables to an appropriate location in the dataframe
  relocate(sc_dsm5, rrb_dsm5, total_dsm5, dsm5_classification, .after = Total_LORD)


# 19.05.2025 DSM Adj ------------------------------------------------------

adi_r_1_calc <- adi_r_1_calc %>%
  mutate(
    # DSM-5 Social Communication Domain Adjusted (SC Adj) - using current instead of mostabn
    sc_dsm5_adj = case_when(
      is.verbal == 1 ~ rowSums(select(., 
                                      # Ever scores for items 34-35
                                      ADI_34a_socchat_ever, ADI_35a_recipcon_ever,
                                      # Current scores for items 42-64 (instead of mostabn)
                                      ADI_42_point, ADI_43_nodding, 
                                      ADI_44_headshake, ADI_45_conventgest,
                                      ADI_46_attendvoice, ADI_47_spontimitation,
                                      ADI_49_imagplay_peers, ADI_50_directgaze,
                                      ADI_51_socsmile, ADI_52_showing,
                                      ADI_53_sharing, ADI_54_sharedenjoy,
                                      ADI_55_offercomfort, ADI_56_qual_social,
                                      ADI_57_rangefacialexp, ADI_59_socialresp,
                                      ADI_61_imitplay, ADI_62_intchild,
                                      ADI_63_respotherchild, ADI_64_grpplay
      ), na.rm = TRUE),
      
      is.verbal == 0 ~ rowSums(select(., 
                                      # Current scores for items 42-64 (instead of mostabn, excluding verbal-only items)
                                      ADI_42_point, ADI_43_nodding, 
                                      ADI_44_headshake, ADI_45_conventgest,
                                      ADI_46_attendvoice, ADI_47_spontimitation,
                                      ADI_49_imagplay_peers, ADI_50_directgaze,
                                      ADI_51_socsmile, ADI_52_showing,
                                      ADI_53_sharing, ADI_54_sharedenjoy,
                                      ADI_55_offercomfort, ADI_56_qual_social,
                                      ADI_57_rangefacialexp, ADI_59_socialresp,
                                      ADI_61_imitplay, ADI_62_intchild,
                                      ADI_63_respotherchild, ADI_64_grpplay
      ), na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    
    # RRB domain remains the same - it already uses "ever" scores which is appropriate
    # But we'll copy it to keep naming consistent
    rrb_dsm5_adj = rrb_dsm5,
    
    # DSM-5 Adjusted Total Score
    total_dsm5_adj = sc_dsm5_adj + rrb_dsm5_adj,
    
    # DSM-5 Adjusted Classification
    dsm5_classification_adj = case_when(
      is.verbal == 1 & total_dsm5_adj > 28 ~ "ASD",
      is.verbal == 0 & total_dsm5_adj > 34 ~ "ASD",
      !is.na(total_dsm5_adj) ~ "Non-ASD",
      TRUE ~ NA_character_
    )
  ) %>%
  # Relocate the new variables in the dataframe
  relocate(sc_dsm5_adj, rrb_dsm5_adj, total_dsm5_adj, dsm5_classification_adj, .after = dsm5_classification)


# relocating --------------------------------------------------------------


adi_r_1_calc <- adi_r_1_calc %>%
  relocate(
    is.verbal, A1, A2, A3, A4, Total_A,
    B1, B4, B2V, B3V, Total_BV, Total_NVB,
    C1, C2, C3, C4, Total_C, Total_D, soc_com_lord, rrb_lord, Total_LORD,
    .after = visit
  )

adi_r_1_interest <- adi_r_1_calc |> 
  select(id, visit, is.verbal,
         A1, A2, A3, A4, Total_A,
         B1, B4, B2V, B3V, Total_BV, Total_NVB,
         C1, C2, C3, C4, Total_C,
         Total_D, soc_com_lord, rrb_lord, Total_LORD, 
         sc_dsm5, rrb_dsm5, total_dsm5, dsm5_classification,
         sc_dsm5_adj, rrb_dsm5_adj, total_dsm5_adj, dsm5_classification_adj)


adi_ados_merge <- full_join(interest, adi_r_1_interest[, c("visit", "id", "is.verbal",
                                                           "A1", "A2", "A3", "A4", "Total_A",
                                                           "B1", "B4", "B2V", "B3V", "Total_BV", "Total_NVB",
                                                           "C1", "C2", "C3", "C4", "Total_C",
                                                           "Total_D", "soc_com_lord", "rrb_lord", "Total_LORD",
                                                           "sc_dsm5", "rrb_dsm5", "total_dsm5", "dsm5_classification",
                                                           "sc_dsm5_adj", "rrb_dsm5_adj", "total_dsm5_adj", "dsm5_classification_adj")], by = "id")

adi_ados_merge <- adi_ados_merge %>%
  # Combine the visit columns into a single column
  mutate(visit = ifelse(!is.na(visit.x), visit.x, visit.y)) %>%
  # Remove the old visit columns
  select(-visit.x, -visit.y) %>%
  # Remove rows where visit equals 4
  filter(visit != 4) %>%
  # Relocate Visits to front
  relocate(visit, .after = "id")


adi_ados_merge <- adi_ados_merge %>%
  mutate(across(
    c(Total_A, Total_BV, Total_NVB, Total_C, 
      soc_com_lord, rrb_lord, Total_LORD,
      sc_dsm5, rrb_dsm5, total_dsm5,
      sc_dsm5_adj, rrb_dsm5_adj, total_dsm5_adj,
      ,  ,  ,  ),
    ~if_else(adi_missing_items_count > 8, NA_real_, .)
  ))

adi_ados_plot <- adi_ados_merge %>% 
  rename(
    "SRS total parent" = srs_total_t,
    "ADI total" = Total_LORD,
    "ADOS severity" = ados_severity,
    "SRS SCI parent" = srs_total_social_t,
    "ADI social-com" = soc_com_lord,
    "ADOS SA severity" = ados_sa_severity,
    "RBSR total" = rbs_6_factor_total,
    "SRS RRB parent" = srs_rrb_t,
    "ADI RRB" = rrb_lord,
    "ADOS RRB severity" = ados_rrb_severity,
    "ADI DSM Total" = total_dsm5,
    "ADI DSM Social" = sc_dsm5,
    "ADI DSM RRB" = rrb_dsm5,
    "ADI DSM Adj Total" = total_dsm5_adj,
    "ADI DSM Adj Social" = sc_dsm5_adj,
    "ADI DSM Adj RRB" = rrb_dsm5_adj,
    rbsr_stereotypy = rbs_6_factor_stereotypy,
    rbsr_self_injurious = rbs_6_factor_self_injurious,
    rbsr_compulsive = rbs_6_factor_compulsive,
    rbsr_ritualistic = rbs_6_factor_ritualistic,
    rbsr_sameness = rbs_6_factor_sameness,
    rbsr_restricted = rbs_6_factor_restricted,
    rbsr_total = rbs_6_factor_total
  ) %>% 
  mutate(
    "RBSR total" = rbsr_total,
    
  )

# Right after final adi_ados_plot is created (before any filtering!)
original_adi_ados_plot <- adi_ados_plot


adi_ados_plot2 <- adi_ados_merge %>% 
  rename(
    "SRS total parent" = srs_total_t,
    adi_total = Total_LORD,
    ados_css = ados_severity,
    "SRS SCI parent" = srs_total_social_t,
    adi_sa = soc_com_lord,
    ados_sa_css = ados_sa_severity,
    "SRS RRB parent" = srs_rrb_t,
    adi_rrb = rrb_lord,
    ados_rrb_css = ados_rrb_severity,
    rbsr_stereotypy = rbs_6_factor_stereotypy,
    rbsr_self_injurious = rbs_6_factor_self_injurious,
    rbsr_compulsive = rbs_6_factor_compulsive,
    rbsr_ritualistic = rbs_6_factor_ritualistic,
    rbsr_sameness = rbs_6_factor_sameness,
    rbsr_restricted = rbs_6_factor_restricted,
    rbsr_total = rbs_6_factor_total
  ) 

# Corrplot ----------------------------------------------------------------
library(corrplot)
create_styled_corrplot <- function(data, title) {
  correlation_matrix <- cor(data, use = "pairwise.complete.obs")
  pmat <- cor.mtest(data, use = "pairwise.complete.obs")
  correlation_matrix[is.na(correlation_matrix)] <- 0
  
  # Create corrplot
  corrplot(correlation_matrix,
           method = "shade",
           type = "lower",
           p.mat = pmat$p,
           order = "original",
           addrect = 3,
           col = colorRampPalette(c("blue", "white", "red"))(200), # Use a color ramp for the palette
           insig = "label_sig",
           sig.level = c(.001, .01, .05),
           pch.cex = 1.4,
           pch.col = "orange",
           tl.col = "black",
           tl.cex = 1,
           addCoef.col = "black",
           tl.srt = 45,
           diag = TRUE,
           number.cex = 1,
           outline = TRUE)
  
  title(title)
}




# prescool plots 180225 ---------------------------------------------------
trace(corrplot, edit = TRUE)

# 1. WITHIN-MEASURE CORRPLOTS


# 1.2 ADOS: 
create_styled_corrplot(
  adi_ados_plot %>% select(`ADOS severity`, `ADOS RRB severity`, `ADOS SA severity`), "")

# 1.3 SRS: 
create_styled_corrplot(
  adi_ados_plot %>% select(`SRS total parent`, `SRS SCI parent`, `SRS RRB parent`,srs_awr_t, srs_cog_t, srs_mot_t, srs_com_t), "")

# 1.4 RBS: using variables in the order: "RBS stereotypy", "RBS self-injurious", "RBS sameness", "RBS compulsive", "RBS restricted", "RBS ritualistic"
create_styled_corrplot(
  adi_ados_plot %>% select(rbsr_total, rbsr_stereotypy, rbsr_self_injurious, rbsr_sameness, rbsr_compulsive,rbsr_restricted,rbsr_ritualistic), "")

# 2. ACROSS-MEASURES CORRPLOTS

# 2.1 Total Severity: (ADOS severity, ADI total, SRS total parent)
create_styled_corrplot(
  adi_ados_plot %>% select(`ADOS severity`, `ADI total`, `SRS total parent`), "")

# 2.2 Social Communication: (ADOS SA severity, ADI social-com, SRS SCI parent)
create_styled_corrplot(
  adi_ados_plot %>% select(`ADOS SA severity`, `ADI social-com`, `SRS SCI parent`), "")

# 2.3 RRB: (ADOS RRB severity, ADI RRB, SRS RRB parent, RBSR total)
create_styled_corrplot(
  adi_ados_plot %>% select(`ADOS RRB severity`, `ADI RRB`, `SRS RRB parent`, `RBSR total`), "")

# 3. EVERYTHING: (ADOS severity, ADOS SA severity, ADOS RRB severity, ADI total, ADI social-com, ADI RRB, SRS total parent, SRS SCI parent, SRS RRB parent, RBSR total)
create_styled_corrplot(adi_ados_plot %>% select(`ADOS severity`, `ADOS SA severity`, `ADOS RRB severity`, `ADI total`, `ADI social-com`, `ADI RRB`, `SRS total parent`, `SRS SCI parent`, `SRS RRB parent`, `RBSR total`), "")



# ilan plts ---------------------------------------------------------------
# 
# create_styled_scatterplot <- function(data, title = "Scatterplot with Regression and Jitter") {
#   # Validate that the data has at least two columns
#   if (ncol(data) < 2) {
#     stop("Data must contain at least two variables.")
#   }
#   
#   # Extract the first two column names as x and y variables
#   vars <- names(data)[1:2]
#   x_var <- vars[1]
#   y_var <- vars[2]
#   
#   # Compute Pearson correlation and its p-value
#   cor_res <- cor.test(data[[x_var]], data[[y_var]], method = "pearson")
#   correlation_coef <- cor_res$estimate
#   p_value <- cor_res$p.value
#   
#   # Append correlation text to the title
#   full_title <- sprintf("%s r = %.2f, p = %.3f", title, correlation_coef, p_value)
#   
#   # Create the jittered scatterplot with a regression line
#   p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
#     geom_jitter(color = "#2c7fb8", size = 3, alpha = 0.7, width = 0.2, height = 0.2) +
#     geom_smooth(method = "lm", color = "black", fill = "gray", se = TRUE) +  # regression line in black
#     labs(title = full_title,
#          x = x_var,
#          y = y_var) +
#     theme_bw(base_size = 14) +
#     theme(
#       plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
#       axis.title = element_text(face = "bold"),
#       axis.text = element_text(color = "black"),
#       # Remove grid lines
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank()
#     )
#   
#   # Print the plot
#   print(p)
# }
# 
# 
# create_styled_scatterplot(
#   adi_ados_plot2 %>% select(ados_css, adi_total),
#   ""
# )
# 
# #social
# create_styled_scatterplot(
#   adi_ados_plot2 %>% select(ados_sa_css, adi_sa),
#   ""
# )
# 
# #rrb
# create_styled_scatterplot(
#   adi_ados_plot2 %>% select(ados_rrb_css, adi_rrb),
#   ""
# )
# 

# write_xlsx(adi_ados_merge, "mind_publish_180225_preschool.xlsx")

# t.test ------------------------------------------------------------------
# 
# t.test(JA ~ is.verbal, data = adi_ados_merge)
# 
# t.test(srs_total_social_t ~ is.verbal, data = adi_ados_merge)
# 
# ggplot(adi_ados_merge, aes(x = as.factor(is.verbal), y = srs_total_social_t, fill = as.factor(is.verbal))) +
#   geom_boxplot(outlier.shape = NA, alpha = 0.5) +  # Boxplot without outliers, semi-transparent
#   geom_jitter(width = 0.2, size = 1.5, alpha = 0.6) +  # Jittered points
#   stat_summary(fun = mean, geom = "point", shape = 22, size = 5, fill = "white") +  # Mean points
#   labs(x = "Verbal Status", y = "Total Social T Score (SRS)", 
#        title = "Comparison of SRS Total Social T Scores by Verbal Status",
#        fill = "Verbal Status") +
#   scale_x_discrete(drop = T, labels = c("0" = "Non-verbal", "1" = "Verbal")) +  # Custom x-axis labels
#   theme_minimal() +  # Clean theme
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Centered and bold title
#     axis.title = element_text(size = 12),  # Axis titles size
#     axis.text = element_text(size = 10),  # Axis text size
#     legend.position = "none"  # Remove legend if not needed
#   ) +
#   geom_signif(comparisons = list(c("0", "1")), 
#               map_signif_level = TRUE,
#               textsize = 6,
#               tip_length = 0.02,
#               y_position = max(adi_ados_merge$srs_total_social_t, na.rm = TRUE) * 1.05) +
# scale_x_discrete(drop = TRUE, limits = c("0", "1"))  # Drop the NA group from the x-axis
# 
# 

# saving plots ------------------------------------------------------------
# 
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

file.copy(from=plots.png.paths, to= "C:/Users/galor/OneDrive/Desktop/MIND/plots180225/plots 25.02.25/MIND_preschool_new")


# chars -------------------------------------------------------------------
adi_ados_merge_char <- adi_ados_merge %>%
  left_join(select(visit_df2, id, sex), by = "id")

adi_ados_chars <- adi_ados_merge_char %>%
  rename(
    # ADI scores
    individual = id,
    adi_social_edx = soc_com_lord,
    adi_rrb_edx = rrb_lord,
    adi_a1 = A1,
    adi_a2 = A2,
    adi_a3 = A3,
    adi_a4 = A4,
    adi_a_total = adi_totalA,
    adi_ba = B1,
    adi_b2 = B2V,
    adi_b3 = B3V,
    adi_b4 = B4,
    adi_b_verbal_total = adi_totalVB,
    adi_b_nonverbal_total = adi_totalNVB,
    adi_c1 = C1,
    adi_c2 = C2,
    adi_c3 = C3,
    adi_c4 = C4,
    adi_c_total = adi_totalC,
    adi_total_edx = Total_LORD,
    
    # ADOS scores
    ados_sa_css = ados_sa_severity,
    ados_rrb_css = ados_rrb_severity,
    ados_css = ados_severity,
    
    # RBS-R scores
    rbsr_i = rbs_6_factor_stereotypy,
    rbsr_ii = rbs_6_factor_self_injurious,
    rbsr_iii = rbs_6_factor_compulsive,
    rbsr_iv = rbs_6_factor_ritualistic,
    rbsr_v = rbs_6_factor_sameness,
    rbsr_vi = rbs_6_factor_restricted,
    rbsr_total = rbs_6_factor_total,
    
    # SRS scores
    srs_total_t = srs_total_t,
    srs_social_t = srs_total_social_t
    
  )
describe(adi_ados_chars)

adi_ados_chars_factors <- adi_ados_chars |> 
  select(is.verbal,sex.x,origin)


# #srsparent
# writexl::write_xlsx(describe(adi_ados_chars), "mind_chars110524.xlsx") <=== 
# כרגע מה שצריך לעשות זה: 
# 4. להריץ הכל על הschool age של ssc, הגרפים שהראת לי היום בעצם ואני צריכה קובץ 
# 5. להריץ הכל על school age של mind, כלומר לפתור את בעיית ה age


results_char <- describeBy(adi_ados_chars, group = adi_ados_chars$sex.x)
# 
# 
# writexl::write_xlsx((results_char[["Male"]]), "mind_male110524.xlsx")
# writexl::write_xlsx((results_char[["Female"]]), "mind_Female110524.xlsx")



# Plotting again 25/02/25 renaming and adding titles  ----------------------------------------------------------
# MALES only
# adi_ados_plot <- original_adi_ados_plot %>% filter(sex == "Female")


create_styled_corrplot <- function(data, title) {
  # Save current graphics parameters
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  
  # Increase top margin for title space
  par(mar = c(5, 4, 4, 2) + 0.1)  # Increased top margin from default
  
  correlation_matrix <- cor(data, use = "pairwise.complete.obs")
  pmat <- cor.mtest(data, use = "pairwise.complete.obs")
  correlation_matrix[is.na(correlation_matrix)] <- 0
  
  # Count only the columns in the subset (not the entire adi_ados_plot)
  n_vars <- ncol(data)
  
  # Create a linear function for dynamic scaling:
  # For 3 variables, we want number_cex = 1.3 and for 10 variables, number_cex = 0.7.
  # Solve for f(n) = a * n + b:
  #   f(3) = 1.3  --> 3a + b = 1.3
  #   f(10) = 0.7 --> 10a + b = 0.7
  # Subtracting gives: 7a = -0.6  ->  a = -0.0857 (approx)
  # Then, b = 1.3 - 3*(-0.0857) ≈ 1.5571
  number_cex <- 1.5571 - 0.0857 * n_vars
  # Similarly, you can adjust pch_cex if desired. Here’s an example scaling:
  pch_cex <- 2.1571 - 0.1 * n_vars  # Adjust or compute dynamically as needed
  
  corrplot(correlation_matrix,
           method = "shade",
           type = "lower",
           p.mat = pmat$p,
           order = "original",
           addrect = 3,
           col = colorRampPalette(c("blue", "white", "red"))(200),
           insig = "label_sig",
           sig.level = c(.001, .01, .05),
           pch.col = "orange",
           pch.cex = pch_cex,
           tl.col = "black",
           tl.cex = 0.9,   # Reduced from 1.0 to prevent label overlap
           addCoef.col = "black",
           tl.srt = 45,
           diag = TRUE,
           outline = TRUE,
           mar = c(0,0,2,0))  # Add margin within corrplot for title
  
  
  # Calculate dynamic adjustment based on title length
  # For 3 chars: adj = 0.15, for 18 chars: adj = 0.03
  # Linear interpolation: adj = 0.15 - (title_length - 3) * (0.15 - 0.03) / (18 - 3)
  title_length <- nchar(title)
  
  # Set bounds and calculate adjustment
  if (title_length <= 3) {
    adj_value <- 0.15
  } else if (title_length >= 18) {
    adj_value <- 0.03
  } else {
    # Linear interpolation for titles between 3 and 18 characters
    adj_value <- 0.15 - (title_length - 3) * 0.008
  }
  
  # Add title with dynamic adjustment
  title(main = title, line = 2.5, adj = adj_value)
}
# pdf("mind_pres_060425.pdf", width = 8, height = 6)
pdf("mind_preschool_ADI_DSM_Adj.pdf", width = 8, height = 6)
# Create output directory
# dir.create("MIND_preschool_plots_ADI_DSM_Adj", showWarnings = FALSE)

# 1. WITHIN-MEASURE CORRPLOTS

# 1.1 ADI DSM Adj ONLY (for MIND preschool):

create_styled_corrplot(
  adi_ados_plot %>% 
    select(`ADI DSM Adj Total`, `ADI DSM Adj Social`, `ADI DSM Adj RRB`) %>%
    setNames(c("ADI Total", "ADI Social Com", "ADI RRB")), 
  "ADI DSM-5 Adjusted")

# 1.2 ADOS: 
create_styled_corrplot(
  adi_ados_plot %>% 
    select(`ADOS severity`, `ADOS SA severity`, `ADOS RRB severity`) %>%
    setNames(c("ADOS Severity", "ADOS SA", "ADOS RRB")), 
  "ADOS")

# 1.3 SRS: 
create_styled_corrplot(
  adi_ados_plot %>% 
    select(`SRS total parent`, `SRS SCI parent`, `SRS RRB parent`, 
           srs_awr_t, srs_cog_t, srs_mot_t, srs_com_t) %>%
    setNames(c("SRS Total", "SRS Social", "SRS RRB", 
               "SRS Awareness", "SRS Cognition", "SRS Motivation", "SRS Communication")), 
  "SRS Parent")

# 1.4 RBS: 
create_styled_corrplot(
  adi_ados_plot %>% 
    select(rbsr_total, rbsr_stereotypy, rbsr_self_injurious, rbsr_compulsive, 
           rbsr_ritualistic, rbsr_sameness, rbsr_restricted) %>%
    setNames(c("RBS-R Total", "RBS-R Stereotypy", "RBS-R Self-Injurious", 
               "RBS-R Compulsive", "RBS-R Ritualistic", 
               "RBS-R Sameness", "RBS-R Restricted")), 
  "RBS-R")

# 2. ACROSS-MEASURES CORRPLOTS

# 2.1 Total Severity (ONLY ADI DSM Adj for MIND preschool):
create_styled_corrplot(
  adi_ados_plot %>% 
    select(`ADOS severity`, `ADI DSM Adj Total`, `SRS total parent`) %>%
    setNames(c("ADOS Severity", "ADI Total", "SRS Total Parent")), 
  "Total Severity")

# 2.2 Social Communication (ONLY ADI DSM Adj for MIND preschool):
create_styled_corrplot(
  adi_ados_plot %>% 
    select(`ADOS SA severity`, `ADI DSM Adj Social`, `SRS SCI parent`) %>%
    setNames(c("ADOS SA", "Adi Social Com", "SRS Social Parent")), 
  "Social Communication")

# 2.3 RRB (ONLY ADI DSM Adj for MIND preschool):
create_styled_corrplot(
  adi_ados_plot %>% 
    select(`ADOS RRB severity`, `ADI DSM Adj RRB`, `SRS RRB parent`, `RBSR total`) %>%
    setNames(c("ADOS RRB", "ADI RRB", "SRS RRB Parent", "RBS-R Total")), 
  "RRB")

create_styled_corrplot <- function(data, title) {
  # Save current graphics parameters and restore on exit
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  
  # Define a custom plotting region that leaves extra space at the top.
  # fig = c(x1, x2, y1, y2) with values between 0 and 1.
  # Here, we leave 15% of the vertical space empty on top.
  par(fig = c(0, 1, 0, 0.95))
  
  correlation_matrix <- cor(data, use = "pairwise.complete.obs")
  pmat <- cor.mtest(data, use = "pairwise.complete.obs")
  correlation_matrix[is.na(correlation_matrix)] <- 0
  
  # Create corrplot in the defined region
  corrplot(correlation_matrix,
           method = "shade",
           type = "lower",
           p.mat = pmat$p,
           order = "original",
           addrect = 3,
           col = colorRampPalette(c("blue", "white", "red"))(200),
           insig = "label_sig",
           sig.level = c(.001, .01, .05),
           pch.cex = 0.8,
           pch.col = "orange",
           tl.col = "black",
           tl.cex = 1,
           addCoef.col = "black",
           tl.srt = 45,
           diag = TRUE,
           number.cex = 0.8,
           outline = TRUE)
  
  # Calculate dynamic adjustment based on title length
  # For 3 chars: adj = 0.15, for 18 chars: adj = 0.03
  # Linear interpolation: adj = 0.15 - (title_length - 3) * (0.15 - 0.03) / (18 - 3)
  title_length <- nchar(title)
  
  # Set bounds and calculate adjustment
  if (title_length <= 3) {
    adj_value <- 0.15
  } else if (title_length >= 18) {
    adj_value <- 0.03
  } else {
    # Linear interpolation for titles between 3 and 18 characters
    adj_value <- 0.15 - (title_length - 3) * 0.008
  }
  
  # Add title with dynamic adjustment
  title(main = title, line = 2.5, adj = adj_value)
}

# 3. EVERYTHING (ONLY ADI DSM Adj scores for MIND preschool):
create_styled_corrplot(
  adi_ados_plot %>% 
    select(`ADOS severity`, `ADOS SA severity`, `ADOS RRB severity`, 
           `ADI DSM Adj Total`, `ADI DSM Adj Social`, `ADI DSM Adj RRB`,
           `SRS total parent`, `SRS SCI parent`, `SRS RRB parent`, `RBSR total`) %>%
    setNames(c("ADOS Severity", "ADOS SA", "ADOS RRB", 
               "ADI Total", "ADI Social Com", "ADI RRB",
               "SRS Total Parent", "SRS Social Parent", "SRS RRB Parent", "RBS-R Total")), 
  "All Measures")

# Export with clear naming for MIND preschool
write_xlsx(adi_ados_plot, "mind_preschool_ADI_DSM_Adj_251224.xlsx")

# create_styled_corrplot <- function(data, title) {
#   # Save current graphics parameters and restore on exit
#   old_par <- par(no.readonly = TRUE)
#   on.exit(par(old_par), add = TRUE)
#   
#   # Define a custom plotting region that leaves extra space at the top.
#   # fig = c(x1, x2, y1, y2) with values between 0 and 1.
#   # Here, we leave 15% of the vertical space empty on top.
#   par(fig = c(0, 1, 0, 0.95))
#   
#   correlation_matrix <- cor(data, use = "pairwise.complete.obs")
#   pmat <- cor.mtest(data, use = "pairwise.complete.obs")
#   correlation_matrix[is.na(correlation_matrix)] <- 0
#   
#   # Create corrplot in the defined region
#   corrplot(correlation_matrix,
#            method = "shade",
#            type = "lower",
#            p.mat = pmat$p,
#            order = "original",
#            addrect = 3,
#            col = colorRampPalette(c("blue", "white", "red"))(200),
#            insig = "label_sig",
#            sig.level = c(.001, .01, .05),
#            pch.cex = 1.2,
#            pch.col = "orange",
#            tl.col = "black",
#            tl.cex = 1,
#            addCoef.col = "black",
#            tl.srt = 45,
#            diag = TRUE,
#            number.cex = 0.7,
#            outline = TRUE)
#   
#   # Add title (it will appear in the margin above the defined plot region)
#   title(title)
# }




dev.off()

write_xlsx(adi_ados_plot, "mind_publish_190525_preschool.xlsx")

adi_ados_plot %>% 
  distinct(id, .keep_all = TRUE) %>% 
  summarise(Total = n(), Male = sum(sex == "Male"), Female = sum(sex == "Female"))

mind_preschool_chars <- adi_ados_plot %>%
  select(
    # Demographics
    id, sex, ados_age,
    # ADOS
    `ADOS severity`, `ADOS SA severity`, `ADOS RRB severity`,
    # ADI DSM Adj (for preschool)
    `ADI DSM Adj Total`, `ADI DSM Adj Social`, `ADI DSM Adj RRB`,
    # SRS Parent
    `SRS total parent`, `SRS SCI parent`, `SRS RRB parent`,
    srs_awr_t, srs_cog_t, srs_mot_t, srs_com_t,
    # RBS-R
    `RBSR total`, rbsr_stereotypy, rbsr_self_injurious,
    rbsr_compulsive, rbsr_ritualistic, rbsr_sameness, rbsr_restricted
  )

# Rename columns for cleaner output
mind_preschool_chars_clean <- mind_preschool_chars %>%
  rename(
    # Demographics
    Age = ados_age,
    # ADOS
    ADOS_Total = `ADOS severity`,
    ADOS_SA = `ADOS SA severity`,
    ADOS_RRB = `ADOS RRB severity`,
    # ADI
    ADI_Total = `ADI DSM Adj Total`,
    ADI_Social = `ADI DSM Adj Social`,
    ADI_RRB = `ADI DSM Adj RRB`,
    # SRS
    SRS_Total_Parent = `SRS total parent`,
    SRS_Social_Parent = `SRS SCI parent`,
    SRS_RRB_Parent = `SRS RRB parent`,
    SRS_Awareness_Parent = srs_awr_t,
    SRS_Cognition_Parent = srs_cog_t,
    SRS_Motivation_Parent = srs_mot_t,
    SRS_Communication_Parent = srs_com_t,
    # RBS-R
    RBSR_Total = `RBSR total`,
    RBSR_Stereotypy = rbsr_stereotypy,
    RBSR_Self_Injurious = rbsr_self_injurious,
    RBSR_Compulsive = rbsr_compulsive,
    RBSR_Ritualistic = rbsr_ritualistic,
    RBSR_Sameness = rbsr_sameness,
    RBSR_Restricted = rbsr_restricted
  )

# Descriptive statistics with variable names preserved
mind_preschool_desc <- describe(mind_preschool_chars_clean %>% select(-id, -sex))
mind_preschool_desc_df <- as.data.frame(mind_preschool_desc)
mind_preschool_desc_df <- rownames_to_column(mind_preschool_desc_df, var = "Variable")

# Gender distribution
mind_preschool_gender <- mind_preschool_chars %>%
  filter(!is.na(sex)) %>%
  count(sex) %>%
  mutate(percentage = n / sum(n) * 100)

# Summary
mind_preschool_summary <- data.frame(
  Sample = "MIND Preschool",
  N = nrow(mind_preschool_chars),
  Age_Mean = mean(mind_preschool_chars$ados_age, na.rm=TRUE),
  Age_SD = sd(mind_preschool_chars$ados_age, na.rm=TRUE),
  Age_Min = min(mind_preschool_chars$ados_age, na.rm=TRUE),
  Age_Max = max(mind_preschool_chars$ados_age, na.rm=TRUE),
  Male_N = sum(mind_preschool_chars$sex == "Male", na.rm=TRUE),
  Female_N = sum(mind_preschool_chars$sex == "Female", na.rm=TRUE),
  Male_Percent = round(sum(mind_preschool_chars$sex == "Male", na.rm=TRUE) / 
                         sum(!is.na(mind_preschool_chars$sex)) * 100, 1),
  Female_Percent = round(sum(mind_preschool_chars$sex == "Female", na.rm=TRUE) / 
                           sum(!is.na(mind_preschool_chars$sex)) * 100, 1)
)

# Export
write_xlsx(list(
  "Summary" = mind_preschool_summary,
  "Descriptive_Stats" = mind_preschool_desc_df,
  "Gender_Distribution" = mind_preschool_gender
), "MIND_Preschool_Characteristics1.xlsx")

# Print summary to console
cat("MIND Preschool Sample Characteristics:\n")
print(mind_preschool_summary)
cat("\nFirst few variables:\n")
print(head(mind_preschool_desc_df))

source("C://Users/galor/OneDrive/Desktop/MIND/diag.R")
source("C://Users//galor//OneDrive//Desktop//MIND//savecorrelations.R")
save_correlation_matrix_MIND(adi_ados_plot, "MIND_Preschool_Girls")
MIND_Preschool_Girls
getwd()
