# libraries ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(qdapRegex, patchwork,coin, ggside, pacman, readxl, writexl, dplyr, tidyverse, gridExtra, openxlsx, ggsignif, lubridate, psych, corrplot, janitor, purrr)


# library(readxl)
# library(writexl)
# library(dplyr)
# library(tidyverse)
# library(gridExtra)
# library(openxlsx)
# library(ggsignif)
# library(lubridate)
# library(psych)
# library(corrplot)
# library(janitor)
# library(purrr)


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


plot_all_distributions <- function(df, plots_per_page = 9) {
  plot_list <- lapply(names(df), function(var) {
    # Remove NA values for the current variable
    df_clean <- df[!is.na(df[[var]]), ]
    
    if (is.numeric(df_clean[[var]])) {
      p <- ggplot(df_clean, aes(x = !!sym(var))) +
        geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
        labs(title = var, x = var, y = "Density") +
        theme_minimal()
      
      mean_val <- mean(df_clean[[var]], na.rm = TRUE)
      sd_val <- sd(df_clean[[var]], na.rm = TRUE)
      
      p + stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), color = "red", linewidth = 1)
    } else {
      p <- ggplot(df_clean, aes(x = !!sym(var))) +
        geom_bar(fill = "blue", color = "black", alpha = 0.7) +
        labs(title = var, x = var, y = "Count") +
        theme_minimal()
      p
    }
  })
  
  # Remove NULL elements from the plot list (in case all values were NA for a variable)
  plot_list <- plot_list[!sapply(plot_list, is.null)]
  
  num_plots <- length(plot_list)
  num_pages <- ceiling(num_plots / plots_per_page)
  
  for (page in 1:num_pages) {
    start <- (page - 1) * plots_per_page + 1
    end <- min(page * plots_per_page, num_plots)
    grid.arrange(grobs = plot_list[start:end], ncol = 3)
  }
}


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

# load --------------------------------------------------------------------


# ssc_common <- read.csv("C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Databases\\SSC\\Summary - general\\ssc_commonly_used.csv")
# ssc_core <- read.csv("C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Databases\\SSC\\Summary - general\\ssc_core_descriptive.csv")
# ssc_ados1 <- read.csv("C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Databases\\SSC\\symptom meausres\\ados_1.csv")
# ssc_ados1_raw <- read.csv("C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Databases\\SSC\\symptom meausres\\ados_1_raw.csv") |> mutate(origin = "ados1")
# ssc_ados2 <- read.csv("C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Databases\\SSC\\symptom meausres\\ados_2.csv")
# ssc_ados2_raw <- read.csv("C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Databases\\SSC\\symptom meausres\\ados_2_raw.csv") |> mutate(origin = "ados2")
# ssc_ados3 <- read.csv("C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Databases\\SSC\\symptom meausres\\ados_3.csv")
# ssc_ados3_raw <- read.csv("C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Databases\\SSC\\symptom meausres\\ados_3_raw.csv") |> mutate(origin = "ados3")
# srs_parent_df <- read.csv("C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Databases\\SSC\\symptom meausres\\srs_parent.csv")
# srs_teacher_df <- read.csv("C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Databases\\SSC\\symptom meausres\\srs_teacher.csv")
# srs_parent_raw <- read.csv("C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Databases\\SSC\\symptom meausres\\srs_parent_recode.csv")
# srs_teacher_raw <- read.csv("C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Databases\\SSC\\symptom meausres\\srs_teacher_recode.csv")
# adi_df <- read.csv("C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Databases\\SSC\\symptom meausres\\adi_r.csv")
# adi_df_scores <- read.csv("C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Databases\\SSC\\symptom meausres/adi_r_diagnostic.csv")
# ssc_rbsr <- read.csv("C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Databases\\SSC\\symptom meausres/rbs_r.csv")
# ssc_rbsr_raw <- read.csv("C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Databases\\SSC\\symptom meausres/rbs_r_raw.csv")
# 



ssc_common <- read.csv("C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/SSC/Summary - general/ssc_commonly_used.csv")
ssc_core <- read.csv("C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/SSC/Summary - general/ssc_core_descriptive.csv")
ssc_ados1 <- read.csv("C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/SSC/symptom meausres/ados_1.csv")
ssc_ados1_raw <- read.csv("C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/SSC/symptom meausres/ados_1_raw.csv") |> mutate(origin = "ados1")
ssc_ados2 <- read.csv("C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/SSC/symptom meausres/ados_2.csv")
ssc_ados2_raw <- read.csv("C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/SSC/symptom meausres/ados_2_raw.csv") |> mutate(origin = "ados2")
ssc_ados3 <- read.csv("C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/SSC/symptom meausres/ados_3.csv")
ssc_ados3_raw <- read.csv("C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/SSC/symptom meausres/ados_3_raw.csv") |> mutate(origin = "ados3")
srs_parent_df <- read.csv("C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/SSC/symptom meausres/srs_parent.csv")
srs_teacher_df <- read.csv("C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/SSC/symptom meausres/srs_teacher.csv")
srs_parent_raw <- read.csv("C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/SSC/symptom meausres/srs_parent_recode.csv")
srs_teacher_raw <- read.csv("C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/SSC/symptom meausres/srs_teacher_recode.csv")
adi_df <- read.csv("C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/SSC/symptom meausres/adi_r.csv")
adi_df_scores <- read.csv("C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/SSC/symptom meausres/adi_r_diagnostic.csv")
ssc_rbsr <- read.csv("C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/SSC/symptom meausres/rbs_r.csv")
ssc_rbsr_raw <- read.csv("C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/SSC/symptom meausres/rbs_r_raw.csv")

# .- - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - -  ------------------------------------------

# ADOS_SSC ------------------------ ----------------------------------------------------------------------
ados_raw <- bind_rows(
  ssc_ados1_raw,
  ssc_ados2_raw,
  ssc_ados3_raw
) %>%
  arrange(individual, measure) |> 
  relocate(origin, .after = measure)


core_selected <- ssc_core %>%
  select(individual,
         sex,
         ados_communication_social, 
         ados_css,
         ados_module,
         ados_restricted_repetitive,
         ados_social_affect,
         age_at_ados)

# Select necessary columns from Commonly Used
common_selected <- ssc_common %>%
  select(individual,
         ados_communication, #suma_old
         ados_reciprocal_social) #sumb_old

# Select necessary columns from ADOS 1, 2, and 3 tables, and add "origin" column
ados1_selected <- ssc_ados1 %>%
  select(individual,
         communication_social, 
         css, 
         restricted_repetitive, 
         social_affect,
         algorithm) %>%
  mutate(origin = "ADOS 1")

ados2_selected <- ssc_ados2 %>%
  select(individual,
         communication_social, 
         css, 
         restricted_repetitive, 
         social_affect,
         algorithm) %>%
  mutate(origin = "ADOS 2")

ados3_selected <- ssc_ados3 %>%
  select(individual,
         communication_social, 
         css, 
         restricted_repetitive, 
         social_affect) %>%
  mutate(origin = "ADOS 3")

# Combine ADOS datasets
ados_combined <- bind_rows(ados1_selected, ados2_selected, ados3_selected)

combined_data <- core_selected %>%
  full_join(common_selected, by = "individual") %>%
  left_join(ados_combined, by = "individual") %>%
  filter(css > 3)  # Assuming 'css' is the variable from ADOS datasets

rawcombined <- combined_data %>%
  left_join(
    ados_raw %>% 
      filter(individual %in% combined_data$individual),
    by = "individual"
  )

ados_raw <- rawcombined %>%
  # First remove the explicit duplicates we know about
  select(-origin.x, -origin.y, -measure) %>%
  # Remove duplicate measure columns if they exist
  select(-any_of(c("measure.x", "measure.y"))) %>%
  # Remove any other duplicate algorithm columns if they exist
  select(-any_of(c("algorithm.x", "algorithm.y"))) |> 
  relocate(algorithm, ados_module, age_at_ados, css, .after = sex)


# Pivot wider to consolidate columns and retain one origin per individual
final_data <- combined_data %>%
  pivot_wider(
    names_from = origin,
    values_from = c(communication_social, css, restricted_repetitive, social_affect),
    names_sep = " "
  ) %>%
  # Coalesce to keep only non-NA values
  mutate(
    communication_social = coalesce(`communication_social ADOS 1`, `communication_social ADOS 2`, `communication_social ADOS 3`),
    css = coalesce(`css ADOS 1`, `css ADOS 2`, `css ADOS 3`),
    restricted_repetitive = coalesce(`restricted_repetitive ADOS 1`, `restricted_repetitive ADOS 2`, `restricted_repetitive ADOS 3`),
    social_affect = coalesce(`social_affect ADOS 1`, `social_affect ADOS 2`, `social_affect ADOS 3`)
  ) %>%
  select(
    individual,
    ados_css,
    ados_module,
    ados_restricted_repetitive, #raw
    ados_social_affect, #raw
    age_at_ados,
    ados_communication,#suma old
    ados_reciprocal_social, #sumb_old
    algorithm
  )

final_data$ados_css <- as.numeric(final_data$ados_css)
final_data$ados_social_affect <- as.numeric(final_data$ados_social_affect)

finaldata <- final_data |>
  full_join(srs_parent_df, by = "individual") |> 
  filter(age_at_ados > 60) |> 
  select( -calculation_method, -awareness_t_score, -awareness_t_score_detail, -communication_t_score, -communication_t_score_detail, -mannerisms_t_score, -mannerisms_t_score_detail, -motivation_t_score, -motivation_t_score_detail, -cognition_t_score, -cognition_t_score_detail, -status, -ados_communication, -ados_reciprocal_social)

# Mutate SA CSS -----------------------------------------------------------

mutate_SA_CSS <- function(data) {
  data %>%
    mutate(`SA CSS` = case_when(
      # No words
      # 0-24 months
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 0:24 & ados_social_affect %in% 0:3 ~ 1,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 0:24 & ados_social_affect %in% 4:5 ~ 2,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 0:24 & ados_social_affect %in% 6:8 ~ 3,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 0:24 & ados_social_affect == 9 ~ 4,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 0:24 & ados_social_affect %in% 10:13 ~ 5,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 0:24 & ados_social_affect %in% 14:16 ~ 6,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 0:24 & ados_social_affect == 17 ~ 7,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 0:24 & ados_social_affect == 18 ~ 8,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 0:24 & ados_social_affect == 19 ~ 9,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 0:24 & ados_social_affect == 20 ~ 10,
      
      # 25-36 months
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 25:36 & ados_social_affect %in% 0:3 ~ 1,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 25:36 & ados_social_affect %in% 4:5 ~ 2,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 25:36 & ados_social_affect %in% 6:9 ~ 3,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 25:36 & ados_social_affect == 10 ~ 4,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 25:36 & ados_social_affect %in% 11:12 ~ 5,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 25:36 & ados_social_affect %in% 13:16 ~ 6,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 25:36 & ados_social_affect == 17 ~ 7,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 25:36 & ados_social_affect == 18 ~ 8,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 25:36 & ados_social_affect == 19 ~ 9,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 25:36 & ados_social_affect == 20 ~ 10,
      
      # 48-168 months
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 37:168 & ados_social_affect %in% 0:2 ~ 1,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 37:168 & ados_social_affect %in% 3:5 ~ 2,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 37:168 & ados_social_affect %in% 6:9 ~ 3,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 37:168 & ados_social_affect == 10 ~ 4,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 37:168 & ados_social_affect %in% 11:12 ~ 5,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 37:168 & ados_social_affect %in% 13:14 ~ 6,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 37:168 & ados_social_affect %in% 15:16 ~ 7,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 37:168 & ados_social_affect %in% 17:18 ~ 8,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 37:168 & ados_social_affect == 19 ~ 9,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 37:168 & ados_social_affect == 20 ~ 10,
      
      # Some words
      # 0-24 months
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 24:36 & ados_social_affect %in% 0:1 ~ 1,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 24:36 & ados_social_affect %in% 2:4 ~ 2,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 24:36 & ados_social_affect == 5 ~ 3,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 24:36 & ados_social_affect %in% 6:7 ~ 4,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 24:36 & ados_social_affect == 8 ~ 5,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 24:36 & ados_social_affect %in% 9:11 ~ 6,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 24:36 & ados_social_affect %in% 12:13 ~ 7,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 24:36 & ados_social_affect %in% 14:15 ~ 8,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 24:36 & ados_social_affect %in% 16:17 ~ 9,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 24:36 & ados_social_affect %in% 18:20 ~ 10,
      
      # 37-48 months
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 37:48 & ados_social_affect %in% 0:1 ~ 1,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 37:48 & ados_social_affect %in% 2:4 ~ 2,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 37:48 & ados_social_affect == 5 ~ 3,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 37:48 & ados_social_affect %in% 6:7 ~ 4,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 37:48 & ados_social_affect == 8 ~ 5,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 37:48 & ados_social_affect %in% 9:11 ~ 6,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 37:48 & ados_social_affect %in% 12:13 ~ 7,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 37:48 & ados_social_affect %in% 14:15 ~ 8,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 37:48 & ados_social_affect %in% 16:17 ~ 9,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 37:48 & ados_social_affect %in% 18:20 ~ 10,
      
      # 49-168 months
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 49:168 & ados_social_affect %in% 0:1 ~ 1,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 49:168 & ados_social_affect %in% 2:3 ~ 2,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 49:168 & ados_social_affect %in% 4:5 ~ 3,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 49:168 & ados_social_affect %in% 6:7 ~ 4,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 49:168 & ados_social_affect %in% 8:9 ~ 5,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 49:168 & ados_social_affect %in% 10:13 ~ 6,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 49:168 & ados_social_affect %in% 14:15 ~ 7,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 49:168 & ados_social_affect == 16 ~ 8,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 49:168 & ados_social_affect %in% 17:18 ~ 9,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 49:168 & ados_social_affect %in% 19:20 ~ 10,
      
      #Module 2
      #First age group
      ados_module == 2 & age_at_ados %in% 24:36 & ados_social_affect %in% 0:1 ~ 1,
      ados_module == 2 & age_at_ados %in% 24:36 & ados_social_affect %in% 2:3 ~ 2,
      ados_module == 2 & age_at_ados %in% 24:36 & ados_social_affect == 4 ~ 3,
      ados_module == 2 & age_at_ados %in% 24:36 & ados_social_affect == 5 ~ 4,
      ados_module == 2 & age_at_ados %in% 24:36 & ados_social_affect == 6 ~ 5,
      ados_module == 2 & age_at_ados %in% 24:36 & ados_social_affect %in% 7:8 ~ 6,
      ados_module == 2 & age_at_ados %in% 24:36 & ados_social_affect %in% 9:10 ~ 7,
      ados_module == 2 & age_at_ados %in% 24:36 & ados_social_affect == 11 ~ 8,
      ados_module == 2 & age_at_ados %in% 24:36 & ados_social_affect %in% 17:18 ~ 9,
      ados_module == 2 & age_at_ados %in% 24:36 & ados_social_affect %in% 19:20 ~ 10,
      
      #Second age group
      ados_module == 2 & age_at_ados %in% 37:48 & ados_social_affect %in% 0:1 ~ 1,
      ados_module == 2 & age_at_ados %in% 37:48 & ados_social_affect == 2 ~ 2,
      ados_module == 2 & age_at_ados %in% 37:48 & ados_social_affect %in% 3:4 ~ 3,
      ados_module == 2 & age_at_ados %in% 37:48 & ados_social_affect %in% 5:6 ~ 4,
      ados_module == 2 & age_at_ados %in% 37:48 & ados_social_affect == 7 ~ 5,
      ados_module == 2 & age_at_ados %in% 37:48 & ados_social_affect %in% 8:9 ~ 6,
      ados_module == 2 & age_at_ados %in% 37:48 & ados_social_affect %in% 10:11 ~ 7,
      ados_module == 2 & age_at_ados %in% 37:48 & ados_social_affect %in% 12:13 ~ 8,
      ados_module == 2 & age_at_ados %in% 37:48 & ados_social_affect %in% 14:15 ~ 9,
      ados_module == 2 & age_at_ados %in% 37:48 & ados_social_affect %in% 16:20 ~ 10,
      
      #Third age group
      ados_module == 2 & age_at_ados %in% 49:72 & ados_social_affect %in% 0:1 ~ 1,
      ados_module == 2 & age_at_ados %in% 49:72 & ados_social_affect %in% 2:3 ~ 2,
      ados_module == 2 & age_at_ados %in% 49:72 & ados_social_affect %in% 4:5 ~ 3,
      ados_module == 2 & age_at_ados %in% 49:72 & ados_social_affect == 6 ~ 4,
      ados_module == 2 & age_at_ados %in% 49:72 & ados_social_affect == 7 ~ 5,
      ados_module == 2 & age_at_ados %in% 49:72 & ados_social_affect %in% 8:9 ~ 6,
      ados_module == 2 & age_at_ados %in% 49:72 & ados_social_affect %in% 10:11 ~ 7,
      ados_module == 2 & age_at_ados %in% 49:72 & ados_social_affect %in% 12:15 ~ 8,
      ados_module == 2 & age_at_ados %in% 49:72 & ados_social_affect == 16 ~ 9,
      ados_module == 2 & age_at_ados %in% 49:72 & ados_social_affect %in% 17:20 ~ 10,
      
      #Fourth age group
      ados_module == 2 & age_at_ados %in% 73:192 & ados_social_affect %in% 0:1 ~ 1,
      ados_module == 2 & age_at_ados %in% 73:192 & ados_social_affect == 2 ~ 2,
      ados_module == 2 & age_at_ados %in% 73:192 & ados_social_affect %in% 3:4 ~ 3,
      ados_module == 2 & age_at_ados %in% 73:192 & ados_social_affect == 5 ~ 4,
      ados_module == 2 & age_at_ados %in% 73:192 & ados_social_affect %in% 6:7 ~ 5,
      ados_module == 2 & age_at_ados %in% 73:192 & ados_social_affect %in% 8:10 ~ 6,
      ados_module == 2 & age_at_ados %in% 73:192 & ados_social_affect %in% 11:13 ~ 7,
      ados_module == 2 & age_at_ados %in% 73:192 & ados_social_affect %in% 14:15 ~ 8,
      ados_module == 2 & age_at_ados %in% 73:192 & ados_social_affect %in% 16:17 ~ 9,
      ados_module == 2 & age_at_ados %in% 73:192 & ados_social_affect %in% 17:20 ~ 10,
      
      #Module 3
      #First age group
      ados_module == 3 & age_at_ados %in% 36:60 & ados_social_affect %in% 0:2 ~ 1,
      ados_module == 3 & age_at_ados %in% 36:60 & ados_social_affect == 3 ~ 2,
      ados_module == 3 & age_at_ados %in% 36:60 & ados_social_affect == 4 ~ 3,
      ados_module == 3 & age_at_ados %in% 36:60 & ados_social_affect == 5 ~ 4,
      ados_module == 3 & age_at_ados %in% 36:60 & ados_social_affect == 6 ~ 5,
      ados_module == 3 & age_at_ados %in% 36:60 & ados_social_affect %in% 7:8 ~ 6,
      ados_module == 3 & age_at_ados %in% 36:60 & ados_social_affect %in% 9:10 ~ 7,
      ados_module == 3 & age_at_ados %in% 36:60 & ados_social_affect %in% 11:12 ~ 8,
      ados_module == 3 & age_at_ados %in% 36:60 & ados_social_affect %in% 13:14 ~ 9,
      ados_module == 3 & age_at_ados %in% 36:60 & ados_social_affect %in% 15:20 ~ 10,
      
      #Second age group
      #First age group
      ados_module == 3 & age_at_ados %in% 61:192 & ados_social_affect %in% 0:1 ~ 1,
      ados_module == 3 & age_at_ados %in% 61:192 & ados_social_affect == 2 ~ 2,
      ados_module == 3 & age_at_ados %in% 61:192 & ados_social_affect %in% 3:4 ~ 3,
      ados_module == 3 & age_at_ados %in% 61:192 & ados_social_affect == 5 ~ 4,
      ados_module == 3 & age_at_ados %in% 61:192 & ados_social_affect == 6 ~ 5,
      ados_module == 3 & age_at_ados %in% 61:192 & ados_social_affect == 7 ~ 6,
      ados_module == 3 & age_at_ados %in% 61:192 & ados_social_affect %in% 8:9 ~ 7,
      ados_module == 3 & age_at_ados %in% 61:192 & ados_social_affect %in% 10:11 ~ 8,
      ados_module == 3 & age_at_ados %in% 61:192 & ados_social_affect %in% 12:14 ~ 9,
      ados_module == 3 & age_at_ados %in% 61:192 & ados_social_affect %in% 15:20 ~ 10,
      # Default case
      TRUE ~ NA_real_
    ))
}

# result <- mutate_SA_CSS(your_data)
result <- mutate_SA_CSS(finaldata)



# mutate RRB CSS --------------------------------------------------------------


mutate_RRB_CSS <- function(data) {
  data %>%
    mutate(`RRB CSS` = case_when(
      # No words
      # 0-24 months
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 0:24 & ados_restricted_repetitive == 0 ~ 1,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 0:24 & ados_restricted_repetitive == 1 ~ 5,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 0:24 & ados_restricted_repetitive == 2 ~ 6,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 0:24 & ados_restricted_repetitive == 3 ~ 7,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 0:24 & ados_restricted_repetitive == 4 ~ 8,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 0:24 & ados_restricted_repetitive == 5 ~ 9,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 0:24 & ados_restricted_repetitive %in% 6:8 ~ 10,
      
      # 25-36 months
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 25:36 & ados_restricted_repetitive == 0 ~ 1,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 25:36 & ados_restricted_repetitive == 1 ~ 5,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 25:36 & ados_restricted_repetitive %in% 2:3 ~ 6,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 25:36 & ados_restricted_repetitive == 4 ~ 7,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 25:36 & ados_restricted_repetitive == 5 ~ 8,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 25:36 & ados_restricted_repetitive == 6 ~ 9,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 25:36 & ados_restricted_repetitive %in% 7:8 ~ 10,
      
      # 48-168 months
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 37:168 & ados_restricted_repetitive == 0 ~ 1,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 37:168 & ados_restricted_repetitive %in% 1:2 ~ 5,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 37:168 & ados_restricted_repetitive == 3 ~ 6,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 37:168 & ados_restricted_repetitive == 4 ~ 7,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 37:168 & ados_restricted_repetitive %in% 5:6 ~ 8,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 37:168 & ados_restricted_repetitive == 7 ~ 9,
      ados_module == 1 & algorithm == "no-words" & age_at_ados %in% 37:168 & ados_restricted_repetitive == 8 ~ 10,
      
      # Some words
      # 0-24 months
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 24:36 & ados_restricted_repetitive == 0 ~ 1,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 24:36 & ados_restricted_repetitive == 1 ~ 5,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 24:36 & ados_restricted_repetitive == 2 ~ 6,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 24:36 & ados_restricted_repetitive == 3 ~ 7,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 24:36 & ados_restricted_repetitive == 4 ~ 8,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 24:36 & ados_restricted_repetitive == 5 ~ 9,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 24:36 & ados_restricted_repetitive %in% 6:8 ~ 10,
      
      # 37-48 months
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 37:48 & ados_restricted_repetitive == 0 ~ 1,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 37:48 & ados_restricted_repetitive == 1 ~ 5,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 37:48 & ados_restricted_repetitive == 2 ~ 6,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 37:48 & ados_restricted_repetitive %in% 3:4 ~ 7,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 37:48 & ados_restricted_repetitive == 5 ~ 8,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 37:48 & ados_restricted_repetitive == 6 ~ 9,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 37:48 & ados_restricted_repetitive %in% 7:8 ~ 10,
      
      # 49-168 months
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 49:168 & ados_restricted_repetitive == 0 ~ 1,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 49:168 & ados_restricted_repetitive == 1 ~ 5,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 49:168 & ados_restricted_repetitive %in% 2:3 ~ 6,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 49:168 & ados_restricted_repetitive == 4 ~ 7,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 49:168 & ados_restricted_repetitive == 5 ~ 8,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 49:168 & ados_restricted_repetitive == 6 ~ 9,
      ados_module == 1 & algorithm == "some-words" & age_at_ados %in% 49:168 & ados_restricted_repetitive %in% 7:8 ~ 10,
      
      #Module 2
      #First age group
      ados_module == 2 & age_at_ados %in% 24:36 & ados_restricted_repetitive == 0 ~ 1,
      ados_module == 2 & age_at_ados %in% 24:36 & ados_restricted_repetitive == 1 ~ 5,
      ados_module == 2 & age_at_ados %in% 24:36 & ados_restricted_repetitive == 2 ~ 6,
      ados_module == 2 & age_at_ados %in% 24:36 & ados_restricted_repetitive == 3 ~ 7,
      ados_module == 2 & age_at_ados %in% 24:36 & ados_restricted_repetitive == 4 ~ 8,
      ados_module == 2 & age_at_ados %in% 24:36 & ados_restricted_repetitive %in% 5:6 ~ 9,
      ados_module == 2 & age_at_ados %in% 24:36 & ados_restricted_repetitive %in% 7:8 ~ 10,
      
      #Second age group
      ados_module == 2 & age_at_ados %in% 37:48 & ados_restricted_repetitive == 0 ~ 1,
      ados_module == 2 & age_at_ados %in% 37:48 & ados_restricted_repetitive == 1 ~ 5,
      ados_module == 2 & age_at_ados %in% 37:48 & ados_restricted_repetitive %in% 2:3 ~ 6,
      ados_module == 2 & age_at_ados %in% 37:48 & ados_restricted_repetitive == 4 ~ 7,
      ados_module == 2 & age_at_ados %in% 37:48 & ados_restricted_repetitive == 5 ~ 8,
      ados_module == 2 & age_at_ados %in% 37:48 & ados_restricted_repetitive == 6 ~ 9,
      ados_module == 2 & age_at_ados %in% 37:48 & ados_restricted_repetitive %in% 7:8 ~ 10,
      
      #Third age group
      ados_module == 2 & age_at_ados %in% 49:72 & ados_restricted_repetitive == 0 ~ 1,
      ados_module == 2 & age_at_ados %in% 49:72 & ados_restricted_repetitive == 1 ~ 5,
      ados_module == 2 & age_at_ados %in% 49:72 & ados_restricted_repetitive %in% 2:3 ~ 6,
      ados_module == 2 & age_at_ados %in% 49:72 & ados_restricted_repetitive == 4 ~ 7,
      ados_module == 2 & age_at_ados %in% 49:72 & ados_restricted_repetitive == 5 ~ 8,
      ados_module == 2 & age_at_ados %in% 49:72 & ados_restricted_repetitive == 6 ~ 9,
      ados_module == 2 & age_at_ados %in% 49:72 & ados_restricted_repetitive %in% 7:8 ~ 10,
      
      #Fourth age group
      ados_module == 2 & age_at_ados %in% 73:192 & ados_restricted_repetitive == 0 ~ 1,
      ados_module == 2 & age_at_ados %in% 73:192 & ados_restricted_repetitive == 1 ~ 5,
      ados_module == 2 & age_at_ados %in% 73:192 & ados_restricted_repetitive %in% 2:3 ~ 6,
      ados_module == 2 & age_at_ados %in% 73:192 & ados_restricted_repetitive == 4 ~ 7,
      ados_module == 2 & age_at_ados %in% 73:192 & ados_restricted_repetitive == 5 ~ 8,
      ados_module == 2 & age_at_ados %in% 73:192 & ados_restricted_repetitive == 6 ~ 9,
      ados_module == 2 & age_at_ados %in% 73:192 & ados_restricted_repetitive %in% 7:8 ~ 10,
      
      #Module 3
      #First age group
      ados_module == 3 & age_at_ados %in% 36:60 & ados_restricted_repetitive == 0 ~ 1,
      ados_module == 3 & age_at_ados %in% 36:60 & ados_restricted_repetitive == 1 ~ 5,
      ados_module == 3 & age_at_ados %in% 36:60 & ados_restricted_repetitive == 2 ~ 7,
      ados_module == 3 & age_at_ados %in% 36:60 & ados_restricted_repetitive == 3 ~ 8,
      ados_module == 3 & age_at_ados %in% 36:60 & ados_restricted_repetitive == 4 ~ 9,
      ados_module == 3 & age_at_ados %in% 36:60 & ados_restricted_repetitive %in% 5:8 ~ 10,
      
      #Second age group
      #First age group
      ados_module == 3 & age_at_ados %in% 61:192 & ados_restricted_repetitive == 0 ~ 1,
      ados_module == 3 & age_at_ados %in% 61:192 & ados_restricted_repetitive == 1 ~ 5,
      ados_module == 3 & age_at_ados %in% 61:192 & ados_restricted_repetitive == 2 ~ 7,
      ados_module == 3 & age_at_ados %in% 61:192 & ados_restricted_repetitive == 3 ~ 8,
      ados_module == 3 & age_at_ados %in% 61:192 & ados_restricted_repetitive %in% 4:5 ~ 9,
      ados_module == 3 & age_at_ados %in% 61:192 & ados_restricted_repetitive %in% 6:8 ~ 10,
      
      # Default case
      TRUE ~ NA_real_
    ))
}
result <- mutate_RRB_CSS(result)

#communication_social is old calculation which will not be used. | NOT USED -> done
#we will cut age at 59. -> done


# describe(result)
# plot_all_distributions(result)


# Mutate sumA and SumB algorithm

# Function to calculate ADOS algorithmic sums
calculate_ados_algorithm_sums <- function(data) {
  data %>%
    group_by(individual) %>%
    mutate(
      JA = case_when(
        ados_module == 1 & algorithm == "no-words" ~ rowSums(data.frame(
          a7_pointing, a8_gestures, 
          b01_eye_contact, b09_showing, 
          b10_spontaneous_joint, b11_response_joint), na.rm = TRUE),
        
        ados_module == 1 & algorithm == "some-words" ~ rowSums(data.frame(
          a7_pointing, a8_gestures, 
          b01_eye_contact, b09_showing, 
          b10_spontaneous_joint), na.rm = TRUE),
        
        ados_module == 2 ~ rowSums(data.frame(
          a6_conversation_2, a7_pointing_2,
          b01_eye_contact_2, b05_showing_2, 
          b06_spontaneous_joint_2, b07_response_joint_2), na.rm = TRUE)
      ),
      
      sumAalgorithm = case_when(
        ados_module == 1 ~ rowSums(data.frame(
          a2_frequency_vocalization, a7_pointing, 
          a8_gestures), na.rm = TRUE),
        
        ados_module == 2 ~ rowSums(data.frame(
          a6_conversation_2, a7_pointing_2), na.rm = TRUE),
        
        ados_module == 3 ~ rowSums(data.frame(
          a7_reporting_events_3, a8_conversation_3, 
          a9_gestures_3), na.rm = TRUE)
      ),
      
      sumBalgorithm = case_when(
        ados_module == 1 ~ rowSums(data.frame(
          b01_eye_contact, b03_facial_others,
          b04_integration_gaze, b05_shared_enjoyment,
          b09_showing, b10_spontaneous_joint,
          b11_response_joint, b12_social_overture), na.rm = TRUE),
        
        ados_module == 2 ~ rowSums(data.frame(
          b01_eye_contact_2, b02_facial_others_2,
          b03_shared_enjoyment_2, b05_showing_2,
          b06_spontaneous_joint_2, b08_quality_overture,
          b11_quality_rapport), na.rm = TRUE),
        
        ados_module == 3 ~ rowSums(data.frame(
          b01_eye_contact_3, b02_facial_others_3,
          b04_shared_enjoyment_3, b07_quality_overture,
          b09_reciprocal_social_3, b10_quality_rapport), na.rm = TRUE)
      )
    )
}


ados_raw <- calculate_ados_algorithm_sums(ados_raw)

ados_raw_selected <- ados_raw %>%
  select(individual, sex, JA, sumAalgorithm, sumBalgorithm) %>%
  distinct() # In case there are duplicates for an individual, take unique rows

result <- result %>%
  left_join(ados_raw_selected, by = "individual")

result %>%
  summarise(
    n_total = n(),
    n_missing_JA = sum(is.na(JA)),
    n_missing_sumA = sum(is.na(sumAalgorithm)),
    n_missing_sumB = sum(is.na(sumBalgorithm)),
    n_missing_sex = sum(is.na(sex))
  )
# .- - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - -  ------------------------------------------

# ADI_SSC----------------------------- -----------------------------------------------------------------
adi_algorithm <- function(df, blacklist_cols = NULL) {
  # Identify columns to check by excluding the blacklist columns
  cols_to_check <- setdiff(names(df), blacklist_cols)
  
  df %>%
    mutate(across(all_of(cols_to_check), ~ {
      if (is.numeric(.)) {
        case_when(
          . %in% c(7, 8, 9) | . < 0 ~ NA_real_,  # Replace 7, 8, 9, or any negative number with NA
          . %in% c(3, 4) ~ 2,                    # Replace 3 or 4 with 2
          . > 60 ~ NA_real_,                     # Replace anything higher than 60 with NA
          TRUE ~ as.numeric(.)                   # Preserve other numeric values
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

blacklist_cols <- c("individual","measure")

adi_df <- adi_algorithm(adi_df, blacklist_cols)

# describe(adi_df)


adi_df <- adi_df %>% 
  mutate(
    # Calculate Social communication_lord (15 items)
    soc_com_lord = rowSums(select(adi_df, 
                                  q31_other_body_current,                # ADI_31_usebody
                                  q42_pointing_express_current,          # ADI_42_point
                                  q43_nodding_current,                   # ADI_43_nodding
                                  q44_head_shaking_current,              # ADI_44_headshake
                                  q45_instrumental_gestures_current,     # ADI_45_conventgest
                                  q51_social_smiling_current,            # ADI_51_socsmile
                                  q52_directing_attention_current,       # ADI_52_showing
                                  q53_offering_share_current,            # ADI_53_sharing
                                  q54_share_enjoyment_current,           # ADI_54_sharedenjoy
                                  q55_offering_comfort_current,          # ADI_55_offercomfort
                                  q56_quality_overtures_current,         # ADI_56_qual_social
                                  q57_facial_expressions_current,        # ADI_57_rangefacialexp
                                  q58_inappropriate_facial_current,      # ADI_58_inappfacialexp
                                  q59_appropriateness_social_current,    # ADI_59_socialresp
                                  q62_interest_children_current),        # ADI_62_intchild
                           na.rm = TRUE),
    
    # Calculate rrb_lord (sum of repetitive sensory motor and insistence on sameness)
    rrb_lord = rowSums(select(adi_df, 
                              q67_unusual_preoccupations_current,        # ADI_67_preoccupy
                              q69_repetitive_use_current,                # ADI_69_repobj
                              q71_unusual_sensory_current,               # ADI_71_unusualsense
                              q77_hand_mannerisms_current,               # ADI_77_handfingerman
                              q78_complex_mannerisms_current,            # ADI_78_othermanner
                              q70_compulsions_current,                   # ADI_70_compulsions
                              q72_sensitivity_noise_current,             # ADI_72_noisesense
                              q73_abnorm_stimuli_current,                # ADI_73_negrespstimuli
                              q74_difficulties_changes_current),         # ADI_74_difficultchange
                       na.rm = TRUE),
    total_lord  = soc_com_lord + rrb_lord
  )
  


# Add DSM-5 and DSM-5 Adj algorithms to the adi_df dataframe for school-age sample
adi_df <- adi_df %>%
  mutate(
    # First determine verbal status
    is.verbal = case_when(
      q30_overall_language == 0 ~ 1,  # Verbal
      q30_overall_language %in% c(1, 2) ~ 0,  # Nonverbal
      TRUE ~ NA_real_
    ),
    
    # DSM-5 Social Communication Domain (SC) - using abnormal versions
    sc_dsm5 = case_when(
      is.verbal == 1 ~ rowSums(select(., 
                                      # Ever scores for items 34-35
                                      q34_social_verbalization_ever, q35_reciprocal_conversation_ever,
                                      # Abnormal scores for items 42-64
                                      q42_pointing_express_abnormal, q43_nodding_abnormal, 
                                      q44_head_shaking_abnormal, q45_instrumental_gestures_abnormal,
                                      q46_attention_voice_abnormal, q47_spontaneous_imitation_abnormal,
                                      q49_play_peers_abnormal, q50_direct_gaze_abnormal,
                                      q51_social_smiling_abnormal, q52_directing_attention_abnormal,
                                      q53_offering_share_abnormal, q54_share_enjoyment_abnormal,
                                      q55_offering_comfort_abnormal, q56_quality_overtures_abnormal,
                                      q57_facial_expressions_abnormal, q59_appropriateness_social_abnormal,
                                      q61_imitative_play_abnormal, q62_interest_children_abnormal,
                                      q63_response_children_abnormal, q64_group_play_abnormal
      ), na.rm = TRUE),
      
      is.verbal == 0 ~ rowSums(select(., 
                                      # Abnormal scores for items 42-64 (excluding verbal-only items)
                                      q42_pointing_express_abnormal, q43_nodding_abnormal, 
                                      q44_head_shaking_abnormal, q45_instrumental_gestures_abnormal,
                                      q46_attention_voice_abnormal, q47_spontaneous_imitation_abnormal,
                                      q49_play_peers_abnormal, q50_direct_gaze_abnormal,
                                      q51_social_smiling_abnormal, q52_directing_attention_abnormal,
                                      q53_offering_share_abnormal, q54_share_enjoyment_abnormal,
                                      q55_offering_comfort_abnormal, q56_quality_overtures_abnormal,
                                      q57_facial_expressions_abnormal, q59_appropriateness_social_abnormal,
                                      q61_imitative_play_abnormal, q62_interest_children_abnormal,
                                      q63_response_children_abnormal, q64_group_play_abnormal
      ), na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    
    # DSM-5 Restricted and Repetitive Behaviors Domain (RRB) - using ever scores
    rrb_dsm5 = case_when(
      is.verbal == 1 ~ rowSums(select(., 
                                      # Ever scores for all RRB items
                                      q33_delayed_echolalia_ever, q39_verbal_rituals_ever,
                                      q67_unusual_preoccupations_ever, q68_circumscribed_interests_ever,
                                      q69_repetitive_use_ever, q70_compulsions_ever,
                                      q71_unusual_sensory_ever, q72_sensitivity_noise_ever,
                                      q73_abnorm_stimuli_ever, q74_difficulties_changes_ever,
                                      q77_hand_mannerisms_ever, q78_complex_mannerisms_ever
      ), na.rm = TRUE),
      
      is.verbal == 0 ~ rowSums(select(., 
                                      # Ever scores for RRB items (excluding verbal-only items)
                                      q67_unusual_preoccupations_ever, q68_circumscribed_interests_ever,
                                      q69_repetitive_use_ever, q70_compulsions_ever,
                                      q71_unusual_sensory_ever, q72_sensitivity_noise_ever,
                                      q73_abnorm_stimuli_ever, q74_difficulties_changes_ever,
                                      q77_hand_mannerisms_ever, q78_complex_mannerisms_ever
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
    ),
    
    # DSM-5 Adjusted Social Communication Domain (SC Adj) - using current instead of abnormal
    sc_dsm5_adj = case_when(
      is.verbal == 1 ~ rowSums(select(., 
                                      # Ever scores for items 34-35
                                      q34_social_verbalization_ever, q35_reciprocal_conversation_ever,
                                      # Current scores for items 42-64
                                      q42_pointing_express_current, q43_nodding_current, 
                                      q44_head_shaking_current, q45_instrumental_gestures_current,
                                      q46_attention_voice_current, q47_spontaneous_imitation_current,
                                      q49_play_peers_current, q50_direct_gaze_current,
                                      q51_social_smiling_current, q52_directing_attention_current,
                                      q53_offering_share_current, q54_share_enjoyment_current,
                                      q55_offering_comfort_current, q56_quality_overtures_current,
                                      q57_facial_expressions_current, q59_appropriateness_social_current,
                                      q61_imitative_play_current, q62_interest_children_current,
                                      q63_response_children_current, q64_group_play_current
      ), na.rm = TRUE),
      
      is.verbal == 0 ~ rowSums(select(., 
                                      # Current scores for items 42-64 (excluding verbal-only items)
                                      q42_pointing_express_current, q43_nodding_current, 
                                      q44_head_shaking_current, q45_instrumental_gestures_current,
                                      q46_attention_voice_current, q47_spontaneous_imitation_current,
                                      q49_play_peers_current, q50_direct_gaze_current,
                                      q51_social_smiling_current, q52_directing_attention_current,
                                      q53_offering_share_current, q54_share_enjoyment_current,
                                      q55_offering_comfort_current, q56_quality_overtures_current,
                                      q57_facial_expressions_current, q59_appropriateness_social_current,
                                      q61_imitative_play_current, q62_interest_children_current,
                                      q63_response_children_current, q64_group_play_current
      ), na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    
    # RRB domain remains the same for DSM-5 Adj - it already uses "ever" scores
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
  relocate(is.verbal, sc_dsm5, rrb_dsm5, total_dsm5, dsm5_classification, 
           sc_dsm5_adj, rrb_dsm5_adj, total_dsm5_adj, dsm5_classification_adj, 
           .after = total_lord)



# Update adi_df_interest to include the new DSM-5 and DSM-5 Adj scores
adi_df_interest <- adi_df |> 
  select(individual, soc_com_lord, rrb_lord, total_lord,
         sc_dsm5, rrb_dsm5, total_dsm5, dsm5_classification,
         sc_dsm5_adj, rrb_dsm5_adj, total_dsm5_adj, dsm5_classification_adj)



# describe(adi_df_interest)
# plot_all_distributions(adi_df_interest)


# adi_totals_abc ----------------------------------------------------------
# adi_df_scores <- read.csv("C:\\Users\\User\\OneDrive - post.bgu.ac.il\\Documents\\Research Projects\\Evaluating the Relationship between Measures of Autism Symptoms\\Databases\\SSC\\symptom meausres/adi_r_diagnostic.csv")
adi_df_scores <- read.csv("C:/Users/galor/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/SSC/symptom meausres/adi_r_diagnostic.csv")

columns_to_merge <- c("individual", 
                      "a1_failure_nonverbal_social", 
                      "a2_failure_peer_relationships",
                      "a3_lack_shared_enjoy",
                      "a4_lack_socioemotional",
                      "a_total",
                      "b1_delay_lang_gest",
                      "b2_fail_initiate_conversation",
                      "b3_idiosyncratic_speech",
                      "b4_lack_make_believe",
                      "b_verbal_total",  
                      "b_non_verbal_total",
                      "c1_encompassing_pattern",
                      "c2_compulsive_rituals",
                      "c3_repetitive_mannerisms",
                      "c4_preoccupation_material",
                      "c_total",
                      "status")

adi_scores_subset <- adi_df_scores[, columns_to_merge]

adi_df_interest <- merge(adi_df_interest, 
                       adi_scores_subset,
                       by = "individual",
                       all.x = TRUE)  # keep all rows from adi_df_interest

#need to remove missing values.

names(adi_df_interest)
# .- - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - -  ------------------------------------------
# srs_parent--------------------------- ------------------------------------------------------------
srs_parent_raw$total_raw_score <- rowSums(srs_parent_raw[, 4:68], na.rm = TRUE)

srs_parent_raw <- srs_parent_raw %>%
  left_join(
    core_selected %>% select(individual, sex),
    by = "individual"
  ) %>%
  filter(!is.na(sex))

srs_parent_raw <- srs_parent_raw %>%
  left_join(
    srs_parent_df %>% select(individual, awareness, cognition, communication, mannerisms, motivation),
    by = "individual"
  )

srs_parent_raw$sci_raw <- rowSums(srs_parent_raw[,c("awareness","cognition","motivation","communication")])


# srs_parent total t ------------------------------------------------------

srs_convert_to_tscore <- function(raw_score, gender) {
  # Input validation
  if (!is.numeric(raw_score)) {
    stop("Raw score must be numeric")
  }
  
  if (!gender %in% c("male", "female")) {
    stop("Gender must be either 'male' or 'female'")
  }
  
  if (gender == "female") {
    # Female conversion logic
    female_tscore <- case_when(
      # 90 and just below
      raw_score >= 123 ~ 90,
      raw_score >= 121 & raw_score <= 122 ~ 89,
      raw_score >= 118 & raw_score <= 120 ~ 88,
      raw_score >= 116 & raw_score <= 117 ~ 87,
      raw_score >= 114 & raw_score <= 115 ~ 86,
      
      # 85 and just below
      raw_score >= 111 & raw_score <= 113 ~ 85,
      raw_score >= 109 & raw_score <= 110 ~ 84,
      raw_score >= 107 & raw_score <= 108 ~ 83,
      raw_score >= 104 & raw_score <= 106 ~ 82,
      raw_score >= 102 & raw_score <= 103 ~ 81,
      
      # 80 and just below
      raw_score >= 99 & raw_score <= 101 ~ 80,
      raw_score >= 97 & raw_score <= 98 ~ 79,
      raw_score >= 95 & raw_score <= 96 ~ 78,
      raw_score >= 92 & raw_score <= 94 ~ 77,
      raw_score >= 90 & raw_score <= 91 ~ 76,
      
      # 75 and just below
      raw_score >= 88 & raw_score <= 89 ~ 75,
      raw_score >= 85 & raw_score <= 87 ~ 74,
      raw_score >= 83 & raw_score <= 84 ~ 73,
      raw_score >= 80 & raw_score <= 82 ~ 72,
      raw_score >= 78 & raw_score <= 79 ~ 71,
      
      # 70 and just below
      raw_score >= 76 & raw_score <= 77 ~ 70,
      raw_score >= 73 & raw_score <= 75 ~ 69,
      raw_score >= 71 & raw_score <= 72 ~ 68,
      raw_score >= 69 & raw_score <= 70 ~ 67,
      raw_score >= 66 & raw_score <= 68 ~ 66,
      
      # 65 and just below
      raw_score >= 64 & raw_score <= 65 ~ 65,
      raw_score >= 61 & raw_score <= 63 ~ 64,
      raw_score >= 59 & raw_score <= 60 ~ 63,
      raw_score >= 57 & raw_score <= 58 ~ 62,
      raw_score >= 54 & raw_score <= 56 ~ 61,
      
      # 60 and just below
      raw_score >= 52 & raw_score <= 53 ~ 60,
      raw_score >= 50 & raw_score <= 51 ~ 59,
      raw_score >= 47 & raw_score <= 49 ~ 58,
      raw_score >= 45 & raw_score <= 46 ~ 57,
      raw_score >= 43 & raw_score <= 44 ~ 56,
      
      # 55 and just below
      raw_score >= 40 & raw_score <= 42 ~ 55,
      raw_score >= 38 & raw_score <= 39 ~ 54, ##algorithm mismatch for 36
      raw_score >= 34 & raw_score <= 35 ~ 53, ##algorithm mismatch for 33
      raw_score >= 31 & raw_score <= 32 ~ 52,
      raw_score >= 28 & raw_score <= 30 ~ 51,
      
      # 50 and just below
      raw_score >= 26 & raw_score <= 27 ~ 50,
      raw_score >= 24 & raw_score <= 25 ~ 49,
      raw_score >= 21 & raw_score <= 23 ~ 48,
      raw_score >= 19 & raw_score <= 20 ~ 47,
      raw_score >= 16 & raw_score <= 18 ~ 46,
      
      # 45 and just below
      raw_score >= 14 & raw_score <= 15 ~ 45,
      raw_score >= 12 & raw_score <= 13 ~ 44,
      raw_score >= 9 & raw_score <= 11 ~ 43,
      raw_score >= 7 & raw_score <= 8 ~ 42,
      raw_score >= 5 & raw_score <= 6 ~ 41,
      
      # 40 and just below
      raw_score >= 2 & raw_score <= 4 ~ 40,
      raw_score >= 0 & raw_score <= 1 ~ 39,
      
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(female_tscore)
    
  } else {
    # Male conversion logic (keeping the existing male table)
    male_tscore <- case_when(
      # 90 and just below
      raw_score >= 134 ~ 90,
      raw_score >= 131 & raw_score <= 133 ~ 89,
      raw_score >= 129 & raw_score <= 130 ~ 88,
      raw_score >= 126 & raw_score <= 128 ~ 87,
      raw_score >= 124 & raw_score <= 125 ~ 86,
      
      # 85 and just below
      raw_score >= 121 & raw_score <= 123 ~ 85,
      raw_score >= 119 & raw_score <= 120 ~ 84,
      raw_score >= 116 & raw_score <= 118 ~ 83,
      raw_score >= 113 & raw_score <= 115 ~ 82,
      raw_score >= 111 & raw_score <= 112 ~ 81,
      
      # 80 and just below
      raw_score >= 108 & raw_score <= 110 ~ 80,
      raw_score >= 106 & raw_score <= 107 ~ 79,
      raw_score >= 103 & raw_score <= 105 ~ 78,
      raw_score >= 101 & raw_score <= 102 ~ 77,
      raw_score >= 98 & raw_score <= 100 ~ 76,
      
      # 75 and just below
      raw_score >= 96 & raw_score <= 97 ~ 75,
      raw_score >= 93 & raw_score <= 95 ~ 74,
      raw_score >= 91 & raw_score <= 92 ~ 73,
      raw_score >= 88 & raw_score <= 90 ~ 72,
      raw_score >= 86 & raw_score <= 87 ~ 71,
      
      # 70 and just below
      raw_score >= 83 & raw_score <= 85 ~ 70,
      raw_score >= 81 & raw_score <= 82 ~ 69,
      raw_score >= 78 & raw_score <= 80 ~ 68,
      raw_score >= 76 & raw_score <= 77 ~ 67, #alo mismatch 76-77, 77-90?
      raw_score >= 73 & raw_score <= 75 ~ 66, #algo mismatch 33?35?
      
      # 65 and just below
      raw_score >= 71 & raw_score <= 72 ~ 65,
      raw_score >= 68 & raw_score <= 70 ~ 64,
      raw_score >= 66 & raw_score <= 67 ~ 63,
      raw_score >= 63 & raw_score <= 65 ~ 62,
      raw_score >= 61 & raw_score <= 62 ~ 61,
      
      # 60 and just below
      raw_score >= 58 & raw_score <= 60 ~ 60,
      raw_score >= 56 & raw_score <= 57 ~ 59,
      raw_score >= 53 & raw_score <= 55 ~ 58,
      raw_score >= 50 & raw_score <= 52 ~ 57,
      raw_score >= 48 & raw_score <= 49 ~ 56,
      
      # 55 and just below
      raw_score >= 45 & raw_score <= 47 ~ 55,
      raw_score >= 43 & raw_score <= 44 ~ 54,
      raw_score >= 40 & raw_score <= 42 ~ 53,
      raw_score >= 38 & raw_score <= 39 ~ 52,
      raw_score >= 35 & raw_score <= 37 ~ 51,
      
      # 50 and just below
      raw_score >= 33 & raw_score <= 34 ~ 50,
      raw_score >= 30 & raw_score <= 32 ~ 49,
      raw_score >= 28 & raw_score <= 29 ~ 48,
      raw_score >= 25 & raw_score <= 27 ~ 47,
      raw_score >= 23 & raw_score <= 24 ~ 46,
      
      # 45 and just below
      raw_score >= 20 & raw_score <= 22 ~ 45,
      raw_score >= 18 & raw_score <= 19 ~ 44,
      raw_score >= 15 & raw_score <= 17 ~ 43,
      raw_score >= 13 & raw_score <= 14 ~ 42,
      raw_score >= 10 & raw_score <= 12 ~ 41,
      
      # 40 and just below
      raw_score >= 8 & raw_score <= 9 ~ 40,
      raw_score >= 5 & raw_score <= 7 ~ 39,
      raw_score >= 3 & raw_score <= 4 ~ 38,
      raw_score >= 0 & raw_score <= 2 ~ 37,
      
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    
    return(male_tscore)
  }
}


# Apply the function
srs_parent_raw$tscore <- mapply(srs_convert_to_tscore, 
                                srs_parent_raw$total_raw_score, 
                                srs_parent_raw$sex)


# srs_parent T SCORE preschool ------- ------------------------------------------------


# srs_parent T SCORE school age ------- ------------------------------------------------

srs_parent_convert_to_tscore_sci_school <- function(raw_score, gender) {
  # Input validation
  if (!is.numeric(raw_score)) {
    stop("Raw score must be numeric")
  }
  
  if (!gender %in% c("male", "female")) {
    stop("Gender must be either 'male' or 'female'")
  }
  
  if (gender == "female") {
    # Female conversion logic
    female_tscore <- case_when(
      # 90 and just below
      raw_score >= 102 ~ 90,
      raw_score >= 100 & raw_score <= 101 ~ 89,
      raw_score >= 98 & raw_score <= 99 ~ 88,
      raw_score >= 96 & raw_score <= 97 ~ 87,
      raw_score >= 94 & raw_score <= 95 ~ 86,
      
      # 85 and just below
      raw_score >= 92 & raw_score <= 93 ~ 85,
      raw_score >= 90 & raw_score <= 91 ~ 84,
      raw_score == 89 ~ 83,
      raw_score >= 87 & raw_score <= 88 ~ 82,
      raw_score >= 85 & raw_score <= 86 ~ 81,
      
      # 80 and just below
      raw_score >= 83 & raw_score <= 84 ~ 80,
      raw_score >= 81 & raw_score <= 82 ~ 79,
      raw_score >= 79 & raw_score <= 80 ~ 78,
      raw_score >= 77 & raw_score <= 78 ~ 77,
      raw_score >= 75 & raw_score <= 76 ~ 76,
      
      # 75 and just below
      raw_score >= 73 & raw_score <= 74 ~ 75,
      raw_score >= 71 & raw_score <= 72 ~ 74,
      raw_score >= 69 & raw_score <= 70 ~ 73,
      raw_score >= 67 & raw_score <= 68 ~ 72,
      raw_score >= 65 & raw_score <= 66 ~ 71,
      
      # 70 and just below
      raw_score >= 63 & raw_score <= 67 ~ 70,
      raw_score >= 61 & raw_score <= 62 ~ 69,
      raw_score >= 59 & raw_score <= 60 ~ 68,
      raw_score >= 57 & raw_score <= 58 ~ 67,
      raw_score >= 55 & raw_score <= 56 ~ 66,
      
      # 65 and just below
      raw_score == 54 ~ 65,
      raw_score >= 52 & raw_score <= 53 ~ 64,
      raw_score >= 50 & raw_score <= 51 ~ 63,
      raw_score >= 48 & raw_score <= 49 ~ 62,
      raw_score >= 46 & raw_score <= 47 ~ 61,
      
      # 60 and just below
      raw_score >= 44 & raw_score <= 45 ~ 60,
      raw_score >= 42 & raw_score <= 43 ~ 59,
      raw_score >= 40 & raw_score <= 41 ~ 58,
      raw_score >= 38 & raw_score <= 39 ~ 57,
      raw_score >= 36 & raw_score <= 37 ~ 56,
      
      # 55 and just below
      raw_score >= 34 & raw_score <= 35 ~ 55,
      raw_score >= 32 & raw_score <= 33 ~ 54, 
      raw_score >= 30 & raw_score <= 31 ~ 53, 
      raw_score >= 28 & raw_score <= 29 ~ 52,
      raw_score >= 26 & raw_score <= 27 ~ 51,
      
      # 50 and just below
      raw_score >= 24 & raw_score <= 25 ~ 50,
      raw_score >= 22 & raw_score <= 23 ~ 49,
      raw_score == 21 ~ 48,
      raw_score >= 19 & raw_score <= 20 ~ 47,
      raw_score >= 17 & raw_score <= 18 ~ 46,
      
      # 45 and just below
      raw_score >= 15 & raw_score <= 16 ~ 45,
      raw_score >= 13 & raw_score <= 14 ~ 44,
      raw_score >= 11 & raw_score <= 12 ~ 43,
      raw_score >= 9 & raw_score <= 10 ~ 42,
      raw_score >= 7 & raw_score <= 8 ~ 41,
      
      # 40 and just below
      raw_score >= 5 & raw_score <= 6 ~ 40,
      raw_score >= 3 & raw_score <= 4 ~ 39,
      raw_score >= 1 & raw_score <= 2 ~ 38,
      raw_score == 0 ~ 37,
      
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(female_tscore)
    
  } else {
    # Male conversion logic (keeping the existing male table)
    male_tscore <- case_when(
      # 90 and just below
      raw_score >= 109 ~ 90,
      raw_score >= 107 & raw_score <= 108 ~ 89,
      raw_score >= 105 & raw_score <= 106 ~ 88,
      raw_score >= 103 & raw_score <= 104 ~ 87,
      raw_score >= 101 & raw_score <= 102 ~ 86,
      
      # 85 and just below
      raw_score >= 99 & raw_score <= 100 ~ 85,
      raw_score >= 97 & raw_score <= 98 ~ 84,
      raw_score >= 95 & raw_score <= 96 ~ 83,
      raw_score >= 93 & raw_score <= 94 ~ 82,
      raw_score >= 91 & raw_score <= 92 ~ 81,
      
      # 80 and just below
      raw_score >= 89 & raw_score <= 90 ~ 80,
      raw_score >= 87 & raw_score <= 88 ~ 79,
      raw_score >= 85 & raw_score <= 86 ~ 78,
      raw_score >= 83 & raw_score <= 84 ~ 77,
      raw_score >= 81 & raw_score <= 82 ~ 76,
      
      # 75 and just below
      raw_score >= 79 & raw_score <= 80 ~ 75,
      raw_score >= 77 & raw_score <= 78 ~ 74,
      raw_score >= 75 & raw_score <= 76 ~ 73,
      raw_score >= 73 & raw_score <= 74 ~ 72,
      raw_score >= 70 & raw_score <= 72 ~ 71,
      
      # 70 and just below
      raw_score >= 68 & raw_score <= 69 ~ 70,
      raw_score >= 66 & raw_score <= 67 ~ 69,
      raw_score >= 64 & raw_score <= 65 ~ 68,
      raw_score >= 62 & raw_score <= 63 ~ 67, 
      raw_score >= 60 & raw_score <= 61 ~ 66, 
      
      # 65 and just below
      raw_score >= 58 & raw_score <= 59 ~ 65,
      raw_score >= 56 & raw_score <= 57 ~ 64,
      raw_score >= 54 & raw_score <= 55 ~ 63,
      raw_score >= 52 & raw_score <= 53 ~ 62,
      raw_score >= 50 & raw_score <= 51 ~ 61,
      
      # 60 and just below
      raw_score >= 48 & raw_score <= 49 ~ 60,
      raw_score >= 46 & raw_score <= 47 ~ 59,
      raw_score >= 44 & raw_score <= 45 ~ 58,
      raw_score >= 42 & raw_score <= 43 ~ 57,
      raw_score >= 40 & raw_score <= 41 ~ 56,
      
      # 55 and just below
      raw_score >= 38 & raw_score <= 39 ~ 55,
      raw_score >= 36 & raw_score <= 37 ~ 54,
      raw_score >= 34 & raw_score <= 35 ~ 53,
      raw_score >= 32 & raw_score <= 33 ~ 52,
      raw_score >= 30 & raw_score <= 31 ~ 51,
      
      # 50 and just below
      raw_score >= 28 & raw_score <= 29 ~ 50,
      raw_score >= 26 & raw_score <= 27 ~ 49,
      raw_score >= 24 & raw_score <= 25 ~ 48,
      raw_score >= 22 & raw_score <= 23 ~ 47,
      raw_score >= 20 & raw_score <= 21 ~ 46,
      
      # 45 and just below
      raw_score >= 18 & raw_score <= 19 ~ 45,
      raw_score >= 16 & raw_score <= 17 ~ 44,
      raw_score >= 14 & raw_score <= 15 ~ 43,
      raw_score >= 12 & raw_score <= 13 ~ 42,
      raw_score >= 10 & raw_score <= 11 ~ 41,
      
      # 40 and just below
      raw_score >= 8 & raw_score <= 9 ~ 40,
      raw_score >= 6 & raw_score <= 7 ~ 39,
      raw_score >= 4 & raw_score <= 5 ~ 38,
      raw_score >= 2 & raw_score <= 3 ~ 37,
      raw_score >= 0 & raw_score <= 1 ~ 36,
      
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    
    return(male_tscore)
  }
}

# Apply the function
srs_parent_raw$tscore_sci <- mapply(srs_parent_convert_to_tscore_sci_school, 
                                    srs_parent_raw$sci_raw, 
                                    srs_parent_raw$sex)

srs_parent_convert_to_tscore_rrb_school <- function(raw_score, gender) {
  # Input validation
  if (!is.numeric(raw_score)) {
    stop("Raw score must be numeric")
  }
  
  if (!gender %in% c("male", "female")) {
    stop("Gender must be either 'male' or 'female'")
  }
  
  if (gender == "female") {
    # Female conversion logic
    female_tscore <- case_when(
      # 90 and just below
      raw_score >= 24 ~ 90,
      raw_score == 23 ~ 88,
      raw_score == 22 ~ 86,
      raw_score == 21 ~ 84,
      raw_score == 20 ~ 82,
      
      # 80 and just below
      raw_score == 19 ~ 80,
      raw_score == 18 ~ 78,
      raw_score == 17 ~ 76,
      raw_score == 16 ~ 74,
      raw_score == 15 ~ 72,
      
      # 70 and just below
      raw_score == 14 ~ 70,
      raw_score == 13 ~ 68,
      raw_score == 12 ~ 66,
      raw_score == 11 ~ 64,
      raw_score == 10 ~ 62, #algorithm problem 21 instead of 10
      
      # 60 and just below
      raw_score == 9 ~ 60,
      raw_score == 8 ~ 58,
      raw_score == 7 ~ 56,
      raw_score == 6 ~ 54,
      raw_score == 5 ~ 52,
      
      # 50 and just below
      raw_score == 4 ~ 50,
      raw_score == 3 ~ 48,
      raw_score == 2 ~ 46,
      raw_score == 1 ~ 44,
      raw_score == 0 ~ 42,
      
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(female_tscore)
    
  } else {
    # Male conversion logic (keeping the existing male table)
    male_tscore <- case_when(
      
      raw_score >= 28 ~ 90,
      raw_score == 27 ~ 89,
      raw_score == 26 ~ 87,
      raw_score == 25 ~ 85,
      raw_score == 24 ~ 83,
      raw_score == 23 ~ 82,
      
      raw_score == 22 ~ 80,
      raw_score == 21 ~ 78,
      raw_score == 20 ~ 76,
      raw_score == 19 ~ 75,
      raw_score == 18 ~ 73,
      raw_score == 17 ~ 71,
      
      raw_score == 16 ~ 69,
      raw_score == 15 ~ 68,
      raw_score == 14 ~ 66,
      raw_score == 13 ~ 64,
      raw_score == 12 ~ 62,
      raw_score == 11 ~ 61,
      
      raw_score == 10 ~ 59,
      raw_score == 9 ~ 57,
      raw_score == 8 ~ 55,
      raw_score == 7 ~ 54,
      raw_score == 6 ~ 52,
      raw_score == 5 ~ 50,
      
      raw_score == 4 ~ 48,
      raw_score == 3 ~ 46,
      raw_score == 2 ~ 45,
      raw_score == 1 ~ 43,
      raw_score == 0 ~ 41,
      
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    
    return(male_tscore)
  }
}

# Apply the function
srs_parent_raw$tscore_rrb <- mapply(srs_parent_convert_to_tscore_rrb_school, 
                                    srs_parent_raw$mannerisms, 
                                    srs_parent_raw$sex)

srs_parent_raw |> 
  select(individual, sex, tscore, tscore_sci, sci_raw, tscore_rrb, mannerisms)
# Function to convert raw motivation (MOT) scores to T-scores for parent report (for males and females)
srs_parent_convert_to_tscore_mot_school <- function(raw_score, gender) {
  # Input validation
  if (!is.numeric(raw_score)) {
    stop("Raw score must be numeric")
  }
  
  if (!gender %in% c("male", "female")) {
    stop("Gender must be either 'male' or 'female'")
  }
  
  if (gender == "female") {
    # Female conversion logic for Motivation (MOT)
    tscore <- case_when(
      raw_score >= 24 ~ 90,
      raw_score == 23 ~ 89,
      raw_score == 22 ~ 86,
      raw_score == 21 ~ 84,
      raw_score == 20 ~ 82,
      raw_score == 19 ~ 80,
      raw_score == 18 ~ 78,
      raw_score == 17 ~ 75,
      raw_score == 16 ~ 73,
      raw_score == 15 ~ 71,
      raw_score == 14 ~ 69,
      raw_score == 13 ~ 67,
      raw_score == 12 ~ 64,
      raw_score == 11 ~ 62,
      raw_score == 10 ~ 60,
      raw_score == 9  ~ 58,
      raw_score == 8  ~ 56,
      raw_score == 7  ~ 54,
      raw_score == 6  ~ 51,
      raw_score == 5  ~ 49,
      raw_score == 4  ~ 47,
      raw_score == 3  ~ 45,
      raw_score == 2  ~ 43,
      raw_score == 1  ~ 40,
      raw_score == 0  ~ 38,
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(tscore)
    
  } else {
    # Male conversion logic for Motivation (MOT)
    tscore <- case_when(
      raw_score >= 26 ~ 90,
      raw_score == 25 ~ 89,
      raw_score == 24 ~ 87,
      raw_score == 23 ~ 85,
      raw_score == 22 ~ 83,
      raw_score == 21 ~ 81,
      raw_score == 20 ~ 79,
      raw_score == 19 ~ 77,
      raw_score == 18 ~ 75,
      raw_score == 17 ~ 73,
      raw_score == 16 ~ 71,
      raw_score == 15 ~ 69,
      raw_score == 14 ~ 67,
      raw_score == 13 ~ 64,
      raw_score == 12 ~ 62,
      raw_score == 11 ~ 60,
      raw_score == 10 ~ 58,
      raw_score == 9  ~ 56,
      raw_score == 8  ~ 54,
      raw_score == 7  ~ 52,
      raw_score == 6  ~ 50,
      raw_score == 5  ~ 48,
      raw_score == 4  ~ 46,
      raw_score == 3  ~ 44,
      raw_score == 2  ~ 42,
      raw_score == 1  ~ 40,
      raw_score == 0  ~ 38,
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(tscore)
  }
}

srs_parent_raw$tscore_mot <- mapply(srs_parent_convert_to_tscore_mot_school, 
                                    srs_parent_raw$motivation, 
                                    srs_parent_raw$sex)


# Function to convert raw communication (COM) scores to T-scores for parent report (for males and females)
srs_parent_convert_to_tscore_com_school <- function(raw_score, gender) {
  # Input validation
  if (!is.numeric(raw_score)) {
    stop("Raw score must be numeric")
  }
  
  if (!gender %in% c("male", "female")) {
    stop("Gender must be either 'male' or 'female'")
  }
  
  if (gender == "female") {
    # Female conversion logic for Communication (COM)
    tscore <- case_when(
      raw_score >= 44 ~ 90,
      raw_score == 43 ~ 89,
      raw_score == 42 ~ 88,
      raw_score == 41 ~ 87,
      raw_score == 40 ~ 86,
      raw_score == 39 ~ 84,
      raw_score == 38 ~ 83,
      raw_score == 37 ~ 82,
      raw_score == 36 ~ 81,
      raw_score == 35 ~ 80,
      raw_score == 34 ~ 79,
      raw_score == 33 ~ 78,
      raw_score == 32 ~ 76,
      raw_score == 31 ~ 75,
      raw_score == 30 ~ 74,
      raw_score == 29 ~ 73,
      raw_score == 28 ~ 72,
      raw_score == 27 ~ 71,
      raw_score == 26 ~ 70,
      raw_score == 25 ~ 68,
      raw_score == 24 ~ 67,
      raw_score == 23 ~ 66,
      raw_score == 22 ~ 65,
      raw_score == 21 ~ 64,
      raw_score == 20 ~ 63,
      raw_score == 19 ~ 62,
      raw_score == 18 ~ 60,
      raw_score == 17 ~ 59,
      raw_score == 16 ~ 58,
      raw_score == 15 ~ 57,
      raw_score == 14 ~ 56,
      raw_score == 13 ~ 55,
      raw_score == 12 ~ 53,
      raw_score == 11 ~ 52,
      raw_score == 10 ~ 51,
      raw_score == 9  ~ 50,
      raw_score == 8  ~ 49,
      raw_score == 7  ~ 48,
      raw_score == 6  ~ 47,
      raw_score == 5  ~ 45,
      raw_score == 4  ~ 44,
      raw_score == 3  ~ 43,
      raw_score == 2  ~ 42,
      raw_score == 1  ~ 41,
      raw_score == 0  ~ 40,
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(tscore)
    
  } else {
    # Male conversion logic for Communication (COM)
    tscore <- case_when(
      raw_score >= 47 ~ 90,
      raw_score == 46 ~ 89,
      raw_score == 45 ~ 88,
      raw_score == 44 ~ 86,
      raw_score == 43 ~ 85,
      raw_score == 42 ~ 84,
      raw_score == 41 ~ 83,
      raw_score == 40 ~ 82,
      raw_score == 39 ~ 81,
      raw_score == 38 ~ 80,
      raw_score == 37 ~ 79,
      raw_score == 36 ~ 78,
      raw_score == 35 ~ 77,
      raw_score == 34 ~ 75,
      raw_score == 33 ~ 74,
      raw_score == 32 ~ 73,
      raw_score == 31 ~ 72,
      raw_score == 30 ~ 71,
      raw_score == 29 ~ 70,
      raw_score == 28 ~ 69,
      raw_score == 27 ~ 68,
      raw_score == 26 ~ 67,
      raw_score == 25 ~ 66,
      raw_score == 24 ~ 65,
      raw_score == 23 ~ 63,
      raw_score == 22 ~ 62,
      raw_score == 21 ~ 61,
      raw_score == 20 ~ 60,
      raw_score == 19 ~ 59,
      raw_score == 18 ~ 58,
      raw_score == 17 ~ 57,
      raw_score == 16 ~ 56,
      raw_score == 15 ~ 55,
      raw_score == 14 ~ 54,
      raw_score == 13 ~ 52,
      raw_score == 12 ~ 51,
      raw_score == 11 ~ 50,
      raw_score == 10 ~ 49,
      raw_score == 9  ~ 48,
      raw_score == 8  ~ 47,
      raw_score == 7  ~ 46,
      raw_score == 6  ~ 45,
      raw_score == 5  ~ 44,
      raw_score == 4  ~ 43,
      raw_score == 3  ~ 41,
      raw_score == 2  ~ 40,
      raw_score == 1  ~ 39,
      raw_score == 0  ~ 38,
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(tscore)
  }
}

# Apply the function
srs_parent_raw$tscore_com <- mapply(srs_parent_convert_to_tscore_com_school, 
                                    srs_parent_raw$communication, 
                                    srs_parent_raw$sex)

# Function to convert raw cognition (COG) scores to T-scores for parent report (for males and females)
srs_parent_convert_to_tscore_cog_school <- function(raw_score, gender) {
  # Input validation
  if (!is.numeric(raw_score)) {
    stop("Raw score must be numeric")
  }
  
  if (!gender %in% c("male", "female")) {
    stop("Gender must be either 'male' or 'female'")
  }
  
  if (gender == "female") {
    # Female conversion logic for Cognition (COG)
    tscore <- case_when(
      raw_score >= 26 ~ 90,
      raw_score == 25 ~ 88,
      raw_score == 24 ~ 86,
      raw_score == 23 ~ 84,
      raw_score == 22 ~ 82,
      raw_score == 21 ~ 80,
      raw_score == 20 ~ 78,
      raw_score == 19 ~ 77,
      raw_score == 18 ~ 75,
      raw_score == 17 ~ 73,
      raw_score == 16 ~ 71,
      raw_score == 15 ~ 69,
      raw_score == 14 ~ 67,
      raw_score == 13 ~ 65,
      raw_score == 12 ~ 63,
      raw_score == 11 ~ 61,
      raw_score == 10 ~ 59,
      raw_score == 9  ~ 57,
      raw_score == 8  ~ 55,
      raw_score == 7  ~ 53,
      raw_score == 6  ~ 51,
      raw_score == 5  ~ 49,
      raw_score == 4  ~ 47,
      raw_score == 3  ~ 45,
      raw_score == 2  ~ 43,
      raw_score == 1  ~ 41,
      raw_score == 0  ~ 39,
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(tscore)
    
  } else {
    # Male conversion logic for Cognition (COG)
    tscore <- case_when(
      raw_score >= 28 ~ 90,
      raw_score == 27 ~ 89,
      raw_score == 26 ~ 87,
      raw_score == 25 ~ 85,
      raw_score == 24 ~ 83,
      raw_score == 23 ~ 81,
      raw_score == 22 ~ 79,
      raw_score == 21 ~ 78,
      raw_score == 20 ~ 76,
      raw_score == 19 ~ 74,
      raw_score == 18 ~ 72,
      raw_score == 17 ~ 70,
      raw_score == 16 ~ 68,
      raw_score == 15 ~ 66,
      raw_score == 14 ~ 65,
      raw_score == 13 ~ 63,
      raw_score == 12 ~ 61,
      raw_score == 11 ~ 59,
      raw_score == 10 ~ 57,
      raw_score == 9  ~ 55,
      raw_score == 8  ~ 53,
      raw_score == 7  ~ 52,
      raw_score == 6  ~ 50,
      raw_score == 5  ~ 47,
      raw_score == 4  ~ 45,
      raw_score == 3  ~ 43,
      raw_score == 2  ~ 41,
      raw_score == 1  ~ 40,
      raw_score == 0  ~ 39,
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(tscore)
  }
}

srs_parent_raw$tscore_cog <- mapply(srs_parent_convert_to_tscore_cog_school, 
                                    srs_parent_raw$cognition, 
                                    srs_parent_raw$sex)

# Function to convert raw awareness (AWR) scores to T-scores for parent report (for males and females)
srs_parent_convert_to_tscore_awr_school <- function(raw_score, gender) {
  # Input validation
  if (!is.numeric(raw_score)) {
    stop("Raw score must be numeric")
  }
  
  if (!gender %in% c("male", "female")) {
    stop("Gender must be either 'male' or 'female'")
  }
  
  if (gender == "female") {
    # Female conversion logic for Awareness (AWR)
    tscore <- case_when(
      raw_score >= 18 ~ 90,
      raw_score == 17 ~ 87,
      raw_score == 16 ~ 84,
      raw_score == 15 ~ 79,
      raw_score == 14 ~ 78,
      raw_score == 13 ~ 75,
      raw_score == 12 ~ 69,
      raw_score == 11 ~ 68,
      raw_score == 10 ~ 65,
      raw_score == 9  ~ 62,
      raw_score == 8  ~ 59,
      raw_score == 7  ~ 56,
      raw_score == 6  ~ 53,
      raw_score == 5  ~ 50,
      raw_score == 4  ~ 47,
      raw_score == 3  ~ 43,
      raw_score == 2  ~ 40,
      raw_score == 1  ~ 37,
      raw_score == 0  ~ 34,
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(tscore)
    
  } else {
    # Male conversion logic for Awareness (AWR)
    tscore <- case_when(
      raw_score >= 19 ~ 90,
      raw_score == 18 ~ 89,
      raw_score == 17 ~ 86,
      raw_score == 16 ~ 82,
      raw_score == 15 ~ 79,
      raw_score == 14 ~ 76,
      raw_score == 13 ~ 73,
      raw_score == 12 ~ 70,
      raw_score == 11 ~ 67,
      raw_score == 10 ~ 64,
      raw_score == 9  ~ 60,
      raw_score == 8  ~ 57,
      raw_score == 7  ~ 54,
      raw_score == 6  ~ 51,
      raw_score == 5  ~ 48,
      raw_score == 4  ~ 45,
      raw_score == 3  ~ 41,
      raw_score == 2  ~ 38,
      raw_score == 1  ~ 35,
      raw_score == 0  ~ 32,
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(tscore)
  }
}

srs_parent_raw$tscore_awr<- mapply(srs_parent_convert_to_tscore_awr_school, 
                                   srs_parent_raw$awareness, 
                                    srs_parent_raw$sex)

srs_parent_tscores <- srs_parent_raw |> 
  mutate(informant = "parent") |> 
  select(individual, sex, informant, tscore_sci, sci_raw, tscore_rrb, mannerisms, tscore_awr, tscore_cog, tscore_com, tscore_mot)


# .- - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - -  ------------------------------------------


# srs_teacher ------------------------- -----------------------------------------





# srs_convert_to_tscore_teacher  function(raw_score, gender) {
#   # Input validation
#   if (!is.numeric(raw_score)) {
#     stop("Raw score must be numeric")
#   }
# 
#   if (!gender %in% c("male", "female")) {
#     stop("Gender must be either 'male' or 'female'")
#   }
# 
#   if (gender == "female") {
#     # Female conversion logic
#     female_tscore <- case_when(
#       # 90 and just below
#       raw_score >= 112 ~ 90,
#       raw_score >= 110 & raw_score <= 111 ~ 89,
#       raw_score >= 108 & raw_score <= 109 ~ 88,
#       raw_score >= 106 & raw_score <= 107 ~ 87,
#       raw_score >= 104 & raw_score <= 105 ~ 86,
# 
#       # 85 and just below
#       raw_score >= 102 & raw_score <= 103 ~ 85,
#       raw_score >= 99 & raw_score <= 101 ~ 84,
#       raw_score >= 97 & raw_score <= 98 ~ 83,
#       raw_score >= 95 & raw_score <= 96 ~ 82,
#       raw_score >= 93 & raw_score <= 94 ~ 81,
# 
#       # 80 and just below
#       raw_score >= 91 & raw_score <= 92 ~ 80,
#       raw_score >= 88 & raw_score <= 90 ~ 79,
#       raw_score >= 86 & raw_score <= 87 ~ 78,
#       raw_score >= 84 & raw_score <= 85 ~ 77,
#       raw_score >= 82 & raw_score <= 83 ~ 76,
# 
#       # 75 and just below
#       raw_score >= 80 & raw_score <= 81 ~ 75,
#       raw_score >= 78 & raw_score <= 79 ~ 74,
#       raw_score >= 75 & raw_score <= 77 ~ 73,
#       raw_score >= 73 & raw_score <= 74 ~ 72,
#       raw_score >= 71 & raw_score <= 72 ~ 71,
# 
#       # 70 and just below
#       raw_score >= 69 & raw_score <= 70 ~ 70,
#       raw_score >= 67 & raw_score <= 68 ~ 69,
#       raw_score >= 64 & raw_score <= 66 ~ 68,
#       raw_score >= 62 & raw_score <= 63 ~ 67,
#       raw_score >= 60 & raw_score <= 61 ~ 66,
# 
#       # 65 and just below
#       raw_score >= 58 & raw_score <= 59 ~ 65,
#       raw_score >= 56 & raw_score <= 57 ~ 64, #code mismatch 57
#       raw_score >= 54 & raw_score <= 55 ~ 63,
#       raw_score >= 51 & raw_score <= 53 ~ 62,
#       raw_score >= 49 & raw_score <= 50 ~ 61,
# 
#       # 60 and just below
#       raw_score >= 47 & raw_score <= 48 ~ 60,
#       raw_score >= 45 & raw_score <= 46 ~ 59,
#       raw_score >= 43 & raw_score <= 44 ~ 58,
#       raw_score >= 40 & raw_score <= 42 ~ 57,
#       raw_score >= 38 & raw_score <= 39 ~ 56, #code mismatch 8-39
# 
#       # 55 and just below
#       raw_score >= 36 & raw_score <= 37 ~ 55,
#       raw_score >= 34 & raw_score <= 35 ~ 54,
#       raw_score >= 32 & raw_score <= 33 ~ 53,
#       raw_score >= 30 & raw_score <= 31 ~ 52,
#       raw_score >= 27 & raw_score <= 29 ~ 51,
# 
#       # 50 and just below
#       raw_score >= 25 & raw_score <= 26 ~ 50,
#       raw_score >= 23 & raw_score <= 24 ~ 49,
#       raw_score >= 21 & raw_score <= 22 ~ 48,
#       raw_score >= 19 & raw_score <= 20 ~ 47,
#       raw_score >= 16 & raw_score <= 18 ~ 46,
# 
#       # 45 and just below
#       raw_score >= 14 & raw_score <= 15 ~ 45,
#       raw_score >= 12 & raw_score <= 13 ~ 44,
#       raw_score >= 10 & raw_score <= 11 ~ 43,
#       raw_score >= 8 & raw_score <= 9 ~ 42,
#       raw_score >= 6 & raw_score <= 7 ~ 41,
# 
#       # 40 and just below
#       raw_score >= 3 & raw_score <= 5 ~ 40,
#       raw_score >= 1 & raw_score <= 2 ~ 39,
#       raw_score == 0 ~ 38,
# 
#       TRUE ~ NA_real_  # Default case if no conditions are met
#     )
#     return(female_tscore)
# 
#   } else {
#     # Male conversion logic (keeping the existing male table)
#     male_tscore <- case_when(
#       # 90 and just below
#       raw_score >= 149 ~ 90,
#       raw_score >= 147 & raw_score <= 148 ~ 89,
#       raw_score >= 144 & raw_score <= 146 ~ 88,
#       raw_score >= 141 & raw_score <= 143 ~ 87,
#       raw_score >= 138 & raw_score <= 140 ~ 86,
# 
#       # 85 and just below
#       raw_score >= 135 & raw_score <= 137 ~ 85,
#       raw_score >= 132 & raw_score <= 134 ~ 84,
#       raw_score >= 129 & raw_score <= 131 ~ 83,
#       raw_score >= 126 & raw_score <= 128 ~ 82,
#       raw_score >= 124 & raw_score <= 125 ~ 81,
# 
#       # 80 and just below
#       raw_score >= 121 & raw_score <= 123 ~ 80,
#       raw_score >= 118 & raw_score <= 120 ~ 79,
#       raw_score >= 115 & raw_score <= 117 ~ 78,
#       raw_score >= 112 & raw_score <= 114 ~ 77,
#       raw_score >= 109 & raw_score <= 111 ~ 76,
# 
#       # 75 and just below
#       raw_score >= 106 & raw_score <= 108 ~ 75,
#       raw_score >= 103 & raw_score <= 105 ~ 74,
#       raw_score >= 100 & raw_score <= 102 ~ 73,
#       raw_score >= 98 & raw_score <= 99 ~ 72,
#       raw_score >= 95 & raw_score <= 97 ~ 71,
# 
#       # 70 and just below
#       raw_score >= 92 & raw_score <= 94 ~ 70,
#       raw_score >= 89 & raw_score <= 91 ~ 69,
#       raw_score >= 86 & raw_score <= 88 ~ 68,
#       raw_score >= 83 & raw_score <= 85 ~ 67,
#       raw_score >= 80 & raw_score <= 82 ~ 66,
# 
#       # 65 and just below
#       raw_score >= 77 & raw_score <= 79 ~ 65,
#       raw_score >= 75 & raw_score <= 76 ~ 64,
#       raw_score >= 72 & raw_score <= 74 ~ 63,
#       raw_score >= 69 & raw_score <= 71 ~ 62,
#       raw_score >= 66 & raw_score <= 68 ~ 61,
# 
#       # 60 and just below
#       raw_score >= 63 & raw_score <= 65 ~ 60,
#       raw_score >= 60 & raw_score <= 62 ~ 59,
#       raw_score >= 57 & raw_score <= 59 ~ 58,
#       raw_score >= 54 & raw_score <= 56 ~ 57,
#       raw_score >= 52 & raw_score <= 53 ~ 56,
# 
#       # 55 and just below
#       raw_score >= 49 & raw_score <= 51 ~ 55,
#       raw_score >= 46 & raw_score <= 48 ~ 54,
#       raw_score >= 43 & raw_score <= 45 ~ 53,
#       raw_score >= 40 & raw_score <= 42 ~ 52,
#       raw_score >= 37 & raw_score <= 39 ~ 51,
# 
#       # 50 and just below
#       raw_score >= 34 & raw_score <= 36 ~ 50,
#       raw_score >= 31 & raw_score <= 33 ~ 49,
#       raw_score >= 28 & raw_score <= 30 ~ 48,
#       raw_score >= 26 & raw_score <= 27 ~ 47,
#       raw_score >= 23 & raw_score <= 25 ~ 46,
# 
#       # 45 and just below
#       raw_score >= 20 & raw_score <= 22 ~ 45,
#       raw_score >= 17 & raw_score <= 19 ~ 44,
#       raw_score >= 14 & raw_score <= 16 ~ 43,
#       raw_score >= 11 & raw_score <= 13 ~ 42,
#       raw_score >= 8 & raw_score <= 10 ~ 41,
# 
#       # 40 and just below
#       raw_score >= 5 & raw_score <= 7 ~ 40,
#       raw_score >= 3 & raw_score <= 4 ~ 39,
#       raw_score >= 0 & raw_score <= 2 ~ 38,
# 
#       TRUE ~ NA_real_  # Default case if no conditions are met
#     )
# 
#     return(male_tscore)
#   }
# }


# srs_teacher_raw$tscore <- mapply(srs_convert_to_tscore_teacher,
#                                 srs_teacher_raw$total_raw_score,
#                                 srs_teacher_raw$sex)



# srs_teacher_t_scores preschool------- -----------------------------------------------------


# srs_teacher_t_scores school age----- ------------------------------------------------
srs_teacher_raw$total_raw_score <- rowSums(srs_teacher_raw[, 4:68], na.rm = TRUE)

srs_teacher_raw <- srs_teacher_raw %>%
  left_join(
    core_selected %>% select(individual, sex),
    by = "individual"
  ) 

srs_teacher_raw <- srs_teacher_raw |> 
  filter(!is.na(sex))

srs_teacher_raw <- srs_teacher_raw %>%
  left_join(
    srs_teacher_df %>% select(individual, awareness, cognition, communication, mannerisms, motivation),
    by = "individual"
  )

srs_teacher_raw$sci_raw <- rowSums(srs_teacher_raw[,c("awareness","cognition","motivation","communication")])

srs_teacher_convert_to_tscore_sci_school <- function(raw_score, gender) {
  # Input validation
  if (!is.numeric(raw_score)) {
    stop("Raw score must be numeric")
  }
  
  if (!gender %in% c("male", "female")) {
    stop("Gender must be either 'male' or 'female'")
  }
  
  if (gender == "female") {
    # Female conversion logic
    female_tscore <- case_when(
      # 90 and just below
      raw_score >= 97 ~ 90,
      raw_score >= 95 & raw_score <= 96 ~ 89,
      raw_score == 94 ~ 88,
      raw_score >= 92 & raw_score <= 93 ~ 87,
      raw_score >= 90 & raw_score <= 91 ~ 86,
      
      # 85 and just below
      raw_score >= 88 & raw_score <= 89 ~ 85,
      raw_score >= 86 & raw_score <= 87 ~ 84,
      raw_score >= 84 & raw_score <= 85 ~ 83,
      raw_score >= 82 & raw_score <= 83 ~ 82,
      raw_score == 81 ~ 81,
      
      # 80 and just below
      raw_score >= 79 & raw_score <= 80 ~ 80,
      raw_score >= 77 & raw_score <= 78 ~ 79,
      raw_score >= 75 & raw_score <= 76 ~ 78,
      raw_score >= 73 & raw_score <= 74 ~ 77,
      raw_score >= 71 & raw_score <= 72 ~ 76,
      
      # 75 and just below
      raw_score >= 69 & raw_score <= 70 ~ 75,
      raw_score >= 67 & raw_score <= 68 ~ 74,
      raw_score == 66 ~ 73,
      raw_score >= 64 & raw_score <= 65 ~ 72,
      raw_score >= 62 & raw_score <= 63 ~ 71,
      
      # 70 and just below
      raw_score >= 60 & raw_score <= 61 ~ 70,
      raw_score >= 58 & raw_score <= 59 ~ 69,
      raw_score >= 56 & raw_score <= 57 ~ 68,
      raw_score >= 54 & raw_score <= 55 ~ 67,
      raw_score >= 52 & raw_score <= 53 ~ 66,
      
      # 65 and just below
      raw_score == 51 ~ 65,
      raw_score >= 49 & raw_score <= 50 ~ 64,
      raw_score >= 47 & raw_score <= 48 ~ 63,
      raw_score >= 45 & raw_score <= 46 ~ 62,
      raw_score >= 43 & raw_score <= 44 ~ 61,
      
      # 60 and just below
      raw_score >= 41 & raw_score <= 42 ~ 60,
      raw_score >= 39 & raw_score <= 40 ~ 59,
      raw_score == 38 ~ 58,
      raw_score >= 36 & raw_score <= 37 ~ 57,
      raw_score >= 34 & raw_score <= 35 ~ 56,
      
      # 55 and just below
      raw_score >= 32 & raw_score <= 33 ~ 55,
      raw_score >= 30 & raw_score <= 31 ~ 54, 
      raw_score >= 28 & raw_score <= 29 ~ 53, 
      raw_score >= 26 & raw_score <= 27 ~ 52,
      raw_score >= 24 & raw_score <= 25 ~ 51,
      
      # 50 and just below
      raw_score == 23 ~ 50,
      raw_score >= 21 & raw_score <= 22 ~ 49,
      raw_score >= 19 & raw_score <= 20 ~ 48,
      raw_score >= 17 & raw_score <= 18 ~ 47,
      raw_score >= 15 & raw_score <= 16 ~ 46,
      
      # 45 and just below
      raw_score >= 13 & raw_score <= 14 ~ 45,
      raw_score >= 11 & raw_score <= 12 ~ 44,
      raw_score >= 9 & raw_score <= 10 ~ 43,
      raw_score == 8 ~ 42,
      raw_score >= 6 & raw_score <= 7 ~ 41,
      
      # 40 and just below
      raw_score >= 4 & raw_score <= 5 ~ 40,
      raw_score >= 2 & raw_score <= 3 ~ 39,
      raw_score >= 0 & raw_score <= 1 ~ 38,
      
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(female_tscore)
    
  } else {
    # Male conversion logic (keeping the existing male table)
    male_tscore <- case_when(
      # 90 and just below
      raw_score >= 125 ~ 90,
      raw_score >= 123 & raw_score <= 124 ~ 89,
      raw_score >= 120 & raw_score <= 122 ~ 88,
      raw_score >= 118 & raw_score <= 119 ~ 87,
      raw_score >= 115 & raw_score <= 117 ~ 86,
      
      # 85 and just below
      raw_score >= 113 & raw_score <= 114 ~ 85,
      raw_score >= 111 & raw_score <= 112 ~ 84,
      raw_score >= 108 & raw_score <= 110 ~ 83,
      raw_score >= 106 & raw_score <= 107 ~ 82,
      raw_score >= 104 & raw_score <= 105 ~ 81,
      
      # 80 and just below
      raw_score >= 101 & raw_score <= 103 ~ 80,
      raw_score >= 99 & raw_score <= 100 ~ 79,
      raw_score >= 96 & raw_score <= 98 ~ 78,
      raw_score >= 94 & raw_score <= 95 ~ 77,
      raw_score >= 92 & raw_score <= 93 ~ 76,
      
      # 75 and just below
      raw_score >= 89 & raw_score <= 91 ~ 75,
      raw_score >= 87 & raw_score <= 88 ~ 74,
      raw_score >= 85 & raw_score <= 86 ~ 73,
      raw_score >= 82 & raw_score <= 84 ~ 72,
      raw_score >= 80 & raw_score <= 81 ~ 71,
      
      # 70 and just below
      raw_score >= 77 & raw_score <= 79 ~ 70,
      raw_score >= 75 & raw_score <= 76 ~ 69,
      raw_score >= 73 & raw_score <= 74 ~ 68,
      raw_score >= 70 & raw_score <= 72 ~ 67, 
      raw_score >= 68 & raw_score <= 69 ~ 66, 
      
      # 65 and just below
      raw_score >= 66 & raw_score <= 67 ~ 65,
      raw_score >= 63 & raw_score <= 65 ~ 64,
      raw_score >= 61 & raw_score <= 62 ~ 63,
      raw_score >= 58 & raw_score <= 60 ~ 62,
      raw_score >= 56 & raw_score <= 57 ~ 61,
      
      # 60 and just below
      raw_score >= 54 & raw_score <= 55 ~ 60,
      raw_score >= 51 & raw_score <= 53 ~ 59,
      raw_score >= 49 & raw_score <= 50 ~ 58,
      raw_score >= 46 & raw_score <= 48 ~ 57,
      raw_score >= 44 & raw_score <= 45 ~ 56,
      
      # 55 and just below
      raw_score >= 42 & raw_score <= 43 ~ 55,
      raw_score >= 39 & raw_score <= 41 ~ 54,
      raw_score >= 37 & raw_score <= 38 ~ 53,
      raw_score >= 35 & raw_score <= 36 ~ 52,
      raw_score >= 32 & raw_score <= 34 ~ 51,
      
      # 50 and just below
      raw_score >= 30 & raw_score <= 31 ~ 50,
      raw_score >= 27 & raw_score <= 29 ~ 49,
      raw_score >= 25 & raw_score <= 26 ~ 48,
      raw_score >= 23 & raw_score <= 24 ~ 47,
      raw_score >= 20 & raw_score <= 22 ~ 46,
      
      # 45 and just below
      raw_score >= 18 & raw_score <= 19 ~ 45,
      raw_score >= 16 & raw_score <= 17 ~ 44,
      raw_score >= 13 & raw_score <= 15 ~ 43,
      raw_score >= 11 & raw_score <= 12 ~ 42,
      raw_score >= 8 & raw_score <= 10 ~ 41,
      
      # 40 and just below
      raw_score >= 5 & raw_score <= 7 ~ 40,
      raw_score >= 4 & raw_score <= 5 ~ 39,
      raw_score >= 1 & raw_score <= 3 ~ 38,
      raw_score == 0 ~ 37,
      
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    
    return(male_tscore)
  }
}

srs_teacher_raw$tscore_sci <- mapply(srs_teacher_convert_to_tscore_sci_school, 
                                     srs_teacher_raw$sci_raw, 
                                     srs_teacher_raw$sex)

srs_teacher_convert_to_tscore_rrb_school <- function(raw_score, gender) {
  # Input validation
  if (!is.numeric(raw_score)) {
    stop("Raw score must be numeric")
  }
  
  if (!gender %in% c("male", "female")) {
    stop("Gender must be either 'male' or 'female'")
  }
  
  if (gender == "female") {
    # Female conversion logic
    female_tscore <- case_when(
      
      raw_score >= 24 ~ 90,
      raw_score == 23 ~ 88,
      raw_score == 22 ~ 86,
      raw_score == 21 ~ 84,
      raw_score == 20 ~ 82,
      
      raw_score >= 19 ~ 90,
      raw_score == 18 ~ 89,
      raw_score == 17 ~ 86,
      raw_score == 16 ~ 84,
      raw_score == 15 ~ 81,
      
      raw_score == 14 ~ 79,
      raw_score == 13 ~ 76,
      raw_score == 12 ~ 74,
      raw_score == 11 ~ 71,
      raw_score == 10 ~ 68,
      
      raw_score == 9 ~ 66,
      raw_score == 8 ~ 63,
      raw_score == 7 ~ 61,
      raw_score == 6 ~ 58,
      raw_score == 5 ~ 56,
      
      raw_score == 4 ~ 53,
      raw_score == 3 ~ 51,
      raw_score == 2 ~ 48,
      raw_score == 1 ~ 45,
      raw_score == 0 ~ 43,
      
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(female_tscore)
    
  } else {
    # Male conversion logic (keeping the existing male table)
    male_tscore <- case_when(
      
      raw_score >= 28 ~ 90,
      raw_score == 27 ~ 89,
      raw_score == 26 ~ 87,
      raw_score == 25 ~ 86,
      raw_score == 24 ~ 84,
      raw_score == 23 ~ 82,
      
      raw_score == 22 ~ 80,
      raw_score == 21 ~ 79,
      raw_score == 20 ~ 77,
      raw_score == 19 ~ 75,
      raw_score == 18 ~ 73,
      raw_score == 17 ~ 72,
      
      raw_score == 16 ~ 70,
      raw_score == 15 ~ 68,
      raw_score == 14 ~ 66,
      raw_score == 13 ~ 65,
      raw_score == 12 ~ 63,
      raw_score == 11 ~ 61,
      
      raw_score == 10 ~ 59,
      raw_score == 9 ~ 58,
      raw_score == 8 ~ 56,
      raw_score == 7 ~ 54,
      raw_score == 6 ~ 52,
      raw_score == 5 ~ 51,
      
      raw_score == 4 ~ 49,
      raw_score == 3 ~ 47,
      raw_score == 2 ~ 45,
      raw_score == 1 ~ 44,
      raw_score == 0 ~ 42,
      
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    
    return(male_tscore)
  }
}

# Apply the function
srs_teacher_raw$tscore_rrb <- mapply(srs_teacher_convert_to_tscore_rrb_school, 
                                     srs_teacher_raw$mannerisms, 
                                     srs_teacher_raw$sex)

srs_teacher_convert_to_tscore_cog_school <- function(raw_score, gender) {
  # Input validation
  if (!is.numeric(raw_score)) {
    stop("Raw score must be numeric")
  }
  
  if (!gender %in% c("male", "female")) {
    stop("Gender must be either 'male' or 'female'")
  }
  
  if (gender == "male") {
    # Male conversion logic for Cognition (COG)
    tscore <- case_when(
      raw_score >= 29 ~ 90,
      raw_score == 28 ~ 88,
      raw_score == 27 ~ 86,
      raw_score == 26 ~ 84,
      raw_score == 25 ~ 83,
      raw_score == 24 ~ 81,
      raw_score == 23 ~ 79,
      raw_score == 22 ~ 77,
      raw_score == 21 ~ 76,
      raw_score == 20 ~ 74,
      raw_score == 19 ~ 73,
      raw_score == 18 ~ 70,
      raw_score == 17 ~ 69,
      raw_score == 16 ~ 68,
      raw_score == 15 ~ 65,
      raw_score == 14 ~ 63,
      raw_score == 13 ~ 62,
      raw_score == 12 ~ 60,
      raw_score == 11 ~ 58,
      raw_score == 10 ~ 45,
      raw_score == 9  ~ 55,
      raw_score == 8  ~ 53,
      raw_score == 7  ~ 51,
      raw_score == 6  ~ 49,
      raw_score == 5  ~ 48,
      raw_score == 4  ~ 46,
      raw_score == 3  ~ 44,
      raw_score == 2  ~ 42,
      raw_score == 1  ~ 41,
      raw_score == 0  ~ 37,
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(tscore)
    
  } else {
    # Female conversion logic for Cognition (COG)
    tscore <- case_when(
      raw_score >= 25 ~ 90,
      raw_score == 24 ~ 89,
      raw_score == 23 ~ 87,
      raw_score == 22 ~ 85,
      raw_score == 21 ~ 83,
      raw_score == 20 ~ 81,
      raw_score == 19 ~ 79,
      raw_score == 18 ~ 77,
      raw_score == 17 ~ 75,
      raw_score == 16 ~ 73,
      raw_score == 15 ~ 71,
      raw_score == 14 ~ 69,
      raw_score == 13 ~ 67,
      raw_score == 12 ~ 65,
      raw_score == 11 ~ 62,
      raw_score == 10 ~ 60,
      raw_score == 9  ~ 58,
      raw_score == 8  ~ 56,
      raw_score == 7  ~ 54,
      raw_score == 6  ~ 52,
      raw_score == 5  ~ 50,
      raw_score == 4  ~ 48,
      raw_score == 3  ~ 46,
      raw_score == 2  ~ 44,
      raw_score == 1  ~ 42,
      raw_score == 0  ~ 40,
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(tscore)
  }
}

srs_teacher_raw$tscore_cog <- mapply(srs_teacher_convert_to_tscore_cog_school, 
                                     srs_teacher_raw$cognition, 
                                     srs_teacher_raw$sex)



# Function to convert raw communication (COM) scores to T-scores for school-age teacher report (for males and females)
srs_teacher_convert_to_tscore_com_school <- function(raw_score, gender) {
  # Input validation
  if (!is.numeric(raw_score)) {
    stop("Raw score must be numeric")
  }
  
  if (!gender %in% c("male", "female")) {
    stop("Gender must be either 'male' or 'female'")
  }
  
  if (gender == "female") {
    # Female conversion logic for Communication (COM)
    tscore <- case_when(
      raw_score >= 41 ~ 90,
      raw_score == 40 ~ 88,
      raw_score == 39 ~ 87,
      raw_score == 38 ~ 86,
      raw_score == 37 ~ 85,
      raw_score == 36 ~ 84,
      raw_score == 35 ~ 82,
      raw_score == 34 ~ 81,
      raw_score == 33 ~ 80,
      raw_score == 32 ~ 79,
      raw_score == 31 ~ 77,
      raw_score == 30 ~ 76,
      raw_score == 29 ~ 75,
      raw_score == 28 ~ 74,
      raw_score == 27 ~ 73,
      raw_score == 26 ~ 71,
      raw_score == 25 ~ 70,
      raw_score == 24 ~ 69,
      raw_score == 23 ~ 68,
      raw_score == 22 ~ 67,
      raw_score == 21 ~ 65,
      raw_score == 20 ~ 64,
      raw_score == 19 ~ 63,
      raw_score == 18 ~ 62,
      raw_score == 17 ~ 60,
      raw_score == 16 ~ 59,
      raw_score == 15 ~ 58,
      raw_score == 14 ~ 57,
      raw_score == 13 ~ 56,
      raw_score == 12 ~ 54,
      raw_score == 11 ~ 53,
      raw_score == 10 ~ 52,
      raw_score == 9  ~ 51,
      raw_score == 8  ~ 50,
      raw_score == 7  ~ 48,
      raw_score == 6  ~ 47,
      raw_score == 5  ~ 46,
      raw_score == 4  ~ 45,
      raw_score == 3  ~ 43,
      raw_score == 2  ~ 42,
      raw_score == 1  ~ 41,
      raw_score == 0  ~ 40,
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(tscore)
    
  } else {
    # Male conversion logic for Communication (COM)
    tscore <- case_when(
      raw_score >= 56 ~ 90,
      raw_score == 55 ~ 89,
      raw_score == 54 ~ 88,
      raw_score == 53 ~ 87,
      raw_score >= 51 & raw_score <= 52 ~ 86,
      raw_score == 50 ~ 85,
      raw_score == 49 ~ 84,
      raw_score == 48 ~ 83,
      raw_score == 47 ~ 82,
      raw_score == 46 ~ 81,
      raw_score == 45 ~ 80,
      raw_score == 44 ~ 79,
      raw_score == 43 ~ 78,
      raw_score == 42 ~ 77,
      raw_score >= 40 & raw_score <= 41 ~ 76,
      raw_score == 39 ~ 75,
      raw_score == 38 ~ 74,
      raw_score == 37 ~ 73,
      raw_score == 36 ~ 72,
      raw_score == 35 ~ 71,
      raw_score == 34 ~ 70,
      raw_score == 33 ~ 69,
      raw_score == 32 ~ 68,
      raw_score == 31 ~ 67,
      raw_score == 30 ~ 66,
      raw_score >= 28 & raw_score <= 29 ~ 65,
      raw_score == 27 ~ 64,
      raw_score == 26 ~ 63,
      raw_score == 25 ~ 62,
      raw_score == 24 ~ 61,
      raw_score == 23 ~ 60,
      raw_score == 22 ~ 59,
      raw_score == 21 ~ 58,
      raw_score == 20 ~ 57,
      raw_score == 19 ~ 56,
      raw_score == 18 ~ 55,
      raw_score >= 16 & raw_score <= 17 ~ 54,
      raw_score == 15 ~ 53,
      raw_score == 14 ~ 52,
      raw_score == 13 ~ 51,
      raw_score == 12 ~ 50,
      raw_score == 11 ~ 49,
      raw_score == 10 ~ 48,
      raw_score == 9  ~ 47,
      raw_score == 8  ~ 46,
      raw_score == 7  ~ 45,
      raw_score == 6  ~ 44,
      raw_score >= 4 & raw_score <= 5 ~ 43,
      raw_score == 3  ~ 42,
      raw_score == 2  ~ 41,
      raw_score == 1  ~ 40,
      raw_score == 0  ~ 39,
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(tscore)
  }
}

srs_teacher_raw$tscore_com <- mapply(srs_teacher_convert_to_tscore_com_school, 
                                     srs_teacher_raw$communication, 
                                     srs_teacher_raw$sex)



# Function to convert raw awareness (AWR) scores to T-scores for school-age teacher report (for males and females)
srs_teacher_convert_to_tscore_awr_school <- function(raw_score, gender) {
  # Input validation
  if (!is.numeric(raw_score)) {
    stop("Raw score must be numeric")
  }
  
  if (!gender %in% c("male", "female")) {
    stop("Gender must be either 'male' or 'female'")
  }
  
  if (gender == "female") {
    # Female conversion logic for Awareness (AWR)
    tscore <- case_when(
      raw_score >= 17 ~ 90,
      raw_score == 16 ~ 86,
      raw_score == 15 ~ 83,
      raw_score == 14 ~ 80,
      raw_score == 13 ~ 76,
      raw_score == 12 ~ 73,
      raw_score == 11 ~ 70,
      raw_score == 10 ~ 67,
      raw_score == 9  ~ 64,
      raw_score == 8  ~ 61,
      raw_score == 7  ~ 57,
      raw_score == 6  ~ 54,
      raw_score == 5  ~ 51,
      raw_score == 4  ~ 48,
      raw_score == 3  ~ 45,
      raw_score == 2  ~ 41,
      raw_score == 1  ~ 38,
      raw_score == 0  ~ 35,
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(tscore)
    
  } else {
    # Male conversion logic for Awareness (AWR)
    tscore <- case_when(
      raw_score >= 21 ~ 90,
      raw_score == 20 ~ 89,
      raw_score == 19 ~ 87,
      raw_score == 18 ~ 84,
      raw_score == 17 ~ 81,
      raw_score == 16 ~ 78,
      raw_score == 15 ~ 76,
      raw_score == 14 ~ 73,
      raw_score == 13 ~ 70,
      raw_score == 12 ~ 66,
      raw_score == 11 ~ 64,
      raw_score == 10 ~ 62,
      raw_score == 9  ~ 59,
      raw_score == 8  ~ 56,
      raw_score == 7  ~ 53,
      raw_score == 6  ~ 51,
      raw_score == 5  ~ 47,
      raw_score == 4  ~ 45,
      raw_score == 3  ~ 42,
      raw_score == 2  ~ 40,
      raw_score == 1  ~ 37,
      raw_score == 0  ~ 33,
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(tscore)
  }
}

srs_teacher_raw$tscore_awr <- mapply(srs_teacher_convert_to_tscore_awr_school, 
                                     srs_teacher_raw$awareness, 
                                     srs_teacher_raw$sex)



# Function to convert raw motivation (MOT) scores to T-scores for school-age teacher report (for males and females)
srs_teacher_convert_to_tscore_mot_school <- function(raw_score, gender) {
  # Input validation
  if (!is.numeric(raw_score)) {
    stop("Raw score must be numeric")
  }
  
  if (!gender %in% c("male", "female")) {
    stop("Gender must be either 'male' or 'female'")
  }
  
  if (gender == "female") {
    # Female conversion logic for Motivation (MOT)
    tscore <- case_when(
      raw_score >= 24 ~ 90,
      raw_score == 23 ~ 89,
      raw_score == 22 ~ 87,
      raw_score == 21 ~ 85,
      raw_score == 20 ~ 83,
      raw_score == 19 ~ 80,
      raw_score == 18 ~ 78,
      raw_score == 17 ~ 76,
      raw_score == 16 ~ 74,
      raw_score == 15 ~ 72,
      raw_score == 14 ~ 70,
      raw_score == 13 ~ 67,
      raw_score == 12 ~ 65,
      raw_score == 11 ~ 63,
      raw_score == 10 ~ 61,
      raw_score == 9  ~ 59,
      raw_score == 8  ~ 57,
      raw_score == 7  ~ 54,
      raw_score == 6  ~ 52,
      raw_score == 5  ~ 50,
      raw_score == 4  ~ 48,
      raw_score == 3  ~ 46,
      raw_score == 2  ~ 44,
      raw_score == 1  ~ 42,
      raw_score == 0  ~ 39,
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(tscore)
    
  } else {
    # Male conversion logic for Motivation (MOT)
    tscore <- case_when(
      raw_score >= 29 ~ 90,
      raw_score == 28 ~ 89,
      raw_score == 27 ~ 87,
      raw_score == 26 ~ 85,
      raw_score == 25 ~ 84,
      raw_score == 24 ~ 82,
      raw_score == 23 ~ 80,
      raw_score == 22 ~ 78,
      raw_score == 21 ~ 76,
      raw_score == 20 ~ 75,
      raw_score == 19 ~ 73,
      raw_score == 18 ~ 71,
      raw_score == 17 ~ 69,
      raw_score == 16 ~ 67,
      raw_score == 15 ~ 66,
      raw_score == 14 ~ 64,
      raw_score == 13 ~ 62,
      raw_score == 12 ~ 60,
      raw_score == 11 ~ 59,
      raw_score == 10 ~ 57,
      raw_score == 9  ~ 55,
      raw_score == 8  ~ 53,
      raw_score == 7  ~ 51,
      raw_score == 6  ~ 50,
      raw_score == 5  ~ 48,
      raw_score == 4  ~ 46,
      raw_score == 3  ~ 44,
      raw_score == 2  ~ 42,
      raw_score == 1  ~ 41,
      raw_score == 0  ~ 39,
      TRUE ~ NA_real_  # Default case if no conditions are met
    )
    return(tscore)
  }
}

srs_teacher_raw$tscore_mot <- mapply(srs_teacher_convert_to_tscore_mot_school, 
                                     srs_teacher_raw$motivation, 
                                     srs_teacher_raw$sex)


# srs_teacher_tscore_df ---------------------------------------------------

srs_teacher_tscores <- srs_teacher_raw |> 
  mutate(informant = "teacher") |> 
  select(individual, sex, informant, tscore_sci, sci_raw, tscore_rrb, mannerisms, tscore_cog, tscore_com, tscore_awr, tscore_mot)

# srs_merge_parent_teacher ------------------------------------------------

srs_combined <- bind_rows(
  srs_parent_tscores %>%
    rename(
      sci_t = tscore_sci,
      rrb_t = tscore_rrb,
      rrb_raw = mannerisms
    ),
  srs_teacher_tscores %>%
    rename(
      mot_t = tscore_mot,
      com_t = tscore_com,
      cog_t = tscore_cog,
      awr_t = tscore_awr,
      sci_t = tscore_sci,
      rrb_t = tscore_rrb,
      rrb_raw = mannerisms
    )
) %>%
  arrange(individual, informant)

check <- srs_combined %>%
  group_by(individual) %>%
  summarise(
    n_informants = n_distinct(informant),
    has_both = n_distinct(informant) == 2
  )

# Print all summary statistics in one block
cat(sprintf("Summary of merged data:
Total rows: %d
Unique individuals: %d
Individuals with both informants: %d\n",
            nrow(srs_combined),
            n_distinct(srs_combined$individual),
            sum(check$has_both)))

describe(srs_combined)
# .- - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - -  ------------------------------------------

# rbsr------------------------------------- --------------------------------------------------------------------
ssc_rbsr <- ssc_rbsr %>%
  select(
    individual, 
    measure,
    status,
    i_stereotyped_behavior_score,
    ii_self_injurious_score,
    iii_compulsive_behavior_score,
    iv_ritualistic_behavior_score,
    v_sameness_behavior_score,
    vi_restricted_behavior_score,
    overall_score
  )

# Optional: verify the new structure
glimpse(ssc_rbsr)
# .- - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - -  ------------------------------------------

# merge everything ---------------------------------------------------------------
srs_combined <- bind_rows(
  srs_parent_tscores %>%
    select(individual, sex, informant, tscore_sci, sci_raw, tscore_rrb, mannerisms,tscore_awr, tscore_cog, tscore_com, tscore_mot) %>%
    left_join(
      srs_parent_df %>% select(individual, t_score),
      by = "individual"
    ),
  srs_teacher_tscores %>%
    select(individual, sex, informant, tscore_sci, sci_raw, tscore_rrb, mannerisms,tscore_awr, tscore_cog, tscore_com, tscore_mot) %>%
    left_join(
      srs_teacher_df %>% select(individual, t_score),
      by = "individual"
    )
) %>%
  pivot_wider(
    id_cols = individual,
    names_from = informant,
    values_from = c(tscore_sci, sci_raw, tscore_rrb, mannerisms, t_score,tscore_awr, tscore_cog, tscore_com, tscore_mot),
    names_prefix = "srs_"
  )

# Join the ADI scores with the result dataframe
result <- result %>%
  left_join(adi_df_interest, by = "individual")

final_combined <- result %>%
  # Step 1: ADOS measures
  select(
    # Basic information
    individual,
    sex,
    ados_module,
    algorithm,
    age_at_ados,
    
    # Core ADOS scores
    ados_social_affect,
    ados_restricted_repetitive,
    ados_css,
    
    # CSS scores
    "SA CSS",
    "RRB CSS"
  ) %>%
  
  # Step 2: ADI scores
  left_join(
    adi_df_interest %>%
      select(
        individual,
        # Lord algorithm scores
        soc_com_lord,
        rrb_lord,
        total_lord,
        
        # ADI DSM
        sc_dsm5,
        rrb_dsm5,
        total_dsm5,
        
        # ADI DSM ADJ
        sc_dsm5_adj,
        rrb_dsm5_adj,
        total_dsm5_adj,
        
        # Domain A: Social Interaction
        a1_failure_nonverbal_social,
        a2_failure_peer_relationships,
        a3_lack_shared_enjoy,
        a4_lack_socioemotional,
        a_total,
        
        # Domain B: Communication
        b1_delay_lang_gest,
        b2_fail_initiate_conversation,
        b3_idiosyncratic_speech,
        b4_lack_make_believe,
        b_verbal_total,
        b_non_verbal_total,
        
        # Domain C: Restricted/Repetitive Behaviors
        c1_encompassing_pattern,
        c2_compulsive_rituals,
        c3_repetitive_mannerisms,
        c4_preoccupation_material,
        c_total
      ),
    by = "individual"
  ) %>%
  
  # Step 3: Add the prepared SRS data
  left_join(srs_combined, by = "individual") %>%
  
  # Step 4: RBSR scores (ordered I to VI)
  left_join(
    ssc_rbsr %>%
      select(
        individual,
        # Ordered subscores I through VI
        i_stereotyped_behavior_score,
        ii_self_injurious_score,
        iii_compulsive_behavior_score,
        iv_ritualistic_behavior_score,
        v_sameness_behavior_score,
        vi_restricted_behavior_score,
        
        # Overall score
        overall_score
      ),
    by = "individual"
  )

# Rename columns with better organization and all ADI totals
# Rename columns with better organization and correct SRS names
ssc_publish <- final_combined %>%
  rename(
    # ADOS: total first, then subscales
    ados_css = ados_css,
    ados_sa = ados_social_affect,
    ados_rrb = ados_restricted_repetitive,
    ados_sa_css = "SA CSS",
    ados_rrb_css = "RRB CSS",
    
    # ADI: all totals first, then subscales
    adi_total_edx = total_lord,
    adi_social_edx = soc_com_lord,
    adi_rrb_edx = rrb_lord,
    adi_a_total = a_total,
    adi_b_verbal_total = b_verbal_total,
    adi_b_nonverbal_total = b_non_verbal_total,
    adi_c_total = c_total,
    
    # Individual ADI items
    adi_a1 = a1_failure_nonverbal_social,
    adi_a2 = a2_failure_peer_relationships,
    adi_a3 = a3_lack_shared_enjoy,
    adi_a4 = a4_lack_socioemotional,
    
    adi_b1 = b1_delay_lang_gest,
    adi_b2 = b2_fail_initiate_conversation,
    adi_b3 = b3_idiosyncratic_speech,
    adi_b4 = b4_lack_make_believe,
    
    adi_c1 = c1_encompassing_pattern,
    adi_c2 = c2_compulsive_rituals,
    adi_c3 = c3_repetitive_mannerisms,
    adi_c4 = c4_preoccupation_material,
    
    # SRS: total first, then subscales (with correct original names)
    srs_total_parent = t_score_srs_parent,
    srs_social_parent = tscore_sci_srs_parent,
    srs_rrb_parent = tscore_rrb_srs_parent,
    
    srs_total_teacher = t_score_srs_teacher,
    srs_social_teacher = tscore_sci_srs_teacher,
    srs_rrb_teacher = tscore_rrb_srs_teacher,
    
    # RBSR: total first, then subscales with roman numerals
    rbsr_total = overall_score,
    rbsr_stereotypy = i_stereotyped_behavior_score,
    rbsr_self_injurious = ii_self_injurious_score,
    rbsr_compulsive = iii_compulsive_behavior_score,
    rbsr_ritualistic = iv_ritualistic_behavior_score,
    rbsr_sameness = v_sameness_behavior_score,
    rbsr_restricted = vi_restricted_behavior_score
  ) %>%
  # Remove SRS raw scores and mannerisms
  select(-matches("(sci_raw|mannerisms)_srs_"))


list(
  "ADOS" = ssc_publish %>% select(ados_sa, ados_rrb, ados_css, ados_sa_css, ados_rrb_css),
  "ADI" = ssc_publish %>% select(adi_total_edx, adi_social_edx, adi_rrb_edx, 
                                 adi_a_total, adi_b_verbal_total, adi_b_nonverbal_total, adi_c_total),
  "SRS Parent" = ssc_publish %>% select(srs_total_parent, srs_social_parent, srs_rrb_parent),
  "SRS Teacher" = ssc_publish %>% select(srs_total_teacher, srs_social_teacher, srs_rrb_teacher),
  "RBSR" = ssc_publish %>% select(rbsr_total, rbsr_stereotypy, rbsr_self_injurious, rbsr_compulsive, rbsr_ritualistic, rbsr_sameness, rbsr_restricted)
) %>% 
  lapply(function(x) {
    desc_stats <- psych::describe(x, na.rm = TRUE)[,c("n", "mean", "sd", "median", "min", "max", "range", "skew")]
    desc_stats$percent <- round(desc_stats$n / nrow(ssc_publish) * 100, 1)
    desc_stats[, c("n", "percent", "mean", "sd", "median", "min", "max", "range", "skew")]  # reorder columns to put percent after n
  }) %>%
  print()

# .- - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - -  ------------------------------------------

plot_all_distributions(ssc_publish)
# plot_all-------------------------------- --------------------------------------------------------------------
trace(corrplot, edit=TRUE)
# Function to create corrplot with adjusted significance labels

# HERE IS WHERE I press for the core plot function
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
              number.cex = 0.7,
              outline = TRUE)
  
  title(title)
}

ssc_plot <- ssc_publish %>% 
    rename(
    "SRS Total Parent" = srs_total_parent,
    "SRS Total Teacher" = srs_total_teacher,
    "ADI Total" = adi_total_edx,
    "ADOS Severity" = ados_css,
    "SRS Social Parent" = srs_social_parent,
    "SRS Social Teacher" = srs_social_teacher,
    "ADI Social" = adi_social_edx,
    "ADOS SA severity" = ados_sa_css,
    "RBSR Total" = rbsr_total,
    "SRS RRB Parent" = srs_rrb_parent,
    "SRS RRB Teacher" = srs_rrb_teacher,
    "ADI RRB" = adi_rrb_edx,
    "ADOS RRB severity" = ados_rrb_css)

# ADOS
create_styled_corrplot(
  ssc_publish %>% select(ados_sa, ados_rrb, ados_css, ados_sa_css, ados_rrb_css),
  ""
)
# ADOS
create_styled_corrplot(
  ssc_plot %>% select(`ADOS Severity`,`ADOS RRB severity`, `ADOS SA severity`),
  ""
)

# ADI edx
create_styled_corrplot(
  ssc_publish %>% select(adi_total_edx, adi_social_edx, adi_rrb_edx),
  "ADI edx Correlations"
)

# ADI totals
create_styled_corrplot(
  ssc_publish %>% select(adi_a_total, adi_b_verbal_total, adi_b_nonverbal_total, adi_c_total),
  "ADI totals Correlations"
)

# SRS Parent
create_styled_corrplot(
  ssc_publish %>% select(srs_total_parent, srs_social_parent, srs_rrb_parent,tscore_awr_srs_parent, tscore_cog_srs_parent, tscore_mot_srs_parent,tscore_com_srs_parent),
  ""
)

# SRS Teacher
create_styled_corrplot(
  ssc_publish %>% select(srs_total_teacher, srs_social_teacher, srs_rrb_teacher, tscore_awr_srs_teacher, tscore_cog_srs_teacher,tscore_mot_srs_teacher,tscore_com_srs_teacher),
  ""
)

# RBSR
create_styled_corrplot(
    ssc_publish %>% select(rbsr_total, rbsr_stereotypy, rbsr_self_injurious, rbsr_sameness, rbsr_compulsive, rbsr_restricted, rbsr_ritualistic),
  ""
)


# SRS PARENT
create_styled_corrplot(
  ssc_publish %>% select(srs_total_parent,  srs_social_parent,srs_rrb_parent,tscore_awr_srs_parent, tscore_cog_srs_parent, tscore_mot_srs_parent, tscore_com_srs_parent),
  ""
)

# SRS TEACHER
create_styled_corrplot(
  ssc_publish %>% select(srs_total_teacher, srs_social_teacher, srs_rrb_teacher,tscore_awr_srs_teacher, tscore_cog_srs_teacher, tscore_mot_srs_teacher, tscore_com_srs_teacher),
  "A. SRS Teacher"
)


# Cross-measure correlations
create_styled_corrplot(
  ssc_publish %>% select(ados_css, adi_total_edx, 
                         srs_total_parent, srs_total_teacher, 
                         ),
  ""
)

# Create comprehensive correlation plot with 10 key variables
create_styled_corrplot(
  ssc_publish %>% 
    select(
      # Total/Overall scores
      srs_total_parent,    # SRS total parent
      srs_total_teacher,   # SRS total teacher 
      adi_total_edx,       # ADI total
      ados_css,            # ADOS severity
      
      # Social/Communication scores
      srs_social_parent,   # SRS sci parent
      srs_social_teacher,  # SRS sci teacher
      adi_social_edx,      # ADI soc_com
      ados_sa_css,         # ADOS sa severity
      
      # RRB/Related scores
      rbsr_total,          # RBSR total
      srs_rrb_parent,      # SRS rrb parent
      srs_rrb_teacher,     # SRS rrb teacher
      adi_rrb_edx,         # ADI rrb
      ados_rrb_css         # ADOS rrb
    ) %>%
    # Rename columns for clearer plotting
    rename(
      "SRS Total Parent" = srs_total_parent,
      "SRS Total Teacher" = srs_total_teacher,
      "ADI Total" = adi_total_edx,
      "ADOS Severity" = ados_css,
      "SRS Social Parent" = srs_social_parent,
      "SRS Social Teacher" = srs_social_teacher,
      "ADI Social" = adi_social_edx,
      "ADOS SA" = ados_sa_css,
      "RBSR Total" = rbsr_total,
      "SRS RRB Parent" = srs_rrb_parent,
      "SRS RRB Teacher" = srs_rrb_teacher,
      "ADI RRB" = adi_rrb_edx,
      "ADOS RRB" = ados_rrb_css
    ),
  "Cross-Measure Correlations of Key Variables"
)

# Create comprehensive correlation plot with 10 key variables
create_styled_corrplot(
  ssc_publish %>% 
    select(
      # Total/Overall scores
      ados_css,            # ADOS severity
      adi_total_edx,       # ADI total
      srs_total_parent,    # SRS total parent
      srs_total_teacher,   # SRS total teacher 
      
      # Social/Communication scores
      srs_social_parent,   # SRS sci parent
      srs_social_teacher,  # SRS sci teacher
      adi_social_edx,      # ADI soc_com
      ados_sa_css,         # ADOS sa severity
      
      # RRB/Related scores
      rbsr_total,          # RBSR total
      srs_rrb_parent,      # SRS rrb parent
      srs_rrb_teacher,     # SRS rrb teacher
      adi_rrb_edx,         # ADI rrb
      ados_rrb_css         # ADOS rrb
    ) %>%
    # Rename columns for clearer plotting
    rename(
      "SRS Total Parent" = srs_total_parent,
      "SRS Total Teacher" = srs_total_teacher,
      "ADI Total" = adi_total_edx,
      "ADOS Severity" = ados_css,
      "SRS Social Parent" = srs_social_parent,
      "SRS Social Teacher" = srs_social_teacher,
      "ADI Social" = adi_social_edx,
      "ADOS SA" = ados_sa_css,
      "RBSR Total" = rbsr_total,
      "SRS RRB Parent" = srs_rrb_parent,
      "SRS RRB Teacher" = srs_rrb_teacher,
      "ADI RRB" = adi_rrb_edx,
      "ADOS RRB" = ados_rrb_css
    ),
  "Cross-Measure Correlations of Key Variables"
)

# Create comprehensive correlation plot for totals
create_styled_corrplot(
  ssc_publish %>% 
    select(
      # Total/Overall scores
      ados_css,            # ADOS severity
      adi_total_edx,       # ADI total
      srs_total_parent,    # SRS total parent
      srs_total_teacher,   # SRS total teacher 
      
    ) %>%
    # Rename columns for clearer plotting
    rename(
      "SRS Total Parent" = srs_total_parent,
      "SRS Total Teacher" = srs_total_teacher,
      "ADI Total" = adi_total_edx,
      "ADOS Severity" = ados_css,
    ),
  ""
)

# Create comprehensive correlation plot for social
create_styled_corrplot(
  ssc_publish %>% 
    select(
     
      # Social/Communication scores
      ados_sa_css,         # ADOS sa severity
      adi_social_edx,      # ADI soc_com
      srs_social_parent,   # SRS sci parent
      srs_social_teacher,  # SRS sci teacher
      
          ) %>%
    # Rename columns for clearer plotting
    rename(
      "SRS Social Parent" = srs_social_parent,
      "SRS Social Teacher" = srs_social_teacher,
      "ADI Social" = adi_social_edx,
      "ADOS SA" = ados_sa_css,
         ),
  ""
)

# Create comprehensive correlation plot RRBs
create_styled_corrplot(
  ssc_publish %>% 
    select(
     
      # RRB/Related scores
      ados_rrb_css,         # ADOS rrb
      adi_rrb_edx,         # ADI rrb
      srs_rrb_parent,      # SRS rrb parent
      rbsr_total,          # RBSR total
      srs_rrb_teacher,     # SRS rrb teacher
      
    ) %>%
    # Rename columns for clearer plotting
    rename(
       "RBSR Total" = rbsr_total,
      "SRS RRB Parent" = srs_rrb_parent,
      "SRS RRB Teacher" = srs_rrb_teacher,
      "ADI RRB" = adi_rrb_edx,
      "ADOS RRB" = ados_rrb_css
    ),
  ""
)


# Create comprehensive correlation plot ADI
create_styled_corrplot(
  ssc_publish %>% 
    select(
      
      # RRB/Related scores
      adi_total_edx,         # ADOS rrb
      adi_social_edx,         # ADI rrb
      adi_rrb_edx,      # SRS rrb parent
      
    ) %>%
    # Rename columns for clearer plotting
    rename(
      "ADI total" = adi_total_edx,
      "ADI social-com" = adi_social_edx,
      "ADI RRB" = adi_rrb_edx,
    ),
  ""
)


# Create comprehensive correlation plot with 10 key variables
create_styled_corrplot(
  ssc_publish %>% 
    select(
      ados_css,            # ADOS severity
      ados_sa_css,         # ADOS sa severity
      ados_rrb_css,        # ADOS rrb
      adi_total_edx,       # ADI total
      adi_social_edx,      # ADI soc_com
      adi_rrb_edx,         # ADI rrb
      srs_total_parent,    # SRS total parent
      srs_social_parent,   # SRS sci parent
      srs_rrb_parent,      # SRS rrb parent
      rbsr_total,          # RBSR total
      srs_total_teacher,   # SRS total teacher 
      srs_social_teacher,  # SRS sci teacher
      srs_rrb_teacher,     # SRS rrb teacher
       
    ) %>%
    # Rename columns for clearer plotting
    rename(
      "SRS Total Parent" = srs_total_parent,
      "SRS Total Teacher" = srs_total_teacher,
      "ADI Total" = adi_total_edx,
      "ADOS Severity" = ados_css,
      "SRS Social Parent" = srs_social_parent,
      "SRS Social Teacher" = srs_social_teacher,
      "ADI Social" = adi_social_edx,
      "ADOS SA" = ados_sa_css,
      "RBSR Total" = rbsr_total,
      "SRS RRB Parent" = srs_rrb_parent,
      "SRS RRB Teacher" = srs_rrb_teacher,
      "ADI RRB" = adi_rrb_edx,
      "ADOS RRB" = ados_rrb_css
    ),
  ""
)



# plot srs ----------------------------------------------------------------
# ssc_long <- ssc_publish %>%
#   pivot_longer(
#     cols = c(srs_social_parent, srs_social_teacher,
#              srs_rrb_parent, srs_rrb_teacher),
#     names_to = c("measure", "informant"),
#     names_pattern = "srs_(.*)_(.*)",
#     values_to = "score"
#   ) %>%
#   mutate(
#     informant = factor(informant, levels = c("parent", "teacher")),
#     sex = factor(sex, levels = c("male", "female"))
#   )
# 
# # Calculate y-limits for each measure
# y_limits_social <- c(
#   floor(min(ssc_long$score[ssc_long$measure == "social"], na.rm = TRUE) - 3),
#   ceiling(max(ssc_long$score[ssc_long$measure == "social"], na.rm = TRUE) + 3)
# )
# y_limits_rrb <- c(
#   floor(min(ssc_long$score[ssc_long$measure == "rrb"], na.rm = TRUE) - 3),
#   ceiling(max(ssc_long$score[ssc_long$measure == "rrb"], na.rm = TRUE) + 3)
# )
# 
# # Plot for Social scores
# p1 <- ssc_long %>%
#   filter(measure == "social") %>%
#   ggplot(aes(x = sex, y = score, fill = informant)) +
#   geom_boxplot(alpha = 0.7, width = 0.7) +
#   scale_fill_brewer(palette = "Set2") +
#   scale_y_continuous(limits = y_limits_social) +
#   labs(
#     title = "Social Scores by Sex and Informant",
#     x = "Sex",
#     y = "Social Score",
#     fill = "Informant"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 12),
#     legend.position = "bottom",
#     plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
#   )
# 
# # Plot for RRB scores
# p2 <- ssc_long %>%
#   filter(measure == "rrb") %>%
#   ggplot(aes(x = sex, y = score, fill = informant)) +
#   geom_boxplot(alpha = 0.7, width = 0.7) +
#   scale_fill_brewer(palette = "Set2") +
#   scale_y_continuous(limits = y_limits_rrb) +
#   labs(
#     title = "RRB Scores by Sex and Informant",
#     x = "Sex",
#     y = "RRB Score",
#     fill = "Informant"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 12),
#     legend.position = "bottom",
#     plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
#   )
# 
# # Combine plots side by side
# combined_plot <- p1 + p2 +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "bottom")
# 
# # Add overall margin to the combined plot
# combined_plot + plot_annotation(
#   theme = theme(plot.margin = margin(2, 2, 2, 2))
# )
# 
# # testplots ---------------------------------------------------------------
# 
# # First, reshape the data
# ssc_long <- ssc_publish %>%
#   pivot_longer(
#     cols = c(srs_social_parent, srs_social_teacher,
#              srs_rrb_parent, srs_rrb_teacher),
#     names_to = c("measure", "informant"),
#     names_pattern = "srs_(.*)_(.*)",
#     values_to = "score"
#   ) %>%
#   mutate(
#     informant = factor(informant, levels = c("parent", "teacher")),
#     sex = factor(sex, levels = c("male", "female"))
#   )
# 
# # Statistical tests
# social_test <- ssc_long %>%
#   filter(measure == "social") %>%
#   rstatix::wilcox_test(score ~ sex, paired = FALSE) %>%
#   rstatix::add_significance()
# 
# rrb_test <- ssc_long %>%
#   filter(measure == "rrb") %>%
#   rstatix::wilcox_test(score ~ sex, paired = FALSE) %>%
#   rstatix::add_significance()
# 
# # Effect size calculations
# social_stats <- ssc_long %>%
#   filter(measure == "social") %>%
#   rstatix::wilcox_effsize(score ~ sex)
# 
# rrb_stats <- ssc_long %>%
#   filter(measure == "rrb") %>%
#   rstatix::wilcox_effsize(score ~ sex)
# 
# # Calculate y-limits
# y_limits_social <- c(
#   floor(min(ssc_long$score[ssc_long$measure == "social"], na.rm = TRUE) - 3),
#   ceiling(max(ssc_long$score[ssc_long$measure == "social"], na.rm = TRUE) + 8)
# )
# y_limits_rrb <- c(
#   floor(min(ssc_long$score[ssc_long$measure == "rrb"], na.rm = TRUE) - 3),
#   ceiling(max(ssc_long$score[ssc_long$measure == "rrb"], na.rm = TRUE) + 8)
# )
# 
# # Define color palettes
# inform_colors <- RColorBrewer::brewer.pal(3, "Set2")[1:2]
# sex_colors <- RColorBrewer::brewer.pal(3, "Set2")[2:3]
# 
# # Plot for Social scores
# p1 <- ssc_long %>%
#   filter(measure == "social") %>%
#   ggplot(aes(x = sex, y = score, fill = informant)) +
#   # Add density plots on margins with matching colors
#   ggside::geom_ysidedensity(
#     aes(fill = informant),
#     alpha = 0.7,
#     show.legend = FALSE
#   ) +
#   ggside::geom_xsidedensity(
#     aes(fill = sex),
#     alpha = 0.7,
#     show.legend = FALSE
#   ) +
#   geom_boxplot(alpha = 0.7, width = 0.7) +
#   scale_fill_brewer(palette = "Set2") +
#   scale_ysidex_continuous(breaks = NULL) +
#   scale_xsidey_continuous(breaks = NULL) +
#   scale_y_continuous(limits = y_limits_social) +
#   labs(
#     title = "Social Scores by Sex and Informant",
#     subtitle = sprintf(
#       "Wilcoxon test: W = %.1f, p = %.3f\nEffect size: %.2f (%s)",
#       social_test$statistic,
#       social_test$p,
#       social_stats$effsize,
#       social_stats$magnitude
#     ),
#     x = "Sex",
#     y = "Social Score",
#     fill = "Informant"
#   ) +
#   ggpubr::stat_compare_means(
#     aes(group = sex),
#     label = "p.signif",
#     method = "wilcox.test",
#     paired = FALSE
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 12),
#     plot.subtitle = element_text(hjust = 0.5, size = 10),
#     legend.position = "bottom",
#     plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
#     ggside.panel.scale = 0.3
#   ) +
#   ggside::theme_ggside_void()
# 
# # Plot for RRB scores
# p2 <- ssc_long %>%
#   filter(measure == "rrb") %>%
#   ggplot(aes(x = sex, y = score, fill = informant)) +
#   # Add density plots on margins with matching colors
#   ggside::geom_ysidedensity(
#     aes(fill = informant),
#     alpha = 0.7,
#     show.legend = FALSE
#   ) +
#   ggside::geom_xsidedensity(
#     aes(fill = sex),
#     alpha = 0.7,
#     show.legend = FALSE
#   ) +
#   geom_boxplot(alpha = 0.7, width = 0.7) +
#   scale_fill_brewer(palette = "Set2") +
#   scale_ysidex_continuous(breaks = NULL) +
#   scale_xsidey_continuous(breaks = NULL) +
#   scale_y_continuous(limits = y_limits_rrb) +
#   labs(
#     title = "RRB Scores by Sex and Informant",
#     subtitle = sprintf(
#       "Wilcoxon test: W = %.1f, p = %.3f\nEffect size: %.2f (%s)",
#       rrb_test$statistic,
#       rrb_test$p,
#       rrb_stats$effsize,
#       rrb_stats$magnitude
#     ),
#     x = "Sex",
#     y = "RRB Score",
#     fill = "Informant"
#   ) +
#   ggpubr::stat_compare_means(
#     aes(group = sex),
#     label = "p.signif",
#     method = "wilcox.test",
#     paired = FALSE
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 12),
#     plot.subtitle = element_text(hjust = 0.5, size = 10),
#     legend.position = "bottom",
#     plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
#     ggside.panel.scale = 0.3
#   ) +
#   ggside::theme_ggside_void()
# 
# # Combine plots side by side
# combined_plot <- p1 + p2 +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "bottom")
# 
# # Add overall margin to the combined plot
# combined_plot + plot_annotation(
#   theme = theme(plot.margin = margin(2, 2, 2, 2))
# )
# 

# sample_char -------------------------------------------------------------

summary_stats <- data.frame(
  Variable = c("Sex", "ADOS Module", "Algorithm"),
  Distribution = c(
    paste(names(table(ssc_publish$sex)), collapse="/"),
    paste(paste0("Module ", names(table(ssc_publish$ados_module))), collapse="/"),
    paste(names(table(ssc_publish$algorithm)), collapse="/")
  ),
  Frequencies = c(
    paste(table(ssc_publish$sex), collapse="/"),
    paste(table(ssc_publish$ados_module), collapse="/"),
    paste(table(ssc_publish$algorithm), collapse="/")
  ),
  Percentages = c(
    paste0(round(prop.table(table(ssc_publish$sex))*100, 1), "%", collapse="/"),
    paste0(round(prop.table(table(ssc_publish$ados_module))*100, 1), "%", collapse="/"),
    paste0(round(prop.table(table(ssc_publish$algorithm))*100, 1), "%", collapse="/")
  )
)

print(summary_stats)


describe(ssc_publish[,c("age_at_ados", "ados_sa", "ados_rrb", "ados_css", 
                        "adi_social_edx", "adi_rrb_edx", "adi_total_edx",
                        "adi_a_total", "adi_b_verbal_total", "adi_b_nonverbal_total", "adi_c_total",
                        "srs_total_parent", "rbsr_total", "srs_total_teacher")])

describeBy(ssc_publish[,c("age_at_ados", "ados_sa", "ados_rrb", "ados_css", 
                        "adi_social_edx", "adi_rrb_edx", "adi_total_edx",
                        "adi_a_total", "adi_b_verbal_total", "adi_b_nonverbal_total", "adi_c_total",
                        "srs_total_parent", "rbsr_total", "srs_total_teacher")],
           group = ssc_publish$sex)

#Ados by gender
describeBy(ssc_publish[,c("age_at_ados", "ados_sa", "ados_rrb", "ados_css")], 
           group = ssc_publish$sex)


#adi total by gender
describeBy(ssc_publish[,c("adi_a_total", "adi_b_verbal_total", "adi_b_nonverbal_total", "adi_c_total")], 
           group = ssc_publish$sex)


#adi edx by gender
describeBy(ssc_publish[,c("adi_social_edx", "adi_rrb_edx", "adi_total_edx")], 
           group = ssc_publish$sex)


#srs parent
describeBy(ssc_publish[,c("srs_total_parent", "srs_social_parent", "srs_rrb_parent")], 
           group = ssc_publish$sex)


#srs teacher
describeBy(ssc_publish[,c("srs_total_teacher", "srs_social_teacher", "srs_rrb_teacher")], 
           group = ssc_publish$sex)

#rbsr
describeBy(ssc_publish[,c("rbsr_total")], 
           group = ssc_publish$sex)

describe(ssc_publish)
describeBy(ssc_publish, group = ssc_publish$ados_module)
(results_char <- describeBy(ssc_publish, group = ssc_publish$sex))


# saving plots ------------------------------------------------------------

# # Create the directory first if it doesn't exist
# dir.create("C:/Users/Gal Or/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/MIND/plots110524", showWarnings = FALSE)
# 
# # Copy the files
# file.copy(
#   from = plots.png.paths, 
#   to = "C:/Users/Gal Or/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/MIND/plots110524",
#   recursive = TRUE
# )
# 
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# 
# file.copy(from=plots.png.paths, to= "C:/Users/Gal Or/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/MIND/plots110524")

# 1. t scores for subslaces teacher and parent srs #delayed

# whats left --------------------------------------------------------------
##Ages
# 1. t scores for subslaces teacher and parent srs #delayed
# 2  see how to code mutate ados/ssc algorithms to css with age groups. #based on pdf on main
# 3. in MIND best instance (slice ages above 61 -> withing them choose best visit)
# write_xlsx(ssc_publish, "ssc_publish180225_schoolage.xlsx")
#Then slice for males and for females:
# TO end with MIND: preschool/school age SSC Preschool/choolage
# End with MIND: Mind preschooM/Mind school , Mind preschoolF/Mind schoolF, SSC PreschoolM/schoolageM, SSC PreschoolF/choolageF



# plotting 25/02/25  -----------------------------------------------
# plot_all--------------------------------
# trace(corrplot, edit=TRUE)
# Function to create corrplot with adjusted significance labels
# 
# create_styled_corrplot <- function(data, title) {
#   # Save current graphics parameters and restore on exit
#   old_par <- par(no.readonly = TRUE)
#   on.exit(par(old_par), add = TRUE)
#   
#   # Define a custom plotting region that leaves extra space at the top
#   par(fig = c(0, 1, 0, 0.95))
#   
#   correlation_matrix <- cor(data, use = "pairwise.complete.obs")
#   pmat <- cor.mtest(data, use = "pairwise.complete.obs")
#   correlation_matrix[is.na(correlation_matrix)] <- 0
#   
#   # Create corrplot
#   corrplot(correlation_matrix,
#            method = "shade",
#            type = "lower",
#            p.mat = pmat$p,
#            order = "original",
#            addrect = 3,
#            col = colorRampPalette(c("blue", "white", "red"))(200),
#            insig = "label_sig",
#            sig.level = c(.001, .01, .05),
#            pch.cex = 1.4,
#            pch.col = "orange",
#            tl.col = "black",
#            tl.cex = 1,
#            addCoef.col = "black",
#            tl.srt = 45,
#            diag = TRUE,
#            number.cex = 1,
#            outline = TRUE)
#   
#   # Add title
#   title(title)
# }
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
  
  # Similarly for pch_cex (asterisk size)
  pch_cex <- 2.1571 - 0.1 * n_vars  # Slightly larger scale for asterisks
  
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
           pch.cex = pch_cex,      # Dynamic asterisk size
           tl.col = "black",
           tl.cex = 0.9,           # Fixed label size
           addCoef.col = "black",
           tl.srt = 45,
           diag = TRUE,
           number.cex = number_cex, # Dynamic number size
           outline = TRUE,
           mar = c(0,0,2,0))       # Internal margin for title
  
  # Calculate dynamic adjustment based on title length
  # For 3 chars: adj = 0.15, for 18 chars: adj = 0.03
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

pdf("ssc_school_26052025.pdf", width = 8, height = 6)

# 1. WITHIN-MEASURE CORRPLOTS
# 1.1 ADI edx: (using the computed EDx scores)
create_styled_corrplot(
  ssc_publish %>% 
    select(total_dsm5, sc_dsm5, rrb_dsm5) %>%
    setNames(c("ADI Total", "ADI Social Com", "ADI RRB")),
  "ADI DSM-5")

# 1.1c Compare ADI DSM-5 and DSM-5 Adj:
create_styled_corrplot(
  ssc_publish %>% 
    select(total_dsm5, sc_dsm5, rrb_dsm5,
           total_dsm5_adj, sc_dsm5_adj, rrb_dsm5_adj) %>%
    setNames(c("ADI DSM Total", "ADI DSM Social", "ADI DSM RRB",
               "ADI DSM Adj Total", "ADI DSM Adj Social", "ADI DSM Adj RRB")),
  "ADI DSM-5 vs. ADI DSM-5 Adjusted"
)

# 1.2 ADOS: (order: ADOS severity, ADOS RRB severity, ADOS SA severity)
create_styled_corrplot(
  ssc_publish %>% 
    select(ados_css, ados_sa_css, ados_rrb_css) %>%
    setNames(c("ADOS Severity", "ADOS SA", "ADOS RRB")),
  "ADOS")

# 1.3 SRS Parent: (using parent-rated SRS scores)
create_styled_corrplot(
  ssc_publish %>% 
    select(srs_total_parent, srs_social_parent, srs_rrb_parent) %>%
    setNames(c("SRS Total Parent", "SRS Social Parent", "SRS RRB Parent")),
  "SRS Parent"
)

# 1.3a SRS Parent with original labels:
create_styled_corrplot(
  ssc_publish %>% 
    select(srs_total_parent, srs_social_parent, srs_rrb_parent, 
           tscore_awr_srs_parent, tscore_cog_srs_parent, tscore_mot_srs_parent, tscore_com_srs_parent) %>%
    setNames(c("SRS Total Parent", "SRS Social Parent", "SRS RRB Parent",
               "SRS Awareness Parent", "SRS Cognition Parent", 
               "SRS Motivation Parent", "SRS Communication Parent")),
  "SRS Parent"
)

# 1.3b SRS Parent with simplified labels:
create_styled_corrplot(
  ssc_publish %>% 
    select(srs_total_parent, srs_social_parent, srs_rrb_parent, 
           tscore_awr_srs_parent, tscore_cog_srs_parent, tscore_mot_srs_parent, tscore_com_srs_parent) %>%
    setNames(c("SRS Total", "SRS Social", "SRS RRB", 
               "SRS Awareness", "SRS Cognition", 
               "SRS Motivation", "SRS Communication")),
  "SRS Parent"
)

# 1.4a SRS Teacher with original labels:
create_styled_corrplot(
  ssc_publish %>% 
    select(srs_total_teacher, srs_social_teacher, srs_rrb_teacher,
           tscore_awr_srs_teacher, tscore_cog_srs_teacher, tscore_mot_srs_teacher, tscore_com_srs_teacher) %>%
    setNames(c("SRS Total Teacher", "SRS Social Teacher", "SRS RRB Teacher",
               "SRS Awareness Teacher", "SRS Cognition Teacher", 
               "SRS Motivation Teacher", "SRS Communication Teacher")),
  "SRS Teacher"
)

# 1.4b SRS Teacher with simplified labels:
create_styled_corrplot(
  ssc_publish %>% 
    select(srs_total_teacher, srs_social_teacher, srs_rrb_teacher,
           tscore_awr_srs_teacher, tscore_cog_srs_teacher, tscore_mot_srs_teacher, tscore_com_srs_teacher) %>%
    setNames(c("SRS Total", "SRS Social", "SRS RRB", 
               "SRS Awareness", "SRS Cognition", 
               "SRS Motivation", "SRS Communication")),
  "SRS Teacher"
)


# 1.4 SRS Teacher: (using teacher-rated SRS scores)
create_styled_corrplot(
  ssc_publish %>% 
    select(srs_total_teacher, srs_social_teacher, srs_rrb_teacher) %>%
    setNames(c("SRS Total Teacher", "SRS Social Teacher", "SRS RRB Teacher")),
  "SRS Teacher"
)

# 1.5 RBSR: (using RBSR subscale scores)
create_styled_corrplot(
  ssc_publish %>% 
    select(rbsr_total, rbsr_stereotypy, rbsr_self_injurious, 
           rbsr_compulsive, rbsr_ritualistic, rbsr_sameness, rbsr_restricted) %>%
    setNames(c("RBS-R Total", "RBS-R Stereotypy", "RBS-R Self-Injurious", 
               "RBS-R Compulsive", "RBS-R Ritualistic", "RBS-R Sameness", "RBS-R Restricted")),
  "RBS-R"
)


# 2. ACROSS-MEASURES CORRPLOTS
# 2.1 Total Severity:
create_styled_corrplot(
  ssc_publish %>% 
    select(ados_css, total_dsm5, srs_total_parent, srs_total_teacher) %>%
    setNames(c("ADOS Severity", "ADI Total", "SRS Total Parent", 
               "SRS Total Teacher")),
  "Total Severity"
)

# 2.2 Social Communication:
create_styled_corrplot(
  ssc_publish %>% 
    select(ados_sa_css, sc_dsm5,  srs_social_parent, srs_social_teacher) %>%
    setNames(c("ADOS SA", "ADI Social Com", "SRS Social Parent", "SRS Social Teacher")),
  "Social Communication"
)

# 2.3 RRB:
create_styled_corrplot(
  ssc_publish %>% 
    select(ados_rrb_css, rrb_dsm5, srs_rrb_parent, rbsr_total, srs_rrb_teacher) %>%
    setNames(c("ADOS RRB", "ADI DSM RRB", "SRS RRB Parent", "RBS-R Total", "SRS RRB Teacher")),
  "RRB"
)
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
           number.cex = 0.7,
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

# 3. EVERYTHING:
create_styled_corrplot(
  ssc_publish %>% 
    select(
      # ADOS:
      ados_css, ados_sa_css, ados_rrb_css,
      # ADI DSM-5:
      total_dsm5, sc_dsm5, rrb_dsm5,
      # SRS Parent:
      srs_total_parent, srs_social_parent, srs_rrb_parent,
      # RBSR:
      rbsr_total,
      # SRS Teacher:
      srs_total_teacher, srs_social_teacher, srs_rrb_teacher
    ) %>%
    setNames(c(
      "ADOS Severity", "ADOS SA", "ADOS RRB",
      "ADI DSM Total", "ADI Social Com", "ADI RRB", 
      "SRS Total Parent", "SRS Social Parent", "SRS RRB Parent",
      "RBS-R Total",
      "SRS Total Teacher", "SRS Social Teacher", "SRS RRB Teacher"
    )),
  "All Measures"
)

dev.off()

# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# # 
# file.copy(from=plots.png.paths, to= "C:/Users/galor/OneDrive/Desktop/MIND/plots180225/plots 25.02.25/SSC_school_new_250225")
write_xlsx(ssc_publish, "ssc_publish_190525_school.xlsx")

ssc_publish %>% 
  distinct(individual, .keep_all = TRUE) %>% 
  summarise(Total = n(), Male = sum(sex == "male"), Female = sum(sex == "female"))


# SSC SCHOOL AGE Sample Characteristics
ssc_school_chars <- ssc_publish %>%
  select(
    # Demographics
    individual, sex, age_at_ados,
    # ADOS
    ados_css, ados_sa_css, ados_rrb_css,
    # ADI DSM (NOT Adj for school age)
    total_dsm5, sc_dsm5, rrb_dsm5,
    # SRS Parent
    srs_total_parent, srs_social_parent, srs_rrb_parent,
    tscore_awr_srs_parent, tscore_cog_srs_parent,
    tscore_mot_srs_parent, tscore_com_srs_parent,
    # SRS Teacher
    srs_total_teacher, srs_social_teacher, srs_rrb_teacher,
    tscore_awr_srs_teacher, tscore_cog_srs_teacher,
    tscore_mot_srs_teacher, tscore_com_srs_teacher,
    # RBS-R
    rbsr_total, rbsr_stereotypy, rbsr_self_injurious,
    rbsr_compulsive, rbsr_ritualistic, rbsr_sameness, rbsr_restricted
  ) %>%
  # Convert sex to numeric for easier calculation
  mutate(sex_numeric = case_when(
    sex == "male" ~ 1,
    sex == "female" ~ 0,
    TRUE ~ NA_real_
  ))

# Rename columns for cleaner output
ssc_school_chars_clean <- ssc_school_chars %>%
  rename(
    # Demographics
    Age = age_at_ados,
    # ADOS
    ADOS_Total = ados_css,
    ADOS_SA = ados_sa_css,
    ADOS_RRB = ados_rrb_css,
    # ADI DSM (not Adj)
    ADI_Total = total_dsm5,
    ADI_Social = sc_dsm5,
    ADI_RRB = rrb_dsm5,
    # SRS Parent
    SRS_Total_Parent = srs_total_parent,
    SRS_Social_Parent = srs_social_parent,
    SRS_RRB_Parent = srs_rrb_parent,
    SRS_Awareness_Parent = tscore_awr_srs_parent,
    SRS_Cognition_Parent = tscore_cog_srs_parent,
    SRS_Motivation_Parent = tscore_mot_srs_parent,
    SRS_Communication_Parent = tscore_com_srs_parent,
    # SRS Teacher
    SRS_Total_Teacher = srs_total_teacher,
    SRS_Social_Teacher = srs_social_teacher,
    SRS_RRB_Teacher = srs_rrb_teacher,
    SRS_Awareness_Teacher = tscore_awr_srs_teacher,
    SRS_Cognition_Teacher = tscore_cog_srs_teacher,
    SRS_Motivation_Teacher = tscore_mot_srs_teacher,
    SRS_Communication_Teacher = tscore_com_srs_teacher,
    # RBS-R
    RBSR_Total = rbsr_total,
    RBSR_Stereotypy = rbsr_stereotypy,
    RBSR_Self_Injurious = rbsr_self_injurious,
    RBSR_Compulsive = rbsr_compulsive,
    RBSR_Ritualistic = rbsr_ritualistic,
    RBSR_Sameness = rbsr_sameness,
    RBSR_Restricted = rbsr_restricted
  )

# Descriptive statistics with variable names preserved
ssc_school_desc <- describe(ssc_school_chars_clean %>% select(-individual, -sex, -sex_numeric))
ssc_school_desc_df <- as.data.frame(ssc_school_desc)
ssc_school_desc_df <- rownames_to_column(ssc_school_desc_df, var = "Variable")

# Gender distribution - using numeric for faster calculation
gender_stats <- ssc_school_chars %>%
  filter(!is.na(sex_numeric)) %>%
  summarise(
    Male_N = sum(sex_numeric == 1),
    Female_N = sum(sex_numeric == 0),
    Total = n(),
    Male_Percent = round(sum(sex_numeric == 1) / n() * 100, 1),
    Female_Percent = round(sum(sex_numeric == 0) / n() * 100, 1)
  )

# Gender distribution table
ssc_school_gender <- data.frame(
  sex = c("male", "female"),
  n = c(gender_stats$Male_N, gender_stats$Female_N),
  percentage = c(gender_stats$Male_Percent, gender_stats$Female_Percent)
)

# Summary
ssc_school_summary <- data.frame(
  Sample = "SSC School Age",
  N = nrow(ssc_school_chars),
  Age_Mean = mean(ssc_school_chars$age_at_ados, na.rm=TRUE),
  Age_SD = sd(ssc_school_chars$age_at_ados, na.rm=TRUE),
  Age_Min = min(ssc_school_chars$age_at_ados, na.rm=TRUE),
  Age_Max = max(ssc_school_chars$age_at_ados, na.rm=TRUE),
  Male_N = gender_stats$Male_N,
  Female_N = gender_stats$Female_N,
  Male_Percent = gender_stats$Male_Percent,
  Female_Percent = gender_stats$Female_Percent
)

# Export
write_xlsx(list(
  "Summary" = ssc_school_summary,
  "Descriptive_Stats" = ssc_school_desc_df,
  "Gender_Distribution" = ssc_school_gender
), "SSC_School_Characteristics.xlsx")

# Print summary to console
cat("SSC School Age Sample Characteristics:\n")
print(ssc_school_summary)
cat("\nGender Distribution:\n")
print(ssc_school_gender)
cat("\nFirst few variables:\n")
print(head(ssc_school_desc_df))
