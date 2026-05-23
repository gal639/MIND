# boot --------------------------------------------------------------------

boot <- if (!require("pacman")) install.packages("pacman")
pacman::p_load(qdapRegex, patchwork,coin, ggside, pacman, readxl, writexl, dplyr, tidyverse, gridExtra, openxlsx, ggsignif, lubridate, psych, corrplot, janitor, purrr)

# functions ---------------------------------------------------------------


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
# 
# filter_and_replace <- function(df, blacklist_cols = NULL) {
#   # Identify columns to check by excluding the blacklist columns
#   cols_to_check <- setdiff(names(df), blacklist_cols)
#   
#   df %>%
#     mutate(across(all_of(cols_to_check), ~ case_when(
#       . %in% c(7, 8, 9) | . < 0 ~ NA,  # Replace 8, 9, or any negative number with NA
#       TRUE ~ .  # Preserve original values
#     )))
# }


blacklist_cols <- c("id","origin",
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

# descriptives raw ------------------------------------------------------------

print(get_descriptives(visit_df_processed), n=length(visit_df_processed))
# plot_all_distributions(visit_df_processed) #This might be long to run.

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


describe(interest)
print(get_descriptives(interest), n=length(interest))
plot_all_distributions(interest)  #This might take some time

# variables_to_plot <- c("adi_totalA", "adi_totalC","adi_totalNVB","adi_totalVB")
# plot_selected_distributions(mastermind, variables_to_plot)

# gaps in some first ados: sumA != scoresum total. eg: 103126-100.


# missing id's from items to mastermind --------------------------------------------------------

# 
# dataframe_list <- list(adg_mod1, adg_mod2, adg_mod3, ad2_mod1, ad2_mod2, ad2_mod3)
# dataframe_names <- c("adg_mod1", "adg_mod2", "adg_mod3", "ad2_mod1", "ad2_mod2", "ad2_mod3")
# filters <- data.frame(ados_version = c("ADOSG", "ADOSG", "ADOSG", "ADOS2", "ADOS2", "ADOS2"),
#                       ados_module = c("module 1", "module 2", "module 3", "module 1", "module 2", "module 3"))
# 
# compare_dataframes <- function(mastermind, df, ados_version, ados_module, df_name = NULL) {
#   df %>%
#     anti_join(mastermind %>%
#                 filter(ados_version == !!ados_version & ados_module == !!ados_module),
#               by = "id") %>%
#     select(id) %>%
#     mutate(originates = df_name)
# }
# 
# missing_in_master <- lapply(seq_along(dataframe_list), function(i) {
#   compare_dataframes(mastermind, dataframe_list[[i]], filters$ados_version[i], filters$ados_module[i], dataframe_names[i])
# })
# 
# missing_df <- bind_rows(missing_in_master)
# 
# missing_df <- missing_df %>%
#   distinct(id, .keep_all = TRUE)

#Missing_df are participants that are present in ITEM level but are abscent in MIND_dataset_2024-05-29
#Some are missing more than once, missing_df shows the EARLIEST instance
#If id is missing on visit 1 and visit 3, only visit 1 is shown at missing_df

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

# adi_duplicates <- adi_r_1 %>% #find duplicates
#   group_by(id) %>%
#   filter(n() > 1) %>%
#   arrange(id)
# 
# adi_dup_count <- adi_r_1 %>% #check for # of visits for each
#   group_by(id) %>%
#   summarise(count = n()) %>%
#   filter(count > 1) %>%
#   arrange(desc(count))

adi_r_1_filter <- adi_r_1 %>% 
  inner_join(interest %>% select(id, visit), by = c("id", "visit"))

# adi_r_1_filter <- adi_r_1 #need to address which cols are not replaced
# #a1a2a3a4->c1c2c3c4 should not get filtered


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
         Total_D, soc_com_lord, rrb_lord, Total_LORD, )

# plot_all_distributions(adi_r_1_interest)

adi_ados_merge <- full_join(interest, adi_r_1_interest[, c("visit", "id", "is.verbal",
                                                    "A1", "A2", "A3", "A4", "Total_A",
                                                    "B1", "B4", "B2V", "B3V", "Total_BV", "Total_NVB",
                                                    "C1", "C2", "C3", "C4", "Total_C",
                                                    "Total_D", "soc_com_lord", "rrb_lord", "Total_LORD")], by = "id") #we lose some ID's that do not have ADOS for example 113807-100 # we want both even if they miss some ados


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

# plot_all_distributions(adi_ados_merge)

adi_selected_plot <- adi_ados_merge %>%
  select(adi_totalA, adi_totalVB, adi_totalNVB, adi_totalNVB, soc_com_lord, Total_LORD, rrb_lord)

# plot_all_distributions(adi_selected_plot)


plot_all_distributions(adi_selected_plot)


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
           number.cex = 0.7,
           outline = TRUE)
  
  title(title)
}



# ilan plts ---------------------------------------------------------------
library(ggplot2)

create_styled_scatterplot <- function(data, title = "Scatterplot with Regression and Jitter") {
  # Validate that the data has at least two columns
  if (ncol(data) < 2) {
    stop("Data must contain at least two variables.")
  }
  
  # Extract the first two column names as x and y variables
  vars <- names(data)[1:2]
  x_var <- vars[1]
  y_var <- vars[2]
  
  # Compute Pearson correlation and its p-value
  cor_res <- cor.test(data[[x_var]], data[[y_var]], method = "pearson")
  correlation_coef <- cor_res$estimate
  p_value <- cor_res$p.value
  
  # Append correlation text to the title
  full_title <- sprintf("%s r = %.2f, p = %.3f", title, correlation_coef, p_value)
  
  # Create the jittered scatterplot with a regression line
  p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_jitter(color = "#2c7fb8", size = 3, alpha = 0.7, width = 0.2, height = 0.2) +
    geom_smooth(method = "lm", color = "black", fill = "gray", se = TRUE) +  # regression line in black
    labs(title = full_title,
         x = x_var,
         y = y_var) +
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      # Remove grid lines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  # Print the plot
  print(p)
}


create_styled_scatterplot(
  adi_ados_plot2 %>% select(ados_css, adi_total),
  ""
)

#social
create_styled_scatterplot(
  adi_ados_plot2 %>% select(ados_sa_css, adi_sa),
  ""
)

#rrb
create_styled_scatterplot(
  adi_ados_plot2 %>% select(ados_rrb_css, adi_rrb),
  ""
)


# prescool plots 180225 ---------------------------------------------------

# 1. WITHIN-MEASURE CORRPLOTS

# 1.1 ADI EDX:
create_styled_corrplot(
  adi_ados_plot %>% select(`ADI total`, `ADI social-com`, `ADI RRB`),
  "")

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

# oldplots ----------------------------------------------------------------


create_styled_corrplot(
  adi_ados_plot %>% select(`ADI total`, `ADI social-com`,`ADOS severity`,`ADOS SA severity`),
  ""
)

create_styled_corrplot(
  adi_ados_plot %>% select(`ADI RRB`,`ADOS RRB severity`),
  ""
)

create_styled_corrplot(
  adi_ados_plot %>% select(`ADI total`, `ADI social-com`, `ADI RRB`),
  ""
)

create_styled_corrplot(
  adi_ados_plot %>% select(`ADI total`, `ADI social-com`, `ADI RRB`),
  ""
)

create_styled_corrplot(
  adi_ados_plot %>% select(`ADOS SA severity`, `ADI social-com`, `SRS SCI parent`),
  ""
)

create_styled_corrplot(
  adi_ados_plot %>% select(`ADOS RRB severity`, `ADI RRB`, `SRS RRB parent`,`RBSR total` ),
  ""
)

create_styled_corrplot(
  adi_ados_plot %>% select(`ADOS severity`, `ADI total`, `SRS total parent`),
  ""
)

create_styled_corrplot(
  adi_ados_plot %>% select(`SRS total parent`, `SRS SCI parent`, `SRS RRB parent`, `srs_awr_t`, `srs_cog_t`,  srs_mot_t, srs_com_t),
  ""
)

create_styled_corrplot(
  adi_ados_plot %>% select(`ADOS severity`, `ADOS SA severity`, `ADOS RRB severity`, `ADI total`, `ADI social-com`,`ADI RRB`, `SRS total parent`, `SRS SCI parent`, `SRS RRB parent`, `RBSR total`),
  ""
)
#the following uses the our own calculations and not MIND

correlation_matrix <- cor(adi_r_1_interest[ ,c("A1", "A2", "A3", "A4", "Total_A",
                                               "B1", "B4", "B2V", "B3V", "Total_BV",
                                               "C1", "C2", "C3", "C4", "Total_C",
                                               "Total_D", "soc_com_lord", "rrb_lord" )], use = "pairwise.complete.obs")

p.mat <- cor.mtest(adi_r_1_interest[ ,c("A1", "A2", "A3", "A4", "Total_A",
                                        "B1", "B4", "B2V", "B3V","Total_BV",
                                        "C1", "C2", "C3", "C4", "Total_C",
                                        "Total_D", "soc_com_lord", "rrb_lord" )])

corrplot(correlation_matrix, method = "color", type = "lower", order = "original",
         p.mat = p.mat$p, # matrix of p-values
         sig.level = c(.001, .01, .05), # significance level for displaying asterisks
         insig = "blank", # do not show insignificant correlations
         addCoef.col = "black", # Add correlation coefficient on the plot
         tl.col="black", # Text label color
         tl.srt=90 # Text label rotation
)

correlation_matrix_NVB <- cor(adi_r_1_interest[ ,c("A1", "A2", "A3", "A4", "Total_A",
                                               "B1", "B4", "Total_NVB",
                                               "C1", "C2", "C3", "C4", "Total_C",
                                               "Total_D", "soc_com_lord", "rrb_lord", "Total_LORD")], use = "pairwise.complete.obs")

p.mat_NVB <- cor.mtest(adi_r_1_interest[ ,c("A1", "A2", "A3", "A4", "Total_A",
                                        "B1", "B4","Total_NVB",
                                        "C1", "C2", "C3", "C4", "Total_C",
                                        "Total_D","soc_com_lord", "rrb_lord", "Total_LORD")])

corrplot(correlation_matrix_NVB, method = "color", type = "lower", order = "original",
         p.mat = p.mat_NVB$p, # matrix of p-values
         sig.level = c(.001, .01, .05), # significance level for displaying asterisks
         insig = "blank", # do not show insignificant correlations
         addCoef.col = "black", # Add correlation coefficient on the plot
         tl.col="black", # Text label color
         tl.srt=90 # Text label rotation
)


# ADI COR
corrplot(correlation_matrix,
         method="shade",
         p.mat = p.mat$p,
         order = "hclust",
         addrect = 5,
         rect.col = "orange",
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.4,
         pch.col = "black",
         tl.col="black",
         tl.cex=1,
         addCoef.col = "black",
         tl.srt=90, # Text label rotation
         outline= T)

corrplot(correlation_matrix,
          method="shade",
          type="lower",
          p.mat = p.mat$p,
          order = "original",
          insig = "label_sig",
          sig.level = c(.001, .01, .05),
          pch.cex = 1.4,
          pch.col = "black",
          tl.col="black",
          tl.cex=1,
          addCoef.col = "black",
          tl.srt=90, # Text label rotation
          outline= T)

# ADI COR NVB
corrplot(correlation_matrix_NVB,
         method="shade",
         p.mat = p.mat_NVB$p,
         order = "hclust",
         addrect = 5,
         rect.col = "orange",
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.4,
         pch.col = "black",
         tl.col="black",
         tl.cex=1,
         addCoef.col = "black",
         tl.srt=90, # Text label rotation
         outline= T)

corrplot(correlation_matrix,
          method="shade",
          type="lower",
          p.mat = p.mat$p,
          order = "original",
          insig = "label_sig",
          sig.level = c(.001, .01, .05),
          pch.cex = 1.4,
          pch.col = "black",
          tl.col="black",
          tl.cex=1,
          addCoef.col = "black",
          tl.srt=90, # Text label rotation
          outline= T)



#create corplots for ADOS, RBS, SRS
#plot everything but for totals (ADOS: 1CSS, 1SA, 1RBS, 2JA, 2SUM A, 2SUMB, ADI: total 1A/1B/1C) #we dont need D in ADI

#1 for each test (with totals)
selected_columns <- c("ados_severity", "ados_rrb_severity", "ados_sa_severity",
                      "adi_totalA", "adi_totalVB", "adi_totalC", "soc_com_lord", "rrb_lord", "Total_LORD",
                      "srs_total_t", "srs_total_social_t", "rbs_6_factor_total",
                      "sumAalgorithm", "sumBalgorithm", "JA", "srs_rrb_t")

subset_data <- adi_ados_merge[ , selected_columns]

# # by type
# subset_data <- subset_data %>%
#   select(ados_severity, srs_total_t, ados_sa_severity, srs_total_social_t, 
#          adi_totalA, sumB, sumBalgorithm, adi_totalVB, sumA, sumAalgorithm, JA, adi_totalC, ados_rrb_severity, 
#          srs_rrb_t, rbs_6_factor_total)

# by test

subset_data <- subset_data %>%
  select(ados_severity, ados_sa_severity, sumAalgorithm,  sumBalgorithm, JA, ados_rrb_severity,
         adi_totalA, adi_totalVB, adi_totalC, soc_com_lord, rrb_lord, Total_LORD, 
         srs_total_t, srs_total_social_t, srs_rrb_t,
         rbs_6_factor_total)

correlation_matrix <- cor(subset_data, use = "pairwise.complete.obs")
pmat <- cor.mtest(subset_data, use = "pairwise.complete.obs")
correlation_matrix[is.na(correlation_matrix)] <- 0

# everything
corrplot(correlation_matrix,
         method="shade",
         type="lower",
         p.mat = pmat$p,
         order = "hclust",
         addrect = 3,
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.4,
         pch.col = "orange",
         tl.col="black",
         tl.cex=1,
         addCoef.col = "black",
         tl.srt=45, # Text label rotation
         diag = T,
         outline= T)

# everything clust
corrplot(correlation_matrix,
         order = "hclust",
         col = COL2("RdYlBu"),
         addrect = 4,
         p.mat=pmat$p,
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.2,
         pch.col = "orange",
         addCoef.col = "black",)

# everything order
corrplot(correlation_matrix,
         order = "original",
         col = COL2("RdYlBu"),
         type = "lower",
         p.mat=pmat$p,
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.2,
         diag = TRUE,
         tl.srt=45, # Text label rotation
         pch.col = "orange",
         addCoef.col = "black",)


#1 with ados: css, sa, rbs, 
ados_corr <- c("ados_severity", "ados_rrb_severity", "ados_sa_severity" )
ados_subset_data <- adi_ados_merge[ , ados_corr]
ados_correlation_matrix <- cor(ados_subset_data, use = "pairwise.complete.obs") #omit NA's and check power again
ados_pmat <- cor.mtest(ados_subset_data, use = "pairwise.complete.obs") #why is 0.82 not signif

corrplot(ados_correlation_matrix,
         method="shade",
         type="lower",
         col = colorRampPalette(c("blue", "white", "red"))(200), # Use a color ramp for the palette
         p.mat = ados_pmat$p,
         order = "original",
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.4,
         pch.col = "orange",
         tl.col="black",
         tl.cex=1,
   
         addCoef.col = "black",
         tl.srt=45, # Text label rotation
         diag = TRUE,
         outline= T)

#1 with adiNVB: totala, totalb, totalc, 
adi_corr <- c("adi_totalA", "adi_totalNVB", "adi_totalC", "soc_com_lord","rrb_lord","Total_LORD")
adi_subset_data <- adi_ados_merge[ , adi_corr]
adi_correlation_matrix <- cor(adi_subset_data, use = "pairwise.complete.obs")
adi_pmat <- cor.mtest(adi_subset_data, use = "pairwise.complete.obs")

corrplot(adi_correlation_matrix,
         method="shade",
         type="lower",
         p.mat = adi_pmat$p,
         order = "original",
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.4,
         pch.col = "black",
         tl.col="black",
         tl.cex=1,
         addCoef.col = "black",
         tl.srt=45, # Text label rotation
         diag = F,
         outline= T)

#ADI VB
adi_corr <- c("adi_totalA", "adi_totalVB", "adi_totalC", "soc_com_lord","rrb_lord","Total_LORD")
adi_subset_data <- adi_ados_merge[ , adi_corr]
adi_correlation_matrix <- cor(adi_subset_data, use = "pairwise.complete.obs")
adi_pmat <- cor.mtest(adi_subset_data, use = "pairwise.complete.obs")

corrplot(adi_correlation_matrix,
         method="shade",
         type="lower",
         p.mat = adi_pmat$p,
         order = "original",
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.4,
         pch.col = "black",
         tl.col="black",
         tl.cex=1,
         addCoef.col = "black",
         tl.srt=45, # Text label rotation
         diag = F,
         outline= T)


#visit 4 should get excluded.
#Plot total a b and c in ADI side-by-side

rbs_corr <- c("rbs_6_factor_total", "rbs_6_factor_compulsive", "rbs_6_factor_restricted", "rbs_6_factor_stereotypy",
              "rbs_6_factor_sameness", "rbs_6_factor_self_injurious", "rbs_6_factor_ritualistic")
rbs_subset_data <- adi_ados_merge[ , rbs_corr]
rbs_correlation_matrix <- cor(rbs_subset_data, use = "pairwise.complete.obs")
rbs_pmat <- cor.mtest(rbs_subset_data, use = "all")

adi_ados_plot <- adi_ados_merge %>% 
  rename(
    "RBS total" = rbs_6_factor_total,
    "RBS compulsive" = rbs_6_factor_compulsive,
    "RBS sameness" = rbs_6_factor_sameness,
    "RBS self-injurious" = rbs_6_factor_self_injurious,
    "RBS restricted" = rbs_6_factor_restricted,
    "RBS ritualistic" = rbs_6_factor_ritualistic,
    "RBS stereotypy" = rbs_6_factor_stereotypy
  )
corrplot(rbs_correlation_matrix,
         method="shade",
         type="lower",
         p.mat = rbs_pmat$p,
         col = colorRampPalette(c("blue", "white", "red"))(200), # Use a color ramp for the palette
         order = "original",
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.4,
         pch.col = "orange",
         tl.col="black",
         tl.cex=1,
         addCoef.col = "black",
         tl.srt=45, # Text label rotation
         diag = TRUE,
         outline= T)

create_styled_corrplot(
  adi_ados_plot %>% select(`RBS total`,`RBS stereotypy`, `RBS self-injurious`, `RBS sameness`, `RBS compulsive`, `RBS restricted`, `RBS ritualistic`),
  ""
)

trace(corrplot, edit = TRUE)

srs_corr <- c("srs_total_t","srs_sci_t", "srs_rrb_t","srs_awr_t", "srs_cog_t", "srs_com_t", "srs_mot_t")
              
srs_subset_data <- adi_ados_merge[ , srs_corr]
srs_correlation_matrix <- cor(srs_subset_data, use = "pairwise.complete.obs")
srs_pmat <- cor.mtest(srs_subset_data, use = "all")

corrplot(srs_correlation_matrix,
         col = colorRampPalette(c("blue", "white", "red"))(200), # Use a color ramp for the palette
         method="shade",
         type="lower",
         p.mat = srs_pmat$p,
         order = "original",
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.4,
         pch.col = "orange",
         tl.col="black",
         tl.cex=1,
         addCoef.col = "black",
         tl.srt=45, # Text label rotation
         diag = TRUE,
         outline= T)

trace(corrplot, edit=TRUE)
#filter NA's merge
#dataframe splicing for corrplots
#significance of correlations

# write_xlsx(adi_ados_merge, "092424_MIND_algorithm.xlsx")

# ADI NVB -----------------------------------------------------------------
mind_plot <- adi_ados_plot %>%
  rename(
    # ADI and ADOS measures
    "ADI Total" = Total_LORD,         # Assuming A domain captures social core features, sum might approximate total
    "ADI Social" = soc_com_lord,        # Domain A is social interaction domain in ADI
    "ADI RRB" = rrb_lord,           # Domain C is restricted/repetitive behaviors domain in ADI
    
    "ADOS Severity" = ados_severity,            # Overall ADOS severity (CSS)
    "ADOS SA severity" = ados_sa_severity,      # ADOS social-affect CSS
    "ADOS RRB severity" = ados_rrb_severity,    # ADOS RRB CSS
    
    # RBS total to RBSR total
    "RBSR Total" = `RBS total`,
    
    # SRS measures - original dataset doesn’t distinguish parent/teacher. 
    # If needed, assign them to one category (e.g., Teacher):
    "SRS Total Parent" = srs_total_t,
    "SRS Social Parent" = srs_sci_t,
    "SRS RRB Parent" = srs_rrb_t
  )

#1 for each test (with totals)
selected_columns <- c("ados_severity", "ados_rrb_severity", "ados_sa_severity",
                      "adi_totalA", "adi_totalNVB", "adi_totalC", "soc_com_lord", "rrb_lord", "Total_LORD",
                      "srs_total_t", "srs_total_social_t", "rbs_6_factor_total",
                      "sumAalgorithm", "sumBalgorithm", "JA", "srs_rrb_t", "srs_sci_t", "srs_rrb_t")

subset_data <- adi_ados_merge[ , selected_columns]

# by type
subset_data <- subset_data %>%
  select(ados_severity, srs_total_t, ados_sa_severity, srs_total_social_t, 
         adi_totalA, soc_com_lord, sumBalgorithm, adi_totalNVB, sumAalgorithm, JA, adi_totalC, ados_rrb_severity, rrb_lord, Total_LORD,
         srs_rrb_t, rbs_6_factor_total)

# by test

subset_data <- subset_data %>%
  select(ados_severity, ados_sa_severity, sumAalgorithm, sumBalgorithm, JA, ados_rrb_severity,
         adi_totalA, adi_totalNVB, adi_totalC, soc_com_lord, rrb_lord, Total_LORD,
         srs_total_t, srs_total_social_t, srs_rrb_t,
         rbs_6_factor_total)

correlation_matrix <- cor(subset_data, use = "pairwise.complete.obs")
pmat <- cor.mtest(subset_data, use = "pairwise.complete.obs")
correlation_matrix[is.na(correlation_matrix)] <- 0

# everything
corrplot(correlation_matrix,
         method="shade",
         type="lower",
         p.mat = pmat$p,
         order = "hclust",
         addrect = 3,
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.4,
         pch.col = "orange",
         tl.col="black",
         tl.cex=1,
         addCoef.col = "black",
         tl.srt=45, # Text label rotation
         diag = T,
         outline= T)

# everything clust
corrplot(correlation_matrix,
         order = "hclust",
         col = COL2("RdYlBu"),
         addrect = 4,
         p.mat=pmat$p,
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.2,
         pch.col = "orange",
         addCoef.col = "black",)

# everything order
corrplot(correlation_matrix,
         order = "original",
         col = COL2("RdYlBu"),
         type = "lower",
         p.mat=pmat$p,
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.2,
         tl.srt=45, # Text label rotation
         pch.col = "orange",
         addCoef.col = "black",)



#1 with adi: totala, totalb, totalc, 
adi_corr <- c("adi_totalA", "adi_totalNVB", "adi_totalC")
adi_subset_data <- adi_ados_merge[ , adi_corr]
adi_correlation_matrix <- cor(adi_subset_data, use = "pairwise.complete.obs")
adi_pmat <- cor.mtest(adi_subset_data, use = "pairwise.complete.obs")

corrplot(adi_correlation_matrix,
         method="shade",
         type="lower",
         p.mat = adi_pmat$p,
         order = "original",
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.4,
         pch.col = "black",
         tl.col="black",
         tl.cex=1,
         addCoef.col = "black",
         tl.srt=45, # Text label rotation
         diag = F,
         outline= T)


#corplot101424

subset_data <- adi_ados_merge %>%
  select(ados_severity, ados_sa_severity, ados_rrb_severity,
         soc_com_lord, rrb_lord, Total_LORD, 
         srs_total_t, srs_sci_t, srs_rrb_t,
         rbs_6_factor_total)


subset_data <- subset_data %>%
  rename(
    srs_total = srs_total_t,
    ADI_total = Total_LORD,
    ados_severity = ados_severity,
    
    srs_sci = srs_sci_t,
    adi_soc_com = soc_com_lord,
    ados_sa = ados_sa_severity,
    
    rbs_total = rbs_6_factor_total,
    srs_rrb = srs_rrb_t,
    adi_rrb = rrb_lord,
    ados_rrb = ados_rrb_severity
  ) %>%
  select(srs_total, adi_total, ados_css, srs_sci, adi_soc_com, ados_sa, rbs_total, srs_rrb, adi_rrb, ados_rrb)


correlation_matrix <- cor(subset_data, use = "pairwise.complete.obs")
pmat <- cor.mtest(subset_data, use = "pairwise.complete.obs")
correlation_matrix[is.na(correlation_matrix)] <- 0

# everything
corrplot(correlation_matrix,
         method = "shade",
         type = "lower",
         p.mat = pmat$p,
         order = "original",
         addrect = 3,
         col = colorRampPalette(c("blue", "white", "red"))(200), # Use a color ramp for the palette
         insig = "label_sig",
         sig.level = c(.001,.01,.05),
         pch.cex = 1.6,
         pch.col = "orange",
         tl.col = "black",
         tl.cex = 1,
         addCoef.col = "black",
         tl.srt = 45, # Text label rotation
         diag = TRUE,
         outline = TRUE)

# everything
corrplot(correlation_matrix,
         method="shade",
         type="lower",
         p.mat = pmat$p,
         order = "original",
         addrect = 3,
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.4,
         pch.col = "orange",
         tl.col="black",
         tl.cex=1,
         addCoef.col = "black",
         tl.srt=45, # Text label rotation
         diag = T,
         outline= T)

plot_all_distributions(subset_data)


# adi_lord ----------------------------------------------------------------

subset_data <- adi_ados_merge %>%
  select(soc_com_lord, rrb_lord, Total_LORD)

subset_data <- subset_data %>%
  rename(
    adi_soc_com = soc_com_lord,
    adi_rrb = rrb_lord,
    adi_total = Total_LORD,
  )
correlation_matrix <- cor(subset_data, use = "pairwise.complete.obs")
pmat <- cor.mtest(subset_data, use = "pairwise.complete.obs")
correlation_matrix[is.na(correlation_matrix)] <- 0

# everything
corrplot(correlation_matrix,
         col = colorRampPalette(c("blue", "white", "red"))(200), # Use a color ramp for the palette
         method="shade",
         type="lower",
         p.mat = pmat$p,
         order = "hclust",
         addrect = 3,
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.4,
         pch.col = "orange",
         tl.col="black",
         tl.cex=1,
         addCoef.col = "black",
         tl.srt=45, # Text label rotation
         diag = T,
         outline= T)
# t.test ------------------------------------------------------------------

t.test(JA ~ is.verbal, data = adi_ados_merge)

t.test(srs_total_social_t ~ is.verbal, data = adi_ados_merge)

ggplot(adi_ados_merge, aes(x = as.factor(is.verbal), y = srs_total_social_t, fill = as.factor(is.verbal))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +  # Boxplot without outliers, semi-transparent
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.6) +  # Jittered points
  stat_summary(fun = mean, geom = "point", shape = 22, size = 5, fill = "white") +  # Mean points
  labs(x = "Verbal Status", y = "Total Social T Score (SRS)", 
       title = "Comparison of SRS Total Social T Scores by Verbal Status",
       fill = "Verbal Status") +
  scale_x_discrete(drop = T, labels = c("0" = "Non-verbal", "1" = "Verbal")) +  # Custom x-axis labels
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Centered and bold title
    axis.title = element_text(size = 12),  # Axis titles size
    axis.text = element_text(size = 10),  # Axis text size
    legend.position = "none"  # Remove legend if not needed
  ) +
  geom_signif(comparisons = list(c("0", "1")), 
              map_signif_level = TRUE,
              textsize = 6,
              tip_length = 0.02,
              y_position = max(adi_ados_merge$srs_total_social_t, na.rm = TRUE) * 1.05) +
scale_x_discrete(drop = TRUE, limits = c("0", "1"))  # Drop the NA group from the x-axis



# saving plots ------------------------------------------------------------
# 
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

# file.copy(from=plots.png.paths, to= "C:/Users/Gal Or/OneDrive - post.bgu.ac.il/המרכז לאוטיזם/DATA/MIND/plots101424")


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
כרגע מה שצריך לעשות זה: 
  3. לנקות את הpre school mind: לפתור את הבעיה שגילינו עם ה11 ילדים האלו- להחליט על חתך גיל 5 או 6 (זה תלוי גם בssc כי הם צריכים להיות תואמים), וגם להעיף את שני הילדים בני ה11, להריץ מחדש את הגרפים והfa.
4. להריץ הכל על הschool age של ssc, הגרפים שהראת לי היום בעצם ואני צריכה קובץ לfa
5. להריץ הכל על school age של mind, כלומר לפתור את בעיית ה age


# results_char <- describeBy(adi_ados_chars, group = adi_ados_chars$sex)                    
# 
# 
# writexl::write_xlsx((results_char[["Male"]]), "mind_male110524.xlsx")
# writexl::write_xlsx((results_char[["Female"]]), "mind_Female110524.xlsx")
