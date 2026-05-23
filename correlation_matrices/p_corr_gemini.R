# -------------------------------------------------------------------------
# Phase 4: Correlation "Hub" Profiles (Line Plots for Connectivity)
# -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, patchwork,MASS)

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

# 2. Reshape Correlation Matrices to Long Format --------------------------
extract_corrs_long <- function(obj, filename) {
  parts <- str_split(filename, "_")[[1]]
  sex   <- tail(parts, 1)
  cohort <- paste(head(parts, -1), collapse = "_") 
  
  # Convert Matrix to Tibble
  # Row = "Hub" (The variable we focus on)
  # Col = "Partner" (The variable it correlates with)
  as_tibble(obj$corr, rownames = "hub") %>%
    pivot_longer(-hub, names_to = "partner", values_to = "r") %>%
    filter(hub != partner) %>% # Remove self-correlation (r=1)
    mutate(sex = sex, cohort = cohort)
}

# Create the Master Correlation Dataframe
big_corr <- imap_dfr(raw_list, \(obj, name) extract_corrs_long(obj, name))


# 3. Plotting Function: The "Hub" Profile ---------------------------------
# This creates one page per variable. 
# It answers: "How does [Hub Variable] talk to the rest of the brain?"

plot_hub_profile <- function(data, target_hub) {
  
  sub <- data %>% filter(hub == target_hub)
  
  ggplot(sub, aes(x = partner, y = r, group = sex, color = sex)) +
    # Reference line at 0 (No correlation)
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    
    # The Lines
    geom_line(size = 1.2, alpha = 0.8) +
    geom_point(size = 3) +
    
    # Facet by Cohort (MIND Pre, MIND School, etc.)
    facet_wrap(~cohort, ncol = 2) +
    
    scale_color_manual(values = c("Boys" = "blue3", "Girls" = "red3")) +
    scale_y_continuous(limits = c(-0.5, 1), breaks = seq(-0.4, 1, 0.2)) +
    
    labs(title = paste("Connectivity Profile:", target_hub),
         subtitle = paste("How", target_hub, "correlates with other symptoms"),
         y = "Pearson Correlation (r)", 
         x = NULL) +
    
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      panel.grid.minor = element_blank()
    )
}

big_corr

# Get list of all variables (Hubs)
all_vars <- unique(big_corr$hub)
all_vars <- unique(big_corr_partial$hub)

# Create a list of plots (One for ADOS, One for ADI, One for SRS, etc.)
list_hub_plots <- map(all_vars, ~plot_hub_profile(big_corr, .x))
names(list_hub_plots) <- all_vars


# 5. BONUS: The "Connectivity Strength" Profile ---------------------------
# Instead of specific correlations, this plots "How Central is this variable?"
# We calculate the Average Absolute Correlation (|r|) for each variable.

connectivity_strength <- big_corr %>%
  group_by(cohort, sex, hub) %>%
  summarise(mean_r = mean(abs(r), na.rm=TRUE)) %>%
  ungroup()
connectivity_strength

p_connectivity <- ggplot(connectivity_strength, aes(x = hub, y = mean_r, group = sex, color = sex)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 3) +
  facet_wrap(~cohort, ncol = 2) +
  scale_color_manual(values = c("Boys" = "blue3", "Girls" = "red3")) +
  labs(title = "Network Centrality Profile",
       subtitle = "Average strength of connection to the rest of the network (Mean |r|)",
       y = "Average Correlation Strength", x = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))


# 6. SAVE TO PDF ----------------------------------------------------------

# Open PDF
pdf("Correlation_Hub_Profiles.pdf", width = 12, height = 8)

# A. Print the "Summary" Connectivity plot first
message("Printing Network Centrality Profile...")
print(p_connectivity)

# B. Loop through every variable and print its Hub Profile
iwalk(list_hub_plots, \(plot, name) {
  message(paste("Printing Hub Profile:", name))
  print(plot)
})

# Close PDF
dev.off()

message("Done! Saved 'Correlation_Hub_Profiles.pdf'")
