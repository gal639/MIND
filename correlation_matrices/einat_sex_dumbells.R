
# Fisher r-to-z sex-difference visualisations  ----------------------------
# Author: ChatGPT (concise, modular, no external deps beyond tidyverse + patchwork)

# 1. libraries ------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, patchwork)

# 2. load correlation objects --------------------------------------------
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

# 3. helper: Fisher r-to-z -------------------------------------------------
fz <- function(r) 0.5*log((1+r)/(1-r))
fisher_test <- function(r1,n1,r2,n2){
  z <- (fz(r1)-fz(r2)) / sqrt(1/(n1-3)+1/(n2-3))
  p <- 2*pnorm(-abs(z))
  tibble(z_stat=z, p_value=p, diff=r1-r2)
}

# 4. crunch all pairs ------------------------------------------------------
make_results <- function(boys,girls,cohort){
  vars <- rownames(boys$corr)
  combs <- t(combn(vars,2))
  map_dfr(seq_len(nrow(combs)),\(i){
    v1<-combs[i,1]; v2<-combs[i,2]
    r1<-boys$corr[v1,v2]; r2<-girls$corr[v1,v2]
    n1<-boys$n[v1,v2];   n2<-girls$n[v1,v2]
    if(any(is.na(c(r1,r2)))) return(NULL)
    fisher_test(r1,n1,r2,n2) %>% 
      mutate(cohort=cohort,var1=v1,var2=v2,
             r_boys=r1,r_girls=r2)
  })
}

cohorts <- list(
  MIND_Preschool = c("MIND_Preschool_Boys","MIND_Preschool_Girls"),
  MIND_Schoolage = c("MIND_Schoolage_Boys","MIND_Schoolage_Girls"),
  SSC_Preschool  = c("SSC_Preschool_Boys","SSC_Preschool_Girls"),
  SSC_Schoolage  = c("SSC_Schoolage_Boys","SSC_Schoolage_Girls")
)

results <- imap_dfr(cohorts,\(fg,lab){
  make_results(data_list[[fg[1]]],data_list[[fg[2]]],lab)
}) %>%
  mutate(p_bonf = p.adjust(p_value,"bonferroni"),
         sig = p_bonf < .05)

# 5. plotting functions ----------------------------------------------------
# (1) scatter --------------------------------------------------------------
plot_scatter <- function(df){
  ggplot(df,aes(r_boys,r_girls,color=sig))+
    geom_abline(slope=1,intercept=0,linetype="dashed")+
    geom_point(alpha=.7)+
    facet_wrap(~cohort)+
    labs(x="r (Boys)",y="r (Girls)",color="Bonferroni\np<.05")+
    theme_bw()
}

# (2) heat-map -------------------------------------------------------------
plot_heat <- function(df,coh){
  sub <- filter(df,cohort==coh)
  vars <- unique(c(sub$var1,sub$var2))
  mat <- matrix(0,length(vars),length(vars),
                dimnames=list(vars,vars))
  apply(sub,1,\(row){
    mat[row["var1"],row["var2"]] <<- as.numeric(row["diff"])
    mat[row["var2"],row["var1"]] <<- as.numeric(row["diff"])
  })
  melt <- as_tibble(as.table(mat),.name_repair="minimal") %>%
    setNames(c("Var1","Var2","diff"))
  ggplot(melt,aes(Var1,Var2,fill=diff))+geom_tile()+
    scale_fill_gradient2(low="blue",mid="white",high="red")+
    labs(title=paste("Δr (B-G) —",coh),x=NULL,y=NULL)+
    theme(axis.text.x=element_text(angle=45,hjust=1),
          panel.background=element_blank())
}

# (3) volcano --------------------------------------------------------------
plot_volcano <- function(df){
  ggplot(df,aes(diff,-log10(p_value),color=sig))+
    geom_vline(xintercept=0,linetype="dotted")+
    geom_point(alpha=.7)+
    facet_wrap(~cohort)+
    labs(x="Δr (Boys−Girls)",y="−log10 p",color="Bonferroni\np<.05")+
    theme_bw()
}

# (4) density / histogram --------------------------------------------------
plot_density <- function(df){
  ggplot(df,aes(diff,fill=cohort))+geom_histogram(alpha=.5,bins=30)+
    geom_vline(xintercept=0)+
    labs(x="Δr (Boys−Girls)",y="Count")+
    theme_bw()
}

# (5) mean Δr bars ---------------------------------------------------------
plot_means <- function(df){
  df %>% group_by(cohort) %>% 
    summarise(mean_diff=mean(diff)) %>%
    ggplot(aes(cohort,mean_diff,fill=cohort))+
    geom_col(show.legend=FALSE)+
    geom_hline(yintercept=0)+
    coord_flip()+
    labs(y="Mean Δr (B-G)",x=NULL)+
    theme_bw()
}
# ---------------------------------------------------------
#  NEW: cohort-by-quadrant visual + top-Δr table
# ---------------------------------------------------------
pacman::p_load(ggrepel)

plot_quadrant <- function(df, cohort, top_n = 10){
  sub <- filter(df, cohort == {{cohort}})
  
  # flag for quadrant (helps with shading)
  sub <- sub %>%
    mutate(
      quadrant = case_when(
        r_boys >= 0 & r_girls >= 0 & r_boys >  r_girls ~ "Boys > Girls (+/+)",
        r_boys >= 0 & r_girls >= 0 & r_boys <= r_girls ~ "Girls ≥ Boys (+/+)",
        r_boys <  0 & r_girls <  0 & r_boys <  r_girls ~ "Boys < Girls (-/-)",
        r_boys <  0 & r_girls <  0 & r_boys >= r_girls ~ "Girls ≥ Boys (-/-)",
        r_boys >= 0 & r_girls <  0                        ~ "Opposite signs (+/−)",
        TRUE                                             ~ "Opposite signs (−/+)"
      )
    )
  
  p <- ggplot(sub, aes(r_boys, r_girls)) +
    # quadrant fills
    annotate("rect", xmin =  0, xmax =  1, ymin =  0, ymax =  1, fill = "grey95") +
    annotate("rect", xmin = -1, xmax =  0, ymin = -1, ymax =  0, fill = "grey95") +
    geom_abline(linetype = "dashed") +
    geom_point(aes(size = abs(diff), colour = sig), alpha = 0.7) +
    geom_text_repel(
      aes(label = paste0(substr(var1,1,4), "/", substr(var2,1,4))),
      size  = 3, max.overlaps = Inf, seed = 42
    ) +
    scale_size_continuous(name = "|Δr|") +
    scale_colour_manual(values = c(TRUE = "red3", FALSE = "black"),
                        name = "Bonferroni\np < .05") +
    labs(title = cohort,
         x = "r (Boys)",
         y = "r (Girls)") +
    theme_bw()
  
  # ---- quick numeric table (printed to console) ----
  cat("\n--- Top Δr for", cohort, "---\n")
  sub %>% 
    arrange(desc(abs(diff))) %>% 
    slice_head(n = top_n) %>% 
    select(var1, var2, r_boys, r_girls, diff, p_bonf) %>% 
    print()
  
  return(p)
}

# create one figure per cohort --------------------------------------------
plots_quadrant <- map(names(cohorts), ~plot_quadrant(results, .x))

# display   (wrap_plots needs patchwork already loaded)
wrap_plots(plots_quadrant, nrow = 2)


# 6. run & display ---------------------------------------------------------
scatter   <- plot_scatter(results)
volcano   <- plot_volcano(results)
density   <- plot_density(results)
means     <- plot_means(results)
heat_LIST <- map(names(cohorts),~plot_heat(results,.x))

# show a quick dashboard (feel free to comment any out)
(scatter | volcano) /
  (density | means) /
  wrap_plots(heat_LIST,nrow=1)







# take 2 ------------------------------------------------------------------




# -------------------------------------------------------------------------
#  Fisher r-to-z results  →  Δr bar-style visualisations (5 flavours)
# -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)

# 1. data -----------------------------------------------------------------
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
dat <- map(files, readRDS)   # list of objects

fz <- function(r) 0.5*log((1+r)/(1-r))
fisher <- function(r1,n1,r2,n2){
  z  <- (fz(r1)-fz(r2))/sqrt(1/(n1-3)+1/(n2-3))
  p  <- 2*pnorm(-abs(z))
  tibble(diff = r1-r2, p = p)
}

cohorts <- list(
  MIND_Preschool = c("MIND_Preschool_Boys","MIND_Preschool_Girls"),
  MIND_Schoolage = c("MIND_Schoolage_Boys","MIND_Schoolage_Girls"),
  SSC_Preschool  = c("SSC_Preschool_Boys","SSC_Preschool_Girls"),
  SSC_Schoolage  = c("SSC_Schoolage_Boys","SSC_Schoolage_Girls")
)

results <- imap_dfr(cohorts,\(fg,lab){
  boys <- dat[[fg[1]]]; girls <- dat[[fg[2]]]
  vars <- rownames(boys$corr)
  comb <- t(combn(vars,2))
  map_dfr(seq_len(nrow(comb)),\(i){
    v1<-comb[i,1]; v2<-comb[i,2]
    r1<-boys$corr[v1,v2]; r2<-girls$corr[v1,v2]
    n1<-boys$n[v1,v2];   n2<-girls$n[v1,v2]
    if(any(is.na(c(r1,r2)))) return(NULL)
    fisher(r1,n1,r2,n2) %>% 
      mutate(cohort=lab,
             pair = paste(v1,v2,sep=" ↔ "),
             r_boys=r1,r_girls=r2)
  })
}) %>%
  mutate(p_bonf = p.adjust(p,"bonferroni"),
         sig     = p_bonf < .05)

# helper for clean ordering inside facets
order_within <- function(x, by){ factor(x, levels = x[order(by)]) }

# 2. FIVE SIMPLE BAR-STYLE VISUALS ----------------------------------------

# extra helper -------------------------------------------------------------
pacman::p_load(tidytext, forcats)   # forcats already comes with tidyverse

# A. Classic horizontal bars (facet) --------------------------------------
plot_A <- ggplot(results,
                 aes(x = reorder_within(pair, diff, cohort),
                     y = diff,
                     fill = diff > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~cohort, scales = "free_y") +
  scale_x_reordered() +           # <<<<<<<<<<<<<< solves duplication
  coord_flip() +
  labs(title = "Δr  (Boys − Girls) — all pairs",
       x = NULL, y = "Difference in correlation") +
  scale_fill_manual(values = c("TRUE"="#0072B2","FALSE"="#D55E00")) +
  theme_bw()

# B. Only Bonferroni-significant bars -------------------------------------
plot_B <- results %>% filter(sig) %>% 
  ggplot(aes(reorder_within(pair, diff, cohort), diff, fill = diff > 0))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~cohort, scales = "free_y")+
  scale_x_reordered()+
  coord_flip()+
  labs(title = "Significant Δr  (Bonferroni p<.05)",
       y = "Difference in correlation", x = NULL)+
  scale_fill_manual(values = c("TRUE"="#009E73","FALSE"="#CC79A7"))+
  theme_bw()

# C. Dumbbell (girls vs boys) ---------------------------------------------
plot_C <- results %>% 
  ggplot(aes(y = reorder_within(pair, diff, cohort)))+
  geom_segment(aes(x = r_girls, xend = r_boys,
                   yend = reorder_within(pair, diff, cohort)),
               colour = "grey50")+
  geom_point(aes(x = r_girls), colour = "#E69F00", size = 2)+
  geom_point(aes(x = r_boys ), colour = "#56B4E9", size = 2)+
  facet_wrap(~cohort, scales = "free_y")+
  scale_y_reordered()+
  labs(title = "Dumbbell: r_girls (orange) vs r_boys (blue)",
       x = "Correlation", y = NULL)+
  theme_bw()

# D. Lollipop --------------------------------------------------------------
plot_D <- ggplot(results,
                 aes(x = diff,
                     y = reorder_within(pair, diff, cohort),
                     colour = diff > 0))+
  geom_segment(aes(xend = 0,
                   yend = reorder_within(pair, diff, cohort)),
               colour = "grey60")+
  geom_point(size = 2)+
  facet_wrap(~cohort, scales = "free_y")+
  scale_y_reordered()+
  labs(title = "Lollipop view of Δr",
       y = NULL, x = "Boys − Girls")+
  scale_colour_manual(values = c("TRUE"="#0072B2","FALSE"="#D55E00"))+
  theme_bw()+theme(legend.position = "none")

# E. Top-N bar per cohort --------------------------------------------------
topN <- 30
plot_E <- results %>%
  group_by(cohort) %>%
  slice_max(order_by = abs(diff), n = topN) %>%
  ungroup %>%
  ggplot(aes(reorder_within(pair, diff, cohort), diff, fill = diff > 0))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~cohort, scales = "free_y")+
  scale_x_reordered()+
  coord_flip()+
  labs(title = paste0("Top ", topN, " absolute Δr per cohort"),
       y = "Difference in correlation", x = NULL)+
  scale_fill_manual(values = c("TRUE"="#F0E442","FALSE"="#009E73"))+
  theme_bw()

# 3. DISPLAY --------------------------------------------------------------
print(plot_A)
print(plot_B)
print(plot_C)
print(plot_D)
print(plot_E)


# 3. DISPLAY --------------------------------------------------------------
print(plot_A)
print(plot_B)
print(plot_C)
print(plot_D)
print(plot_E)



# ─────────────────────────────────────────────────────────────────────────
#  EXTRA VISUALS: Boys shown first, tools stay in original order
# ─────────────────────────────────────────────────────────────────────────
pacman::p_load(tidyverse, patchwork)   # tidyverse already loaded, patchwork for layouts

# 0. PREP: long format + tool ordering ------------------------------------
# derive tool order from the first correlation matrix (row names)
tool_order <- rownames(data_list[[1]]$corr)          # assumes same order in all files

pair_levels <- results %>% 
  arrange(match(var1, tool_order), match(var2, tool_order)) %>% 
  distinct(pair) %>% 
  pull(pair)

long <- results %>% 
  pivot_longer(c(r_boys, r_girls),
               names_to = "sex", values_to = "r") %>% 
  mutate(
    sex  = recode(sex, r_boys = "Boys", r_girls = "Girls"),
    pair = factor(pair, levels = pair_levels)  # keeps tool-pair order
  )

# 1. GROUPED (DODGED) BARS -------------------------------------------------
plot1 <- ggplot(long,
                aes(pair, r, fill = sex))+
  geom_col(position = position_dodge(width = 0.75))+
  facet_wrap(~cohort, scales = "free_x")+
  coord_flip()+
  scale_fill_manual(values = c(Boys = "#56B4E9", Girls = "#E69F00"))+
  labs(title = "Grouped bars: Boys (blue) first, then Girls (orange)",
       x = NULL, y = "Correlation")+
  theme_bw()

# 2. SLOPEGRAPH  (x-axis = sex) -------------------------------------------
plot2 <- ggplot(long,
                aes(sex, r, group = pair, colour = pair))+
  geom_line(alpha = .4)+
  geom_point(size = 2)+
  facet_wrap(~cohort)+
  scale_x_discrete(limits = c("Boys","Girls"))+  # boys column left
  labs(title = "Slopegraph: Each line = tool pair",
       x = NULL, y = "Correlation")+
  theme_bw()+theme(legend.position = "none")

# 3. DUMBBELL  (pairs on y, dots left/right) ------------------------------
plot3 <- ggplot(long,
                aes(x = r,
                    y = pair,
                    colour = sex))+
  geom_line(aes(group = pair), colour = "grey60")+
  geom_point(size = 2)+
  facet_wrap(~cohort, scales = "free_y")+
  scale_colour_manual(values = c(Boys = "#56B4E9", Girls = "#E69F00"))+
  labs(title = "Dumbbell: Boys (blue) vs Girls (orange)",
       y = NULL, x = "Correlation")+
  theme_bw()

# 4. SIDE-BY-SIDE HEATMAPS  (boys left, girls right) ----------------------
plot_heat <- function(cohort_nm){
  sub <- filter(long, cohort == cohort_nm)
  ggplot(sub, aes(var1, var2, fill = r))+
    geom_tile()+
    facet_wrap(~sex, nrow = 1, labeller = as_labeller(c(Boys = "Boys", Girls = "Girls")))+
    scale_fill_gradient2(low = "blue", mid = "white", high = "red")+
    labs(title = cohort_nm, x = NULL, y = NULL)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.spacing = unit(0.2, "lines"))
}
plot4 <- wrap_plots(map(unique(long$cohort), plot_heat), ncol = 1)

# 5. PAIRED BOXPLOTS  (overall distribution check) ------------------------
plot5 <- ggplot(long,
                aes(sex, r, fill = sex))+
  geom_boxplot(outlier.shape = NA, width = .6)+
  geom_jitter(width = .15, alpha = .2)+
  facet_wrap(~cohort)+
  scale_fill_manual(values = c(Boys = "#56B4E9", Girls = "#E69F00"))+
  labs(title = "Boxplots: Boys vs Girls, all tool pairs",
       x = NULL, y = "Correlation")+
  theme_bw()+theme(legend.position = "none")

# ── DISPLAY or SAVE ──────────────────────────────────────────────────────
print(plot1)
print(plot2)
print(plot3)
print(plot4)
print(plot5)



# ─────────────────────────────────────────────────────────────────────────
# DATA PREP (assumes results already exists) + 10 VISUAL OPTIONS
#   • First 5 plots → only TOP-15 pairs per cohort
#   • Next 5 plots → offer BOTH: full data & TOP-15
# -------------------------------------------------------------------------
#   Dependencies: tidyverse, tidytext, patchwork
# ─────────────────────────────────────────────────────────────────────────
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidytext, patchwork)

## 0.  helpers -------------------------------------------------------------
topN  <- 15
tool_order <- rownames(data_list[[1]]$corr)

pair_levels <- results %>%
  arrange(match(var1, tool_order), match(var2, tool_order)) %>%
  distinct(pair) %>% pull(pair)

# full vs top-15 datasets in *long* form
long_all <- results %>%
  pivot_longer(c(r_boys, r_girls), names_to = "sex", values_to = "r") %>%
  mutate(
    sex  = recode(sex, r_boys = "Boys", r_girls = "Girls"),
    pair = factor(pair, levels = pair_levels)
  )

long_top <- results %>%
  group_by(cohort) %>%
  slice_max(order_by = abs(diff), n = topN, with_ties = FALSE) %>%
  ungroup() %>%
  pivot_longer(c(r_boys, r_girls), names_to = "sex", values_to = "r") %>%
  mutate(
    sex  = recode(sex, r_boys = "Boys", r_girls = "Girls"),
    pair = factor(pair, levels = pair_levels)
  )

## 1. plotting functions (return ggplot objects) --------------------------
cols_sex <- c(Boys = "#56B4E9", Girls = "#E69F00")

# P1 ─ Grouped bars
P1 <- \(d) ggplot(d, aes(reorder_within(pair, r, cohort), r, fill = sex)) +
  geom_col(position = position_dodge(0.75)) +
  facet_wrap(~cohort, scales = "free_y") +
  scale_x_reordered() + coord_flip() +
  scale_fill_manual(values = cols_sex) +
  labs(title = "Grouped bars", x = NULL, y = "Correlation") +
  theme_bw()

# P2 ─ Slopegraph
P2 <- \(d) ggplot(d, aes(sex, r, group = pair, colour = pair)) +
  geom_line(alpha = .4) + geom_point(size = 2) +
  facet_wrap(~cohort) +
  scale_x_discrete(limits = c("Boys", "Girls")) +
  labs(title = "Slopegraph", x = NULL, y = "Correlation") +
  theme_bw() + theme(legend.position = "none")

# P3 ─ Dumbbell
P3 <- \(d) ggplot(d, aes(x = r, y = reorder_within(pair, r, cohort), colour = sex)) +
  geom_line(aes(group = pair), colour = "grey60") +
  geom_point(size = 2) +
  facet_wrap(~cohort, scales = "free_y") +
  scale_y_reordered() +
  scale_colour_manual(values = cols_sex) +
  labs(title = "Dumbbell", y = NULL, x = "Correlation") +
  theme_bw()

# P4 ─ Twin heat-maps (boys left, girls right)
heat_one <- \(df, coh) {
  ggplot(df %>% filter(cohort == coh),
         aes(var1, var2, fill = r)) +
    geom_tile() +
    facet_wrap(~sex, nrow = 1) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
    labs(title = coh, x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.spacing = unit(0.15, "lines"))
}
P4 <- \(d) wrap_plots(map(unique(d$cohort), ~heat_one(d, .x)), ncol = 1)

# P5 ─ Paired boxplots
P5 <- \(d) ggplot(d, aes(sex, r, fill = sex)) +
  geom_boxplot(outlier.shape = NA, width = .6) +
  geom_jitter(width = .15, alpha = .2) +
  facet_wrap(~cohort) +
  scale_fill_manual(values = cols_sex) +
  labs(title = "Boxplots", x = NULL, y = "Correlation") +
  theme_bw() + theme(legend.position = "none")

# ---------- next 5 visuals (provide both datasets) -----------------------

# P6 ─ Diverging bars (positive right, negative left)
P6 <- \(d) ggplot(d, aes(reorder_within(pair, diff, cohort), diff, fill = diff > 0)) +
  geom_col() + facet_wrap(~cohort, scales = "free_y") +
  scale_x_reordered() + coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#0072B2", "FALSE" = "#D55E00"), guide = "none") +
  labs(title = "Diverging bars (Δr)", x = NULL, y = "Boys − Girls") +
  theme_bw()

# P7 ─ Caterpillar plot (z-stat ranking)
P7 <- \(d) {
  tmp <- results %>%
    mutate(pair = factor(pair, levels = pair_levels))
  ggplot(tmp, aes(z_stat, pair, colour = cohort)) +
    geom_point() +
    geom_vline(xintercept = 0, linetype = "dotted") +
    labs(title = "Caterpillar plot (z statistics)", y = NULL, x = "z-stat") +
    theme_bw()
}

# P8 ─ Volcano (Δr vs −log10 p)
P8 <- \(d) ggplot(d, aes(diff, -log10(p_value), colour = sig)) +
  geom_point(alpha = .7) + facet_wrap(~cohort) +
  scale_colour_manual(values = c(TRUE = "red3", FALSE = "grey50")) +
  labs(title = "Volcano: effect vs significance", x = "Δr", y = "−log10 p") +
  theme_bw() + theme(legend.position = "none")

# P9 ─ Beeswarm of correlations, by sex
pacman::p_load(ggbeeswarm)
P9 <- \(d) ggplot(d, aes(sex, r, colour = sex)) +
  geom_beeswarm(cex = 1.2) +
  facet_wrap(~cohort) +
  scale_colour_manual(values = cols_sex, guide = "none") +
  labs(title = "Beeswarm (correlations by sex)", x = NULL, y = "Correlation") +
  theme_bw()

# P10 ─ Mean ± SD bar (boys vs girls)
P10 <- \(d) ggplot(d %>% group_by(cohort, sex) %>%
                     summarise(mu = mean(r), sd = sd(r), .groups = "drop"),
                   aes(cohort, mu, fill = sex)) +
  geom_col(position = "dodge", width = .6) +
  geom_errorbar(aes(ymin = mu - sd, ymax = mu + sd),
                position = position_dodge(.6), width = .2) +
  scale_fill_manual(values = cols_sex) +
  labs(title = "Mean ± SD of correlations", x = NULL, y = "Mean r") +
  theme_bw()

## 2. PRINTING / USING -----------------------------------------------------
# ---- FIRST 5  (top-15 only) --------------------------------------------
print(P1(long_top))
print(P2(long_top))
print(P3(long_top))
print(P4(long_top))
print(P5(long_top))

# ---- NEXT 5  (both versions) -------------------------------------------
# full dataset
print(P6(long_all));  print(P7(long_all))
print(P8(long_all));  print(P9(long_all));  print(P10(long_all))

# top-15 dataset
print(P6(long_top));  print(P7(long_top))
print(P8(long_top));  print(P9(long_top));  print(P10(long_top))

# --- NEW: Per-cohort corrplots with Fisher difference significance (UNCORRECTED) ---
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
library(corrplot)

# Helper to make a named empty matrix
.make_mat <- function(vars, fill = NA_real_) {
  m <- matrix(fill, nrow = length(vars), ncol = length(vars),
              dimnames = list(vars, vars))
  diag(m) <- 1
  m
}

# Build matrices for a single cohort
build_mats_for_cohort <- function(results, cohort_name) {
  dat <- results %>%
    filter(cohort == cohort_name) %>%
    mutate(
      var1 = trimws(as.character(var1)),
      var2 = trimws(as.character(var2))
    )
  vars <- sort(unique(c(dat$var1, dat$var2)))
  
  cor_boys  <- .make_mat(vars)
  cor_girls <- .make_mat(vars)
  p_diff    <- matrix(NA_real_, nrow = length(vars), ncol = length(vars),
                      dimnames = list(vars, vars))
  diag(p_diff) <- NA_real_
  
  # Fill symmetric cells
  for (i in seq_len(nrow(dat))) {
    v1 <- dat$var1[i]; v2 <- dat$var2[i]
    if (!(v1 %in% vars && v2 %in% vars) || v1 == v2) next
    r1 <- dat$r_boys[i];  r2 <- dat$r_girls[i]
    p  <- dat$p_value[i]  # <-- uncorrected Fisher test p-value (boys vs girls)
    
    cor_boys [v1, v2] <- cor_boys [v2, v1] <- r1
    cor_girls[v1, v2] <- cor_girls[v2, v1] <- r2
    p_diff   [v1, v2] <- p_diff   [v2, v1] <- p
  }
  
  list(vars = vars, cor_boys = cor_boys, cor_girls = cor_girls, p_diff = p_diff)
}

# Star labels for significance (based on UNCORRECTED p)
make_sig_levels <- function() c(0.001, 0.01, 0.05)

# Create all plots
all_cohorts <- unique(results$cohort)

for (cn in all_cohorts) {
  mats <- build_mats_for_cohort(results, cn)
  with(mats, {
    # 1) DIFFERENCE matrix (Girls - Boys), blanking non-significant diffs (p >= .05)
    diff_mat <- cor_girls - cor_boys
    diag(diff_mat) <- 0
    
    png(sprintf("Corrplot_DIFF_%s.png", cn), width = 2400, height = 2000, res = 300)
    corrplot(
      diff_mat, method = "color", type = "lower", diag = FALSE,
      tl.col = "black", tl.srt = 45, tl.cex = 0.8,
      col = colorRampPalette(c("#313695","#4575B4","#74ADD1","#ABD9E9","#E0F3F8",
                               "#FFFFBF","#FEE090","#FDAE61","#F46D43","#D73027","#A50026"))(200),
      is.corr = FALSE, # center on 0, not treating as correlation
      cl.cex = 0.8,
      title = sprintf("Girls − Boys (r): Significant Cells Only (uncorrected Fisher)\n%s", cn),
      mar = c(0,0,3,0),
      p.mat = p_diff,              # use Fisher *difference* p-values
      sig.level = 0.05,
      insig = "blank",             # blank non-significant differences
      addgrid.col = "grey85",
      number.cex = 0.7
    )
    # zero line color scale center happens automatically in corrplot when is.corr = FALSE
    dev.off()
    
    # 2) Boys vs Girls side-by-side, overlaying *difference* significance as stars
    #    (stars mark cells where Boys vs Girls differ at p < .05 uncorrected)
    png(sprintf("Corrplot_BG_withSig_%s.png", cn), width = 3000, height = 1500, res = 300)
    op <- par(mfrow = c(1,2), mar = c(1,1,3,1))
    # Boys
    corrplot(
      cor_boys, method = "circle", type = "lower", diag = FALSE,
      tl.col = "black", tl.srt = 45, tl.cex = 0.8,
      col = colorRampPalette(c("white","lightblue","darkblue"))(100),
      cl.cex = 0.8,
      title = sprintf("Boys (overlay stars = Girls≠Boys, uncorrected)\n%s", cn),
      mar = c(0,0,3,0),
      p.mat = p_diff,                  # pass Fisher *difference* p
      sig.level = make_sig_levels(),   # use star coding
      insig = "label_sig",             # draw stars for significant differences
      pch.col = "black", pch.cex = 1.1
    )
    # Girls
    corrplot(
      cor_girls, method = "circle", type = "lower", diag = FALSE,
      tl.col = "black", tl.srt = 45, tl.cex = 0.8,
      col = colorRampPalette(c("white","lightblue","darkblue"))(100),
      cl.cex = 0.8,
      title = sprintf("Girls (overlay stars = Girls≠Boys, uncorrected)\n%s", cn),
      mar = c(0,0,3,0),
      p.mat = p_diff,
      sig.level = make_sig_levels(),
      insig = "label_sig",
      pch.col = "black", pch.cex = 1.1
    )
    par(op)
    dev.off()
  })
}

message("Done: per-cohort corrplots created with uncorrected Fisher difference significance.")


# --- NEW: Per-cohort corrplots with Fisher difference significance (UNCORRECTED) ---
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
library(corrplot)

# Helper to make a named empty matrix
.make_mat <- function(vars, fill = NA_real_) {
  m <- matrix(fill, nrow = length(vars), ncol = length(vars),
              dimnames = list(vars, vars))
  diag(m) <- 1
  m
}

# Build matrices for a single cohort
build_mats_for_cohort <- function(results, cohort_name) {
  dat <- results %>%
    filter(cohort == cohort_name) %>%
    mutate(
      var1 = trimws(as.character(var1)),
      var2 = trimws(as.character(var2))
    )
  vars <- sort(unique(c(dat$var1, dat$var2)))
  
  cor_boys  <- .make_mat(vars)
  cor_girls <- .make_mat(vars)
  p_diff    <- matrix(NA_real_, nrow = length(vars), ncol = length(vars),
                      dimnames = list(vars, vars))
  diag(p_diff) <- NA_real_
  
  # Fill symmetric cells
  for (i in seq_len(nrow(dat))) {
    v1 <- dat$var1[i]; v2 <- dat$var2[i]
    if (!(v1 %in% vars && v2 %in% vars) || v1 == v2) next
    r1 <- dat$r_boys[i];  r2 <- dat$r_girls[i]
    p  <- dat$p_value[i]  # <-- uncorrected Fisher test p-value (boys vs girls)
    
    cor_boys [v1, v2] <- cor_boys [v2, v1] <- r1
    cor_girls[v1, v2] <- cor_girls[v2, v1] <- r2
    p_diff   [v1, v2] <- p_diff   [v2, v1] <- p
  }
  
  list(vars = vars, cor_boys = cor_boys, cor_girls = cor_girls, p_diff = p_diff)
}

# Star labels for significance (based on UNCORRECTED p)
make_sig_levels <- function() c(0.001, 0.01, 0.05)

# Create all plots
all_cohorts <- unique(results$cohort)

for (cn in all_cohorts) {
  mats <- build_mats_for_cohort(results, cn)
  with(mats, {
    # 1) DIFFERENCE matrix (Girls - Boys), blanking non-significant diffs (p >= .05)
    diff_mat <- cor_girls - cor_boys
    diag(diff_mat) <- 0
    
    png(sprintf("Corrplot_DIFF_%s.png", cn), width = 2400, height = 2000, res = 300)
    corrplot(
      diff_mat, method = "color", type = "lower", diag = FALSE,
      tl.col = "black", tl.srt = 45, tl.cex = 0.8,
      col = colorRampPalette(c("#313695","#4575B4","#74ADD1","#ABD9E9","#E0F3F8",
                               "#FFFFBF","#FEE090","#FDAE61","#F46D43","#D73027","#A50026"))(200),
      is.corr = FALSE, # center on 0, not treating as correlation
      cl.cex = 0.8,
      title = sprintf("Girls − Boys (r): Significant Cells Only (uncorrected Fisher)\n%s", cn),
      mar = c(0,0,3,0),
      p.mat = p_diff,              # use Fisher *difference* p-values
      sig.level = 0.05,
      insig = "blank",             # blank non-significant differences
      addgrid.col = "grey85",
      number.cex = 0.7
    )
    # zero line color scale center happens automatically in corrplot when is.corr = FALSE
    dev.off()
    
    # 2) Boys vs Girls side-by-side, overlaying *difference* significance as stars
    #    (stars mark cells where Boys vs Girls differ at p < .05 uncorrected)
    png(sprintf("Corrplot_BG_withSig_%s.png", cn), width = 3000, height = 1500, res = 300)
    op <- par(mfrow = c(1,2), mar = c(1,1,3,1))
    # Boys
    corrplot(
      cor_boys, method = "circle", type = "lower", diag = FALSE,
      tl.col = "black", tl.srt = 45, tl.cex = 0.8,
      col = colorRampPalette(c("white","lightblue","darkblue"))(100),
      cl.cex = 0.8,
      title = sprintf("Boys (overlay stars = Girls≠Boys, uncorrected)\n%s", cn),
      mar = c(0,0,3,0),
      p.mat = p_diff,                  # pass Fisher *difference* p
      sig.level = make_sig_levels(),   # use star coding
      insig = "label_sig",             # draw stars for significant differences
      pch.col = "black", pch.cex = 1.1
    )
    # Girls
    corrplot(
      cor_girls, method = "circle", type = "lower", diag = FALSE,
      tl.col = "black", tl.srt = 45, tl.cex = 0.8,
      col = colorRampPalette(c("white","lightblue","darkblue"))(100),
      cl.cex = 0.8,
      title = sprintf("Girls (overlay stars = Girls≠Boys, uncorrected)\n%s", cn),
      mar = c(0,0,3,0),
      p.mat = p_diff,
      sig.level = make_sig_levels(),
      insig = "label_sig",
      pch.col = "black", pch.cex = 1.1
    )
    par(op)
    dev.off()
  })
}
z
message("Done: per-cohort corrplots created with uncorrected Fisher difference significance.")

library(dplyr)
library(corrplot)

# --- 0) QUICK OVERVIEW ---
cat("Working directory is:\n", getwd(), "\n\n")
cat("Cohorts in results and row counts:\n")
print(results %>% count(cohort))
cat("\nColumns present:\n")
print(names(results))

# --- 1) PICK ONE COHORT TO SHOW ON SCREEN (no files) ---
one_cn <- (results %>% count(cohort) %>% arrange(desc(n)) %>% slice(1) %>% pull(cohort))
cat("\nShowing cohort on-screen:", one_cn, "\n")

build_mats_for_cohort <- function(results, cohort_name) {
  dat <- results %>%
    filter(cohort == cohort_name) %>%
    mutate(var1 = trimws(as.character(var1)),
           var2 = trimws(as.character(var2)))
  if (nrow(dat) == 0) return(NULL)
  
  vars <- sort(unique(c(dat$var1, dat$var2)))
  mk <- function(vars, fill = NA_real_) {
    m <- matrix(fill, length(vars), length(vars), dimnames = list(vars, vars))
    diag(m) <- 1
    m
  }
  cor_boys  <- mk(vars)
  cor_girls <- mk(vars)
  p_diff    <- matrix(NA_real_, length(vars), length(vars), dimnames = list(vars, vars))
  
  for (i in seq_len(nrow(dat))) {
    v1 <- dat$var1[i]; v2 <- dat$var2[i]
    if (!(v1 %in% vars && v2 %in% vars) || v1 == v2) next
    cor_boys [v1, v2] <- cor_boys [v2, v1] <- dat$r_boys[i]
    cor_girls[v1, v2] <- cor_girls[v2, v1] <- dat$r_girls[i]
    p_diff   [v1, v2] <- p_diff   [v2, v1] <- dat$p_value[i]   # uncorrected Fisher test p
  }
  list(vars=vars, cor_boys=cor_boys, cor_girls=cor_girls, p_diff=p_diff)
}

mats <- build_mats_for_cohort(results, one_cn)
if (is.null(mats)) stop("No rows for cohort ‘", one_cn, "’.")
with(mats, {
  cat("\nVars:", length(vars), "— first 10:\n")
  print(head(vars, 10))
  cat("\nAny lower-tri p-values NOT NA? -> ",
      any(!is.na(p_diff[lower.tri(p_diff)])), "\n")
  cat("Share of significant diffs (p<.05) -> ",
      mean(p_diff[lower.tri(p_diff)] < .05, na.rm = TRUE), "\n\n")
  
  # SHOW NOW: difference heatmap (no file)
  diff_mat <- cor_girls - cor_boys
  diag(diff_mat) <- 0
  corrplot(diff_mat, method="color", type="lower", diag=FALSE,
           tl.col="black", tl.srt=45, tl.cex=0.8,
           col=colorRampPalette(c("#313695","#4575B4","#74ADD1","#ABD9E9","#E0F3F8",
                                  "#FFFFBF","#FEE090","#FDAE61","#F46D43","#D73027","#A50026"))(200),
           is.corr=FALSE, cl.cex=0.8,
           title=paste0("Girls − Boys (r), significant cells only (uncorr Fisher)\n", one_cn),
           mar=c(0,0,3,0),
           p.mat=p_diff, sig.level=0.05, insig="blank", addgrid.col="grey85")
})

# If you still see an empty plot:
# - If the console said "Share of significant diffs -> 0", everything was blanked.
#   Try insig = "pch" to show faint points instead of blank:
# corrplot(diff_mat, ..., p.mat=p_diff, sig.level=0.05, insig="pch", pch.col="grey70")

# --- 2) SAVE ALL COHORT PNGs WITH EXPLICIT PATHS ---
out_dir <- file.path(getwd(), "figs_corrplots")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
cat("Saving PNGs to:\n", out_dir, "\n\n")

sig_levels <- c(0.001, 0.01, 0.05)

for (cn in unique(results$cohort)) {
  mats <- build_mats_for_cohort(results, cn)
  if (is.null(mats)) {
    cat("Skipping", cn, "— no rows.\n"); next
  }
  with(mats, {
    # If all p are NA or no lower-tri entries, avoid blank output
    use_insig <- if (all(is.na(p_diff[lower.tri(p_diff)]))) "pch" else "blank"
    
    # 1) Diff heatmap
    diff_mat <- cor_girls - cor_boys; diag(diff_mat) <- 0
    f1 <- file.path(out_dir, paste0("Corrplot_DIFF_", cn, ".png"))
    png(f1, width=2400, height=2000, res=300)
    corrplot(diff_mat, method="color", type="lower", diag=FALSE,
             tl.col="black", tl.srt=45, tl.cex=0.8,
             col=colorRampPalette(c("#313695","#4575B4","#74ADD1","#ABD9E9","#E0F3F8",
                                    "#FFFFBF","#FEE090","#FDAE61","#F46D43","#D73027","#A50026"))(200),
             is.corr=FALSE, cl.cex=0.8,
             title=paste0("Girls − Boys (r), significant cells only (uncorr Fisher)\n", cn),
             mar=c(0,0,3,0),
             p.mat=p_diff, sig.level=0.05, insig=use_insig, addgrid.col="grey85")
    dev.off(); cat("Wrote:", f1, "\n")
    
    # 2) Boys vs Girls with star overlay (difference significance)
    f2 <- file.path(out_dir, paste0("Corrplot_BG_withSig_", cn, ".png"))
    png(f2, width=3000, height=1500, res=300)
    op <- par(mfrow=c(1,2), mar=c(1,1,3,1))
    
    corrplot(cor_boys, method="circle", type="lower", diag=FALSE,
             tl.col="black", tl.srt=45, tl.cex=0.8,
             col=colorRampPalette(c("white","lightblue","darkblue"))(100),
             cl.cex=0.8,
             title=paste0("Boys (stars = Girls≠Boys, uncorrected)\n", cn),
             mar=c(0,0,3,0),
             p.mat=p_diff, sig.level=sig_levels,
             insig="label_sig", pch.col="black", pch.cex=1.1)
    
    corrplot(cor_girls, method="circle", type="lower", diag=FALSE,
             tl.col="black", tl.srt=45, tl.cex=0.8,
             col=colorRampPalette(c("white","lightblue","darkblue"))(100),
             cl.cex=0.8,
             title=paste0("Girls (stars = Girls≠Boys, uncorrected)\n", cn),
             mar=c(0,0,3,0),
             p.mat=p_diff, sig.level=sig_levels,
             insig="label_sig", pch.col="black", pch.cex=1.1)
    
    par(op); dev.off(); cat("Wrote:", f2, "\n")
  })
}
