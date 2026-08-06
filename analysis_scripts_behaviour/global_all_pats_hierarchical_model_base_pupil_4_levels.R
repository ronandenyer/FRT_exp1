library(dplyr)
library(nlme)
library(patchwork)
# =========================================================================
# 1. Load Both Datasets Freshly
# =========================================================================
df_tvns   <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')
df_reward <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')

# Ensure Subject IDs are characters/factors to avoid type mismatches during merging
df_tvns$SID   <- as.character(df_tvns$SID)
df_reward$SID <- as.character(df_reward$SID)

# =========================================================================
# 2. Intersect and Merge by Common Columns Only
# =========================================================================
# Find columns present in BOTH dataframes
common_cols <- intersect(names(df_tvns), names(df_reward))

# Subset and bind rows cleanly
shooting_combined <- rbind(df_tvns[, common_cols], df_reward[, common_cols])

# =========================================================================
# 3. Data Cleaning & Standardization
# =========================================================================
shooting_combined <- shooting_combined %>%
  # Filter out bad timing values and missing rows in core variables
  filter(
    eff_pt > 0,
    !is.na(eff_pt),
    !is.na(success),
    !is.na(pupil_size_phasic_mean_norm),
    !is.na(SID)
  ) %>%
  mutate(
    # Convert preparation time from milliseconds to seconds
    eff_pt = eff_pt / 1000,
    SID = as.factor(SID)
  )

# =========================================================================
# 4. Compute Within-Subject Phasic Tertiles on the Combined Pool
# =========================================================================
shooting_combined <- shooting_combined %>%
  group_by(SID) %>%
  mutate(
    pupil_phasic_tertile = cut(
      pupil_size_phasic_mean_norm, 
      breaks = quantile(pupil_size_phasic_mean_norm, probs = seq(0, 1, by = 1/3), na.rm = TRUE),
      labels = c("1", "2", "3"), 
      include.lowest = TRUE
    ),
    pupil_phasic_tertile = as.factor(pupil_phasic_tertile)
  ) %>%
  ungroup()

# Print a quick diagnostic to verify data integration sizes
cat("Total integrated observations:", nrow(shooting_combined), "\n")
print(table(shooting_combined$pupil_phasic_tertile))

# =========================================================================
# 5. Run the High-Powered Tertile Psychometric Model
# =========================================================================
nlme_combined_tertile <- nlme(
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  fixed = list(mu ~ pupil_phasic_tertile, 
               sigma ~ pupil_phasic_tertile, 
               asym ~ pupil_phasic_tertile, 
               gamma ~ 1), 
  
  random = mu ~ 1 | SID,
  
  data = shooting_combined,
  
  # 10 starting values matching the 3-level parameter expansions
  start = list(fixed = c(0.20, -0.01,  0.00,   # mu (Low, Med_diff, High_diff)
                         0.02,  0.001, 0.00,   # sigma (Low, Med_diff, High_diff)
                         0.95,  0.004, 0.00,   # asym (Low, Med_diff, High_diff)
                         0.25))                # gamma
)

# =========================================================================
# 6. Evaluate Results
# =========================================================================
print(summary(nlme_combined_tertile))
print(anova(nlme_combined_tertile))
emmeans(nlme_combined_tertile, pairwise ~ pupil_phasic_tertile, param = "mu")
# Ensure you have patchwork installed: install.packages("patchwork")
library(dplyr)
library(ggplot2)
library(patchwork)

# =========================================================================
# DATA PREPARATION (From previous steps)
# =========================================================================
# 1. Binned empirical data
shooting_binned_3way <- shooting_combined %>%
  filter(!is.na(pupil_phasic_tertile), !is.na(success), !is.na(eff_pt)) %>%
  mutate(bin = round(eff_pt / 0.02) * 0.02) %>%
  group_by(bin, pupil_phasic_tertile) %>%
  summarize(prop_success = mean(success), n = n(), .groups = "drop") %>%
  filter(bin >= 0 & bin <= 0.5)

shooting_binned_3way$Arousal <- factor(
  shooting_binned_3way$pupil_phasic_tertile, 
  levels = c("1", "2", "3"), 
  labels = c("Low Phasic", "Medium Phasic\n(Sweet Spot)", "High Phasic\n(Over-arousal)")
)

# 2. Fixed effects smooth lines grid
plot_grid_3way <- expand.grid(eff_pt = seq(0, 0.5, length.out = 300), pupil_phasic_tertile = factor(c("1", "2", "3")))
plot_grid_3way$predicted <- predict(nlme_combined_tertile, newdata = plot_grid_3way, level = 0)
plot_grid_3way$Arousal <- factor(
  plot_grid_3way$pupil_phasic_tertile, 
  levels = c("1", "2", "3"), 
  labels = c("Low Phasic", "Medium Phasic\n(Sweet Spot)", "High Phasic\n(Over-arousal)")
)

# 3. Subject-level curves grid
subj_ids_combined <- unique(shooting_combined$SID) 
indiv_grid_3way <- expand.grid(eff_pt = seq(0, 0.5, length.out = 100), pupil_phasic_tertile = factor(c("1", "2", "3")), SID = subj_ids_combined)
indiv_grid_3way$predicted_indiv <- predict(nlme_combined_tertile, newdata = indiv_grid_3way, level = 1)
indiv_grid_3way$Arousal <- factor(
  indiv_grid_3way$pupil_phasic_tertile, 
  levels = c("1", "2", "3"), 
  labels = c("Low Phasic", "Medium Phasic\n(Sweet Spot)", "High Phasic\n(Over-arousal)")
)

# 4. NEW: Create dataframe for the MU parameter panel from your emmeans output
mu_estimates <- data.frame(
  Arousal = factor(c("Low Phasic", "Medium Phasic\n(Sweet Spot)", "High Phasic\n(Over-arousal)"),
                   levels = c("Low Phasic", "Medium Phasic\n(Sweet Spot)", "High Phasic\n(Over-arousal)")),
  mu = c(0.19995, 0.19524, 0.19752), # Absolute estimates derived from your emmeans
  SE = c(0.00433, 0.00432, 0.00431)  # Associated Standard Errors
)

# =========================================================================
# SHARED AESTHETICS
# =========================================================================
palette_3way <- c(
  "Low Phasic" = "#718096", 
  "Medium Phasic\n(Sweet Spot)" = "#2B6CB0", 
  "High Phasic\n(Over-arousal)" = "#E53E3E"
)

# =========================================================================
# PANEL C: FULL PSYCHOMETRIC CURVES
# =========================================================================
goldilocks_curve <- ggplot() +
  geom_line(data = indiv_grid_3way, aes(x = eff_pt, y = predicted_indiv, color = Arousal, group = interaction(SID, Arousal)), 
            alpha = 0.03, linewidth = 0.3, show.legend = FALSE) +
  geom_point(data = shooting_binned_3way, aes(x = bin, y = prop_success, color = Arousal, size = n), alpha = 0.2) +
  geom_line(data = plot_grid_3way, aes(x = eff_pt, y = predicted, color = Arousal), linewidth = 1) +
  geom_hline(yintercept = 0.25, linetype = "dotted", color = "gray40", alpha = 0.7) +
  scale_color_manual(values = palette_3way) +
  scale_size_area(max_size = 5) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(0.15, 0.3), breaks = seq(0, 0.5, 0.1)) +
  labs(x = "Preparation Time (s)", y = "Probability of Success", size = "Trial Count", color = "Phasic Arousal State", tag = "C.") +
  theme_classic() +
  guides(color = guide_legend(order = 1, override.aes = list(linewidth = 2, alpha = 1)), size = guide_legend(order = 2)) +
  theme(
    legend.text = element_text(size = 10), legend.title = element_text(size = 11, face = "bold"),
    legend.background = element_blank(), legend.box.background = element_blank(),
    axis.title.y = element_text(size = 18, margin = margin(r = 10)), axis.title.x = element_text(size = 18, margin = margin(t = 10)),
    axis.text = element_text(size = 14, color = "black"),
    legend.position = c(0.68, 0.28), plot.tag = element_text(size = 28, face = "bold", vjust = 1)
  )

# =========================================================================
# PANEL D: NEW MU PARAMETER SUMMARY (Half-width focus panel)
# =========================================================================
mu_plot <- ggplot(mu_estimates, aes(x = Arousal, y = mu, color = Arousal)) +
  # Vertical error bars mapping standard error bounds
  geom_errorbar(aes(ymin = mu - SE, ymax = mu + SE), width = 0.15, linewidth = 1.2, alpha = 0.8) +
  # Large indicator points for the absolute mean values
  geom_point(size = 4.5) +
  scale_color_manual(values = palette_3way, guide = "none") +
  # Tightly focused y-limits highlight the parabolic inflection without clipping error bars
  scale_y_continuous(limits = c(0.188, 0.208), breaks = seq(0.190, 0.205, 0.005)) +
  labs(x = "Phasic Arousal State", y = "Preparation Threshold \u03bc (s)", tag = "D.") +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 18, margin = margin(r = 10)),
    axis.title.x = element_text(size = 18, margin = margin(t = 15)),
    axis.text.x = element_text(size = 11, color = "black", angle = 15, vjust = 0.9, hjust = 0.9),
    axis.text.y = element_text(size = 14, color = "black"),
    plot.tag = element_text(size = 28, face = "bold", vjust = 1)
  )
print(mu_plot)
# =========================================================================
# COMBINE PANELS USING PATCHWORK (2:1 Width Ratio Layout)
# =========================================================================
# Using the plot_layout tool to lock Panel D to exactly half the horizontal footprint of Panel C
final_manuscript_figure <- goldilocks_curve + mu_plot + plot_layout(widths = c(2, 1))

# Render combined figure to your R plot window
print(final_manuscript_figure)