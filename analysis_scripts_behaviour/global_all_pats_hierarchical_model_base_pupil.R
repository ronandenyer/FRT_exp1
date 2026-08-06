library(dplyr)
library(nlme)
library(car)
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
    pupil_size_mean = pupil_size_mean*1e6,
    pupil_size_phasic_mean_norm = pupil_size_phasic_mean_norm*1e6,
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



library(dplyr)
library(ggplot2)

# =========================================================================
# 1. Calculate the Median Phasic Size per Subject per Tertile
# =========================================================================
subject_tertile_medians <- shooting_combined %>%
  filter(!is.na(pupil_phasic_tertile), !is.na(pupil_size_phasic_mean_norm)) %>%
  group_by(SID, pupil_phasic_tertile) %>%
  summarize(
    subject_median = median(pupil_size_phasic_mean_norm, na.rm = TRUE),
    .groups = "drop"
  )

# =========================================================================
# 2. Calculate the Mean of those Subject Medians per Tertile
# =========================================================================
group_tertile_summary <- subject_tertile_medians %>%
  group_by(pupil_phasic_tertile) %>%
  summarize(
    mean_of_medians = mean(subject_median, na.rm = TRUE),
    se_of_medians = sd(subject_median, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Print the exact numeric values to check against your continuous plot peak
cat("\n--- Physiological Centers of Gravity per Tertile ---\n")
print(group_tertile_summary)

# =========================================================================
# 3. Visualize the Alignment
# =========================================================================
goldilocks_check_plot <- ggplot(group_tertile_summary, 
                                aes(x = pupil_phasic_tertile, y = mean_of_medians, fill = pupil_phasic_tertile)) +
  geom_point(alpha = 0.8, width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = mean_of_medians - se_of_medians, 
                    ymax = mean_of_medians + se_of_medians), 
                width = 0.2, linewidth = 0.8) +
  
  # Matching your high-contrast manuscript palette
  scale_fill_manual(values = c("1" = "#4A5568", "2" = "#DD6B20", "3" = "#3182CE")) +
  scale_x_discrete(labels = c("1" = "Low\n(Tertile 1)", 
                              "2" = "Intermediate\n(Tertile 2)", 
                              "3" = "High\n(Tertile 3)")) +
  
  labs(
    title = "Physiological Centers of the Noradrenergic States",
    subtitle = "Does the mean of Tertile 2 match the peak of the continuous quadratic fit?",
    x = "Within-Subject Phasic Tertile",
    y = "Normalized Phasic Pupil Burst\n(Mean of Subject Medians)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 12)
  )

print(goldilocks_check_plot)

# Print a quick diagnostic to verify data integration sizes
cat("Total integrated observations:", nrow(shooting_combined), "\n")
print(table(shooting_combined$pupil_phasic_tertile))


# 1. Regress preparation time on the pupil tertiles with subject random intercepts
eff_check_model <- lmer(eff_pt ~ pupil_phasic_tertile + (1 | SID), data = shooting_combined)

# 2. Run an Type III Wald Chi-Square Test (Omnibus Test)
Anova(eff_check_model, type = "III")
# =========================================================================
# 5. Run the High-Powered Tertile Psychometric Model
# =========================================================================
nlme_combined_tertile <- nlme(
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  fixed = list(mu ~ pupil_phasic_tertile, 
               sigma ~ 1, 
               asym ~ pupil_phasic_tertile, 
               gamma ~ 1), 
  
  random = mu  ~ 1 | SID,
  
  data = shooting_combined,
  
  # 10 starting values matching the 3-level parameter expansions
  start = list(fixed = c(0.20, -0.01,  0.00,   # mu (Low, Med_diff, High_diff)
                         0.02, # sigma (Low, Med_diff, High_diff)
                         0.95,  0.004, 0.00,   # asym (Low, Med_diff, High_diff)
                         0.25))                # gamma
)

# =========================================================================
# 6. Evaluate Results
# =========================================================================
print(summary(nlme_combined_tertile))
print(anova(nlme_combined_tertile))

library(emmeans)

# 1. Post-hoc pairwise comparisons for the Threshold (mu)
emmeans(nlme_combined_tertile, pairwise ~ pupil_phasic_tertile, param = "mu")

# 2. Post-hoc pairwise comparisons for the Precision/Noise (sigma)
# Since your ANOVA showed sigma was also significant, you will definitely want this one too:
emmeans(nlme_combined_tertile, pairwise ~ pupil_phasic_tertile, param = "sigma")
library(dplyr)
library(nlme)
library(ggplot2)

# Define your plot font size variables here so ggplot can grab them
x_axis_title_size <- 16
y_axis_title_size <- 16
axis_text_size     <- 14
legend_coords      <- c(0.8, 0.3) # Adjust based on layout preferences

# =========================================================================
# 1. Generate Population-Level Predictions (plot_grid)
# =========================================================================
plot_grid <- expand.grid(
  eff_pt = seq(0, 0.5, length.out = 300),
  pupil_phasic_tertile = factor(c("1", "2", "3"), levels = c("1", "2", "3"))
)

# Extract fixed-effects predictions (level = 0)
plot_grid$predicted <- predict(nlme_combined_tertile, newdata = plot_grid, level = 0)

# =========================================================================
# 2. Generate Individual Subject-Level Predictions (indiv_grid)
# =========================================================================
indiv_grid <- expand.grid(
  eff_pt = seq(0, 0.5, length.out = 100),
  pupil_phasic_tertile = factor(c("1", "2", "3"), levels = c("1", "2", "3")),
  SID = levels(shooting_combined$SID)
)

# Extract subject-specific predictions (level = 1)
indiv_grid$predicted_indiv <- predict(nlme_combined_tertile, newdata = indiv_grid, level = 1)

# =========================================================================
# 3. Generate Empirical Binned Data Points (shooting_binned)
# =========================================================================
shooting_binned <- shooting_combined %>%
  filter(!is.na(pupil_phasic_tertile)) %>%
  # Create 20ms bins for empirical trial grouping
  mutate(bin = round(eff_pt / 0.02) * 0.02) %>%
  group_by(bin, pupil_phasic_tertile) %>%
  summarize(
    prop_success = mean(success),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(bin >= 0 & bin <= 0.5)

sat_tertile_curve <- ggplot() +
  # 1. Plot the raw individual subject curves (Faint spaghetti lines)
  geom_line(data = indiv_grid, 
            aes(x = eff_pt, y = predicted_indiv, color = pupil_phasic_tertile, 
                group = interaction(SID, pupil_phasic_tertile)), 
            alpha = 0.10, linewidth = 0.4) + 
  
  # 2. Plot the raw binned data points (Sized by trial count)
  geom_point(data = shooting_binned, 
             aes(x = bin, y = prop_success, color = pupil_phasic_tertile, size = n), 
             alpha = 0.4) +
  
  # 3. Plot the primary model-fitted population lines
  geom_line(data = plot_grid, 
            aes(x = eff_pt, y = predicted, color = pupil_phasic_tertile), 
            linewidth = 2) +
  
  # 4. Aesthetics & Palette Configuration
  scale_color_manual(values = c("1" = "#4A5568", "2" = "#DD6B20", "3" = "#3182CE"), 
                     labels = c("Low Phasic (1)", "Intermediate Phasic (2)", "High Phasic (3)")) +
  scale_size_area(max_size = 8) +
  
  # The 1/4 chance floor
  geom_hline(yintercept = 0.25, linetype = "dashed", alpha = 0.5) + 
  
  # Labels and Axis ranges
  labs(
    x = "Preparation Time (s)",
    y = "Probability of Success",
    size = "Trial Count",
    color = "Pupil State",
    tag = "B."
  ) +
  guides(
    size = guide_legend(order = 1),   
    color = guide_legend(order = 2)   
  ) +
  scale_y_continuous(limits = c(0, 1), 
                     breaks = seq.int(0, 1, 0.2)) +
  xlim(0., 0.5) +
  theme_classic() +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black", fill = "white"),
    axis.title.y = element_text(size = y_axis_title_size),
    axis.title.x = element_text(size = x_axis_title_size),
    axis.text = element_text(size = axis_text_size),
    legend.position = legend_coords,
    plot.tag = element_text(size = 45, face = "bold", hjust = 0, vjust = 1)
  ) 

# Render the polished plot
sat_tertile_curve


