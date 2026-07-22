shooting_tvns <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')
shooting_reward <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')

shooting_tvns$condition = shooting_tvns$stim_type
shooting_reward$condition = shooting_reward$reward_type

library(dplyr)

# Add the 'condition' mapping and a source tracker
shooting_tvns$condition <- shooting_tvns$stim_type
shooting_tvns$experiment_source <- "tVNS_Session"

shooting_reward$condition <- shooting_reward$reward_type
shooting_reward$experiment_source <- "Reward_Session"

# Find common columns (this will now automatically include 'condition' and 'experiment_source')
common_cols <- intersect(names(shooting_tvns), names(shooting_reward))

# Bind them
shooting_combined <- bind_rows(
  shooting_tvns %>% select(all_of(common_cols)),
  shooting_reward %>% select(all_of(common_cols))
)


# Remove NAs
shooting_combined <- shooting_combined[!is.na(shooting_combined$eff_pt) & 
                       !is.na(shooting_combined$success) & 
                       !is.na(shooting_combined$pupil_size_mean), ]

# Standardize units (Fixed double division of eff_pt)
shooting_combined$eff_pt <- shooting_combined$eff_pt / 1000 # Convert to seconds
shooting_combined$pupil_size_phasic_mean_norm <- shooting_combined$pupil_size_phasic_mean_norm * 1e4
shooting_combined$pupil_size_phasic_mean <- shooting_combined$pupil_size_phasic_mean * 1e4
shooting_combined$pupil_size_phasic_slope <- shooting_combined$pupil_size_phasic_slope * 1e4
shooting_combined$pupil_size_max <- shooting_combined$pupil_size_max * 1e4

shooting_combined$pupil_size_mean <- shooting_combined$pupil_size_mean * 1e4
shooting_combined$condition = as.factor(shooting_combined$condition)
# -------------------------------------------------------------------------
# 2. WITHIN-SUBJECT CENTERING & SCALING
# -------------------------------------------------------------------------
shooting_combined <- shooting_combined %>%
  group_by(SID) %>%
  mutate(
    pupil_state = pupil_size_mean - mean(pupil_size_mean, na.rm = TRUE),
    pupil_phasic = pupil_size_phasic_mean_norm - mean(pupil_size_phasic_mean_norm, na.rm = TRUE),
    pupil_slope = pupil_size_phasic_slope - mean(pupil_size_phasic_slope, na.rm = TRUE),
    pupil_max = pupil_size_max - mean(pupil_size_max, na.rm = TRUE),
    pupil_trait = mean(pupil_size_mean, na.rm = TRUE),
    vigor_wc = peak_tanVel - mean(peak_tanVel, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    pupil_state_scaled = pupil_state / sd(pupil_state, na.rm = TRUE),
    pupil_phasic_scaled = pupil_phasic / sd(pupil_phasic, na.rm = TRUE),
    pupil_slope_scaled = pupil_slope / sd(pupil_slope, na.rm = TRUE),
    pupil_max_scaled = pupil_max / sd(pupil_max, na.rm = TRUE),
    vigor_scaled = vigor_wc / sd(vigor_wc, na.rm = TRUE)
  )


pupil_state_lmer = lmer(pupil_max ~ condition + 
                          (1| SID),
                        data = shooting_combined)

summary(pupil_state_lmer)


# 2. Calculate the estimated marginal means for the condition factor
pupil_phasic_emms <- emmeans(pupil_phasic_lmer, specs = ~ condition)

# 3. View the means, standard errors, and confidence intervals per condition
print(pupil_phasic_emms)

# 4. Perform pairwise comparisons between conditions (with automatic Tukey adjustment)
pupil_phasic_pairs <- pairs(pupil_phasic_emms)
print(pupil_phasic_pairs)


# Check your Inverted-U theory right away:
# Does the tVNS session have a higher/wider baseline pupil distribution?
ggplot(shooting_combined, aes(x = as.factor(condition), y = pupil_size_max)) +
  stat_summary(alpha = 0.5) +
  theme_bw()

library(dplyr)
library(ggplot2)

# 1. Create a helper function to identify valid data (non-outliers)
# Uses 3x IQR for "extreme" outliers. Change multiplier to 1.5 for "mild" outliers.
is_not_extreme_outlier <- function(x, multiplier = 3) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  lower_bound <- q1 - (multiplier * iqr)
  upper_bound <- q3 + (multiplier * iqr)
  
  # Returns TRUE if the value is within the acceptable bounds
  return(x >= lower_bound & x <= upper_bound)
}
shooting_combined$condition <- factor(shooting_combined$condition, levels = c("1", "2", "3", "4"))

# 2. Filter the data, then calculate tertiles
shooting_data_plot <- shooting_combined %>%
  # Filter outliers WITHIN each subject and session to account for baseline shifts
  group_by(SID, experiment_source) %>%
  filter(
    is_not_extreme_outlier(pupil_state),
    is_not_extreme_outlier(pupil_max)
  ) %>%
  ungroup() %>%
  
  # Now group by session (day) alone to calculate the tertiles
  #group_by() %>%
  mutate(
    pupil_tertile = ntile(pupil_state, 2), 
    pupil_tertile = factor(pupil_tertile, 
                           levels = c(1, 2), 
                           labels = c("Low", "High"))
  ) #%>%
  #ungroup()

# Pass the exact vector of levels in the order you want them

pupil_median_state_lmer = lmer(pupil_max ~ condition*pupil_tertile + 
                          (1| SID),
                        data = shooting_data_plot)
summary(pupil_median_state_lmer)

# 3. Build the plotlibrary(ggplot2)

# ====================================================================
# 1. PARAMETERS: Define your custom colors and labels here
# Note: Change "1", "2", "3", "4" to match the EXACT names 
# of the condition levels currently in your dataframe!
# ====================================================================

cond_colors <- c(
  "1" = "#E41A1C",  # Red
  "2" = "#377EB8",  # Blue
  "3" = "#4DAF4A",  # Green
  "4" = "#984EA3"   # Purple
)

cond_labels <- c(
  "1" = "Sham", 
  "2" = "tVNS", 
  "3" = "No Reward", 
  "4" = "Reward"
)

# ====================================================================
# 2. BUILD THE PLOT
# ====================================================================

# x is now condition, so we can see them side-by-side in the facets
ggplot(shooting_data_plot, aes(x = condition, y = pupil_size_phasic_mean_norm, color = condition)) +
  
  # Means and error bars (line removed, no dodge needed)
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  
  # Facet by tertile
  #facet_wrap(~ pupil_tertile) +
  
  # Apply custom colors and labels
  scale_color_manual(values = cond_colors, labels = cond_labels) +
  scale_x_discrete(labels = cond_labels) +
  
  # Labels and themes
  labs(
    title = "Max Pupil Size by Condition Across Pupil State Tertiles",
    subtitle = "Extreme outliers (3x IQR) removed within-subject",
    x = "Condition",
    y = "Mean Pupil Max Size",
    color = "Condition"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray95"),
    # Tilts the x-axis labels 45 degrees so they don't overlap if names are long
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )