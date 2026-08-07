library(ggplot2)
library(ggeffects)
library(dplyr)

# --- REQUIRED: Ensure you have your model object loaded ---
# prev_abs_trial_pupil_tvns <- lmer(...)

# 1. Extract marginal effects for the population average trajectories
pred_pop <- ggpredict(
  prev_abs_trial_pupil_tvns, 
  terms = c("trial_num_small [all]", "stim_type")
)

# 2. Add labeling for the condition (matches pred_pop group)
# We do this for the original data so it matches the legend
shooting_data_plot <- shooting_pre_pupil %>%
  mutate(condition_labeled = factor(stim_type, levels=c(1,2), labels=c("Sham", "tVNS")))

# 3. Build the plot with ALL individual trial points included
ggplot() +
  
  # --- THE INDIVIDUAL TRIAL POINTS (RAW DATA) ---
  # We specify the actual raw dataset here
  # alpha = 0.05 is mandatory for N > 10,000 to manage overplotting
  geom_point(
    data = shooting_data_plot,
    aes(x = trial_num_small, y = pupil_state, color = condition_labeled),
    alpha = 0.05,
    size = 0.8
  ) +
  
  # --- OVERLAID: Population average 95% CI ribbons (from ggpredict) ---
  # We plot the ribbons underneath the lines for better visibility
  geom_ribbon(
    data = pred_pop,
    aes(x = x, ymin = conf.low, ymax = conf.high, fill = factor(group)),
    alpha = 0.25,
    color = NA
  ) +
  
  # --- OVERLAID: Population average trajectories (bold lines) ---
  geom_line(
    data = pred_pop,
    aes(x = x, y = predicted, color = factor(group)),
    size = 1.4
  ) +
  
  # --- Colors & Scales ---
  # Ensures colors match and are labeled correctly (Sham = Blue, tVNS = Orange)
  scale_color_manual(
    values = c("Sham" = "#2B5C8F", "tVNS" = "#D95F02", "1" = "#2B5C8F", "2" = "#D95F02"),
    breaks = c("Sham", "tVNS"),
    labels = c("Sham", "tVNS")
  ) +
  scale_fill_manual(
    values = c("1" = "#2B5C8F", "2" = "#D95F02"),
    labels = c("Sham", "tVNS")
  ) +
  
  # --- Labels & Theme ---
  labs(
    x = "Trial Number (Rescaled)",
    y = "Pupil Baseline State",
    title = "Trial-Level Pupil State (Individual Data + Population Fit)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.title = element_blank()
  )