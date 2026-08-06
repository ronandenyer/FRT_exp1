# -------------------------------------------------------------------------
# 1. SETUP & DATA CLEANING
# -------------------------------------------------------------------------
library(nlme)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(ggsignif)

# Load data
shooting <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')

# Remove specific subject and set up factors
shooting$reward_type <- as.factor(shooting$reward_type - 2)
shooting$reward_type <- relevel(shooting$reward_type, ref = "1") # Set baseline reference

# Remove NAs
shooting <- shooting[!is.na(shooting$eff_pt) & 
                       !is.na(shooting$success) & 
                       !is.na(shooting$pupil_size_mean), ]

# Standardize units (Fixed double division of eff_pt)
shooting$eff_pt <- shooting$eff_pt / 1000 # Convert to seconds
shooting$pupil_size_phasic_mean_norm <- shooting$pupil_size_phasic_mean_norm * 1e4
shooting$pupil_size_phasic_mean <- shooting$pupil_size_phasic_mean * 1e4
shooting$pupil_size_pre_phasic_mean_norm <- shooting$pupil_size_pre_phasic_mean_norm * 1e4
shooting$pupil_size_pre_phasic_mean <- shooting$pupil_size_pre_phasic_mean * 1e4
shooting$pupil_size_phasic_slope <- shooting$pupil_size_phasic_slope * 1e4
shooting$pupil_size_max <- shooting$pupil_size_max * 1e4
shooting$prev_trial_success = as.factor(shooting$prev_trial_success)
shooting$pupil_size_mean <- shooting$pupil_size_mean * 1e4

# -------------------------------------------------------------------------
# 2. WITHIN-SUBJECT CENTERING & SCALING
# -------------------------------------------------------------------------
shooting <- shooting %>%
  group_by(SID) %>%
  mutate(
    pupil_state = pupil_size_mean - mean(pupil_size_mean, na.rm = TRUE),
    pupil_phasic = pupil_size_phasic_mean_norm - mean(pupil_size_phasic_mean_norm, na.rm = TRUE),
    pupil_pre_phasic = pupil_size_pre_phasic_mean_norm - mean(pupil_size_pre_phasic_mean_norm, na.rm = TRUE),
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

# Trimmed dataset (Generated but held in reserve, models use 'shooting')
shooting_trimmed <- shooting %>%
  filter(
    abs(pupil_state_scaled) <= 3,
    abs(pupil_slope_scaled) <= 3,
    abs(pupil_max_scaled) <= 3,
    abs(vigor_scaled) <= 3
  )


# -------------------------------------------------------------------------
# 2B. MAIN EFFECTS OF reward ON MAIN VARIABLES
# -------------------------------------------------------------------------
library(dplyr)

shooting_trimmed <- shooting_trimmed %>%
  group_by(SID) %>%
  # lag() automatically shifts the column down by 1 
  # and correctly inserts NA for the first trial of every participant
  mutate(reward_previous = lag(reward_type)) %>%
  ungroup()# change in baseline pupil (mean average 1 second before reward starts)

shooting_prev = shooting_trimmed[!is.nan(shooting_trimmed$prev_trial_success),]
pupil_state_reward <- lmer(
  pupil_max ~ reward_previous*prev_trial_success + 
    (1  | SID), 
  data = shooting_prev
)

summary(pupil_state_reward)

# change in max pupil during phasic ramp (1.5 second window prior to GO cue)

pupil_pre_phasic_reward <- lmer(
  pupil_size_pre_phasic_mean_norm ~ reward_type  + 
    (1 | SID), 
  data = shooting
)

summary(pupil_pre_phasic_reward)

pupil_phasic_reward <- lmer(
  pupil_size_phasic_mean_norm ~ reward_type  + 
    (1 | SID), 
  data = shooting
)

summary(pupil_phasic_reward)

pupil_pre_cue_reward <- lmer(
  pupil_size_mean ~ reward_type  + 
    (1 | SID), 
  data = shooting
)

summary(pupil_pre_cue_reward)

pupil_slope_reward <- lmer(
  pupil_size_phasic_slope ~ reward_type  + 
    (1 | SID), 
  data = shooting
)

summary(pupil_slope_reward)

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyr)

# ==========================================
# USER CONFIGURATION: Colors & Layout
# ==========================================
condition_colors <- c(
  "No Reward" = "gray25", 
  "Reward"    = "#ff5050"
)
dodge_width <- 0.6
library(ggplot2)
library(dplyr)
library(tidyr)

# ==========================================
# USER CONFIGURATION: Colors & Layout
# ==========================================
condition_colors <- c(
  "No Reward" = "gray25", 
  "Reward"    = "#ff5050"
)
dodge_width <- 0.6

# 1. Reshape the data and match the custom x-axis labels in both if_else and levels
plot_shooting <- shooting %>%
  pivot_longer(
    cols = c(pupil_size_pre_phasic_mean_norm, pupil_size_phasic_mean_norm),
    names_to = "pupil_metric",
    values_to = "pupil_value"
  ) %>%
  mutate(
    metric_label = factor(
      if_else(pupil_metric == "pupil_size_pre_phasic_mean_norm", "Prep Tone\n-1s", "Prep Tone\n+1s"),
      levels = c("Prep Tone\n-1s", "Prep Tone\n+1s")
    ),
    condition_name = if_else(reward_type == 1, "No Reward", "Reward")
  )

# 2. Calculate within-subject means for each condition
within_subject_means <- plot_shooting %>%
  group_by(SID, metric_label, reward_type, condition_name) %>%
  summarise(mean_pupil = mean(pupil_value, na.rm = TRUE), .groups = "drop")

# 3. Define layout positioning for side-by-side dodging
jd <- position_jitterdodge(jitter.width = 0.12, dodge.width = dodge_width)
# Calculate dynamic height bounds for the significance brackets
y_max <- max(within_subject_means$mean_pupil, na.rm = TRUE)
y_range <- y_max - min(within_subject_means$mean_pupil, na.rm = TRUE)
bracket_y <- y_max + (y_range * 0.05)
tick_len <- y_range * 0.015

# Offset matching the dodge width (centers the brackets over the two dodged points)
shift <- dodge_width / 4 

# 4. Generate the visualization with manual significance brackets
phasic_mean <- ggplot(within_subject_means, aes(x = metric_label, y = mean_pupil, color = condition_name, group = condition_name)) +
  
  # Within-subject means as side-by-side jittered points
  geom_point(position = jd, alpha = 0.6, size = 2.5) +
  
  # Separate crossbars for each condition/metric combination
  stat_summary(
    fun.data = mean_se,
    geom = "crossbar",
    position = position_dodge(width = dodge_width),
    width = 0.25,
    fatten = 1.5,
    show.legend = FALSE
  ) +
  
  # --- MANUAL SIGNIFICANCE BRACKETS ---
  # Horizontal bars for Prep Tone -1s (x=1) and Prep Tone +1s (x=2)
  geom_segment(aes(x = 1 - shift, xend = 1 + shift, y = bracket_y, yend = bracket_y), color = "black", inherit.aes = FALSE) +
  geom_segment(aes(x = 2 - shift, xend = 2 + shift, y = bracket_y, yend = bracket_y), color = "black", inherit.aes = FALSE) +
  
  # Left and right vertical ticks ("feet") for -1s bracket
  geom_segment(aes(x = 1 - shift, xend = 1 - shift, y = bracket_y, yend = bracket_y - tick_len), color = "black", inherit.aes = FALSE) +
  geom_segment(aes(x = 1 + shift, xend = 1 + shift, y = bracket_y, yend = bracket_y - tick_len), color = "black", inherit.aes = FALSE) +
  
  # Left and right vertical ticks for +1s bracket
  geom_segment(aes(x = 2 - shift, xend = 2 - shift, y = bracket_y, yend = bracket_y - tick_len), color = "black", inherit.aes = FALSE) +
  geom_segment(aes(x = 2 + shift, xend = 2 + shift, y = bracket_y, yend = bracket_y - tick_len), color = "black", inherit.aes = FALSE) +
  
  # Asterisk annotations
  geom_text(aes(x = 1, y = bracket_y + (y_range * 0.015), label = "*"), color = "black", fontface = "bold", size = 4.5, inherit.aes = FALSE) +
  geom_text(aes(x = 2, y = bracket_y + (y_range * 0.015), label = "*"), color = "black", fontface = "bold", size = 4.5, inherit.aes = FALSE) +
  # -----------------------------------

# Custom configurable color palette
scale_color_manual(values = condition_colors) +
  
  # Expand y-axis limits slightly to ensure brackets aren't clipped
  expand_limits(y = y_max + (y_range * 0.18)) +
  
  # Styling and labels
  labs(
    x = "Time",
    y = expression("Pupil Size "[Baseline~corrected]*" (a.u.)"),
    color = "Condition"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10))
  )

print(phasic_mean)
library(ggplot2)
library(dplyr)

# ==========================================
# USER CONFIGURATION: Colors & Layout
# ==========================================
condition_colors <- c(
  "No Reward" = "gray25", 
  "Reward"    = "#ff5050"
)

# 1. Prepare data and map condition labels
df_mean <- shooting %>%
  mutate(condition_name = if_else(reward_type == 1, "No Reward", "Reward"))

# 2. Calculate within-subject means for each condition
within_subject_mean <- df_mean %>%
  group_by(SID, reward_type, condition_name) %>%
  summarise(mean_pupil = mean(pupil_size_mean, na.rm = TRUE), .groups = "drop")

# 3. Generate the visualization
pre_cue_mean = ggplot(within_subject_mean, aes(x = condition_name, y = mean_pupil, color = condition_name)) +
  # Within-subject means as jittered points
  geom_jitter(width = 0.15, alpha = 0.6, size = 2.5) +
  
  # Group mean crossbar with standard error
  stat_summary(
    fun.data = mean_se,
    geom = "crossbar",
    width = 0.3,
    fatten = 1.5,
    show.legend = FALSE
  ) +
  
  # Custom configurable color palette
  scale_color_manual(values = condition_colors) +
  
  # Styling and labels
  labs(
    x = "Condition",
    y = expression("Pupil Size (a.u.)"),
    color = "Condition"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10))
  )

library(ggplot2)
library(dplyr)

# ==========================================
# USER CONFIGURATION: Colors & Layout
# ==========================================
condition_colors <- c(
  "No Reward" = "gray25", 
  "Reward"    = "#ff5050"
)

# 1. Prepare data and map condition labels
df_slope <- shooting %>%
  mutate(condition_name = if_else(reward_type == 1, "No Reward", "Reward"))

# 2. Calculate within-subject means for each condition
within_subject_slope <- df_slope %>%
  group_by(SID, reward_type, condition_name) %>%
  summarise(mean_pupil = mean(pupil_size_phasic_slope, na.rm = TRUE), .groups = "drop")

# 3. Generate the visualization
slope_plot = ggplot(within_subject_slope, aes(x = condition_name, y = mean_pupil, color = condition_name)) +
  # Within-subject means as jittered points
  geom_jitter(width = 0.15, alpha = 0.6, size = 2.5) +
  
  # Group mean crossbar with standard error
  stat_summary(
    fun.data = mean_se,
    geom = "crossbar",
    width = 0.3,
    fatten = 1.5,
    show.legend = FALSE
  ) +
  
  # Custom configurable color palette
  scale_color_manual(values = condition_colors) +
  
  # Styling and labels
  labs(
    x = "Condition",
    y = expression("Pupil Size Rate of Change"[Baseline~corrected]*" (a.u.)"),
    color = "Condition"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10))
  )

library(patchwork)
combined_layout <- (pre_cue_mean + phasic_mean + slope_plot) +
  plot_layout(widths = c(1, 2, 1), guides = "collect") &
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

# Render the final composite plot
print(combined_layout)

# Save the composite plot as a 300 DPI high-resolution PNG
ggsave(
  filename = "pupil_dynamics_combined_plot.png",
  plot = combined_layout,
  width = 1050*(300/96),
  height = 450*(300/96),
  dpi = 300,
  units = "px"
)