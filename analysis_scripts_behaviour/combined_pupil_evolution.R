# -------------------------------------------------------------------------
# 1. LOAD, INTEGRATE, AND CLEAN DATA FROM BOTH EXPERIMENTS
# -------------------------------------------------------------------------
library(nlme)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(grid)

# Load individual files
reward_data <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')
tvns_data   <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')

# Safely map reward conditions using native reward_type
reward_data <- reward_data %>%
  mutate(
    condition_name = if_else(reward_type == min(reward_type, na.rm = TRUE), "No Reward", "Reward")
  )

# Safely map tVNS conditions using native stim_type
tvns_data <- tvns_data %>%
  mutate(
    condition_name = if_else(stim_type == min(stim_type, na.rm = TRUE), "Sham", "tVNS")
  )

# Combine datasets into a single master dataframe
shooting <- bind_rows(reward_data, tvns_data)

# Set explicit factor ordering for all 4 conditions
shooting$condition_name <- factor(
  shooting$condition_name, 
  levels = c("No Reward", "Reward", "Sham", "tVNS")
)

# Remove NAs
shooting <- shooting[!is.na(shooting$eff_pt) & 
                       !is.na(shooting$success) & 
                       !is.na(shooting$pupil_size_mean), ]

# Standardize units
shooting$eff_pt <- shooting$eff_pt / 1000 
shooting$pupil_size_phasic_mean_norm <- shooting$pupil_size_phasic_mean_norm * 1e4
shooting$pupil_size_phasic_mean <- shooting$pupil_size_phasic_mean * 1e4
shooting$pupil_size_pre_phasic_mean_norm <- shooting$pupil_size_pre_phasic_mean_norm * 1e4
shooting$pupil_size_pre_phasic_mean <- shooting$pupil_size_pre_phasic_mean * 1e4
shooting$pupil_size_phasic_slope <- shooting$pupil_size_phasic_slope * 1e4
shooting$pupil_size_max <- shooting$pupil_size_max * 1e4
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

# -------------------------------------------------------------------------
# 3. VISUALIZATION (4-Condition Panel Layout)
# -------------------------------------------------------------------------

condition_colors <- c(
  "No Reward" = "gray10", 
  "Reward"    = "#e74c3c",
  "Sham"      = "#3498db",
  "tVNS"      = "green"
)
dodge_width <- 0.8

# --- PLOT 2: Normalized Dynamics (Phasic Mean, 50% width) ---
plot_shooting <- shooting %>%
  filter(!is.na(condition_name)) %>%
  pivot_longer(
    cols = c(pupil_size_pre_phasic_mean_norm, pupil_size_phasic_mean_norm),
    names_to = "pupil_metric",
    values_to = "pupil_value"
  ) %>%
  mutate(
    metric_label = factor(
      if_else(pupil_metric == "pupil_size_pre_phasic_mean_norm", "Prep Tone\n-1s", "Prep Tone\n+1s"),
      levels = c("Prep Tone\n-1s", "Prep Tone\n+1s")
    )
  )

within_subject_means <- plot_shooting %>%
  group_by(SID, metric_label, condition_name) %>%
  summarise(mean_pupil = mean(pupil_value, na.rm = TRUE), .groups = "drop")

jd <- position_jitterdodge(jitter.width = 0.05, dodge.width = dodge_width)

phasic_mean <- ggplot(within_subject_means, aes(x = metric_label, y = mean_pupil, color = condition_name, group = condition_name)) +
  geom_point(position = jd, alpha = 0.5, size = 2) +
  stat_summary(
    fun.data = mean_se,
    geom = "crossbar",
    position = position_dodge(width = dodge_width),
    width = 0.2,
    fatten = 1.2,
    show.legend = TRUE
  ) +
  scale_color_manual(values = condition_colors) +
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
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(face = "bold")
  )

# --- PLOT 1: Pre-Cue Mean (25% width) ---
within_subject_mean <- shooting %>%
  filter(!is.na(condition_name)) %>%
  group_by(SID, condition_name) %>%
  summarise(mean_pupil = mean(pupil_size_mean, na.rm = TRUE), .groups = "drop")

pre_cue_mean <- ggplot(within_subject_mean, aes(x = condition_name, y = mean_pupil, color = condition_name)) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 2) +
  stat_summary(
    fun.data = mean_se,
    geom = "crossbar",
    width = 0.4,
    fatten = 1.2,
    show.legend = FALSE
  ) +
  scale_color_manual(values = condition_colors) +
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
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 25, hjust = 1, face = "bold")
  )

# --- PLOT 3: Slope / Velocity Plot (25% width) ---
within_subject_slope <- shooting %>%
  filter(!is.na(condition_name)) %>%
  group_by(SID, condition_name) %>%
  summarise(mean_pupil = mean(pupil_size_phasic_slope, na.rm = TRUE), .groups = "drop")

bar_y <- max(within_subject_slope$mean_pupil, na.rm = TRUE) * 1.15

slope_plot <- ggplot(within_subject_slope, aes(x = condition_name, y = mean_pupil, color = condition_name)) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 2) +
  stat_summary(
    fun.data = mean_se,
    geom = "crossbar",
    width = 0.4,
    fatten = 1.2,
    show.legend = FALSE
  ) +
  # Horizontal bracket spanning from x = 1.5 to x = 3.5
  annotate("segment", x = 1.5, xend = 3.5, y = bar_y, yend = bar_y, color = "black", linewidth = 0.8) +
  annotate("segment", x = 1.5, xend = 1.5, y = bar_y, yend = bar_y * 0.97, color = "black", linewidth = 0.8) +
  annotate("segment", x = 3.5, xend = 3.5, y = bar_y, yend = bar_y * 0.97, color = "black", linewidth = 0.8) +
  annotate("text", x = 2.5, y = bar_y * 1.02, label = "*", size = 6, fontface = "bold", hjust = 0.5) +
  
  scale_color_manual(values = condition_colors) +
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
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 25, hjust = 1, face = "bold")
  )

# -------------------------------------------------------------------------
# 4. COMBINE AND SAVE (Patchwork)
# -------------------------------------------------------------------------
combined_layout <- (pre_cue_mean + phasic_mean + slope_plot) +
  plot_layout(widths = c(1, 2, 1), guides = "collect") &
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

# Render composite plot
print(combined_layout)

# Save the composite plot
ggsave(
  filename = "four_condition_pupil_dynamics_plot.png",
  plot = combined_layout,
  width = 1400 * (300 / 96),
  height = 450 * (300 / 96),
  dpi = 300,
  units = "px"
)