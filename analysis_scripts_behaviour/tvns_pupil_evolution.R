# -------------------------------------------------------------------------
# 1. SETUP & DATA CLEANING (tVNS Experiment)
# -------------------------------------------------------------------------
library(nlme)
library(lmerTest)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(patchwork)

# Load Data
shooting <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')

# Factorize and level conditions (1 = Sham)
shooting$stim_type <- as.factor(shooting$stim_type)
shooting$stim_type <- relevel(shooting$stim_type, ref = 1) 

# Remove NAs
shooting <- shooting[!is.na(shooting$eff_pt) & 
                       !is.na(shooting$success) & 
                       !is.na(shooting$pupil_size_mean), ]

# Standardize units
shooting$eff_pt <- shooting$eff_pt / 1000 # Convert to seconds
shooting$pupil_size_phasic_mean_norm <- shooting$pupil_size_phasic_mean_norm * 1e4
shooting$pupil_size_phasic_mean <- shooting$pupil_size_phasic_mean * 1e4
shooting$pupil_size_pre_phasic_mean_norm <- shooting$pupil_size_pre_phasic_mean_norm * 1e4
shooting$pupil_size_pre_phasic_mean <- shooting$pupil_size_pre_phasic_mean * 1e4
shooting$pupil_size_phasic_slope <- shooting$pupil_size_phasic_slope * 1e4
shooting$pupil_size_max <- shooting$pupil_size_max * 1e4
shooting$prev_trial_success <- as.factor(shooting$prev_trial_success)
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

# Trimmed dataset
shooting_trimmed <- shooting %>%
  filter(
    abs(pupil_state_scaled) <= 3,
    abs(pupil_slope_scaled) <= 3,
    abs(pupil_max_scaled) <= 3,
    abs(vigor_scaled) <= 3
  )

# -------------------------------------------------------------------------
# 3. LINEAR MIXED MODELS (tVNS)
# -------------------------------------------------------------------------
shooting_trimmed <- shooting_trimmed %>%
  group_by(SID) %>%
  mutate(stim_previous = lag(stim_type)) %>%
  ungroup()

shooting_prev = shooting_trimmed[!is.nan(shooting_trimmed$prev_trial_success), ]

pupil_state_tvns <- lmer(
  pupil_max ~ stim_previous * prev_trial_success + (1 | SID), 
  data = shooting_prev
)
summary(pupil_state_tvns)

pupil_pre_phasic_tvns <- lmer(
  pupil_size_pre_phasic_mean_norm ~ stim_type + (1 | SID), 
  data = shooting
)
summary(pupil_pre_phasic_tvns)

pupil_phasic_tvns <- lmer(
  pupil_size_phasic_mean_norm ~ stim_type + (1 | SID), 
  data = shooting
)
summary(pupil_phasic_tvns)

pupil_pre_cue_tvns <- lmer(
  pupil_size_mean ~ stim_type + (1 | SID), 
  data = shooting
)
summary(pupil_pre_cue_tvns)

pupil_slope_tvns <- lmer(
  pupil_size_phasic_slope ~ stim_type + (1 | SID), 
  data = shooting
)
summary(pupil_slope_tvns)

# -------------------------------------------------------------------------
# 4. VISUALIZATION (Combined Patchwork Panel)
# -------------------------------------------------------------------------

# ==========================================
# USER CONFIGURATION: Colors & Layout
# ==========================================
condition_colors <- c(
  "Sham" = "gray25", 
  "tVNS" = "#3498db"
)
dodge_width <- 0.6

# --- PLOT 2: Normalized Dynamics (Phasic Mean, 50% width) ---
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
    condition_name = if_else(stim_type == 1, "Sham", "tVNS")
  )

within_subject_means <- plot_shooting %>%
  group_by(SID, metric_label, stim_type, condition_name) %>%
  summarise(mean_pupil = mean(pupil_value, na.rm = TRUE), .groups = "drop")

jd <- position_jitterdodge(jitter.width = 0.12, dodge.width = dodge_width)
y_max <- max(within_subject_means$mean_pupil, na.rm = TRUE)
y_range <- y_max - min(within_subject_means$mean_pupil, na.rm = TRUE)
bracket_y <- y_max + (y_range * 0.05)
tick_len <- y_range * 0.015
shift <- dodge_width / 4 

phasic_mean <- ggplot(within_subject_means, aes(x = metric_label, y = mean_pupil, color = condition_name, group = condition_name)) +
  geom_point(position = jd, alpha = 0.6, size = 2.5) +
  stat_summary(
    fun.data = mean_se,
    geom = "crossbar",
    position = position_dodge(width = dodge_width),
    width = 0.25,
    fatten = 1.5,
    show.legend = FALSE
  ) +
  geom_segment(aes(x = 1 - shift, xend = 1 + shift, y = bracket_y, yend = bracket_y), color = "black", inherit.aes = FALSE) +
  geom_segment(aes(x = 2 - shift, xend = 2 + shift, y = bracket_y, yend = bracket_y), color = "black", inherit.aes = FALSE) +
  geom_segment(aes(x = 1 - shift, xend = 1 - shift, y = bracket_y, yend = bracket_y - tick_len), color = "black", inherit.aes = FALSE) +
  geom_segment(aes(x = 1 + shift, xend = 1 + shift, y = bracket_y, yend = bracket_y - tick_len), color = "black", inherit.aes = FALSE) +
  geom_segment(aes(x = 2 - shift, xend = 2 - shift, y = bracket_y, yend = bracket_y - tick_len), color = "black", inherit.aes = FALSE) +
  geom_segment(aes(x = 2 + shift, xend = 2 + shift, y = bracket_y, yend = bracket_y - tick_len), color = "black", inherit.aes = FALSE) +
  geom_text(aes(x = 1, y = bracket_y + (y_range * 0.015), label = "*"), color = "black", fontface = "bold", size = 4.5, inherit.aes = FALSE) +
  geom_text(aes(x = 2, y = bracket_y + (y_range * 0.015), label = "*"), color = "black", fontface = "bold", size = 4.5, inherit.aes = FALSE) +
  scale_color_manual(values = condition_colors) +
  expand_limits(y = y_max + (y_range * 0.18)) +
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

# --- PLOT 1: Pre-Cue Mean (25% width) ---
df_mean <- shooting %>%
  mutate(condition_name = if_else(stim_type == 1, "Sham", "tVNS"))

within_subject_mean <- df_mean %>%
  group_by(SID, stim_type, condition_name) %>%
  summarise(mean_pupil = mean(pupil_size_mean, na.rm = TRUE), .groups = "drop")

pre_cue_mean <- ggplot(within_subject_mean, aes(x = condition_name, y = mean_pupil, color = condition_name)) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 2.5) +
  stat_summary(
    fun.data = mean_se,
    geom = "crossbar",
    width = 0.3,
    fatten = 1.5,
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
    axis.title.y = element_text(face = "bold", margin = margin(r = 10))
  )

# --- PLOT 3: Slope / Velocity Plot (25% width) ---
df_slope <- shooting %>%
  mutate(condition_name = if_else(stim_type == 1, "Sham", "tVNS"))

within_subject_slope <- df_slope %>%
  group_by(SID, stim_type, condition_name) %>%
  summarise(mean_pupil = mean(pupil_size_phasic_slope, na.rm = TRUE), .groups = "drop")

slope_plot <- ggplot(within_subject_slope, aes(x = condition_name, y = mean_pupil, color = condition_name)) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 2.5) +
  stat_summary(
    fun.data = mean_se,
    geom = "crossbar",
    width = 0.3,
    fatten = 1.5,
    show.legend = FALSE
  ) +
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
    axis.title.y = element_text(face = "bold", margin = margin(r = 10))
  )

# -------------------------------------------------------------------------
# 5. COMBINE AND SAVE (Patchwork)
# -------------------------------------------------------------------------
combined_layout <- (pre_cue_mean + phasic_mean + slope_plot) +
  plot_layout(widths = c(1, 2, 1), guides = "collect") &
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

# Render composite plot
print(combined_layout)

# Save the composite plot as a 300 DPI high-resolution PNG
ggsave(
  filename = "tvns_pupil_dynamics_combined_plot.png",
  plot = combined_layout,
  width = 1050 * (300 / 96),
  height = 450 * (300 / 96),
  dpi = 300,
  units = "px"
)