library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(ggeffects)
library(patchwork)

# =========================================================================
# 1. Data Loading & Clean-up
# =========================================================================
shooting = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')
shooting$stim_type = as.factor(shooting$stim_type)
shooting$stim_type = relevel(shooting$stim_type, ref = 1)

# Filter missing data and scale units to standard values
shooting = shooting[!is.nan(shooting$pupil_size_phasic_mean), ]
shooting$pupil_size_phasic_mean_norm = shooting$pupil_size_phasic_mean_norm * 1e6
shooting$pupil_size_mean = shooting$pupil_size_mean * 1e6

# Generate Within-Subject Centered Baseline
shooting <- shooting %>%
  group_by(SID) %>%
  mutate(pupil_baseline_wc = pupil_size_mean - mean(pupil_size_mean, na.rm = TRUE)) %>%
  ungroup()

# =========================================================================
# 2. Continuous Linear Mixed-Effects Model
# =========================================================================
unified_model_cont <- lmer(
  pupil_size_phasic_mean_norm ~ stim_type * ChangeSigma * pupil_baseline_wc + (1 | SID) + (1 | SID:stim_type), 
  data = shooting
)

# =========================================================================
# 3. Generate Representative Continuous Profiles (-1 SD, Mean, +1 SD)
# =========================================================================
sigma_mean <- mean(shooting$ChangeSigma, na.rm = TRUE)
sigma_sd   <- sd(shooting$ChangeSigma, na.rm = TRUE)

val_low  <- round(sigma_mean - sigma_sd, 2) 
val_mid  <- round(sigma_mean, 2)            
val_high <- round(sigma_mean + sigma_sd, 2) 

range_limits <- quantile(shooting$pupil_baseline_wc, probs = c(0.05, 0.95), na.rm = TRUE)

# FIX: Using "[all]" ensures ggpredict samples across your entire 10,000-unit scale
# so the lines are guaranteed to run fully from edge to edge.
pred_continuous <- ggpredict(
  unified_model_cont, 
  terms = c(
    "pupil_baseline_wc [all]", 
    "stim_type", 
    sprintf("ChangeSigma [%s, %s, %s]", val_low, val_mid, val_high)
  )
)

df_continuous <- as.data.frame(pred_continuous)

# Clean up factor labels for the legend
df_continuous$Condition <- factor(
  df_continuous$group, 
  levels = c("1", "2"), 
  labels = c("Sham", "Active tVNS")
)

unique_facets <- sort(as.numeric(as.character(unique(df_continuous$facet))))
df_continuous$Phenotype <- factor(
  df_continuous$facet, 
  levels = as.character(unique_facets), 
  labels = c(
    paste0("Strong Shift (\u0394\u03c3 = ", val_low, ")"),
    paste0("Mean Shift (\u0394\u03c3 = ", val_mid, ")"),
    paste0("Weak Shift (\u0394\u03c3 = ", val_high, ")")
  )
)

# =========================================================================
# 4. Symmetrical High-Fidelity Gradient Setup (Explicitly Scaled)
# =========================================================================
# FIX: Explicitly multiplying these by 1e6 ensures the shading coordinates
# match your scaled y-axis exactly, preventing the shading from disappearing.
optimal_center <- mean(subject_tertile_medians$subject_median[subject_tertile_medians$pupil_phasic_tertile == "2"], na.rm = TRUE)
t1_val         <- mean(subject_tertile_medians$subject_median[subject_tertile_medians$pupil_phasic_tertile == "1"], na.rm = TRUE)
t3_val         <- mean(subject_tertile_medians$subject_median[subject_tertile_medians$pupil_phasic_tertile == "3"], na.rm = TRUE)

n_slices <- 500
y_lower  <- seq(t1_val, optimal_center, length.out = n_slices)
y_upper  <- seq(optimal_center, t3_val, length.out = n_slices)

power_factor   <- 1 
intensity_vals <- seq(0, 1, length.out = n_slices - 1) ^ power_factor 

grad_data <- rbind(
  data.frame(ymin = y_lower[-n_slices], ymax = y_lower[-1], intensity = intensity_vals),
  data.frame(ymin = y_upper[-n_slices], ymax = y_upper[-1], intensity = rev(intensity_vals))
)

# =========================================================================
# 5. Main Trajectory Plot Panel (p1)
# =========================================================================
p1 <- ggplot(df_continuous, aes(x = x, y = predicted, color = Condition, fill = Condition)) +
  
  # A. Background Target Window Gradient
  geom_rect(data = grad_data, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, alpha = intensity), 
            fill = "#DD6B20", inherit.aes = FALSE) +
  scale_alpha_continuous(range = c(0, 0.4), guide = "none") +
  
  # B. Confidence Interval Ribbons
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high, 
   #               group = interaction(Condition, Phenotype)), 
    #          alpha = 0.15, color = NA) +
  
  # C. Main Continuous Trajectory Lines
  geom_line(aes(linetype = Phenotype), linewidth = 1.2) +
  
  # Aesthetics and Layout Locking
  scale_color_manual(values = c("Sham" = "#4A5568", "Active tVNS" = "#DD6B20")) +
  scale_fill_manual(values = c("Sham" = "#4A5568", "Active tVNS" = "#DD6B20")) +
  
  # FIX: Removed the hardcoded string names so it dynamically maps to your 3 profiles
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_x_continuous(limits = range_limits, expand = c(0, 0)) +
  ylim(-0.5, 1.2) +
  labs(
    title = "State-Dependent Neuromodulation Trajectories",
    subtitle = "Continuous interaction modeled at Mean and \u00b11 SD of \u0394\u03c3",
    x = NULL, 
    y = "Predicted Phasic Pupil Size"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "right", 
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank()
  )

# =========================================================================
# 6. Marginal Data Density Panel (p2)
# =========================================================================
p2 <- ggplot(shooting, aes(x = pupil_baseline_wc)) +
  geom_histogram(fill = "#718096", alpha = 0.6, bins = 50) +
  scale_x_continuous(limits = range_limits, expand = c(0, 0)) +
  labs(x = "Baseline Pupil Size (Mean-Centered, a.u.)", y = "Trial Count") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# =========================================================================
# 7. Layout Stacking & Render
# =========================================================================
final_plot <- p1 / p2 + plot_layout(heights = c(4, 1))
print(final_plot)