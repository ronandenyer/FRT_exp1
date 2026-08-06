library(lme4)
library(lmerTest)
library(ggplot2)
library(ggrepel) # For non-overlapping text labels
library(dplyr)
library(sjPlot)
library(emmeans)
library(influence.ME)
library(ggeffects)
library(patchwork)
# nonlinear mixed effect model
# the model has there parameters: mu, sigma, and the asymptote, we set the chance to 1/8 as there are # eight targets. You can include chance as a parameter if needed.
# the “start" statement is the initial value of those three parameters for each group for optimization, so there are nine # # values.
shooting = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')
shooting$stim_type = as.factor(shooting$stim_type)
shooting$stim_type = relevel(shooting$stim_type, ref = 1)
shooting$block_num = as.factor(shooting$block_num)
#shooting$eff_pt = shooting$eff_pt > 0
shooting$HighShamSigma = as.factor(shooting$HighShamSigma)
shooting = shooting[!is.nan(shooting$pupil_size_phasic_mean),]
shooting$pupil_size_phasic_mean_norm = shooting$pupil_size_phasic_mean_norm * 1e6
shooting$pupil_size_phasic_mean = shooting$pupil_size_phasic_mean * 1e6
shooting$pupil_size_phasic_slope = shooting$pupil_size_phasic_slope * 1e6
shooting$pupil_size_mean = shooting$pupil_size_mean * 1e6
#shooting = shooting[shooting$SID != "FRTP30" & shooting$SID != "FRTP33",]

# Extract random effects directly from the model
re <- ranef(shooting.nlme)

# Broadcast mu.stim_type2 to all trials for each subject
shooting$ChangeMuNLME <- re[match(shooting$SID, rownames(re)), "mu.stim_type2"]
shooting$Sham_mu <- re[match(shooting$SID, rownames(re)), "mu.(Intercept)"]

shooting$pos_responder <- as.factor(ifelse(shooting$ChangeMuNLME < 0, 1, 0))
#write.csv(shooting,'C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')

base_change_cor = lm(data = shooting, ChangeMuNLME ~ Sham_mu)
tab_model(base_change_cor)

# Centering baseline pupil size (leaves the units in a.u., just shifts the mean to 0)
shooting$pupil_baseline_c <- shooting$pupil_size_mean - mean(shooting$pupil_size_mean, na.rm = TRUE)

library(dplyr)

shooting <- shooting %>%
  group_by(SID) %>%
  mutate(
    # Option 1: Within-Subject Centering 
    # (Removes between-subject mean differences, keeps original variance/units)
    pupil_baseline_ws_c = pupil_size_mean - mean(pupil_size_mean, na.rm = TRUE),
    
    # Option 2: Within-Subject Z-Scoring [RECOMMENDED]
    # (Removes mean differences AND standardizes variance, controlling for hardware scaling)
    pupil_baseline_z = (pupil_size_mean - mean(pupil_size_mean, na.rm = TRUE)) / sd(pupil_size_mean, na.rm = TRUE),
    
    # Option 3: Within-Subject Min-Max Scaling (0 to 1) 
    # (What you originally suggested, but vulnerable to extreme outliers)
    pupil_baseline_minmax = (pupil_size_mean - min(pupil_size_mean, na.rm = TRUE)) / 
      (max(pupil_size_mean, na.rm = TRUE) - min(pupil_size_mean, na.rm = TRUE))
  ) %>%
  ungroup()

# 1. Create a true Within-Subject Centered Baseline (The Fix)
shooting <- shooting %>%
  group_by(SID) %>%
  mutate(pupil_baseline_wc = pupil_size_mean - mean(pupil_size_mean, na.rm = TRUE)) %>%
  ungroup()


unified_model <- lmer(pupil_size_phasic_slope ~ stim_type * pos_responder * pupil_baseline_ws_c + (1 | SID) + (1 | SID:stim_type), 
                      data = shooting)
tab_model(unified_model)

unified_model_cont <- lmer(pupil_size_phasic_mean_norm ~ stim_type  * ChangeMuNLME * pupil_baseline_z + (1 | SID) + (1 | SID:stim_type), 
                      data = shooting)
tab_model(unified_model_cont)

baseline_diff = lmer(pupil_size_mean ~ ChangeMuNLME*stim_type + (1 | SID) + (1 | SID:stim_type), 
                     data = shooting)

tab_model(baseline_diff)

library(robustlmm)

cat("Fitting Robust LMM. This takes slightly longer than standard lmer...\n")

# Fit the model using rlmer
# The syntax is identical to your existing lmer code
robust_model <- rlmer(
  pupil_size_phasic_mean_norm ~ stim_type * pos_responder * pupil_baseline_z + 
    (1 | SID) + (1 | SID:stim_type), 
  data = shooting
)

# View the summary
summary(robust_model)
tab_model(robust_model)
anova(unified_model, unified_model_cont)

# 1. Initialize a dataframe with columns for all three effects
stability_results <- data.frame(
  Excluded_SID = character(),
  Est_3way = numeric(),          P_3way = numeric(),
  Est_Stim_Pupil = numeric(),    P_Stim_Pupil = numeric(),
  Est_Pupil_Resp = numeric(),    P_Pupil_Resp = numeric(),
  stringsAsFactors = FALSE
)

cat("Running leave-one-out models for all interactions...\n")

# 2. Loop through subjects
for (sub in subjects) {
  
  temp_data <- shooting[shooting$SID != sub, ]
  
  suppressMessages(
    temp_model <- lmer(
      pupil_size_phasic_mean_norm ~ stim_type * pos_responder * pupil_baseline_z + (1 | SID) + (1 | SID:stim_type), 
      data = temp_data
    )
  )
  
  coefs <- summary(temp_model)$coefficients
  
  # Helper function to grab row index safely
  get_idx <- function(pattern) {
    idx <- grep(pattern, rownames(coefs))
    if (length(idx) == 0) return(nrow(coefs)) # Fallback to last row if not found
    return(idx[1])
  }
  
  # Define regex patterns for the 3 interactions
  # 3-Way
  idx_3way <- get_idx("stim_type2.*pos_responder.*pupil_baseline_z|stim_type2.*pupil_baseline_z.*pos_responder")
  # 2-Way: Stim_Type * Pupil_Baseline
  idx_stim_pupil <- get_idx("stim_type2.*pupil_baseline_z|pupil_baseline_z.*stim_type2")
  # 2-Way: Pupil_Baseline * pos_responder
  idx_pupil_resp <- get_idx("pupil_baseline_z.*pos_responder|pos_responder.*pupil_baseline_z")
  
  # Save to table
  stability_results <- rbind(stability_results, data.frame(
    Excluded_SID = as.character(sub),
    Est_3way       = round(coefs[idx_3way, 1], 3),       P_3way       = round(coefs[idx_3way, 5], 4),
    Est_Stim_Pupil = round(coefs[idx_stim_pupil, 1], 3), P_Stim_Pupil = round(coefs[idx_stim_pupil, 5], 4),
    Est_Pupil_Resp = round(coefs[idx_pupil_resp, 1], 3), P_Pupil_Resp = round(coefs[idx_pupil_resp, 5], 4)
  ))
}

# 3. View Results
print(stability_results)


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

# Calculate the thresholds for the Goldilocks Zone
# We look at the distribution of subject medians for Tertile 2
goldilocks_bounds <- subject_tertile_medians %>%
  filter(pupil_phasic_tertile == "2") %>%
  summarize(
    lower_bound = quantile(subject_median, 0.05),
    upper_bound = quantile(subject_median, 0.95)
  )

gold_lower <- goldilocks_bounds$lower_bound
gold_upper <- goldilocks_bounds$upper_bound

# 1. Generate continuous predictions across the full baseline pupil range
range_limits <- quantile(shooting$pupil_baseline_z, probs = c(0.025, 0.975), na.rm = TRUE)

pred_continuous <- ggpredict(robust_model, 
                             terms = c(paste0("pupil_baseline_z [", range_limits[1], ":", range_limits[2],"by = 0.1]"), 
                                       "stim_type", 
                                       "pos_responder"))

# 2. Convert to a standard data frame
df_continuous <- as.data.frame(pred_continuous)

# Note on ggeffects mapping for this layout:
# x         = pupil_baseline_c (Tonic arousal spectrum)
# predicted = Phasic Pupil Size (Normalized)
# group     = stim_type (1 = Sham, 2 = Active tVNS) -> mapped to color
# facet     = pos_responder (0 = Negative, 1 = Positive) -> mapped to linetype

# 3. Clean up the factor labels for the legend
df_continuous$Condition <- factor(df_continuous$group, 
                                  levels = c("1", "2"), 
                                  labels = c("Sham", "Active tVNS"))

df_continuous$Phenotype <- factor(df_continuous$facet, 
                                  levels = c("1", "0"), # Ordering Positive first in legend
                                  labels = c("Positive Responders (\u0394\u03c3 < 0)", 
                                             "Negative Responders (\u0394\u03c3 \u2265 0)"))



# =========================================================================
# 1. Gradient Logic (Same as before, but mapped to Alpha)
# =========================================================================
optimal_center <- mean(subject_tertile_medians$subject_median[subject_tertile_medians$pupil_phasic_tertile == "2"], na.rm = TRUE)
t1_val <- mean(subject_tertile_medians$subject_median[subject_tertile_medians$pupil_phasic_tertile == "1"], na.rm = TRUE)
t3_val <- mean(subject_tertile_medians$subject_median[subject_tertile_medians$pupil_phasic_tertile == "3"], na.rm = TRUE)

n_slices <- 500
y_lower <- seq(t1_val, optimal_center, length.out = n_slices)
y_upper <- seq(optimal_center, t3_val, length.out = n_slices)

# Use a power factor (e.g., 0.5 for a sharp peak, 2 for a soft glow)
power_factor <- 1 

# Create linear 0-1 sequence
lin_seq <- seq(0, 1, length.out = n_slices - 1)

# Apply power transformation
# 0.5 makes the transition "steeper" at the center
intensity_vals <- lin_seq ^ power_factor 

grad_data <- rbind(
  data.frame(ymin = y_lower[-n_slices], ymax = y_lower[-1], intensity = intensity_vals),
  data.frame(ymin = y_upper[-n_slices], ymax = y_upper[-1], intensity = rev(intensity_vals))
)

# =========================================================================
# 2. Main Trajectory Plot (p1)
# =========================================================================
p1 <- ggplot(df_continuous, aes(x = x, y = predicted, color = Condition, fill = Condition)) +
  
  # A. Background Gradient using Alpha (No conflict with Ribbon Fill)
  # We set the fill to fixed color and map the intensity to alpha (transparency)
  geom_rect(data = grad_data, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, alpha = intensity), 
            fill = "#DD6B20", inherit.aes = FALSE) +
  
  # Set alpha range: 0 (invisible/white edge) to 0.4 (visible/dark orange center)
  scale_alpha_continuous(range = c(0, 0.4), guide = "none") +
  
  # B. Confidence Interval Ribbons (Mapped to Fill)
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high, 
   #               group = interaction(Condition, Phenotype)), 
    #          alpha = 0.2, color = NA) +
  
  # C. Main Trajectory Lines
  geom_line(aes(linetype = Phenotype), linewidth = 1.5) +
  
  # Aesthetics
  scale_color_manual(values = c("Sham" = "#4A5568", "Active tVNS" = "#DD6B20")) +
  scale_fill_manual(values = c("Sham" = "#4A5568", "Active tVNS" = "#DD6B20")) +
  scale_linetype_manual(values = c("Positive Responders (\u0394\u03c3 < 0)" = "solid", 
                                   "Negative Responders (\u0394\u03c3 \u2265 0)" = "dotted")) +
  
  labs(title = "State-Dependent Neuromodulation Trajectories",
       x = NULL, 
       y = "Predicted Phasic Pupil Size") +
  theme_bw(base_size = 14) +
  theme(legend.position = "right", 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank()) +
  scale_x_continuous(limits = range_limits, expand = c(0, 0)) +
  theme(legend.position = "right", 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank())

# 2. Update p2 (Replace xlim with this)
p2 <- ggplot(shooting, aes(x = pupil_baseline_z)) +
  geom_histogram(fill = "#718096", alpha = 0.6, bins = 50) +
  labs(x = "Baseline Pupil Size (Mean-Centered, a.u.)", y = "Trial Count") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank()) +
  # Add this:
  scale_x_continuous(limits = range_limits, expand = c(0, 0)) 

# 3. Stack them
final_plot <- p1 / p2 + plot_layout(heights = c(4, 1))

print(final_plot)
