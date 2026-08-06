library(lme4)
library(lmerTest)
library(ggplot2)
library(ggrepel) # For non-overlapping text labels
library(dplyr)
library(sjPlot)
library(emmeans)
# nonlinear mixed effect model
# the model has there parameters: mu, sigma, and the asymptote, we set the chance to 1/8 as there are # eight targets. You can include chance as a parameter if needed.
# the “start" statement is the initial value of those three parameters for each group for optimization, so there are nine # # values.
shooting = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')
shooting$reward_type = as.factor(shooting$reward_type)
shooting$reward_type = relevel(shooting$reward_type, ref = '3')
shooting$block_num = as.factor(shooting$block_num)
shooting$HighNoRewardSigma = as.factor(shooting$HighNoRewardSigma)
shooting = shooting[!is.nan(shooting$pupil_size_phasic_mean),]
shooting$pupil_size_phasic_mean_norm = shooting$pupil_size_phasic_mean_norm * 100000000
shooting$pupil_size_phasic_mean = shooting$pupil_size_phasic_mean * 100000000

shooting$pupil_size_mean = shooting$pupil_size_mean * 100000000
shooting$med_responder = median(shooting$ChangeSigma)
shooting$pos_responder <- as.factor(ifelse(shooting$ChangeSigma < shooting$med_responder, 1, 0))
#shooting = shooting[shooting$SID != "FRTP32",]
pupil_sigma = lmer(pupil_size_phasic_mean_norm ~ reward_type*HighNoRewardSigma*block_num + (1 | SID) + (1 | SID:reward_type) + (1 |SID:block_num), 
                   data = shooting)

tab_model(pupil_sigma)




# Centering baseline pupil size (leaves the units in a.u., just shifts the mean to 0)
shooting$pupil_baseline_c <- shooting$pupil_size_mean - mean(shooting$pupil_size_mean, na.rm = TRUE)

ceiling_model <- lmer(pupil_size_phasic_mean_norm ~ reward_type * pupil_baseline_c + (1 | SID) + (1 | SID:reward_type), 
                      data = shooting)
tab_model(ceiling_model)

unified_model <- lmer(pupil_size_phasic_mean_norm ~ reward_type * pos_responder * pupil_baseline_c + (1 | SID) + (1 | SID:reward_type), 
                      data = shooting)
tab_model(unified_model)
# 1. Generate continuous predictions across the full baseline pupil range
pred_continuous <- ggpredict(unified_model, 
                             terms = c("pupil_baseline_c [-300:400, by=5]", 
                                       "reward_type", 
                                       "pos_responder"))

# 2. Convert to a standard data frame
df_continuous <- as.data.frame(pred_continuous)

# Note on ggeffects mapping for this layout:
# x         = pupil_baseline_c (Tonic arousal spectrum)
# predicted = Phasic Pupil Size (Normalized)
# group     = reward_type (1 = Sham, 2 = Active tVNS) -> mapped to color
# facet     = pos_responder (0 = Negative, 1 = Positive) -> mapped to linetype

# 3. Clean up the factor labels for the legend
df_continuous$Condition <- factor(df_continuous$group, 
                                  levels = c("3", "4"), 
                                  labels = c("No Reward", "Reward"))

df_continuous$Phenotype <- factor(df_continuous$facet, 
                                  levels = c("1", "0"), # Ordering Positive first in legend
                                  labels = c("Positive Responders (\u0394\u03c3 < 0)", 
                                             "Negative Responders (\u0394\u03c3 \u2265 0)"))

# 4. Build the single-panel plot
reward_base_phasic_plot = ggplot(df_continuous, aes(x = x, y = predicted, color = Condition, linetype = Phenotype)) +
  # Model interaction lines (thickened slightly for contrast without CIs)
  geom_line(linewidth = 1.5) +
  
  # High-contrast manuscript colors
  scale_color_manual(values = c("No Reward" = "#4A5568", "Reward" = "#DD6B20")) +
  
  # Explicitly force solid vs dashed lines
  scale_linetype_manual(values = c("Positive Responders (\u0394\u03c3 < 0)" = "solid", 
                                   "Negative Responders (\u0394\u03c3 \u2265 0)" = "dotted")) +
  
  # Labels and axes
  labs(
    title = "State-Dependent Neuromodulation Trajectories",
    subtitle = "3-Way Interaction: Reward \u00d7 Responder Phenotype \u00d7 Baseline State",
    x = "Baseline Pupil Size (Mean-Centered, a.u.)",
    y = "Predicted Phasic Pupil Size (Normalized)",
    color = "Condition",
    linetype = "Behavioral Phenotype"
  ) +
  ylim(-200,230) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(reward_base_phasic_plot)