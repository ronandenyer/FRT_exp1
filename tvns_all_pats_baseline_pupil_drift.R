library(lme4)
library(lmerTest)
library(ggplot2)
library(ggrepel) # For non-overlapping text labels
library(dplyr)
# nonlinear mixed effect model
# the model has there parameters: mu, sigma, and the asymptote, we set the chance to 1/8 as there are # eight targets. You can include chance as a parameter if needed.
# the “start" statement is the initial value of those three parameters for each group for optimization, so there are nine # # values.
shooting = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')
shooting$stim_type = as.factor(shooting$stim_type)
shooting$stim_type = relevel(shooting$stim_type, ref = 1)
shooting = shooting[!is.nan(shooting$pupil_size_mean),]
shooting$pupil_size_mean = shooting$pupil_size_mean * 100000000

pupil_drift = lmer(pupil_size_mean ~ trial_num*stim_type + (1 | SID), 
                   data = shooting)
pupil_drift_block = lmer(pupil_size_mean ~ trial_num*stim_type + block_num + (1 | SID), 
                   data = shooting)

anova(pupil_drift, pupil_drift_block)

tab_model(pupil_drift_block)

shooting = shooting[shooting$block_num == 4,]

ggplot(shooting, aes(x = trial_num, y = pupil_size_mean, color = stim_type, fill = stim_type)) +
  # na.rm = TRUE silences the missing values warning safely
  geom_point(alpha = 0.2, size = 1.5, na.rm = TRUE) + 
  
  # Changed 'size' to 'linewidth' to fix the deprecation warning
  geom_smooth(method = "lm", formula = y ~ x, linewidth = 1.2, alpha = 0.15, na.rm = TRUE) +
  
  labs(    
    x = "Trial Number",
    y = "Mean Pupil Size",
    color = "Condition",
    fill = "Condition",
    title = "Pupil Size Drift over Trials"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  ) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2")


ggplot(shooting, aes(x = as.factor(block_num), y = pupil_size_mean, color = stim_type, group = stim_type)) +
  # Add points and error bars (Standard Error)
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, position = position_dodge(0.1)) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge(0.1)) +
  # Connect the points with lines to show the trend across blocks
  stat_summary(fun = mean, geom = "line", size = 1, position = position_dodge(0.1)) +
  
  scale_color_manual(values = c("1" = "#4A90E2", "2" = "#808080")) +
  labs(
    title = "Mean Baseline Size by Block and Stimulation",
    x = "Block Number",
    y = "Mean Phasic Pupil Size (Raw)",
    color = "Stimulation"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    legend.position = "top"
  )