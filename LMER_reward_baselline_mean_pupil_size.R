library(sjPlot)
library(lme4)
library(lmerTest)
library(emmeans)
df = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment/reward_pupil_means.csv')


df$pre_cue_mean_pupil_size = df$pre_cue_mean_pupil_size * 1000000
df$post_cue_norm_mean_pupil_size = df$post_cue_norm_mean_pupil_size * 1000000
# 1. Pre-processing: Ensure 'No Reward' is the first level
# 1. Pre-processing: Ensure 'No Reward' is the first level
df$stim_condition <- factor(df$reward_condition, levels = c(3,4))

lmm_model <- lmer(pre_cue_mean_pupil_size ~ reward_condition + (1 | SID), data = df)

# View the full results
tab_model(lmm_model)

# Quick interpretation:
# Look at 'Fixed effects' -> 'reward_type'Reward''. 
# The 'Estimate' is the average increase/decrease caused by the reward.
# The 'Pr(>|t|)' is your p-value.