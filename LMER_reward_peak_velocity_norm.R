library(sjPlot)
library(lme4)
library(lmerTest)
library(emmeans)
df = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//reward_mean_peak_velocity_norm.csv')



# 1. Pre-processing: Ensure 'No Reward' is the first level
df$reward_type = df$Condition
df$reward_type <- factor(df$reward_type, levels = c("NO_REWARD", "REWARD"))

lmm_model <- lmer(MeanPeakVel ~ reward_type + (1 | SID), data = df)

# View the full results
tab_model(lmm_model)

# Quick interpretation:
# Look at 'Fixed effects' -> 'reward_type'Reward''. 
# The 'Estimate' is the average increase/decrease caused by the reward.
# The 'Pr(>|t|)' is your p-value.