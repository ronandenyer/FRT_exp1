library(nlme)
library(sjPlot)
library(ggplot2)
library(ggrepel) # For non-overlapping text labels
library(dplyr)
# nonlinear mixed effect model
# the model has there parameters: mu, sigma, and the asymptote, we set the chance to 1/8 as there are # eight targets. You can include chance as a parameter if needed.
# the “start" statement is the initial value of those three parameters for each group for optimization, so there are nine # # values.
shooting = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')
shooting$stim_type = as.factor(shooting$reward_type)
shooting$stim_type = relevel(shooting$stim_type, ref = 1)
shooting$abs_angle = abs(shooting$motion_angle_rot - 90)
shooting = shooting[shooting$eff_pt > 0 & !is.nan(shooting$eff_pt),]
shooting_hit = shooting[shooting$success==1,]
model_acc <- lmer(abs_angle ~ reward_type + (1 | SID), data = shooting_hit)

tab_model(model_acc)