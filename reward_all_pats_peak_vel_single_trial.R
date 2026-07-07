library(sjPlot)
library(lme4)
library(lmerTest)
library(emmeans)
library(dplyr)
# nonlinear mixed effect model
# the model has there parameters: mu, sigma, and the asymptote, we set the chance to 1/8 as there are # eight targets. You can include chance as a parameter if needed.
# the “start" statement is the initial value of those three parameters for each group for optimization, so there are nine # # values.
shooting = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')
shooting$reward_type = as.factor(shooting$reward_type)
shooting$eff_pt = shooting$eff_pt/1000
#shooting$success = as.factor(shooting$success)
#shooting_med_prep <- shooting %>%
  #filter(eff_pt < 0.3) 
# Note: You can use a comma instead of '&' in filter()


peak_vel_error = lmer(peak_tanVel ~ reward_type*success + (1 | SID) + (1|SID:reward_type), data = shooting_med_prep)

tab_model(peak_vel_error)

success_rate = glmer(success ~ reward_type + (1 | SID) + (1 | SID:reward_type), 
                       data = shooting, 
                       family = binomial)

tab_model(success_rate)


eff_pt_model = lmer(eff_pt ~ reward_type + (1 | SID) + (1|SID:reward_type), data = shooting)

tab_model(eff_pt_model)