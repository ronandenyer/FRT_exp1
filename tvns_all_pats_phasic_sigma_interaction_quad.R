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
shooting = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')
shooting$stim_type = as.factor(shooting$stim_type)
shooting$stim_type = relevel(shooting$stim_type, ref = 1)
shooting$block_num = as.factor(shooting$block_num)
shooting$HighShamSigma = as.factor(shooting$HighShamSigma)
shooting = shooting[!is.nan(shooting$pupil_size_phasic_mean),]
shooting$pupil_size_phasic_mean_norm = shooting$pupil_size_phasic_mean_norm * 100000000
shooting$pupil_size_phasic_mean = shooting$pupil_size_phasic_mean * 100000000

shooting$pupil_size_mean = shooting$pupil_size_mean * 100000000
shooting$pos_responder <- as.factor(ifelse(shooting$ChangeSigma < 0, 1, 0))

base_change_cor = lm(data = shooting, ChangeSigma ~ Sham_sigma)
tab_model(base_change_cor)

# 1. Create a true Within-Subject Centered Baseline (The Fix)
shooting <- shooting %>%
  group_by(SID) %>%
  mutate(pupil_baseline_wc = pupil_size_mean - mean(pupil_size_mean, na.rm = TRUE)) %>%
  ungroup()


unified_model <- lmer(pupil_size_phasic_mean_norm ~ stim_type * ChangeSigma * pupil_baseline_wc + (1 | SID) + (1 | SID:stim_type), 
                      data = shooting)
tab_model(unified_model)

responder_baseline_check <- lmer(pupil_size_mean ~ pos_responder + (1 | SID) + (1 | SID:stim_type), 
                      data = shooting)
tab_model(responder_baseline_check)

# 1. Update the model with a quadratic term
# This allows the model to "curve" the relationship to fit the floor
unified_model_poly <- lmer(
  pupil_size_phasic_mean_norm ~ stim_type * pos_responder * poly(pupil_baseline_wc, 2) + 
    (1 | SID) + (1 | SID:stim_type), 
  data = shooting
)
tab_model(unified_model_poly)

anova(unified_model, unified_model_poly)
# 2. Re-generate predictions
# ggpredict automatically understands poly() terms and will curve the lines
pred_continuous_poly <- ggpredict(unified_model_poly, 
                                  terms = c("pupil_baseline_wc [-150:150, by=5]", 
                                            "stim_type", 
                                            "pos_responder"))

df_continuous_poly <- as.data.frame(pred_continuous_poly)

# (Re-apply your factor labels as before)
df_continuous_poly$Condition <- factor(df_continuous_poly$group, levels = c("1", "2"), labels = c("Sham", "Active tVNS"))
df_continuous_poly$Phenotype <- factor(df_continuous_poly$facet, levels = c("1", "0"), labels = c("Positive Responders (\u0394\u03c3 < 0)", "Negative Responders (\u0394\u03c3 \u2265 0)"))

# 3. Build the plot (the ggplot code remains the same, just point it to df_continuous_poly)
ggplot() +
  geom_point(data = shooting_binned_wc, 
             aes(x = baseline_bin, y = mean_phasic, color = Condition, shape = Phenotype, size = n), 
             alpha = 0.6) +
  geom_line(data = df_continuous_poly, 
            aes(x = x, y = predicted, color = Condition, linetype = Phenotype), 
            linewidth = 1.5) +
  
  scale_color_manual(values = c("Sham" = "#4A5568", "Active tVNS" = "#DD6B20")) +
  scale_linetype_manual(values = c("Positive Responders (\u0394\u03c3 < 0)" = "solid", 
                                   "Negative Responders (\u0394\u03c3 \u2265 0)" = "dotted")) +
  scale_shape_manual(values = c("Positive Responders (\u0394\u03c3 < 0)" = 16, 
                                "Negative Responders (\u0394\u03c3 \u2265 0)" = 17)) +
  scale_size_area(max_size = 7, guide = "none") + 
  labs(
    title = "Non-Linear Neuromodulation Trajectories (Polynomial Fit)",
    x = "Trial-Level Baseline Pupil Size (Within-Subject Centered, a.u.)",
    y = "Predicted Phasic Pupil Size (Normalized)"
  ) +
  #ylim(-50, 130) +
  theme_bw(base_size = 14)