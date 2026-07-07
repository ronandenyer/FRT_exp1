shooting_combined$pupil_centered <- scale(shooting_combined$pupil_size_phasic_mean_norm, scale = FALSE)
# 1. Calculate the 2.5th and 97.5th percentiles of your original data
lower_bound <- quantile(shooting_combined$pupil_size_phasic_mean_norm, probs = 0.025, na.rm = TRUE)
upper_bound <- quantile(shooting_combined$pupil_size_phasic_mean_norm, probs = 0.975, na.rm = TRUE)

# 2. Filter the dataframe to keep only the middle 95%
shooting_trimmed <- subset(shooting_combined, 
                           pupil_size_phasic_mean_norm >= lower_bound & 
                             pupil_size_phasic_mean_norm <= upper_bound)

# 3. CRUCIAL STEP: Re-center the pupil variable on this new trimmed dataset
# (Because the mean changes slightly when you cut the tails)
shooting_trimmed$pupil_centered <- scale(shooting_trimmed$pupil_size_phasic_mean_norm, scale = FALSE)

nlme_combined_continuous <- nlme(
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  fixed = list(
    mu ~ pupil_centered,   # Needs 2 starting values (Intercept, Slope)
    sigma ~ 1,                   # Needs 1 starting value
    asym ~ pupil_centered, # Needs 2 starting values (Intercept, Slope)
    gamma ~ 1                    # Needs 1 starting value
  ), 
  
  random = mu ~ 1 | SID,
  
  data = shooting_trimmed,
  
  # Total of 6 starting values matching the continuous formulation
  start = list(fixed = c(
    0.20, -0.01,  # mu (Intercept, Slope for pupil)
    0.02,        # sigma
    0.95,  0.004, # asym (Intercept, Slope for pupil)
    0.25         # gamma
  ))                 
)


print(summary(nlme_combined_continuous))
print(anova(nlme_combined_continuous))




nlme_combined_quadratic <- nlme(
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  fixed = list(
    # mu gets the linear AND squared term (3 parameters)
    mu ~ pupil_centered + I(pupil_centered^2),   
    sigma ~ 1,                   
    asym ~ pupil_centered, # Keeps asym linear (2 parameters)
    gamma ~ 1                    
  ), 
  
  random = mu ~ 1 | SID,
  
  data = shooting_trimmed,
  
  # Total of 7 starting values
  start = list(fixed = c(
    0.20, -0.01, -0.005, # mu (Intercept, Linear Slope, Quadratic Slope)
    0.02,                # sigma
    0.95,  0.004,        # asym (Intercept, Linear Slope)
    0.25                 # gamma
  ))                 
)

print(summary(nlme_combined_quadratic))
print(anova(nlme_combined_quadratic))

nlme_combined_asymmetric <- nlme(
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  fixed = list(
    # mu gets the linear, squared, AND cubic term (4 parameters)
    mu ~ pupil_centered + I(pupil_centered^2) + I(pupil_centered^3),   
    sigma ~ 1,                   
    asym ~ 1, # Keeps asym linear (2 parameters)
    gamma ~ 1                    
  ), 
  
  random = mu ~ 1 | SID,
  
  data = shooting_trimmed,
  
  # Total of 8 starting values
  start = list(fixed = c(
    0.20, 0.20, 0.00, 0.005, # Intercept, Flat Linear (minimum at center), Positive Quadratic
    0.02,                       # sigma
    0.95,               # asym (Intercept, Linear)
    0.25                        # gamma
  ))                 
)

print(summary(nlme_combined_asymmetric))
print(anova(nlme_combined_asymmetric))
anova(nlme_combined_continuous, nlme_combined_quadratic, nlme_combined_asymmetric)

# Shift the raw, trimmed variable so the lowest value starts at 1, then take the natural log
min_val <- min(shooting_trimmed$pupil_size_phasic_mean_norm, na.rm = TRUE)

shooting_trimmed$pupil_log <- log(
  (shooting_trimmed$pupil_size_phasic_mean_norm - min_val) + 1
)

nlme_combined_decay <- nlme(
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  fixed = list(
    mu ~ pupil_log,   # The slope now represents the logarithmic decay
    sigma ~ 1,                   
    asym ~ 1, 
    gamma ~ 1                    
  ), 
  
  random = mu ~ 1 | SID,
  
  data = shooting_trimmed,
  
  # 5 starting values (2 for mu, 1 for sigma, 1 for asym, 1 for gamma)
  start = list(fixed = c(
    0.20, -0.05,  # mu (Intercept, Log Slope)
    0.02,         # sigma
    0.95,         # asym
    0.25          # gamma
  ))                 
)

print(summary(nlme_combined_decay))
print(anova(nlme_combined_decay))
anova(nlme_combined_continuous, nlme_combined_quadratic, nlme_combined_asymmetric, nlme_combined_decay)


# =========================================================================
# 1. Calculate the Median Phasic Size per Subject per Tertile
# =========================================================================
subject_tertile_medians <- shooting_trimmed %>%
  filter(!is.na(pupil_phasic_tertile), !is.na(pupil_size_phasic_mean_norm)) %>%
  group_by(SID, pupil_phasic_tertile) %>%
  summarize(
    subject_median = median(pupil_size_phasic_mean_norm, na.rm = TRUE),
    .groups = "drop"
  )

nlme_combined_tertile <- nlme(
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  fixed = list(mu ~ pupil_phasic_tertile, 
               sigma ~ 1, 
               asym ~ pupil_phasic_tertile, 
               gamma ~ 1), 
  
  random = mu  ~ 1 | SID,
  
  data = shooting_trimmed,
  
  # 10 starting values matching the 3-level parameter expansions
  start = list(fixed = c(0.20, -0.01,  0.00,   # mu (Low, Med_diff, High_diff)
                         0.02, # sigma (Low, Med_diff, High_diff)
                         0.95,  0.004, 0.00,   # asym (Low, Med_diff, High_diff)
                         0.25))                # gamma
)

# =========================================================================
# 6. Evaluate Results
# =========================================================================
print(summary(nlme_combined_tertile))
print(anova(nlme_combined_tertile))

anova(nlme_combined_continuous, nlme_combined_quadratic,
      nlme_combined_asymmetric, nlme_combined_decay,nlme_combined_tertile)


library(ggplot2)

# 1. Run your final, streamlined linear model to ensure we grab the exact coefficients
nlme_final_linear <- nlme(
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ pupil_centered,   
    sigma ~ 1,                   
    asym ~ 1,              
    gamma ~ 1                    
  ), 
  random = mu ~ 1 | SID,
  data = shooting_trimmed,
  start = list(fixed = c(0.20, -0.003, 0.02, 0.95, 0.25))                 
)

# 2. Extract the fixed effects for mu
coefs_final <- fixef(nlme_final_linear)
b0_final <- coefs_final["mu.(Intercept)"]
b1_final <- coefs_final["mu.pupil_centered"]

# 3. Create prediction data using the exact data boundaries of your trimmed dataset
min_pupil <- min(shooting_trimmed$pupil_centered, na.rm = TRUE)
max_pupil <- max(shooting_trimmed$pupil_centered, na.rm = TRUE)

plot_data_final <- data.frame(
  pupil_centered = seq(min_pupil, max_pupil, length.out = 500)
)

# 4. Calculate the straight linear prediction for mu
plot_data_final$predicted_mu <- b0_final + (b1_final * plot_data_final$pupil_centered)

# 5. Plot the winning linear fit
ggplot(plot_data_final, aes(x = pupil_centered, y = predicted_mu)) +
  geom_line(color = "#27ae60", linewidth = 1.2) + # Clean green to represent the final winning model
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Effect of Pupil Size on Mu (Linear Fit)",
    subtitle = paste("Significant linear slope =", round(b1_final, 5), "(p = 0.041)"),
    x = "Centered Pupil Size (0 = Average)",
    y = "Predicted Mu (Curve Midpoint)"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )