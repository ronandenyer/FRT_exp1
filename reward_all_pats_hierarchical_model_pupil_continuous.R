library(nlme)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(nlme)
library(dplyr)

# 1. Load and Clean
shooting <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')
shooting <- shooting[shooting$SID != "FRTP15", ]
shooting$reward_type = shooting$reward_type - 2
shooting$reward_type <- as.factor(shooting$reward_type)
shooting$reward_type <- relevel(shooting$reward_type, ref = 1) # Ensure baseline is the reference

# Clean data
shooting <- shooting[!is.na(shooting$eff_pt), ]
shooting <- shooting[!is.na(shooting$success), ]
shooting <- shooting[!is.na(shooting$pupil_size_mean), ]
shooting$eff_pt <- shooting$eff_pt / 1000
# 2. Normalize and Scale Pupil Data (CRITICAL for model stability)
# Convert to same units as your tVNS experiment
shooting$pupil_size_mean <- shooting$pupil_size_mean * 1e6

# Within-Subject Centering
shooting <- shooting %>%
  group_by(SID) %>%
  mutate(
    pupil_baseline_wc = pupil_size_mean - mean(pupil_size_mean, na.rm = TRUE)
  ) %>%
  ungroup()

shooting = shooting[shooting$eff_pt > 0,]

# Scale the centered predictor (avoids the chol.default error)
shooting$pupil_baseline_scaled <- shooting$pupil_baseline_wc / sd(shooting$pupil_baseline_wc, na.rm = TRUE)

# 3. Define the Safe Pnorm Wrapper
safe_pnorm <- function(x, mu, sigma) {
  safe_sigma <- pmax(sigma, 1e-4) 
  pnorm(x, mean = mu, sd = safe_sigma)
}

library(nlme)

# 1. First, find empirical starting values using a standard NLS
# This ignores random effects for a second to find the 'neighborhood'
nls_fit <- nls(
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  data = shooting,
  start = list(mu = 0.2, sigma = 0.02, asym = 0.95, gamma = 0.25)
)

# Extract these optimized values
nls_coefs <- coef(nls_fit)

# 2. Now, pass these into your nlme model
# We set slopes to 0, but use the empirical intercepts from the NLS fit
nlme_reward_pupil_continuous <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  
  fixed = list(
    mu ~ reward_type * pupil_baseline_scaled, 
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ), 
  
  random = mu ~ reward_type | SID,
  data = shooting,
  
  start = list(fixed = c(
    nls_coefs["mu"],    # Intercept
    0,                  # Main effect Reward
    0,                  # Main effect Pupil
    0,                  # Interaction
    nls_coefs["sigma"], 
    nls_coefs["asym"],  
    nls_coefs["gamma"]
  )),
  
  control = nlmeControl(opt = "nlm", pnlsMaxIter = 300, msMaxIter = 300)
)

summary(nlme_reward_pupil_continuous)

summary(nlme_reward_pupil_continuous)
anova(nlme_reward_pupil_continuous, type = "marginal")
anova(nlme_reward_pupil_continuous)
# ways to output model coefficients
nlme_reward_pupil_continuous$coefficients
library(ggplot2)

# 1. Extract fixed effects from the REWARD model
coefs <- fixef(nlme_reward_pupil_continuous)

# 2. Define the equations
# No Reward (Reference)
intercept_no_reward <- coefs["mu.(Intercept)"]
slope_no_reward     <- coefs["mu.pupil_baseline_scaled"]

# Reward (Type 2)
# Note: Check names(coefs) to ensure 'mu.reward_type2' is the exact name
intercept_reward <- coefs["mu.(Intercept)"] + coefs["mu.reward_type2"]
slope_reward     <- coefs["mu.pupil_baseline_scaled"] + coefs["mu.reward_type2:pupil_baseline_scaled"]

# 3. Create a clean sequence
pupil_range <- seq(-3, 3, length.out = 100)

# 4. Calculate predicted mu values
# Using the variable names we actually defined above
df_plot <- data.frame(
  pupil_scaled = rep(pupil_range, 2),
  Condition = rep(c("No Reward", "Reward"), each = 100),
  predicted_mu = c(
    intercept_no_reward + slope_no_reward * pupil_range,
    intercept_reward + slope_reward * pupil_range
  )
)

# 5. Plot it
ggplot(df_plot, aes(x = pupil_scaled, y = predicted_mu, color = Condition)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("No Reward" = "#4A5568", "Reward" = "#DD6B20")) +
  labs(
    title = "Reward Effects on Neural Efficiency",
    subtitle = "Baseline arousal modulates the effect of Reward on processing efficiency (mu)",
    x = "Baseline Pupil Size (Within-Subject Scaled)",
    y = expression("Predicted Processing Efficiency (" * mu * ")")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )