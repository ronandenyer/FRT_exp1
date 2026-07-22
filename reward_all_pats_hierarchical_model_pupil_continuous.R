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

# 4. Standardize units 
shooting$pupil_size_phasic_mean_norm <- shooting$pupil_size_phasic_mean_norm * 1e4
shooting$pupil_size_phasic_mean <- shooting$pupil_size_phasic_mean * 1e4
shooting$pupil_size_phasic_slope <- shooting$pupil_size_phasic_slope * 1e4
shooting$pupil_size_mean <- shooting$pupil_size_mean * 1e4
shooting$eff_pt <- shooting$eff_pt / 1000 # Convert to seconds

# 5. Extract State (Within-Subject) and Trait (Between-Subject) pupil variables
shooting <- shooting %>%
  group_by(SID) %>%
  mutate(
    # State: Trial-by-trial fluctuation (Mean is exactly 0 for everyone)
    pupil_state = pupil_size_mean - mean(pupil_size_mean, na.rm = TRUE),
    #pupil_slope = pupil_size_phasic_slope - mean(pupil_size_phasic_slope, na.rm = TRUE),
    # Trait: The person's overall average baseline arousal
    pupil_trait = mean(pupil_size_mean, na.rm = TRUE)
  ) %>%
  ungroup()


# 2. Normalize and Scale Vigor Data (Peak Tangential Velocity)
# Within-Subject Centering
shooting <- shooting %>%
  group_by(SID) %>%
  mutate(
    vigor_wc = peak_tanVel - mean(peak_tanVel, na.rm = TRUE)
  ) %>%
  ungroup()


# 1. Scale the variance of your already-centered variables
shooting <- shooting %>%
  mutate(
    # Because mean is already 0, dividing by SD creates a true Z-score
    pupil_state_scaled = pupil_state / sd(pupil_state, na.rm = TRUE),
    vigor_scaled = vigor_wc / sd(vigor_wc, na.rm = TRUE)
  )

# 2. Trim the extreme physiological/kinematic artifacts
shooting_trimmed <- shooting %>%
  filter(
    abs(pupil_state_scaled) <= 3,
    #abs(pupil_slope_scaled) <= 3,
    abs(vigor_scaled) <= 3
  )


# 1. First, find empirical starting values using a standard NLS
# This ignores random effects for a second to find the 'neighborhood'
nls_fit <- nls(
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  data = shooting,
  start = list(mu = 0.2, sigma = 0.02, asym = 0.95, gamma = 0.25)
)

# Extract these optimized values
nls_coefs <- coef(nls_fit)
# 1. Ensure you still have the log-transformed function loaded in your environment
safe_pnorm_log <- function(eff_pt, mu, log_sigma) {
  pnorm(eff_pt, mean = mu, sd = exp(log_sigma)) 
}

# 2. Run the model with log_sigma
nlme_reward_pupil_continuous <- nlme(
  # CHANGE 1: Use safe_pnorm_log and log_sigma
  success ~ (gamma + (asym - gamma) * safe_pnorm_log(eff_pt, mu, log_sigma)), 
  
  fixed = list(
    mu ~ reward_type * pupil_state_scaled, 
    log_sigma ~ 1,  # CHANGE 2: Estimate log_sigma
    asym ~ 1, 
    gamma ~ 1
  ), 
  
  random = list(SID = pdDiag(mu + log_sigma ~ 1)), # CHANGE 3: Random log_sigma
  
  data = shooting_trimmed,
  
  start = list(fixed = c(
    nls_coefs["mu"],        # Intercept
    -0.01,                  # Main effect Reward
    0,                      # Main effect Pupil
    0,                      # Interaction
    log(nls_coefs["sigma"]), # CHANGE 4: CRITICAL! You must log() the raw NLS guess
    nls_coefs["asym"],  
    nls_coefs["gamma"]
  )),
  
  control = nlmeControl(opt = "nlm", pnlsMaxIter = 300, msMaxIter = 300)
)
summary(nlme_reward_pupil_continuous)

anova(nlme_reward_pupil_continuous, type = "marginal")
anova(nlme_reward_pupil_continuous)
# ways to output model coefficients
nlme_reward_pupil_continuous$coefficients

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
    x = "Baseline Pupil Size (Within-Subject Scaled)",
    y = expression("Predicted Processing Efficiency (" * mu * ")")
  ) +
  
  ylim(0.180, 0.220) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )