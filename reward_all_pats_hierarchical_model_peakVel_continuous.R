library(dplyr)
library(nlme)
library(lme4) # Added for the quick collinearity check

# 1. Load and Clean
shooting <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')
shooting <- shooting[shooting$SID != "FRTP15", ]
shooting$reward_type = shooting$reward_type - 2
shooting$reward_type <- as.factor(shooting$reward_type)
shooting$reward_type <- relevel(shooting$reward_type, ref = 1) # Ensure baseline is the reference

# Clean data: Remove NAs for success, eff_pt, AND peak_tanVel
shooting <- shooting[!is.na(shooting$eff_pt), ]
shooting <- shooting[!is.na(shooting$success), ]
shooting <- shooting[!is.na(shooting$peak_tanVel), ]
shooting$eff_pt <- shooting$eff_pt / 1000

# Remove negative eff_pt trials
shooting <- shooting[shooting$eff_pt > 0,]


# 2. Normalize and Scale Vigor Data (Peak Tangential Velocity)
# Within-Subject Centering
shooting <- shooting %>%
  group_by(SID) %>%
  mutate(
    vigor_wc = peak_tanVel - mean(peak_tanVel, na.rm = TRUE)
  ) %>%
  ungroup()

# Scale the centered predictor to units of SD
shooting$vigor_scaled <- shooting$vigor_wc / sd(shooting$vigor_wc, na.rm = TRUE)


# --- NEW: Quick Collinearity Check ---
# Does Reward fundamentally change Reach Vigor?
cat("\n--- Collinearity Check: Effect of Reward on Vigor ---\n")
lmer_vigor <- lmer(vigor_scaled ~ reward_type + (1 | SID), data = shooting)
print(summary(lmer_vigor)$coefficients)
cat("---------------------------------------------------\n\n")


# 3. Define the Safe Pnorm Wrapper
safe_pnorm <- function(x, mu, sigma) {
  safe_sigma <- pmax(sigma, 1e-4) 
  pnorm(x, mean = mu, sd = safe_sigma)
}


# 4. Run the Non-Linear Model
# Find empirical starting values using a standard NLS
nls_fit <- nls(
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  data = shooting,
  start = list(mu = 0.2, sigma = 0.02, asym = 0.95, gamma = 0.25)
)

nls_coefs <- coef(nls_fit)

# Pass these into the nlme model
nlme_reward_vigor_continuous <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  
  fixed = list(
    mu ~ reward_type * vigor_scaled,  # Now testing the Vigor interaction
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ), 
  
  random = mu ~ reward_type | SID, # Kept your updated random effect structure
  data = shooting,
  
  start = list(fixed = c(
    nls_coefs["mu"],    # Intercept
    0,                  # Main effect Reward
    0,                  # Main effect Vigor
    0,                  # Interaction (Reward * Vigor)
    nls_coefs["sigma"], 
    nls_coefs["asym"],  
    nls_coefs["gamma"]
  )),
  
  control = nlmeControl(opt = "nlm", pnlsMaxIter = 300, msMaxIter = 300)
)

summary(nlme_reward_vigor_continuous)

library(ggplot2)

# 1. Extract fixed effects from the REWARD Vigor model
coefs_reward <- fixef(nlme_reward_vigor_continuous)

# 2. Define the equations
# No Reward (Reference)
intercept_no_reward <- coefs_reward["mu.(Intercept)"]
slope_no_reward     <- coefs_reward["mu.vigor_scaled"]

# Reward (Type 2)
# Note: Check names(coefs_reward) if you get a subscript out of bounds error
intercept_reward <- coefs_reward["mu.(Intercept)"] + coefs_reward["mu.reward_type2"]
slope_reward     <- coefs_reward["mu.vigor_scaled"] + coefs_reward["mu.reward_type2:vigor_scaled"]

# 3. Create a clean sequence for scaled vigor (-3 SD to +3 SD)
vigor_range <- seq(-3, 3, length.out = 100)

# 4. Calculate predicted mu values
df_plot_reward <- data.frame(
  vigor_scaled = rep(vigor_range, 2),
  Condition = rep(c("No Reward", "Reward"), each = 100),
  predicted_mu = c(
    intercept_no_reward + slope_no_reward * vigor_range,
    intercept_reward + slope_reward * vigor_range
  )
)

# 5. Plot it
ggplot(df_plot_reward, aes(x = vigor_scaled, y = predicted_mu, color = Condition)) +
  geom_line(linewidth = 1.5) +
  # Using Orange to distinguish Reward from the tVNS plot
  scale_color_manual(values = c("No Reward" = "#4A5568", "Reward" = "#DD6B20")) + 
  labs(
    title = "Reward Shifts Cognitive-Motor Coupling",
    subtitle = "Endogenous reward provides a constant efficiency boost across all vigor states",
    x = "Peak Reach Velocity (Within-Subject Scaled)",
    y = expression("Predicted Processing Efficiency (" * mu * ")")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )