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
#shooting <- shooting[shooting$eff_pt > 0,]


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
    mu ~ reward_type * vigor_wc,  # Now testing the Vigor interaction
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ), 
  
  random = list(SID = pdDiag(mu + sigma ~ 1)),
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
library(ggplot2)
# 1. Extract fixed effects and Variance-Covariance Matrix
beta_all <- fixef(nlme_reward_vigor_continuous)
vc_all   <- vcov(nlme_reward_vigor_continuous)

# 2. Subset to only the 'mu' parameters (The first 4 coefficients)
# mu.(Intercept), mu.reward_type2, mu.vigor_scaled, mu.reward_type2:vigor_scaled
beta <- beta_all[1:4]
vc   <- vc_all[1:4, 1:4]

# 3. Create Design Matrices
vigor_range <- seq(-3, 3, length.out = 100)

# 3. Create Design Matrices
grid_nr <- data.frame(
  reward_type = factor(rep("1", 100), levels = c("1", "2")),
  vigor_scaled = vigor_range
)
grid_r  <- data.frame(
  reward_type = factor(rep("2", 100), levels = c("1", "2")),
  vigor_scaled = vigor_range
)

# Use model.matrix, but manually specify the formula for the mu equation
# This matches the subsetted beta/vc
X_no_reward <- model.matrix(~ reward_type * vigor_scaled, data = grid_nr)
X_reward    <- model.matrix(~ reward_type * vigor_scaled, data = grid_r)

# 4. Calculate Predicted Mu and SE
# (The function remains the same)
calc_mu_ci <- function(X, beta, vc) {
  mu_pred <- X %*% beta
  se_pred <- sqrt(diag(X %*% vc %*% t(X)))
  return(data.frame(mu = mu_pred, se = se_pred))
}

nr_data <- calc_mu_ci(X_no_reward, beta, vc)
r_data  <- calc_mu_ci(X_reward, beta, vc)

# 5. Assemble and Plot
df_plot_reward <- data.frame(
  vigor_scaled = rep(vigor_range, 2),
  Condition = rep(c("No Reward", "Reward"), each = 100),
  predicted_mu = c(nr_data$mu, r_data$mu),
  se = c(nr_data$se, r_data$se)
)

df_plot_reward$upper <- df_plot_reward$predicted_mu + (1.96 * df_plot_reward$se)
df_plot_reward$lower <- df_plot_reward$predicted_mu - (1.96 * df_plot_reward$se)

ggplot(df_plot_reward, aes(x = vigor_scaled, y = predicted_mu, color = Condition, fill = Condition)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("No Reward" = "#4A5568", "Reward" = "#DD6B20")) + 
  scale_fill_manual(values = c("No Reward" = "#4A5568", "Reward" = "#DD6B20")) +
  labs(
    title = "Reward Shifts Cognitive-Motor Coupling",
    subtitle = "Endogenous reward provides a constant efficiency boost (95% CI)",
    x = "Peak Reach Velocity (Within-Subject Scaled)",
    y = expression("Predicted Processing Efficiency (" * mu * ")")
  ) +
  theme_minimal(base_size = 14)
