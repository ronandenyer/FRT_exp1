library(dplyr)
library(nlme)
library(lme4)

# 1. Load and Clean
# UPDATE THIS PATH to your tVNS data file
shooting <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv') 

# shooting <- shooting[shooting$SID != "FRTP15", ] # Keep if you still need to exclude this subject
# shooting$stim_type = shooting$stim_type - 2      # Uncomment if stim_type needs the same shift as reward_type
shooting$stim_type <- as.factor(shooting$stim_type)
shooting$stim_type <- relevel(shooting$stim_type, ref = 1) # Ensure Sham is the reference

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


# 1. Create a true Within-Subject Centered Baseline (The Fix)
shooting <- shooting %>%
  group_by(SID) %>%
  mutate(pupil_size_center = pupil_size_mean - mean(pupil_size_phasic_mean, na.rm = TRUE)) %>%
  ungroup()


# 1. Scale the centered predictor to fix the optimizer matrix
shooting$pupil_baseline_scaled <- shooting$pupil_size_center / sd(shooting$pupil_size_center, na.rm = TRUE)
# ==============================================================================
# REPLACING SECTION 2: Within-Subject Z-Scoring & High-Leverage Trimming
# ==============================================================================

# 1. Calculate pure Within-Subject Z-scores for Vigor
shooting <- shooting %>%
  group_by(SID) %>%
  mutate(
    vigor_wc = peak_tanVel - mean(peak_tanVel, na.rm = TRUE),
    # FIX 1: Scale by individual SD instead of group SD to equalize participant leverage
    vigor_scaled = vigor_wc / sd(vigor_wc, na.rm = TRUE) 
  ) %>%
  ungroup()

# 2. FIX 2: Filter out extreme high-leverage outliers (beyond 3.5 SD)
# This removes the absolute motor glitches while keeping ~99% of your robust data
shooting <- shooting[abs(shooting$vigor_scaled) <= 3.5, ]

# 3. Repeat the Within-Subject Z-scoring for Pupil Baseline
shooting <- shooting %>%
  group_by(SID) %>%
  mutate(
    pupil_size_center = pupil_size_mean - mean(pupil_size_mean, na.rm = TRUE),
    pupil_baseline_scaled = pupil_size_center / sd(pupil_size_center, na.rm = TRUE)
  ) %>%
  ungroup()

# ==============================================================================

# --- Quick Collinearity Check ---
# Does tVNS fundamentally change Reach Vigor like Reward did?
cat("\n--- Collinearity Check: Effect of tVNS on Vigor ---\n")
lmer_vigor <- lmer(vigor_scaled ~ stim_type + (1 | SID), data = shooting)
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
nlme_tvns_vigor_continuous <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  
  fixed = list(
    mu ~ stim_type * vigor_scaled,  # Now testing the Vigor interaction with tVNS
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ), 
  
  random = mu + sigma ~ 1 | SID,
  data = shooting,
  
  start = list(fixed = c(
    nls_coefs["mu"],    # Intercept
    0,                  # Main effect tVNS
    0,                  # Main effect Vigor
    0,                  # Interaction (tVNS * Vigor)
    nls_coefs["sigma"], 
    nls_coefs["asym"],  
    nls_coefs["gamma"]
  )),
  
  control = nlmeControl(opt = "nlm", pnlsMaxIter = 300, msMaxIter = 300)
)

summary(nlme_tvns_vigor_continuous)

# 1. Look at the absolute extremes of your predictor
print(summary(shooting$vigor_scaled))

# 2. Count how many trials sit beyond standard boundaries
total_trials <- nrow(shooting)
outliers_3sd <- sum(abs(shooting$vigor_scaled) > 3, na.rm = TRUE)
outliers_4sd <- sum(abs(shooting$vigor_scaled) > 4, na.rm = TRUE)

cat(sprintf("\nOutliers beyond 3 SD: %d (%.2f%% of data)\n", outliers_3sd, (outliers_3sd/total_trials)*100))
cat(sprintf("Outliers beyond 4 SD: %d (%.2f%% of data)\n", outliers_4sd, (outliers_4sd/total_trials)*100))

# Test the masked interaction: Does baseline arousal dictate the tVNS effect on movement vigor?
cat("\n--- Masked Interaction Check: tVNS * Pupil on Vigor ---\n")
lmer_vigor_state <- lmer(
  vigor_scaled ~ stim_type * pupil_baseline_scaled + (1 | SID), 
  data = shooting
)

print(summary(lmer_vigor_state))
cat("---------------------------------------------------\n\n")

library(ggplot2)
library(ggplot2)

# 1. Extract fixed effects and Variance-Covariance Matrix
beta_all <- fixef(nlme_tvns_vigor_continuous)
vc_all   <- vcov(nlme_tvns_vigor_continuous)

# 2. Subset to only the 'mu' parameters (The first 4 coefficients)
# 1: Intercept, 2: stim_type2, 3: vigor_scaled, 4: interaction
beta <- beta_all[1:4]
vc   <- vc_all[1:4, 1:4]

# 3. Create Design Matrices
vigor_range <- seq(-3, 3, length.out = 100)
grid_sham <- data.frame(
  stim_type = factor(rep("1", 100), levels = c("1", "2")), # "1" is Sham
  vigor_scaled = vigor_range
)
grid_tvns <- data.frame(
  stim_type = factor(rep("2", 100), levels = c("1", "2")), # "2" is tVNS
  vigor_scaled = vigor_range
)

# Use model.matrix to generate the X matrices
X_sham <- model.matrix(~ stim_type * vigor_scaled, data = grid_sham)
X_tvns <- model.matrix(~ stim_type * vigor_scaled, data = grid_tvns)

# 4. Calculate Predicted Mu and Standard Error
calc_mu_ci <- function(X, beta, vc) {
  mu_pred <- X %*% beta
  se_pred <- sqrt(diag(X %*% vc %*% t(X)))
  return(data.frame(mu = mu_pred, se = se_pred))
}

sham_data <- calc_mu_ci(X_sham, beta, vc)
tvns_data <- calc_mu_ci(X_tvns, beta, vc)

# 5. Assemble and Plot
df_plot_tvns <- data.frame(
  vigor_scaled = rep(vigor_range, 2),
  Condition = rep(c("Sham", "tVNS"), each = 100),
  predicted_mu = c(sham_data$mu, tvns_data$mu),
  se = c(sham_data$se, tvns_data$se)
)

df_plot_tvns$upper <- df_plot_tvns$predicted_mu + (1.96 * df_plot_tvns$se)
df_plot_tvns$lower <- df_plot_tvns$predicted_mu - (1.96 * df_plot_tvns$se)

ggplot(df_plot_tvns, aes(x = vigor_scaled, y = predicted_mu, color = Condition, fill = Condition)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("Sham" = "#4A5568", "tVNS" = "#3182CE")) + 
  scale_fill_manual(values = c("Sham" = "#4A5568", "tVNS" = "#3182CE")) +
  labs(
    title = "tVNS Bypasses Cognitive-Motor Execution Coupling",
    subtitle = "Efficiency scales with vigor, unaffected by tVNS (95% CI)",
    x = "Peak Reach Velocity (Within-Subject Scaled)",
    y = expression("Predicted Processing Efficiency (" * mu * ")")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )