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
  
  random = mu ~ 1 | SID,
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

# Test the masked interaction: Does baseline arousal dictate the tVNS effect on movement vigor?
cat("\n--- Masked Interaction Check: tVNS * Pupil on Vigor ---\n")
lmer_vigor_state <- lmer(
  vigor_scaled ~ stim_type * pupil_baseline_scaled + (1 | SID), 
  data = shooting
)

print(summary(lmer_vigor_state))
cat("---------------------------------------------------\n\n")

library(ggplot2)

# 1. Extract fixed effects from the tVNS Vigor model
coefs_tvns <- fixef(nlme_tvns_vigor_continuous)

# 2. Define the equations
# Sham (Reference)
intercept_sham <- coefs_tvns["mu.(Intercept)"]
slope_sham     <- coefs_tvns["mu.vigor_scaled"]

# tVNS (Type 2)
# Note: Double check names(coefs_tvns) if you get an error here
intercept_tvns <- coefs_tvns["mu.(Intercept)"] + coefs_tvns["mu.stim_type2"]
slope_tvns     <- coefs_tvns["mu.vigor_scaled"] + coefs_tvns["mu.stim_type2:vigor_scaled"]

# 3. Create a clean sequence for scaled vigor (-3 SD to +3 SD)
vigor_range <- seq(-3, 3, length.out = 100)

# 4. Calculate predicted mu values
df_plot_tvns <- data.frame(
  vigor_scaled = rep(vigor_range, 2),
  Condition = rep(c("Sham", "tVNS"), each = 100),
  predicted_mu = c(
    intercept_sham + slope_sham * vigor_range,
    intercept_tvns + slope_tvns * vigor_range
  )
)

# 5. Plot it
ggplot(df_plot_tvns, aes(x = vigor_scaled, y = predicted_mu, color = Condition)) +
  geom_line(linewidth = 1.5) +
  # Using Blue for tVNS to visually distinguish it from the Orange Reward plots
  scale_color_manual(values = c("Sham" = "#4A5568", "tVNS" = "#3182CE")) + 
  labs(
    title = "tVNS Bypasses Cognitive-Motor Execution Coupling",
    subtitle = "Efficiency scales strongly with vigor, unaffected by noradrenergic stimulation",
    x = "Peak Reach Velocity (Within-Subject Scaled)",
    y = expression("Predicted Processing Efficiency (" * mu * ")")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )