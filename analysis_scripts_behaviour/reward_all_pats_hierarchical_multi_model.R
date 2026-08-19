# -------------------------------------------------------------------------
# 1. SETUP & DATA CLEANING
# -------------------------------------------------------------------------
library(nlme)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(ggsignif)

# Load data
shooting <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')

# Remove specific subject and set up factors
shooting$reward_type <- as.factor(shooting$reward_type - 2)
shooting$reward_type <- relevel(shooting$reward_type, ref = "1") # Set baseline reference

# Remove NAs
shooting <- shooting[!is.na(shooting$eff_pt) & 
                       !is.na(shooting$success) & 
                       !is.na(shooting$pupil_size_mean), ]

# Standardize units (Fixed double division of eff_pt)
shooting$eff_pt <- (shooting$eff_pt + 40) / 1000 # Convert to seconds
shooting$rt <- (shooting$rt + 40)/1000 # Convert to seconds

shooting$pupil_size_phasic_mean_norm <- shooting$pupil_size_phasic_mean_norm * 1e4
shooting$pupil_size_phasic_mean <- shooting$pupil_size_phasic_mean * 1e4
shooting$pupil_size_pre_phasic_mean <- shooting$pupil_size_pre_phasic_mean * 1e4
shooting$pupil_size_pre_phasic_mean_norm <- shooting$pupil_size_pre_phasic_mean_norm * 1e4

shooting$pupil_size_phasic_slope <- shooting$pupil_size_phasic_slope * 1e4
shooting$pupil_size_max <- shooting$pupil_size_max * 1e4

shooting$pupil_size_mean <- shooting$pupil_size_mean * 1e4

shooting$post_cue_mean_force_z_abs = abs(shooting$post_cue_mean_force_z)
shooting$baseline_mean_force_z_abs = abs(shooting$baseline_mean_force_z)

shooting <- shooting %>%
  group_by(SID) %>%
  mutate(prev_trial_reward_type = lag(reward_type)) %>%
  ungroup()
shooting$prev_trial_reward_type[shooting$trial_num == 1] <- NA
# -------------------------------------------------------------------------
# 2. WITHIN-SUBJECT CENTERING & SCALING
# -------------------------------------------------------------------------
shooting <- shooting %>%
  group_by(SID) %>%
  mutate(
    pupil_state = pupil_size_mean - median(pupil_size_mean, na.rm = TRUE),
    pupil_phasic = pupil_size_phasic_mean_norm - median(pupil_size_phasic_mean_norm, na.rm = TRUE),
    pupil_phasic_raw = pupil_size_phasic_mean - median(pupil_size_phasic_mean, na.rm = TRUE),
    
    pupil_pre_phasic = pupil_size_pre_phasic_mean_norm - median(pupil_size_phasic_mean_norm, na.rm = TRUE),
    pupil_pre_phasic_raw = pupil_size_pre_phasic_mean - mean(pupil_size_phasic_mean, na.rm = TRUE),
    pupil_slope = pupil_size_phasic_slope - mean(pupil_size_phasic_slope, na.rm = TRUE),
    pupil_max = pupil_size_max - mean(pupil_size_max, na.rm = TRUE),
    pupil_trait = mean(pupil_size_mean, na.rm = TRUE),
    vigor_wc = peak_tanVel - median(peak_tanVel, na.rm = TRUE),
    post_force_z = post_cue_mean_force_z - median(post_cue_mean_force_z, na.rm = TRUE),
    post_force_abs = post_cue_mean_force_z_abs - median(post_cue_mean_force_z_abs, na.rm = TRUE),
    
    baseline_force_z = baseline_mean_force_z - median(baseline_mean_force_z, na.rm = TRUE),
    baseline_force_abs = baseline_mean_force_z_abs - median(baseline_mean_force_z_abs, na.rm = TRUE)
    
  ) %>%
  ungroup() %>%
  mutate(
    pupil_state_scaled = pupil_state / sd(pupil_state, na.rm = TRUE),
    pupil_phasic_scaled = pupil_phasic / sd(pupil_phasic, na.rm = TRUE),
    pupil_pre_phasic_scaled = pupil_pre_phasic / sd(pupil_pre_phasic, na.rm = TRUE),
    pupil_pre_phasic_raw_scaled = pupil_pre_phasic_raw / sd(pupil_pre_phasic_raw, na.rm = TRUE),
    pupil_slope_scaled = pupil_slope / sd(pupil_slope, na.rm = TRUE),
    pupil_max_scaled = pupil_max / sd(pupil_max, na.rm = TRUE),
    vigor_scaled = vigor_wc / sd(vigor_wc, na.rm = TRUE),
    post_force_z_scaled = post_force_z / sd(post_force_z, na.rm = TRUE),
    baseline_force_abs_scaled = baseline_force_abs / sd(baseline_force_abs, na.rm = TRUE)
  )

# Trimmed dataset (Generated but held in reserve, models use 'shooting')
shooting_trimmed <- shooting %>%
  filter(
    abs(pupil_state_scaled) <= 3,
    abs(pupil_slope_scaled) <= 3,
    abs(pupil_max_scaled) <= 3,
    abs(vigor_scaled) <= 3,
    abs(pupil_pre_phasic_scaled) <= 3
    
  )

shooting_force_trimmed <- shooting %>%
  filter(

    abs(vigor_scaled) <= 3,
    abs(post_force_z_scaled) <= 3
    
  )
# testing residual approach
# 1. Regress the maximum pupil size on the baseline state
# We do this within-subject to account for individual anatomical limits
shooting_trimmed <- shooting_trimmed %>%
  group_by(SID) %>%
  mutate(
    # The residuals represent the true phasic burst independent of baseline
    phasic_residual = resid(lm(pupil_phasic ~ pupil_state, na.action = na.exclude))
  ) %>%
  ungroup()

shooting_trimmed <- shooting_trimmed %>%
  group_by(SID) %>%
  mutate(
    # The residuals represent the true phasic burst independent of baseline
    pre_phasic_residual = resid(lm(pupil_pre_phasic ~ pupil_state, na.action = na.exclude))
  ) %>%
  ungroup()

# 1. Fit a linear model predicting phasic dilation from the baseline state
# We use na.action = na.exclude to ensure the residuals map correctly back 
# to the rows in the dataframe, even if some trials have NAs.
m_pupil_correction <- lm(
  pupil_phasic ~ pupil_state, 
  data = shooting_trimmed, 
  na.action = na.exclude
)

# 2. Extract the residuals and add them to your dataframe
# A positive residual means they dilated MORE than expected for their baseline.
# A negative residual means they dilated LESS than expected.
shooting_trimmed$phasic_residual <- resid(m_pupil_correction)

# 3. Optional but recommended: Scale the new residual metric
# This makes the coefficient in your nlme model comparable to vigor_scaled
shooting_trimmed$phasic_residual_scaled <- scale(shooting_trimmed$phasic_residual)


# -------------------------------------------------------------------------
# 2B. MAIN EFFECTS OF reward ON MAIN VARIABLES
# -------------------------------------------------------------------------

# change in baseline pupil (mean average 1 second before reward starts)
rt_sub_data = shooting_trimmed[shooting_trimmed$rt > 0.1 & shooting_trimmed$rt < 1, ]

pupil_state_reward <- lmer(
  vigor_wc ~ reward_type*rt + 
    (1 + reward_type | SID), 
  data = rt_sub_data
)

summary(pupil_state_reward)



rt_force <- lmer(
  rt ~ post_force_abs*reward_type + 
    (1  | SID), 
  data = shooting_force_trimmed)

summary(rt_force)

force_z_reward <- lmer(
  post_force_abs ~ reward_type + 
    (1 + reward_type | SID), 
  data = shooting_force_trimmed
)

summary(force_z_reward)

force_z_pupil_reward = lmer(
  baseline_force_abs ~ pupil_state * prev_trial_reward_type + 
    (1 + prev_trial_reward_type | SID), 
  data = shooting_trimmed
)
summary(force_z_pupil_reward)


pupil_state_reward <- lmer(
  rt ~ pupil_state + 
    (0 + reward_type | SID), 
  data = rt_sub_data
)

summary(pupil_state_reward)

pupil_state_vigor <- lmer(
  pupil_state ~ vigor_wc +  baseline_force_abs + trial_num + block_num +
    (1  | SID), 
  data = shooting_trimmed
)

summary(pupil_state_vigor)

# change in reach peak velocity
pupil_pre_phasic_vigor <- lmer(
  vigor_wc ~ pupil_pre_phasic_raw*reward_type + 
    (1  + reward_type | SID), 
  data = shooting_trimmed
)

summary(pupil_pre_phasic_vigor)

# change in reach peak velocity
pupil_pre_phasic_vigor_component <- lmer(
  vigor_wc ~ pupil_state*reward_type + pre_phasic_residual*reward_type +
    (1  + reward_type | SID), 
  data = shooting_trimmed
)

summary(pupil_pre_phasic_vigor_component)
# change in max pupil during phasic ramp (1.5 second window prior to GO cue)

pupil_max_reward <- lmer(
  phasic_residual ~ reward_type + 
    (1 + reward_type | SID), 
  data = shooting_trimmed
)

summary(pupil_max_reward)



# -------------------------------------------------------------------------
# 3. BASELINE ESTIMATES & HELPER FUNCTIONS
# -------------------------------------------------------------------------
# Standard safe_pnorm (no log_sigma needed since variance is fixed)
safe_pnorm <- function(eff_pt, mu, sigma) {
  pnorm(eff_pt, mean = mu, sd = sigma) 
}

# Get empirical starting values
nls_fit <- nls(
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  data = shooting,
  start = list(mu = 0.2, sigma = 0.02, asym = 0.95, gamma = 0.25)
)
nls_coefs <- coef(nls_fit)

# Shared nlme control settings
ctrl <- nlmeControl(opt = "nlm", pnlsMaxIter = 400, msMaxIter = 400)

# -------------------------------------------------------------------------
# 4. STEPWISE MODELING PIPELINE (Data = shooting)
# -------------------------------------------------------------------------

# --- Step A: Test if Reward affects Asymptote ---
# Model 1: Reward on both Mu and Asym
m1_reward_asym <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ reward_type, 
    sigma ~ 1, 
    asym ~ reward_type, 
    gamma ~ 1
  ),
  random = list(SID = pdDiag(mu + sigma ~ 1)),
  data = shooting_trimmed,
  start = list(fixed = c(nls_coefs["mu"], -0.01, nls_coefs["sigma"], nls_coefs["asym"], 0, nls_coefs["gamma"])),
  control = ctrl
)
summary(m1_reward_asym)

# Model 2: Reward on Mu only (Asym fixed to 1)
m2_reward_mu_only <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ reward_type, 
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  random = list(SID = pdDiag(mu ~ 1)),
  data = shooting_trimmed,
  start = list(fixed = c(nls_coefs["mu"], -0.01, nls_coefs["sigma"], nls_coefs["asym"], nls_coefs["gamma"])),
  control = ctrl
)
summary(m2_reward_mu_only)
# Compare Step A Models
cat("\n--- Does Reward affect Asymptote? ---\n")

# Note: The following steps assume m2_reward_mu_only wins (Asym is stable). 
# If m1 wins, simply add `asym ~ reward_type` and adjust the start vectors below.

# --- Step B: Incorporate Vigor (Is Mu effect driven by vigor?) ---
m3_reward_vigor <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ reward_type * vigor_wc, 
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  random = list(SID = pdDiag(mu  ~ 1)),
  data = shooting_trimmed,
  # mu starts: Intercept, reward_type, vigor_scaled, reward:vigor
  start = list(fixed = c(nls_coefs["mu"], -0.01, -0.005, 0, nls_coefs["sigma"], nls_coefs["asym"], nls_coefs["gamma"])),
  control = ctrl
)

summary(m3_reward_vigor)

# Then run your nlme using the _winsorized variables
m4_pupil_vigor <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ reward_type * pupil_state + vigor_wc,
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  random = list(SID = pdDiag(mu ~ reward_type)),
  data = shooting_trimmed,
  # mu starts: Int, rew, vig, pup, rew:vig, rew:pup, vig:pup, rew:vig:pup
  start = list(fixed = c(
    nls_coefs["mu"], -0.01, -0.005, -0.006, 0.004,
    nls_coefs["sigma"], nls_coefs["asym"], nls_coefs["gamma"]
  )),
  control = ctrl
)

summary(m4_pupil_vigor)



# 1. Run a stripped-down base model to grab stable starting values
m_reward_main <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ reward_type + pupil_state + pupil_pre_phasic,
    sigma ~ 1,
    asym ~ 1, 
    gamma ~ 1
  ),
  random = list(SID = pdDiag(mu ~ 1)),
  data = shooting_trimmed,
  # Assuming these match your standard nls_coefs
  start = c(0.2, 0, 0, 0, 0.02, 0.95, 0.25) 
)

# Extract those stable base coefficients
starts_reward_main <- fixef(m_reward_main) 

# 2. Pad the start vector with zeros for the new interaction terms
# We need exactly 11 starting values: 
# 8 for mu (Intercept + 3 main effects + 3 two-way + 1 three-way) + sigma + asym + gamma
new_starts_reward <- c(
  starts_reward_main[1], # mu.(Intercept)
  starts_reward_main[2], # mu.reward_type
  starts_reward_main[3], # mu.pupil_state
  starts_reward_main[4], # mu.pupil_pre_phasic
  0,                     # mu.reward_type:pupil_state (Start at 0)
  0,                     # mu.reward_type:pupil_pre_phasic (Start at 0)
  starts_reward_main[5], # sigma.(Intercept)
  starts_reward_main[6], # asym.(Intercept)
  starts_reward_main[7]  # gamma.(Intercept)
)

# 3. Run the full orthogonalized model with reward_type
m5_reward_pupil_state_pre_phasic <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ reward_type * pupil_state + reward_type*pupil_pre_phasic,
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  # Allow subjects to vary on baseline threshold and their sensitivity to reward
  random = list(SID = pdDiag(mu ~ reward_type, asym ~ reward_type)),
  data = shooting_trimmed,
  start = new_starts_reward,
  control = nlmeControl(opt = "nlminb", pnlsMaxIter = 1000, msMaxIter = 1000)
)

summary(m5_reward_pupil_state_pre_phasic)

# now look at how vigor figures with this 

# 1. Run a stripped-down base model to grab stable starting values
m_reward_main <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ reward_type + pupil_state + pre_phasic_residual + vigor_wc, 
    sigma ~ 1,
    asym ~ 1, 
    gamma ~ 1
  ),
  random = list(SID = pdDiag(mu ~ 1)),
  data = shooting_trimmed,
  # Assuming these match your standard nls_coefs
  start = c(0.2, 0, 0, 0,0, 0.02, 0.95, 0.25) 
)

# Extract those stable base coefficients
starts_reward_main <- fixef(m_reward_main) 

# 2. Pad the start vector with zeros for the new interaction terms
# We need exactly 11 starting values: 
# 8 for mu (Intercept + 3 main effects + 3 two-way + 1 three-way) + sigma + asym + gamma
new_starts_reward <- c(
  starts_reward_main[1], # mu.(Intercept)
  starts_reward_main[2], # mu.reward_type
  starts_reward_main[3], # mu.pupil_state
  starts_reward_main[4], # mu.pupil_pre_phasic
  starts_reward_main[5], # mu.vigor_wc
  0,                     # mu.reward_type:pupil_state (Start at 0)
  0,                     # mu.reward_type:pupil_pre_phasic (Start at 0)
  0,                     # mu.vigor_wc:pupil_pre_phasic_raw (Start at 0)


  starts_reward_main[6], # sigma.(Intercept)
  starts_reward_main[7], # asym.(Intercept)
  starts_reward_main[8]  # gamma.(Intercept)
)

# 3. Run the full orthogonalized model with reward_type
m6_reward_pupil_state_pre_phasic_vigor <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ pupil_state*reward_type + reward_type*pre_phasic_residual + vigor_wc*pupil_state,
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  # Allow subjects to vary on baseline threshold and their sensitivity to reward
  random = list(SID = pdDiag(mu  ~ reward_type, asym ~ 1)),
  data = shooting_trimmed,
  start = new_starts_reward,
  control = nlmeControl(opt = "nlminb", pnlsMaxIter = 1000, msMaxIter = 1000)
)

summary(m6_reward_pupil_state_pre_phasic_vigor)


m_reward_main <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ reward_type  + phasic_residual_scaled + vigor_wc + pupil_state, 
    sigma ~ 1,
    asym ~ 1, 
    gamma ~ 1
  ),
  random = list(SID = pdDiag(mu ~ 1)),
  data = shooting_trimmed,
  # Assuming these match your standard nls_coefs
  start = c(0.2, 0, 0, 0,0, 0.02, 0.95, 0.25) 
)

new_starts_reward <- c(
  starts_reward_main[1], # mu.(Intercept)
  starts_reward_main[2], # mu.(Intercept)
  
  starts_reward_main[3], # mu.pupil_state
  starts_reward_main[4], # mu.pupil_pre_phasic
  starts_reward_main[5], # mu.vigor_wc
  0,                     # mu.reward_type:pupil_state (Start at 0)
  0,
  0,
  0,
  starts_reward_main[6], # sigma.(Intercept)
  starts_reward_main[7], # asym.(Intercept)
  starts_reward_main[8]  # gamma.(Intercept)
)

# 3. Run the full orthogonalized model with reward_type
m7_pupil_state_pre_phasic_vigor <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ reward_type*phasic_residual*vigor_wc + pupil_state,
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  # Allow subjects to vary on baseline threshold and their sensitivity to reward
  random = list(SID = pdDiag(mu  ~ reward_type, asym ~ 1)),
  data = shooting_trimmed,
  start = new_starts_reward,
  control = nlmeControl(opt = "nlminb", pnlsMaxIter = 1000, msMaxIter = 1000)
)
summary(m7_pupil_state_pre_phasic_vigor)


anova(m1_reward_asym,m2_reward_mu_only, m3_reward_vigor, m4_pupil_vigor, m5_reward_pupil_state_pre_phasic, m6_reward_pupil_state_pre_phasic_vigor, m7_pupil_state_pre_phasic_vigor)



library(ggplot2)
library(dplyr)

# 1. Extract coefficients from your model
beta <- fixef(m7_pupil_state_pre_phasic_vigor)

# 2. Create a prediction grid across the range of your residualized variable
# (Adjust the min/max range if your pre_phasic_residual spans a wider/narrower window)
pred_grid <- expand.grid(
  pre_phasic_residual = seq(
    min(-0.005, na.rm = TRUE),
    max(0.005, na.rm = TRUE),
    length.out = 100
  ),
  reward_type = c("No Reward", "Reward")
)

# 3. Calculate predicted mu using the model's fixed effects equation
# (Holding pupil_state at 0, which is its median/mean-centered baseline)
pred_grid <- pred_grid %>%
  mutate(
    mu_pred = case_when(
      reward_type == "No Reward" ~ beta["mu.(Intercept)"] + 
        (beta["mu.pre_phasic_residual"] * pre_phasic_residual),
      
      reward_type == "Reward"    ~ (beta["mu.(Intercept)"] + beta["mu.reward_type2"]) + 
        ((beta["mu.pre_phasic_residual"] + beta["mu.reward_type2:pre_phasic_residual"]) * pre_phasic_residual)
    )
  )

# 4. Generate the publication-ready plot
ggplot(pred_grid, aes(x = pre_phasic_residual, y = mu_pred, color = reward_type)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("No Reward" = "#2b5c8f", "Reward" = "#d95f02")) +
  labs(
    title = "The Reward Paradox: Cost of Cognitive Readjustment",
    subtitle = "Top-down cognitive pre-loading improves execution normally, but causes a choking penalty under reward.",
    x = "Top-Down Cognitive Readjustment (pre_phasic_residual)",
    y = "Predicted Motor Cost (μ)\n[Higher Value = Worse Precision / Slower Execution]",
    color = "Trial Context"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom"
  )

