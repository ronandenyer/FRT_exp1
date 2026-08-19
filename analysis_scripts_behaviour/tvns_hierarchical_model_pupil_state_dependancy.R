# ==============================================================================
# tVNS & REWARD FORCED RESPONSE TASK - DATA PIPELINE
# ==============================================================================
library(nlme)
library(ggplot2)
library(ggrepel) 
library(dplyr)
library(car)
# ------------------------------------------------------------------------------
# SECTION 1: DATA LOADING AND FEATURE ENGINEERING
# ------------------------------------------------------------------------------
# 1. Load Data
shooting <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')

# 2. Factorize and level conditions
shooting$stim_type <- as.factor(shooting$stim_type)
shooting$stim_type <- relevel(shooting$stim_type, ref = 1) # 1 = Sham

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

# -------------------------------------------------------------------------
# 2B. MAIN EFFECTS OF TVNS ON MAIN VARIABLES
# -------------------------------------------------------------------------

# change in baseline pupil (mean average 1 second before tvns starts)
pupil_state_tvns <- lmer(
  pupil_size_phasic_mean_norm ~ stim_type*pupil_size_pre_phasic_mean_norm  + 
    (1 + stim_type | SID), 
  data = shooting_trimmed
)

summary(pupil_state_tvns)

pupil_state_vigor <- lmer(
   vigor_wc~ stim_type*pupil_state  + 
    (1 + stim_type + pupil_state | SID), 
  data = shooting_trimmed
)

summary(pupil_state_vigor)

force_z_pupil_tvns = lmer(
  post_force_abs ~ pupil_state + 
    (1  | SID), 
  data = shooting_trimmed
)
summary(force_z_pupil_tvns)
# change in max pupil during phasic ramp (1.5 second window prior to GO cue)

pupil_max_tvns <- lmer(
  pupil_max ~ stim_type + 
    (1 + stim_type | SID), 
  data = shooting_trimmed
)

summary(pupil_max_tvns)

pupil_prev_trial_tvns <- lmer(
  pupil_size_mean ~ stim_type*prev_trial_success + 
    (1 + stim_type + prev_trial_success | SID), 
  data = shooting_trimmed
)

summary(pupil_prev_trial_tvns)

# change in reach peak velocity
vigor_tvns <- lmer(
  vigor_wc ~ stim_type + 
    (1 + stim_type | SID), 
  data = shooting_trimmed
)

summary(vigor_tvns)

vigor_prev_trial_tvns <- lmer(
  peak_tanVel ~ prev_trial_success + 
    (1 + prev_trial_success | SID), 
  data = shooting_trimmed
)

summary(vigor_prev_trial_tvns)

rt_prev_trial_tvns <- lmer(
  rt ~ prev_trial_success*stim_type + 
    (1 + prev_trial_success | SID), 
  data = shooting_trimmed
)

summary(rt_prev_trial_tvns)

# relationship between previous trial max pupil and current trial base pupil
shooting_trimmed$prev_trial_pupil_phasic = 0
shooting_trimmed$prev_trial_pupil_phasic[2:nrow(shooting_trimmed)] = shooting_trimmed$pupil_size_phasic_mean_norm[1:nrow(shooting_trimmed)-1]

shooting_pre_pupil = shooting_trimmed[shooting_trimmed$trial_num != 1,]
shooting_pre_pupil$trial_num_small = shooting_pre_pupil$trial_num/100000
shooting_pre_pupil$block_num_small = shooting_pre_pupil$block_num/100000


prev_trial_pupil_tvns <- lmer(
  pupil_size_mean ~ stim_type*prev_trial_pupil_phasic +
    (1 | SID), 
  data = shooting_pre_pupil
)

summary(prev_trial_pupil_tvns)

shooting_pre_pupil <- shooting_pre_pupil %>%
  # Make sure the data is sorted chronologically per subject first!
  # arrange(SID, original_timestamp_or_raw_trial_column) %>% 
  group_by(SID) %>%
  mutate(
    absolute_trial = row_number(),
    # Immediately z-score it to prevent optimizer scaling warnings
    absolute_trial_z = scale(absolute_trial)[,1] 
  ) %>%
  ungroup()

prev_abs_trial_pupil_tvns <- lmer(
  pupil_state ~ stim_type*trial_num_small+block_num_small +
    (1 + stim_type | SID), 
  data = shooting_pre_pupil
)

summary(prev_abs_trial_pupil_tvns)
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
  data = shooting_trimmed,
  start = list(mu = 0.2, sigma = 0.02, asym = 0.95, gamma = 0.25)
)
nls_coefs <- coef(nls_fit)

# Shared nlme control settings
ctrl <- nlmeControl(opt = "nlm", pnlsMaxIter = 400, msMaxIter = 400)


# ------------------------------------------------------------------------------
# SECTION 2: STEP-WISE MODEL SELECTION (MAIN EFFECTS)
# ------------------------------------------------------------------------------

# MODEL 1: Full Main-Effects Model (Does tVNS alter threshold OR asymptote?)
shooting.nlme <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(mu ~ stim_type, sigma ~ 1, asym ~ stim_type, gamma ~ 1), 
  random = list(SID = pdDiag(mu ~ 1)),
  data = shooting_trimmed,
  start = list(fixed = c(0.2, -0.01, 0.02, 0.95, 0.01, 0.25)),
  control = ctrl
)

summary(shooting.nlme)
# MODEL 2: Trimmed Main-Effects Model (Lock asymptote to test if it's needed)
shooting.no_asym_int_nlme <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(mu ~ stim_type, sigma ~ 1, asym ~ 1, gamma ~ 1), 
  random = list(SID = pdDiag(mu ~ 1)),
  data = shooting_trimmed,
  start = list(fixed = c(0.2, -0.01, 0.02, 0.95, 0.25))
)
summary(shooting.no_asym_int_nlme)

shooting.nlme_trial_num <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ stim_type * trial_num, 
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  # Ensure the random list uses the exact names of your parameters
  random = list(SID = pdDiag(mu ~ 1 + asym ~ 1)), # or list(mu ~ 1, asym ~ 1) depending on your original setup
  data = shooting_trimmed,
  # You must provide a start vector whose length matches your total fixed effects.
  # Count: mu intercept (1), stim_type (1), trial_num (1), block_num (1), stim:trial (1), sigma (1), asym (1), gamma (1) = 8 parameters.
  start = list(fixed = c(0.2, 0, 0, 0, 0.02, 0.95, 0.25)),
  control = ctrl
)

summary(shooting.nlme_trial_num)

shooting_trimmed <- shooting_trimmed %>%
  mutate(
    # Median split: 1-27 is Early, 28-54 is Late
    block_half = ifelse(trial_num <= 27, "Early", "Late"),
    # Convert to factor and explicitly set Early as the baseline reference
    block_half = factor(block_half, levels = c("Early", "Late"))
  )

# MODEL 1: Block-Split Model (Does tVNS interaction with fatigue alter threshold?)
shooting.nlme <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ stim_type * block_half,  # Added the 2x2 interaction here
    sigma ~ stim_type * block_half, 
    asym ~ stim_type,             # Left asym as just stim_type, per your original
    gamma ~ stim_type
  ), 
  random = list(SID = pdDiag(mu ~ 1)),
  data = shooting_trimmed,
  # START VECTOR MAP:
  # mu_int, mu_stim, mu_half, mu_stim:half, sigma, asym_int, asym_stim, gamma
  start = list(fixed = c(0.2, -0.01, 0, 0, 0.02, 0,0,0, 0.95, 0.01, 0.25, 0)),
  control = ctrl
)

summary(shooting.nlme)

# Test 1: Can we drop the asymptote parameter? (Expecting p > 0.05)
cat("\n--- LRT: Can we drop Asymptote from the model? ---\n")

shooting_post_error = shooting_trimmed[!is.na(shooting_trimmed$prev_trial_success),]

shooting.prev_trial_int_nlme <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(mu ~ stim_type * prev_trial_success + vigor_wc + pupil_state, sigma ~ 1, asym ~ stim_type*prev_trial_success, gamma ~ 1), 
  random = list(SID = pdDiag(mu + asym + sigma ~ 1)),
  data = shooting_post_error,
  # The starting vector requires 7 values now: 
  # 4 for mu (Intercept, stim_type, prev_trial_success, interaction) + 1 for sigma + 1 for asym + 1 for gamma
  start = list(fixed = c(0.2, -0.01, 0.00, 0, 0, 0.00, 0.02, 0.95, 0, 0, 0, 0.25))
)
summary(shooting.prev_trial_int_nlme)
# ------------------------------------------------------------------------------
# SECTION 3: STATE-DEPENDENT HYPOTHESIS & TRAIT CONTROL
# ------------------------------------------------------------------------------

# 1. Step A: Fit the main-effects only model (7 params)
# mu ~ stim_type + pupil_state + pupil_pre_phasic
m_tvns_main <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(mu ~ stim_type + pupil_state + phasic_residual, 
               sigma ~ 1, asym ~ 1, gamma ~ 1),
  random = list(SID = pdDiag(mu ~ 1)),
  data = shooting_trimmed,
  start = c(0.2, 0, 0, 0.02, 0.95, 0.25) # Provided your nls_coefs map to this
)

starts_main <- fixef(m_tvns_main) 

# 1. Correct the start vector for the 7 fixed parameters:
# mu(4) + sigma(1) + asym(1) + gamma(1) = 7 total parameters
new_starts <- c(
  starts_main[1],        # mu Intercept
  starts_main[2],        # mu stim_type2
  starts_main[3],        # mu pupil_state
  0,                     # mu stim_type2:pupil_state (New interaction start at 0)
  starts_main[4],        # sigma (Intercept)
  starts_main[5],        # asym (Intercept)
  starts_main[6]         # gamma (Intercept)
)

# 2. Run the model with a clean, matching random structure
nlme_tvns_pupil_state <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ stim_type *  pupil_state,
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  # Match the random structure strictly to the parameters that have variance
  random = list(SID = pdDiag(mu  ~ stim_type, asym ~ 1)),
  data = shooting_trimmed,
  start = new_starts,
  control = nlmeControl(opt = "nlminb", pnlsMaxIter = 1000, msMaxIter = 1000)
)

summary(nlme_tvns_pupil_state)


# 1. Correct the start vector for the 7 fixed parameters:
# mu(4) + sigma(1) + asym(1) + gamma(1) = 7 total parameters
new_starts <- c(
  starts_main[1],        # mu Intercept
  starts_main[2],        # mu stim_type2
  starts_main[3],        # mu pupil_state
  starts_main[4],
  0,                     # mu stim_type2:pupil_state (New interaction start at 0)
  0,


  starts_main[5],        # sigma (Intercept)
  starts_main[6],        # asym (Intercept)
  starts_main[7]         # gamma (Intercept)
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


# 2. Run the model with a clean, matching random structure
nlme_tvns_pupil_state_residual <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ stim_type *  pupil_state + phasic_residual + vigor_wc,
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  # Match the random structure strictly to the parameters that have variance
  random = list(SID = pdDiag(mu  ~ stim_type, asym ~ stim_type)),
  data = shooting_trimmed,
  start = new_starts,
  control = nlmeControl(opt = "nlminb", pnlsMaxIter = 1000, msMaxIter = 1000)
)

summary(nlme_tvns_pupil_state_residual)

