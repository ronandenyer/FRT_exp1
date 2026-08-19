library(nlme)
library(ggplot2)
library(ggrepel) 
library(dplyr)
library(car)
# ------------------------------------------------------------------------------
# SECTION 1: DATA LOADING AND FEATURE ENGINEERING
# ------------------------------------------------------------------------------
# 1. Load Data
shooting <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Combined_experiment//all_pats_combined_hit_data.csv')
shooting = shooting[shooting$SID != "FRTC04",]
#shooting = shooting[shooting$SID != "FRTC05",]

shooting$SID <- as.factor(shooting$SID)
# 2. Factorize and level conditions
shooting$stim_type <- as.factor(shooting$stim_type)
shooting$stim_type <- relevel(shooting$stim_type, ref = 1) # 1 = Sham
shooting$reward_type <- as.factor(shooting$reward_type - 2)
shooting$reward_type <- relevel(shooting$reward_type, ref = "1") # Set baseline reference

# Remove NAs
shooting <- shooting[!is.na(shooting$eff_pt) & 
                       !is.na(shooting$success) &
                       !is.na(shooting$pupil_size_mean), ]


# Standardize units (Fixed double division of eff_pt)
shooting$eff_pt <- (shooting$eff_pt + 40) / 1000 # Convert to seconds
shooting$pupil_size_phasic_mean_norm <- shooting$pupil_size_phasic_mean_norm * 1e4
shooting$pupil_size_phasic_mean <- shooting$pupil_size_phasic_mean * 1e4
shooting$pupil_size_pre_phasic_mean_norm <- shooting$pupil_size_pre_phasic_mean_norm * 1e4
shooting$pupil_size_pre_phasic_mean <- shooting$pupil_size_pre_phasic_mean * 1e4
shooting$pupil_size_phasic_slope <- shooting$pupil_size_phasic_slope * 1e4
shooting$pupil_size_max <- shooting$pupil_size_max * 1e4

shooting$pupil_size_mean <- shooting$pupil_size_mean * 1e4

# -------------------------------------------------------------------------
# 2. WITHIN-SUBJECT CENTERING & SCALING
# -------------------------------------------------------------------------
shooting <- shooting %>%
  group_by(SID) %>%
  mutate(
    pupil_state = pupil_size_mean - median(pupil_size_mean, na.rm = TRUE),
    pupil_phasic = pupil_size_phasic_mean_norm - mean(pupil_size_phasic_mean_norm, na.rm = TRUE),
    pupil_pre_phasic = pupil_size_pre_phasic_mean_norm - mean(pupil_size_pre_phasic_mean_norm, na.rm = TRUE),
    pupil_pre_phasic_raw = pupil_size_pre_phasic_mean - mean(pupil_size_pre_phasic_mean, na.rm = TRUE),
    
    pupil_slope = pupil_size_phasic_slope - mean(pupil_size_phasic_slope, na.rm = TRUE),
    pupil_max = pupil_size_max - mean(pupil_size_max, na.rm = TRUE),
    pupil_trait = mean(pupil_size_mean, na.rm = TRUE),
    vigor_wc = peak_tanVel - mean(peak_tanVel, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    pupil_state_scaled = pupil_state / sd(pupil_state, na.rm = TRUE),
    pupil_phasic_scaled = pupil_phasic / sd(pupil_phasic, na.rm = TRUE),
    pupil_pre_phasic_scaled = pupil_pre_phasic / sd(pupil_pre_phasic, na.rm = TRUE),
    
    pupil_slope_scaled = pupil_slope / sd(pupil_slope, na.rm = TRUE),
    pupil_max_scaled = pupil_max / sd(pupil_max, na.rm = TRUE),
    vigor_scaled = vigor_wc / sd(vigor_wc, na.rm = TRUE)
  )

# Trimmed dataset (Generated but held in reserve, models use 'shooting')
shooting_trimmed <- shooting %>%
  filter(
    abs(pupil_state_scaled) <= 3,
    abs(pupil_pre_phasic_scaled) <= 3,
    
    abs(pupil_slope_scaled) <= 3,
    abs(pupil_max_scaled) <= 3,
    abs(vigor_scaled) <= 3
  )

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

# 1. Step A: Fit the main-effects only model (7 params)
# mu ~ stim_type + pupil_state + pupil_pre_phasic
m_combined_main <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(mu ~ stim_type + reward_type + vigor_wc, 
               sigma ~ 1, asym ~ 1, gamma ~ 1),
  random = list(SID = pdDiag(mu ~ 1)),
  data = shooting_trimmed,
  start = c(0.2, 0, 0, 0, 0.02, 0.95, 0.25) # Provided your nls_coefs map to this
)

starts_main <- fixef(m_combined_main) 

# 1. Correct the start vector for the 7 fixed parameters:
# mu(4) + sigma(1) + asym(1) + gamma(1) = 7 total parameters
new_starts <- c(
  starts_main[1],        # mu Intercept
  starts_main[2],        # mu stim_type2
  starts_main[3],        # mu reward_type
  0,
  #starts_main[4],        # mu vigor_wc
  starts_main[5],        # sigma (Intercept)
  0,
  0,
  0,
  starts_main[6],         # asym (Intercept)
  starts_main[7]         # gamma (Intercept),
)
# MODEL 1: Full Main-Effects Model (Does tVNS alter threshold OR asymptote?)
shooting.nlme <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(mu ~ stim_type*reward_type, sigma ~ stim_type*reward_type, asym ~ 1, gamma ~ 1), 
  random = list(SID = pdDiag(mu + sigma ~ 1)),
  data = shooting_trimmed,
  start = new_starts,
  control = ctrl
)

summary(shooting.nlme)

library(nlme)
library(dplyr)

# 1. Define your custom safe_pnorm function (if not already loaded in your environment)
safe_pnorm <- function(x, mu, sigma) {
  # Forces sigma to be strictly positive to prevent NaNs
  pnorm(x, mean = mu, sd = max(sigma, 1e-5))
}
# 2. Set the Pilot Parameters (Extracted dynamically from your 4-subject model)

# Extract fixed effects as a named numeric vector
model_fixef <- fixef(shooting.nlme)

# Map the fixed effects directly to your simulation variables
fixed_mu_int       <- model_fixef["mu.(Intercept)"]
fixed_mu_stim      <- model_fixef["mu.stim_type2"]
fixed_mu_rew       <- model_fixef["mu.reward_type2"]
fixed_mu_stim_rew  <- model_fixef["mu.stim_type2:reward_type2"] # Target interaction
fixed_sigma        <- model_fixef["sigma"]
fixed_asym         <- model_fixef["asym"]
fixed_gamma        <- model_fixef["gamma"]

# Extract Random Effects
# VarCorr() outputs a character matrix, so we isolate the cell and convert to numeric
model_varcorr <- VarCorr(shooting.nlme)
sd_mu_int     <- as.numeric(model_varcorr["mu.(Intercept)", "StdDev"])

# Task Design Parameters
trials_per_subject <- 800  # Adjust this to match your actual per-subject trial count
target_n           <- 28  # The sample size you are testing for power
n_sims             <- 100  # Number of simulation loops (use 500+ for final publication)

# 3. Create the Data Generation Function
simulate_subject_data <- function(n_subj) {
  df_list <- list()
  
  for(i in 1:n_subj) {
    # Generate the subject-specific random intercept for mu
    subj_mu_offset <- rnorm(1, mean = 0, sd = sd_mu_int)
    
    # Create the trial grid for this subject
    # Assuming a balanced 2x2 design (stim_type x reward_type)
    subj_df <- expand.grid(
      trial = 1:(trials_per_subject / 4),
      stim_type = c(1, 2),
      reward_type = c(1, 2)
    ) %>%
      mutate(
        SID = as.factor(i),
        # Generate a realistic spread of effective preparation times (e.g., 100ms to 400ms)
        # Adjust these bounds if your eff_pt distribution is different
        eff_pt = runif(n(), min = 0.100, max = 0.400) 
      ) %>%
      mutate(
        # Convert factors to numeric (1/2 to 0/1) for matrix multiplication
        stim_dummy = ifelse(stim_type == 2, 1, 0),
        rew_dummy  = ifelse(reward_type == 2, 1, 0),
        
        # Calculate the exact mu threshold for each trial using the model math
        trial_mu = (fixed_mu_int + subj_mu_offset) + 
          (fixed_mu_stim * stim_dummy) + 
          (fixed_mu_rew * rew_dummy) + 
          (fixed_mu_stim_rew * stim_dummy * rew_dummy),
        
        # Calculate the probability of success for this specific trial
        p_success = fixed_gamma + (fixed_asym - fixed_gamma) * safe_pnorm(eff_pt, trial_mu, fixed_sigma),
        
        # Draw a binary outcome (1 or 0) based on that exact probability
        success = rbinom(n(), size = 1, prob = p_success)
      )
    
    # We only need to return the variables required for the model
    subj_df$stim_type <- as.factor(subj_df$stim_type)
    subj_df$reward_type <- as.factor(subj_df$reward_type)
    df_list[[i]] <- subj_df[, c("SID", "stim_type", "reward_type", "eff_pt", "success")]
  }
  
  return(do.call(rbind, df_list))
}

# 4. Set up the starting vector based on your exact pilot parameters
sim_starts <- c(
  fixed_mu_int, fixed_mu_stim, fixed_mu_rew, fixed_mu_stim_rew, 
  fixed_sigma, fixed_asym, fixed_gamma
)

# 5. Run the Monte Carlo Loop
p_values <- numeric(n_sims)
convergence_failures <- 0

set.seed(42) # For reproducibility

cat("Starting simulation for N =", target_n, "\n")

for(s in 1:n_sims) {
  if(s %% 10 == 0) cat("Running iteration", s, "of", n_sims, "...\n")
  
  # Generate fake data
  sim_data <- simulate_subject_data(target_n)
  
  # Fit the model inside a tryCatch to prevent errors from breaking the loop
  model_fit <- tryCatch({
    nlme(
      success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
      fixed = list(mu ~ stim_type * reward_type, sigma ~ 1, asym ~ 1, gamma ~ 1), 
      random = list(SID = pdDiag(mu ~ 1)),
      data = sim_data,
      start = sim_starts,
      control = nlmeControl(opt = "nlminb", pnlsMaxIter = 1000, msMaxIter = 1000, returnObject = TRUE)
    )
  }, error = function(e) {
    return(NULL) # Return NULL if the model fails to converge
  })
  
  # Extract the p-value for the interaction term if the model converged
  if(!is.null(model_fit)) {
    # Extract the p-value matrix from the summary
    coef_table <- summary(model_fit)$tTable
    # The interaction term is the 4th row (mu.stim_type2:reward_type2)
    p_values[s] <- coef_table["mu.stim_type2:reward_type2", "p-value"]
  } else {
    p_values[s] <- NA
    convergence_failures <- convergence_failures + 1
  }
}

# 6. Calculate and Print Final Power
valid_p_values <- na.omit(p_values)
statistical_power <- mean(valid_p_values < 0.05)

cat("\n========================================\n")
cat("SIMULATION RESULTS\n")
cat("========================================\n")
cat("Target Sample Size (N):", target_n, "\n")
cat("Total Iterations:", n_sims, "\n")
cat("Convergence Failures:", convergence_failures, "\n")
cat("Estimated Power:", round(statistical_power * 100, 2), "%\n")
cat("========================================\n")