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
shooting$eff_pt <- shooting$eff_pt / 1000 # Convert to seconds
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
    pupil_state = pupil_size_mean - mean(pupil_size_mean, na.rm = TRUE),
    pupil_phasic = pupil_size_phasic_mean_norm - mean(pupil_size_phasic_mean_norm, na.rm = TRUE),
    pupil_pre_phasic = pupil_size_pre_phasic_mean_norm - mean(pupil_size_pre_phasic_mean_norm, na.rm = TRUE),
    
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

# change in baseline pupil (mean average 1 second before tvns starts)
pupil_state_tvns <- lmer(
  pupil_pre_phasic ~ stim_type  + 
    (1 + stim_type | SID), 
  data = shooting_trimmed
)



summary(pupil_state_tvns)

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
  fixed = list(mu ~ stim_type + pupil_state, 
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
  0,
  starts_main[4],        # sigma (Intercept)
  starts_main[5],        # asym (Intercept)
  starts_main[6]         # gamma (Intercept)
)

# 2. Run the model with a clean, matching random structure
nlme_tvns_pupil_state <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ stim_type * pupil_pre_phasic + pupil_state,
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


# 1. Calculate the participant-level means per stim condition
subject_pupil_means <- shooting_trimmed %>%
  group_by(SID, stim_type) %>%
  summarize(
    mean_pupil_state = median(pupil_size_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Pivot wide so each participant has a column for tVNS and a column for Sham
  tidyr::pivot_wider(
    names_from = stim_type, 
    values_from = mean_pupil_state,
    names_prefix = "mean_pupil_"
  )

# 2. (Optional) If your stim_type levels are coded 1 and 2:
# Rename them to something intuitive if pivot_wider creates "mean_pupil_1" and "mean_pupil_2"
# subject_pupil_means <- subject_pupil_means %>%
#   rename(mean_pupil_sham = mean_pupil_1, mean_pupil_tvns = mean_pupil_2)

# 3. Compute the within-subject Delta (tVNS minus Sham)
subject_pupil_means <- subject_pupil_means %>%
  mutate(
    # Adjust column names below to match whatever pivot_wider generated for you
    pupil_delta_tvns_sham = mean_pupil_2 - mean_pupil_1 
  )

# 4. Merge these new metrics back into your main shooting dataframe by SID
shooting_with_covariates <- shooting_trimmed %>%
  left_join(subject_pupil_means, by = "SID")

library(dplyr)
library(broom.mixed)
# 1. Grab the full subject-specific coefficients (Fixed Effect + Subject Deviation)
# This gives you the actual net value per subject directly
subject_coefs <- coef(nlme_tvns_pupil_state)

# Depending on how nlme names its coefficient columns, let's pull out mu for stim_type2
# Usually the column names look like "mu.stim_type2" or similar
# Let's inspect the names first if needed, but this works directly:
subject_mu_effect_df <- data.frame(
  SID = rownames(subject_coefs),
  # Adjust the column name below if your summary table calls it something slightly different
  total_mu_tVNS_effect = subject_coefs[["mu.stim_type2"]] 
)

# 2. Join with your trait covariate dataframe
trait_test_df <- shooting_with_covariates %>%
  distinct(SID, pupil_delta_tvns_sham, mean_pupil_1, mean_pupil_2) %>%
  left_join(subject_mu_effect_df, by = "SID")

# 3. Run the trait-level test
trait_model <- lm(total_mu_tVNS_effect ~ pupil_delta_tvns_sham, data = trait_test_df)
summary(trait_model)

# Add a squared term to test for an accelerated / S-shaped / non-linear trend
trait_model_quad <- lm(
  total_mu_tVNS_effect ~ pupil_delta_tvns_sham + I(pupil_delta_tvns_sham^2), 
  data = trait_test_df
)

summary(trait_model_quad)

# Test if tVNS pupil size predicts mu, while controlling for their Sham baseline pupil
trait_model_controlled <- lm(
  total_mu_tVNS_effect ~ mean_pupil_2 + mean_pupil_1, 
  data = trait_test_df
)
summary(trait_model_controlled)

# 1. Define the range of pupil_pre_phasic to plot
p_max_seq <- seq(-0.01, 0.01, length.out = 100)

# 2. Extract the relevant fixed effects (including the interaction term)
target_params <- c("mu.(Intercept)", "mu.stim_type2", "mu.pupil_pre_phasic")
coefs <- fixef(nlme_tvns_pupil_state)[target_params] # Replace 'model' with your actual nlme model object name if different

# 3. Extract the Variance-Covariance matrix for these parameters
vcov_mat <- vcov(nlme_tvns_pupil_state)[target_params, target_params]
# 4. Build the Marginal Design Matrix (holding stim_type at 0.5 to average across conditions)
# Columns: Intercept(1), stim_type (0.5), pupil_pre_phasic (seq)
X_marginal <- cbind(1, 0.5, p_max_seq)

# 5. Calculate predicted mu and Standard Errors for the single marginal line
pred_marginal <- X_marginal %*% coefs
se_marginal <- sqrt(diag(X_marginal %*% vcov_mat %*% t(X_marginal)))

# 6. Calculate empirical means of pupil_pre_phasic for each condition
pmax_sham <- mean(shooting$pupil_pre_phasic[shooting$stim_type == 1], na.rm = TRUE)
pmax_tvns <- mean(shooting$pupil_pre_phasic[shooting$stim_type == 2], na.rm = TRUE)

# 7. Calculate exact predicted mu for these means ON THE MARGINAL LINE
marginal_intercept <- coefs["mu.(Intercept)"] + (0.5 * coefs["mu.stim_type2"])
marginal_slope <- coefs["mu.pupil_pre_phasic"]

mu_sham_marginal <- marginal_intercept + (marginal_slope * pmax_sham)
mu_tvns_marginal <- marginal_intercept + (marginal_slope * pmax_tvns)

# 8. Create the plotting dataframe for the single line
plot_df <- data.frame(
  pupil_pre_phasic = p_max_seq,
  mu_pred = as.vector(pred_marginal),
  ci_lower = as.vector(pred_marginal - 1.96 * se_marginal),
  ci_upper = as.vector(pred_marginal + 1.96 * se_marginal)
)

# Define custom colors for the drop-lines
color_sham <- "#3498db"  
color_tvns <- "green"  

# 9. Generate the single-line plot with custom colored drop-lines
pre_phasic_mu_tvns = ggplot(plot_df, aes(x = pupil_pre_phasic, y = mu_pred)) +
  # Confidence ribbon around the single line
  #geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "purple", alpha = 0.2, linetype = 0) +
  # Single main trajectory line
  geom_line(color = "purple4", linewidth = 1.5) +
  
  # --- DROP-LINES: SHAM ---
  geom_segment(aes(x = pmax_sham, xend = pmax_sham, y = -Inf, yend = mu_sham_marginal), 
               linetype = "dashed", color = color_sham, linewidth = 1) +
  geom_segment(aes(x = -Inf, xend = pmax_sham, y = mu_sham_marginal, yend = mu_sham_marginal), 
               linetype = "dashed", color = color_sham, linewidth = 1) +
  geom_point(aes(x = pmax_sham, y = mu_sham_marginal), color = color_sham, size = 4) +
  
  # --- DROP-LINES: tVNS ---
  geom_segment(aes(x = pmax_tvns, xend = pmax_tvns, y = -Inf, yend = mu_tvns_marginal), 
               linetype = "dashed", color = color_tvns, linewidth = 1) +
  geom_segment(aes(x = -Inf, xend = pmax_tvns, y = mu_tvns_marginal, yend = mu_tvns_marginal), 
               linetype = "dashed", color = color_tvns, linewidth = 1) +
  geom_point(aes(x = pmax_tvns, y = mu_tvns_marginal), color = color_tvns, size = 4) +
  
  # --- ANNOTATIONS ---
  annotate("text", x = pmax_sham - 0.001, y = mu_sham_marginal + 0.002, 
           label = "Sham Mean", color = color_sham, fontface = "bold", hjust = 1) +
  annotate("text", x = pmax_tvns + 0.001, y = mu_tvns_marginal - 0.002, 
           label = "tVNS Mean", color = color_tvns, fontface = "bold", hjust = 0) +
  
  theme_bw(base_size = 15) +
  labs(
    title = "Effect of Pre-Phasic Pupil Size on\nPreparation Time (tVNS)",
    x = "Pre Prep Tone Pupil Size\n(a.u., [mean-centered])",
    y = expression("Estimated Preparation Time ( " * mu * "; s)")
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )+
  ylim(0.185, 0.21)

# Save the composite plot as a 300 DPI high-resolution PNG
ggsave(
  filename = "pre_phasic_mu_tvns.png",
  plot = pre_phasic_mu_tvns,
  width = 390*(300/96),
  height = 550*(300/96),
  dpi = 300,
  units = "px"
)

# Load required library
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# 1. Define the range of pupil_max to plot
p_max_seq <- seq(-0.02, 0.02, length.out = 100)

# 2. Extract the exact fixed effects in the order they appear in your output
target_params <- c("mu.(Intercept)", "mu.stim_type2", "mu.pupil_phasic", 
                   "mu.pupil_state",  "mu.stim_type2:pupil_state")
coefs <- fixef(nlme_tvns_pupil_state_int)[target_params]

# 3. Extract the Variance-Covariance matrix
vcov_mat <- vcov(nlme_tvns_pupil_state_int)[target_params, target_params]

# 4. Build Design Matrices
# Holding pupil_state and vigor_wc at 0 (their means)
# Columns: Intercept(1), stim, p_max, p_state(0), vigor(0), stim*p_max
X_sham <- cbind(1, 0, p_max_seq, 0, 0)
X_tvns <- cbind(1, 1, p_max_seq, 0, 1 * p_max_seq)

# 5. Calculate Predictions and Confidence Intervals
pred_sham <- X_sham %*% coefs
se_sham <- sqrt(diag(X_sham %*% vcov_mat %*% t(X_sham)))

pred_tvns <- X_tvns %*% coefs
se_tvns <- sqrt(diag(X_tvns %*% vcov_mat %*% t(X_tvns)))

# 6. Build Plotting Dataframe
plot_df <- data.frame(
  pupil_max = rep(p_max_seq, 2),
  Condition = factor(rep(c("Sham", "tVNS"), each = 100), levels = c("Sham", "tVNS")),
  mu_pred = c(as.vector(pred_sham), as.vector(pred_tvns)),
  ci_lower = c(as.vector(pred_sham - 1.96 * se_sham), as.vector(pred_tvns - 1.96 * se_tvns)),
  ci_upper = c(as.vector(pred_sham + 1.96 * se_sham), as.vector(pred_tvns + 1.96 * se_tvns))
)

# 7. Generate the Plot
ggplot(plot_df, aes(x = pupil_max, y = mu_pred, color = Condition, fill = Condition)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.5) +
  
  # Custom colors: Classic dark gray for Sham, Electric blue for tVNS
  scale_color_manual(values = c("Sham" = "gray20", "tVNS" = "dodgerblue3")) +
  scale_fill_manual(values = c("Sham" = "gray20", "tVNS" = "dodgerblue3")) +
  
  theme_bw(base_size = 15) +
  labs(
    title = "tVNS Blunts the Utility of Phasic Arousal",
    subtitle = "Electrical stimulation flattens the computational benefit of a pupil burst",
    x = "Phasic Pupil Response (pupil_max [mean-centered])",
    y = expression("Estimated Preparation Time (" * mu * ")")
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_blank()
  )

# 3. Run the model with block_num
shooting_trimmed$block_num = shooting_trimmed$block_num - 2.5 # center on zero
# Step A: Fit a main-effects only model first (this almost always converges)
m_tvns_main <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(mu ~ stim_type + pupil_state + block_num + pupil_max, sigma ~ 1, asym ~ 1, gamma ~ 1),
  random = list(SID = pdDiag(mu ~ 1)),
  data = shooting_trimmed, # Make sure this is trimmed!
  start = list(fixed = c(nls_coefs["mu"], 0, 0, 0, 0, nls_coefs["sigma"], nls_coefs["asym"], nls_coefs["gamma"]))
)
# 1. Ensure starts_main is extracted correctly
starts_main <- fixef(m_tvns_main) 
# starts_main should have: (Intercept), stim_type2, pupil_state, pupil_max, sigma, asym, gamma (7 total)

# 2. Build the start vector for the 5-coefficient mu model
# mu: Int, stim, state, max, stim:state  (5 values)
# Then: sigma, asym, gamma (3 values)
new_starts <- c(
  starts_main[1],          # Intercept
  starts_main[2],          # stim_type2
  starts_main[3],          # pupil_state
  starts_main[4],          # block_num
  starts_main[5],          # pupil_max
  0,  # stim:pupil
  0,  # block_num:pupil
  0,  # block_num:stim
  0,  # block_num:stim:pupil
  starts_main[6:8]         # sigma, asym, gamma
)

nlme_tvns_pupil_state_block <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ stim_type * pupil_state * block_num + pupil_max,
    sigma ~ 1, asym ~ 1, gamma ~ 1
  ),
  random = mu ~ 1 | SID,
  data = shooting_trimmed,
  start = new_starts,
  control = nlmeControl(opt = "nlminb", pnlsMaxIter = 1000, msMaxIter = 1000)
)

summary(nlme_tvns_pupil_state_block)

# anova to test fit of block_num model
anova(shooting.nlme, shooting.no_asym_int_nlme, nlme_tvns_pupil_state, nlme_tvns_pupil_state_block)
# clearly demonstrates adding block_num just muddies up results, justifies removing this

# RETRY model with sigma effects too

# Step A: Fit a main-effects only model first (this almost always converges)
m_tvns_main <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(mu ~ stim_type + pupil_state + pupil_max, sigma ~ stim_type + pupil_state, asym ~ 1, gamma ~ 1),
  random = list(SID = pdDiag(mu ~ 1)),
  data = shooting, # Make sure this is trimmed!
  start = list(fixed = c(nls_coefs["mu"], 0, 0, 0, nls_coefs["sigma"], 0, 0, nls_coefs["asym"], nls_coefs["gamma"]))
)
# 1. Ensure starts_main is extracted correctly
starts_main <- fixef(m_tvns_main) 
# starts_main should have: (Intercept), stim_type2, pupil_state, pupil_max, sigma, asym, gamma (7 total)

# mu: Int, stim, state, max, stim:state  (5 values)
# Then: sigma, asym, gamma (3 values)
new_starts <- c(
  starts_main[1:4], # mu main effects (Int, stim, state, max)
  0,                # mu interaction term (stim:state)
  starts_main[5:7], # sigma main effects (Int, stim, state)
  0,                # sigma interaction term (stim:state)
  starts_main[8],   # asym
  starts_main[9]    # gamma
)

# 3. Run the model with a more robust optimizer
nlme_tvns_pupil_state_sigma <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ stim_type * pupil_state + pupil_max,
    sigma ~ stim_type * pupil_state, 
    asym ~ 1, gamma ~ 1
  ),
  random = mu ~ 1 | SID,
  data = shooting_trimmed,
  start = new_starts,
  control = nlmeControl(opt = "nlminb", pnlsMaxIter = 1000, msMaxIter = 1000)
)

summary(nlme_tvns_pupil_state_sigma)
#model will not converge, unsurprising, justifies sticking to mu effects since our focus is preparation speed

# Check Collinearity with vif()

vif_model <- lm(success ~ stim_type * pupil_state + pupil_max, data = shooting_trimmed)

# Run the VIF analysis
vif(vif_model)
# confirms no dangerous vif values

# Residual Diagnostics

qqnorm(nlme_tvns_pupil_state, ~ranef(.))

plot(nlme_tvns_pupil_state, resid(., type = "p") ~ fitted(.) | stim_type)

# test pupil dependancy

autonomic_override_model <- lmer(
  pupil_max ~ pupil_state * stim_type + 
    (1 | SID), 
  data = shooting_trimmed
)

summary(autonomic_override_model)

# view crossover



# 1. Extract marginal effects (same as before)
model_preds <- ggpredict(autonomic_override_model, terms = c("pupil_state_scaled", "stim_type"))

# 2. Build the unified plot
ggplot() +
  # Raw data points in the background
  geom_point(data = shooting_trimmed, 
             aes(x = pupil_state, y = pupil_max, color = stim_type), 
             alpha = 0.15, size = 1) +
  
  # Model predictions: confidence ribbons and regression lines
  geom_ribbon(data = model_preds, 
              aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.3) +
  geom_line(data = model_preds, 
            aes(x = x, y = predicted, color = group), 
            linewidth = 1.2) +
  
  # Aesthetics and labels
  theme_minimal(base_size = 14) +
  
  # Fixed color manual
  scale_color_manual(name = "tVNS Condition",
                     values = c("1" = "#3182bd", "2" = "#de2d26"), 
                     labels = c("1" = "Sham", "2" = "tVNS")) +
  
  # Added fill manual with identical parameters to beautifully merge the legends
  scale_fill_manual(name = "Condition",
                    values = c("1" = "#3182bd", "2" = "#de2d26"), 
                    labels = c("1" = "Sham", "2" = "tVNS")) +
  
  labs(
    title = "Tonic-Phasic Pupil Dynamics by tVNS Condition",
    x = "Baseline Pupil Size (Tonic)",
    y = "Peak Pupil Response (Phasic)"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

# --- PLOT 2: The Pupil State Interaction ---
coefs <- fixef(nlme_tvns_pupil_state)

# Calculate Sham and tVNS slopes based on model coefficients
intercept_sham <- coefs["mu.(Intercept)"]
slope_sham <- coefs["mu.pupil_state"]
intercept_tvns <- coefs["mu.(Intercept)"] + coefs["mu.stim_type2"]
slope_tvns <- coefs["mu.pupil_state"] + coefs["mu.stim_type2:pupil_state"]

pupil_range <- seq(min(shooting_trimmed$pupil_state), max(shooting_trimmed$pupil_state), length.out = 100)
df_plot <- data.frame(
  pupil_scaled = rep(pupil_range, 2),
  Condition = rep(c("Sham", "Active tVNS"), each = 100),
  predicted_mu = c(
    intercept_sham + slope_sham * pupil_range,
    intercept_tvns + slope_tvns * pupil_range
  )
)

pupil_interaction_plot <- ggplot(df_plot, aes(x = pupil_scaled, y = predicted_mu, color = Condition)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("Sham" = "#4A5568", "Active tVNS" = "#DD6B20")) +
  labs(
    title = "tVNS hurts under low and helps under high base pupil",
    x = "Trial-by-Trial Phasic Pupil Size (Within-Subject normalized)",
    y = expression("Predicted Decision Threshold (" * mu * ")"),
  ) +
  ylim(0.175, 0.225) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 18),
    plot.tag = element_text(size = 35, face = "bold")
  )
print(pupil_interaction_plot)


# Load required library
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# 1. Define your axes and representative values
# Range for x-axis
p_max_seq <- seq(-0.015, 0.015, length.out = 100) 

# REPLACE THESE with actual -1 SD and +1 SD of pupil_state
p_state_low <- -0.01  
p_state_high <- 0.01  

# 2. Extract the exact fixed effects needed
target_params <- c("mu.(Intercept)", "mu.stim_type2", "mu.pupil_state", "mu.pupil_max", 
                   "mu.stim_type2:pupil_state", "mu.stim_type2:pupil_max", 
                   "mu.pupil_state:pupil_max", "mu.stim_type2:pupil_state:pupil_max")

coefs <- fixef(nlme_tvns_pupil_state_int)[target_params]
vcov_mat <- vcov(nlme_tvns_pupil_state_int)[target_params, target_params]

# 3. Build Design Matrices for the 4 combinations
# Columns: Int(1), stim, p_state, p_max, stim*p_state, stim*p_max, p_state*p_max, stim*p_state*p_max

# SHAM (stim_type2 = 0)
X_sham_low  <- cbind(1, 0, p_state_low, p_max_seq,  0, 0,  p_state_low*p_max_seq,  0)
X_sham_high <- cbind(1, 0, p_state_high, p_max_seq, 0, 0,  p_state_high*p_max_seq, 0)

# tVNS (stim_type2 = 1)
X_tvns_low  <- cbind(1, 1, p_state_low, p_max_seq,  1*p_state_low,  1*p_max_seq,  p_state_low*p_max_seq,  1*p_state_low*p_max_seq)
X_tvns_high <- cbind(1, 1, p_state_high, p_max_seq, 1*p_state_high, 1*p_max_seq,  p_state_high*p_max_seq, 1*p_state_high*p_max_seq)

# Combine all matrices
X_all <- rbind(X_sham_low, X_sham_high, X_tvns_low, X_tvns_high)

# 4. Calculate Predictions and Confidence Intervals
pred_mu <- X_all %*% coefs
se_mu <- sqrt(diag(X_all %*% vcov_mat %*% t(X_all)))

# 5. Build Plotting Dataframe
plot_df <- data.frame(
  pupil_max = rep(p_max_seq, 4),
  Stimulation = factor(rep(c("Sham", "Sham", "tVNS", "tVNS"), each = 100), levels = c("Sham", "tVNS")),
  Tonic_State = factor(rep(c("Low Tonic State", "High Tonic State", "Low Tonic State", "High Tonic State"), each = 100), 
                       levels = c("High Tonic State", "Low Tonic State")), # High first so it plots on top
  mu_pred = as.vector(pred_mu),
  ci_lower = as.vector(pred_mu - 1.96 * se_mu),
  ci_upper = as.vector(pred_mu + 1.96 * se_mu)
)

# 6. Generate the Plot
ggplot(plot_df, aes(x = pupil_max, y = mu_pred, color = Tonic_State, fill = Tonic_State)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.5) +
  
  # Split into Sham and tVNS panels
  facet_wrap(~Stimulation) +
  
  # Custom colors: Dark Red for High Tonic, Steel Blue for Low Tonic
  scale_color_manual(values = c("High Tonic State" = "darkred", "Low Tonic State" = "steelblue")) +
  scale_fill_manual(values = c("High Tonic State" = "darkred", "Low Tonic State" = "steelblue")) +
  
  theme_bw(base_size = 15) +
  labs(
    title = "tVNS Inverts the Synergistic Effect of Arousal",
    subtitle = "Under tVNS, combining high tonic and high phasic arousal actively degrades prep time",
    x = "Phasic Pupil Response (pupil_max [mean-centered])",
    y = expression("Estimated Preparation Time (" * mu * ")")
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 14), # Bolds the Sham/tVNS panel headers
    legend.position = "top",
    legend.title = element_blank()
  )