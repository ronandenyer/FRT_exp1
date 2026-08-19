library(lmerTest)
library(dplyr)
library(emmeans)
library(dplyr)

shooting_tvns <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')
shooting_reward <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')

# Add the 'condition' mapping and a source tracker
shooting_tvns$condition <- shooting_tvns$stim_type
shooting_tvns$experiment_source <- "tVNS_Session"

shooting_reward$condition <- shooting_reward$reward_type
shooting_reward$experiment_source <- "Reward_Session"

# Find common columns 
common_cols <- intersect(names(shooting_tvns), names(shooting_reward))

# Bind them, explicitly forcing dplyr namespace
shooting_combined <- bind_rows(
  shooting_tvns %>% dplyr::select(all_of(common_cols)),
  shooting_reward %>% dplyr::select(all_of(common_cols))
)


# Remove NAs
shooting_combined <- shooting_combined[!is.na(shooting_combined$eff_pt) & 
                       !is.na(shooting_combined$success) & 
                       !is.na(shooting_combined$pupil_size_mean), ]

# Standardize units (Fixed double division of eff_pt)
shooting_combined$eff_pt <- shooting_combined$eff_pt / 1000 # Convert to seconds
shooting_combined$pupil_size_phasic_mean_norm <- shooting_combined$pupil_size_phasic_mean_norm * 1e4
shooting_combined$pupil_size_phasic_mean <- shooting_combined$pupil_size_phasic_mean * 1e4
shooting_combined$pupil_size_pre_phasic_mean_norm <- shooting_combined$pupil_size_pre_phasic_mean_norm * 1e4
shooting_combined$pupil_size_pre_phasic_mean <- shooting_combined$pupil_size_pre_phasic_mean * 1e4
shooting_combined$pupil_size_phasic_slope <- shooting_combined$pupil_size_phasic_slope * 1e4
shooting_combined$pupil_size_max <- shooting_combined$pupil_size_max * 1e4

shooting_combined$pupil_size_mean <- shooting_combined$pupil_size_mean * 1e4
shooting_combined$condition = as.factor(shooting_combined$condition)
shooting_combined$condition <- factor(shooting_combined$condition, levels = c("3", "1", "2", "4"))
# -------------------------------------------------------------------------
# 2. WITHIN-SUBJECT CENTERING & SCALING
# -------------------------------------------------------------------------
shooting_combined <- shooting_combined %>%
  group_by(SID) %>%
  mutate(
    pupil_state = pupil_size_mean - mean(pupil_size_mean, na.rm = TRUE),
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
shooting_combined_trimmed <- shooting_combined %>%
  filter(
    abs(pupil_state_scaled) <= 3,
    abs(pupil_pre_phasic_scaled) <= 3,
    abs(pupil_slope_scaled) <= 3,
    abs(pupil_max_scaled) <= 3,
    abs(vigor_scaled) <= 3
  )

library(dplyr)
library(tidyr)

# 1. Extract the Mean and SD of the baseline pupil for every participant on both days
# (Assuming you have a combined dataframe with a column like 'experiment_day')
pupil_metrics <- shooting_combined_trimmed %>%
  group_by(SID, experiment_source) %>%
  summarize(
    subject_mean = mean(pupil_size_mean, na.rm = TRUE),
    subject_sd = sd(pupil_size_mean, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Test the Mean Difference (Does the center shift up?)
t.test(subject_mean ~ experiment_source, data = pupil_metrics, paired = TRUE)

# 3. Test the Variability Difference (Does the variance shrink?)
t.test(subject_sd ~ experiment_source, data = pupil_metrics, paired = TRUE)

pupil_state_lmer = lmer(pupil_slope ~ pupil_state*experiment_source + 
                          (1 + pupil_state + experiment_source| SID),
                        data = shooting_combined_trimmed)

summary(pupil_state_lmer)


rt_lmer = lmer(rt ~ condition + 
                          (1| SID),
                        data = shooting_combined)

summary(rt_lmer)

pupil_state_prev_trial = lmer(pupil_size_pre_phasic_mean ~ experiment_source*prev_trial_success + 
                          (1 + experiment_source+prev_trial_success| SID),
                        data = shooting_combined)

summary(pupil_state_prev_trial)

library(emmeans)

# 1. Calculate estimated marginal means for each condition
emm <- emmeans(pupil_state_lmer, ~ condition)
print(emm)

# 2. Run pairwise group comparisons (with Tukey p-value adjustment by default)
pairwise_comparisons <- pairs(emm, adjust = "tukey")
print(summary(pairwise_comparisons))
# 1. Load required libraries


# 2. Ensure condition is a factor
shooting_combined$condition <- as.factor(shooting_combined$condition)

# 3. The Global Baseline Model
# Using a random intercept for SID since estimating 4 random slopes 
# for 28 participants might crash the optimizer
global_baseline_model <- lmer(
  pupil_size_mean ~ experiment_source + (1 | SID), 
  data = shooting_combined
)

# 4. Check the overall main effect of condition
summary(global_baseline_model)

# 5. The Critical Step: Pairwise Contrasts
# This calculates the exact baseline for all 4 conditions and compares them
baseline_contrasts <- emmeans(global_baseline_model, pairwise ~ condition, adjust = "tukey")

# 6. View the results
summary(baseline_contrasts)

library(lmerTest)
library(emmeans)

# 1. Run the combined interaction model (simplified random effect for stability)
combined_gain_model <- lmer(
  pupil_size_max ~ condition * pupil_size_pre_phasic_mean_norm + 
    (1 | SID), 
  data = shooting_trimmed
)



# 2. Extract and compare the SLOPES (the neural gain multiplier)
# var = specifies which continuous variable's slope we are testing
slope_contrasts <- emtrends(
  combined_gain_model, 
  pairwise ~ condition, 
  var = "pupil_size_pre_phasic_mean_norm",
  adjust = "tukey"
)

# 3. View the results
summary(slope_contrasts)



# 2. Calculate the estimated marginal means for the condition factor
pupil_phasic_emms <- emmeans(pupil_phasic_lmer, specs = ~ condition)

# 3. View the means, standard errors, and confidence intervals per condition
print(pupil_phasic_emms)

# 4. Perform pairwise comparisons between conditions (with automatic Tukey adjustment)
pupil_phasic_pairs <- pairs(pupil_phasic_emms)
print(pupil_phasic_pairs)


# Check your Inverted-U theory right away:
# Does the tVNS session have a higher/wider baseline pupil distribution?
ggplot(shooting_combined, aes(x = as.factor(condition), y = pupil_size_max)) +
  stat_summary(alpha = 0.5) +
  theme_bw()

library(dplyr)
library(ggplot2)

# 1. Create a helper function to identify valid data (non-outliers)
# Uses 3x IQR for "extreme" outliers. Change multiplier to 1.5 for "mild" outliers.
is_not_extreme_outlier <- function(x, multiplier = 3) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  lower_bound <- q1 - (multiplier * iqr)
  upper_bound <- q3 + (multiplier * iqr)
  
  # Returns TRUE if the value is within the acceptable bounds
  return(x >= lower_bound & x <= upper_bound)
}
shooting_combined$condition <- factor(shooting_combined$condition, levels = c("1", "2", "3", "4"))

# 2. Filter the data, then calculate tertiles
shooting_data_plot <- shooting_combined %>%
  # Filter outliers WITHIN each subject and session to account for baseline shifts
  group_by(SID, experiment_source) %>%
  filter(
    is_not_extreme_outlier(pupil_state),
    is_not_extreme_outlier(pupil_max)
  ) %>%
  ungroup() %>%
  
  # Now group by session (day) alone to calculate the tertiles
  #group_by() %>%
  mutate(
    pupil_tertile = ntile(pupil_state, 2), 
    pupil_tertile = factor(pupil_tertile, 
                           levels = c(1, 2), 
                           labels = c("Low", "High"))
  ) #%>%
  #ungroup()

# Pass the exact vector of levels in the order you want them

pupil_median_state_lmer = lmer(pupil_max ~ condition*pupil_tertile + 
                          (1| SID),
                        data = shooting_data_plot)
summary(pupil_median_state_lmer)

library(nlme)

## test prev_trial_success effects


shooting_post_error_c = shooting_combined[!is.na(shooting_combined$prev_trial_success),]
shooting.prev_trial_combined_nlme <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(mu ~ prev_trial_success, sigma ~ prev_trial_success, asym ~ prev_trial_success, gamma ~ 1), 
  random = list(SID = pdDiag(mu + asym + sigma ~ 1)),
  data = shooting_post_error_c,
  start = list(fixed = c(0.2, 0, 0.02, 0, 0.95, 0, 0.25))
)
summary(shooting.prev_trial_combined_nlme)


# ==========================================
# STAGE 1: Fit the Main-Effects Model First
# ==========================================
# This model estimates the intercepts for all conditions, pupil_state, 
# and pupil_pre_phasic without the interaction complexity, ensuring easy convergence.

m_unified_main <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ condition + pupil_state + pupil_pre_phasic, 
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  random = list(SID = pdDiag(mu ~ 1)),
  data = shooting_trimmed,
  # Provide reasonable baseline starts: 
  # mu intercept (~0.2), 3 condition dummies (0), pupil_state (0), pupil_pre_phasic (0), sigma (~0.02), asym (~0.95), gamma (~0.23)
  start = list(fixed = c(0.2, 0, 0, 0, 0, 0, 0.02, 0.95, 0.23)),
  control = nlmeControl(opt = "nlminb", pnlsMaxIter = 500, msMaxIter = 500)
)

# Extract the converged main-effect coefficients
starts_main <- fixef(m_unified_main)
# Expected length of starts_main: 9 coefficients 
# [1] Intercept, [2-4] condition dummies, [5] pupil_state, [6] pupil_pre_phasic, [7] sigma, [8] asym, [9] gamma


# ==========================================
# STAGE 2: Build the Full Interaction Model
# ==========================================
# Now we expand the formula to include condition:pupil_pre_phasic interactions.
# We inject 0s for the interaction terms using the coefficients from Stage 1.

new_starts_unified <- c(
  starts_main[1],          # Intercept
  starts_main[2:4],        # condition coefficients (e.g., condition2, condition3, condition4)
  starts_main[6],          # pupil_state
  starts_main[5], 
  -0.3,# pupil_pre_phasic
  0,                # New interaction terms (condition x pupil_pre_phasic) set to 0 initially
  starts_main[7:9]         # sigma, asym, gamma
)

# Run the master unified interaction model
nlme_unified_interaction <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ condition * pupil_state,
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  random = mu + sigma ~ 1 | SID,
  data = shooting_combined,
  start = new_starts_unified,
  control = nlmeControl(opt = "nlminb", pnlsMaxIter = 1000, msMaxIter = 1000)
)

# View the master results
summary(nlme_unified_interaction)
# 1. Map conditions to their empirical tonic arousal rank
shooting_combined$tonic_rank <- NA
shooting_combined$tonic_rank[shooting_combined$condition == "3"] <- 1 # No Reward (Lowest)
shooting_combined$tonic_rank[shooting_combined$condition == "4"] <- 2 # Reward
shooting_combined$tonic_rank[shooting_combined$condition == "1"] <- 3 # Sham
shooting_combined$tonic_rank[shooting_combined$condition == "2"] <- 4 # tVNS (Highest)

# 2. Center the rank so 0 sits in the middle (between Reward and Sham)
shooting_combined$tonic_rank_c <- shooting_combined$tonic_rank - mean(shooting_combined$tonic_rank)

# 3. Create the squared term to test for non-linear diminishing returns / curvature
shooting_combined$tonic_rank_c_sq <- shooting_combined$tonic_rank_c^2



# 1. Define the safe probability function
safe_pnorm <- function(q, mean, sd) {
  pnorm(q, mean = mean, sd = pmax(sd, 1e-4))
}

# 2. Properly structure the dataset:
# - Calculate condition means using the RAW metric (fixing the normalization flaw)
# - Orthogonalize the trial-level raw metric so it doesn't collide with the condition anchors
shooting_combined <- shooting_combined %>%
  group_by(condition) %>%
  mutate(
    condition_mean_raw = mean(pupil_size_pre_phasic_mean_norm, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    # Macro anchors: Condition-level raw means centered globally
    tonic_empirical_c = condition_mean_raw - mean(condition_mean_raw, na.rm = TRUE),
    tonic_empirical_c_sq = tonic_empirical_c^2,
    
    # Micro trial-level metric: Within-condition raw variation (orthogonalized)
    # This uses the true raw scale while removing collinearity with the condition anchors
    pupil_raw_orthogonal = pupil_pre_phasic_raw - condition_mean_raw,
    pupil_raw_cent = as.numeric(scale(pupil_raw_orthogonal)),
    
    # State control (centered)
    pupil_state_cent = as.numeric(scale(pupil_size_mean))
  )

# 3. Fit the corrected anchored Yerkes-Dodson model with the raw trial metric
nlme_fixed_anchored_model <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  
  fixed = list(
    mu ~ (tonic_empirical_c + tonic_empirical_c_sq) * pupil_raw_cent + pupil_state_cent,
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  
  random = mu + sigma ~ 1 | SID,
  data = shooting_combined,
  
  start = c(
    "mu.(Intercept)"                         = 0.20,
    "mu.tonic_empirical_c"                   = 0.00,
    "mu.tonic_empirical_c_sq"                = 0.00,
    "mu.pupil_raw_cent"                      = 0.00,
    "mu.pupil_state_cent"                    = 0.00,
    "mu.tonic_empirical_c:pupil_raw_cent"    = 0.00,
    "mu.tonic_empirical_c_sq:pupil_raw_cent" = 0.00,
    "sigma"                                  = 0.025,
    "asym"                                   = 0.948,
    "gamma"                                  = 0.245
  ),
  
  control = nlmeControl(opt = "nlminb", pnlsMaxIter = 1000, msMaxIter = 1000)
)

# View the corrected results
summary(nlme_fixed_anchored_model)
library(nlme)
library(dplyr)

# 1. Define the safe probability function
safe_pnorm <- function(q, mean, sd) {
  pnorm(q, mean = mean, sd = pmax(sd, 1e-4))
}

# 2. Structure the dataset strictly for the macro condition anchors
shooting_combined <- shooting_combined %>%
  group_by(condition) %>%
  mutate(
    # Using the normalized metric as your reliable empirical baseline per condition
    condition_mean_tonic = mean(pupil_pre_phasic, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    # Macro anchors: Condition-level means centered globally
    tonic_empirical_c    = condition_mean_tonic - mean(condition_mean_tonic, na.rm = TRUE),
    tonic_empirical_c_sq = tonic_empirical_c^2,
    
    # General trial-to-trial state control (centered)
    pupil_state_cent     = as.numeric(scale(pupil_size_mean))
  )

# 3. Fit the pure macro Yerkes-Dodson model (No single-trial predictors)
nlme_pure_anchored_model <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  
  fixed = list(
    mu ~ tonic_empirical_c + tonic_empirical_c_sq + pupil_state_cent,
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  
  random = mu + sigma ~ 1 | SID,
  data = shooting_combined,
  
  start = c(
    "mu.(Intercept)"         = 0.20,
    "mu.tonic_empirical_c"   = 0.00,
    "mu.tonic_empirical_c_sq" = 0.00,
    "mu.pupil_state_cent"    = -0.17,
    "sigma"                  = 0.025,
    "asym"                   = 0.948,
    "gamma"                  = 0.245
  ),
  
  control = nlmeControl(opt = "nlminb", pnlsMaxIter = 1000, msMaxIter = 1000)
)

# View the pure, clean results
summary(nlme_pure_anchored_model)

library(nlme)
library(dplyr)

# 1. Define the safe probability function
safe_pnorm <- function(q, mean, sd) {
  pnorm(q, mean = mean, sd = pmax(sd, 1e-4))
}

# 2. Calculate condition means PER SUBJECT to isolate within-subject shifts
shooting_combined <- shooting_combined %>%
  group_by(SID, condition) %>%
  mutate(
    subject_condition_mean = mean(pupil_size_pre_phasic_mean_norm, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    # Macro anchors: Per-subject condition means, centered globally across the whole dataset
    tonic_empirical_c    = subject_condition_mean - mean(subject_condition_mean, na.rm = TRUE),
    tonic_empirical_c_sq = tonic_empirical_c^2,
    
    # General trial-to-trial state control (centered)
    pupil_state_cent     = as.numeric(scale(pupil_size_mean))
  )

# 3. Fit the pure macro Yerkes-Dodson model with per-subject anchors
nlme_subject_anchored_model <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  
  fixed = list(
    mu ~ tonic_empirical_c + tonic_empirical_c_sq + pupil_state_cent,
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  
  random = mu + sigma ~ 1 | SID,
  data = shooting_combined,
  
  start = c(
    "mu.(Intercept)"         = 0.20,
    "mu.tonic_empirical_c"   = 0.00,
    "mu.tonic_empirical_c_sq" = 0.00,
    "mu.pupil_state_cent"    = -0.17,
    "sigma"                  = 0.025,
    "asym"                   = 0.948,
    "gamma"                  = 0.245
  ),
  
  control = nlmeControl(opt = "nlminb", pnlsMaxIter = 1000, msMaxIter = 1000)
)

# View the results
summary(nlme_subject_anchored_model)

# 1. Extract the required fixed effects from the gradient model
coefs <- fixef(nlme_fixed_anchored_model)
mu_int  <- coefs["mu.(Intercept)"]
mu_lin  <- coefs["mu.tonic_empirical_c"]
mu_quad <- coefs["mu.tonic_empirical_c_sq"]

# 2. Define the exact "Sweet Spot" (the vertex of the parabola: -b / 2a)
sweet_spot_x <- -mu_lin / (2 * mu_quad)
sweet_spot_y <- mu_int + (mu_lin * sweet_spot_x) + (mu_quad * sweet_spot_x^2)

# 3. Create a continuous sequence of tonic pupil sizes for the curve
x_seq <- seq(-0.00065, 0.00065, length.out = 200)

# 4. Extract the variance-covariance matrix to build the confidence ribbon
target_params <- c("mu.(Intercept)", "mu.tonic_empirical_c", "mu.tonic_empirical_c_sq")
vcov_mat <- vcov(nlme_gradient_model)[target_params, target_params]

# Build design matrix and calculate predicted mu + Standard Errors
X_mat <- cbind(1, x_seq, x_seq^2)
mu_pred_seq <- X_mat %*% c(mu_int, mu_lin, mu_quad)
se_pred <- sqrt(diag(X_mat %*% vcov_mat %*% t(X_mat)))

# Create the curve dataframe
curve_df <- data.frame(
  tonic_c  = x_seq,
  mu_pred  = as.vector(mu_pred_seq),
  ci_lower = as.vector(mu_pred_seq - 1.96 * se_pred),
  ci_upper = as.vector(mu_pred_seq + 1.96 * se_pred)
)

# 5. Extract and map the 4 condition anchor points
cond_points <- shooting_combined %>%
  group_by(condition) %>%
  summarise(
    tonic_c = first(tonic_empirical_c),
    tonic_c_sq = first(tonic_empirical_c_sq),
    .groups = "drop"
  ) %>%
  mutate(
    mu_pred = mu_int + (mu_lin * tonic_c) + (mu_quad * tonic_c_sq),
    # Map the numerical condition codes to their actual names
    condition_name = case_when(
      condition == 1 ~ "Sham",
      condition == 2 ~ "tVNS",
      condition == 3 ~ "No Reward",
      condition == 4 ~ "Reward"
    )
  )

# Define distinct colors for the named conditions
cond_colors <- c(
  "Sham"      = "gray50",   # Neutral
  "tVNS"      = "#e74c3c",  # Active stimulation
  "No Reward" = "#3498db",  # Baseline reward 
  "Reward"    = "#f1c40f"   # Active reward 
)

# 6. Generate the Yerkes-Dodson Plot
ggplot(curve_df, aes(x = tonic_c, y = mu_pred)) +
  # Confidence ribbon
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "gray50", alpha = 0.15) +
  # The unified quadratic curve
  geom_line(color = "gray30", linewidth = 1.2) +
  
  # The Sweet Spot marker
  geom_vline(xintercept = sweet_spot_x, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  annotate("text", x = sweet_spot_x + 0.00002, y = max(curve_df$mu_pred) - 0.005, 
           label = "Optimal Arousal\n(Sweet Spot)", hjust = 0, fontface = "italic", color = "gray40") +
  
  # Condition points
  geom_point(data = cond_points, aes(color = condition_name), size = 5) +
  
  # Condition labels (Staggering Reward downwards so it doesn't overlap with Sham)
  geom_text(data = cond_points %>% filter(condition_name != "Reward"), 
            aes(label = condition_name, color = condition_name), 
            vjust = -1.2, fontface = "bold", show.legend = FALSE) +
  geom_text(data = cond_points %>% filter(condition_name == "Reward"), 
            aes(label = condition_name, color = condition_name), 
            vjust = 2.5, fontface = "bold", show.legend = FALSE) +
  
  # Visualizing the y-axis direction (Lower = Faster Prep Time)
  annotate("segment", x = -0.0006, xend = -0.0006, 
           y = mu_int + 0.005, yend = mu_int - 0.005, 
           arrow = arrow(length = unit(0.3, "cm")), color = "black", linewidth = 1) +
  annotate("text", x = -0.00058, y = mu_int, 
           label = "Faster Prep Time\n(Better Performance)", hjust = 0, fontface = "bold") +
  
  # Scales and theming
  scale_color_manual(values = cond_colors, name = "Condition") +
  theme_bw(base_size = 14) +
  labs(
    title = "Yerkes-Dodson Effect of Tonic Arousal on Preparation Time",
    subtitle = "Quadratic mixed-effects model anchoring empirical condition states",
    x = "Tonic Pupil Size (Centered Baseline Fluctuation; a.u.)",
    y = expression("Estimated Preparation Time (" * mu * "; s)")
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

library(nlme)

# 1. Create the squared term for the endogenous trial-by-trial pupil
shooting_combined$pupil_pre_phasic_sq <- shooting_combined$pupil_pre_phasic_scaled^2
library(dplyr)
library(nlme)

# 1. Identify the 1st and 99th percentiles to trim Eyelink artifacts
lower_bound <- quantile(shooting_combined$pupil_pre_phasic_scaled, 0.01, na.rm = TRUE)
upper_bound <- quantile(shooting_combined$pupil_pre_phasic_scaled, 0.99, na.rm = TRUE)



# Print how many trials were dropped (should be roughly 460 out of 23,000)
cat("Trials remaining after 1% trim:", nrow(shooting_trimmed), "\n")

# 3. Fit the optimized model
nlme_endogenous_optimized <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ condition + pupil_pre_phasic_scaled + pupil_pre_phasic_sq + pupil_state,
    sigma ~ 1, asym ~ 1, gamma ~ 1
  ),
  random = mu ~ 1 | SID,
  data = shooting_trimmed,
  start = c(
    0.20,    # mu.(Intercept)
    -0.005,  # mu.condition1
    -0.006,  # mu.condition2
    -0.010,  # mu.condition4
    -0.001,  # mu.pupil_pre_phasic_scaled (Linear slope)
    0.0004,  # mu.pupil_pre_phasic_sq (Quadratic curvature)
    0,       # mu.pupil_state (Absorbing the background drift)
    0.026,   # sigma
    0.948,   # asym
    0.245    # gamma
  ),
  control = nlmeControl(opt = "nlminb", pnlsMaxIter = 1000, msMaxIter = 1000)
)

summary(nlme_endogenous_optimized)


# Standardize pupil_positive to Z-scores (mean = 0, sd = 1)
shooting_clean <- shooting_combined %>%
  filter(!is.na(eff_pt), !is.na(success), !is.na(pupil_size_pre_phasic_mean)) %>%
  mutate(pupil_positive = as.numeric(scale(pupil_size_pre_phasic_mean)))

safe_pnorm <- function(q, mean, sd) {
  pnorm(q, mean = mean, sd = pmax(sd, 1e-4))
}
# Step 1: Fit simple fixed-effects NLS to find exact optimal start points
nls_start <- nls(
  success ~ (0.25 + (0.95 - 0.25) * safe_pnorm(eff_pt, mu_base + span * (1 - exp(-rate * pupil_positive)), 0.026)),
  data = shooting_clean,
  start = list(mu_base = 0.20, span = -0.01, rate = 0.5)
)

# Step 2: Use estimated parameters as nlme start values
start_vals <- coef(nls_start)

nlme_exponential_model <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu_base + span * (1 - exp(-rate * pupil_positive)), sigma)),
  fixed = list(mu_base ~ 1, span ~ 1, rate ~ 1, sigma ~ 1, asym ~ 1, gamma ~ 1),
  random = mu_base ~ 1 | SID,
  data = shooting_clean,
  start = c(
    "mu_base" = unname(start_vals["mu_base"]),
    "span"    = unname(start_vals["span"]),
    "rate"    = unname(start_vals["rate"]),
    "sigma"   = 0.026,
    "asym"    = 0.95,
    "gamma"   = 0.25
  ),
  control = nlmeControl(opt = "nlminb", minScale = 1e-6)
)

# View model summary
summary(nlme_exponential_model)


library(nlme)
library(dplyr)

# 1. Define the safe probability function
safe_pnorm <- function(q, mean, sd) {
  pnorm(q, mean = mean, sd = pmax(sd, 1e-4))
}
library(nlme)
library(dplyr)

# 1. Define the safe probability function
safe_pnorm <- function(q, mean, sd) {
  pnorm(q, mean = mean, sd = pmax(sd, 1e-4))
}

# 2. Prepare the full dataset with both state and orthogonal quadratic phasic terms
shooting_clean <- shooting_combined %>%
  filter(!is.na(eff_pt), !is.na(success), 
         !is.na(pupil_size_pre_phasic_mean_norm), !is.na(pupil_size_mean)) %>%
  mutate(
    # Center and scale baseline fluctuations
    pupil_state = as.numeric(scale(pupil_size_mean)),
    
    # Center and scale the pre-phasic metric for quadratic stability
    pupil_cent = as.numeric(scale(pupil_size_pre_phasic_mean_norm)),
    
    # Create the squared term
    pupil_sq = pupil_cent^2
  )

# 3. Fit the Quadratic Mixed-Effects Model with Baseline Control
nlme_quad_state_model <- nlme(
  # Formula for mu: mu_base + (baseline control) + (linear phasic) + (quadratic phasic)
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu_base + beta_state * pupil_state + beta1 * pupil_cent + beta2 * pupil_sq, sigma)),
  
  fixed = list(
    mu_base    ~ 1,   # Global intercept
    beta_state ~ 1,   # Linear effect of slow baseline fluctuations
    beta1      ~ 1,   # Linear effect of phasic pupil size
    beta2      ~ 1,   # Quadratic (U-shape) effect of phasic pupil size
    sigma      ~ 1,   # Psychometric slope
    asym       ~ 1,   # Upper asymptote
    gamma      ~ 1    # Lower asymptote
  ),
  
  random = mu_base  ~ 1 | SID,
  data = shooting_clean,
  
  # Starting values (linear and quadratic terms start flat at 0)
  start = c(
    "mu_base"    = 0.20,
    "beta_state" = 0.00,
    "beta1"      = 0.00,
    "beta2"      = 0.00,
    "sigma"      = 0.026,
    "asym"       = 0.95,
    "gamma"      = 0.25
  ),
  
  # Standard control settings
  control = nlmeControl(opt = "nlminb", pnlsMaxIter = 1000, msMaxIter = 1000)
)

# View the results
summary(nlme_quad_state_model)