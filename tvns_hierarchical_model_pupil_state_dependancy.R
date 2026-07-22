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
    pupil_slope = pupil_size_phasic_slope - mean(pupil_size_phasic_slope, na.rm = TRUE),
    pupil_max = pupil_size_max - mean(pupil_size_max, na.rm = TRUE),
    pupil_trait = mean(pupil_size_mean, na.rm = TRUE),
    vigor_wc = peak_tanVel - mean(peak_tanVel, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    pupil_state_scaled = pupil_state / sd(pupil_state, na.rm = TRUE),
    pupil_phasic_scaled = pupil_phasic / sd(pupil_phasic, na.rm = TRUE),
    pupil_slope_scaled = pupil_slope / sd(pupil_slope, na.rm = TRUE),
    pupil_max_scaled = pupil_max / sd(pupil_max, na.rm = TRUE),
    vigor_scaled = vigor_wc / sd(vigor_wc, na.rm = TRUE)
  )

# Trimmed dataset (Generated but held in reserve, models use 'shooting')
shooting_trimmed <- shooting %>%
  filter(
    abs(pupil_state_scaled) <= 3,
    abs(pupil_slope_scaled) <= 3,
    abs(pupil_max_scaled) <= 3,
    abs(vigor_scaled) <= 3
  )

# -------------------------------------------------------------------------
# 2B. MAIN EFFECTS OF TVNS ON MAIN VARIABLES
# -------------------------------------------------------------------------

# change in baseline pupil (mean average 1 second before tvns starts)
pupil_state_tvns <- lmer(
  pupil_state ~ stim_type + 
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

# change in reach peak velocity
vigor_tvns <- lmer(
  vigor_wc ~ stim_type + 
    (1 + stim_type | SID), 
  data = shooting_trimmed
)

summary(vigor_tvns)


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

# Test 1: Can we drop the asymptote parameter? (Expecting p > 0.05)
cat("\n--- LRT: Can we drop Asymptote from the model? ---\n")


# ------------------------------------------------------------------------------
# SECTION 3: STATE-DEPENDENT HYPOTHESIS & TRAIT CONTROL
# ------------------------------------------------------------------------------


# Step A: Fit a main-effects only model first (this almost always converges)
m_tvns_main <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(mu ~ stim_type + pupil_state + pupil_phasic, sigma ~ 1, asym ~ 1, gamma ~ 1),
  random = list(SID = pdDiag(mu ~ 1)),
  data = shooting_trimmed, # Make sure this is trimmed!
  start = list(fixed = c(nls_coefs["mu"], 0, 0, 0, nls_coefs["sigma"], nls_coefs["asym"], nls_coefs["gamma"]))
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
  starts_main[4],          # pupil_max
  0,                       # New interaction term (stim:state)
  starts_main[5:7]         # sigma, asym, gamma
)

# 3. Run the model with a more robust optimizer
nlme_tvns_pupil_state <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ stim_type * pupil_state + pupil_phasic,
    sigma ~ 1, asym ~ 1, gamma ~ 1
  ),
  random = mu ~ 1 | SID,
  data = shooting_trimmed,
  start = new_starts,
  control = nlmeControl(opt = "nlminb", pnlsMaxIter = 1000, msMaxIter = 1000)
)

summary(nlme_tvns_pupil_state)

anova(shooting.nlme, shooting.no_asym_int_nlme, nlme_tvns_pupil_state)

# try with 3 way interaction


# Step A: Fit a main-effects only model first (this almost always converges)
m_tvns_main <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(mu ~ stim_type + pupil_state + pupil_phasic + vigor_wc, sigma ~ 1, asym ~ 1, gamma ~ 1),
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
  starts_main[4],          # pupil_max
  #starts_main[5],          # vigor_wc
  
  0,                       # New interaction term (stim:state)

  starts_main[6:8]         # sigma, asym, gamma
)

# 3. Run the model with a more robust optimizer
nlme_tvns_pupil_state_int <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ stim_type * pupil_state  + pupil_phasic,
    sigma ~ 1, asym ~ 1, gamma ~ 1
  ),
  random = mu ~ 1 | SID,
  data = shooting_trimmed,
  start = new_starts,
  control = nlmeControl(opt = "nlminb", pnlsMaxIter = 1000, msMaxIter = 1000)
)

summary(nlme_tvns_pupil_state_int)

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