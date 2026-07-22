# ==============================================================================
# tVNS & REWARD FORCED RESPONSE TASK - DATA PIPELINE
# ==============================================================================
library(nlme)
library(ggplot2)
library(ggrepel) 
library(dplyr)

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
  fixed = list(mu ~ stim_type + pupil_state + pupil_max, sigma ~ 1, asym ~ 1, gamma ~ 1),
  random = list(SID = pdDiag(mu ~ 1)),
  data = shooting, # Make sure this is trimmed!
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
    mu ~ stim_type * pupil_state + pupil_max,
    sigma ~ 1, asym ~ 1, gamma ~ 1
  ),
  random = mu ~ 1 | SID,
  data = shooting_trimmed,
  start = new_starts,
  control = nlmeControl(opt = "nlminb", pnlsMaxIter = 1000, msMaxIter = 1000)
)

summary(nlme_tvns_pupil_state)

anova(shooting.nlme, shooting.no_asym_int_nlme, nlme_tvns_pupil_state)

# Step A: Fit a main-effects only model first (this almost always converges)
m_tvns_main <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(mu ~ stim_type + pupil_state + pupil_max + vigor_wc, sigma ~ 1, asym ~ 1, gamma ~ 1),
  random = list(SID = pdDiag(mu ~ 1)),
  data = shooting, # Make sure this is trimmed!
  start = list(fixed = c(nls_coefs["mu"], 0, 0, 0, -0.006, nls_coefs["sigma"], nls_coefs["asym"], nls_coefs["gamma"]))
)

# Step B: Extract those exact coefficients to use as starts for the complex model
starts_main <- fixef(m_tvns_main)

# Step C: Run your 3-way interaction, feeding it the known good starts, 
# and adding 0s ONLY for the new interaction terms.
nlme_tvns_pupil_state <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ stim_type  * pupil_max * vigor_wc + pupil_state,
    sigma ~ 1, asym ~ 1, gamma ~ 1
  ),
  random = list(SID = pdDiag(mu ~ stim_type)),
  data = shooting_trimmed,
  # Format: c(Main Effects from previous model, Intercepts for new interactions, sigma, asym, gamma)
  start = list(fixed = c(starts_main[1:5], 0, 0, 0, 0, starts_main[6:8])), 
  control = nlmeControl(opt = "nlminb", pnlsMaxIter = 1000, msMaxIter = 1000)
)
# Test 2: Does the state interaction improve model fit? (Expecting p < 0.05)
summary(nlme_tvns_pupil_state)

pupil_tvns <- lmer(
  pupil_state ~ stim_type + 
    (1 + stim_type | SID), 
  data = shooting_trimmed
)

summary(pupil_tvns)

cat("\n--- LRT: Does Trial-by-Trial Arousal Gate tVNS? ---\n")
anova(shooting.no_asym_int_nlme, nlme_tvns_pupil_state)

# MODEL 6: Pupil x Vigor model
nlme_tvns_pupil_vigor_state <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ stim_type * pupil_max_scaled * vigor_scaled,   
    sigma ~ 1,                    
    asym ~ 1,
    gamma ~ 1                     
  ), 
  random = mu ~ 1 | SID,
  data = shooting_trimmed,
  start = list(fixed = c(0.199, 0.000, 0.000, 0, 0, 0, 0, 0.000, 0.023, 0.954, 0.254)),
  control = nlmeControl(opt = "nlm", pnlsMaxIter = 300, msMaxIter = 300)
)

cat("\n--- Summary: Trait vs State Interaction ---\n")
summary(nlme_tvns_pupil_vigor_state)

anova(nlme_tvns_pupil_state, nlme_tvns_pupil_vigor_state)



# We model the Phasic burst as a function of the Tonic baseline and Reward type.
# The interaction term is the critical test.
autonomic_override_model <- lmer(
  pupil_max_scaled ~ pupil_state_scaled * stim_type + 
    (1 + pupil_state_scaled | SID) + (1 + stim_type | SID), 
  data = shooting_trimmed
)
summary(autonomic_override_model)
# ------------------------------------------------------------------------------
# SECTION 4: VISUALIZATIONS
# ------------------------------------------------------------------------------

# --- PLOT 1: Speed-Accuracy Curve (Raw Binned Data vs Model Fit) ---
shooting_binned <- shooting %>%
  mutate(bin = round(eff_pt / 0.02) * 0.02) %>%
  group_by(bin, stim_type) %>%
  summarize(prop_success = mean(success), n = n(), .groups = "drop")

plot_grid <- expand.grid(
  eff_pt = seq(min(shooting$eff_pt), max(shooting$eff_pt), length.out = 200),
  stim_type = factor(c(1, 2))
)
plot_grid$predicted <- predict(shooting.no_asym_int_nlme, newdata = plot_grid, level = 0)

tvns_curve <- ggplot() +
  geom_line(data = plot_grid, aes(x = eff_pt, y = predicted, color = stim_type), linewidth = 2) +
  scale_color_manual(values = c("1" = "black", "2" = "green"), labels = c("Sham", "tVNS")) +
  scale_size_area(max_size = 8) +
  geom_hline(yintercept = 0.25, linetype = "dashed", alpha = 0.5) + 
  labs(
    x = "Preparation Time (s)", y = "Probability of Success",
    size = "Trial Count", color = "Condition", tag = "A."
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq.int(0, 1, 0.2)) +
  xlim(0, 0.5) +
  theme_classic() +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.position = c(0.85, 0.45),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 20),
    plot.tag = element_text(size = 35, face = "bold")
  )
print(tvns_curve)


# --- PLOT 2: The Pupil State Interaction ---
coefs <- fixef(nlme_tvns_pupil_state)

# Calculate Sham and tVNS slopes based on model coefficients
intercept_sham <- coefs["mu.(Intercept)"]
slope_sham <- coefs["mu.pupil_state_scaled"]
intercept_tvns <- coefs["mu.(Intercept)"] + coefs["mu.stim_type2"]
slope_tvns <- coefs["mu.pupil_state_scaled"] + coefs["mu.stim_type2:pupil_state_scaled"]

pupil_range <- seq(-3, 3, length.out = 100)
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
  ylim(0.180, 0.220) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 18),
    plot.tag = element_text(size = 35, face = "bold")
  )
print(pupil_interaction_plot)
library(ggplot2)
library(dplyr)

# 1. Generate a sequence of baseline pupil states (e.g., -2.5 to +2.5 Standard Deviations)
pupil_range <- seq(-2.5, 2.5, by = 0.1)

# 2. Hard-code the mu coefficients from your nlme model output
intercept <- 0.1973          # mu.(Intercept)
beta_stim <- 0.0047         # mu.stim_type2
beta_pupil <- 0.003775         # mu.pupil_state_scaled
beta_interaction <- -0.006  # mu.stim_type2:pupil_state_scaled

# 3. Build the prediction dataframe
plot_data <- data.frame(
  pupil_state_scaled = rep(pupil_range, 2),
  stim_type = factor(rep(c("Sham", "tVNS"), each = length(pupil_range)), levels = c("Sham", "tVNS"))
)

# 4. Calculate predicted Mu using the model math
# Sham: Intercept + (Pupil Beta * Pupil State)
# tVNS: (Intercept + Stim Beta) + ((Pupil Beta + Interaction Beta) * Pupil State)
plot_data <- plot_data %>%
  mutate(
    predicted_mu = case_when(
      stim_type == "Sham" ~ intercept + (beta_pupil * pupil_state_scaled),
      stim_type == "tVNS" ~ (intercept + beta_stim) + ((beta_pupil + beta_interaction) * pupil_state_scaled)
    )
  )

# 5. Generate the plot
ggplot(plot_data, aes(x = pupil_state_scaled, y = predicted_mu, color = stim_type)) +
  geom_line(linewidth = 1.2) +
  # Using a colorblind-friendly palette that clearly separates the conditions
  scale_color_manual(values = c("Sham" = "#E69F00", "tVNS" = "#56B4E9")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "State-Dependent Neuromodulation by tVNS",
    subtitle = "tVNS neutralizes the cognitive penalty of high baseline arousal",
    x = "Tonic Baseline Pupil (Z-Score)",
    y = expression(paste("Predicted Decision Threshold (", mu, ")")),
    color = "Condition"
  ) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  # Optional: Add a vertical line at 0 to denote the participant's average baseline
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5)



# 1. Extract marginal effects (same as before)
model_preds <- ggpredict(autonomic_override_model, terms = c("pupil_state_scaled", "stim_type"))

# 2. Build the unified plot
ggplot() +
  # Raw data points in the background
  geom_point(data = shooting_trimmed, 
             aes(x = pupil_state_scaled, y = pupil_max_scaled, color = stim_type), 
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