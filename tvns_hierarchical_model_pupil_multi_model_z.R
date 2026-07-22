# ==============================================================================
# tVNS & REWARD FORCED RESPONSE TASK - DATA PIPELINE (WITHIN-SUBJECT Z-SCORE)
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

# 3. Clean NA / NaN values
shooting <- shooting[!is.na(shooting$eff_pt),]
shooting <- shooting[!is.na(shooting$success),]
shooting <- shooting[!is.nan(shooting$pupil_size_phasic_mean),]

# 4. Standardize units 
shooting$pupil_size_phasic_mean_norm <- shooting$pupil_size_phasic_mean_norm * 1e4
shooting$pupil_size_phasic_mean <- shooting$pupil_size_phasic_mean * 1e4
shooting$pupil_size_phasic_slope <- shooting$pupil_size_phasic_slope * 1e4
shooting$pupil_size_mean <- shooting$pupil_size_mean * 1e4
shooting$eff_pt <- shooting$eff_pt / 1000 # Convert to seconds

# 5. Extract State (Within-Subject Z-Score) and Trait (Between-Subject) pupil variables
shooting <- shooting %>%
  group_by(SID) %>%
  mutate(
    # State: Trial-by-trial fluctuation
    pupil_state_raw = pupil_size_mean - mean(pupil_size_mean, na.rm = TRUE),
    # Within-Subject Z-Score: Divides by individual's standard deviation
    pupil_state = pupil_state_raw / sd(pupil_state_raw, na.rm = TRUE),
    
    # Trait: The person's overall average baseline arousal
    pupil_trait = mean(pupil_size_mean, na.rm = TRUE)
  ) %>%
  ungroup()

# 6. Globally scale the trait variable to prevent optimizer crashes 
# (State is already scaled within-subject above)
shooting$pupil_trait <- shooting$pupil_trait / sd(shooting$pupil_trait, na.rm = TRUE)

# 7. Define safe probability wrapper for the optimizer
safe_pnorm <- function(x, mu, sigma) {
  safe_sigma <- pmax(sigma, 1e-4) # Prevents NaNs during optimization jumps
  pnorm(x, mean = mu, sd = safe_sigma)
}


# ------------------------------------------------------------------------------
# SECTION 2: STEP-WISE MODEL SELECTION (MAIN EFFECTS)
# ------------------------------------------------------------------------------

# MODEL 1: Full Main-Effects Model (Does tVNS alter threshold OR asymptote?)
shooting.nlme <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(mu ~ stim_type, sigma ~ 1, asym ~ stim_type, gamma ~ 1), 
  random = mu ~ 1 | SID,
  data = shooting,
  start = list(fixed = c(0.2, -0.01, 0.02, 0.95, 0.01, 0.25))
)

# MODEL 2: Trimmed Main-Effects Model (Lock asymptote to test if it's needed)
shooting.no_asym_int_nlme <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(mu ~ stim_type, sigma ~ 1, asym ~ 1, gamma ~ 1), 
  random = mu ~ 1 | SID,
  data = shooting,
  start = list(fixed = c(0.2, -0.01, 0.02, 0.95, 0.25))
)

# Test 1: Can we drop the asymptote parameter? (Expecting p > 0.05)
cat("\n--- LRT: Can we drop Asymptote from the model? ---\n")
anova(shooting.nlme, shooting.no_asym_int_nlme)


# ------------------------------------------------------------------------------
# SECTION 3: STATE-DEPENDENT HYPOTHESIS & TRAIT CONTROL (Z-SCORED)
# ------------------------------------------------------------------------------

# MODEL 3: The State-Dependent Model (Primary Finding using Z-scores)
nlme_tvns_pupil_state <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ stim_type * pupil_state,   
    sigma ~ 1,                    
    asym ~ 1,
    gamma ~ 1                     
  ), 
  random = mu ~ 1 | SID,
  data = shooting,
  start = list(fixed = c(0.199, 0.000, 0.000, 0.000, 0.023, 0.954, 0.254)),
  control = nlmeControl(opt = "nlm", pnlsMaxIter = 100, msMaxIter = 100)
)

# Test 2: Does the state interaction improve model fit? (Expecting p < 0.05)
cat("\n--- LRT: Does Trial-by-Trial Arousal Gate tVNS? ---\n")
anova(shooting.no_asym_int_nlme, nlme_tvns_pupil_state)
summary(nlme_tvns_pupil_state)

# MODEL 4: Trait vs State Control
nlme_tvns_trait_and_state <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ stim_type * pupil_state + stim_type * pupil_trait,   
    sigma ~ 1, asym ~ 1, gamma ~ 1                     
  ), 
  random = mu ~ 1 | SID,
  data = shooting,
  start = list(fixed = c(
    0.199, 0.000, 0.000, 0.000, 0.000, 0.000, # mu params
    0.023, 0.954, 0.254                       # sigma, asym, gamma
  )),
  control = nlmeControl(opt = "nlm", pnlsMaxIter = 100, msMaxIter = 100)
)

cat("\n--- Summary: Trait vs State Interaction (Z-SCORES) ---\n")
summary(nlme_tvns_trait_and_state)


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


# --- PLOT 2: The Pupil State Interaction (Using dynamically calculated limits) ---
coefs <- fixef(nlme_tvns_pupil_state)

# Calculate Sham and tVNS slopes based on model coefficients
intercept_sham <- coefs["mu.(Intercept)"]
slope_sham <- coefs["mu.pupil_state"]
intercept_tvns <- coefs["mu.(Intercept)"] + coefs["mu.stim_type2"]
slope_tvns <- coefs["mu.pupil_state"] + coefs["mu.stim_type2:pupil_state"]

# Standardized Z-Score range for visualization (typically -3 to 3)
pupil_range <- seq(-3, 3, length.out = 100)

df_plot <- data.frame(
  pupil_state = rep(pupil_range, 2),
  Condition = rep(c("Sham", "Active tVNS"), each = 100),
  predicted_mu = c(
    intercept_sham + slope_sham * pupil_range,
    intercept_tvns + slope_tvns * pupil_range
  )
)

pupil_interaction_plot <- ggplot(df_plot, aes(x = pupil_state, y = predicted_mu, color = Condition)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("Sham" = "#4A5568", "Active tVNS" = "#DD6B20")) +
  labs(
    title = "tVNS Decouples Decision Threshold from Arousal State",
    subtitle = "Higher baseline pupil drives urgency in Sham, but is overridden by tVNS",
    x = "Trial-by-Trial Phasic Pupil Size (Within-Subject Z-Score)",
    y = expression("Predicted Decision Threshold (" * mu * ")"),
    tag = "B."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 18),
    plot.tag = element_text(size = 35, face = "bold")
  )
print(pupil_interaction_plot)