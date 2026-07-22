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
shooting <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')

# 2. Factorize and level conditions
shooting$reward_type <- as.factor(shooting$reward_type)

# 3. Clean NA / NaN values
shooting <- shooting[!is.na(shooting$eff_pt),]
shooting <- shooting[!is.na(shooting$success),]
shooting <- shooting[!is.nan(shooting$pupil_size_phasic_mean),]
#shooting <- shooting[shooting$eff_pt> 0,]

# 4. Standardize units 
shooting$pupil_size_phasic_mean_norm <- shooting$pupil_size_phasic_mean_norm * 1e4
shooting$pupil_size_phasic_mean <- shooting$pupil_size_phasic_mean * 1e4
shooting$pupil_size_phasic_slope <- shooting$pupil_size_phasic_slope * 1e4
shooting$pupil_size_mean <- shooting$pupil_size_mean * 1e4
shooting$eff_pt <- shooting$eff_pt / 1000 # Convert to seconds

# 5. Extract State (Within-Subject) and Trait (Between-Subject) pupil variables
shooting <- shooting %>%
  group_by(SID) %>%
  mutate(
    # State: Trial-by-trial fluctuation (Mean is exactly 0 for everyone)
    pupil_state = pupil_size_mean - mean(pupil_size_mean, na.rm = TRUE),
    # Trait: The person's overall average baseline arousal
    pupil_trait = mean(pupil_size_mean, na.rm = TRUE)
  ) %>%
  ungroup()


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



# 6. Globally scale variables to prevent optimizer crashes
shooting$pupil_state_scaled <- shooting$pupil_state / sd(shooting$pupil_state, na.rm = TRUE)
shooting$pupil_trait_scaled <- shooting$pupil_trait / sd(shooting$pupil_trait, na.rm = TRUE)

library(dplyr)

# 1. Scale the variance of your already-centered variables
shooting <- shooting %>%
  mutate(
    # Because mean is already 0, dividing by SD creates a true Z-score
    pupil_state_scaled = pupil_state / sd(pupil_state, na.rm = TRUE),
    vigor_scaled = vigor_wc / sd(vigor_wc, na.rm = TRUE)
  )

# 2. Trim the extreme physiological/kinematic artifacts
shooting_trimmed <- shooting %>%
  filter(
    abs(pupil_state_scaled) <= 3,
    abs(vigor_scaled) <= 3
  )

# 3. Verify the drop rate
trials_lost <- nrow(shooting) - nrow(shooting_trimmed)
percent_lost <- (trials_lost / nrow(shooting)) * 100
cat("Dropped", trials_lost, "trials (", round(percent_lost, 2), "% )\n")


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
  fixed = list(mu ~ reward_type, sigma ~ 1, asym ~ reward_type, gamma ~ 1), 
  random = list(SID = pdDiag(mu + sigma ~ 1)),
  data = shooting,
  start = list(fixed = c(0.2, -0.01, 0.02, 0.95, 0.01, 0.25))
)

# MODEL 2: Trimmed Main-Effects Model (Lock asymptote to test if it's needed)
shooting.no_asym_int_nlme <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(mu ~ reward_type, sigma ~ 1, asym ~ 1, gamma ~ 1), 
  random = list(SID = pdDiag(mu + sigma ~ 1)),
  data = shooting,
  start = list(fixed = c(0.2, -0.01, 0.02, 0.95, 0.25))
)

# Test 1: Can we drop the asymptote parameter? (Expecting p > 0.05)
cat("\n--- LRT: Can we drop Asymptote from the model? ---\n")
anova(shooting.nlme, shooting.no_asym_int_nlme)


# ------------------------------------------------------------------------------
# SECTION 3: STATE-DEPENDENT HYPOTHESIS & TRAIT CONTROL
# ------------------------------------------------------------------------------

# MODEL 3: The State-Dependent Model (Primary Finding)
nlme_reward_pupil_state <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ reward_type * pupil_state_scaled,                        # The mechanical link
    sigma ~ 1,   # The LC-NA gain hypothesis
    asym ~ 1, 
    gamma ~ 1
  ), 
  random = list(SID = pdDiag(mu  ~ 1)),                      # Only estimate random intercepts for mu
  data = shooting_trimmed,
  start = list(fixed = c(0.2, -0.01, 0,0, 0.02, 0.95, 0.25)), # Adjust start vector length
  control = nlmeControl(opt = "nlm", pnlsMaxIter = 300, msMaxIter = 300)
)

summary(nlme_reward_pupil_state)
# Test 2: Does the state interaction improve model fit? (Expecting p < 0.05)
cat("\n--- LRT: Does Trial-by-Trial Arousal Gate tVNS? ---\n")
anova(shooting.no_asym_int_nlme, nlme_tvns_pupil_state)


# MODEL 4: Trait vs State Control (Is it individual differences or dynamic state?)
nlme_tvns_trait_and_state2 <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ reward_type * pupil_state_scaled + reward_type * pupil_trait_scaled,   
    sigma ~ 1, asym ~ 1, gamma ~ 1                     
  ), 
  random = list(SID = pdDiag(mu + sigma ~ 1)),
  data = shooting,
  start = list(fixed = c(
    0.199, 0.000, 0.000, 0.000, 0.000, 0.000, # mu params
    0.023, 0.954, 0.254                       # sigma, asym, gamma
  )),
  control = nlmeControl(opt = "nlm", pnlsMaxIter = 100, msMaxIter = 100)
)

cat("\n--- Summary: Trait vs State Interaction ---\n")
summary(nlme_tvns_trait_and_state)


# MODEL 6: Pupil x Vigor model
nlme_tvns_pupil_vigor_state <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ reward_type * pupil_state_scaled * vigor_scaled,   
    sigma ~ 1,                    
    asym ~ 1,
    gamma ~ 1                     
  ), 
  random = mu ~ 1 | SID,
  data = shooting,
  start = list(fixed = c(0.199, 0.000, 0.000, 0, 0, 0, 0, 0.000, 0.023, 0.954, 0.254)),
  control = nlmeControl(opt = "nlm", pnlsMaxIter = 300, msMaxIter = 300)
)

cat("\n--- Summary: Trait vs State Interaction ---\n")
summary(nlme_tvns_pupil_vigor_state)

anova(nlme_tvns_pupil_state, nlme_tvns_pupil_vigor_state)
# ------------------------------------------------------------------------------
# SECTION 4: VISUALIZATIONS
# ------------------------------------------------------------------------------

# --- PLOT 1: Speed-Accuracy Curve (Raw Binned Data vs Model Fit) ---
shooting_binned <- shooting %>%
  mutate(bin = round(eff_pt / 0.02) * 0.02) %>%
  group_by(bin, reward_type) %>%
  summarize(prop_success = mean(success), n = n(), .groups = "drop")

plot_grid <- expand.grid(
  eff_pt = seq(min(shooting$eff_pt), max(shooting$eff_pt), length.out = 200),
  reward_type = factor(c(1, 2))
)
plot_grid$predicted <- predict(shooting.no_asym_int_nlme, newdata = plot_grid, level = 0)

tvns_curve <- ggplot() +
  geom_line(data = plot_grid, aes(x = eff_pt, y = predicted, color = reward_type), linewidth = 2) +
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
intercept_tvns <- coefs["mu.(Intercept)"] + coefs["mu.reward_type2"]
slope_tvns <- coefs["mu.pupil_state_scaled"] + coefs["mu.reward_type2:pupil_state_scaled"]

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
library(nlme)

# 1. Extract the raw coefficients from the model
fe <- fixef(nlme_tvns_pupil_vigor_state)

# 2. Build the prediction grid
pred_grid <- expand.grid(
  vigor_scaled = seq(-2, 2, by = 0.1),
  stim_num = c(0, 1), # 0 = Sham/Reference, 1 = tVNS
  pupil_state_scaled = c(-1, 0, 1) # Low, Mean, High
)

# 3. Manually reconstruct the formula for mu
pred_grid <- pred_grid %>%
  mutate(
    # Create clean labels for plotting
    reward_type = ifelse(stim_num == 1, "tVNS", "Sham"),
    pupil_facet = factor(pupil_state_scaled, 
                         levels = c(-1, 0, 1),
                         labels = c("Low Baseline Arousal (-1 SD)", 
                                    "Average Arousal (Mean)", 
                                    "High Baseline Arousal (+1 SD)")),
    
    # Calculate predicted mu exactly as the model formula defines it
    predicted_mu = fe["mu.(Intercept)"] +
      fe["mu.reward_type2"] * stim_num +
      fe["mu.pupil_state_scaled"] * pupil_state_scaled +
      fe["mu.vigor_scaled"] * vigor_scaled +
      fe["mu.reward_type2:pupil_state_scaled"] * stim_num * pupil_state_scaled +
      fe["mu.reward_type2:vigor_scaled"] * stim_num * vigor_scaled +
      fe["mu.pupil_state_scaled:vigor_scaled"] * pupil_state_scaled * vigor_scaled +
      fe["mu.reward_type2:pupil_state_scaled:vigor_scaled"] * stim_num * pupil_state_scaled * vigor_scaled
  )

# 4. Plot the newly calculated mu parameter directly
ggplot(pred_grid, aes(x = vigor_scaled, y = predicted_mu, color = reward_type, linetype = reward_type)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~pupil_facet) +
  scale_color_manual(values = c("Sham" = "#4A4A4A", "tVNS" = "#D55E00")) +
  scale_linetype_manual(values = c("Sham" = "dashed", "tVNS" = "solid")) +
  labs(
    title = "State-Dependent Effect of tVNS on Cognitive-Motor Co-regulation",
    x = "Trial-by-Trial Movement Vigor (Scaled)",
    y = expression(paste("Predicted Decision Threshold (", mu, ") - Lower is Better")),
    color = "Stimulation",
    linetype = "Stimulation"
  ) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  )