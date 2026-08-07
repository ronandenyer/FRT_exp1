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
# 2B. MAIN EFFECTS OF reward ON MAIN VARIABLES
# -------------------------------------------------------------------------

# change in baseline pupil (mean average 1 second before reward starts)
pupil_state_reward <- lmer(
  peak_tanVel ~ reward_type*rt + 
    (1 + reward_type | SID), 
  data = shooting_trimmed
)

summary(pupil_state_reward)

# change in max pupil during phasic ramp (1.5 second window prior to GO cue)

pupil_max_reward <- lmer(
  pupil_max ~ reward_type + 
    (1 + reward_type | SID), 
  data = shooting_trimmed
)

summary(pupil_max_reward)

# change in reach peak velocity
vigor_reward <- lmer(
  vigor_wc ~ reward_type*pupil_state + 
    (1 + reward_type + pupil_state | SID), 
  data = shooting_trimmed
)

summary(vigor_reward)


# ==============================================================================
# 1. PARAMETERS (Change these to customize your plot)
# ==============================================================================

# Labels for reward_type (1 = No Reward, 2 = Reward)
label_no_reward <- "No Reward"
label_reward    <- "Reward"

# Colors for the violins
color_no_reward <- "black" # Orange
color_reward    <- "red" # Blue

# Axis labels
y_axis_label    <- "Peak Velocity (m/s [mean centred])"
x_axis_label    <- "Condition"
plot_title      <- "Effect of Reward on Vigor"

# ==============================================================================
# 2. DATA PREP & PLOTTING
# ==============================================================================

# Convert the numeric 1 and 2 into a factor for ggplot
plot_data <- shooting_trimmed %>%
  mutate(
    reward_factor = factor(
      reward_type, 
      levels = c(1, 2), 
      labels = c(label_no_reward, label_reward)
    )
  )

# Create the plot
ggplot(plot_data, aes(x = reward_factor, y = vigor_wc, fill = reward_factor)) +
  # The violin plot itself
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  
  # Optional: Adds a small boxplot inside for medians/quartiles
  geom_boxplot(width = 0.15, fill = "white", color = "black", outlier.shape = NA) +
  
  # Apply your custom colors
  scale_fill_manual(values = c(color_no_reward, color_reward)) +
  
  # Apply your custom labels
  labs(
    title = plot_title,
    x = x_axis_label,
    y = y_axis_label
  ) +
  geom_signif(
    comparisons = list(c(label_no_reward, label_reward)),
    annotations = "p < .0005", # Change to match your lmer output (e.g., "*", "**", "p = 0.02")
    y_position = max(plot_data$vigor_wc, na.rm = TRUE) + 0.1, # Adjust the + 0.5 to move the bar up or down
    tip_length = 0.03,
    textsize = 2.5,
    vjust = -0.1
  ) +
  
  # Clean, minimal theme
  theme_minimal() +
  theme(
    legend.position = "none", # Hides the legend since the x-axis already labels the groups
    plot.title      = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title      = element_text(size = 14, face = "bold"),
    axis.text       = element_text(size = 12),
    panel.grid.major.x = element_blank() # Removes vertical grid lines for a cleaner look
  ) + 
  ylim(-0.6, 0.6)

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
  data = shooting,
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
  data = shooting,
  start = list(fixed = c(nls_coefs["mu"], -0.01, nls_coefs["sigma"], nls_coefs["asym"], nls_coefs["gamma"])),
  control = ctrl
)
summary(m2_reward_mu_only)
# Compare Step A Models
cat("\n--- Does Reward affect Asymptote? ---\n")
anova(m1_reward_asym, m2_reward_mu_only)

# Note: The following steps assume m2_reward_mu_only wins (Asym is stable). 
# If m1 wins, simply add `asym ~ reward_type` and adjust the start vectors below.

# --- Step B: Incorporate Vigor (Is Mu effect driven by vigor?) ---
m3_reward_vigor <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ reward_type * vigor_scaled, 
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  random = list(SID = pdDiag(mu  ~ 1)),
  data = shooting,
  # mu starts: Intercept, reward_type, vigor_scaled, reward:vigor
  start = list(fixed = c(nls_coefs["mu"], -0.01, -0.005, 0, nls_coefs["sigma"], nls_coefs["asym"], nls_coefs["gamma"])),
  control = ctrl
)

summary(m3_reward_vigor)
cat("\n--- Does Vigor scale the Reward effect on Mu? ---\n")
anova(m2_reward_mu_only, m3_reward_vigor)


# --- Step C: Add Pupil State (Does arousal modulate the vigor/reward link?) ---
m4_reward_vigor_pupil <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ reward_type * pupil_max_scaled * vigor_scaled ,
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  random = list(SID = pdDiag(mu ~ 1)),
  data = shooting_trimmed,
  # mu starts: Int, rew, vig, pup, rew:vig, rew:pup, vig:pup, rew:vig:pup
  start = list(fixed = c(
    nls_coefs["mu"], -0.01, -0.005, 0, 0, 0, 0, 0,
    nls_coefs["sigma"], nls_coefs["asym"], nls_coefs["gamma"]
  )),
  control = ctrl
)
summary(m4_reward_vigor_pupil)
cat("\n--- Does Pupil State interact with the Reward/Vigor system? ---\n")
anova(m3_reward_vigor, m4_reward_vigor_pupil)
library(dplyr)

# Cap the extreme lever arms at +/- 2.5 SD
shooting_trimmed <- shooting_trimmed %>%
  mutate(
    vigor_winsorized = ifelse(vigor_scaled > 2.5, 2.5, 
                              ifelse(vigor_scaled < -2.5, -2.5, vigor_scaled)),
    
    pupil_max_winsorized = ifelse(pupil_max_scaled > 2.5, 2.5, 
                                  ifelse(pupil_max_scaled < -2.5, -2.5, pupil_max_scaled))
  )

# 1. Run a simpler model first (no interactions)
m_simple <- nlme(success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
                 fixed = list(mu ~ reward_type + pupil_max + pupil_state + vigor_wc, sigma ~ 1, asym ~ 1, gamma ~ 1),
                 random = mu ~ 1 | SID, 
                 start = c(
                   # mu parameters: Intercept + 4 slopes (assuming reward_type has 2 levels)
                   "mu.(Intercept)"    = 0.2,  
                   "mu.reward_type"    = -0.01,  
                   "mu.pupil_max"      = 0,
                   "mu.pupil_state"    = 0,
                   "mu.vigor_wc"       = 0,
                   
                   # other parameters
                   "sigma"             = 0.01,  # Good guess for standard deviation
                   "asym"              = 1,  # Good guess for an upper asymptote
                   "gamma"             = 0.25   # Good guess for a lower asymptote
                 ),
                 data = shooting_trimmed)

# 2. Extract those coefficients to start your complex model
my_starts <- coef(m_simple) # This gets you the intercepts
# Then inject these into your start list for the complex model

# Then run your nlme using the _winsorized variables
m4_corrected <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ reward_type * pupil_max * pupil_state + vigor_wc,
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  random = list(SID = pdDiag(mu ~ reward_type)),
  data = shooting_trimmed,
  # mu starts: Int, rew, vig, pup, rew:vig, rew:pup, vig:pup, rew:vig:pup
  start = list(fixed = c(
    nls_coefs["mu"], -0.01, -0.005, -0.006, 0.004, 0, 0, 0, 0,
    nls_coefs["sigma"], nls_coefs["asym"], nls_coefs["gamma"]
  )),
  control = ctrl
)
summary(m4_corrected)

# Then run your nlme using the _winsorized variables
m4_quadratic <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    # Added the quadratic term here
    mu ~ reward_type * pupil_max_scaled * pupil_state_scaled + I(pupil_state_scaled^2) + vigor_scaled,
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  random = list(SID = pdDiag(mu ~ 1)),
  data = shooting,
  
  # mu starts now requires 10 values. 
  # Order: Int, rew, p_max, p_state, p_state^2 (NEW), vigor, rew:p_max, rew:p_state, p_max:p_state, rew:p_max:p_state
  start = list(fixed = c(
    nls_coefs["mu"], -0.01, -0.005, -0.006, 0, 0.004, 0, 0, 0, 0,
    nls_coefs["sigma"], nls_coefs["asym"], nls_coefs["gamma"]
  )),
  control = ctrl
)
summary(m4_quadratic)
anova(m4_quadratic, m4_corrected)

# 1. Update the control to include a minimum bound for the parameters
# The 'msMinPar' defines the lower bound for the parameters in the order 
# they appear in the fixed effects: c(mu_int, mu_reward, mu_vigor, mu_vigor_effpt, sigma, asym, gamma)
# We set the 5th parameter (sigma) to 0.005.
strat_data = shooting[shooting$eff_pt < 0.4 ,]
strat_check_model <- lmer(
  peak_tanVel ~ eff_pt + (1 + eff_pt | SID), 
  data = strat_data
)

summary(strat_check_model)

pupil_check_model <- lmer(
  vigor_scaled ~ pupil_slope_scaled + (1 + pupil_slope_scaled | SID), 
  data = strat_data
)

summary(pupil_check_model)
plot( shooting$vigor_scaled, shooting$pupil_state_scaled)


# We model the Phasic burst as a function of the Tonic baseline and Reward type.
# The interaction term is the critical test.
autonomic_override_model <- lmer(
  pupil_max_scaled ~ pupil_state_scaled * reward_type + 
    (1 + pupil_state_scaled | SID), 
  data = shooting_trimmed
)

summary(autonomic_override_model)


library(ggplot2)
library(dplyr)

# 1. Aggregate the data to get participant means
# This calculates one mean pupil value per participant, per condition
participant_summary <- shooting_trimmed %>%
  group_by(SID, reward_type) %>%
  summarize(
    participant_mean_pupil = mean(pupil_max_scaled, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Generate the plot
ggplot(participant_summary, aes(x = reward_type, y = participant_mean_pupil)) +
  
  # A. Add the jittered dots representing individual participants
  # width controls how far they spread horizontally; alpha makes overlapping dots visible
  geom_jitter(aes(color = reward_type), width = 0.15, size = 3, alpha = 0.6) +
  
  # B. Add the Crossbar for the Group Mean
  # We use stat_summary to calculate the mean of the participant means on the fly
  stat_summary(
    fun = mean, 
    geom = "crossbar", 
    width = 0.4,           # Width of the crossbar
    linewidth = 1,         # Thickness of the line
    color = "black"        # Keeps the mean line distinct from the colored dots
  ) +
  
  # C. Styling and Colors (matching the previous interaction plot)
  scale_color_manual(values = c("No Reward" = "#3182bd", "High Reward" = "#de2d26")) +
  
  # D. Labels and Clean Formatting
  labs(
    title = "Reward Consistently Elevates Phasic Arousal",
    subtitle = "Dots represent individual participant means; black bar denotes group mean",
    x = "Condition",
    y = "Mean Phasic Burst (Z-scored)"
  ) +
  
  # Clean, publication-ready theme
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none", # Legend is redundant since x-axis is labeled
    panel.grid.major.x = element_blank(), # Removes vertical grid lines for a cleaner look
    plot.title = element_text(face = "bold")
  )


library(ggplot2)
library(dplyr)

# 1. Aggregate the data to get participant standard deviations
# This calculates one standard deviation value per participant, per condition
participant_sd_summary <- shooting_trimmed %>%
  group_by(SID, reward_type) %>%
  summarize(
    participant_sd_pupil = sd(pupil_max, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Generate the plot
ggplot(participant_sd_summary, aes(x = reward_type, y = participant_sd_pupil)) +
  
  # A. Add the jittered dots representing individual participant SDs
  geom_jitter(aes(color = reward_type), width = 0.15, size = 3, alpha = 0.6) +
  
  # B. Add the Crossbar for the Group Mean of the SDs
  stat_summary(
    fun = mean, 
    geom = "crossbar", 
    width = 0.4,           
    linewidth = 1,         
    color = "black"        
  ) +
  
  # C. Styling and Colors (maintaining consistency)
  scale_color_manual(values = c("No Reward" = "#3182bd", "High Reward" = "#de2d26")) +
  
  # D. Labels tailored to variability
  labs(
    title = "Reward Increases Consistency of Autonomic Arousal",
    subtitle = "Dots represent individual participant standard deviations; black bar denotes group mean",
    x = "Condition",
    y = "Standard Deviation of Phasic Burst (Z-scored)"
  ) +
  
  # Clean, publication-ready theme
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none", 
    panel.grid.major.x = element_blank(), 
    plot.title = element_text(face = "bold")
  )

# 1. Extract marginal effects (same as before)
model_preds <- ggpredict(autonomic_override_model, terms = c("pupil_state_scaled", "reward_type"))

# 2. Build the unified plot
ggplot() +
  # Raw data points in the background
  geom_point(data = shooting_trimmed, 
             aes(x = pupil_state_scaled, y = pupil_max_scaled, color = reward_type), 
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
  scale_color_manual(name = "Reward Condition",
                     values = c("1" = "#3182bd", "2" = "#de2d26"), 
                     labels = c("1" = "No Reward", "2" = "Reward")) +
  
  # Added fill manual with identical parameters to beautifully merge the legends
  scale_fill_manual(name = "Reward Condition",
                    values = c("1" = "#3182bd", "2" = "#de2d26"), 
                    labels = c("1" = "No Reward", "2" = "Reward")) +
  
  labs(
    title = "Tonic-Phasic Pupil Dynamics by Reward Condition",
    x = "Baseline Pupil Size (Tonic)",
    y = "Peak Pupil Response (Phasic)"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )