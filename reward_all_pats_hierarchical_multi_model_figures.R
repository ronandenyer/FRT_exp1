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
  pupil_state ~ reward_type + 
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
  vigor_wc ~ reward_type + 
    (1 + reward_type | SID), 
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
color_no_reward <- "gray10" # Orange
color_reward    <- "#ff5050" # Blue

# Axis labels
y_axis_label    <- "Peak Velocity (m/s [mean centred])"
x_axis_label    <- "Condition"
plot_title      <- "Effect of Reward on Movement Speed"

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
  
  stat_summary(
    fun.data = "mean_se", 
    geom = "crossbar", 
    width = 0.15, 
    color = "black"
  ) +
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
    textsize = 4,
    vjust = -0.1
  ) +
  
  # Clean, minimal theme
  theme_bw() +
  theme(
    legend.position = "none", # Hides the legend since the x-axis already labels the groups
    plot.title      = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title      = element_text(size = 14, face = "bold"),
    axis.text       = element_text(size = 12),
    #panel.grid.major.x = element_blank() # Removes vertical grid lines for a cleaner look
  ) + 
  ylim(-0.6, 0.6)

# pupil state

# Axis labels
y_axis_label    <- "Pupil Size (a.u., [mean centred])"
x_axis_label    <- "Condition"
plot_title      <- "Pre Cue Pupil Size\n"

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
ggplot(plot_data, aes(x = reward_factor, y = pupil_state, fill = reward_factor)) +
  # The violin plot itself
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  
  stat_summary(
    fun.data = "mean_se", 
    geom = "crossbar", 
    width = 0.15, 
    color = "black"
  ) +
  # Apply your custom colors
  scale_fill_manual(values = c(color_no_reward, color_reward)) +
  
  # Apply your custom labels
  labs(
    title = plot_title,
    x = x_axis_label,
    y = y_axis_label
  ) +
  
  
  # Clean, minimal theme
  theme_bw() +
  theme(
    legend.position = "none", # Hides the legend since the x-axis already labels the groups
    plot.title      = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title      = element_text(size = 14, face = "bold"),
    axis.text       = element_text(size = 12),
    #panel.grid.major.x = element_blank() # Removes vertical grid lines for a cleaner look
  ) + 
  ylim(-0.03, 0.03)

# pupil state

# Axis labels
y_axis_label    <- "Pupil Size (a.u., [mean centred])"
x_axis_label    <- "Condition"
plot_title      <- "Effect of Reward on\nMax Phasic Pupil Size"

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
ggplot(plot_data, aes(x = reward_factor, y = pupil_max, fill = reward_factor)) +
  # The violin plot itself
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  
  stat_summary(
    fun.data = "mean_se", 
    geom = "crossbar", 
    width = 0.15, 
    color = "black"
  ) +
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
    annotations = "p < .001", # Change to match your lmer output (e.g., "*", "**", "p = 0.02")
    y_position = max(plot_data$pupil_max, na.rm = TRUE) + 0.005, # Adjust the + 0.5 to move the bar up or down
    tip_length = 0.03,
    textsize = 4,
    vjust = -0.1
  ) +
  # Clean, minimal theme
  theme_bw() +
  theme(
    legend.position = "none", # Hides the legend since the x-axis already labels the groups
    plot.title      = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title      = element_text(size = 14, face = "bold"),
    axis.text       = element_text(size = 12),
    #panel.grid.major.x = element_blank() # Removes vertical grid lines for a cleaner look
  ) + 
  ylim(-0.03, 0.03)

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
    mu ~ reward_type * vigor_wc, 
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  random = list(SID = pdDiag(mu ~ reward_type)),
  data = shooting_trimmed,
  # mu starts: Intercept, reward_type, vigor_scaled, reward:vigor
  start = list(fixed = c(nls_coefs["mu"], -0.01, -0.005, 0, nls_coefs["sigma"], nls_coefs["asym"], nls_coefs["gamma"])),
  control = ctrl
)

summary(m3_reward_vigor)

# 1. Define the range of vigor_wc to plot
# (Adjust the -1 and 1 based on the actual min/max of shooting$vigor_wc)
vigor_seq <- seq(-0.3, 0.3, length.out = 100)

# 2. Extract the fixed effects for mu (baseline intercept and vigor slope)
coefs <- fixef(m3_reward_vigor)[c("mu.(Intercept)", "mu.vigor_wc")]

# 3. Extract the Variance-Covariance matrix for just those two parameters
# This is required to calculate the correct confidence intervals
vcov_mat <- vcov(m3_reward_vigor)[c("mu.(Intercept)", "mu.vigor_wc"), 
                                  c("mu.(Intercept)", "mu.vigor_wc")]

# 4. Build the design matrix
X <- cbind(1, vigor_seq)

# 5. Calculate predicted mu values (the regression line)
pred_mu <- X %*% coefs

# 6. Calculate the standard error for each prediction using matrix multiplication
se_mu <- sqrt(diag(X %*% vcov_mat %*% t(X)))

# 7. Create a dataframe for plotting
plot_df <- data.frame(
  vigor_wc = vigor_seq,
  mu_pred = as.vector(pred_mu),
  ci_lower = as.vector(pred_mu - 1.96 * se_mu),
  ci_upper = as.vector(pred_mu + 1.96 * se_mu)
)

# 8. Generate the plot
ggplot(plot_df, aes(x = vigor_wc, y = mu_pred)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "dodgerblue", alpha = 0.3) +
  geom_line(color = "dodgerblue4", linewidth = 1.5) +
  theme_bw(base_size = 15) +
  labs(
    title = "Effect of Movement Speed on Preparation Time",
    x = "Peak Velocty (m/s [mean centred])",
    y = expression("Estimated Preparation Time (" * mu * ")")
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

# Now do the same but with added lines howing reward effect

# 1. Define the average vigor values from the lmer model
vigor_no_reward <- -0.018370
vigor_reward <- 0.0169

# 2. Calculate the corresponding predicted mu values 
# Using the nlme coefficients: Intercept = 0.2015183, Slope = -0.0448395
mu_no_reward <- 0.2015183 + (-0.0448395 * vigor_no_reward)
mu_reward <- 0.2015183 + (-0.0448395 * vigor_reward)

# 8. Generate the plot
ggplot(plot_df, aes(x = vigor_wc, y = mu_pred)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "dodgerblue", alpha = 0.3) +
  geom_line(color = "dodgerblue4", linewidth = 1.5) +
  
  # --- NEW LAYERS: NO REWARD ---
  # Vertical line up from x-axis
  geom_segment(aes(x = vigor_no_reward, xend = vigor_no_reward, y = -Inf, yend = mu_no_reward), 
               linetype = "dashed", color = color_no_reward, linewidth = 1) +
  # Horizontal line across to y-axis
  geom_segment(aes(x = -Inf, xend = vigor_no_reward, y = mu_no_reward, yend = mu_no_reward), 
               linetype = "dashed", color = color_no_reward, linewidth = 1) +
  # Intersection point
  geom_point(aes(x = vigor_no_reward, y = mu_no_reward), color = color_no_reward, size = 4) +
  
  # --- NEW LAYERS: REWARD ---
  # Vertical line up from x-axis
  geom_segment(aes(x = vigor_reward, xend = vigor_reward, y = -Inf, yend = mu_reward), 
               linetype = "dashed", color = color_reward, linewidth = 1) +
  # Horizontal line across to y-axis
  geom_segment(aes(x = -Inf, xend = vigor_reward, y = mu_reward, yend = mu_reward), 
               linetype = "dashed", color = color_reward, linewidth = 1) +
  # Intersection point
  geom_point(aes(x = vigor_reward, y = mu_reward), color = color_reward, size = 4) +
  
  # --- ANNOTATIONS ---
  annotate("text", x = vigor_no_reward - 0.005, y = mu_no_reward + 0.001, 
           label = "No Reward", color = color_no_reward, fontface = "bold", hjust = 1) +
  annotate("text", x = vigor_reward + 0.005, y = mu_reward - 0.001, 
           label = "Reward", color = color_reward, fontface = "bold", hjust = 0) +
  
  theme_bw(base_size = 15) +
  labs(
    title = "Effect of Movement Speed on Preparation Time",
    x = "Peak Velocty (m/s [mean centred])",
    y = expression("Estimated Preparation Time (" * mu * ")")
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

cat("\n--- Does Vigor scale the Reward effect on Mu? ---\n")
anova(m2_reward_mu_only, m3_reward_vigor)


# --- Step C: Add Pupil State (Does arousal modulate the vigor/reward link?) ---
m4_reward_vigor_pupil <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ reward_type * pupil_max_scaled + vigor_scaled ,
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
                 fixed = list(mu ~ reward_type + pupil_state + pupil_phasic + vigor_wc, sigma ~ 1, asym ~ 1, gamma ~ 1),
                 random = mu ~ 1 | SID, 
                 start = c(
                   # mu parameters: Intercept + 4 slopes (assuming reward_type has 2 levels)
                   "mu.(Intercept)"    = 0.2,  
                   "mu.reward_type"    = -0.01,  
                   "mu.pupil_state"      = 0,
                   "mu.pupil_max"    = 0,
                   "mu.vigor_wc"       = 0,
                   
                   # other parameters
                   "sigma"             = 0.01,  # Good guess for standard deviation
                   "asym"              = 1,  # Good guess for an upper asymptote
                   "gamma"             = 0.25   # Good guess for a lower asymptote
                 ),
                 data = shooting_trimmed)

# 2. Extract those coefficients to start your complex model
my_starts <- fixef(m_simple) # This gets you the intercepts
# Then inject these into your start list for the complex model

# Then run your nlme using the _winsorized variables
m4_corrected <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  fixed = list(
    mu ~ reward_type * pupil_state + pupil_phasic + vigor_wc,
    sigma ~ 1, 
    asym ~ 1, 
    gamma ~ 1
  ),
  random = list(SID = pdDiag(mu ~ 1)),
  data = shooting_trimmed,
  # mu starts: Int, rew, vig, pup, rew:vig, rew:pup, vig:pup, rew:vig:pup
  start = list(fixed = c(
    my_starts[1:5], 0,
    my_starts[6:8]
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



# 1. Define the range of pupil_max to plot
p_max_seq <- seq(-0.015, 0.015, length.out = 100)

# 2. Extract the relevant fixed effects
target_params <- c("mu.(Intercept)", "mu.reward_type2", "mu.pupil_max", "mu.reward_type2:pupil_max")
coefs <- fixef(m4_corrected)[target_params]

# 3. Extract the Variance-Covariance matrix
vcov_mat <- vcov(m4_corrected)[target_params, target_params]

# 4. Build the Marginal Design Matrix
# Averages the Reward and No Reward conditions (0.5)
X_marginal <- cbind(1, 0.5, p_max_seq, 0.5 * p_max_seq)

# 5. Calculate predicted mu and Standard Errors for the marginal line
pred_marginal <- X_marginal %*% coefs
se_marginal <- sqrt(diag(X_marginal %*% vcov_mat %*% t(X_marginal)))

# 6. Define the empirical means from your lmer model
pmax_no_reward <- -0.0007554
pmax_reward <- -0.0007554 + 0.0006746 # Equals -0.0000808

# 7. Calculate exact predicted mu for these means ON THE MARGINAL LINE
marginal_intercept <- coefs["mu.(Intercept)"] + (0.5 * coefs["mu.reward_type2"])
marginal_slope <- coefs["mu.pupil_max"] + (0.5 * coefs["mu.reward_type2:pupil_max"])

mu_no_reward_marginal <- marginal_intercept + (marginal_slope * pmax_no_reward)
mu_reward_marginal <- marginal_intercept + (marginal_slope * pmax_reward)

# 8. Create the plotting dataframe
plot_df <- data.frame(
  pupil_max = p_max_seq,
  mu_pred = as.vector(pred_marginal),
  ci_lower = as.vector(pred_marginal - 1.96 * se_marginal),
  ci_upper = as.vector(pred_marginal + 1.96 * se_marginal)
)

# 9. Generate the single-line plot with custom colored drop-lines
ggplot(plot_df, aes(x = pupil_max, y = mu_pred)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "purple", alpha = 0.2) +
  geom_line(color = "purple4", linewidth = 1.5) +
  
  # --- DROP-LINES: NO REWARD ---
  geom_segment(aes(x = pmax_no_reward, xend = pmax_no_reward, y = -Inf, yend = mu_no_reward_marginal), 
               linetype = "dashed", color = color_no_reward, linewidth = 1) +
  geom_segment(aes(x = -Inf, xend = pmax_no_reward, y = mu_no_reward_marginal, yend = mu_no_reward_marginal), 
               linetype = "dashed", color = color_no_reward, linewidth = 1) +
  geom_point(aes(x = pmax_no_reward, y = mu_no_reward_marginal), color = color_no_reward, size = 4) +
  
  # --- DROP-LINES: REWARD ---
  geom_segment(aes(x = pmax_reward, xend = pmax_reward, y = -Inf, yend = mu_reward_marginal), 
               linetype = "dashed", color = color_reward, linewidth = 1) +
  geom_segment(aes(x = -Inf, xend = pmax_reward, y = mu_reward_marginal, yend = mu_reward_marginal), 
               linetype = "dashed", color = color_reward, linewidth = 1) +
  geom_point(aes(x = pmax_reward, y = mu_reward_marginal), color = color_reward, size = 4) +
  
  # --- ANNOTATIONS ---
  annotate("text", x = pmax_no_reward - 0.001, y = mu_no_reward_marginal + 0.002, 
           label = "No Reward Mean", color = color_no_reward, fontface = "bold", hjust = 1) +
  annotate("text", x = pmax_reward + 0.001, y = mu_reward_marginal - 0.001, 
           label = "Reward Mean", color = color_reward, fontface = "bold", hjust = 0) +
  
  theme_bw(base_size = 15) +
  labs(
    title = "Effect of Phasic Arousal on Preparation Time",
    x = "Phasic Pupil Response (a.u., [mean-centered])",
    y = expression("Estimated Preparation Time (" * mu * ")")
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

# 1. Update the control to include a minimum bound for the parameters
# The 'msMinPar' defines the lower bound for the parameters in the order 
# they appear in the fixed effects: c(mu_int, mu_reward, mu_vigor, mu_vigor_effpt, sigma, asym, gamma)
# We set the 5th parameter (sigma) to 0.005.
strat_data = shooting[shooting$eff_pt < 0.4 ,]
strat_check_model <- lmer(
  peak_tanVel ~ eff_pt + (1 + eff_pt | SID), 
  data = shooting_trimmed
)

summary(strat_check_model)

pupil_check_model <- lmer(
  vigor_wc ~ pupil_max + (1  | SID), 
  data = shooting_trimmed
)

summary(pupil_check_model)
# Load required librar

# Generate the plot directly from your trimmed dataframe
ggplot(shooting_trimmed, aes(x = pupil_max, y = vigor_wc)) +
  
  # Add raw data points with high transparency to prevent the "black blob" effect
  # The points will naturally cluster and darken around the center
  geom_point(alpha = 0.05, color = "gray30", size = 1.5) +
  
  # Add the linear regression line 
  # Because random effects variance was 0, this exactly matches your lmer fixed effect
  geom_smooth(method = "lm", color = "black", fill = "dodgerblue", 
              linewidth = 1.5, alpha = 0.3) +
  
  # Optional: Add a dashed horizontal line at y = 0 to anchor the visual center of vigor
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  theme_bw(base_size = 15) +
  labs(
    title = "Phasic Arousal and Movement Speed are Uncorrelated",
    x = "Phasic Pupil Response (a.u., [mean centered])",
    y = "Movement Vigor (m/s, [mean centered])"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold")
  )