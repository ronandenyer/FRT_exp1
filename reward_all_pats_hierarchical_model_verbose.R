# -------------------------------------------------------------------------
# Nonlinear mixed-effects model for shooting success
# -------------------------------------------------------------------------
#
# This script fits a nonlinear mixed-effects model using the nlme package.
#
# The outcome variable is:
#   success
#
# The main predictor is:
#   eff_pt
#
# The model assumes that success changes with eff_pt following a cumulative
# normal curve. This is useful when performance gradually increases from a
# lower limit, called the "guess rate" or "chance level", toward an upper
# limit, called the asymptote.
#
# The model has four key nonlinear parameters:
#
#   mu
#     The location of the curve.
#     Roughly, this is the value of eff_pt where performance is halfway
#     between chance level and the asymptote.
#
#   sigma
#     The spread or slope-related parameter of the curve.
#     Smaller values of sigma make the curve steeper.
#     Larger values make the transition more gradual.
#
#   asym
#     The upper asymptote.
#     This is the maximum predicted success rate as eff_pt becomes large.
#
#   gamma
#     The lower asymptote, also called the chance level or guess rate.
#     If there are 8 targets and participants are guessing randomly,
#     chance performance would be 1/8 = 0.125.
#
# In this version of the model, gamma is estimated from the data rather than
# fixed at 1/8 or 1/4.
#
# The model also includes random effects for participants, meaning that each
# participant is allowed to have their own deviation from the average mu and
# sigma values.
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Load the data
# -------------------------------------------------------------------------

shooting = read.csv(
  'C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv'
)

# Convert reward_type to a factor.
#
# This is important because we want reward_type to be treated as a categorical
# variable rather than a continuous numeric variable.
#
# In the model below, reward_type is used to estimate different parameter
# values for different reward conditions.
shooting$reward_type = as.factor(shooting$reward_type)

# Optional line, currently commented out:
#
# If success is coded as 0/1, it can often be left numeric because the model
# predicts a probability-like value between gamma and asym.
#
# If success were converted to a factor, nlme would not treat it as a numeric
# response, which could cause problems for this model.
#
# shooting$success = as.factor(shooting$success)


# -------------------------------------------------------------------------
# Clean and rescale the predictor variable
# -------------------------------------------------------------------------

# Remove rows where eff_pt is NaN.
#
# NaN means "Not a Number". These values can appear after invalid calculations,
# such as 0/0.
#
# The nlme model cannot estimate parameters properly if the predictor contains
# NaN values, so these rows are removed.
shooting = shooting[!is.nan(shooting$eff_pt), ]

# Rescale eff_pt by dividing by 1000.
#
# This changes the units of eff_pt. For example, if eff_pt was originally in
# milliseconds, this converts it to seconds.
#
# Rescaling can also help model fitting, because nonlinear models often behave
# better when predictors are on a reasonable numerical scale.
shooting$eff_pt = shooting$eff_pt / 1000


# The KINARM seems to have a delay of 0.02 seconds between when commands are 
# output to when they are logged in data, so we subtract this from values
#
shooting$eff_pt = shooting$eff_pt - 0.02



# -------------------------------------------------------------------------
# Fit the nonlinear mixed-effects model
# -------------------------------------------------------------------------

shooting.nlme <- nlme(
  
  # -----------------------------------------------------------------------
  # Nonlinear model formula
  # -----------------------------------------------------------------------
  #
  # The model predicts success using a cumulative normal function:
  #
  #   success = gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)
  #
  # pnorm(eff_pt, mu, sigma) gives the cumulative probability from a normal
  # distribution with:
  #
  #   mean = mu
  #   standard deviation = sigma
  #
  # The value returned by pnorm ranges from 0 to 1.
  #
  # Multiplying by (asym - gamma) stretches this 0-to-1 curve so that it spans
  # the range between the lower and upper performance limits.
  #
  # Adding gamma shifts the lower bound upward.
  #
  # Therefore:
  #
  #   When eff_pt is very small:
  #     pnorm(...) is close to 0
  #     predicted success is close to gamma
  #
  #   When eff_pt is very large:
  #     pnorm(...) is close to 1
  #     predicted success is close to asym
  #
  # This means gamma is the lower asymptote and asym is the upper asymptote.
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  
  # -----------------------------------------------------------------------
  # Fixed effects
  # -----------------------------------------------------------------------
  #
  # Fixed effects describe the average pattern across the whole sample.
  #
  # Here, mu, sigma, and asym are allowed to differ by reward_type.
  #
  # For example:
  #
  #   mu ~ reward_type
  #
  # means that the model estimates:
  #
  #   1. a baseline mu value for the reference reward_type level
  #   2. a difference in mu for the other reward_type level
  #
  # If reward_type has two levels, this gives two fixed-effect coefficients
  # for mu: an intercept and a reward-type difference.
  #
  # The same logic applies to sigma and asym.
  #
  # gamma ~ 1 means gamma is estimated as one common value across reward
  # types. This is simpler and usually more stable than allowing gamma to
  # vary by reward_type.
  #
  # If you wanted gamma to differ between reward conditions, you could use:
  #
  #   gamma ~ reward_type
  #
  # However, this would add more parameters and may make the model harder to
  # fit reliably.
  fixed = list(
    mu    ~ reward_type,
    sigma ~ reward_type,
    asym  ~ reward_type,
    gamma ~ 1
  ),
  
  
  # -----------------------------------------------------------------------
  # Random effects
  # -----------------------------------------------------------------------
  #
  # Random effects allow individual participants to vary around the group
  # average.
  #
  # Here:
  #
  #   random = mu + sigma ~ 1 | SID
  #
  # means that each participant, identified by SID, is allowed to have their
  # own random deviation in:
  #
  #   mu
  #   sigma
  #
  # In simpler terms, participants can differ in where their curve is located
  # and how steep or spread out their curve is.
  #
  # The model does not include random effects for asym or gamma here. That is
  # often a practical choice, because nonlinear mixed models can become
  # unstable if too many random effects are included.
  random = mu + sigma ~ 1 | SID,
  
  
  # -----------------------------------------------------------------------
  # Dataset
  # -----------------------------------------------------------------------
  #
  # This tells nlme which dataframe contains the variables used in the model.
  data = shooting,
  
  
  # -----------------------------------------------------------------------
  # Starting values
  # -----------------------------------------------------------------------
  #
  # Nonlinear models require starting values for the parameters.
  #
  # These are initial guesses used by the optimizer. The optimizer then adjusts
  # them repeatedly to find the values that best fit the data.
  #
  # Good starting values are important. Poor starting values can cause the
  # model to fail, converge slowly, or converge to an unrealistic solution.
  #
  # Because the fixed-effects structure is:
  #
  #   mu    ~ reward_type
  #   sigma ~ reward_type
  #   asym  ~ reward_type
  #   gamma ~ 1
  #
  # and assuming reward_type has two levels, we need:
  #
  #   2 starting values for mu:
  #     intercept and reward_type difference
  #
  #   2 starting values for sigma:
  #     intercept and reward_type difference
  #
  #   2 starting values for asym:
  #     intercept and reward_type difference
  #
  #   1 starting value for gamma:
  #     common guess-rate parameter
  #
  # Total = 7 starting values.
  #
  # The order of values must match the order of parameters in the fixed list.
  start = list(
    fixed = c(
      0.2,  -0.01,  # mu: intercept, reward_type difference
      0.02,  0.007, # sigma: intercept, reward_type difference
      0.95,  0.01,  # asym: intercept, reward_type difference
      0.25          # gamma: common lower asymptote / guess rate
    )
  ),
  
  
  # -----------------------------------------------------------------------
  # Model-fitting controls
  # -----------------------------------------------------------------------
  #
  # Nonlinear mixed-effects models can be difficult to fit, especially when
  # the model has several parameters or random effects.
  #
  # These settings allow the optimizer to try for longer before giving up.
  control = nlmeControl(
    
    # Maximum number of iterations for the main optimization step.
    msMaxIter = 200,
    
    # Maximum number of iterations for the penalized nonlinear least-squares
    # step used internally by nlme.
    pnlsMaxIter = 200,
    
    # If TRUE, prints progress information while the model is fitting.
    # This is useful for diagnosing convergence problems because you can see
    # whether the optimizer is still improving the fit.
    msVerbose = TRUE
  )
)


# -------------------------------------------------------------------------
# Inspect the fitted model
# -------------------------------------------------------------------------

# Print a detailed summary of the fitted model.
#
# This includes:
#
#   - fixed-effect estimates
#   - random-effect estimates
#   - residual standard error
#   - approximate tests for fixed effects
#   - correlations among parameter estimates
#
# For this model, the fixed effects tell you whether mu, sigma, or asym differ
# between reward types.
summary(shooting.nlme)


# -------------------------------------------------------------------------
# Test fixed effects
# -------------------------------------------------------------------------

# Run an ANOVA-style summary for the fitted nlme model.
#
# This provides approximate tests for the fixed effects.
#
# In this case, it can help assess whether reward_type significantly affects
# parameters such as mu, sigma, or asym.
anova(shooting.nlme)

# -------------------------------------------------------------------------
# Visualize SAT Curve for Reward
# -------------------------------------------------------------------------


# 1. Create binned data for the raw points (e.g., every 40ms)
# This lets us see the actual "empirical" psychometric curve
shooting_binned <- shooting %>%
  mutate(bin = round(eff_pt / 0.02) * 0.02) %>%
  group_by(bin, reward_type) %>%
  summarize(
    prop_success = mean(success),
    n = n(),
    .groups = "drop"
  )


# 2. Generate smooth prediction lines
# Create a grid for the population average (Level 0)
plot_grid <- expand.grid(
  eff_pt = seq(min(shooting$eff_pt), max(shooting$eff_pt), length.out = 500),
  reward_type = factor(c(1, 2))
)
plot_grid$predicted <- predict(shooting.nlme, newdata = plot_grid, level = 0)

# NEW: Create a grid for individual participants (Level 1)
# Replace 'SID' with whatever your grouping variable is named in your model/data
subj_ids <- unique(shooting$SID) 
indiv_grid <- expand.grid(
  eff_pt = seq(min(shooting$eff_pt), max(shooting$eff_pt), length.out = 500), # Fewer points to save memory
  reward_type = factor(c(1, 2)),
  SID = subj_ids
)
indiv_grid$predicted_indiv <- predict(shooting.nlme, newdata = indiv_grid, level = 1)

# 3. Build the plot
ggplot() +
  # A. Individual Participant Lines (Faint)
  geom_line(data = indiv_grid, 
            aes(x = eff_pt, y = predicted_indiv, color = reward_type, group = interaction(SID, reward_type)), 
            alpha = 0.15, linewidth = 0.4) + # Low alpha makes them "faint"
  
  # B. Raw binned data points
  geom_point(data = shooting_binned, 
             aes(x = bin, y = prop_success, color = reward_type, size = n), 
             alpha = 0.3) +
  
  # C. Main Model-fitted lines (Thick)
  geom_line(data = plot_grid, 
            aes(x = eff_pt, y = predicted, color = reward_type), 
            linewidth = 2) +
  
  # Aesthetics
  scale_color_manual(values = c("1" = "black", "2" = "firebrick"), 
                     labels = c("No Reward", "Reward")) +
  scale_size_area(max_size = 6) +
  geom_hline(yintercept = 0.25, linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Speed Accuracy Curve",
    x = "Preparation Time (seconds)",
    y = "Probability of Success",
    color = "Condition",
    size = "Trial Count"
  ) +
  xlim(0, 0.5) +
  theme_bw()+
  theme(
    plot.title = element_text(size = 26, color = "black", hjust = 0.5),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.text.y = element_text(size = 20, color = "black"),
    axis.title.x = element_text(size = 20, margin = margin(t = 15)),
    axis.title.y = element_text(size = 20, margin = margin(r = 15))
  )