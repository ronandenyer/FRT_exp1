library(nlme)
library(ggplot2)

# nonlinear mixed effect model
# the model has there parameters: mu, sigma, and the asymptote, we set the chance to 1/8 as there are # eight targets. You can include chance as a parameter if needed.
# the “start" statement is the initial value of those three parameters for each group for optimization, so there are nine # # values.
shooting = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')
shooting$reward_type = as.factor(shooting$reward_type)
shooting = shooting[shooting$eff_pt > 0 & !is.nan(shooting$eff_pt),]
shooting$eff_pt = shooting$eff_pt/1000
shooting$eff_pt = shooting$eff_pt - 0.02
shooting = shooting[shooting$SID != "FRTP15",]
# Remove NAs in the specific column
shooting <- shooting[!is.nan(shooting$prev_trial_success), ]

# Ensure the columns are strictly factors with no 'ghost' levels
shooting$reward_type <- factor(shooting$reward_type)
shooting$prev_trial_success <- factor(shooting$prev_trial_success)

# 2. Update the NLME model
# Ensure prev_trial_success is a factor so R treats it as 0/1
shooting$prev_trial_success <- as.factor(shooting$prev_trial_success)

shooting.nlme_updated <- nlme(
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  fixed = list(
    mu ~ reward_type * prev_trial_success, # 4 parameters
    sigma ~ reward_type * prev_trial_success,                   # 2 parameters
    asym ~ reward_type * prev_trial_success,# 4 parameters
    gamma ~ 1                            # 1 parameter
  ), 
  
  random = mu  ~ 1 | SID,
  data = shooting,
  
  # Total of 9 values (4 + 2 + 2 + 1)
  start = list(fixed = c(
    0.2, 0, 0, 0,   # mu: Int, stim_diff, prev_diff, interaction
    0.02, 0.007, 0,0,   # sigma: Int, stim_diff
    0.95, 0.01, 0, 0,     # asym: Int, stim_diff
    0.25            # gamma: Int
  ))
)

summary(shooting.nlme_updated)
# output statistical value for test effects
anova(shooting.nlme_updated)
# ways to output model coefficients
library(ggplot2)
library(dplyr)

# 1. Clean factors and labels
shooting_plot <- shooting %>%
  mutate(
    # Map the numeric codes to readable strings
    Condition = ifelse(reward_type == 1, "No Reward", "Reward"),
    History = ifelse(prev_trial_success == 1, "After Success", "After Fail"),
    # Combine for the color aesthetic
    Group = interaction(Condition, History)
  )

# 2. Binned data for points
shooting_binned <- shooting_plot %>%
  mutate(bin = round(eff_pt / 0.01) * 0.01) %>%
  group_by(bin, Condition, History, Group) %>%
  summarize(prop_success = mean(success), n = n(), .groups = "drop")

# 3. Generate smooth prediction lines
plot_grid <- expand.grid(
  eff_pt = seq(min(shooting$eff_pt, na.rm = TRUE), 0.5, length.out = 200),
  reward_type = factor(c(1, 2)),
  prev_trial_success = factor(c(0, 1))
)

plot_grid$predicted <- predict(shooting.nlme_updated, newdata = plot_grid, level = 0)

# Match labels in plot_grid for the legend
plot_grid <- plot_grid %>%
  mutate(
    Condition = ifelse(reward_type == 1, "No Reward", "Reward"),
    History = ifelse(prev_trial_success == 1, "After Success", "After Fail"),
    Group = interaction(Condition, History)
  )

# 4. Build the Plot
reward_curve_clean = ggplot() +
  # Raw data points (faded in background)
  geom_point(data = shooting_binned, 
             aes(x = bin, y = prop_success, color = Condition, size = n), 
             alpha = 0.2, show.legend = c(color = TRUE, size = FALSE)) +
  # Model-fitted lines
  geom_line(data = plot_grid, 
            aes(x = eff_pt, y = predicted, color = Condition, linetype = History), 
            linewidth = 1.8) +
  # Use your requested colors
  scale_color_manual(values = c("No Reward" = "grey40", "Reward" = "red")) +
  # Differentiate history by line type (Solid for Success, Dashed for Fail)
  scale_linetype_manual(values = c("After Success" = "solid", "After Fail" = "dotted")) +
  scale_size_area(max_size = 10) +
  geom_hline(yintercept = 0.25, linetype = "dotted", alpha = 0.5) +
  labs(
    title = "Effect of Reward and Trial History",
    x = "Preparation Time (seconds)",
    y = "Probability of Success",
    color = "Reward Type",
    linetype = "Previous Trial"
  ) +
  xlim(0.1, 0.25) +
  ylim(0, 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 18, color = "black"),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

print(reward_curve_clean)

library(ggplot2)
library(patchwork) # For combining the plots

# Function to get estimates for Mu and Sigma
get_params <- function(model, param_name) {
  # Get fixed effects and variance-covariance matrix
  fix_ef <- fixef(model)
  v_cov  <- vcov(model)
  
  # Identify the relevant indices for the parameter (mu or sigma)
  # R names these like "mu.(Intercept)", "mu.reward_type2", etc.
  idx <- grep(paste0("^", param_name, "\\."), names(fix_ef))
  
  beta <- fix_ef[idx]
  V    <- v_cov[idx, idx]
  
  # Design matrix for our 4 conditions: 
  # Sham-Fail (0,0,0), Sham-Succ (0,1,0), tVNS-Fail (1,0,0), tVNS-Succ (1,1,1)
  X <- matrix(c(
    1, 0, 0, 0, # Sham After Fail
    1, 0, 1, 0, # Sham After Success
    1, 1, 0, 0, # tVNS After Fail
    1, 1, 1, 1  # tVNS After Success
  ), nrow = 4, byrow = TRUE)
  
  est <- X %*% beta
  se  <- sqrt(diag(X %*% V %*% t(X)))
  
  data.frame(
    Condition = rep(c("No Reward", "Reward"), each = 2),
    History = rep(c("After Fail", "After Success"), 2),
    Estimate = as.numeric(est),
    SE = as.numeric(se)
  )
}

mu_data <- get_params(shooting.nlme_updated, "mu")
sig_data <- get_params(shooting.nlme_updated, "sigma")

# 2. Plotting functions
plot_param <- function(df, title, y_lab) {
  ggplot(df, aes(x = History, y = Estimate, color = Condition, group = Condition)) +
    geom_line(position = position_dodge(0.3), alpha = 0.5) +
    geom_errorbar(aes(ymin = Estimate - 1.96*SE, ymax = Estimate + 1.96*SE), 
                  width = 0.2, position = position_dodge(0.3), linewidth = 1) +
    geom_point(position = position_dodge(0.3), size = 4) +
    scale_color_manual(values = c("No Reward" = "grey40", "Reward" = "red")) +
    labs(title = title, y = y_lab, x = NULL) +
    theme_minimal() +
    theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))
}

p_mu  <- plot_param(mu_data, "Mu (Threshold)", "Time (s)")
p_sig <- plot_param(sig_data, "Sigma (Noise)", "Noise Level")

# 3. Combine with your existing curve (assuming tvns_curve_clean is defined)
# We use the / and | operators from the patchwork library
final_layout <- reward_curve_clean / (p_mu | p_sig) + 
  plot_layout(heights = c(2, 1))

print(final_layout)

library(ggplot2)

# Create the 4-level grouping for visualization
shooting <- shooting %>%
  mutate(Condition_History = interaction(reward_type, prev_trial_success, sep = "-"))

ggplot(shooting, aes(x = eff_pt, fill = Condition_History)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("1-0" = "grey70", "1-1" = "grey30", 
                               "2-0" = "lightgreen", "2-1" = "darkgreen"),
                    labels = c("Sham-Fail", "Sham-Success", "tVNS-Fail", "tVNS-Success")) +
  labs(title = "Distribution of Preparation Time (eff_pt) across Conditions",
       x = "Preparation Time (s)",
       fill = "Condition") +
  theme_minimal()

# Check if prep time differs by condition or history
check_dist <- aov(eff_pt ~ reward_type * prev_trial_success, data = shooting)
summary(check_dist)