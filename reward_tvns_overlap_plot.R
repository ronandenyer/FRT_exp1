library(nlme)
library(ggplot2)
library(gridExtra)

# --- 1. Load and Clean Reward Data ---
reward_data = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')

reward_data$reward_type = as.factor(reward_data$reward_type)
reward_data = reward_data[!is.nan(reward_data$eff_pt),]

# Convert ms to seconds and apply offset
reward_data$eff_pt = reward_data$eff_pt / 1000
reward_data$eff_pt = reward_data$eff_pt - 0.02

# Exclude specific participants
reward_data = reward_data[reward_data$SID != "FRTP15",]

# --- 2. Run Reward NLME Model ---
reward.nlme <- nlme(
  # Model formula: success probability based on cumulative normal distribution
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  # Fixed effects: testing the impact of reward_type on threshold, noise, and asymptote
  fixed = list(mu ~ reward_type,  
               sigma ~ reward_type,  
               asym ~ reward_type,  
               gamma ~ 1),  
  
  # Random effects: allow intercept of mu (threshold) to vary by Subject ID
  random = mu ~ 1 | SID,
  data = reward_data,
  
  # Start values (7 total: 2 for mu, 2 for sigma, 2 for asym, 1 for gamma)
  start = list(fixed = c(0.2, -0.01,  # mu (Intercept, diff)
                         0.02, 0.007, # sigma (Intercept, diff)
                         0.95, 0.01,  # asym (Intercept, diff)
                         0.25))       # gamma (guess rate)
)

# --- 3. Review Results ---
summary(reward.nlme)
anova(reward.nlme)

# --- 1. Load and Clean tVNS Data ---
tvns_data = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')

tvns_data$stim_type = as.factor(tvns_data$stim_type)
# Ensure data is valid and remove NaN or 0 values
tvns_data = tvns_data[tvns_data$eff_pt > 0 & !is.nan(tvns_data$eff_pt),]

# Convert ms to seconds and apply the 20ms offset
tvns_data$eff_pt = tvns_data$eff_pt / 1000
tvns_data$eff_pt = tvns_data$eff_pt - 0.02

# Exclude specific participant
tvns_data = tvns_data[tvns_data$SID != "FRTP15",]

# --- 2. Run tVNS NLME Model ---
tvns.nlme <- nlme(
  # Model formula
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  # Fixed effects: testing impact of stim_type (Sham vs tVNS)
  fixed = list(mu ~ stim_type,  
               sigma ~ stim_type,  
               asym ~ stim_type,  
               gamma ~ 1),  
  
  # Random effects: Mu varies by Subject ID
  random = mu ~ 1 | SID,
  data = tvns_data,
  
  # Start values (7 total)
  start = list(fixed = c(0.2, -0.01,  # mu (Intercept, diff)
                         0.02, 0.007, # sigma (Intercept, diff)
                         0.95, 0.01,  # asym (Intercept, diff)
                         0.25))       # gamma (guess rate)
)

# --- 3. Review Results ---
summary(tvns.nlme)
anova(tvns.nlme)
# --- 1. Generate Prediction Grids ---
# Create a standard timeline for the x-axis
time_seq <- seq(0, 0.5, length.out = 300)

# Predictions for Reward Experiment
reward_grid <- expand.grid(eff_pt = time_seq, reward_type = factor(c(1, 2)))
reward_grid$predicted <- predict(reward.nlme, newdata = reward_grid, level = 0)
reward_grid$Condition <- ifelse(reward_grid$reward_type == 1, "No Reward", "Reward")

# Predictions for tVNS Experiment
tvns_grid <- expand.grid(eff_pt = time_seq, stim_type = factor(c(1, 2)))
tvns_grid$predicted <- predict(tvns.nlme, newdata = tvns_grid, level = 0)
tvns_grid$Condition <- ifelse(tvns_grid$stim_type == 1, "Sham", "tVNS")

# --- 2. Combine into one plotting data frame ---
# We subset to only the columns we need so they stack perfectly
plot_combined <- rbind(
  reward_grid[, c("eff_pt", "predicted", "Condition")],
  tvns_grid[, c("eff_pt", "predicted", "Condition")]
)

# Reorder factor for a logical legend
plot_combined$Condition <- factor(plot_combined$Condition, 
                                  levels = c("Sham", "No Reward", "tVNS", "Reward"))

# --- 3. Create the Final Plot ---


ggplot(plot_combined, aes(x = eff_pt, y = predicted, color = Condition, linetype = Condition)) +
  # Draw the model-fitted curves
  geom_line(linewidth = 1.5) +
  
  # Custom Aesthetics
  scale_color_manual(values = c(
    "Sham" = "black", 
    "No Reward" = "gray45", 
    "tVNS" = "forestgreen", 
    "Reward" = "firebrick"
  )) +
  scale_linetype_manual(values = c(
    "Sham" = "dashed", 
    "No Reward" = "dotted", 
    "tVNS" = "solid", 
    "Reward" = "solid"
  )) +
  
  # The 25% Guess Rate Line
  geom_hline(yintercept = 0.25, linetype = "solid", color = "gray90") +
  annotate("text", x = 0.45, y = 0.28, label = "Chance", size = 3, color = "gray70") +
  
  # Labels and Formatting
  
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  coord_cartesian(xlim = c(0, 0.5)) +
  labs(
    title = "Speed Accuracy Curve - All Conditions",
    x = "Preparation Time (seconds)",
    y = "Probability of Success",
    color = "Condition",
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