library(ggplot2)
library(dplyr)

# 1. Calculate exact mu values from the model coefficients
# Reward Experiment: Intercept = No Reward, Intercept + reward_type2 = Reward
mu_no_reward <- 0.2020459
mu_reward    <- 0.2020459 + (-0.0101667)

# tVNS Experiment: Intercept = Sham, Intercept + stim_type2 = tVNS
mu_sham      <- 0.2003983
mu_tvns      <- 0.2003983 + (-0.0016293)

# 2. Build the tidy dataframe for plotting
plot_data <- data.frame(
  condition = factor(c("No Reward", "Reward", "Sham", "tVNS"), 
                     levels = c("No Reward", "Reward", "Sham", "tVNS")),
  mu_value = c(mu_no_reward, mu_reward, mu_sham, mu_tvns),
  experiment = c("Reward Paradigm", "Reward Paradigm", "tVNS Paradigm", "tVNS Paradigm")
)

# 3. Define the requested color palette
condition_colors <- c(
  "No Reward" = "gray10", 
  "Reward"    = "#e74c3c",
  "Sham"      = "#3498db",
  "tVNS"      = "green"
)

# 4. Generate the dot plot
ggplot(plot_data, aes(x = condition, y = mu_value, color = condition)) +
  # Connecting lines grouped by experiment paradigm for visual clarity
  #geom_line(aes(group = experiment), color = "gray70", linetype = "dashed", linewidth = 0.8) +
  # Points for each condition
  geom_point(size = 6) +
  # Custom colors
  scale_color_manual(values = condition_colors) +
  # Theme and labels
  theme_classic(base_size = 14) +
  labs(
    title = "Estimated Preparation Time (μ) \n All Conditions",
    x = "Condition",
    y = expression("Estimated Preparation Time (" * mu * "; s)")
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30"),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  ) +ylim(0.19, 0.203)