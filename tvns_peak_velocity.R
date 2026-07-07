library(ggplot2)
df = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//tvns_mean_peak_velocity.csv')

library(ggplot2)
library(ggsignif)

# 1. Pre-processing: Ensure Sham is the first level
df$stim_type <- factor(df$stim_type, levels = c("Sham", "tVNS"))

# 2. Create the Plot
ggplot(df, aes(x = stim_type, y = peak_vel, fill = stim_type)) +
  # Distribution & Outline
  geom_violin(color = "black", alpha = 0.3, size = 0.8, trim = FALSE) +
  
  # Individual connections
  geom_line(aes(group = SID), color = "gray40", alpha = 0.4, linewidth = 0.6) +
  
  # Individual dots
  geom_point(size = 3.5, alpha = 0.9, shape = 21, color = "black") +
  
  # Mean crossbar
  stat_summary(fun = mean, geom = "crossbar", width = 0.35, color = "red", size = 1) +
  
  
  
  # Styling and LARGE Axis Text
  scale_fill_manual(values = c("Sham" = "#bdc3c7", "tVNS" = "#3498db")) +
  theme_minimal(base_size = 18) + # Higher base size scales everything up
  labs(
    title = "Peak Velocity",
    x = "Condition",
    y = "Peak Velocity (m/s)"
  ) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    # SPECIFIC AXIS TEXT CONTROLS:
    axis.text.x = element_text(size = 18, color = "black", face = "bold"),
    axis.text.y = element_text(size = 18, color = "black"),
    axis.title.x = element_text(size = 20, margin = margin(t = 15)),
    axis.title.y = element_text(size = 20, margin = margin(r = 15))
  )
# Load necessary libraries
library(lme4)
library(lmerTest) # This package provides p-values for the lmer output

# Fit the model
# peak_vel: dependent variable
# stim_type: fixed effect (the thing you manipulated)
# (1 | SID): random intercept (accounts for baseline differences between individuals)
lmm_model <- lmer(peak_vel ~ stim_type + (1 | SID), data = df)

# View the full results
summary(lmm_model)

# Quick interpretation:
# Look at 'Fixed effects' -> 'stim_typetVNS'. 
# The 'Estimate' is the average increase/decrease caused by the reward.
# The 'Pr(>|t|)' is your p-value.