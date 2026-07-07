library(ggplot2)
library(tidyr)
library(dplyr)
# Load libraries
library(ggplot2)
library(patchwork) # For placing plots side-by-side

# 1. Prepare the data
df <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//tvns_delta_mu_sigma.csv')

# Determine common Y limits to ensure the axes are the same
# We find the absolute min and max across both columns
y_limit <- c(-0.07, 0.1)

# 2. Create Plot 1 (delta_mu)
p1 <- ggplot(df, aes(x = "Delta Mu", y = delta_mu)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_violin(fill = "#56B4E9", alpha = 0.4, trim = FALSE) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.7) +
  ylim(y_limit) + # Force identical Y axis
  theme_bw() +
  labs(x = NULL, y = "Delta Mu (tVNS - Sham)")+
  theme(
    plot.title = element_text(size = 26, color = "black", hjust = 0.5),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.text.y = element_text(size = 20, color = "black"),
    axis.title.x = element_text(size = 20, margin = margin(t = 15)),
    axis.title.y = element_text(size = 20, margin = margin(r = 15))
  )

# 3. Create Plot 2 (delta_sigma)
p2 <- ggplot(df, aes(x = "Delta Sigma", y = delta_sigma)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_violin(fill = "#E69F00", alpha = 0.4, trim = FALSE) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.7) +
  ylim(y_limit) + # Force identical Y axis
  theme_bw() +
  labs(x = NULL, y = 'Delta Sigma (tVNS - Sham)')+
  theme(
    plot.title = element_text(size = 26, color = "black", hjust = 0.5),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.text.y = element_text(size = 20, color = "black"),
    axis.title.x = element_text(size = 20, margin = margin(t = 15)),
    axis.title.y = element_text(size = 20, margin = margin(r = 15))
  )

# 4. Combine them side-by-side
# Using patchwork syntax: p1 + p2
combined_plot <- p1 + p2

# Display
combined_plot