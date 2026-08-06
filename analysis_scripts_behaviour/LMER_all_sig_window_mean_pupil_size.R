library(sjPlot)
library(lme4)
library(lmerTest)
library(ggplot2)
library(emmeans)
library(dplyr)
df1 = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//reward_pupil_means.csv')
df1$condition = df1$reward_type
df2 = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//tvns_pupil_means.csv')
df2$condition = df2$stim_type
# 1. Find the columns they have in common
common_cols <- intersect(names(df1), names(df2))

# 2. Subset both data frames to only those columns and stack them
df <- bind_rows(
  df1[, common_cols, drop = FALSE], 
  df2[, common_cols, drop = FALSE]
)
df$mean_pupil_size_sig_window = df$mean_pupil_size_sig_window * 100000000
df$max_pupil_size_sig_window = df$max_pupil_size_sig_window * 100000000

# 1. Pre-processing: Ensure 'No Reward' is the first level
df$condition <- factor(df$condition, levels = c("no_reward", "reward", "Sham", "tVNS"))

lmm_model <- lmer(max_pupil_size_sig_window ~ condition + (1 | SID), data = df)

# View the full results
tab_model(lmm_model)


# 1. Get the estimated marginal means for each condition
em_pairs <- emmeans(lmm_model, specs = ~ condition)

# 2. View the estimated means, standard errors, and confidence intervals
print(em_pairs)

# 3. Test the simple main effects (pairwise comparisons)
# Adjusts for multiple comparisons automatically (Tukey method)
contrast(em_pairs, method = "pairwise")


# Create the plot
ggplot(df, aes(x = condition, y = max_pupil_size_sig_window, fill = condition)) +
  # 1. Add the violin plot (trim = FALSE shows the full density)
  geom_violin(alpha = 0.4, color = "black", trim = FALSE) +
  
  # 2. Add the individual points with jitter
  # width = 0.2 prevents points from spreading too far horizontally
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.5, color = "black") +
  
  # 3. Optional: Add a mean line or crossbar to show the central tendency
  stat_summary(fun = "mean", geom = "crossbar", width = 0.2, color = "red") +
  
  # Styling
  theme_minimal() +
  labs(
    title = "Pupil Size by Condition",
    subtitle = "Points represent individual SID observations",
    x = "Condition",
    y = "Mean Pupil Size (Signal Window)"
  ) +
  theme(legend.position = "none") # Hide legend as X-axis already labels conditions