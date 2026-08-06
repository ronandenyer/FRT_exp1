library(sjPlot)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)

df = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//tvns_pupil_means.csv')

df$mean_pupil_size_sig_window = df$mean_pupil_size_sig_window * 100000000
# 1. Pre-processing: Ensure 'No Reward' is the first level
df$stim_type <- factor(df$stim_type, levels = c("Sham","tVNS"))

lmm_model <- lmer(mean_pupil_size_sig_window ~ stim_type*HighShamSigma + (1 | SID), data = df)

# View the full results
tab_model(lmm_model)


# Ensure SID is treated as a factor so lines connect properly
df$SID <- as.factor(df$SID)

ggplot(df, aes(x = stim_type, y = mean_pupil_size_sig_window)) +
  # 1. Add the violin plot base (trimmed = FALSE keeps the beautiful violin shape)
  geom_violin(aes(fill = stim_type), alpha = 0.4, trim = FALSE, show.legend = FALSE) +
  
  # 2. Add lines connecting individual data points across the stim_types
  geom_line(aes(group = SID), color = "gray60", alpha = 0.5, linewidth = 0.5) +
  
  # 3. Add individual data points on top
  geom_point(aes(color = stim_type), size = 2.5, alpha = 0.8, show.legend = FALSE) +
  
  # 4. Optional: Add a boxplot inside for a clean summary of the distribution
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA, alpha = 0.6) +
  
  # Styling and Labels
  labs(
    x = "Stimulation Type",
    y = "Mean Pupil Size (sig window)",
    title = "Pupil Size Distribution by Stimulation Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(), # Removes unnecessary vertical grids
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")