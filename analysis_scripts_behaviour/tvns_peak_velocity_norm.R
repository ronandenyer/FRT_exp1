library(ggplot2)
df = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//tvns_mean_peak_velocity_norm.csv')

library(ggplot2)
library(ggsignif)

# 1. Pre-processing: Ensure Sham is the first level
df$stim_type <- factor(df$Condition, levels = c("Sham", "tVNS"))


x_axis_title_size = 28
y_axis_title_size = 20
axis_text_size = 18

# 2. Create the Plot
tVNS_peak_velocity_plot = ggplot(df, aes(x = stim_type, y = MeanPeakVel, fill = stim_type)) +
  # Distribution & Outline
  geom_violin(color = "black", alpha = 0.3, size = 0.8, trim = FALSE) +
  
  # Individual connections
  geom_line(aes(group = SID), color = "gray40", alpha = 0.4, linewidth = 0.6) +
  
  # Individual dots
  geom_point(size = 3.5, alpha = 0.9, shape = 21, color = "black") +
  
  # Mean crossbar
  stat_summary(aes(color = stim_type),fun = mean, geom = "crossbar", width = 0.35, size = 1) +
  
  
  # Styling and LARGE Axis Text
  scale_fill_manual(values = c("Sham" = "gray20", "tVNS" = "green")) +
  scale_color_manual(values = c("Sham" = "black", "tVNS" = "green")) +
  ylim(0, 350) +
  scale_x_discrete(limits = c("Sham", "tVNS"),
                   labels = c("Sham", "tVNS")) +
  theme_minimal(base_size = 18) + # Higher base size scales everything up
  labs(
    tag = "C.",    
    x = "Condition",
    y = expression("Peak Velocity "[Baseline~normalized]*" (%)")
  ) +
  theme_classic()+
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black", fill = "white"),
    axis.title.y = element_text(size = y_axis_title_size),
    axis.title.x = element_text(size = x_axis_title_size),
    axis.text = element_text(size = axis_text_size ),
    legend.position = "none",
    plot.tag = element_text(size = 45, face = "bold", hjust = 0, vjust = 1)
  ) 
tVNS_peak_velocity_plot
library(patchwork)

# Combine into 1 row, 3 columns with 40/40/20 width split
tVNS_final_grid <- tvns_pupil_plot + tvns_curve + tVNS_peak_velocity_plot + 
  plot_layout(ncol = 3, widths = c(0.4, 0.4, 0.2))

# View the plot in RStudio
tVNS_final_grid

plot_w = 1820 * (300/96)
plot_h = 580 * (300/96)

png("tVNS_prememoire_plots.png", width = plot_w, height = plot_h, res=300)

# Generate the plot
print(tVNS_final_grid)  # If it's a ggplot object


# Close the device to save the file
dev.off()