library(nlme)
library(ggplot2)
library(ggrepel) # For non-overlapping text labels
library(dplyr)
# nonlinear mixed effect model
# the model has there parameters: mu, sigma, and the asymptote, we set the chance to 1/4 as there are # eight targets. You can include chance as a parameter if needed.
# the “start" statement is the initial value of those three parameters for each group for optimization, so there are nine # # values.
shooting = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')
shooting$stim_type = as.factor(shooting$stim_type)
shooting$stim_type = relevel(shooting$stim_type, ref = 1)
shooting = shooting[!is.na(shooting$eff_pt),]
shooting = shooting[!is.na(shooting$success),]

shooting$eff_pt = shooting$eff_pt/1000

#shooting = shooting[shooting$SID != "FRTP22",]

shooting.nlme <- nlme(
  # Replace 1/4 with gamma
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  # Add gamma to the fixed effects list
  # We'll start by assuming gamma is the same for both reward types (gamma ~ 1)
  # because estimating two asymptotes can make the model very unstable.
  fixed = list(mu ~ stim_type*, 
               sigma ~ 1, 
               asym ~ stim_type, 
               gamma ~ 1), 
  
  random = mu ~ stim_type | SID,
  data = shooting,
  
  # Update start values: you now need 7 values 
  # (2 for mu, 2 for sigma, 2 for asym, and 1 for gamma)
  start = list(fixed = c(0.2, -0.01,  # mu (Intercept, diff)
                         0.02, # sigma (Intercept, diff)
                         0.95, 0.01,  # asym (Intercept, diff)
                         0.25))       # gamma (the guess rate)
)

summary(shooting.nlme)
# output statistical value for test effects
anova(shooting.nlme)
# ways to output model coefficients
shooting.nlme$coefficients
# This gives you just the random deviations (same as your manual indexing)
random_effects <- ranef(shooting.nlme)



shooting.nlmecomplex <- nlme(
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  # Total parameters: 2 (mu) + 2 (sigma) + 1 (asym) + 1 (gamma) = 6
  fixed = list(mu ~ stim_type, 
               sigma ~ stim_type, 
               asym ~ stim_type, 
               gamma ~ 1),
  
  random = mu ~ stim_type | SID,
  data = shooting,
  
  # Start list must have exactly 6 values
  start = list(fixed = c(0.2, 0.0,   # mu: Intercept, stim_type effect
                         0.02, 0.0,  # sigma: Intercept, stim_type effect
                         0.95, 0,       # asym: Intercept only
                         0.25)),      # gamma: Intercept only
  control = nlmeControl(msMaxIter = 400, 
                        pnlsMaxIter = 200, 
                        msVerbose = TRUE) # msVerbose lets you see it "working"
)
# output statistical value for test effects
summary(shooting.nlmecomplex)

anova(shooting.nlmecomplex)
# ways to output model coefficients
shooting.nlmecomplex$coefficients
# Visualize data

# oldham test to check validity of correlation
# 1. Extract coefficients for every subject (SID)
subj_params <- coef(shooting.nlmecomplex)

# 2. Isolate the sigma parameters
# Based on your summary, the columns should be 'sigma.(Intercept)' and 'sigma.stim_type2'
sigma_baseline <- subj_params[,"sigma.(Intercept)"]
sigma_change   <- subj_params[,"sigma.stim_type2"]

# 3. Calculate the 'Treatment' (tVNS) sigma for each person
# Change = Treatment - Baseline -> Treatment = Baseline + Change
sigma_tvns <- sigma_baseline + sigma_change

# 4. Calculate the Oldham Mean (Average of both states)
sigma_oldham_mean <- (sigma_baseline + sigma_tvns) / 2

# Perform the 'artifact-free' correlation
oldham_test <- cor.test(sigma_oldham_mean, sigma_change)

# View the results
print(oldham_test)


# 1. Create a "fake" dataset for plotting smooth curves
plot_data <- expand.grid(
  eff_pt = seq(0, 0.5, length.out = 100),
  stim_type = factor(c(1, 2))
)

# 2. Predict success probability using your model (fixed effects only)
plot_data$predicted_prob <- predict(shooting.nlmecomplex, newdata = plot_data, level = 0)

# 3. Plot it
ggplot(plot_data, aes(x = eff_pt, y = predicted_prob, color = stim_type)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("black", "green"), 
                     labels = c("Sham", "tVNS")) +
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

#x = cor.test(plot_data$Baseline_Sigma, plot_data$Stim_Effect_Sigma)
# 1. Create binned data for the raw points (e.g., every 40ms)
# This lets us see the actual "empirical" psychometric curve
shooting_binned <- shooting %>%
  mutate(bin = round(eff_pt / 0.02) * 0.02) %>%
  group_by(bin, stim_type) %>%
  summarize(
    prop_success = mean(success),
    n = n(),
    .groups = "drop"
  )

# 2. Generate smooth prediction lines from the nlme model
# This uses the fixed effects coefficients you just calculated
plot_grid <- expand.grid(
  eff_pt = seq(min(shooting$eff_pt), max(shooting$eff_pt), length.out = 200),
  stim_type = factor(c(1, 2))
)

# level = 0 uses fixed effects (the "average" person)
plot_grid$predicted <- predict(shooting.nlme, newdata = plot_grid, level = 0)

# NEW: Create a grid for individual participants (Level 1)
# Replace 'SID' with whatever your grouping variable is named in your model/data
subj_ids <- unique(shooting$SID) 
indiv_grid <- expand.grid(
  eff_pt = seq(min(shooting$eff_pt), max(shooting$eff_pt), length.out = 500), # Fewer points to save memory
  stim_type = factor(c(1, 2)),
  SID = subj_ids
)
indiv_grid$predicted_indiv <- predict(shooting.nlme, newdata = indiv_grid, level = 1)


# 3. Build the plot

legend_text_size = 20
legend_coords = c(0.85, 0.45)

x_axis_title_size = 28
y_axis_title_size = 24
axis_text_size = 26

tvns_curve = ggplot() +
  # Plot the raw binned data points
  # The size of the point represents how much data is in that bin
  #geom_line(data = indiv_grid, 
   #         aes(x = eff_pt, y = predicted_indiv, color = stim_type, group = interaction(SID, stim_type)), 
    #        alpha = 0.15, linewidth = 0.4) + # Low alpha makes them "faint"
  #geom_point(data = shooting_binned, 
  #           aes(x = bin, y = prop_success, color = stim_type, size = n), 
   #          alpha = 0.4) +
  # Plot the model-fitted lines
  geom_line(data = plot_grid, 
            aes(x = eff_pt, y = predicted, color = stim_type), 
            linewidth = 2) +
  # Aesthetics
  scale_color_manual(values = c("1" = "black", "2" = "green"), 
                     labels = c("Sham", "tVNS")) +
  scale_size_area(max_size = 8) +
  geom_hline(yintercept = 0.25, linetype = "dashed", alpha = 0.5) + # The 1/4 chance floor
  #annotate("text", x = 0.4, y = 0.28, label = "Chance (25%)", size = 3, alpha = 0.5) +
  labs(
    x = "Preparation Time (s)",
    y = "Probability of Success",
    size = "Trial Count",
    color = "Condition",
    tag = "B."
  ) +
  guides(
    size = guide_legend(order = 1),   # "Trial Count" (size) goes first
    color = guide_legend(order = 2)   # "Condition" (color) goes second
  ) +
  scale_y_continuous(limits = c(0, 1), 
                     breaks = seq.int(0, 1, 0.2)) +
  xlim(0,0.5) +
  theme_classic()+
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black", fill = "white"),
    axis.title.y = element_text(size = y_axis_title_size),
    axis.title.x = element_text(size = x_axis_title_size),
    axis.text = element_text(size = axis_text_size ),
    legend.position = legend_coords,
    plot.tag = element_text(size = 45, face = "bold", hjust = 0, vjust = 1)
  ) 
tvns_curve
