library(nlme)
library(ggplot2)
library(gridExtra)
library(dplyr)

# nonlinear mixed effect model
# the model has there parameters: mu, sigma, and the asymptote, we set the chance to 1/8 as there are # eight targets. You can include chance as a parameter if needed.
# the “start" statement is the initial value of those three parameters for each group for optimization, so there are nine # # values.
shooting = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')
shooting$reward_type = as.factor(shooting$reward_type)

shooting$effort = as.numeric(shooting$end_target > 3.5) + 1
shooting$effort = as.factor(shooting$effort)

#shooting$success = as.factor(shooting$success)
shooting = shooting[!is.nan(shooting$eff_pt),]
shooting$eff_pt = shooting$eff_pt/1000
#shooting$eff_pt = shooting$eff_pt - 0.02

shooting = shooting[shooting$SID != "FRTP15",]

shooting.nlme <- nlme(
  # Replace 1/4 with gamma
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  # Add gamma to the fixed effects list
  # We'll start by assuming gamma is the same for both reward types (gamma ~ 1)
  # because estimating two asymptotes can make the model very unstable.
  fixed = list(mu ~ effort, 
               sigma ~ effort, 
               asym ~ effort, 
               gamma ~ 1), 
  
  random = mu ~ 1| SID,
  data = shooting,
  
  # Update start values: you now need 7 values 
  # (2 for mu, 2 for sigma, 2 for asym, and 1 for gamma)
  start = list(fixed = c(0.2, -0.01,  # mu (Intercept, diff)
                         0.02, 0.007, # sigma (Intercept, diff)
                         0.95, 0.01,  # asym (Intercept, diff)
                         0.25)),       # gamma (the guess rate)
  control = nlmeControl(msMaxIter = 200, 
                        pnlsMaxIter = 200, 
                        msVerbose = TRUE) # msVerbose lets you see it "working"
)

summary(shooting.nlme)
anova(shooting.nlme)
# ways to output model coefficients
shooting.nlme$coefficients

shooting.nlmecomplex <- nlme(
  # Replace 1/4 with gamma
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  # Add gamma to the fixed effects list
  # We'll start by assuming gamma is the same for both reward types (gamma ~ 1)
  # because estimating two asymptotes can make the model very unstable.
  fixed = list(mu ~ effort, 
               sigma ~ effort, 
               asym ~ effort, 
               gamma ~ effort), 
  
  random = mu + sigma  ~ 1 | SID,
  data = shooting,
  
  # Update start values: you now need 7 values 
  # (2 for mu, 2 for sigma, 2 for asym, and 1 for gamma)
  start = list(fixed = c(0.2, -0.01,  # mu (Intercept, diff)
                         0.02, 0.007, # sigma (Intercept, diff)
                         0.95, 0.01,  # asym (Intercept, diff)
                         0.25, 0)),       # gamma (the guess rate)
  control = nlmeControl(msMaxIter = 200, 
                        pnlsMaxIter = 200, 
                        msVerbose = FALSE) # msVerbose lets you see it "working"
)

summary(shooting.nlmecomplex)
# output statistical value for test effects
anova(shooting.nlmecomplex)
# ways to output model coefficients
shooting.nlmecomplex$coefficients
# test which has bette rfit
anova(shooting.nlme, shooting.nlmecomplex)
# Visualize data



# 1. Create binned data for the raw points (e.g., every 40ms)
# This lets us see the actual "empirical" psychometric curve
shooting_binned <- shooting %>%
  mutate(bin = round(eff_pt / 0.02) * 0.02) %>%
  group_by(bin, effort) %>%
  summarize(
    prop_success = mean(success),
    n = n(),
    .groups = "drop"
  )

# 2. Generate smooth prediction lines from the nlme model
# This uses the fixed effects coefficients you just calculated
plot_grid <- expand.grid(
  eff_pt = seq(min(shooting$eff_pt), max(shooting$eff_pt), length.out = 200),
  effort = factor(c(1, 2))
)

# level = 0 uses fixed effects (the "average" person)
plot_grid$predicted <- predict(shooting.nlme, newdata = plot_grid, level = 0)


# 2. Generate smooth prediction lines
# Create a grid for the population average (Level 0)
plot_grid <- expand.grid(
  eff_pt = seq(min(shooting$eff_pt), max(shooting$eff_pt), length.out = 500),
  effort = factor(c(1, 2))
)
plot_grid$predicted <- predict(shooting.nlmecomplex, newdata = plot_grid, level = 0)

# NEW: Create a grid for individual participants (Level 1)
# Replace 'SID' with whatever your grouping variable is named in your model/data
subj_ids <- unique(shooting$SID) 
indiv_grid <- expand.grid(
  eff_pt = seq(min(shooting$eff_pt), max(shooting$eff_pt), length.out = 500), # Fewer points to save memory
  effort = factor(c(1, 2)),
  SID = subj_ids
)
indiv_grid$predicted_indiv <- predict(shooting.nlme, newdata = indiv_grid, level = 1)

# 3. Build the plot

legend_text_size = 20
legend_coords = c(0.85, 0.45)

x_axis_title_size = 28
y_axis_title_size = 24
axis_text_size = 26

reward_sat_plot = ggplot() +
  # A. Individual Participant Lines (Faint)
  #geom_line(data = indiv_grid, 
   #         aes(x = eff_pt, y = predicted_indiv, color = effort, group = interaction(SID, effort)), 
    #        alpha = 0.15, linewidth = 0.4) + # Low alpha makes them "faint"
  
  # B. Raw binned data points
  #geom_point(data = shooting_binned, 
   #          aes(x = bin, y = prop_success, color = effort, size = n), 
    #         alpha = 0.3) +
  
  # C. Main Model-fitted lines (Thick)
  geom_line(data = plot_grid, 
            aes(x = eff_pt, y = predicted, color = effort), 
            linewidth = 2) +
  
  # Aesthetics
  scale_color_manual(values = c("1" = "black", "2" = "#ff5050"), 
                     labels = c("Low Effort", "High Effort")) +
  scale_size_area(max_size = 6) +
  geom_hline(yintercept = 0.25, linetype = "dashed", alpha = 0.5) +
  labs(
    x = "Preparation Time (s)",
    y = "Probability of Success",
    color = "Condition",
    size = "Trial Count",
    tag = "B.",
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

reward_sat_plot