library(nlme)
library(ggplot2)
library(ggrepel) # For non-overlapping text labels
library(dplyr)

# =========================================================================
# 1. Load and Clean Data (Strict NA filtering for nlme stability)
# =========================================================================
shooting <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')

# Ensure factors are set up correctly
shooting$stim_type <- as.factor(shooting$stim_type)
shooting$stim_type <- relevel(shooting$stim_type, ref = 1)
shooting$SID <- as.factor(shooting$SID)

# Rescale preparation time to seconds
shooting$eff_pt <- shooting$eff_pt / 1000

# Strict Row Filtering: nlme will fail completely if any model variable contains NA
shooting <- shooting %>%
  filter(
    eff_pt > 0,
    !is.na(eff_pt),
    !is.na(success),
    !is.na(pupil_size_phasic_mean),
    !is.na(stim_type),
    !is.na(SID)
  )

# =========================================================================
# 2. Compute Global Median Split on Phasic Pupil Mean
# =========================================================================
global_median <- median(shooting$pupil_size_phasic_mean, na.rm = TRUE)
shooting$pupil_baseline_split <- ifelse(shooting$pupil_size_phasic_mean > global_median, 2, 1)
shooting$pupil_baseline_split <- as.factor(shooting$pupil_baseline_split)

# =========================================================================
# 3. Run the Fully Interacted 2x2 Psychometric Model
# =========================================================================
shooting.nlme_interaction <- nlme(
  # Core psychometric sigmoidal equation
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  # Expanded fixed effects to test the 2x2 interaction on all dimensions
  fixed = list(
    mu ~ pupil_baseline_split * stim_type,    
    sigma ~ pupil_baseline_split * stim_type, 
    asym ~ pupil_baseline_split * stim_type,  
    gamma ~ 1
  ), 
  
  # Stable random intercept per subject
  random = mu ~ 1 | SID,
  
  data = shooting,
  
  # 13 Starting values mapped precisely to R's internal expansion order:
  # Order per parameter: (Intercept), Split_diff, Stim_diff, Interaction_diff
  start = list(fixed = c(
    0.20, -0.01,  0.00,  0.00,  # mu coefficients
    0.02,  0.005, 0.00,  0.00,  # sigma coefficients
    0.95,  0.01,  0.00,  0.00,  # asym coefficients
    0.25                    # gamma (guess rate fixed at 25%)
  ))
)

# Print structural summaries
summary(shooting.nlme_interaction)

anova(shooting.nlme_interaction)
# output statistical value for test effects
anova(shooting.nlme)
# ways to output model coefficients
shooting.nlme$coefficients
# This gives you just the random deviations (same as your manual indexing)
random_effects <- ranef(shooting.nlme)

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

library(dplyr)
library(ggplot2)

# =========================================================================
# 1. Create Empirical Binned Data (Raw Points)
# =========================================================================
shooting_binned <- shooting %>%
  filter(!is.na(pupil_baseline_split), !is.na(stim_type), !is.na(success), !is.na(eff_pt)) %>%
  mutate(bin = round(eff_pt / 0.02) * 0.02) %>%
  group_by(bin, stim_type, pupil_baseline_split) %>%
  summarize(
    prop_success = mean(success),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(bin >= 0 & bin <= 0.5)

# Create a combined factor for plotting raw binned points easily
shooting_binned$Condition_Split <- interaction(shooting_binned$stim_type, shooting_binned$pupil_baseline_split)

# =========================================================================
# 2. Generate Smooth Prediction Lines (Population Fixed Effects - Level 0)
# =========================================================================
plot_grid <- expand.grid(
  eff_pt = seq(0, 0.5, length.out = 300),
  stim_type = factor(c("1", "2")),
  pupil_baseline_split = factor(c("1", "2"))
)

# Generate predictions using the full interaction model
plot_grid$predicted <- predict(shooting.nlme_interaction, newdata = plot_grid, level = 0)

# Create clean display names for the legends
plot_grid$Condition <- factor(plot_grid$stim_type, levels = c("1", "2"), labels = c("Sham", "tVNS"))
plot_grid$Arousal <- factor(plot_grid$pupil_baseline_split, levels = c("1", "2"), labels = c("Low Phasic Pupil", "High Phasic Pupil"))

# =========================================================================
# 3. Create Prediction Grid for Individual Curves (Subject Level - Level 1)
# =========================================================================
subj_ids <- unique(shooting$SID) 
indiv_grid <- expand.grid(
  eff_pt = seq(0, 0.5, length.out = 100), # Lower resolution to keep it fast
  stim_type = factor(c("1", "2")),
  pupil_baseline_split = factor(c("1", "2")),
  SID = subj_ids
)

indiv_grid$predicted_indiv <- predict(shooting.nlme_interaction, newdata = indiv_grid, level = 1)
indiv_grid$Condition <- factor(indiv_grid$stim_type, levels = c("1", "2"), labels = c("Sham", "tVNS"))
indiv_grid$Arousal <- factor(indiv_grid$pupil_baseline_split, levels = c("1", "2"), labels = c("Low Phasic Pupil", "High Phasic Pupil"))

# =========================================================================
# 4. Build the 2x2 Psychometric Plot
# =========================================================================
# Custom layout parameters for an elegant figure layout
x_axis_title_size <- 24
y_axis_title_size <- 24
axis_text_size <- 18

tvns_2x2_curve <- ggplot() +
  # A. Faint Individual Subject Fits (Shows variance across your 28 groups)
  geom_line(data = indiv_grid, 
            aes(x = eff_pt, y = predicted_indiv, 
                color = Condition, linetype = Arousal,
                group = interaction(SID, Condition, Arousal)), 
            alpha = 0.05, linewidth = 0.3, show.legend = FALSE) +
  
  # B. Empirical Binned Points (Color matched to condition, shapes matched to arousal)
  geom_point(data = shooting_binned, 
             aes(x = bin, y = prop_success, 
                 color = factor(stim_type, labels = c("Sham", "tVNS")), 
                 shape = factor(pupil_baseline_split, labels = c("Low Phasic Pupil", "High Phasic Pupil")), 
                 size = n), 
             alpha = 0.3) +
  
  # C. Main Model Fixed Effects Lines
  geom_line(data = plot_grid, 
            aes(x = eff_pt, y = predicted, color = Condition, linetype = Arousal), 
            linewidth = 1.8) +
  
  # D. Reference line for 25% Chance Performance
  geom_hline(yintercept = 0.25, linetype = "dotted", color = "gray40", alpha = 0.7) +
  
  # Discrete Mappings & Professional Palette
  scale_color_manual(values = c("Sham" = "#4A5568", "tVNS" = "#DD6B20")) +
  scale_linetype_manual(values = c("Low Phasic Pupil" = "dotted", "High Phasic Pupil" = "solid")) +
  scale_shape_manual(values = c("Low Phasic Pupil" = 16, "High Phasic Pupil" = 17)) +
  scale_size_area(max_size = 6) +
  
  # Set Scales
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(0.15, 0.25), breaks = seq(0, 0.5, 0.1)) +
  
  # Labels and Legends
  labs(
    x = "Preparation Time (s)",
    y = "Probability of Success",
    size = "Trial Count",
    color = "Stimulation",
    linetype = "Phasic State",
    shape = "Phasic State",
    tag = "B."
  ) +
  theme_classic() +
  # Sync shape and linetype under a single unified legend entry
  guides(
    color = guide_legend(order = 1, override.aes = list(linewidth = 2)),
    linetype = guide_legend(order = 2),
    shape = guide_legend(order = 2),
    size = guide_legend(order = 3)
  ) +
  theme(
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold"),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "gray80", fill = "white"),
    axis.title.y = element_text(size = y_axis_title_size, margin = margin(r = 10)),
    axis.title.x = element_text(size = x_axis_title_size, margin = margin(t = 10)),
    axis.text = element_text(size = axis_text_size, color = "black"),
    legend.position = c(0.78, 0.35),
    legend.spacing.y = unit(0.1, "cm"),
    plot.tag = element_text(size = 36, face = "bold", vjust = 1)
  )

# Draw it
print(tvns_2x2_curve)