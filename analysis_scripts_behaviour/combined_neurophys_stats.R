library(lmerTest)
library(dplyr)
library(emmeans)
library(dplyr)

shooting_tvns <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')
shooting_reward <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')

# Add the 'condition' mapping and a source tracker
shooting_tvns$condition <- shooting_tvns$stim_type
shooting_tvns$experiment_source <- "tVNS_Session"

shooting_reward$condition <- shooting_reward$reward_type
shooting_reward$experiment_source <- "Reward_Session"

# Find common columns 
common_cols <- intersect(names(shooting_tvns), names(shooting_reward))

# Bind them, explicitly forcing dplyr namespace
shooting_combined <- bind_rows(
  shooting_tvns %>% dplyr::select(all_of(common_cols)),
  shooting_reward %>% dplyr::select(all_of(common_cols))
)


# Remove NAs
shooting_combined <- shooting_combined[!is.na(shooting_combined$eff_pt) & 
                                         !is.na(shooting_combined$success) & 
                                         !is.na(shooting_combined$pupil_size_mean), ]

# Standardize units (Fixed double division of eff_pt)
shooting_combined$eff_pt <- shooting_combined$eff_pt / 1000 # Convert to seconds
shooting_combined$pupil_size_phasic_mean_norm <- shooting_combined$pupil_size_phasic_mean_norm * 1e4
shooting_combined$pupil_size_phasic_mean <- shooting_combined$pupil_size_phasic_mean * 1e4
shooting_combined$pupil_size_pre_phasic_mean_norm <- shooting_combined$pupil_size_pre_phasic_mean_norm * 1e4
shooting_combined$pupil_size_pre_phasic_mean <- shooting_combined$pupil_size_pre_phasic_mean * 1e4
shooting_combined$pupil_size_phasic_slope <- shooting_combined$pupil_size_phasic_slope * 1e4
shooting_combined$pupil_size_max <- shooting_combined$pupil_size_max * 1e4

shooting_combined$pupil_size_mean <- shooting_combined$pupil_size_mean * 1e4
shooting_combined$condition = as.factor(shooting_combined$condition)
shooting_combined$condition <- factor(shooting_combined$condition, levels = c("3", "1", "2", "4"))
# -------------------------------------------------------------------------
# 2. WITHIN-SUBJECT CENTERING & SCALING
# -------------------------------------------------------------------------
shooting_combined <- shooting_combined %>%
  group_by(SID) %>%
  mutate(
    pupil_state = pupil_size_mean - mean(pupil_size_mean, na.rm = TRUE),
    pupil_phasic = pupil_size_phasic_mean_norm - mean(pupil_size_phasic_mean_norm, na.rm = TRUE),
    pupil_pre_phasic = pupil_size_pre_phasic_mean_norm - mean(pupil_size_pre_phasic_mean_norm, na.rm = TRUE),
    pupil_pre_phasic_raw = pupil_size_pre_phasic_mean - mean(pupil_size_pre_phasic_mean, na.rm = TRUE),
    
    pupil_slope = pupil_size_phasic_slope - mean(pupil_size_phasic_slope, na.rm = TRUE),
    pupil_max = pupil_size_max - mean(pupil_size_max, na.rm = TRUE),
    pupil_trait = mean(pupil_size_mean, na.rm = TRUE),
    vigor_wc = peak_tanVel - mean(peak_tanVel, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    pupil_state_scaled = pupil_state / sd(pupil_state, na.rm = TRUE),
    pupil_phasic_scaled = pupil_phasic / sd(pupil_phasic, na.rm = TRUE),
    pupil_pre_phasic_scaled = pupil_pre_phasic / sd(pupil_pre_phasic, na.rm = TRUE),
    
    pupil_slope_scaled = pupil_slope / sd(pupil_slope, na.rm = TRUE),
    pupil_max_scaled = pupil_max / sd(pupil_max, na.rm = TRUE),
    vigor_scaled = vigor_wc / sd(vigor_wc, na.rm = TRUE)
  )


# Trimmed dataset (Generated but held in reserve, models use 'shooting')
shooting_combined_trimmed <- shooting_combined %>%
  filter(
    abs(pupil_state_scaled) <= 3,
    abs(pupil_pre_phasic_scaled) <= 3,
    abs(pupil_slope_scaled) <= 3,
    abs(pupil_max_scaled) <= 3,
    abs(vigor_scaled) <= 3
  )

# Test whether baseline pupil (task engagement) differed between experiments
global_baseline_model <- lmer(
  pupil_size_mean ~ experiment_source + (1 + experiment_source | SID), 
  data = shooting_combined_trimmed
)

# 4. Check the overall main effect of condition
summary(global_baseline_model)


# Now check whether prev_trial_success (0 = miss, 1 = hit) differed between experiments
pupil_state_lmer = lmer(pupil_size_mean ~ prev_trial_success*experiment_source + 
                          (1 + experiment_source | SID),
                        data = shooting_combined_trimmed)

summary(pupil_state_lmer)

# now check how much pupil size changes post reward/no reward/sham/tvns

pupil_pre_phasic_lmer = lmer(pupil_size_pre_phasic_mean_norm ~ condition + 
                          (1 + condition | SID),
                        data = shooting_combined_trimmed)

summary(pupil_pre_phasic_lmer)


# Load the package
library(emmeans)

# 1. Calculate the estimated marginal means for 'condition'
pupil_emmeans <- emmeans(pupil_pre_phasic_lmer, specs = ~ condition)

# Print the means, standard errors, and 95% confidence intervals
print(pupil_emmeans)

# 2. Run pairwise comparisons (contrasts) to see if the conditions are significantly different
pupil_contrasts <- pairs(pupil_emmeans)

# Print the p-values for the differences 
# (Tukey adjustment is applied by default if there are more than 2 conditions)
summary(pupil_contrasts, adjust = "tukey")


# Now check vigor pupil coreelations
# first a pre_trial pupil

pupil_state_vigor_lmer = lmer(vigor_wc ~ pupil_state*condition + 
                               (1  | SID),
                             data = shooting_combined_trimmed)

summary(pupil_state_vigor_lmer)


# see how drop in pupil_state across trials and blocks differs between days

m_pupil_evolution = lmer(pupil_state ~ trial_num*experiment_source + block_num*experiment_source +
                                (1  | SID),
                              data = shooting_combined_trimmed)

summary(m_pupil_evolution)

library(ggplot2)
library(dplyr)

# 1. Extract fixed-effect coefficients
beta <- fixef(m_pupil_evolution)

# 2. Build grid for trial_num across experiment sources
grid_trial <- expand.grid(
  trial_num = seq(
    min(shooting_combined_trimmed$trial_num, na.rm = TRUE),
    max(shooting_combined_trimmed$trial_num, na.rm = TRUE),
    length.out = 100
  ),
  experiment_source = c("Baseline", "tVNS_Session"),
  block_num = 0 # Held at baseline/reference
)

# 3. Calculate predicted pupil_state
grid_trial <- grid_trial %>%
  mutate(
    pupil_pred = case_when(
      experiment_source == "Baseline" ~ 
        beta["(Intercept)"] + (beta["trial_num"] * trial_num),
      
      experiment_source == "tVNS_Session" ~ 
        (beta["(Intercept)"] + beta["experiment_sourcetVNS_Session"]) + 
        ((beta["trial_num"] + beta["trial_num:experiment_sourcetVNS_Session"]) * trial_num)
    )
  )

# 4. Generate Plot
ggplot(grid_trial, aes(x = trial_num, y = pupil_pred, color = experiment_source)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("Baseline" = "#2b5c8f", "tVNS_Session" = "#d95f02")) +
  labs(
    title = "Within-Block Pupil Dynamics Across Trials",
    subtitle = "tVNS Session shows a significantly steeper within-block habituation/decline in pupil state.",
    x = "Trial Number (Within Block)",
    y = "Predicted Pupil State (Tonic Arousal)",
    color = "Experiment Source"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

# 1. Build grid for block_num across experiment sources
grid_block <- expand.grid(
  block_num = seq(
    min(shooting_combined_trimmed$block_num, na.rm = TRUE),
    max(shooting_combined_trimmed$block_num, na.rm = TRUE),
    length.out = 100
  ),
  experiment_source = c("Baseline", "tVNS_Session"),
  trial_num = 0 # Held at baseline/reference
)

# 2. Calculate predicted pupil_state
grid_block <- grid_block %>%
  mutate(
    pupil_pred = case_when(
      experiment_source == "Baseline" ~ 
        beta["(Intercept)"] + (beta["block_num"] * block_num),
      
      experiment_source == "tVNS_Session" ~ 
        (beta["(Intercept)"] + beta["experiment_sourcetVNS_Session"]) + 
        ((beta["block_num"] + beta["experiment_sourcetVNS_Session:block_num"]) * block_num)
    )
  )

# 3. Generate Plot
ggplot(grid_block, aes(x = block_num, y = pupil_pred, color = experiment_source)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("Baseline" = "#2b5c8f", "tVNS_Session" = "#d95f02")) +
  labs(
    title = "Across-Block Fatigue & Cumulative Pupil Drift",
    subtitle = "Baseline session shows minimal block-to-block change, whereas tVNS session shows a massive cumulative drop.",
    x = "Block Number",
    y = "Predicted Pupil State (Tonic Arousal)",
    color = "Experiment Source"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))