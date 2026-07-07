library(dplyr)
library(nlme)
library(car)
# =========================================================================
# 1. Load Both Datasets Freshly
# =========================================================================
df_tvns   <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')
df_reward <- read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')

# Ensure Subject IDs are characters/factors to avoid type mismatches during merging
df_tvns$SID   <- as.character(df_tvns$SID)
df_reward$SID <- as.character(df_reward$SID)

# =========================================================================
# 2. Intersect and Merge by Common Columns Only
# =========================================================================
# Find columns present in BOTH dataframes
common_cols <- intersect(names(df_tvns), names(df_reward))

# Subset and bind rows cleanly
shooting_combined <- rbind(df_tvns[, common_cols], df_reward[, common_cols])
library(dplyr)

# =========================================================================
# 1. Build the 2x2 LC State Matrix
# =========================================================================
shooting_combined <- shooting_combined %>%
  # Ensure we only use rows with valid data for both measures
  filter(!is.na(pupil_size_mean), !is.na(pupil_size_phasic_mean_norm)) %>%
  group_by(SID) %>%
  mutate(
    # Within-subject median split for Tonic Baseline
    tonic_split = ifelse(pupil_size_mean <= median(pupil_size_mean, na.rm = TRUE), 
                         "Low Tonic", "High Tonic"),
    
    # Within-subject median split for Phasic Burst
    phasic_split = ifelse(pupil_size_phasic_mean_norm <= median(pupil_size_phasic_mean_norm, na.rm = TRUE), 
                          "Low Phasic", "High Phasic"),
    
    # Combine into a single 4-level factor
    lc_state = paste(tonic_split, phasic_split, sep = " / "),
    
    # Explicitly set factor levels so the model uses State A as the intercept
    lc_state = factor(lc_state, levels = c(
      "Low Tonic / Low Phasic",    # State A (Intercept)
      "Low Tonic / High Phasic",   # State B (The Goldilocks State)
      "High Tonic / Low Phasic",   # State C (The Stressed/Suppressed State)
      "High Tonic / High Phasic"   # State D (The Danger Zone)
    ))
  ) %>%
  ungroup()

shooting_combined$eff_pt = shooting_combined$eff_pt/1000

# =========================================================================
# 2. Diagnostic: Global Trial Counts
# =========================================================================
cat("\n--- Global Trial Counts per LC State ---\n")
print(table(shooting_combined$lc_state))

# =========================================================================
# 3. Diagnostic: Subject-Level Viability for nlme
# =========================================================================
cat("\n--- Trial Viability per Subject ---\n")
# This checks if any specific subject lacks enough data in a specific cell
# to physically fit an individual SAT curve.
cell_diagnostics <- shooting_combined %>%
  group_by(SID, lc_state) %>%
  summarize(n_trials = n(), .groups = "drop") %>%
  group_by(lc_state) %>%
  summarize(
    absolute_min_trials = min(n_trials), 
    mean_trials = round(mean(n_trials), 1),
    subjects_under_40_trials = sum(n_trials < 40)
  )
print(cell_diagnostics)
# =========================================================================
# Robust 2x2 LC State Model
# =========================================================================
nlme_combined_2x2 <- nlme(
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  fixed = list(mu ~ lc_state, 
               sigma ~ lc_state, 
               asym ~ 1,      # Lock asym globally to stabilize
               gamma ~ 1), 
  
  # Simplest stable random structure to force convergence
  random = mu ~ 1 | SID,
  
  data = shooting_combined,
  na.action = na.omit, 
  
  # Adjusted starting values:
  # mu Intercept: 0.20, sigma Intercept: 0.03, asym: 0.95, gamma: 0.24
  start = list(fixed = c(
    0.20, 0, 0, 0,  # mu: Intercept + 3 contrasts
    0.03, 0, 0, 0,  # sigma: Intercept + 3 contrasts
    0.95,           # asym: Global
    0.24            # gamma: Global
  )),
  
  control = nlmeControl(maxIter = 1000, msMaxIter = 1000, niterEM = 1000)
)

summary(nlme_combined_2x2)
anova(nlme_combined_2x2)

emmeans(nlme_combined_2x2, pairwise ~ lc_state, param = "mu")
