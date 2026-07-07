# nonlinear mixed effect model
# the model has there parameters: mu, sigma, and the asymptote, we set the chance to 1/4 as there are # eight targets. You can include chance as a parameter if needed.
# the “start" statement is the initial value of those three parameters for each group for optimization, so there are nine # # values.
shooting = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')
shooting$stim_type = as.factor(shooting$stim_type)
shooting$stim_type = relevel(shooting$stim_type, ref = 1)
shooting = shooting[!is.na(shooting$eff_pt),]
shooting = shooting[!is.na(shooting$success),]

shooting = shooting[!is.nan(shooting$pupil_size_phasic_mean),]
shooting$pupil_size_phasic_mean_norm = shooting$pupil_size_phasic_mean_norm * 1e4
shooting$pupil_size_phasic_mean = shooting$pupil_size_phasic_mean * 1e4
shooting$pupil_size_phasic_slope = shooting$pupil_size_phasic_slope * 1e4
shooting$pupil_size_mean = shooting$pupil_size_mean * 1e4

shooting$eff_pt = shooting$eff_pt/1000



# 1. Create a true Within-Subject Centered Baseline (The Fix)
shooting <- shooting %>%
  group_by(SID) %>%
  mutate(pupil_size_phasic_center = pupil_size_mean - mean(pupil_size_mean, na.rm = TRUE)) %>%
  ungroup()

# 1. Define a wrapper function that forces sigma to be positive
safe_pnorm <- function(x, mu, sigma) {
  # pmax ensures sigma never drops below 0.0001, preventing NaNs
  safe_sigma <- pmax(sigma, 1e-4) 
  pnorm(x, mean = mu, sd = safe_sigma)
}

# 1. Define a wrapper function that forces sigma to be positive
safe_pnorm <- function(x, mu, sigma) {
  # pmax ensures sigma never drops below 0.0001, preventing NaNs
  safe_sigma <- pmax(sigma, 1e-4) 
  pnorm(x, mean = mu, sd = safe_sigma)
}

# 1. Scale the centered predictor to fix the optimizer matrix
shooting$pupil_phasic_center_scaled <- shooting$pupil_size_phasic_center / sd(shooting$pupil_size_phasic_center, na.rm = TRUE)

# 2. Run the model using the scaled predictor
nlme_tvns_pupil_continuous <- nlme(
  success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
  
  fixed = list(
    # Use the newly scaled predictor here
    mu ~ stim_type * pupil_phasic_center_scaled,   
    sigma ~ 1,                    
    asym ~ 1,
    gamma ~ 1                     
  ), 
  
  random = mu ~ 1 | SID,
  
  data = shooting,
  
  start = list(fixed = c(
    #--- mu parameters ---
    0.199,  
    0.000,  
    0.000,  # The optimizer can now actually calculate this slope
    0.000,  
    
    #--- other parameters ---
    0.023,  
    0.954,  
    0.254   
  )),
  
  control = nlmeControl(opt = "nlm", pnlsMaxIter = 300, msMaxIter = 300)
)

summary(nlme_tvns_pupil_continuous)
anova(nlme_tvns_pupil_continuous, type = "marginal")


library(nlme)

# 1. Define the subjects and storage
subjects <- unique(shooting$SID)
stability_results_nlme <- data.frame(
  Excluded_SID = character(),
  Est_Interaction = numeric(),
  P_Interaction = numeric(),
  stringsAsFactors = FALSE
)

# 2. Run the loop with error handling
for (sub in subjects) {
  cat("Running LOSO for SID:", sub, "\n")
  temp_data <- shooting[shooting$SID != sub, ]
  
  # Try to fit the model; if it fails, return NULL
  fit <- tryCatch({
    nlme(
      success ~ (gamma + (asym - gamma) * safe_pnorm(eff_pt, mu, sigma)),
      fixed = list(mu ~ stim_type * pupil_phasic_center_scaled, sigma ~ 1, asym ~ 1, gamma ~ 1),
      random = mu ~ 1 | SID,
      data = temp_data,
      start = list(fixed = c(0.199, 0.000, 0.000, 0.000, 0.023, 0.954, 0.254)),
      control = nlmeControl(opt = "nlm", pnlsMaxIter = 100, msMaxIter = 100)
    )
  }, error = function(e) return(NULL))
  
  # If fit succeeded, extract the p-value
  if (!is.null(fit)) {
    summary_fit <- summary(fit)
    coefs <- summary_fit$tTable
    # Extract the interaction row
    p_val <- coefs["mu.stim_type2:pupil_phasic_center_scaled", "p-value"]
    est <- coefs["mu.stim_type2:pupil_phasic_center_scaled", "Value"]
    
    stability_results_nlme <- rbind(stability_results_nlme, data.frame(
      Excluded_SID = as.character(sub),
      Est_Interaction = est,
      P_Interaction = p_val
    ))
  } else {
    cat("Model failed to converge for SID:", sub, "\n")
  }
}

print(stability_results_nlme)

print(stability_results)
# Plot the distribution of p-values to see if any specific subject is a "driver"
hist(stability_results$P_3way, main="Distribution of P-values (Leave-One-Out)", xlab="P-value")



library(ggplot2)
library(dplyr)

# 1. Extract the fixed effects from your model
coefs <- fixef(nlme_tvns_pupil_continuous)

# 2. Define the exact equations for the mu slopes based on the coefficients
# Sham: Intercept + (Pupil Main Effect * pupil)
intercept_sham <- coefs["mu.(Intercept)"]
slope_sham <- coefs["mu.pupil_phasic_center_scaled"]

# tVNS: (Intercept + Stim Main Effect) + (Pupil Main Effect + Interaction) * pupil
intercept_tvns <- coefs["mu.(Intercept)"] + coefs["mu.stim_type2"]
slope_tvns <- coefs["mu.pupil_phasic_center_scaled"] + coefs["mu.stim_type2:pupil_phasic_center_scaled"]

# 3. Create a clean sequence of scaled pupil sizes (e.g., -3 SD to +3 SD)
pupil_range <- seq(-3, 3, length.out = 100)

# 4. Calculate predicted mu values
df_plot <- data.frame(
  pupil_scaled = rep(pupil_range, 2),
  Condition = rep(c("Sham", "Active tVNS"), each = 100),
  predicted_mu = c(
    intercept_sham + slope_sham * pupil_range,
    intercept_tvns + slope_tvns * pupil_range
  )
)

# 5. Plot it
ggplot(df_plot, aes(x = pupil_scaled, y = predicted_mu, color = Condition)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("Sham" = "#4A5568", "Active tVNS" = "#DD6B20")) +
  labs(
    title = "tVNS Decouples Decision Threshold from Arousal State",
    subtitle = "Higher phasic pupil drives urgency in Sham, but not under tVNS",
    x = "Trial-by-Trial Phasic Pupil Size (Within-Subject Scaled)",
    y = expression("Predicted Decision Threshold (" * mu * ")")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )