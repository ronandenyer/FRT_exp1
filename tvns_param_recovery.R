library(nlme)
library(ggplot2)
library(ggrepel) # For non-overlapping text labels
library(dplyr)
# nonlinear mixed effect model
# the model has there parameters: mu, sigma, and the asymptote, we set the chance to 1/8 as there are # eight targets. You can include chance as a parameter if needed.
# the “start" statement is the initial value of those three parameters for each group for optimization, so there are nine # # values.
shooting = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')
shooting$stim_type = as.factor(shooting$stim_type)
shooting$stim_type = relevel(shooting$stim_type, ref = 1)
shooting = shooting[shooting$eff_pt > 0 & !is.nan(shooting$eff_pt),]
shooting$eff_pt = shooting$eff_pt/1000

shooting.nlme <- nlme(
  # Replace 1/4 with gamma
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  # Add gamma to the fixed effects list
  # We'll start by assuming gamma is the same for both reward types (gamma ~ 1)
  # because estimating two asymptotes can make the model very unstable.
  fixed = list(mu ~ stim_type, 
               sigma ~ stim_type, 
               asym ~ stim_type, 
               gamma ~ 1), 
  
  random = mu  ~ 1 | SID,
  data = shooting,
  
  # Update start values: you now need 7 values 
  # (2 for mu, 2 for sigma, 2 for asym, and 1 for gamma)
  start = list(fixed = c(0.2, -0.01,  # mu (Intercept, diff)
                         0.02, 0.007, # sigma (Intercept, diff)
                         0.95, 0.01,  # asym (Intercept, diff)
                         0.25))       # gamma (the guess rate)
)


# 1. Define 'True' parameters (Use the means from your real data)
true_mu <- 0.2
true_sigma <- 0.024
true_asym <- 0.95
true_gamma <- 0.25 

# 2. Setup simulation environment
iterations <- 50
results <- data.frame()

# Use the eff_pt distribution from your real data so the 'difficulty' range is identical
real_eff_pt <- shooting$eff_pt 

for(i in 1:iterations) {
  # Simulate success based on the Psychometric Function (Probit)
  # success = P(correct) = gamma + (asym - gamma) * pnorm(x, mu, sigma)
  prob_correct <- true_gamma + (true_asym - true_gamma) * pnorm(real_eff_pt, true_mu, true_sigma)
  
  # Bernoulli trial: Success is 1 if random draw < prob
  sim_success <- rbinom(length(prob_correct), 1, prob_correct)
  
  # Create temp dataframe
  sim_data <- data.frame(success = sim_success, eff_pt = real_eff_pt, SID = shooting$SID)
  
  # 3. Fit the model to the synthetic data
  # Note: Use your exact fitting function here
  fit <- try(nlme(
    success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
    fixed = list(mu ~ 1, sigma ~ 1, asym ~ 1, gamma ~ 1),
    random = mu ~ 1 | SID,
    data = sim_data,
    start = list(fixed = c(0.2, 0.024, 0.95, 0.25))
  ), silent = TRUE)
  
  if(!inherits(fit, "try-error")) {
    params <- fixef(fit)
    results <- rbind(results, data.frame(iter = i, 
                                         rec_mu = params[1], 
                                         rec_sigma = params[2], 
                                         rec_asym = params[3]))
  }
}

# Plot True vs Recovered for Sigma (your main concern)
plot(rep(true_sigma, nrow(results)), results$rec_sigma, 
     xlab = "True Sigma", ylab = "Recovered Sigma",
     main = "Parameter Recovery: Sigma")
abline(0, 1, col = "red", lwd = 2) # Perfect recovery line