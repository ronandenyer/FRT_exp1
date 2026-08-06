library(nlme)
library(ggplot2)
library(dplyr)
library(ggrepel) # For non-overlapping text labels
# nonlinear mixed effect model
# the model has there parameters: mu, sigma, and the asymptote, we set the chance to 1/8 as there are # eight targets. You can include chance as a parameter if needed.
# the “start" statement is the initial value of those three parameters for each group for optimization, so there are nine # # values.
shooting_tvns = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv')
shooting_tvns$condition = shooting_tvns$stim_type
shooting_reward = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')
shooting_reward$condition = shooting_reward$reward_type + 2


shooting <- bind_rows(
  tvns   = shooting_tvns,
  reward = shooting_reward,
  .id = "experiment"
)

shooting = shooting[shooting$SID != "FRTP31",]

shooting$condition = as.factor(shooting$condition)
shooting$condition = relevel(shooting$condition, ref = 3)
shooting = shooting[shooting$eff_pt > 0 & !is.nan(shooting$eff_pt),]
shooting$eff_pt = shooting$eff_pt/1000
shooting$eff_pt = shooting$eff_pt - 0.02

shooting.nlme <- nlme(
  # Replace 1/4 with gamma
  success ~ (gamma + (asym - gamma) * pnorm(eff_pt, mu, sigma)),
  
  # Add gamma to the fixed effects list
  # We'll start by assuming gamma is the same for both reward types (gamma ~ 1)
  # because estimating two asymptotes can make the model very unstable.
  fixed = list(mu ~ condition, 
               sigma ~ condition, 
               asym ~ condition, 
               gamma ~ 1), 
  
  random = mu + sigma + asym  ~ 1 | SID,
  data = shooting,
  
  # Update start values: you now need 7 values 
  # (2 for mu, 2 for sigma, 2 for asym, and 1 for gamma)
  start = list(fixed = c(0.2, 0, 0, 0,  # mu (Intercept, diff)
                         0.02, 0, 0, 0, # sigma (Intercept, diff)
                         0.95, 0, 0, 0,  # asym (Intercept, diff)
                         0.25))       # gamma (the guess rate)
)

summary(shooting.nlme)
# output statistical value for test effects
anova(shooting.nlme)
# ways to output model coefficients
get_param_contrasts <- function(model, param = c("mu", "sigma")) {
  param <- match.arg(param)
  b <- fixef(model)
  V <- vcov(model)
  
  make_L <- function(cond) {
    L <- setNames(rep(0, length(b)), names(b))
    L[paste0(param, ".(Intercept)")] <- 1
    if (cond != 3) {
      L[paste0(param, ".condition", cond)] <- 1
    }
    L
  }
  
  conds <- c(1, 2, 3, 4)
  
  # estimated parameter value in each condition
  L_est <- do.call(rbind, lapply(conds, make_L))
  rownames(L_est) <- paste0("cond", conds)
  
  est <- as.vector(L_est %*% b)
  se  <- sqrt(diag(L_est %*% V %*% t(L_est)))
  estimates <- data.frame(
    condition = rownames(L_est),
    estimate = est,
    SE = se,
    lower = est - 1.96 * se,
    upper = est + 1.96 * se
  )
  
  # pairwise contrasts
  pairs <- combn(conds, 2, simplify = FALSE)
  L_pair <- do.call(rbind, lapply(pairs, function(x) make_L(x[1]) - make_L(x[2])))
  rownames(L_pair) <- sapply(pairs, function(x) paste0("cond", x[1], " - cond", x[2]))
  
  est2 <- as.vector(L_pair %*% b)
  se2  <- sqrt(diag(L_pair %*% V %*% t(L_pair)))
  z2   <- est2 / se2
  p2   <- 2 * (1 - pnorm(abs(z2)))
  
  contrasts <- data.frame(
    contrast = rownames(L_pair),
    estimate = est2,
    SE = se2,
    z = z2,
    p = p2,
    p_holm = p.adjust(p2, method = "holm"),
    lower = est2 - 1.96 * se2,
    upper = est2 + 1.96 * se2
  )
  
  list(estimates = estimates, contrasts = contrasts)
}

mu_out <- get_param_contrasts(shooting.nlme, "mu")
sigma_out <- get_param_contrasts(shooting.nlme, "sigma")

mu_out$estimates
mu_out$contrasts

sigma_out$estimates
sigma_out$contrasts

library(nlme)
library(ggplot2)
library(dplyr)

# Extract fixed effects from the fitted nlme model
b <- fixef(shooting.nlme)

# Get condition-specific mu values
mu_vals <- c(
  `1` = b["mu.(Intercept)"] + b["mu.condition1"],
  `2` = b["mu.(Intercept)"] + b["mu.condition2"],
  `3` = b["mu.(Intercept)"],
  `4` = b["mu.(Intercept)"] + b["mu.condition4"]
)

# Get condition-specific sigma values
sigma_vals <- c(
  `1` = b["sigma.(Intercept)"] + b["sigma.condition1"],
  `2` = b["sigma.(Intercept)"] + b["sigma.condition2"],
  `3` = b["sigma.(Intercept)"],
  `4` = b["sigma.(Intercept)"] + b["sigma.condition4"]
)

# Put parameter estimates into a data frame with readable labels
params <- data.frame(
  condition = factor(
    c(1, 2, 3, 4),
    levels = c(1, 2, 3, 4),
    labels = c("Sham", "tVNS", "No Reward", "Reward")
  ),
  mu = as.numeric(mu_vals),
  sigma = as.numeric(sigma_vals)
)

# Create x-axis range based on all estimated distributions
x_grid <- seq(
  min(params$mu - 4 * params$sigma),
  max(params$mu + 4 * params$sigma),
  length.out = 1000
)

# Build data for overlapping Gaussian curves
plot_df <- bind_rows(lapply(1:nrow(params), function(i) {
  data.frame(
    eff_pt = x_grid,
    density = dnorm(x_grid, mean = params$mu[i], sd = params$sigma[i]),
    condition = params$condition[i]
  )
}))

# Plot overlapping Gaussian distributions
ggplot(plot_df, aes(x = eff_pt, y = density, color = condition, fill = condition)) +
  geom_area(alpha = 0.15, position = "identity") +
  geom_line(linewidth = 1.2) +
  geom_vline(
    data = params,
    aes(xintercept = mu, color = condition),
    linetype = "dashed",
    linewidth = 0.8,
    show.legend = FALSE
  ) +
  labs(
    x = "Preparation Time (s)",
    y = "Gaussian Density",
    color = "Condition",
    fill = "Condition",
    title = "Estimated Gaussian Distributions by Condition"
  ) +
  theme_classic(base_size = 14)