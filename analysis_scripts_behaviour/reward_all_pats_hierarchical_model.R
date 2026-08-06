library(nlme)
library(ggplot2)
library(gridExtra)
library(dplyr)

# nonlinear mixed effect model
# the model has there parameters: mu, sigma, and the asymptote, we set the chance to 1/8 as there are # eight targets. You can include chance as a parameter if needed.
# the “start" statement is the initial value of those three parameters for each group for optimization, so there are nine # # values.
shooting = read.csv('C://Users//denyer//OneDrive - UCL//FRT_data//Reward_experiment//all_pats_reward_hit_data.csv')
shooting$reward_type = as.factor(shooting$reward_type)
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
  fixed = list(mu ~ reward_type, 
               sigma ~ reward_type, 
               asym ~ reward_type, 
               gamma ~ 1), 
  
  random = mu + sigma ~ 1| SID,
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
  fixed = list(mu ~ reward_type, 
               sigma ~ reward_type, 
               asym ~ reward_type, 
               gamma ~ reward_type), 
  
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

