
#####################
## Load Packages ##
#####################

require(R2jags)
require(mcmcplots)
require(MCMCvis)
require(ggplot2) 
require(dplyr)
require(rstanarm)

###################
## Read in Data ##
###################

#Read in RDS file with covs 
final.data <- readRDS("Data Management/RData/Individual-Specific Movement Process/Working_SSF_Ready.RDS") %>%
  dplyr::select(BirdID, case_, step_id_,
                elev) 

#Create vector of elevation data
elev.vec <- final.data$elev

#Create identity matrix 
I <- matrix(c(1,0,0,0,0,
              1,0,0,0,0,
              1,0,0,0,0,
              elev.vec), 
            nrow = 4, ncol = 5, byrow=T,)

#Subset final.data to first 100 rows
subset.final.data <- head(final.data, 100)

####################################
## Rstanarm Logistic Regression ##
####################################

# # Bayesian logistic regression model
# log.model <- stan_glm(case_ ~ elev, data = final.data, family = binomial())
# 
# # Print summary of the model
# summary(log.model)
# 
# ################################################
# ## Rstanarm Conditional Logistic Regression ##
# ################################################
# 
# # Bayesian conditional logistic regression model
# con.log.model <- stan_glmer(case_ ~ elev +(1|step_id_)+(1|BirdID), data = final.data, family = binomial())
# 
# # Print summary of the model
# summary(con.log.model)

###########################
## Build model in JAGS ##
###########################

# JAGS model specification
elev.mod <- "  
model{
  for (i in 1: subset.final.data) {
    case[i] ~ dbern(p[i])
    logit(p[i]) <- beta0 + beta1 * elev[i] #Logit link constrains values on 0-1 scale
  }
  
  # Priors
  beta0 ~ dnorm(0, .001) #Prior for interceot
  beta1 ~ dnorm(0, .001) #Prior for elevation
}
"
# Data list for JAGS
jags_data <- list(
  subset.final.data = nrow(subset.final.data),   # Number of observations
  case = subset.final.data$case,        # Response variable
  elev = subset.final.data$elev # Predictor variable
  )

# Initialize JAGS
jags_model <- jags.model(textConnection(elev.mod), data = jags_data, n.chains = 3)

# Burn-in and sampling
jags_samples <- coda.samples(jags_model, c("beta0", "beta1"), n.iter = 10000,  n.burnin = 2000)

# Summary of posterior samples
summary(jags_samples)

#Use MCMCsummary to pull off posterior means
bayesests <- MCMCpstr(jags_samples, params = c("beta0", "beta1"), func = mean)
bayesests

#Use MCMCsummary to pull of upper and lower 95% credible interval limits
bayesci <-  MCMCpstr(jags_samples, params = c("beta0", "beta1"), 
                     func = function(x) quantile(x, probs = c(0.025, 0.975)))
bayesci
