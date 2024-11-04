
####################
## MCMC Example ##
####################

require(R2jags)
require(mcmcplots)
require(MCMCvis)
require(ggplot2) 
require(ggthemes)

males<-c(120, 107, 110, 116, 114, 111, 113, 117, 114, 112)
females<-c(110, 111, 107, 108, 110, 105, 107, 106, 111, 111)

jaw.mod<-function(){
  
  # Priors 
  mu.male ~ dnorm(100, 0.001) # mean of male jaw lengths
  mu.female ~ dnorm(100, 0.001) # mean of female jaw lengths
  sigma ~ dunif(0, 30) # common sigma
  tau <- 1/(sigma*sigma) #precision
  
  # Likelihood (Y | mu) = normal(mu[gender], sigma^2)  
  for(i in 1:nmales){
    males[i] ~ dnorm(mu.male, tau) 
  }
  for(i in 1:nfemales){
    females[i] ~ dnorm(mu.female, tau)
  }
  
  # Derived quantities:  difference in means
  mu.diff <- mu.male - mu.female
}

# Function to generate initial values
init.vals<-function(){
  mu.male <- rnorm(1, 100, 100)
  mu.female <- rnorm(1, 100, 100)
  sigma <- runif(1, 0, 10) 
  out <- list(mu.male = mu.male, mu.female = mu.female, sigma = sigma)
}

#' Create rest of the data for the model
nmales<-length(males)
nfemales<-length(females)

t.test.jags <- jags(data=c("males", "females", "nmales",  "nfemales"),
                    inits = init.vals,
                    parameters.to.save = c("mu.male", "mu.female", "sigma", "mu.diff"), 
                    progress.bar = "none",
                    n.iter = 10000, #specifies the total number of samples we want to generate
                    n.burnin = 5000, #specifies that we want to throw away the first 5000 samples
                    model.file = jaw.mod, #specifies the function containing the model specification
                    n.thin = 1, #specifies that we want to keep all of the samples. We can save memory by saving say every other sample if we change this to n.thin = 2. If the chains are highly autocorrelated, we won’t loose much information by keeping every other sample.
                   n.chains = 3) #specifies that we want to generate 3 Markov chains, each generated with a different set of starting values.

t.test.jags #View outputs

#View results of select parameters
MCMCvis::MCMCsummary(t.test.jags, params = c("mu.male", "mu.female"))

#Use MCMCsummary to pull off posterior means
bayesests <- MCMCpstr(t.test.jags, params = c("mu.male", "mu.female"), func = mean)
bayesests

#Use MCMCsummary to pull of upper and lower 95% credible interval limits
bayesci <-  MCMCpstr(t.test.jags, params = c("mu.male", "mu.female"), 
                     func = function(x) quantile(x, probs = c(0.025, 0.975)))
bayesci



#Fit linear model in frequentist framework
jawdat<-data.frame(jaws=c(males, females), 
                   sex=c(rep("Male", length(males)), 
                         rep("Female", length(females))))
lm.jaw<-lm(jaws~sex-1, data=jawdat)
#Store results
betaf <- coef(lm.jaw)
CIf <-confint(lm.jaw) 
ests<-data.frame(estimate = c(bayesests$mu.female, bayesests$mu.male, betaf), 
                 LCL = c(bayesci$mu.female[1],   bayesci$mu.male[1], CIf[,1]), 
                 UCL = c(bayesci$mu.female[2],   bayesci$mu.male[2], CIf[,2]), 
                 param = c("Mean females", "Mean males"),
                 Method = rep(c("Bayesian", "Frequentist"), each = 2))

ggplot(ests, aes(param,estimate, col = Method)) + 
  geom_point(position = position_dodge(width = 0.2))+ 
  geom_pointrange(aes(ymin = LCL, ymax= UCL), position = position_dodge(width = 0.2))+
  ylab("Estimate") + xlab("") +
  scale_x_discrete(labels = c('Mean females' = expression(mu[f]),
                              'Mean males'   = expression(mu[m]))) + 
  ggthemes::scale_colour_colorblind()+
  theme(text = element_text(size = 20))
