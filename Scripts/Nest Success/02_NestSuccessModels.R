
#'---
#' title: Nest Success Modeling of Wild Turkeys in the Mid-Atlantic Region
#' authors: "K. Smelter, F. Buderman"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output:
#'   html_document: 
#'     toc: true
#'---
#'
#+ include = FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE, cache = TRUE)
#'  
#' **Purpose**: This script runs Bayesian logistic regression nest success models for wild turkeys in our study


#####################
## Load Packages ##
#####################

#' Vector of package names
packages <- c("R2jags",
              "MCMCvis",
              "dplyr",
              "mcmcplots")


#' Function to load a package or install it if not already installed
load_packages <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    require(package_name, character.only = TRUE)
  }
}

#' Apply the function to each package name
lapply(packages, load_packages)


###########################
## Build Model in JAGS ##
###########################

#' JAGS model specification
#' No Developed because it is highly correlated with agriculture
#' Uninformative Priors
#' Response is nest fate (Succeeded or failed)
nestsucces.mod<- "  
model{
logit(p) <- beta0
  for (i in 1:n.ind) {
  for (t in (first[i]+1):last[i]) {
    fate[i] ~ dbern(p[i])
    logit(p[i]) <- beta0 + elev[i]*beta.elev + dailyprecip[i] * beta.precip + tmax[i] * beta.tmax + visob[i] *beta.visob + percwoody[i] * beta.percwoody + agriculture[i] * beta.agriculture + forest[i] * beta.forest + timespentaway[i] * beta.timespentaway + cumdist[i]* beta.cumdist + alpha[BirdID[i]]  
  }
  
  #Priors
  beta0 ~ dnorm(0, .001) #Prior for Interceot
  beta1 ~ dnorm(0, .001) #Prior for Elevation
  beta2 ~ dnorm(0, .001) #Prior for Daily precipitation
  beta3 ~ dnorm(0, .001) #Prior for Daily Maximum Temperature
  beta4 ~ dnorm(0, .001) #Prior for Visual Obstruction
  beta5 ~ dnorm(0, .001) #Prior for Percent Woody
  beta6 ~ dnorm(0, .001) #Prior for Agriculture
  beta7 ~ dnorm(0, .001) #Prior for Forest
  beta8 ~ dnorm(0, .001) #Prior for Time spent away from nest
  beta9 ~ dnorm(0, .001) #Prior for cumulative distance traveled during recesses
  
   for (k in 1:K){
   alpha[k]~dnorm(0,0.000001)
   }
}
"
#' Data list for JAGS
#' Update this when time comes 
jags_data <- list(
  subset.final.data = nrow(subset.final.data),   # Number of observations
  case = subset.final.data$case,        # Response variable
  elev = subset.final.data$elev # Predictor variable
  )

#' Initialize JAGS
jags_model<- jags.model(textConnection(nestsucces.mod), data = jags_data.laying, n.chains = 3)

#' Burn-in and sampling
jags_samples <- coda.samples(jags_model, 
                                         c("beta0",
                                           "beta.elev", 
                                           "beta.precip",
                                           "beta.tmax",
                                           "beta.visob",
                                           "beta.percwoody",
                                           "beta.agriculture",
                                           "beta.forest",
                                           "beta.timespentaway",
                                           "beta.cumdist"),
                                            n.iter = 10000, 
                                            n.burnin = 2000)


end=Sys.time()
end-start


MCMCtrace(jags_samples, pdf=T)


