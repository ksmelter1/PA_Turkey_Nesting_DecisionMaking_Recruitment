
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
#' **Purpose**: This script fits a Bayesian known fate model for female wild turkeys in our study
#' 

#####################
## Load Packages ##
#####################

#' Vector of package names
packages <- c("R2jags",
              "MCMCvis",
              "mcmcplots",
              "tidyverse")

#' Function to load a package or install it if not already installed
load_packages <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    require(package_name, character.only = TRUE)
  }
}

#' Apply the function to each package name
lapply(packages, load_packages)

################
## Data Prep ##
################

#' Following Heather Gaya's tutorial 
#' https://github.com/heathergaya/JAGS-NIMBLE-Tutorials/blob/master/Known_Fate/Known_Fate_Models.Rmd
#' Load in subset of nesting data
#' Check structure 
hens.nests <- read.csv("Csvs/NestAttempts_nodigits_KyleTest.csv") 
glimpse(hens.nests)

#' Rename index column to BirdID 
#' Check structure
colnames(hens.nests)[1] <- "BirdID"
glimpse(hens.nests)

#' Convert start and end dates from characters to date objects
#' Check date format
hens.nests$start.date <- as.Date(hens.nests$start.date, format = "%Y-%m-%d")
hens.nests$end.date <- as.Date(hens.nests$end.date, format = "%Y-%m-%d")
glimpse(hens.nests)

#' Simulate nest fate column in hens.nests df
#' In the process of linking the observations by nestid but not needed for this exercise
#' 1 = At least one egg hatched from a nest (Success)
#' 0 = Nest failed 
#' 44 nests
set.seed(123)
nest_fate <- rbinom(46, 1, 0.3)
hens.nests$nest_fate <- nest_fate
glimpse(hens.nests)

#' Remove rows with NA values in the start and end date columns 
#' Summary check to see if NA values remain
#' Check structure of new df
hens.nests.ready <- tidyr::drop_na(hens.nests)
summary(hens.nests.ready)
glimpse(hens.nests.ready)

#' Create incubation dates object
#' Occurrences object is just the length of the incubation dates
#' We need a list of every day between our start and end period
#' 147 encounter days is the output for occs
inc.dates <- sort(unique(c(hens.nests.ready$start.date, hens.nests.ready$end.date)))
inc.dates <- seq(inc.dates[1],inc.dates[length(inc.dates)], by = 1)
occs <- length(inc.dates)

#' Create Encounter History
#' Loops through each individual and extracts information
#' First = The first day the bird began incubating (Start.Date)
#' Last = The last day before termination of the nesting attempt 
#' Surv.Caps = The encounter history for each individual 
first <- last <- array(NA, dim = nrow(hens.nests.ready))
surv.caps <-  matrix(data = NA, nrow = nrow(hens.nests.ready), ncol = occs) 
for(i in 1:nrow(hens.nests.ready)){ 
  first[i] <- which(inc.dates == hens.nests.ready$start.date[i]) 
  last[i] <- which(inc.dates == hens.nests.ready$end.date[i]) 
  surv.caps[i,first[i]:last[i]] <- 1 
  if(hens.nests.ready$nest_fate[i] == 0)
  {surv.caps[i,last[i]] <- 0} 
}

#' Check work on first individual
hens.nests.ready[1,]

#' NA values for survival caps
#' Need to address this 
surv.caps[1,1:10]

###########################
## Build Model in JAGS ##
###########################

#' Heather Gaya Tutorial Known Fate Code
#' Intercept only nest success model
#' Modeling Daily Nest Survival Probability
modelstring.ns_null = "
model {
logit(phi) <- beta0          # Intercept
for (i in 1:n.ind){
  for(t in (first[i]+1):last[i]){
    mu[i,t] <- phi*y[i,t-1]
    y[i,t] ~dbern(mu[i,t])
  }
}
beta0 ~ dunif(-6,-6)    # Prior for Intercept
}
"

#' Information to provide JAGS
#' jd = lists of individual capture histories, first observed incubating, last observed incubating
#' ji = inits (0.5)
#' jp = model parameters
jd <- list(n.ind= nrow(hens.nests.ready), y = surv.caps, first = first, last = last)
ji <- function(){list(beta0 = .5)}
jp <- c("beta0", "phi")

#' Fit model using JAGS
#' 2000 iterations 
#' 1000 burnin
#' 1 chain
ns.null <- jags.samples(model = modelstring.ns_null, 
                        data = jd, 
                        inits = ji, 
                        n.chains = 1, 
                        burnin = 1000, 
                        n.iter = 2000)

################################################################################
################################################################################
