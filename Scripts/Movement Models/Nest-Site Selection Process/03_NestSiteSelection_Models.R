
#'---
#' title: Nest-site selection of female wild turkeys in Pennsylvania (an SSF analysis)
#' author: "K. Smelter, F. Buderman"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output:
#'   html_document: 
#'     toc: true
#'---
#'
#+ include = FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE, cache = TRUE)
#'  
#' **Purpose**: This script creates a Bayesian conditional logistic regression model for nest-site selection in JAGs using the gathered covariates 

#####################
## Load Packages ##
####################

#' Vector of package names
packages <- c("R2jags",
              "MCMCvis",
              "dplyr")

#' Function to load a package or install it if not already installed
load_packages <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    require(package_name, character.only = TRUE)
  }
}

#' Apply the function to each package name
lapply(packages, load_packages)

####################
## Organize Data ##
####################

#' Create nest.data object for models
#' Rename variables
#' Scale all covariates except for proportions
#' Change covariates back from a matrix to numeric
nest.data <-readRDS("Data Management/RData/Nest-Site Selection/nest-siteselection.RData") %>%
  dplyr::select(nestd_v, bandid, wmu_v, avrgmxv, grssfrb, percwdy,
                case, Developed, Forest, Agriculture, elev,
                distfromroad)  %>%
  dplyr::rename("avgvisob"= avrgmxv) %>%
  dplyr::rename("nestid_v"= nestd_v) %>%
  dplyr::rename("developed"= Developed) %>%
  dplyr::rename("agriculture"= Agriculture) %>%
  dplyr::rename("forest"= Forest) %>%
  dplyr::mutate(percwdy = scale(percwdy)) %>%
  dplyr::mutate(grssfrb = scale(grssfrb)) %>%
  dplyr::mutate(avgvisob = scale(avgvisob)) %>%
  dplyr::mutate(elev = scale(elev)) %>%
  dplyr::mutate(distfromroad = scale(distfromroad)) %>%
  dplyr::mutate(percwdy = as.numeric(percwdy)) %>%
  dplyr::mutate(grssfrb = as.numeric(grssfrb)) %>%
  dplyr::mutate(avgvisob = as.numeric(avgvisob)) %>%
  dplyr::mutate(elev = as.numeric(elev)) %>%
  dplyr::mutate(distfromroad = as.numeric(distfromroad)) 


#' Check data
glimpse(nest.data)

#' Check structure
str(nest.data)

#' Change case to integer
nest.data$case <- as.numeric(nest.data$case)

#' Order df by nestid
nest.data <- nest.data[order(nest.data$nestid_v),]

#' Change Nest_ID_V to numeric 
#' First must remove underlines
nest.data$nestid_v <- gsub("_", "", nest.data$nestid_v)
nest.data$nestid_v<- as.numeric(nest.data$nestid_v)

#' Create str_id column 
#' This allows the loop to iterate through the steps associated with each bird 
#' cur_group_id() gives a unique numeric identifier for the current group.
nest.data <- nest.data %>%
  group_by(nestid_v) %>%
  mutate(str_ID=cur_group_id())

#' Check
str(nest.data)
glimpse(nest.data)
summary(nest.data)

#' Function to fill NA values with 0 throughout dataframe
#' NA's represent 0 observations with the NLCD covariates so switch them to 0
fill_NA_with_value <- function(df, value = 0) {
  #' Ensure that all columns are of numeric type to handle the replacement
  df[] <- lapply(df, function(col) {
    if (is.numeric(col)) {
      #' Replace NA values with the specified value
      replace(col, is.na(col), value)
    } else {
      #' Return the column unchanged if it's not numeric
      col
    }
  })
  return(df)
}

#' Apply the function to fill NA values with 0
nest.data.ready <- fill_NA_with_value(nest.data)

#' Check
summary(nest.data.ready)

#' Check correlations by plotting 
#' Predictors are highly correlated
#' I'm gonna try fitting separate models for each land cover predictor
plot(nest.data.ready$developed)
plot(nest.data.ready$agriculture)
plot(nest.data.ready$forest)

######################################################
## Bayesian Conditional Logistic Regression Models ##
######################################################

##################
## Full Model ##
##################

nest.selection.full.mod <- "  
 model{
  for (i in 1:I){
   case[i]~dpois(lambda[i])
   log(lambda[i])<-distfromroad[i]*beta.distfromroad+forest[i]*beta.forest+ agriculture[i]*beta.agriculture+ developed[i]*beta.developed+ elevation[i] *beta.elevation + visob[i]* beta.visob+ grassforb[i]*beta.grassforb+ percwoody[i]*beta.percwoody+alpha[str_ID[i]] 
  }
  
  #Priors
  beta.forest~dnorm(0,0.0001) #Forest
  beta.developed~dnorm(0,0.0001) #Developed
  beta.agriculture~dnorm(0,0.0001) #Agriculture
  beta.distfromroad~dnorm(0,0.0001) #Distance from Road
  beta.elevation~dnorm(0,0.0001) #Elevation
  beta.visob~dnorm(0,0.0001) #Average Visual Obstruction
  beta.grassforb~dnorm(0,0.0001) #Percent Grass Forb
  beta.percwoody~dnorm(0,0.0001) #Percent Woody

  for (k in 1:K){
   alpha[k]~dnorm(0,0.000001)
  }
 }
"
#' Data list for JAGS
jags_data <- list(
  case = nest.data.ready$case,
  distfromroad = nest.data.ready$distfromroad,
  forest= nest.data.ready$forest,
  developed= nest.data.ready$developed,
  agriculture= nest.data.ready$agriculture,
  elevation= nest.data.ready$elev,
  visob= nest.data.ready$avgvisob,
  grassforb= nest.data.ready$grssfrb,
  percwoody= nest.data.ready$percwdy,
  str_ID = c(nest.data.ready$str_ID),
  I=nrow(nest.data.ready),
  K=length(unique(nest.data.ready$str_ID))
)

#' Initialize JAGS
jags_model <- jags.model(textConnection(nest.selection.full.mod), data = jags_data, n.chains = 1)

#' Burn-in and sampling from posterior distribution
jags_samples <- coda.samples(jags_model, c("beta.forest",
                                           "beta.agriculture",
                                           "beta.developed",
                                           "beta.distfromroad",
                                           "beta.elevation", 
                                           "beta.visob",
                                           "beta.percwoody",
                                           "beta.grassforb"), 
                             n.iter = 10000,  n.burnin = 2000)

MCMCtrace(jags_samples, pdf=F)


####################
## No Developed ##
####################

#' Remove Developed from the model 
nest.selection.nodev.mod <- "  
 model{
  for (i in 1:I){
   case[i]~dpois(lambda[i])
   log(lambda[i])<-distfromroad[i]*beta.distfromroad+forest[i]*beta.forest+ agriculture[i]*beta.agriculture+ elevation[i] *beta.elevation + visob[i]* beta.visob+ grassforb[i]*beta.grassforb+ percwoody[i]*beta.percwoody+alpha[str_ID[i]] 
  }
  
  #Priors
  beta.forest~dnorm(0,0.0001) #Forest
  beta.agriculture~dnorm(0,0.0001) #Agriculture
  beta.distfromroad~dnorm(0,0.0001) #Distance from Road
  beta.elevation~dnorm(0,0.0001) #Elevation
  beta.visob~dnorm(0,0.0001) #Average Visual Obstruction
  beta.grassforb~dnorm(0,0.0001) #Percent Grass Forb
  beta.percwoody~dnorm(0,0.0001) #Percent Woody

  for (k in 1:K){
   alpha[k]~dnorm(0,0.000001)
  }
 }
"
#' Data list for JAGS
jags_data <- list(
  case = nest.data.ready$case,
  distfromroad = nest.data.ready$distfromroad,
  forest= nest.data.ready$forest,
  agriculture= nest.data.ready$agriculture,
  elevation= nest.data.ready$elev,
  visob= nest.data.ready$avgvisob,
  grassforb= nest.data.ready$grssfrb,
  percwoody= nest.data.ready$percwdy,
  str_ID = c(nest.data.ready$str_ID),
  I=nrow(nest.data.ready),
  K=length(unique(nest.data.ready$str_ID))
)

#' Initialize JAGS
jags_model <- jags.model(textConnection(nest.selection.nodev.mod), data = jags_data, n.chains = 1)

#' Burn-in and sampling from posterior distribution
jags_samples <- coda.samples(jags_model, c("beta.forest",
                                           "beta.agriculture",
                                           "beta.distfromroad",
                                           "beta.elevation", 
                                           "beta.visob",
                                           "beta.percwoody",
                                           "beta.grassforb"), 
                             n.iter = 10000,  n.burnin = 2000)

MCMCtrace(jags_samples, pdf=F)


