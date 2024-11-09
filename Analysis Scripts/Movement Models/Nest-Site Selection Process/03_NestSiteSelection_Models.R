
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
              "dplyr",
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

####################
## Organize Data ##
####################

load("Data Management/RData/Nest-Site Selection/02_Covs.RData")



#' Create nest.data object for models
#' Rename variables
#' Scale all covariates except for proportions
#' Change covariates back from a matrix to numeric
nest.data <- pa.nests.covs 
nest.data$grssfrb <- pa.nests$grssfrb

  nest.data <- nest.data %>%
    dplyr::select(nest_id, bandid, avrgmxv, grssfrb, percwdy,
                case, Developed, Deciduous, Mixed, Evergreen, Agriculture, elev,
                primary, secondary)  %>%
  dplyr::mutate(percwdy = scale(percwdy)) %>%
  dplyr::mutate(grssfrb = scale(grssfrb)) %>%
  dplyr::mutate(avgvisob = scale(avrgmxv)) %>%
  dplyr::mutate(elev = scale(elev)) %>%
  dplyr::mutate(primary = scale(primary)) %>%
  dplyr::mutate(secondary = scale(secondary)) %>%
  dplyr::mutate(percwdy = as.numeric(percwdy)) %>%
  dplyr::mutate(grssfrb = as.numeric(grssfrb)) %>%
  dplyr::mutate(avgvisob = as.numeric(avgvisob)) %>%
  dplyr::mutate(elev = as.numeric(elev)) %>%
  dplyr::mutate(primary = as.numeric(primary)) %>%
  dplyr::mutate(secondary = as.numeric(secondary)) 

#' Check data
glimpse(nest.data)

#' Check structure
str(nest.data)

#' Change case to integer
nest.data$case <- as.numeric(nest.data$case)

#' Order df by nestid
nest.data <- nest.data[order(nest.data$nest_id),]

#' Change Nest_ID_V to numeric 
#' First must remove underlines
nest.data$nest_id <- gsub("_", "", nest.data$nest_id)
nest.data$nest_id<- as.numeric(nest.data$nest_id)

#' Create str_id column 
#' This allows the loop to iterate through the steps associated with each bird 
#' cur_group_id() gives a unique numeric identifier for the current group.
nest.data <- nest.data %>%
  group_by(nest_id) %>%
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
   log(lambda[i])<-primary[i]*beta.primary +
   secondary[i]*beta.secondary +
   deciduous[i]*beta.deciduous_forest + 
   mixed[i]*beta.mixed_forest + 
   evergreen[i]*beta.evergreen_forest + 
   agriculture[i]*beta.agriculture + 
   developed[i]*beta.developed + 
   elevation[i] *beta.elevation + 
   visob[i]* beta.visob +
   grassforb[i]*beta.grassforb +
   percwoody[i]*beta.percwoody + 
   alpha[str_ID[i]] 
  }
  
  #Priors
  beta.deciduous_forest~dnorm(0,0.0001) #Deciduous Forest
  beta.mixed_forest~dnorm(0,0.0001) #Mixed Forest
  beta.evergreen_forest~dnorm(0,0.0001) #Mixed Forest
  beta.developed~dnorm(0,0.0001) #Developed
  beta.agriculture~dnorm(0,0.0001) #Agriculture
  beta.primary~dnorm(0,0.0001) #Primary
  beta.secondary~dnorm(0,0.0001) #Secondary
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
  primary = nest.data.ready$primary,
  secondary = nest.data.ready$secondary,
  mixed = nest.data.ready$Mixed,
  evergreen = nest.data.ready$Evergreen,
  deciduous = nest.data$Deciduous,
  developed = nest.data.ready$Developed,
  agriculture = nest.data.ready$Agriculture,
  elevation = nest.data.ready$elev,
  visob = nest.data.ready$avgvisob,
  grassforb = nest.data.ready$grssfrb,
  percwoody = nest.data.ready$percwdy,
  str_ID = c(nest.data.ready$str_ID),
  I = nrow(nest.data.ready),
  K =length(unique(nest.data.ready$str_ID))
)

#' Initialize JAGS
jags_model <- jags.model(textConnection(nest.selection.full.mod), data = jags_data, n.chains = 1)

#' Burn-in and sampling from posterior distribution
jags_samples <- coda.samples(jags_model, c("beta.deciduous_forest",
                                           "beta.mixed_forest",
                                           "beta.evergreen_forest",
                                           "beta.agriculture",
                                           "beta.developed",
                                           "beta.primary",
                                           "beta.secondary",
                                           "beta.elevation", 
                                           "beta.visob",
                                           "beta.grassforb",
                                           "beta.percwoody"), 
                             n.iter = 10000,  n.burnin = 2000)

MCMCtrace(jags_samples, pdf=F)


####################
## No Developed ##
####################

nest.selection.subset.mod <- "  
 model{
  for (i in 1:I){
   case[i]~dpois(lambda[i])
   log(lambda[i])<-primary[i]*beta.primary +
   secondary[i]*beta.secondary +
   deciduous[i]*beta.deciduous_forest + 
   mixed[i]*beta.mixed_forest + 
   evergreen[i]*beta.evergreen_forest + 
   agriculture[i]*beta.agriculture + 
   elevation[i] *beta.elevation + 
   visob[i]* beta.visob +
   grassforb[i]*beta.grassforb +
   percwoody[i]*beta.percwoody + 
   alpha[str_ID[i]] 
  }
  
  #Priors
  beta.deciduous_forest~dnorm(0,0.0001) #Deciduous Forest
  beta.mixed_forest~dnorm(0,0.0001) #Mixed Forest
  beta.evergreen_forest~dnorm(0,0.0001) #Mixed Forest
  beta.agriculture~dnorm(0,0.0001) #Agriculture
  beta.primary~dnorm(0,0.0001) #Primary
  beta.secondary~dnorm(0,0.0001) #Secondary
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
  primary = nest.data.ready$primary,
  secondary = nest.data.ready$secondary,
  mixed = nest.data.ready$Mixed,
  evergreen = nest.data.ready$Evergreen,
  deciduous = nest.data$Deciduous,
  agriculture = nest.data.ready$Agriculture,
  elevation = nest.data.ready$elev,
  visob = nest.data.ready$avgvisob,
  grassforb = nest.data.ready$grssfrb,
  percwoody = nest.data.ready$percwdy,
  str_ID = c(nest.data.ready$str_ID),
  I = nrow(nest.data.ready),
  K =length(unique(nest.data.ready$str_ID))
)

#' Initialize JAGS
jags_model <- jags.model(textConnection(nest.selection.subset.mod), 
                         data = jags_data,
                         n.chains = 3)

#' Burn-in and sampling from posterior distribution
jags_samples <- coda.samples(jags_model, c("beta.deciduous_forest",
                                           "beta.mixed_forest",
                                           "beta.evergreen_forest",
                                           "beta.agriculture",
                                           "beta.primary",
                                           "beta.secondary",
                                           "beta.elevation", 
                                           "beta.visob",
                                           "beta.grassforb",
                                           "beta.percwoody"), 
                             n.iter = 10000,  n.burnin = 2000)

MCMCtrace(jags_samples, pdf=F)

#' Convert mcmc.list to matrix
samples_matrix <- as.matrix(jags_samples)

#' Convert the matrix to a data frame
samples_df <- as.data.frame(samples_matrix)

#' View the first few rows of the samples data frame
head(samples_df)

#' Reshape the data into long format for ggplot
samples_long <- samples_df %>%
  pivot_longer(cols = everything(), 
               names_to = "parameter", 
               values_to = "estimate") 

#' View the reshaped data
head(samples_long)

#' Calculate Bayesian credible intervals
credible_intervals <- samples_long %>%
  group_by(parameter) %>%
  summarise(
    lower = quantile(estimate, 0.025),   # 2.5th percentile
    upper = quantile(estimate, 0.975),   # 97.5th percentile
    .groups = 'drop'
  )

#' Create plot
ggplot(samples_long, aes(x = estimate, fill = parameter)) +
  geom_density(alpha = 0.5) +  # Density plot with transparency
  facet_wrap(~parameter, scales = "free", ncol = 2) +  # Create separate panels for each parameter
  labs(x = "Parameter Estimate", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none") +
  #' Add vertical lines for the 95% credible intervals
  geom_vline(data = credible_intervals, aes(xintercept = lower), linetype = "dashed", color = "red") +
  geom_vline(data = credible_intervals, aes(xintercept = upper), linetype = "dashed", color = "red")
