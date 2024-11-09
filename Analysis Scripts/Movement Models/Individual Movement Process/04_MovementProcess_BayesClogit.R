
#'---
#' title: Habitat selection of female wild turkeys during incubation (an SSF analysis)
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
#' **Purpose**: This script runs SSF models in frequentist and Bayesian frameworks and extracts coefficient data


####################
## Load Packages ##
####################

#' Vector of package names
packages <- c("R2jags",
              "MCMCvis",
              "dplyr",
              "survival",
              "lme4",
              "rstanarm",
              "tidyr",
              "jagsUI")


#' Function to load a package or install it if not already installed
#' Found this function 
load_packages <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    require(package_name, character.only = TRUE)
  }
}

#' Apply the function to each package name
lapply(packages, load_packages)


#' Visually checked and the projections look good
load("Data Management/RData/Individual-Specific Movement Process/RData Files/Covs_Ready.RData") 

#' Paste in distance from primary and secondary roads
dat_2$prim.road <- dist.to.prim.road.out.df$dist.to.prim.road.out
dat_2$sec.road <- dist.to.sec.road.out.df$dist.to.sec.road.out

  
dat_2.ready <- dat_2%>% dplyr::mutate(elev = scale(elev)) %>%
                 dplyr::mutate(sec.road.scaled = scale(sec.road)) %>%
                 dplyr::mutate(prim.road.scaled = scale(prim.road)) %>%
                 dplyr::mutate(elev = as.numeric(elev)) %>%
                 dplyr::mutate(sec.road.scaled = as.numeric(sec.road.scaled)) %>%
                 dplyr::mutate(prim.road.scaled = as.numeric(prim.road.scaled)) 

#' Plot out data based on covariates
# plot(final.data$Shrub)
# plot(final.data$Agriculture)
# plot(final.data$Forest)
# plot(final.data$Developed)

####################
## Organize Data ##
####################

#' Check
str(dat_2.ready)

#' Change ID to numeric
dat_2.ready$id <- gsub("_", "", dat_2.ready$id)
dat_2.ready$id<- as.numeric(dat_2.ready$id)

#' Create strata column by merging BirdID and step_id
dat_2.ready$NA_ID <- paste(dat_2.ready$id, dat_2.ready$step_id_, sep = "_")

#' Change NA_ID to integer
dat_2.ready$NA_ID <- gsub("_", "", dat_2.ready$NA_ID)
dat_2.ready$NA_ID<- as.numeric(dat_2.ready$NA_ID)

class(dat_2.ready)
str(dat_2.ready)

#' Add numerical variable for animals:
dat_2.ready$ANIMAL_ID <- as.numeric(as.factor(dat_2.ready$id))

#' Stratum ID is given as "NA_ID" in the data; 
#' It is easier to have sequential enumeration, so let's generate a new stratum-ID variable str_ID:
d.map <- data.frame(NA_ID=unique(dat_2.ready$NA_ID),str_ID=1:length(unique(dat_2.ready$NA_ID)))
dat_2.ready$str_ID <- d.map[match(dat_2.ready$NA_ID,d.map$NA_ID),"str_ID"]
dat_2.ready <- dat_2.ready[order(dat_2.ready$str_ID),] 
glimpse(dat_2.ready)

#' Function to fill NA values with 0 throughout dataframe
#' Not sure if this will be the approach we take but I'm going with this for now
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

#' Apply the function to fill NA values with 0.0001
dat_2.ready <- fill_NA_with_value(dat_2.ready)

#' Check
which(dat_2.ready$step_id_==3)
table(dat_2.ready$case_)
class(dat_2.ready)
summary(dat_2.ready)



##################
## Full Model ##
#################

#' Specify JAGS model structure
#' Removed Developed term due to its correlation with Agriculture
nodev.mod <- "  
 model{
  for (i in 1:I){
   case[i]~dpois(lambda[i])
   log(lambda[i])<-sec.road[i]*beta.sec.road+ prim.road[i]*beta.prim.road+forest[i]*beta.forest+ elevation[i] *beta.elevation + agriculture[i]*beta.agriculture+ shrub[i]*beta.shrub+ alpha[str_ID[i]] 
  }
  
  #Priors
  beta.forest~dnorm(0,0.0001) #Forest
  beta.sec.road~dnorm(0,0.0001)#Secondary Road
  beta.prim.road~dnorm(0,0.0001)#Primary Road
  beta.elevation~dnorm(0,0.0001) #Elevation
  beta.agriculture~dnorm(0,0.0001) #Agriculture
  beta.shrub~dnorm(0,0.0001) #Shrub

  for (k in 1:K){
   alpha[k]~dnorm(0,0.000001)
  }
 }
"
#' Data list for JAGS
jags_data <- list(
  case = dat_2.ready$case_,
  sec.road= dat_2.ready$sec.road.scaled,
  prim.road= dat_2.ready$prim.road.scaled,
  forest= dat_2.ready$Forest,
  elevation= dat_2.ready$elev,
  agriculture= dat_2.ready$Agriculture,
  shrub= dat_2.ready$Shrub,
  str_ID = dat_2.ready$str_ID,
  I=nrow(dat_2.ready),
  K=length(unique(dat_2.ready$str_ID))
)

start=Sys.time()

#' Initial values
#' Check with Franny what the inits should be
inits <- function(){list(beta.forest~dnorm(0,0.0001), #Forest
                         beta.sec.road~dnorm(0,0.0001),#Secondary Road
                         beta.prim.road~dnorm(0,0.0001),#Primary Road
                         beta.elevation~dnorm(0,0.0001) ,#Elevation
                         beta.agriculture~dnorm(0,0.0001), #Agriculture
                         beta.shrub~dnorm(0,0.0001))} #Shrub


#' Parameters to save in jags model
parameters.to.save <- c("beta.forest",
                        "beta.sec.road",
                        "beta.prim.road",
                        "beta.agriculture", 
                        "beta.elevation", 
                        "beta.shrub")
  
#' Initialize JAGS
#' Takes a while
jags_model <- jags(textConnection(nodev.mod), 
                  data = jags_data, 
                   n.chains = 1, inits=inits,
                   parameters.to.save = parameters.to.save,   
                   n.iter = 40000,  n.burnin = 10000,
                   parallel = T)

#' Burn-in and sampling from posterior distribution
#' Takes even longer 
# jags_samples <- jags.parallel(jags_model, 
#                              c("beta.forest",
#                                "beta.sec.road",
#                                "beta.prim.road",
#                                "beta.agriculture", 
#                                "beta.elevation", 
#                                "beta.shrub"), 
#                              n.iter = 40000,  n.burnin = 10000)

end=Sys.time()
end-start


MCMCtrace(jags_samples, pdf=F)

################################################################################
################################################################################

#######################################
## Conditional Logistic Regression ##
#######################################

#' amt code
#' fit_clogit is a wrapper for the survival package

#' m1 <- final.data %>% fit_clogit(case_ ~ Developed + strata(step_id_) +strata(BirdID))
#' m2 <- final.data %>% fit_clogit(case_ ~ Agriculture + strata(step_id_) + strata(BirdID))
#' m3 <- final.data %>% fit_clogit(case_ ~ Forest + strata(step_id_) + strata(BirdID))
#' m4 <- final.data %>% fit_clogit(case_ ~ Shrub + strata(step_id_) + strata(BirdID))
#' m5<- final.data %>% fit_clogit(case_ ~ distfromroad + strata(step_id_) + strata(BirdID))
#' m6 <- final.data %>% fit_clogit(case_ ~ elev + strata(step_id_) + strata(BirdID))
#' 
#' summary(m1)
#' summary(m2)
#' summary(m3)
#' summary(m4)
#' summary(m5)
#' summary(m6)
#' 
#' ###############################################
#' ## Generalized Linear Mixed Effects Models ##
#' ###############################################
#' 
#' #' Likelihood framework using glmer package
#' m7 <- lmer(case_ ~ Developed + (1|step_id_) +(Developed|BirdID),data = final.data)
#' m8 <- lmer(case_ ~ Agriculture + (1|step_id_) +(Agriculture|BirdID),data = final.data)
#' m9 <- lmer(case_ ~ Forest + (1|step_id_) +(Forest|BirdID),data = final.data)
#' m10 <- lmer(case_ ~ Shrub+ (1|step_id_) +(Shrub|BirdID),data = final.data)
#' m11 <- lmer(case_ ~ elev + (1|step_id_) +(elev|BirdID),data = final.data)
#' m12 <- lmer(case_ ~ distfromroad + (1|step_id_) +(distfromroad|BirdID),data = final.data)
#' 
#' summary(m7)
#' summary(m8)
#' summary(m9)
#' summary(m10)
#' summary(m11)
#' summary(m12)
#' 
#' 
#' ########################################################
#' ## Bayesian Generalized Linear Mixed Effects Models ##
#' ########################################################
#' 
#' #' Bayesian glmm using rstanarm
#' #' Doesn't run
#' m13 <- rstanarm::stan_lmer(case_ ~ Developed + (1|step_id_) +(Developed|BirdID),data = final.data)
#' m14 <- rstanarm::stan_lmer(case_ ~ Agriculture + (1|step_id_) +(Agriculture|BirdID),data = final.data)
#' m15 <- rstanarm::stan_lmer(case_ ~ Forest + (1|step_id_) +(Forest|BirdID),data = final.data)
#' m16 <- rstanarm::stan_lmer(case_ ~ Shrub+ (1|step_id_) +(Shrub|BirdID),data = final.data)
#' m17 <- rstanarm::stan_lmer(case_ ~ elev + (1|step_id_) +(elev|BirdID),data = final.data)
#' m18 <- rstanarm::stan_lmer(case_ ~ distfromroad + (1|step_id_) +(distfromroad|BirdID),data = final.data)
#' 
#' summary(m13)
#' summary(m14)
#' summary(m15)
#' summary(m16)
#' summary(m17)
#' summary(m18)
#' 
#' 
#' ############################
#' ## Extract Coefficients ##
#' ############################
#' 
#' Dist2Road <- broom::tidy(m5$model) %>% 
#' filter( term== "distfromroad") %>%
#' bind_cols(confint(m5$model)) %>%
#' rename(est=estimate, l95=`2.5 %`, u95=`97.5 %`) 
#' 
#' elev <- broom::tidy(m6$model) %>% 
#' filter(term == "elev") %>%
#' bind_cols(confint(m6$model)) %>%
#'  rename(est=estimate, l95=`2.5 %`, u95=`97.5 %`)
#' 
#' Developed <- broom::tidy(m1$model) %>% 
#'   filter(term== "Developed") %>%
#'   bind_cols(confint(m1$model)) %>%
#'   rename(est=estimate, l95=`2.5 %`, u95=`97.5 %`)
#' 
#' Forest <- broom::tidy(m3$model) %>% 
#'   filter(term == "Forest") %>%
#'   bind_cols(confint(m3$model)) %>%
#'   rename(est=estimate, l95=`2.5 %`, u95=`97.5 %`)
#' 
#' Shrub <- broom::tidy(m4$model) %>% 
#'   filter(term == "Shrub") %>%
#'   bind_cols(confint(m4$model)) %>%
#'   rename(est=estimate, l95=`2.5 %`, u95=`97.5 %`)
#' 
#' Agriculture <- broom::tidy(m2$model) %>% 
#'   filter(term == "Agriculture") %>%
#'   bind_cols(confint(m2$model)) %>%
#'   rename(est=estimate, l95=`2.5 %`, u95=`97.5 %`)
#' 
#' #' Create df for visualizations
#' ggdf <- bind_rows(Developed,Forest, Shrub,
#'                   Agriculture, Dist2Road, elev) 
#' 
#' #' Changing data to factor in order to rename names from rows using levesls
#' ggdf$term<-as.factor(ggdf$term)
#' levels(ggdf$term)[levels(ggdf$term) == 'distfromroad'] <- 'Distance from Road'
#' levels(ggdf$term)[levels(ggdf$term) == 'elev'] <- 'Elevation'
#' levels(ggdf$term)[levels(ggdf$term) == 'Agriculture'] <- 'Agriculture'
#' levels(ggdf$term)[levels(ggdf$term) == 'Developed'] <- 'Developed'
#' levels(ggdf$term)[levels(ggdf$term) == 'Forest'] <- 'Forest'
#' levels(ggdf$term)[levels(ggdf$term) == 'Shrub'] <- 'Shrub/Scrub'
#' 
#' 
#' #' Save RDS file of ggdf for plots
#' #saveRDS(ggdf, "Working_SSF_Outputs.RDS")
#' 
#' 
