#####################################################
##  Kery and Schaub code to analyze dead-recovery data
##  Adjusted for class project
######################################################
library(jagsUI)
#library(mcmcplots)
library(MCMCvis)
library(lubridate)
library(tidyverse)
library(dplyr)
#library(jagshelper)

#' Model output for reference 
model.output <-readRDS("Data Management/RData/KnownFate_WFS560/mr.ssModel.RDS")


##### DATA #####
#Read in the data
#### Read in data and select data used for analysis
df2a <- read.csv("WFS560_KnownFate/Band Data Export Report 2020-23 9-08-2023 To PSU.csv")  # release data
df2b <- read.csv("WFS560_KnownFate/Reported Band Data Export Report 2020-2023 To PSU 9-08-23.csv")  # recovery data
df <- left_join(df2a[,c(1,2,6,7,11,17,22,35)], df2b[,c(1,6,7,15)], join_by(band_id==BandNumber))
df <- df[df$recapture!="Yes" & df$Turkey_Age!="U",] # remove recaptures and unknown age
df <- df[df$Turkey_Sex!="F",]  # remove females
df$capyr <- year(as.Date(df$Date_Capture, "%m/%d/%Y"))

### Table that defines grouping of WMUs
wmu <- c("1A","1B","2A","2B","2C","2D","2E","2F","2G","2H","3A","3B","3C","3D","4A","4B","4C","4D","4E","5A","5B","5C","5D")
grp <- c(1,1,2,3,2,2,2,4,4,4,4,4,5,6,7,7,8,7,8,9,9,10,10)

### Group WMUs
LookUp <- as.data.frame(cbind(wmu,grp))
df <- left_join(df,LookUp, by="wmu")
#Delete unneeded columns and rename columns
df <- df %>% select(-transmitter_id, -Date_Capture, -recapture, -Turkey_Sex, -Age)
df <- rename(df, "recovYr" = "Year")
df <- rename(df, "bandYr" = "capyr")
df <- rename(df, "age" = "Turkey_Age")
df$grp <- as.integer(df$grp)


## first banding year is Year 1, recovery year is relative to banding year
df$recovYr <- ifelse(!is.na(df$recovYr),df$recovYr-min(df$recovYr,na.rm=T)+1,
                     max(df$recovYr,na.rm=T)-min(df$recovYr,na.rm=T)+2)
df$bandYr <- df$bandYr-min(df$bandYr,na.rm=T)+1

## make an encounter matrix 
MR <- matrix(0, nrow=nrow(df), ncol=max(df$bandYr)+1)

for (i in 1:dim(MR)[1]) {
  MR[i,df$bandYr[i]] <- 1 #add the one for the yr it was banded
  if (df$recovYr[i]<max(df$recovYr)) {MR[i,df$recovYr[i]+1] = 1} #adds one for yr recovered, if recovered
}

## make an age matrix
Age <- matrix(NA, nrow=nrow(df), ncol=max(df$bandYr)) #1 and 0 taking into account if it grew up

for (i in 1:dim(Age)[1]) {
  for (j in df$bandYr[i]:max(df$bandYr)) {
    if (df$age[i]=="J" & j==df$bandYr[i]) {Age[i,j] <- 1
    } else {Age[i,j] <- 0}
  }
}

reward <- ifelse(df$reward_band=="Y",1,0)


I <- matrix(c(1,0,0,
              0,1,0,
              0,0,1,
              0,0,0), 
            nrow = 4, ncol = 3, byrow=T,)

group <- df$grp

##### Functions #####

# Define function to create a matrix of initial values for latent state z
mr.init.z <- function(mr){
  ch <- matrix(NA, nrow = dim(mr)[1], ncol = dim(mr)[2])
  rec <- which(rowSums(mr)==1)
  for (i in 1:length(rec)){
    n1 <- which(mr[rec[i],]==1)
    ch[rec[i],n1:dim(mr)[2]] <- 0
    ch[rec[i],n1] <- NA
  }
  return(ch)
}

# Define function to create a matrix with information about known latent state z
known.state.mr <- function(mr){
  state <- matrix(NA, nrow = dim(mr)[1], ncol = dim(mr)[2])
  rec <- which(rowSums(mr)==2)
  for (i in 1:length(rec)){
    n1 <- min(which(mr[rec[i],]==1))
    n2 <- max(which(mr[rec[i],]==1))
    state[rec[i],n1:n2] <- 1
    state[rec[i],n1] <- NA
    state[rec[i],n2:dim(mr)[2]] <- 0
  }
  return(state)
}

get.first <- function(x) min(which(x!=0))
f <- apply(MR, 1, get.first)



##### JAGS #####

# Bundle data
jags.data <- list(y = MR, age=Age, f = f, I = I, g = group, G = max(group), reward = reward, nind = dim(MR)[1], n.occasions = dim(MR)[2], z = known.state.mr(MR))


# Initial values
inits <- function(){list(z = mr.init.z(MR), mu = dnorm(1,0,1.3), beta = dnorm(1,0,1.3), alpha1 = dnorm(1,0,1.3), alpha2 = dnorm(1,0,1.3), alpha3 = dnorm(1,0,1.3), jv.r = runif(1,0,1), ad.r = runif(1,0,1), sigma.s = runif(1,0,10), delta = runif(1,0,1))}  

# Parameters monitored
parameters <- c("Sa", "Sj", "f.ad", "f.jv", "H.ad", "H.jv", "lambda", "rAdR", "rJvR", "rAdN", "rJvN")

# MCMC settings
ni <- 50000
nt <- 6
nb <- 8000
nc <- 3

# Call JAGS from R (BRT 4 min)
mr.ss <- jags(jags.data, inits, parameters, "SageTimeWMU_rAgeRew_Model_Final.txt", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

#options(max.print = 10000)
print(mr.ss, digits = 3)

#options(digits = 3)
#out_df <- jags_df(mr.ss)
#write.csv(out_df, file="TurkeyData.csv", row.names = F)
#print(mr.ss$mean, digits = 3)
#mcmcplot(mr.ss)
MCMCtrace(mr.ss, pdf=F)