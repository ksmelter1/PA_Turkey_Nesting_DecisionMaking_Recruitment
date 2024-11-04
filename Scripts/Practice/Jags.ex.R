## Load Packages
library(R2jags)

## Read in data file
data <- read.csv( "Scripts/Nest Success/AppendixS4.csv", head=T )


## Create unique nest ID's and get total number of nests 
data$NESTID2 <- as.character( data$NESTID2 )
data$NEST <- NA

for( i in 1:nrow(data) ){
  data$NEST[i] <- substr( strsplit( data$NESTID2[i], "_" )[[1]][1], 2, nchar( strsplit( data$NESTID2[i], "_" )[[1]][1] ))
}

data$NestID <- paste( data$PLOT, paste( data$NEST, data$YEAR, sep="." ), sep="." )
Nests <- unique( data$NestID )
n.nests <- length( Nests )


## Number of intervals each nest was observed for
n.t<- rep( NA,n.nests )
for( i in 1:n.nests ){
  n.t[i] <- nrow( subset( data, NestID==Nests[i] ))
}


## Unique plot levels and number of plots
Plots <- levels( data$PLOT )
n.plots <-length( Plots )


## Unique year levels and number of years
Year <- unique( data$YEAR )
n.years <- length( Year )


## Create covariates
##      Nest level covariates
Plot <- rep( NA, n.nests )
Plot.num <- rep( NA, n.nests )
Year <- rep( NA, n.nests )
Year.num <- rep( NA, n.nests )
Nest.num<-1:265
Spine <- rep( NA, n.nests )
TotalShrubs <- rep( NA, n.nests )
Height <- rep( NA, n.nests )
Coverage <- rep( NA, n.nests )

for( i in 1:n.nests ){
  dat <- subset( data, NestID==Nests[i] )
  Plot[i] <- levels( data$PLOT )[ dat$PLOT[1] ]
  Plot.num[i] <- dat$PLOT[1]
  Year[i] <- dat$YEAR[1]
  Year.num[i] <- Year[i]-1992
  Spine[i] <- dat$PLANT_SPINE[1]
  TotalShrubs[i] <- dat$TOTAL_SHRUBS[1]
  Height[i] <- dat$NEST_HEIGHT[1]
  Coverage[i] <- dat$AVGSIDECOV[1]
}

## Center and scale continious variables
TotalShrubs <- scale( TotalShrubs )[1:n.nests]
Height <- scale( Height )[1:n.nests]
Coverage <- scale( Coverage )[1:n.nests]


## Interval level covariates
##     loop over each nest and each interval within each nest
MaxInt <- max(n.t) #max number of intervals observed

JulianDate <- matrix( NA, nrow=n.nests, ncol=MaxInt )
for( i in 1:n.nests ){
  dat <- subset( data, NestID==Nests[i] )
  for( j in 1:n.t[i] ){
    JulianDate[i,j] <- dat$END.JULIAN[j]
  }
}

## Center and scale Julian dates
mJD <- mean( JulianDate,na.rm=T )
sdJD <- sd( JulianDate,na.rm=T )
JulianDate2 <- ( JulianDate-mJD )/sdJD

## Calculate interval length for each interval
data$Int <- data$END.JULIAN - data$START.JULIAN
t <- matrix( NA, nrow=n.nests, ncol=MaxInt )
for( i in 1:n.nests ){
  dat <- subset( data, NestID==Nests[i] )
  for( j in 1:n.t[i] ){
    t[i,j] <- dat$Int[j]
  }
}

## What stage is each interval part of
Stage <- matrix( NA, nrow=n.nests, ncol=MaxInt )
for( i in 1:n.nests ){
  dat <- subset( data, NestID==Nests[i] )
  for( j in 1:n.t[i] ){
    Stage[i,j] <- levels( data$STAGE )[dat$STAGE[j]]
  }
}

## Create indicator variables for use in the model
Stage.Incub <- matrix( NA, nrow=n.nests, ncol=MaxInt )
for( i in 1:n.nests ){
  for( j in 1:n.t[i] ){
    ifelse(Stage[i,j] == "incub", Stage.Incub[i,j] <- 1, Stage.Incub[i,j] <- 0)
  }
}

Stage.Young <- matrix( NA, nrow=n.nests, ncol=MaxInt )
for( i in 1:n.nests ){
  for( j in 1:n.t[i] ){
    ifelse( Stage[i,j] == "young", Stage.Young[i,j] <- 1, Stage.Young[i,j] <-0 )
  }
}

## Observed interval success/failures
y <- matrix( NA, nrow=n.nests, ncol=MaxInt )
for( i in 1:n.nests ){
  for( j in 1:n.t[i] ){
    if( subset( data, NestID == Nests[i] )$FATE[j] == "S" ) y[i,j] <- 1
    if( subset( data, NestID == Nests[i] )$FATE[j] == "F" ) y[i,j] <- 0
  }
}


## Assumed total number of days in each nesting stage
##     Used to calculate period survival probabilites
LayDays<-3
IncubDays<-12
YoungDays<-10

## Median Nesting Days for survival prob calculations ##
Lay.Med.JD <- (143-mJD)/sdJD
Incub.Med.JD <- (150.5-mJD)/sdJD
Young.Med.JD <- (161.5-mJD)/sdJD


## Set up JAGS Model
LogExp.BTSP.model<-function(){
  #Prior distributions on parameters
  a.plot.sig ~ dunif( 0.1, 2 )
  a.plot.tau <- ( 1/( a.plot.sig * a.plot.sig ))
  
  a.year.sig ~ dunif( 0.1, 2 )
  a.year.tau <- ( 1/( a.year.sig * a.year.sig ))
  
  a.nest.sig~dunif(0.1,2)
  a.nest.tau<-1/(a.nest.sig*a.nest.sig)
  
  a.0 ~ dnorm( 4, .25 ) ##Center prior for intercept at a reasonable value on probability scale for a daily survival probability
  a.Spine ~ dnorm( 0, .333 )
  a.TotalShrubs ~ dnorm( 0, .333 )
  a.Height ~ dnorm( 0, .333 )
  a.Coverage ~ dnorm( 0, .333 )
  
  b.JulianDate ~ dnorm( 0, .333 )
  b.StageIncub ~ dnorm( 0, .333 )
  b.StageYoung ~ dnorm( 0, .333 )
  
  ## random effects for plot and year
  for( g in 1:n.plots ){
    a.plot[g] ~ dnorm( 0, a.plot.tau )
  }
  
  for( h in 1:n.years ){
    a.year[h] ~ dnorm( 0, a.year.tau )
  }
  
  for( f in 1:n.nests ){
    a.nest[f] ~ dnorm( 0, a.nest.tau )
  }
  
  for( i in 1:n.nests ){
    for( j in 1:n.t[i] ){
      z[i,j] <- a.0 + a.plot[Plot.num[i]] + a.year[Year.num[i]] + a.nest[Nest.num[i]] + 
        a.Spine*Spine[i] + a.TotalShrubs*TotalShrubs[i] + a.Height*Height[i] + a.Coverage*Coverage[i] + 
        b.StageIncub*Stage.Incub[i,j] + b.StageYoung*Stage.Young[i,j] + b.JulianDate*JulianDate2[i,j]
      
      theta[i,j] <- pow( ( exp(z[i,j]) /( 1+exp( z[i,j] ))), t[i,j] )
      ##Keep theta away from 0/1
      mu.theta[i,j] <- min( 0.999, max( 0.001, theta[i,j] ))
      
      y[i,j] ~ dbern( mu.theta[i,j] )
      
    }
  }
  
  
  ## Calculate daily survival probabilities for spiny(s) and non-spiny(ns) nests
  ##     for each of the 3 nesting stages: laying(L), incubation(I), nestling(N)
  ##      
  # Non-Spiny plants
  SL.ns<-exp(a.0 + b.JulianDate*Lay.Med.JD)/(1+exp(a.0 + b.JulianDate*Lay.Med.JD))
  SI.ns<-exp(a.0 + b.StageIncub + b.JulianDate*Incub.Med.JD)/(1+exp(a.0 + b.StageIncub + b.JulianDate*Incub.Med.JD))
  SY.ns<-exp(a.0 + b.StageYoung + b.JulianDate*Young.Med.JD)/(1+exp(a.0 + b.StageYoung + b.JulianDate*Young.Med.JD))
  
  # Spiny plants
  SL.s<-exp(a.0 + a.Spine  + b.JulianDate*Lay.Med.JD)/(1+exp(a.0 + a.Spine  + b.JulianDate*Lay.Med.JD))
  SI.s<-exp(a.0 + a.Spine + b.StageIncub + b.JulianDate*Incub.Med.JD)/(1+exp(a.0 + a.Spine + b.StageIncub + b.JulianDate*Incub.Med.JD))
  SY.s<-exp(a.0 + a.Spine + b.StageYoung + b.JulianDate*Young.Med.JD)/(1+exp(a.0 + a.Spine + b.StageYoung + b.JulianDate*Young.Med.JD))
  
  
  ## Calculate period survival probabilities for spiny and non-spiny nests
  P.ns <- pow( SL.ns, LayDays ) * pow( SI.ns, IncubDays ) * pow( SY.ns, YoungDays )
  P.s <- pow( SL.s, LayDays ) * pow( SI.s, IncubDays ) * pow( SY.s, YoungDays )
  
  
  ## Calculate contrast between period survival probabilities
  P.SvNS <- P.s - P.ns
  
  ## Calculate contrasts between daily survival probabilites
  DSL.SvNS <- SL.s - SL.ns
  DSI.SvNS <- SI.s - SI.ns
  DSY.SvNS <- SY.s - SY.ns
  
}

## Write text file of model
write( "model{LogExp.BTSP.model}", "LogExp.BTSP.model.txt" )
model.file <- "LogExp.BTSP.model.txt"

## Set data for the model
data.LogExp <- list( n.plots=n.plots, n.years=n.years, 
                     n.nests=n.nests, n.t=n.t,
                     Plot.num=Plot.num, Year.num=Year.num, Nest.num=Nest.num,
                     Spine=Spine, TotalShrubs=TotalShrubs, Height=Height, Coverage=Coverage,
                     Stage.Incub=Stage.Incub, Stage.Young=Stage.Young,JulianDate2=JulianDate2,
                     t=t, y=y,
                     LayDays=LayDays, IncubDays=IncubDays, YoungDays=YoungDays,
                     Lay.Med.JD=Lay.Med.JD, Incub.Med.JD=Incub.Med.JD, Young.Med.JD=Young.Med.JD
)

## Parameters to be saved
params.LogExp <- c ("a.plot.sig", "a.year.sig", "a.nest.sig",
                    "a.0", "a.plot", "a.year", "a.nest",
                    "a.Spine", "a.TotalShrubs", "a.Height", "a.Coverage",
                    "b.StageIncub", "b.StageYoung", "b.JulianDate",
                    "SL.ns", "SI.ns","SY.ns",
                    "SL.s", "SI.s","SY.s",
                    "P.ns", "P.s",
                    "P.SvNS","DSL.SvNS", "DSI.SvNS", "DSY.SvNS",
                    "mu.theta"
)


## Fit the model in JAGS
LogExp.BTSP <- jags( data=data.LogExp,
                     parameters.to.save=params.LogExp, model.file=LogExp.BTSP.model,
                     n.chains=3, n.iter=200000, n.burnin=100000, n.thin=50,
                     DIC=TRUE, working.directory=NULL, 
                     refresh = 40, progress.bar = "text", digits=5)

## Save model workspace
save.image("BTSP_model.RData")