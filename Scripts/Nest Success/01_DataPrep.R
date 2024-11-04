########################################
## Data Prep for Nest Success Model ##
#######################################

####################
## Load Packages ##
####################

require(reshape2)
require(ggplot2)
require(MASS)
require(lme4)
require(visreg)
require(sjPlot)
require(sf)

##########################

dat <- nests.veg %>%
  dplyr::rename("fate"= analysisfate)

#Subset data
dat2 <- subset(dat,
               select=c(nestid,exposure,checkdate,fate,
                        averagemaxvo, percgrassforb, percwoody, birdid, litterht))
dat2S <- subset(dat2,exposure>0)

mdat <- melt(dat2S,id.var=1:4)
ggplot(mdat,aes(x=value,y=fate))+
  geom_point(alpha=0.5,aes(size=exposure))+geom_smooth(method="loess")+
  facet_wrap(~variable,scale="free_x")+
  coord_cartesian(ylim=c(-0.05,1.05))+xlab("value")+ylab("Daily Nest Survival")


#GLM Method
logexp <- function(exposure = 1) {
  ## hack to help with visualization, post-prediction etc etc
  get_exposure <- function() {
    if (exists("..exposure", env=.GlobalEnv))
      return(get("..exposure", envir=.GlobalEnv))
    exposure
  }
  linkfun <- function(mu) qlogis(mu^(1/get_exposure()))
  ## FIXME: is there some trick we can play here to allow
  ##   evaluation in the context of the 'data' argument?
  linkinv <- function(eta) plogis(eta)^get_exposure()
  logit_mu_eta <- function(eta) {
    ifelse(abs(eta)>30,.Machine$double.eps,
           exp(eta)/(1+exp(eta))^2)
  }
  mu.eta <- function(eta) {       
    get_exposure() * plogis(eta)^(get_exposure()-1) *
      logit_mu_eta(eta)
  }
  valideta <- function(eta) TRUE
  link <- paste("logexp(", deparse(substitute(exposure)), ")",
                sep="")
  structure(list(linkfun = linkfun, linkinv = linkinv,
                 mu.eta = mu.eta, valideta = valideta, 
                 name = link),
            class = "link-glm")
}

