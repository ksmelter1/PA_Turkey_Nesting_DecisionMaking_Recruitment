
#'---
#' title: Nest-site selection of wild turkeys in Pennsylvania (an SSF analysis)
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
#' **Purpose**: This script produces coefficient and prediction plots of the model outputs
#' 
##########################
## Make Plot of Betas ##
##########################

covs <-ggdf %>% 
  ggplot(mapping = aes(x = names, y = est)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_point() +
  geom_errorbar(aes(ymin = l95, ymax = u95)
                ,position=position_dodge2(0.9,reverse = TRUE))+
  theme_classic() +
  xlab("Covariate") +
  ylab("Beta Estimate") +
  coord_flip() +
  scale_x_discrete(limits=rev)
covs  

#########################
## Prediction Plots ##
#########################

#' https://stackoverflow.com/questions/47080625/plotting-predictions-from-a-logistic-regression

#Visual Obstruction
Visob.Predict <- ggplot(final.data, aes(x=Visual.Obstruction, y=case)) +
  stat_smooth(method="glm",
              method.args = list(family="binomial"), se=TRUE,
              fullrange=TRUE) +
  theme_classic()+
  labs(x="Visual Obstruction", y="Probability of Use")+
  ylim(0,1)+
  expand_limits(x=10)
Visob.Predict

#' Grass Forb
GrassForb.Predict <- ggplot(final.data, aes(x= GrassForb, y=case)) +
  stat_smooth(method="glm",
              method.args = list(family="binomial"), se=TRUE,
              fullrange=TRUE) +
  theme_classic()+
  labs(x="Grass Forb", y="Probability of Use")+
  ylim(0,1)+
  expand_limits(x=20)
GrassForb.Predict

#' Percent Woody
PercWoody.Predict <- ggplot(final.data, aes(x= PercentWoody, y=case)) +
  stat_smooth(method="glm",
              method.args = list(family="binomial"), se=TRUE,
              fullrange=TRUE) +
  theme_classic()+
  labs(x="Percent Woody", y="Probability of Use")+
  ylim(0,1)+
  expand_limits(x=50)
PercWoody.Predict

#' Distance from Road
Dist2Road.Predict <- ggplot(final.data, aes(x= NEARDIST, y=case)) +
  stat_smooth(method="glm",
              method.args = list(family="binomial"), se=TRUE,
              fullrange=TRUE) +
  theme_classic()+
  labs(x="Distance from Road", y="Probability of Use")+
  ylim(0,1)+
  expand_limits(x=80)
Dist2Road.Predict

#' Agriculture
Agriculture.Z.Predict <- ggplot(final.data, aes(x= Agriculture.Z, y=case)) +
  stat_smooth(method="glm",
              method.args = list(family="binomial"), se=TRUE,
              fullrange=TRUE) +
  theme_classic()+
  labs(x="Percent Agriculture", y="Probability of Use")+
  ylim(0,1)+
  expand_limits(x=100)
Agriculture.Z.Predict

#' Forest
Forest.Z.Predict <- ggplot(final.data, aes(x= Forested.Z, y=case)) +
  stat_smooth(method="glm",
              method.args = list(family="binomial"), se=TRUE,
              fullrange=TRUE) +
  theme_classic()+
  labs(x="Percent Forest", y="Probability of Use")+
  ylim(0,1)+
  expand_limits(x=100)
Forest.Z.Predict 

#' Developed
Developed.Z.Predict <- ggplot(final.data, aes(x= Developed.Z, y=case)) +
  stat_smooth(method="glm",
              method.args = list(family="binomial"), se=TRUE,
              fullrange=TRUE) +
  theme_classic()+
  labs(x="Percent Developed", y="Probability of Use")+
  ylim(0,1)+
  expand_limits(x=100)
Developed.Z.Predict 

#' Odds Ratios
odds.data <- pa.nests.prop %>%
  dplyr::select(avrgmxv, case, nestd_v, percwdy, grssfrb)

odds.data$case <- as.numeric(odds.data$case)

fit17=clogit(case~ avrgmxv +
               strata(nestd_v), 
             data = odds.data)

summary(fit17)

fit18=clogit(case~ percwdy +
               strata(nestd_v), 
             data = odds.data)

summary(fit18)


fit19= clogit(case~ grssfrb +
                strata(nestd_v), 
              data = odds.data)

summary(fit19)
