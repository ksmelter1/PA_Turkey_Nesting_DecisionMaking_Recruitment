---
  title: "ODBA vs Observers Nesting Example"
author: "David Wolfson"
output: html_document
---
  
  
 # ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, 
                      out.width='100%')
knitr::opts_chunk$set(cache= TRUE, warning= FALSE, message = FALSE)
options(digits = 3)
options(scipen = 999)
``

#```{r theme_ggplot}
library(ggplot2)

my_theme<-function(base_size = 14) {
  theme_bw(base_size = base_size,
           base_family = "serif") %+replace%
    theme(
      # The whole figure
      plot.title = element_text(size = rel(1), face = "bold",
                                margin = margin(0,0,5,0), hjust = 0.5),
      
      # Area where the graph is located
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      
      # The axes
      axis.title = element_text(size = rel(0.85), face = "bold"),
      axis.text = element_text(size = rel(0.70), face = "bold"),
      axis.line = element_line(color = "black"),
      
      # The legend
      legend.title = element_text(size = rel(0.85), face = "bold"),
      legend.text = element_text(size = rel(0.70), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      
      # The labels in the case of facetting
      strip.background = element_rect(fill = "#17252D", color = "#17252D"),
      strip.text = element_text(size = rel(0.85), face = "bold",
                                color = "white", margin = margin(5,0,5,0))
    )
}

# Change the default theme
theme_set(my_theme())

#```

#```{r load libraries}
# package names
packages<-c("tidyverse", "here", "mcp", "lubridate", "knitr", "ezknitr", "loo", "flextable")

# install any packages not previously installed
installed_packages<-packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))
#```

#Load observation data
#```{r load observation data}
obs<-read_csv(here("Data Management/Csvs/Misc/all_0A_obs.csv"))
head(obs)
#```

#Load raw accelerometry data
#```{r}
df<-read_csv(here("Data Management/Csvs/Misc/0A_sensor_data.csv"))
head(df)
#```

#### Deal with bursts. 
#The current transmitter settings are GPS fixes every 15 minutes throughout the 24 hour cycle and ACC fixes every 5 minutes with a 10Hz 3 second burst.   

#Convert to POSIXct format
#```{r}
df$UTC_datetime<-ymd_hms(df$UTC_datetime)
#```

#Filter out data before ACC sensors were activated and from the start of observations of nesting
#```{r}
df<-df %>% filter(UTC_datetime>"2021-04-24")
#```

#Assign a burst ID to all rows
#```{r}
df<-df %>% 
  group_by(five_min_burst=cut(UTC_datetime, "5 min")) %>% 
  mutate(burst_row_number=1:n(),
         burst_ID=cur_group_id())
#```


#Convert acc units into g-force
#```{r}
df$acc_x_g<-df$acc_x/1024
df$acc_y_g<-df$acc_y/1024
df$acc_z_g<-df$acc_z/1024
#```

#Calculate ODBA
#```{r}
df<-df %>% 
  group_by(burst_ID) %>% 
  mutate(ODBA=sum(abs(acc_x_g-mean(acc_x_g)),abs(acc_y_g-mean(acc_y_g)),abs(acc_z_g-mean(acc_z_g))))

df$julian_day<-yday(df$UTC_datetime)
#```

#Average hourly ODBA
#```{r}
df<-df %>% 
  mutate(hourly_burst=cut(UTC_datetime, "1 hour")) %>% 
  group_by(hourly_burst) %>% 
  mutate(hr_odba=mean(ODBA))

df$time<-ymd_hms(df$hourly_burst)
#```


#Subset to just 1 value per hour
#```{r}
hourly<-df %>% 
  distinct(hourly_burst, hr_odba)
#```


#### Join nesting status observations 
#```{r}
obs$Day<-as.Date(obs$datetime, format="%m/%d/%Y")
obs<-obs %>% 
  mutate(date_time=ymd_hms(paste(Day,time)))
obs$Jday<-yday(obs$date_time)
obs$hour<-hour(obs$date_time)

hourly$date<-ymd_hms(hourly$hourly_burst)
hourly$Jday<-yday(hourly$date)
hourly$hour<-hour(hourly$date)


hourly<-left_join(x=hourly, y=obs, by=c("Jday", "hour"))
#```
#The marked swan was consistently seen incubating the next from April 25 - May 13. On May 25th and May 28th the collared swan was not seen, but was presumed to be on the nest. June 2nd was the first date that cygnets were observed. Ill take the midpoint between those two times as the estimate of hatch date from human observers.


#Plot both datasets with aligned x-axes.
#```{r}
hourly$hours_since_start<-as.numeric(difftime(hourly$date, hourly$date[1], units="hours"))

raw1<-ggplot(hourly, aes(hours_since_start, hr_odba)) + geom_point() + 
  labs(x="Hourly time intervals", y="Average hourly ODBA value", title="Accelerometry Dataset")+
  my_theme()

obs$new_date<-mdy_hms(paste(obs$datetime, obs$time, sep=" "))
first_cyg<-head(obs[obs$status=="Cygnets","new_date"],1)
last_nest<-tail(obs[obs$status=="On nest","new_date"],1)

hatch<-as.POSIXct((as.numeric(first_cyg$new_date)+
                     as.numeric(last_nest$new_date))/2,
                  origin="1970-01-01")

raw2<-ggplot(hourly, aes(date, ID)) +
  geom_point(data=filter(hourly, is.na(status) !=TRUE), aes(date, col = status), size =4)+
  geom_vline(aes(xintercept = hatch),linetype="dashed", size=1.5, color="purple")+
  labs(x="Date", color="Nesting status", title="Visual Observations Dataset")+
   scale_color_manual(labels=c("Cygnets present","Incubating"), values=c("blue", "orange"))+
  my_theme()+
  theme(legend.position = c(0.1,0.9))
raw2
cowplot::plot_grid(raw1, raw2, align = "h", axis="l", ncol=1)
#```


#### Use mcp to detect the breakpoint for the time of nest hatching

#Models can take some time to fit, so the following 2 chunks won't be run in this .Rmd, and instead I'll import a saved model object.
#```{r, eval=F}
hourly<-drop_na(hourly, "hours_since_start")

m0<-list(
  hr_odba~1)

m1<-list(
 hr_odba~1,
        ~1
)

m2<-list(
  hr_odba~1,
         ~1,
         ~1
)

f0<-mcp(m0,hourly[,c("hr_odba", "hours_since_start")],
         par_x="hours_since_start")

f1<-mcp(m1, hourly[,c("hr_odba", "hours_since_start")],
         par_x="hours_since_start")

f2<-mcp(m2,hourly[,c("hr_odba", "hours_since_start")],
         par_x="hours_since_start")



#{r loo, eval=F}
mods<-list(f0, f1, f2)
for(i in 1:length(mods)){
  mods[[i]]$loo<-loo(mods[[i]])
}

loo_compare(mods[[1]]$loo,
            mods[[2]]$loo,
            mods[[3]]$loo)

#R model comparison
#mods<-readRDS(here("DRUM_Materials/!revised_materials/ODBA_nesting/saved_models/all_with_loo.rds"))

loos<-loo_compare(mods[[1]]$loo, mods[[2]]$loo, mods[[3]]$loo)
loo_df<-as.data.frame(loos[1:3, 1:2])
loo_df<-cbind.data.frame('Model Syntax'=c("Two Intercepts", 
                                          "Three Intercepts",
                                          "One Intercept"), loo_df)


loo_df %>%  flextable() %>% 
  colformat_double(digits=1) %>% 
  set_header_labels(values=list("Model Syntax" = "Model Syntax", 
                         "elpd_diff" = "ELPD Difference", 
                        "se_diff" = "SE of Difference")) %>% 
  set_caption(caption = "Leave-one-out Cross Validation was used to compute the Estimated Log Predictive Density (ELPD) of three different models; one fit with a single intercept, one with two intercepts, and one with three intercepts. The higher ELPD for the model with two intercepts indicates that it has a higher predictive accuracy.") %>% 
  border_outer() %>% 
  vline(part="header") %>% 
  align(align ='center', part = 'all') %>% 
  bg(part="header", bg="#dbdbdb") %>% 
  bold(part="header") %>%  
  set_table_properties(layout="autofit")
#```

#Top model has a single breakpoint and two segments with separate intercepts for activity levels.
#```{r}
summary(mods[[2]])
plot_pars(mods[[2]])
#```


#In the the figure below, the vertical purple dashed line is the estimated hatch time based on visual observations, and the vertical brown solid line is the estimated hatch time based on the piecewise regression model fit. Therefore the piecewise regression model is able to detect the breakpoint at the time of nest hatch when activity transitions from a lower state to a higher state as incubation ends.
#```{r}
invisible({capture.output({out<-summary(mods[[2]])})})
mu<-out$mean[[1]]

invisible({capture.output({fitted<-as.data.frame(fitted(mods[[2]]))})})
names(fitted)[[2]]<-"hr_odba"
fitted$date<-as.POSIXct(fitted$hours_since_start*60*60, 
                        origin = "2021-04-24 05:00:00")
#```

#```{r eval=F}
ggplot(hourly, aes(date, hr_odba)) + geom_point() + 
  labs(x="Date", y="Average hourly ODBA value", color="Nesting Status")+
  geom_line(data=fitted,aes(date, hr_odba), color="grey", size=2)+
  geom_errorbar(data=fitted, aes(ymin=Q2.5, ymax=Q97.5), color="red", 
                linetype="dashed", size=0.4)+
  geom_rug(data=filter(hourly, is.na(status) !=TRUE), 
           aes(date, hr_odba, col = status), 
           size =2, sides="b")+
  scale_color_manual(labels=c("Cygnets present","Incubating"), values=c("blue", "orange"))+
  geom_vline(aes(xintercept = hatch), linetype="dashed", size=1.5, color="purple")+
    geom_vline(aes(xintercept = as.POSIXct(mu*3600, 
                                  origin = "2021-04-24 05:00:00")),
                   linetype="solid", color="brown", size=1.5)+
  my_theme()+
  theme(legend.position = c(0.1, 0.9))+

  annotate("segment", x=as.POSIXct(mu*3600, origin = "2021-04-24 05:00:00")+10000,
           xend= as.POSIXct(mu*3600, origin = "2021-04-24 05:00:00")+750000, y=20,
           yend=22.5 , color="brown", size=2)+
  
  annotate(x=as.POSIXct(mu*3600, origin = "2021-04-24 05:00:00")+750000,
           y=+Inf, label="Piecewise Regression", vjust=2, geom="label", color="brown")+
   
  annotate("segment", x=hatch-500000, xend=hatch-10000, y=22.5, yend=20, color="purple", size=2)+
  annotate(x=hatch-750000, y=+Inf, label="Visual Observations", vjust=2, geom="label", color="purple")


#{r nesting2 fig}
knitr::include_graphics(here("writing/methods_paper/images/nesting2.jpg"))
#```


#```{r}
sessionInfo()
