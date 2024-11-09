
#'---
#' title: Habitat selection of female wild turkeys during incubation (an SSF analysis)
#' author: "K. Smelter, F. Buderman"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: RandomSteps_Prep(R Workspace)
#'   html_document: 
#'     toc: true
#'---
#'
#+ include = FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE, cache = TRUE)
#'  
#' **Purpose**: This script creates tracks and extracts covariates using the amt package
#'

#####################
## Load Packages ##
#####################


#' Vector of package names
packages <- c("sf",
              "amt",
              "tigris",
              "FedData",
              "dplyr",
              "terra",
              "ggplot2")

#' Function to load a package or install it if not already installed
load_packages <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    require(package_name, character.only = TRUE)
  }
}

#' Apply the function to each package name
lapply(packages, load_packages)

###############################
## Prepare NLCD Covariates ##
###############################

#' GEOIDs above 60 are territories and islands so they are being removed for scaling
st <- tigris::states() 
#' 
#' Transform to albers equal area conic projection, epsg code is 5070
 st <- st_transform(st, 5070)
#' 
#' Grab outline of PA
pa.outline <- subset(st, st$NAME=="Pennsylvania") %>%
       st_transform(5070)
 
#' Obtain Pennsylvania NLCD raster
 pa.nlcd <- FedData::get_nlcd(template= pa.outline, year = 2019,
                              label = 'pa', 
                              force.redo = T)
 
#' Reclassify NLCD -- Skyrockets RAM
#' See covertyps for NLCD here: https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description
 terra::values(pa.nlcd) <- ifelse(terra::values(pa.nlcd) %in% c(21:24), yes = "Developed", ## Developed Open, low, medium, high intensity
                                  no = ifelse(terra::values(pa.nlcd) %in% c(41), yes = " Deciduous Forest", ## Deciduous
                                              no= ifelse(terra::values(pa.nlcd) %in% c(42), yes= "Evergreen Forest", ## Evergreen Forest
                                                         no= ifelse(terra::values(pa.nlcd) %in% c(43), yes= "Mixed Forest", ## Evergreen Forest
                                                          no= ifelse(terra::values(pa.nlcd) %in% c(81:83), yes= "Agriculture", ## Pasture, Cultivated Crops
                                                                     no= "NonForest")))))#Everything else
#' #' Crop PA NLCD raster to the Pennsylvania outline
#' pa.nlcd <- crop(pa.nlcd, vect(pa.outline))
#' #' Mask the PA NLCD raster according to the Pennsylvania outline 
#' pa.nlcd <- mask(pa.nlcd, vect(pa.outline))
#' 
#' #' Save raster
#' #writeRaster(pa.nlcd, "paNLCD.tiff", overwrite=T)

#' Load Raster
pa.nlcd <- terra::rast("Data Management/Rasters/nlcd/paNLCD.tiff")

#################
## Elevation ##
################

#' Download elevation data following the pa.outline 
DEM <- FedData::get_ned(template = pa.outline, 
                        res = 1, label = 'dem', force.redo = T )
plot(DEM) #' Check

#' Save raster
writeRaster(DEM,"paDEM.tiff", overwrite=T)

DEM.proj <- terra::rast("Data Management/Rasters/nlcd/paDEM.tiff") 
DEM.crs <- terra::project(DEM.proj, pa.nlcd, method= "bilinear")

##############################
## Create and Save Tracks ##
##############################

#' I follow the AMT Vignette with multiple individuals
#' https://cran.r-project.org/web/packages/amt/vignettes/p1_getting_started.html
#' 1. Load in data from movebank prep script
#' 2. Make a track using dataset with multiple individuals
#' 3. Nest the track by id
#' 4. Take one individual's data, view its sampling rate and adjust using steps by burst
#' 5. Use the map function to apply the same steps by burst parameters across the marked population

#' Load in RData
load ("Data Management/RData/Individual-Specific Movement Process/RData Files/MovementProcess_Prep.RData") 

#' Complete hen movement dataset from movebank
dat <- hens.all %>%
  dplyr::mutate("BirdID1"=BirdID)

#' Create a track
#' New amt track function is just amt::track
trk <- amt::make_track(tbl=dat, .x= long, .y=lat, .t=timestamp, id=BirdID,
                             crs= 4326) %>% amt::transform_coords(5070)
#' Check
class(trk)

#' Group the track by id and nest the track
trk1 <- trk %>% nest(data = -"id")
trk1

#' get the data for the first animal
x <- trk1$data[[1]]

#' Set criteria for steps 
#' Rate: sampling rate
#' Tolerance: the tolerance of deviations of the sampling rate
#' Steps by burst: Returns NA for zero step lengths and calculates step lengths for points on a path
#' Time of day: Was the fix taken during the day or night? we could filter out night locations
x %>% track_resample(rate = minutes(30), tolerance = minutes(5)) %>%
  amt::steps_by_burst()

#' Check
class(x)

#' Summarize sampling rate
amt::summarize_sampling_rate(x)

#' Apply the same track resampling format to each hen within the dataset
#' This done by using the map function
#' Steps dataframe is the newly created column
trk2 <- trk1 %>%
  mutate(steps = map(data, function(x) 
    x %>% amt::track_resample(rate = minutes(30), tolerance = minutes(5))
    %>% amt::steps_by_burst())) 

#' Check
class(trk2)
glimpse(trk2)

#' Visualize step length distribution following vignette
# trk2 %>% dplyr::select(id, steps) %>% unnest(cols = steps) %>% dplyr::filter(id=="8202_2022_1_1") %>%
#   ggplot(aes(sl_, fill = factor(id))) + geom_density(alpha = 0.4)

#' Create object with all used steps for analysis
stps<- trk2 %>% dplyr::select(id, steps) %>% unnest(cols = steps)

#' Check
glimpse(stps)

#' Create random steps
#' Exponential step length used due to issues with formatting gamma
#' Extract covariates at the end of each used and available step
#' Random step lengths drawn from gamma distribution
#' Random turning angles drawn from a vonmises distribution
#' Include_observed: Include all used steps in the analysis
#' Rename and organize columns
random_steps<- amt::random_steps(
  stps,
  n_control = 10,
  sl_distr = amt::fit_distr(stps$sl_, "gamma"),
  ta_distr = amt::fit_distr(stps$ta_, "vonmises"),
  include_observed = T) %>%
  amt::extract_covariates(DEM.crs, where ="end") %>%
  amt::extract_covariates(pa.nlcd, where ="end") %>%
  dplyr::rename("landuse"= Class) %>%
  dplyr::rename("elev" = Layer_1) 


#' Check
which(random_steps$step_id_==5)
table(random_steps$case_)

#' Convert land cover classifications to a categorical variable and create separate columns
random_steps$Agriculture <- ifelse(random_steps$landuse == "Agriculture", 1, 0)
random_steps$Developed <- ifelse(random_steps$landuse == "Developed", 1, 0)
random_steps$Deciduous <- ifelse(random_steps$landuse == " Deciduous Forest", 1, 0)
random_steps$Evergreen <- ifelse(random_steps$landuse == "Evergreen Forest", 1, 0)
random_steps$Mixed <- ifelse(random_steps$landuse == "Mixed Forest", 1, 0)

#' Change case to numeric
random_steps$case_ <- as.numeric(random_steps$case_)

plot(random_steps$Deciduous)
plot(random_steps$Evergreen)
plot(random_steps$Agriculture)
plot(random_steps$Mixed)

##########################
## Prediction Plots ##
#########################

#' Mixed Forest Prediction Plot
Mixed <- ggplot(random_steps, aes(x=Mixed, y= case_)) +
  stat_smooth(method="glm",
              method.args = list(family="binomial"), se=TRUE,
              fullrange=TRUE) +
  labs(x="Mixed Forest", y="Probability of Use") +
  expand_limits(x=20) +
  theme_minimal() +
  ylim(0,1)
Mixed

#' Deciduous Forest Prediction Plot
Deciduous <- ggplot(random_steps, aes(x= Deciduous, y= case_)) +
  stat_smooth(method="glm",
              method.args = list(family="binomial"), se=TRUE,
              fullrange=TRUE) +
  labs(x="Deciduous Forest", y="Probability of Use")+
  expand_limits(x=20) +
  theme_minimal() + 
  ylim(0,1)
Deciduous

#' Evergreen Forest Prediction Plot
Evergreen <- ggplot(random_steps, aes(x= Evergreen, y= case_)) +
  stat_smooth(method="glm",
              method.args = list(family="binomial"), se=TRUE,
              fullrange=TRUE) +
  labs(x="Evergreen Forest", y="Probability of Use")+
  expand_limits(x=20) +
  ylim(0,1) +
  theme_minimal()
Evergreen

#' Developed Prediction Plot
Developed <- ggplot(random_steps, aes(x= Developed, y= case_)) +
  stat_smooth(method="glm",
              method.args = list(family="binomial"), se=TRUE,
              fullrange=TRUE) +
  labs(x="Developed", y="Probability of Use")+
  expand_limits(x=20) +
  ylim(0,1) +
  theme_minimal()
Developed

#' Agriculture Prediction Plot
Agriculture <- ggplot(random_steps, aes(x= Agriculture, y= case_)) +
  stat_smooth(method="glm",
              method.args = list(family="binomial"), se=TRUE,
              fullrange=TRUE) +
  labs(x="Agriculture", y="Probability of Use")+
  ylim(0,1) +
  expand_limits(x=20) +
  theme_minimal()
Agriculture



