#############################################
## Script to Create Nesting Home Ranges ##
#############################################

####################
## Load Packages ##
####################

require(adehabitatHR)
require(sf)
require(dplyr)
require(purrr)
require(mapview)

#################
## Data Prep ##
#################

#Read in 4D RDS file based upon movement data from nesting hens in 2022 and 2023
hens.4d <- readRDS("Data Management/RData/Individual-Specific Movement Process/Study Area RDS Files/full_all_4d.RDS") 

    hens.4d <- hens.4d %>% dplyr::mutate(long = unlist(map(hens.4d$geometry,1)),
           lat = unlist(map(hens.4d$geometry,2))) %>%
    dplyr::rename("BirdID"=individual_local_identifier) %>%
    dplyr::select(BirdID, timestamp,long, lat) 

#Read in 3D RDS file based upon movement data from nesting hens in 2022 and 2023
hens.3d <- readRDS("Data Management/RData/Individual-Specific Movement Process/Study Area RDS Files/full_all_3d.RDS") 
  
hens.3d <- hens.3d %>% dplyr::mutate(long = unlist(map(hens.3d$geometry,1)),
                lat = unlist(map(hens.3d$geometry,2))) %>%
  dplyr::rename("BirdID"=individual_local_identifier) %>%
  dplyr::select(BirdID, timestamp,long, lat) 

#Read in 2D RDS file based upon movement data from nesting hens in 2022 and 2023
hens.2d <- readRDS("Data Management/RData/Individual-Specific Movement Process/Study Area RDS Files/full_all_2d.RDS") 

  hens.2d <- hens.2d %>% dplyr::mutate(long = unlist(map(hens.2d$geometry,1)),
                lat = unlist(map(hens.2d$geometry,2))) %>%
  dplyr::rename("BirdID"=individual_local_identifier) %>%
  dplyr::select(BirdID, timestamp,long, lat) 

#Read in 5C RDS file based upon movement data from nesting hens in 2022 and 2023
hens.5c <- readRDS("Data Management/RData/Individual-Specific Movement Process/Study Area RDS Files/full_all_5c.RDS") 
  
hens.5c <- hens.5c %>% dplyr::mutate(long = unlist(map(hens.5c$geometry,1)),
                lat = unlist(map(hens.5c$geometry,2))) %>%
  dplyr::rename("BirdID"=individual_local_identifier) %>%
  dplyr::select(BirdID, timestamp,long, lat) 

#Read in nest veg csv with available nests
pa.nests <- read.csv("Data Management/Csvs/processed data/Nests/nests_22_23_clean.csv") %>%
  st_as_sf(coords= c("long_n", "lat_n"), crs=4326) %>%
  st_transform(32618)
  

#Create object with data from each study area
full_all <- rbind(hens.2d, hens.3d, hens.4d, hens.5c) %>%
  as.data.frame()

#########################
## Create Projections ##
#########################

#Change to sp (Fuck sp)
#Need a class SpatialPointsDataFrame to use mcp function in adehabitatHR package
loc <- data.frame("x"=full_all$long,"y"=full_all$lat)

turkey.spdf <- SpatialPointsDataFrame(coords=loc,data=full_all,
                                      proj4string = CRS(SRS_string = "EPSG:4326")) 

turkey.spdf_utm <- turkey.spdf %>%
  spTransform(CRS("+proj=utm +zone=18 +datum=WGS84"))

turkey.spdf_utm$BirdID <- as.factor(turkey.spdf_utm$BirdID)

#Create mcps based upon individual turkeys, 1st column is for BirdID
#Need that unique identifier
cp <- mcp(turkey.spdf_utm[,1], percent=95, #(95% is the default)
)

#The size of the bounding polygon
hens.hr <-as.data.frame(cp) %>%
  dplyr::mutate("num_hr"=nrow(1:216))

plot(cp)

#Home range areas
areas <-mcp.area(turkey.spdf[,1], percent = 95,
                 unin = "km",
                 unout = "km2")

##################
## Check HRs ##
#################

#Check 1
points.4255_1 <- dplyr::filter(full_all, BirdID=="4255_1") %>%
  st_as_sf(coords=c("long", "lat"), crs=4326)

nest.4255 <- dplyr::filter(pa.nests, nestid =="4255_2022_1")

mapview(cp[1,]) + mapview(points.4255_1) + mapview(nest.4255, col.regions="red")

#Check 2
points.4419_1 <- dplyr::filter(full_all, BirdID=="4419_1") %>%
  st_as_sf(coords=c("long", "lat"), crs=4326)

nest.4419 <- dplyr::filter(pa.nests, nestid =="4419_2022_1")
  
mapview(cp[5,]) + mapview(points.4419_1) +mapview(nest.4419, col.regions="red")

#Check 3
points.4603_1 <- dplyr::filter(full_all, BirdID=="4603_1") %>%
  st_as_sf(coords=c("long", "lat"), crs=4326)

nest.4603 <- dplyr::filter(pa.nests, nestid =="4603_2023_1")

mapview(cp[6,]) + mapview(points.4603_1)+mapview(nest.4603, col.regions="red")

#Check 4
points.4613_1 <- dplyr::filter(full_all, BirdID=="4613_1") %>%
  st_as_sf(coords=c("long", "lat"), crs=4326)

nest.4613 <- dplyr::filter(pa.nests, nestid =="4613_2023_1")

mapview(cp[9,]) + mapview(points.4613_1)+mapview(nest.4613, col.regions="red")

#Check 5
points.4698_1 <- dplyr::filter(full_all, BirdID=="4698_1") %>%
  st_as_sf(coords=c("long", "lat"), crs=4326)

nest.4698 <- dplyr::filter(pa.nests, nestid =="4698_2022_1")

mapview(cp[15,]) + mapview(points.4698_1)+mapview(nest.4698, col.regions="red")

#Check 6
points.4660_2 <- dplyr::filter(full_all, BirdID=="4660_2") %>%
  st_as_sf(coords=c("long", "lat"), crs=4326)

nest.4660 <- dplyr::filter(pa.nests, nestid =="4660_2023_2")

mapview(cp[12,]) + mapview(points.4660_2)+mapview(nest.4660, col.regions="red")

