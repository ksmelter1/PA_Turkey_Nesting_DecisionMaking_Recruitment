
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
#' **Purpose**: This script obtains Pennsylvania NLCD data and creates buffers for each used and available nest to extract the proportion of landcover within

#######################
## Load Packages ##
#######################

require(tigris)
require(FedData)
require(dplyr)
require(terra)
require(sf)
require(exactextractr)
require(mapview)
require(tidyr)
require(zoo)

##########################
## Download NLCD Data ##
##########################

#' Use the tigris package to filter through states and remove GEOIDs above 60
#' GEOIDs above 60 are territories and islands so they are being removed for scaling
st <- tigris::states() 

#' Transform to albers equal area conic projection, epsg code is 5070
st <- st_transform(st, 5070)

#' Grab outline of PA
pa.outline <- subset(st, st$NAME=="Pennsylvania")

#' Grab outline of the Mid-Atlantic Region 
#' mar.outline <- subset(st, st$NAME== "Pennsylvania"| st$NAME== "Maryland"| st$NAME == "New Jersey") 

#' Obtain Pennsylvania NLCD raster
pa.nlcd <- FedData::get_nlcd(template= pa.outline, year = 2021, 
                             label = 'pa', 
                             force.redo = T)

###########################
## Landcover Analysis ##
###########################

#' Will be able to extract nest locations from entire region so no separate code like distance to road below
#' Read in raster with NLCD data from the Mid-Atlantic Region 
#' May save space later and not write these out 
#mar.nlcd <- terra::rast("rasters/nlcd/MAR_NLCD.tiff")
#pa.nlcd <- terra::rast("rasters/nlcd/PA.NLCD.tiff")

#1= Developed 
#2= Forest
#3= Agriculture

#' Reclassify NLCD -- Skyrockets RAM
#' See covertyps for NLCD here: https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description
terra::values(pa.nlcd) <- ifelse(terra::values(pa.nlcd) %in% c(21:24), yes = "Developed", ## Developed Open, low, medium, high intensity
                                no = ifelse(terra::values(pa.nlcd) %in% c(41:43), yes = "Forest", ## Deciduous, Evergreen, Mixed Forest -- Everything else is Non
                                            no = ifelse(terra::values(pa.nlcd) %in% c(81:83), yes = "Agriculture", no="NonForest"))) ## Pasture, Cultivated Crops

#' Code to classify land cover types into groups 1-4
#' Essentially I wanted to be sure each land cover types matched so I 3 created groups
m <- c(21, 24, 1, 41,43, 2, 81, 83, 3)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- terra::classify(pa.nlcd, rclmat)
plot(rc) #Check

#' Read in nests shapefile
#' Transform to albers 
#' Rename unique identifier column 
pa.nests <- st_read("Data Management/Shapefiles/nests/nests and veg/nest.veg.locations_cleaned1.shp") %>%
  sf::st_transform(5070) %>%
  dplyr::rename("uniqueid"=uniquID) 

#' Buffer nests by 60m
nests_buffered <- sf::st_buffer(pa.nests, 60)
mapview(nests_buffered) #Check

#' Create dataframe that has buffered nest values 
#' Extract land cover type values using the exact extractr in R
nests.buffered.values <- exactextractr::exact_extract(rc, st_as_sf(nests_buffered))
head(nests.buffered.values[[1]])#Percent in each category (buffers 1-6)
#' Create a table of land cover types within each buffer
et=lapply(nests.buffered.values,table)

#' Create proportions list object using for loop
prop <- list()
for(i in 1:length(nests.buffered.values)[1] ){
  prop[[i]] <- round((margin.table(et[[i]],1)/margin.table(et[[i]])),digits = 6)
}

M <- coredata(do.call(cbind, lapply(prop, zoo)))
colnames(M) <- NULL
#' Transpose matrix so land cover become separate columns of data
matrix <- t(M)
#' Now convert the matrix to a data frame so it is easier to manipulate
dfland <- as.data.frame(matrix)
head(dfland) #Check
#' Assing column names to land cover
colnames(dfland) <- c("Developed","Forest", "Agriculture", "NonForest")
head(dfland)

#' Function to fill dataframe NA values with 0.000001
fill_NA_with_value <- function(df, value = 0.000001) {
  df[is.na(dfland)] <- 0.000001
  return(df)
 }

#' Apply the function to fill NA values with 0.0001
dfland.final <- fill_NA_with_value(dfland) %>%
  dplyr::select(-NonForest)

#' Write csv
#write.csv(dfland.final, "dfland.final.csv")


#' Create nests dataframe with proportions
pa.nests.prop <- cbind(pa.nests, dfland.final)






