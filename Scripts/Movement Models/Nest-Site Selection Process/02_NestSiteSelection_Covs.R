
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
#' **Purpose**: This script calculates the distance from each 30m x 30m raster cell to the nearest road and extracts elevation data from DEM


####################
## Load Packages ##
####################

require(dplyr)
require(terra)
require(FedData)

###########################################
## Nest Distance from Road Calculations ##
###########################################

#' Read in road raster created in individual movement process 
#' This measures the distance from each 30m x 30m cell to the nearest road
dist <- terra::rast("Data Management/Rasters/PA Roads/paroadrast.tiff")

#' Extract distance from road outputs from raster
dist.outputs <- terra::extract(dist, pa.nests.prop, fun=min)

#' Bind columns together to create dataframe 
pa.nests.prop <- cbind(dist.outputs, pa.nests.prop) %>%
  dplyr::rename("distfromroad"= layer) %>%
  dplyr::select(-ID)
#' Write csv
#write.csv(dist2road, "Csvs/dist2road.out.csv")
################################################################################

#################
## Elevation ##
################

#' Download elevation data following the pa.outline 
DEM <- FedData::get_ned(template = pa.outline, 
                        res = 1, label = 'dem', force.redo = T )
plot(DEM) #Check

#' Extract elevation values from DEM for each of the non-buffered nest locations
pa.nests.elev <- terra::extract(DEM, pa.nests)

#' Create dataframe for analysis
pa.nests.prop.f <- cbind(pa.nests.prop, pa.nests.elev) %>%
  dplyr::rename("elev"= Layer_1) %>%
  dplyr::select(-ID)

saveRDS(pa.nests.prop.f, "nest-siteselection.RData")
