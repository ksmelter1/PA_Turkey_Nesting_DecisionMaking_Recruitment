#'---
#' title: Extraction of precipitation, tmin, tmax, tmean, and snow water equivalent from Daymet
#' author: "K. Smelter, F. Buderman", "K. Bondo", "D. Walter"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: 5m x 5m daily resolution rasters under Daymet tab in one drive
#'   html_document: 
#'     toc: true
#'---
#'
#+ include = FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE, cache = TRUE)
#'  
#' **Purpose**: This script downloads weather covariates in a daily temporal resolution to be used in the nest sucess model
#'
#'

####################
## Load Packages ##
####################

require(foreach)
require(daymetr) #https://cran.r-project.org/web/packages/daymetr/vignettes/daymetr-vignette.html
require(rgdal)
require(aweek)
require(lubridate)
require(fasterize)
require(ncdf4)
require(sf)
require(terra)



#############################
## Specify Tiles and Path ##
#############################

#' Daymet tiles for Pennsylvania, Maryland and New Jersey
tiles.final=c(11751, 11752, 11753, 11750, 11571, 11572, 11573, 11753, 11570)

#' Preliminary data is organized in separate folders based upon category in daymet folder
nc_path <- file.path("Data Management/Rasters/daymet/nc_files/")


#####################
## Download Data ##
#####################

#' Download Daily Minimum Temperature
 for(i in 2022:2023){
  lapply(tiles.final, function(x)
    download_daymet_tiles(tiles = x,
                          start = i,
                          end = i,
                          param = "tmin",
                          path = nc_path))
}

#' Download Daily Maximum Temperature
  for(i in 2022:2023){
  lapply(tiles.final, function(x) 
    download_daymet_tiles(tiles = x,
                          start = i,
                          end = i,
                          param = "tmax",
                          path =  nc_path))
  
}

#' Download Precipitation
for(i in 2022:2023){
  lapply(tiles.final, function(x)
    download_daymet_tiles(tiles = x,
                          start = i,
                          end = i,
                          param = "prcp",
                          path =  nc_path))
}


#' Download Snow Water Equivalent
#' This is for chapter 2
for(i in 2022:2023){
  lapply(tiles.final, function(x) 
    download_daymet_tiles(tiles = x,
                          start = i,
                          end = i,
                          param = "swe",
                          path =  nc_path)) 
}


################################################################################
################################################################################

#' Years of data to build as .tif
years=c(2022:2023)

#' List of files with the .nc designation 
#' Creates a file list across years
file_list <- list.files(nc_path, 
                        pattern = '.*nc$')

#' Function to create daymet variables by year
#' Doesn't run
create.daymet.variables.per.year <- function(index.years, pattern) {
  #' Create a list of files that all end with the pattern .nc
  file_list_2 <- grep(file_list, pattern = '.*nc$', value = TRUE)
  
  #' Creating temporary files for function
  temp.files <- unlist(lapply(years[index.years], function(x) grep(file_list_2, pattern = '.*nc$', value = TRUE)))
  
  #' Preparing files and using the terra package to create rasters
  all.prep <- lapply(temp.files, function(y) terra::rast(paste0("Data Management/Rasters/daymet/nc_files/", y)))
  
  #' Project and set crs for rasters
  for(j in 1:length(all.prep)) {
    terra::crs(all.prep[[j]]) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  }
  
  # Merge rasters
  temp <- lapply(1:nlyr(all.prep[[1]]), function(y) {
    rasters_to_merge <- lapply(all.prep, function(x) x[[y]])
    do.call(merge, rasters_to_merge)
  })
  
  #' Stacking rasters
  final.i=do.call(stack, temp)
  
  #' Write the raster
  output_filename <- paste0("Data Management/Rasters/daymet/daymet_tifs/", pattern, "_", years[i], ".tiff")
  terra::writeRaster(final.i, output_filename, filetype = "tiff", overwrite = TRUE)
}


################################################################################
################################################################################

#' Other function to track the dates
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#-------------------------------#
# ************ TMIN *********** #
#-------------------------------#


 start=Sys.time()

#' create the  tmin.tif per year 
foreach(i=seq_along(c(1:length(years))), .packages = "terra") %do% {
  
  create.daymet.variables.per.year(index.years=i,
                                   pattern="tmin")}

end=Sys.time()
end-start

#-------------------------------#
# ************ TMAX *********** #
#-------------------------------#

#' create the  tmin.tif per year 
#start=Sys.time()

foreach(i=seq_along(c(1:length(years))), .packages = "terra") %do% {
  
  create.daymet.variables.per.year(index.years=i,
                                   pattern="tmax")}

  #-------------------------------------------------------#
  # **** CONSTRUCT THE MEAN TEMPERATURE TIFS PER DAY **** #
  #-------------------------------------------------------#
  
  years=c(2022:2023)
  
  #' paths for tmax
  paths_max=paste0("Data Management/Rasters/daymet/daymet_tifs/TMax/", paste0("tmax_", c(2022:2023), ".tif")) 
  
  
  #' paths for tmin
  paths_min=paste0("Data Management/Rasters/daymet/daymet_tifs/TMin/", paste0("tmin_", c(2022:2023), ".tif")) 
  
  
  #' open tmax files
  tmax.per.year=lapply(paths_max, function(x) rast(x))
  
  #' make a single stack
  tmax.full=rast(c(tmax.per.year))
  
  #' open tmin files
  tmin.per.year=lapply(paths_min, function(x) rast(x))
  
  #' make a single stack
  tmin.full=rast(c(tmin.per.year))
  
  #' list each layer of the tmin and tmax objects
  
  tmax.each.day=lapply(1:nlyr(tmax.full), function(x) tmax.full[[x]])
  
  tmin.each.day=lapply(1:nlyr(tmin.full), function(x) tmin.full[[x]])
  
  
  tmean.per.year<-vector(mode = "list", length = length(years))
  
  #' get the mean per day
  
  for(i in 1:length(years)){
    
    tmean.per.year[[i]]=c(
      lapply(
        lapply(c(1:nlyr(tmax.per.year[[1]])), function(x)
          c(tmax.per.year[[i]][[x]],
            tmin.per.year[[i]][[x]])), function(y)
              
              merge(y, fun=mean)))
  }                                        
  
  lapply(c(1:length(tmean.per.year)), function(x)
    writeRaster(tmean.per.year[[x]],  paste0(
      "~/Spatial_Disease_Modeling/1_Raw_Data/Daymet_tifs/tmean_", c(2017:2020)[x], ".tif"), 
      filetype='GTiff', overwrite=TRUE, options=c("INTERLEAVE=BAND","COMPRESS=LZW")))
  
  
  #-------------------------------#
  # ******* PRECIPITATION ******* #
  #-------------------------------#
  
  #' create the .tif per year 
  
  #' start=Sys.time()
  
  foreach(i=seq_along(c(1:length(years))), .packages = "terra") %do% {
    
    create.daymet.variables.per.year(index.year=i,
                                     pattern="prcp")}
  
  
  #' end=Sys.time()
  #' end-start
  
  
  #-------------------------------#
  # ************ SNOW *********** #
  #-------------------------------#
  
  #' create the .tif per year 
  
  #' start=Sys.time()
  
  
  foreach(i=seq_along(c(1:length(years))), .packages = "terra") %do% {
    
    create.daymet.variables.per.year(index.year=i,
                                     pattern="swe")}
  
  
  #' end=Sys.time()
  # 
  #' end-start
  
  
  # --------------------------------------------------------------------------------------------------#
  # 3) Construct Rasters of 1 Km Resolution                                                           # 
  #                                                                                                   #
  # --------------------------------------------------------------------------------------------------#
  
  #' Make a sub-folder in project directory to save Daymet raster files with 5 km resolution
  subfolder3 <- "Spatial_Disease_Modeling/2_Spatial_Data_Ready_To_Use/DayMet_5KM_Rasters"
  dir.create(file.path(working.directory, subfolder3), recursive=TRUE)
  
  # -----------------------------------------------------------------#
  # First, make a template, meaning a grid polygon of 5km resolution #
  # -----------------------------------------------------------------#
  
  #' The rasters are changed to a larger resolution by obtaining the weighted mean of each variable in the larger grid cell based on the values in the smaller grid cells 
  #' and by accounting for the surface of the smaller grid cells that overlap the larger grid cells.
  
  nad83.2011.pa.north="+proj=lcc +lat_1=41.95 +lat_2=40.88333333333333 +lat_0=40.16666666666666 +lon_0=-77.75 +x_0=600000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
  
  #' Chosen resolution:
  resolution=1000
  
  
  states <- tigris::states(resolution = "5m",
                           class = "sf",
                           progress_bar = FALSE)
  
  PA=states[states$STUSPS=="PA",]
  
  
  template=st_buffer(st_transform(PA, crs(nad83.2011.pa.north)), dist = 10000) # template 10 km out of PA borders
  
  #' Create raster of specified resolution and projection
  grid.template <- rast(ext(template), 
                        res = c(resolution,resolution),
                        crs =  crs(template))
  
  #' Move it to a spatial object (before it was a empty raster)
  gridPolygon <- as.polygons(grid.template)
  template <- st_as_sf(gridPolygon)[template,]
  
  ncell(template) # 5555 cells
  
  #' Check point: plot the template, PA
  plot(template)
  plot(st_transform(PA, nad83.2011.pa.north), add=T, border="red", lwd=2)
  
  
  #' Make the 5km resolution grid with the mean values of each environmental variable.
  
  # ----------------- #
  # ***** TMEAN ***** #
  # ------------=---- #
  
  paths=paste0("~/Spatial_Disease_Modeling/1_Raw_Data/Daymet_tifs/", paste0("tmean_", c(2017:2020), ".tif")) # paths in the cluster #Change working directory to your computer
  
  #' open rasters. These are grouped per month (30 to 31 rasters per month depending on the month)
  
  tmean.per.year=lapply(paths, function(x) rast(x))
  
  #' Reproject the raster as nad83.2011.pa.north and with the desired larger resolution
  tmean.repro.5km=lapply(tmean.per.year, function(y)
    lapply(c(1:nlyr(y)), function(x)
      project(y[[x]],  grid.template, method='bilinear')))
  
  
  tmean.repro.5km2=c(tmean.repro.5km,rast, warn=TRUE)
  
  #' Mask to the extent of the study area
  tmean.repro.5km3=lapply(mask(c(tmean.repro.5km, template)))
  
  
  #' Save the daily 5 km grid cell tmin rasters (365*4 = 1460 rasters)
  
  #' start=Sys.time()
  writeRaster(tmean.repro.5km, paste0(
    "~/Spatial_Disease_Modeling/2_Spatial_Data_Ready_To_Use/Daymet_5km_Rasters/daily_mean_temp_5km_all_PA_day",
    c(1:length(nlayers(tmean.repro.5km)))),
    bylayer=TRUE,
    filetype='GTiff', overwrite=TRUE, options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
  
  # ------------------------ #
  # ***** PRECIPITAION ***** #
  # ------------------------ #
  
  paths=paste0("~/Spatial_Disease_Modeling/1_Raw_Data/Daymet_tifs/", paste0("prcp_", c(2017:2020), ".tif")) # paths in the cluster
  
  #' Open rasters. These are grouped per month (30 to 31 rasters per month depending on the month)
  
  precip.per.year=lapply(paths, function(x) rast(x))
  
  precip.repro.5km=lapply(precip.per.year, function(y)
    lapply(c(1:nlyr(y)), function(x)
      project(y[[x]],  grid.template, method='bilinear')))
  
  
  precip.repro.5km2=c(precip.repro.5km, rast, warn = TRUE)
  
  #' Mask to the extent of the study area
  precip.repro.5km3=(mask(c(precip.repro.5km, template)))
  
  
  #' Save the daily 5 km grid cell tmin rasters (365*4 = 1460 rasters)
  
  #' start=Sys.time()
  writeRaster(precip.repro.5km, paste0(
    "~/Spatial_Disease_Modeling/2_Spatial_Data_Ready_To_Use/Daymet_5km_Rasters/daily_mean_precip_5km_all_PA_day",
    c(1:length(nlyr(precip.repro.5km)))),
    bylayer=TRUE,
    filetype='GTiff', overwrite=TRUE, options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
  
  
  # --------------------------------- #
  # ***** Snow Water Equivalent ***** #
  # --------------------------------- #
  
  paths=paste0("~/Spatial_Disease_Modeling/1_Raw_Data/Daymet_tifs/", paste0("swe_", c(2017:2020), ".tif")) # paths in the cluster
  
  snow.per.year=lapply(paths, function(x) rast(x))
  
  
  snow.repro.5km=lapply(snow.per.year, function(y)
    lapply(c(1:nlyr(y)), function(x)
      projectRaster(y[[x]],  grid.template, method='bilinear')))
  
  
  snow.repro.5km2=c(snow.repro.5km, rast, warn= TRUE)
  
  #' Mask to the extent of the study area
  snow.repro.5km3=mask(c(snow.repro.5km, template))
  
  
  #' Save the daily 5 km grid cell tmin rasters (365*4 = 1460 rasters)
  
  #' start=Sys.time()
  writeRaster(snow.repro.5km, paste0(
    "~/Spatial_Disease_Modeling/2_Spatial_Data_Ready_To_Use/Daymet_5km_Rasters/daily_mean_snow_5km_all_PA_day",
    c(1:length(nlyr(snow.repro.5km)))),
    bylayer=TRUE,
    filetype='GTiff', overwrite=TRUE, options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
  
  
  #################################################################
  # Plot a single layer of the 5 km raster with the study area #
  #################################################################
  
  test <- rast("~/Spatial_Disease_Modeling/2_Spatial_Data_Ready_To_Use/Daymet_5km_Rasters/daily_mean_precip_5km_all_PA_day1_1.tif")
  
  plot(test)
  plot(template, col=NA, add=T)
  plot(st_transform(PA), crs = st_crs(test), add=T, border="white")
  plot(mask(test, template))
  plot(template, add=T, col=NA)
  
  