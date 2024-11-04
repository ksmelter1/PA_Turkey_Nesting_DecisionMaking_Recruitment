#################################################################################
## This Script will Organize NLCD and Road Data for the Macroscale Analysis ##
#################################################################################

###############################
## Kyle J. Smelter: 3/5/24 ##
###############################

##########################
## Download NLCD Data ##
##########################

#Use the tigris package to filter through states and remove GEOIDs above 60
#GEOIDs above 60 are territories and islands so they are being removed for scaling
st <- tigris::states() %>%
  dplyr::filter(GEOID < "60") %>% 
  tigris::shift_geometry()

#Transform to albers equal area conic projection, epsg code is 5070
st <- st_transform(st, 5070)

#Grab outline of PA
pa.outline <- subset(st, st$NAME=="Pennsylvania")

#Grab outline of the Mid-Atlantic Region 
#mar.outline <- subset(st, st$NAME== "Pennsylvania"| st$NAME== "Maryland"| st$NAME == "New Jersey") 

#Obtain Pennsylvania NLCD raster
pa.nlcd <- FedData::get_nlcd(template= pa.outline, year = 2019, 
                             label = 'pa', 
                             force.redo = T)

#Obtain study area NLCD information 
#mar.nlcd <- FedData::get_nlcd(template= mar.outline, year = 2019, label = 'mar', force.redo = T)

#Write .tiff files 
#Stored in onedrive folder with rasters
#raster::writeRaster(mar.nlcd, "MAR_NLCD.tiff")
#raster::writeRaster(pa.nlcd, "PA.NLCD.tiff")

################################################################################
################################################################################

##########################
## Study Area Analysis ##
##########################

#Load PA Wildlife Management Units Shapefile from PASDA
pa.wmus <- st_read("shapefiles/WMUs/PGC_BNDWildlifeManagementUnits2021.shp") %>%
  sf::st_transform(5070)

#The hen study takes place here in PA in 2D, 3D, 4D, and 5C
study.area <- subset(pa.wmus, WMU_ID=="2D"| WMU_ID=="3D"| WMU_ID=="4D"| WMU_ID=="5C")

pa.wmus.nlcd <- FedData::get_nlcd(template= study.area, year = 2019, 
                                  label = 'pa', 
                                  force.redo = T)

m <- c(0, 24, 1, 30,31, 2, 39, 52, 3, 70, 95, 4)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
pa.wmus.rc<- classify(pa.wmus.nlcd, rclmat)
studyarea.rast <- crop(pa.wmus.rc,study.area, mask=TRUE)
plot(studyarea.rast)

#Calculate the landscape statistics for available metrics
# wmu.l.level= calculate_lsm(studyarea.rast, level = "landscape")
# wmu.l.level

# #Calculate the class level statistics for available metrics
# wmu.c.level= calculate_lsm(studyarea.rast, level = "Class")
# wmu.c.level

#Calculate the patch level statistics for available metrics
# wmu.p.level= calculate_lsm(studyarea.rast, level = "Patch")
# wmu.p.level

###########################
## Landcover Analysis ##
###########################

#Will be able to extract nest locations from entire region so no separate code like distance to road below

#Read in raster with NLCD data from the Mid-Atlantic Region 
#May save space later and not write these out 
#mar.nlcd <- terra::rast("rasters/nlcd/MAR_NLCD.tiff")
#pa.nlcd <- terra::rast("rasters/nlcd/PA.NLCD.tiff")

#1=Developed 0-24
#2=Barren/Rock 30-31
#3= Forested 39-52
#4= Agriculture 70-95
# reclassify the values into 7 groups all values between 0 and 20 equal 1, etc. while removing water (11)
m <- c(0, 24, 1, 30,31, 2, 39, 52, 3, 70, 95, 4)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- classify(pa.nlcd, rclmat)
plot(rc) #Check

#Read in nests shapefile
#Transform to albers (Hoping to later combine these nests with others from MD and NJ)
#Rename unique identifier column 
pa.nests <- st_read("shapefiles/nests/nests and veg/nest.veg.locations_cleaned1.shp") %>%
  sf::st_transform(5070) %>%
  dplyr::rename("uniqueid"=uniquID) 

#Buffer nests by 80m to simulate nesting home range (Badyaev 1995)
nests_buffered <- sf::st_buffer(pa.nests, 80)
mapview(nests_buffered) #Check

#Create dataframe that has buffered nest values 
#Extract land cover type values using the exact extractr in R
nests.buffered.values <- exactextractr::exact_extract(rc, st_as_sf(nests_buffered))
head(nests.buffered.values[[1]])#Percent in each category (buffers 1-6)
#Create a table of land cover types within each buffer
et=lapply(nests.buffered.values,table)

#Create proportions list object using for loop
prop <- list()
for(i in 1:length(nests.buffered.values)[1] ){
  prop[[i]] <- round((margin.table(et[[i]],1)/margin.table(et[[i]])),digits = 6)
}

M <- coredata(do.call(cbind, lapply(prop, zoo)))
colnames(M) <- NULL
#Transpose matrix so land cover become separate columns of data
matrix <- t(M)
#Now convert the matrix to a data frame so it is easier to manipulate
dfland <- as.data.frame(matrix)
head(dfland) #Check
#Assing column names to land cover
colnames(dfland) <- c("Developed","Barren","Forested", "Agriculture")
head(dfland)

#Function to fill dataframe NA values with 0.000001
fill_NA_with_value <- function(df, value = 0.000001) {
  df[is.na(dfland)] <- 0.000001
  return(df)
}

# Apply the function to fill NA values with 0.0001
dfland.final <- fill_NA_with_value(dfland)

#Write csv
#write.csv(dfland.final, "dfland.final.csv")

#The barren is really surprising to me. I haven't seen that in the turkey literature
#The field crew in 4D hinted at this when I went out trapping though

#Create nests dataframe with proportions
pa.nests.prop <- cbind(pa.nests, dfland.final)

########################
##  Road Shapefiles ##
#######################

#PA State Roads
#WGS to Albers
#pa.state.roads <- sf::st_read("shapefiles/roads/PaStateRoads2023_10.shp") %>%
#st_transform(5070)

#PA local roads
#WGS to Albers
#pa.local.roads <- sf::st_read("shapefiles/roads/PaLocalRoads2023_10.shp") %>%
#st_transform(5070)

#Merge shapefiles
#Create PA roads
#pa.roads <-dplyr::bind_rows(pa.state.roads, pa.local.roads)

#Write new shapefile
#st_write(pa.roads, "pa.roads.shp")

###########################################
## Nest Distance from Road Calculations ##
###########################################

#source: https://gis.stackexchange.com/questions/310489/calculating-euclidian-distance-in-r-between-lines-and-points

#Read in roads shapefile
#Drop all roads that have an NA value for ID
#Will check outputs for this later
#Big file
pa.roads <- st_read("shapefiles/roads/pa.roads.shp") %>%
  sf::st_transform(5070) %>%
  dplyr::select(ID) %>%
  tidyr::drop_na(ID)

#Empty lists to store final outputs
pa.nests.out <- list()
road.out <- list()

#Bounding box of nests in PA as a spatial object
#st_as_sfc is class 3 method for bbox
pa.nests_bbox <- sf::st_bbox(pa.nests) %>% st_as_sfc()

#Bounding box of roads in PA as a spatial object
#st_as_sfc is class 3 method for bbox
pa.roads_bbox <- sf::st_bbox(pa.roads) %>% st_as_sfc() 

#For loop to extract the distance from each nest to the nearest road
#Takes me about 40 minutes to run so I've included RData results below
for (i in 1: nrow(pa.nests)) {
  
  #join the nests and roads
  #st_nearest feature: get index of nearest feature
  #Spatial join of pa.nests and pa.roads
  #[[]] first entire object within list, indexing of lists
  neardist <- sf::st_join(
    pa.nests[i,],
    pa.roads, 
    join= sf::st_nearest_feature
  ) 
  
  #find the distance of each nest to the nearest road
  #we know this is in meters by units(dist.to.road)
  dist.to.road <- sf::st_distance(pa.nests[i,],
                                  pa.roads %>%
                                    dplyr::filter(ID==neardist$ID))
  
  #create a df with things we need
  dist.to.road.df <- data.frame(nestid = (pa.nests$uniqueid)[i],
                                plottype = (pa.nests$plottyp)[i],
                                bandid= (pa.nests$bandid)[i],
                                case= (pa.nests$case)[i],
                                NEARDIST = as.numeric(dist.to.road))
  
  #put the df in output
  road.out[[i]] <- dist.to.road.df 
  
  #Printing i here means that for every time the loop runs the row is recorded in the console
  print(i)
  
}

#Create dist2road object
dist2road <- do.call(rbind,(road.out))

#Write csv
#write.csv(dist2road, "Csvs/dist2road.out.csv")