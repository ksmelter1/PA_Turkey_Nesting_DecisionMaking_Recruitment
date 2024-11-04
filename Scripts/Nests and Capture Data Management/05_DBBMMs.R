
#############################
## Kyle J. Smelter 3/6/24 ##
#############################

###############################################
## Dynamic Brownian Bridge Movement Models ##
###############################################

#Login
login <- movebankLogin(username = "Kyle.Smelter", password="Rayshawks5!")

##############
## WMU 2D ##
#############

#Download hen movement data from each study area 
#This generates a movestack object
dat.2d.move <- getMovebankData(study = "Wild Turkey Pennsylvania WMU 2D", login = login,
                               removeDuplicatedTimestamps=T) 

#This turns the movestack object into a datframe and subsets the data
dat.2d <- dat.2d.move@data %>%
  dplyr::mutate(BirdID = gsub(x = dat.2d.move@trackId, pattern = "X", replacement = "")) %>%
  dplyr::rename( Lat=location_lat, Long=location_long) %>%
  mutate(Year = lubridate::year(timestamp), TrackID = paste(BirdID, Year, sep = "_")) %>%
  dplyr::select(tag_id, Year, eobs_temperature, Lat, Long, BirdID, event_id, 
                comments, timestamp, update_ts, eobs_speed_accuracy_estimate, 
                eobs_horizontal_accuracy_estimate, ground_speed, TrackID )

#filter year to 2023
dat.2d.2023 <- dat.2d %>% dplyr::filter(Year=="2023")

#Create a birdid object storing all unique BirdIDs from the downloaded data
birdid<- unique(dat.2d.2023$BirdID) %>%
  data.frame() 

#Was getting issues with these birds so I removed them for the time being
birdid <- birdid[birdid != 8577]
birdid <- birdid[birdid != 8659]
  
#Create nesting season dataframe 
spring.season.df <- data.frame(
  BirdID = birdid,
  Year =  "2023",
  Season = "Nesting",
  Start = paste0("2023","0301000000000"),
  End = paste0("2023","0731000000000")
) 

#################
## 2D DBBMMs ##
################

### Turn each into 95% Utilization Distributions (estimates of home range)
for(i in 1:nrow(spring.season.df)){
  #for(i in 47){
  #Creating animal name and specifying timestart, timeend, year and ID parameters
  #Derived from spring.season.df above
  animalName <- as.character(spring.season.df$BirdID[i])
  timestart <- spring.season.df$Start[i]
  timeend <- spring.season.df$End[i]
  year <- spring.season.df$Year[i]
  id <- paste(animalName, year, spring.season.df$Season[i], sep = "_")

  #Download movebank data into turkey turkeygps object by specified conditions
  turkeygps <- getMovebankData(study = "Wild Turkey Pennsylvania WMU 2D", 
                               login = login,
                               animalName = animalName,
                               timestamp_start = timestart,
                               timestamp_end = timeend)
  t_turkeygps <- spTransform(turkeygps, crs=5070,center=T) #spTransform is tied into move
  #This line projects the downloaded data to the albers equal area conic projection
  
  #raster: Object or a numeric value, 30 x 30m resolution matches NLCD
  #dimsize:The number of cells along the largest dimension of the track
  #location.error: Single numeric value that describes the error of the location
  #margin: The margin used for a behavioral change point analysis
  #Window.size: The size of the moving window along a track, larger windows provide better estimates of brownian motion variance
  turk_dBBMM <- move::brownian.bridge.dyn(t_turkeygps,raster=30, 
                                          location.error=17,
                                          margin = 5, window.size = 15, ext = 1.5)
  
  #The function converts a raster UD(stack) object to a SpatialLinesDataFrame. This allows to re-project the contours to different projections.
  turkey_UD <-raster2contour(turk_dBBMM, level=c(.95))
  #Creating sf object
  dBBMM_line <- st_as_sf(turkey_UD, "SpatialLines")
  #st_cast: Cast geometry to another type: either simplify, or cast explicitly
  dBBMM_poly <- st_cast(dBBMM_line, "POLYGON")
  dBBMM_poly$id <- id
  
  print (i)
  
  #Create list to store home ranges
  if(exists("wmu.2d.nesting.2023.list")){
    dBBMM_poly <- st_transform(dBBMM_poly, st_crs(wmu.2d.nesting.2023.list))
    wmu.2d.nesting.2023.list <- rbind(wmu.2d.nesting.2023.list, dBBMM_poly)
  }else{
    wmu.2d.nesting.2023.list <- dBBMM_poly
    
  }}

#Plot out home ranges using mapview
wmu.2d.2023.homeranges <-mapview(wmu.2d.nesting.2023.list)

#Write shapefile
st_write(wmu.2d.nesting.2023.list, "wmu.2d.nesting.hr.2023.shp")

#Save mapview object for Franny
#Doesn't work
saveWidget(wmu.2d.nesting.2023.list, file="wmu.2d.2023.homeranges.html")

########################################
## Summarize each home range by size ##
#######################################

#Create an empty data frame to store summaries
summary_df <- data.frame()

#Iterate through each polygon
for (i in 1:nrow(wmu.2d.nesting.2023.list)) {
  #Extract the polygon
  current_polygon <- wmu.2d.nesting.2023.list[i,]
  
  #Calculate the area of the polygon
  area <- st_area(wmu.2d.nesting.2023.list$geometry[i],
                  )
  
  #Create a summary for the current polygon
  summary_row <- data.frame(
    Polygon_ID = wmu.2d.nesting.2023.list$id [i],
    Area = area
  )
  
  #Append the summary to the main data frame
  summary_df <- rbind(summary_df, summary_row)
}

#Print the summary data frame
print(summary_df)

mean(summary_df$Area)

##############
## WMU 4D ##
#############

#Download hen movement data from each study area 
#This generates a movestack object
dat.4d.move <- getMovebankData(study = "Wild Turkey Pennsylvania WMU 4D", login = login,
                               removeDuplicatedTimestamps=T) 

#This turns the movestack object into a datframe and subsets the data
dat.4d <- dat.4d.move@data %>%
  dplyr::mutate(BirdID = gsub(x = dat.4d.move@trackId, pattern = "X", replacement = "")) %>%
  dplyr::rename( Lat=location_lat, Long=location_long) %>%
  mutate(Year = lubridate::year(timestamp), TrackID = paste(BirdID, Year, sep = "_")) %>%
  dplyr::select(tag_id, Year, eobs_temperature, Lat, Long, BirdID, event_id, 
                comments, timestamp, update_ts, eobs_speed_accuracy_estimate, 
                eobs_horizontal_accuracy_estimate, ground_speed, TrackID )

#filter year to 2023
dat.4d.2023 <- dat.4d %>% dplyr::filter(Year=="2023")

#Create a birdid object storing all unique BirdIDs from the downloaded data
birdid<- unique(dat.4d.2023$BirdID) %>%
  data.frame() 

#Was getting issues with these birds so I removed them for the time being
birdid <- birdid[birdid != 8209]
birdid <- birdid[birdid != 8220]
birdid <- birdid[birdid != 8764]
birdid <- birdid[birdid != 8768]
birdid <- birdid[birdid != 8773]
birdid <- birdid[birdid != 8806]

#Create nesting season dataframe 
spring.season.df.4d <- data.frame(
  BirdID = birdid,
  Year =  "2023",
  Season = "Nesting",
  Start = paste0("2023","0301000000000"),
  End = paste0("2023","0731000000000")
) 

#################
## 4D DBBMMs ##
################

### Turn each into 95% Utilization Distributions (estimates of home range)
for(i in 1:nrow(spring.season.df.4d)){
  #for(i in 47){
  #Creating animal name and specifying timestart, timeend, year and ID parameters
  #Derived from spring.season.df above
  animalName <- as.character(spring.season.df.4d$BirdID[i])
  timestart <- spring.season.df.4d$Start[i]
  timeend <- spring.season.df.4d$End[i]
  year <- spring.season.df.4d$Year[i]
  id <- paste(animalName, year, spring.season.df.4d$Season[i], sep = "_")
  
  #Download movebank data into turkey turkeygps object by specified conditions
  turkeygps <- getMovebankData(study = "Wild Turkey Pennsylvania WMU 4D", 
                               login = login,
                               animalName = animalName,
                               timestamp_start = timestart,
                               timestamp_end = timeend)
  t_turkeygps <- spTransform(turkeygps, crs=5070,center=T) #spTransform is tied into move
  #This line projects the downloaded data to the albers equal area conic projection
  
  #raster: Object or a numeric value, 30 x 30m resolution matches NLCD
  #dimsize:The number of cells along the largest dimension of the track
  #location.error: Single numeric value that describes the error of the location
  #margin: The margin used for a behavioral change point analysis
  #Window.size: The size of the moving window along a track, larger windows provide better estimates of brownian motion variance
  turk_dBBMM <- move::brownian.bridge.dyn(t_turkeygps,raster=30, 
                                          location.error=17,
                                          margin = 5, window.size = 15, ext = 1.5)
  
  #The function converts a raster UD(stack) object to a SpatialLinesDataFrame. This allows to re-project the contours to different projections.
  turkey_UD <-raster2contour(turk_dBBMM, level=c(.95))
  #Creating sf object
  dBBMM_line <- st_as_sf(turkey_UD, "SpatialLines")
  #st_cast: Cast geometry to another type: either simplify, or cast explicitly
  dBBMM_poly <- st_cast(dBBMM_line, "POLYGON")
  dBBMM_poly$id <- id
  
  print (i)
  
  #Create list to store home ranges
  if(exists("wmu.4d.nesting.2023.list")){
    dBBMM_poly <- st_transform(dBBMM_poly, st_crs(wmu.4d.nesting.2023.list))
    wmu.4d.nesting.2023.list <- rbind(wmu.4d.nesting.2023.list, dBBMM_poly)
  }else{
    wmu.4d.nesting.2023.list <- dBBMM_poly
    
  }}

#Plot out home ranges using mapview
wmu.4d.2023.homeranges <-mapview(wmu.4d.nesting.2023.list)

#Write shapefile
st_write(wmu.4d.nesting.2023.list, "wmu.4d.nesting.hr.2023.shp")

turkeys.2d <-st_read(file.choose())
turkeys.4d <-st_read(file.choose()) 
trial.hr.dataset <-bind_rows(turkeys.2d, turkeys.4d)

