
#'---
#' title: Habitat selection of female wild turkeys during incubation (an SSF analysis)
#' author: "K. Smelter, F. Buderman"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: MovementProcess_Prep.RData (R workspace)
#'   html_document: 
#'     toc: true
#'---
#'
#+ include = FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE, cache = TRUE)
#'  
#' **Purpose**: This script downloads movement data associated with each hens nesting attempt from movebank and exports hen movement data as RDS files.



#####################
## Load Packages ##
####################

#' Vector of package names
packages <- c("purrr",
              "lubridate",
              "dplyr",
              "move2")

#' Function to load a package or install it if not already installed
load_packages <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    require(package_name, character.only = TRUE)
  }
}

#' Apply the function to each package name
lapply(packages, load_packages)


#' Read in nests csv
pa.nests <- read.csv("Data Management/Csvs/processed data/Nests/nests_22_23_clean.csv")

############################
## Prepare 4D Nest Data ##
############################

#' Subset nesting data for 4D in year 2022
pa.nests.4D <- dplyr::filter(pa.nests, wmu_n =="4D")%>%
  dplyr::select(bandid, checkyr, checkmo, checkday, nestid, wmu_n)


#' Clean up issue with zeros in dates column
pa.nests.4D$checkday<-ifelse(nchar(pa.nests.4D$checkday)==1,paste(0,pa.nests.4D$checkday,sep=""),pa.nests.4D$checkday)
pa.nests.4D$checkmo<-ifelse(nchar(pa.nests.4D$checkmo)==1,paste(0,pa.nests.4D$checkmo,sep=""),pa.nests.4D$checkmo)

#' Build df with information we need
pa.nests.4D$checkdate <- paste0(pa.nests.4D$checkyr, 
                                     pa.nests.4D$checkmo, 
                                     pa.nests.4D$checkday) 

#' Format timestamps correctly in year, month, day format and create start date column 
pa.nests.4D$checkdate<-as.Date(as.character(pa.nests.4D$checkdate),
                               format="%Y%m%d")

#' This will be the birds exact incubation period once I get that information
pa.nests.4D$startdate <-pa.nests.4D$checkdate - days(30)

############################
## Prepare 3D Nest Data ##
############################

#' Subset nesting data for 4D in year 2022
pa.nests.3D <- dplyr::filter(pa.nests, wmu_n =="3D")%>%
  dplyr::select(bandid, checkyr, checkmo, checkday, nestid, wmu_n)


#' Clean up issue with zeros in dates column
pa.nests.3D$checkday<-ifelse(nchar(pa.nests.3D$checkday)==1,paste(0,pa.nests.3D$checkday,sep=""),pa.nests.3D$checkday)
pa.nests.3D$checkmo<-ifelse(nchar(pa.nests.3D$checkmo)==1,paste(0,pa.nests.3D$checkmo,sep=""),pa.nests.3D$checkmo)

#' Build df with information we need
pa.nests.3D$checkdate <- paste0(pa.nests.3D$checkyr, 
                                pa.nests.3D$checkmo, 
                                pa.nests.3D$checkday) 

#' Format timestamps correctly in year, month, day format and create start date column 
pa.nests.3D$checkdate<-as.Date(as.character(pa.nests.3D$checkdate),
                               format="%Y%m%d")
pa.nests.3D$startdate <-pa.nests.3D$checkdate - days(30)

############################
## Prepare 2D Nest Data ##
############################

#' Subset nesting data for 4D in year 2022
pa.nests.2D <- dplyr::filter(pa.nests, wmu_n =="2D")%>%
  dplyr::select(bandid, checkyr, checkmo, checkday, nestid, wmu_n)


#' Clean up issue with zeros in dates column
pa.nests.2D$checkday<-ifelse(nchar(pa.nests.2D$checkday)==1,paste(0,pa.nests.2D$checkday,sep=""),pa.nests.2D$checkday)
pa.nests.2D$checkmo<-ifelse(nchar(pa.nests.2D$checkmo)==1,paste(0,pa.nests.2D$checkmo,sep=""),pa.nests.2D$checkmo)

#' Build df with information we need
pa.nests.2D$checkdate <- paste0(pa.nests.2D$checkyr, 
                                pa.nests.2D$checkmo, 
                                pa.nests.2D$checkday) 

#' Format timestamps correctly in year, month, day format and create start date column 
pa.nests.2D$checkdate<-as.Date(as.character(pa.nests.2D$checkdate),
                               format="%Y%m%d")
pa.nests.2D$startdate <-pa.nests.2D$checkdate - days(30)

############################
## Prepare 5C Nest Data ##
############################

#' Subset nesting data for 4D in year 2022
pa.nests.5C <- dplyr::filter(pa.nests, wmu_n =="5C")%>%
  dplyr::select(bandid, checkyr, checkmo, checkday, nestid, wmu_n)



#' Clean up issue with zeros in dates column
pa.nests.5C$checkday<-ifelse(nchar(pa.nests.5C$checkday)==1,paste(0,pa.nests.5C$checkday,sep=""),pa.nests.5C$checkday)
pa.nests.5C$checkmo<-ifelse(nchar(pa.nests.5C$checkmo)==1,paste(0,pa.nests.5C$checkmo,sep=""),pa.nests.5C$checkmo)

#' Build df with information we need
pa.nests.5C$checkdate <- paste0(pa.nests.5C$checkyr, 
                                pa.nests.5C$checkmo, 
                                pa.nests.5C$checkday) 

#' Format timestamps correctly in year, month, day format and create start date column 
pa.nests.5C$checkdate<-as.Date(as.character(pa.nests.5C$checkdate),
                               format="%Y%m%d")
pa.nests.5C$startdate <-pa.nests.5C$checkdate - days(30)



############################################################
## Pull Movebank Data from Movebank for Specified Dates ##
############################################################

#' Login to movebank
login <- movebank_store_credentials(username = "Kyle.Smelter",
                                    password="Rayshawks5!",
                                    key="Kyle",
                                    force= T)

##########
## 4D ##
##########

#' Filter out 8770 
pa.nests.4D<- pa.nests.4D %>% 
  dplyr::mutate(col1=stringr::str_detect(bandid,"8770"))%>%
  dplyr::filter(!col1) %>%
  dplyr::select(-col1)

#' List of unique identifier
unique.ID.4d<-unique(pa.nests.4D$nestid)

for (j in 1:length(unique.ID.4d)){
  tmp.subset<-pa.nests.4D[which(pa.nests.4D$nestid==unique.ID.4d[j]),]
  tmp.subset$TrackID<-paste(unique.ID.4d[j],seq(1,nrow(tmp.subset),1),sep="_")
  
for(i in 1:nrow(tmp.subset)){
  BirdID<- as.character(tmp.subset[i,1])
  EndDate <- gsub("\\D","", tmp.subset$checkdate[i]) 
  #' (Format for time is YYYYMMDDHHSSMM000)
  StartDate <- gsub("\\D","", tmp.subset$startdate[i]) 
  #' 30 days earlier than check date 
  #' This will be the birds exact incubation period 
  Year <-tmp.subset$checkyr[i]
 #' track_id <- as.character(tmp.subset$band_nestid[i])
  
  dat.4d<- movebank_download_study(study ="Wild Turkey Pennsylvania WMU 4D", 
                                   login = login,
                                   individual_local_identifier= BirdID,
                                   timestamp_start= StartDate,
                                   timestamp_end= EndDate,
                                   removeDuplicatedTimestamps=T)
  mt_track_id(dat.4d)<-rep(tmp.subset$TrackID[i],nrow(dat.4d))
  
  if(exists("full_all_4d")){ #' rbind ind bird data to create one large df
    full_all_4d <- rbind(full_all_4d, dat.4d)
    
    
  }else{
    full_all_4d <- dat.4d
  }
}
}

saveRDS(full_all_4d, "full_all_4d.RDS")

##########
## 3D ##
##########

#' List of unique identifier
unique.ID.3d<-unique(pa.nests.3D$nestid)

for (j in 1:length(unique.ID.3d)){
  tmp.subset.3d<-pa.nests.3D[which(pa.nests.3D$nestid==unique.ID.3d[j]),]
  tmp.subset.3d$TrackID<-paste(unique.ID.3d[j],seq(1,nrow(tmp.subset.3d),1),sep="_")
  
  for(i in 1:nrow(tmp.subset.3d)){
    BirdID<- as.character(tmp.subset.3d[i,1])
    EndDate <- gsub("\\D","", tmp.subset.3d$checkdate[i]) 
    #' (Format for time is YYYYMMDDHHSSMM000)
    StartDate <- gsub("\\D","", tmp.subset.3d$startdate[i]) 
    #' 30 days earlier than check date 
    #' This will be the birds exact incubation period 
    Year <-tmp.subset.3d$checkyr[i]
    #' track_id <- as.character(tmp.subset$band_nestid[i])
    
    dat.3d<- movebank_download_study(study ="Wild Turkey Pennsylvania WMU 3D", 
                                     login = login,
                                     individual_local_identifier= BirdID,
                                     timestamp_start= StartDate,
                                     timestamp_end= EndDate,
                                     removeDuplicatedTimestamps=T)
    mt_track_id(dat.3d)<-rep(tmp.subset.3d$TrackID[i],nrow(dat.3d))
    
    if(exists("full_all_3d")){ #' rbind ind bird data to create one large df
      full_all_3d <- rbind(full_all_3d, dat.3d)
      
      
    }else{
      full_all_3d <- dat.3d
    }
  }
}

saveRDS(full_all_3d, "full_all_3d.RDS")

##########
## 2D ##
##########

#' Filter out 8166 and 8172from the analysis due to unknown ind ID in study
pa.nests.2D<- pa.nests.2D %>% 
  dplyr::mutate(col1=stringr::str_detect(bandid,"8166"))%>%
  dplyr::mutate(col2=stringr::str_detect(bandid,"8172"))%>%
  dplyr::filter(!col1)%>%
  dplyr::filter(!col2)%>%
  dplyr::select(-col1)%>%
  dplyr::select(-col2)


#' List of unique identifier
unique.ID.2d<-unique(pa.nests.2D$nestid)

for (j in 1:length(unique.ID.2d)){
  tmp.subset.2d<-pa.nests.2D[which(pa.nests.2D$nestid==unique.ID.2d[j]),]
  tmp.subset.2d$TrackID<-paste(unique.ID.2d[j],seq(1,nrow(tmp.subset.2d),1),sep="_")
  
  for(i in 1:nrow(tmp.subset.2d)){
    BirdID<- as.character(tmp.subset.2d[i,1])
    EndDate <- gsub("\\D","", tmp.subset.2d$checkdate[i]) 
    #' (Format for time is YYYYMMDDHHSSMM000)
    StartDate <- gsub("\\D","", tmp.subset.2d$startdate[i]) 
    #' 30 days earlier than check date 
    #' This will be the birds exact incubation period 
    Year <-tmp.subset.2d$checkyr[i]
    #' track_id <- as.character(tmp.subset$band_nestid[i])
    
    dat.2d<- movebank_download_study(study ="Wild Turkey Pennsylvania WMU 2D", 
                                     login = login,
                                     individual_local_identifier= BirdID,
                                     timestamp_start= StartDate,
                                     timestamp_end= EndDate,
                                     removeDuplicatedTimestamps=T)
    mt_track_id(dat.2d)<-rep(tmp.subset.2d$TrackID[i],nrow(dat.2d))
    
    if(exists("full_all_2d")){ #' rbind ind bird data to create one large df
      full_all_2d <- rbind(full_all_2d, dat.2d)
      
      
    }else{
      full_all_2d <- dat.2d
    }
  }
}

saveRDS(full_all_2d, "full_all_2d.RDS")

##########
## 5C ##
##########

#' List of unique identifier
unique.ID.5c<-unique(pa.nests.5C$nestid)

for (j in 1:length(unique.ID.5c)){
  tmp.subset.5c<-pa.nests.5C[which(pa.nests.5C$nestid==unique.ID.5c[j]),]
  tmp.subset.5c$TrackID<-paste(unique.ID.5c[j],seq(1,nrow(tmp.subset.5c),1),sep="_")
  
  for(i in 1:nrow(tmp.subset.5c)){
    BirdID<- as.character(tmp.subset.5c[i,1])
    EndDate <- gsub("\\D","", tmp.subset.5c$checkdate[i]) 
    #' (Format for time is YYYYMMDDHHSSMM000)
    StartDate <- gsub("\\D","", tmp.subset.5c$startdate[i]) 
    #' 30 days earlier than check date 
    #' This will be the birds exact incubation period 
    Year <-tmp.subset.5c$checkyr[i]
    #' track_id <- as.character(tmp.subset$band_nestid[i])
    
    dat.5c<- movebank_download_study(study ="Wild Turkey Pennsylvania WMU 5C", 
                                     login = login,
                                     individual_local_identifier= BirdID,
                                     timestamp_start= StartDate,
                                     timestamp_end= EndDate,
                                     removeDuplicatedTimestamps=T)
    mt_track_id(dat.5c)<-rep(tmp.subset.5c$TrackID[i],nrow(dat.5c))
    
    if(exists("full_all_5c")){ #' rbind ind bird data to create one large df
      full_all_5c <- rbind(full_all_5c, dat.5c)
      
      
    }else{
      full_all_5c <- dat.5c
    }
  }
}

  
saveRDS(full_all_5c, "full_all_5c.RDS")

###############################
## Prep Data for Function ##
##############################

#' Read RDS files
full_all_3d <- readRDS("Data Management/RData/Individual-Specific Movement Process/Study Area RDS Files/full_all_3d.RDS")
full_all_4d <- readRDS("Data Management/RData/Individual-Specific Movement Process/Study Area RDS Files/full_all_4d.RDS")
full_all_2d <- readRDS("Data Management/RData/Individual-Specific Movement Process/Study Area RDS Files/full_all_2d.RDS")
full_all_5c <- readRDS("Data Management/RData/Individual-Specific Movement Process/Study Area RDS Files/full_all_5c.RDS")

#' Convert move objects to dataframes
full_all_3d <- as.data.frame(full_all_3d)
full_all_4d <- as.data.frame(full_all_4d)
full_all_2d <- as.data.frame(full_all_2d)
full_all_5c <- as.data.frame(full_all_5c)

  #' Create df with all study areas
  df.all <- rbind(full_all_5c, 
                  full_all_3d, 
                  full_all_2d, 
                  full_all_4d) %>%
    dplyr::rename("BirdID"= individual_local_identifier) 
  
#' Separate geometry lat and longs into separate columns and create new dataframe
#' Organize timestamp to be formatted in year, month, day, hour, minutes, seconds
#' Map function applies a function to each element of a vector
hens.all <- df.all%>%
  mutate(long = unlist(map(df.all$geometry,1)),
         lat = unlist(map(df.all$geometry,2))) %>%
  dplyr::select(BirdID, timestamp,long, lat) 


######################################
## Save RDS Files for all Hens ##
######################################

#' #' Function to save RDS files 
#' #' Directory=file path
#' #' Savebirddata is the function df: hens.all, id: BirdID, fp: file path
#' directory <- paste(getwd(),"NestMovementData", sep="/")
#' unique_bird_ids <- unique(hens.all$BirdID)
#' save_bird_data <- function(df, id, fp) {
#'   #' Create directory if it doesn't exist
#'   if (!file.exists(fp)) {
#'     dir.create(fp)
#'   }
#' 
#'   for(k in 1:length(id)){
#'     #' Subset data for the current bird id
#'     #' placeholder looping through range of numbers (k)
#'     #' Iterate over unique BirdIDs
#'     bird_data <- dplyr::filter(df,BirdID==id[k])
#'     
#'     #' Save data as RDS file
#'     file_name <- paste("hen",id[k], ".RDS", sep = "")
#'     file_path <- file.path(fp, file_name)
#'     saveRDS(bird_data, file_path)
#'   }
#' }
#' 
#' save_bird_data(df=hens.all, id=unique_bird_ids, fp=directory)
#' 
#' 
#' 
#' 
