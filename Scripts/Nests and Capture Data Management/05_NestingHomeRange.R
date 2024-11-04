
#############################
## Kyle J. Smelter 5/8/24 ##
#############################

##############################################
## MCP for Turkey Nesting Home Range Size ##
#############################################

#This script will create nesting home ranges in the form of MCPs for 56 hens in 4D in 2022

#Read in nests csv
pa.nests <- read.csv("Csvs/processed data/Nests/nests_22_23_clean.csv")

#Subset nesting data for 4D in year 2022
pa.nests.2022 <- dplyr::filter(pa.nests, checkyr=="2022") 
pa.nests.2022.4D <- dplyr::filter(pa.nests.2022, wmu_n=="4D")%>%
  dplyr::select(bandid, checkyr, checkmo, checkday, nestid)

#Create check date column for movebank
#Need to rewrite this to create a parseable date for movebank
pa.nests.2022.4D$checkdate <- paste0(pa.nests.2022.4D$checkyr, 
                                     pa.nests.2022.4D$checkmo, 
                                     pa.nests.2022.4D$checkday) 
#Write csv
write.csv(pa.nests.2022.4D, "pa.nests.2022.4D.csv")

#Reformatted dates in excel and subtracted 28 days automatically from each checkday for start date


######################################
## Pull Movebank Data from WMU 4D ##
######################################

#read csv
move.csv <- read.csv("Csvs/processed data/Nests/pa.nests.2022.4D.edited.csv") 

#Login to movebank
login <- movebank_store_credentials(username = "Kyle.Smelter",
                                    password="Rayshawks5!",
                                    key_name = "Kyle",
                                    force= F)

for(i in 1:nrow(move.csv)){
  BirdID<- as.character(move.csv[i,2])
  EndDate <- gsub("\\D","", move.csv$checkdate[i]) 
  #(Format for time is YYYYMMDDHHSSMM000)
  StartDate <- gsub("\\D","", move.csv$startdate[i]) 
  #28 days earlier than check date (Not exact if nest failed)
  Year <-move.csv$checkyr[i]
  
dat.4d<- movebank_download_study(study ="Wild Turkey Pennsylvania WMU 4D", 
                               login = login,
                               individual_local_identifier = BirdID,
                               timestamp_start= StartDate,
                               timestamp_end= EndDate,
                               removeDuplicatedTimestamps=T)
t_turkeygps <- st_transform(dat.4d, crs=5070,center=T) #spTransform is tied into move

cp <- mcp(t_turkeygps[,2], percent=95)#(95% is the default)

print(i=1)

if(exists("full_all")){ #rbind ind bird data to create one large df
  full_all <- rbind(full_all, dat.4d@data)
  
  
}else{
  full_all <- dat.4d@data
}
}

#############################################
## Calculate the size of each home range ##
#############################################



