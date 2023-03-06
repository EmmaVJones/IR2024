#had to run below in R4.1.2 bc issue in driver or ODBC in 3.6.2

library(tidyverse)
library(sf)
library(readxl)
library(lubridate)
library(pool)
library(dbplyr)

# connect to ODS
pool <- dbPool(
drv = odbc::odbc(),
Driver = "ODBC Driver 11 for SQL Server", #"SQL Server", ##"SQL Server Native Client 11.0",
Server= "DEQ-SQLODS-PROD,50000",
database = "ODS",
trusted_connection = "yes"
)

#had to run below in R4.1.2 bc issue in driver or ODBC in 3.6.2
stations <-  pool %>%
  tbl(in_schema('wqa', 'WQA Station Details')) %>%
  filter(`Assessment Cycle` == 2022) %>%
  as_tibble()


# ran this solo bc took a while
stationsWithWQS <- pool %>%
  tbl(in_schema('wqa', 'WQA Assessment Unit Details')) %>%
  filter(`Assessment Unit Detail Id` %in% !! stations$`Assessment Unit Detail Id`) %>% 
  as_tibble() 

# Join now that data local
citmonStationsWithWQS <- left_join(dplyr::select(stations, `Assessment Unit Detail Id`, `Station Id`, `Water Type`, `Station Type List`, `Station Type 1`:`Station Type 10`),
                                   dplyr::select(stationsWithWQS, `Assessment Unit Detail Id`, `Water Type`, `WQS Class`, `WQS Section`, `WQS Special Standard`),
                                   by = c('Assessment Unit Detail Id', 'Water Type')) %>% 
  filter( ! `Station Type 1` %in% c('A', 'B', 'C', 'CB', 'CB-B','CR', 'C2','FPM','IM','L', 'PA', 'SS','TM', 'TR' )) %>% 
  filter( ! `Station Type 2` %in% c('A', 'B', 'C', 'CB', 'CB-B','CR', 'C2','FPM','IM','L', 'PA', 'SS','TM', 'TR' )) %>% 
  filter( ! `Station Type 3` %in% c('A', 'B', 'C', 'CB', 'CB-B','CR', 'C2','FPM','IM','L', 'PA', 'SS','TM', 'TR' )) %>% 
  filter( ! `Station Type 4` %in% c('A', 'B', 'C', 'CB', 'CB-B','CR', 'C2','FPM','IM','L', 'PA', 'SS','TM', 'TR' )) %>% 
  filter( ! `Station Type 5` %in% c('A', 'B', 'C', 'CB', 'CB-B','CR', 'C2','FPM','IM','L', 'PA', 'SS','TM', 'TR' )) %>% 
  filter( ! `Station Type 6` %in% c('A', 'B', 'C', 'CB', 'CB-B','CR', 'C2','FPM','IM','L', 'PA', 'SS','TM', 'TR' )) %>% 
  filter( ! `Station Type 7` %in% c('A', 'B', 'C', 'CB', 'CB-B','CR', 'C2','FPM','IM','L', 'PA', 'SS','TM', 'TR' )) %>% 
  filter( ! `Station Type 8` %in% c('A', 'B', 'C', 'CB', 'CB-B','CR', 'C2','FPM','IM','L', 'PA', 'SS','TM', 'TR' )) %>% 
  filter( ! `Station Type 9` %in% c('A', 'B', 'C', 'CB', 'CB-B','CR', 'C2','FPM','IM','L', 'PA', 'SS','TM', 'TR' )) %>% 
  filter( ! `Station Type 10` %in% c('A', 'B', 'C', 'CB', 'CB-B','CR', 'C2','FPM','IM','L', 'PA', 'SS','TM', 'TR' )) %>% 
  filter( ! is.na(`WQS Class`))
  

unique(citmonStationsWithWQS$`Station Type List`)


# Bring in citmon station that need WQS
distinctSites_sf <- readRDS('data/distinctSites_sf03062023.RDS')#'data/distinctSites_sf_withCitmon02232023.RDS')##distinctSites_sf_withCitmon.RDS')

# bring in any existing citmon WQS information from previous work
citmonStationsWithWQS <- readRDS('data/citmonStationsWithWQS.RDS') # pin_get('ejones/citmonStationsWithWQSFinal', board= 'rsconnect')# RSconnect error bc R version

##### # stations that were sent to app for manual review after spatial snapping
###### forReview <- readRDS('data/WQStableWithCitmon11142022.RDS')
# # remove sites from forReview that have WQS info from ODS from AU level
# fine <- filter(forReview, StationID %in% citmonStationsWithWQS$`Station Id`)
# notFine <- filter(forReview, ! StationID %in% citmonStationsWithWQS$`Station Id`)


# save new version of assessor WQS review list for App
#saveRDS(notFine, 'data/WQStableWithCitmon11292022.RDS')
saveRDS(citmonStationsWithWQS, 'data/citmonStationsWithWQS.RDS')

# Back to R 3.6.2 

citmonStationsWithWQS <- readRDS('data/citmonStationsWithWQS.RDS')
distinctSites_sf <-  readRDS('data/distinctSites_sf03062023.RDS') %>% #readRDS('data/distinctSites_sf_withCitmon02232023.RDS') %>% 
  st_drop_geometry()
  #add in super large pull and remove non distinct
distinctSites_sf_allIR2022citmonSites <- readRDS('data/distinctSites_sf_withCitmon.RDS') %>% st_drop_geometry()

distinctSites_sf1 <- rbind(distinctSites_sf, 
                               filter(distinctSites_sf_allIR2022citmonSites,! FDT_STA_ID %in% distinctSites_sf$FDT_STA_ID) %>% 
                             #mutate(GROUP_STA_ID= NA_character_) %>% 
                             dplyr::select(names(distinctSites_sf))  ) #%>% 
# Check for duplicates
  # group_by(FDT_STA_ID) %>% 
  # mutate( n = n()) %>% 
  # dplyr::select(n, everything()) %>% 
  # filter(n>1) %>% 
  # View()



WQStable <- tibble(StationID = NA, WQS_ID = NA)


# Now we need to do backflips to get the right WQS info for lakes 
lakeSites <- filter(citmonStationsWithWQS, `Water Type` == 'RESERVOIR') %>% 
  # get lat lng
  left_join(dplyr::select(distinctSites_sf1, FDT_STA_ID, Latitude, Longitude),
            by = c('Station Id' ='FDT_STA_ID')) %>% 
  filter(!is.na(Latitude) | !is.na(Longitude)) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = T, # don't remove these lat/lon cols from df
           crs = 4326)

  
source('preprocessingModules/WQS_lakePoly.R')

lakesPoly <- st_read('../GIS/draft_WQS_shapefiles_2022/lakes_reservoirs_draft2022.shp',
                     fid_column_name = "OBJECTID") %>%
  st_transform(4326)

WQStable <- lakePolygonJoin(lakesPoly, lakeSites %>% dplyr::select(FDT_STA_ID = `Station Id`), WQStable)


# not touching estuarine stuff now bc could be either line or polygon and don't want to dig into that rabbithole since there is a very established process already


# Format these citmon stations so this info can be bulk uploaded to automated tools in step 3
lakeSitesWQS <- WQStable %>% 
  left_join(lakesPoly %>% st_drop_geometry()) %>% 
  mutate(`Buffer Distance` = as.character(NA),
         Comments = as.character(NA)) %>% 
  dplyr::select(StationID, `Buffer Distance`, Comments, everything())

# reorganize other sites to this format
citmonStationsWithWQSFinal <- lakeSitesWQS %>% 
  bind_rows(filter(citmonStationsWithWQS, ! `Station Id` %in% lakeSitesWQS$StationID) %>% 
              dplyr::select(STATION_ID = `Station Id`, CLASS = `WQS Class`, SEC = `WQS Section`, SPSTDS = `WQS Special Standard`)) %>% 
  mutate(StationID = coalesce(StationID, STATION_ID)) %>% 
  dplyr::select(-c(STATION_ID, OBJECTID)) %>% 
  mutate( ZONE= as.character(NA),
          StreamType = as.factor(NA),
          WQS_COMMENT = as.character(NA)) %>% 
  dplyr::select(StationID, `WQS_ID`, `Buffer Distance`, Comments, GNIS_ID:SPSTDS,
                Basin_Code, PWS, Edit_Date, Tier_III, Backlog, ZONE, 
                SECTION_DESCRIPTION = SECTION_DE, created_user= created_us, created_date = created_da, 
                last_edited_user = last_edite, last_edited_date = last_edi_1,
                Shape_Length = Shape_Leng, Shape_Area,
                WQS_COMMENT, Trout, StreamType, Lakes_187B)


# add in manual additions
#write.csv(citmonStationsWithWQSFinal, 'citmonStationsWithWQSFinal.csv', na='', row.names = F)
citmonWQS1 <- read.csv('data/CitizenMonitoringNonAgencyStationInformationFromRegions/citmonStationsWithWQSFinal.csv')
# these only have WQS_ID, will join in actual WQS information later but for now this is okay bc all we care about is which stations have WQSinformation available

citmonStationsWithWQSFinal <- citmonWQS1 %>% 
  rename("Buffer Distance" = "Buffer.Distance")


# double check that data it correct format

library(pins)
library(config)
library(purrr)
library(lubridate)

# Connect to server
conn <- config::get("connectionSettings") # get configuration settings

board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))

WQSlookup <- pin_get('ejones/WQSlookup-withStandards', board = 'rsconnect')

names(citmonStationsWithWQSFinal) == names(WQSlookup)

citmonStationsWithWQSFinal1 <- dplyr::select(citmonStationsWithWQSFinal, any_of(names(WQSlookup)))
names(citmonStationsWithWQSFinal1) == names(WQSlookup) # okay don't match perfectly


pin(citmonStationsWithWQSFinal1, name = 'ejones/citmonStationsWithWQSFinal', 
    descrption = 'IR2024 citmon stations with WQS that were attributed by assessors during IR2022 and some manually attributed', board = 'rsconnect')

citmonStationsWithWQSFinal <- pin_get('ejones/citmonStationsWithWQSFinal', board = 'rsconnect')

