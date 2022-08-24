# Pinning these to server for IR2024 since these are used across multiple IR2024 applications
#  and since RSConnect can't bundle anything outside the working directory for upload to the server
library(tidyverse)
library(sf)
library(pins)
library(config)
library(purrr)
library(lubridate)

# Connect to server
conn <- config::get("connectionSettings") # get configuration settings

board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))


assessmentRegions <- st_read( 'GIS/AssessmentRegions_simple.shp')
assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) 
subbasins <- st_read('GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
  rename('SUBBASIN' = 'SUBBASIN_1')

pin(assessmentRegions, name = 'AssessmentRegions_simple',
    description = 'Simple polygon layer of DEQ Assessment regions', 
    board = 'rsconnect')
pin(assessmentLayer, name = 'AssessmentRegions_VA84_basins',
    description = 'VAHU6 layer with major basin and subbasin information joined', 
    board = 'rsconnect')
pin(subbasins, name = 'DEQ_VAHUSB_subbasins_EVJ',
    description = 'Subbasin layer by Assessment region with subbasin codes and applicable VAHU6 metadata', 
    board = 'rsconnect')
