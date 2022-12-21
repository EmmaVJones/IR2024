httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))

library(tidyverse)
library(shiny)
library(shinyjs)
library(sf)
library(pins)
library(EnvStats)
library(lubridate)
library(config)
library(leaflet)
library(mapview)
library(inlmisc)
library(DT)
library(plotly)
library(readxl)
library(round)


# Bring in assessment Functions and app modules
source('appModulesAndFunctions/updatedBacteriaCriteria.R')
source('appModulesAndFunctions/multipleDependentSelectizeArguments.R')
source('appModulesAndFunctions/automatedAssessmentFunctions.R')


# Connect to R server to bring in pinned data
conn <- config::get("connectionSettings") # get configuration settings
board_register_rsconnect(key = conn$CONNECT_API_KEY,  
                         server = conn$CONNECT_SERVER)




# modulesToReadIn <- c('temperature','pH','DO','Ecoli', 'Enteroccoci','SpCond','salinity','TN','chlA','Enteroccoci', 'TP','sulfate','Ammonia', 
#                      'Chloride', 'Nitrate','metals', 'fecalColiform','SSC','Benthics', 'toxics')
# for (i in 1:length(modulesToReadIn)){
#   source(paste('appModulesAndFunctions/',modulesToReadIn[i],'Module.R',sep=''))
# }

stationsTemplate <- pin_get('ejones/stationsTable2024begin', board = 'rsconnect')[0,] %>% 
  mutate(across(matches(c("LATITUDE", "LONGITUDE", "EXC", "SAMP")), as.numeric)) 

template <- read_csv('userDataToUpload/stationTableResults.csv')
lastUpdated <- as.Date(file.info('userDataToUpload/stationTableResults.csv')$mtime)

### Change these to pinned data -------------------------------------------------------------------------------------------------------------------------------------------------
# markPCB <- read_excel('data/2022 IR PCBDatapull_EVJ.xlsx', sheet = '2022IR Datapull EVJ')
# fishPCB <- read_excel('data/FishTissuePCBsMetals_EVJ.xlsx', sheet= 'PCBs')
# fishMetals <- read_excel('data/FishTissuePCBsMetals_EVJ.xlsx', sheet= 'Metals')
### Change these to pinned data -------------------------------------------------------------------------------------------------------------------------------------------------



# Helpful lookup table to ease data filtering
subbasinToVAHU6 <- read_csv('data/subbasinToVAHU6conversion.csv') 

# Loading screen
load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}



