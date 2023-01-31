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
source('appModulesAndFunctions/benthicAssessmentFunctions.R')


# Connect to R server to bring in pinned data
conn <- config::get("connectionSettings") # get configuration settings
board_register_rsconnect(key = conn$CONNECT_API_KEY,  
                         server = conn$CONNECT_SERVER)


modulesToReadIn <- c('stationStatusMap','temperature','pH','DO', 'SpCond', 'salinity',
                     'TN','chlA', 'TP', 'fecalColiform','SSC', 'Ammonia', 'Ecoli','Enterococci',
                     'Chloride','Sulfate', 'Nitrate','Benthics','metals')
#,'toxics')
for (i in 1:length(modulesToReadIn)){
  source(paste('appModulesAndFunctions/',modulesToReadIn[i],'Module.R',sep=''))
}



# Helpful lookup table to ease data filtering
subbasinToVAHU6 <- read_csv('data/subbasinToVAHU6conversion.csv') 

# Water Column Metals with static criteria (helpful for metals module)
staticLimit <- c("Antimony PWS", "Antimony All Other Surface Waters", "Arsenic Acute Freshwater", "Arsenic Chronic Freshwater", "Arsenic PWS",
                 "Arsenic Acute Saltwater", "Arsenic Chronic Saltwater", "Barium PWS","Cadmium PWS","ChromiumIII PWS",
                 "ChromiumVI Acute Freshwater", "ChromiumVI Chronic Freshwater", "ChromiumVI Acute Saltwater", "ChromiumVI Chronic Saltwater", 
                 "Lead PWS", "Mercury Acute Freshwater", "Mercury Chronic Freshwater", "Mercury Acute Saltwater", "Mercury Chronic Saltwater",
                 "Nickel PWS",  "Nickel All Other Surface Waters", "Uranium PWS","Selenium Acute Freshwater", "Selenium Chronic Freshwater", 
                 "Selenium PWS", "Selenium All Other Surface Waters","Thallium PWS", "Thallium All Other Surface Waters","Zinc PWS", 
                 "Zinc All Other Surface Waters")




# parameter name crosswalk for prettier names later
parameterSTATcrosswalk <- tibble(Parameter = c("Temperature", 'Dissolved Oxygen', 'pH', 'E.coli', 'Enterococci', 'Ammonia', 'Water Column Metals',
                                               'Water Column Toxics', 'Sediment Metals', 'Sediment Toxics', 'Fish Tissue Metals', 'Fish Tissue Toxics',
                                               'Benthics', 'Total Phosphorus', 'Chlorophyll a'),
                                 ParameterSTAT = c("TEMP_STAT", "DO_STAT", "PH_STAT", "ECOLI_STAT", "ENTER_STAT", "AMMONIA_STAT", "WAT_MET_STAT", 
                                                   "WAT_TOX_STAT", "SED_MET_STAT", "SED_TOX_STAT", "FISH_MET_STAT", "FISH_TOX_STAT", "BENTHIC_STAT",
                                                   "NUT_TP_STAT", "NUT_CHLA_STAT"))


# Some data reorg to identify individual parameter issues and "rank" station (basically identifies worst status and colors
#  station that one color, user can dig in to station results to figure out what is causing the flag)
VAHU6stationSummary <- function(stationTable, VAHU6chosen, parameterSTATcrosswalk){
  vahu6StationSummary <- filter(stationTable, VAHU6 %in% VAHU6chosen) 
  
  if(nrow(vahu6StationSummary) > 0){
    vahu6StationSummary <- vahu6StationSummary %>%
      dplyr::select(STATION_ID, contains('_STAT')) %>% 
      group_by(STATION_ID) %>%
      pivot_longer(-STATION_ID, names_to = 'ParameterSTAT', values_to = 'Status') %>%
      mutate(individualColor = case_when(Status %in% c("10.5% Exceedance", 'IM') ~ 'red',
                                         Status %in% c('IN', 'Review') ~ 'yellow',
                                         Status %in% c("S") ~ 'green',
                                         is.na(Status) ~ 'gray'),
             # create a variable that ranks statuses to easily combine into single "score"
             individualScore = case_when(individualColor == 'red' ~ 1,
                                         individualColor == 'yellow' ~ 2,
                                         individualColor == 'green' ~ 3,
                                         individualColor == 'gray' ~ 4))
    # Gives one "rank" per station
    overall <- vahu6StationSummary %>%
      group_by(STATION_ID, individualColor, individualScore) %>%
      dplyr::summarise(`n Parameters of lowest status` = n()) 
    # join number of ranks causing color info
    overall2 <- overall %>%
      group_by(STATION_ID) %>%
      summarise(stationOverallScore = min(individualScore)) %>%
      left_join(overall, by = c('STATION_ID','stationOverallScore' = 'individualScore')) %>%
      dplyr::select(-individualColor)
    
    return(left_join(vahu6StationSummary, overall2, by = 'STATION_ID') %>%
             mutate(stationColor = case_when(stationOverallScore == 1 ~ 'red',
                                             stationOverallScore == 2 ~ 'yellow',
                                             stationOverallScore == 3 ~ 'green',
                                             stationOverallScore == 4 ~ 'gray'),
                    stationOverallScore = as.factor(stationOverallScore), 
                    `Overall Station Result` = case_when(stationColor == 'red' ~ 'Station contains at least one parameter status of IM or 10.5% Exceedance',
                                                         stationColor == 'yellow' ~ 'Station contains at least one parameter status of IN or Review',
                                                         stationColor == 'green' ~ 'Station contains at least one parameter status of S and no IM, IN, 10.5% Exceedance, or Review',
                                                         stationColor == 'gray' ~ 'Station contains all NA statuses')) %>%
             left_join(parameterSTATcrosswalk, by = 'ParameterSTAT') %>%
             left_join(dplyr::select(stationTable, STATION_ID, LATITUDE, LONGITUDE), by = 'STATION_ID') %>%
             ungroup() %>%
             st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
                      remove = T, # remove these lat/lon cols from df
                      crs = 4326) ) # add projection, needs to be geographic for now bc entering lat/lng
    
    # Gives ranked scale of n statuses
    #  overall <- vahu6StationSummary %>%
    #    group_by(STATION_ID, individualColor, individualScore) %>%
    #    dplyr::summarise(n = n()) %>%
    #    left_join(dplyr::select(stationTable, STATION_ID, LATITUDE, LONGITUDE), by = 'STATION_ID') %>%
    #    ungroup() %>%
    #    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
    #             remove = T, # remove these lat/lon cols from df
    #             crs = 4269)  # add projection, needs to be geographic for now bc entering lat/lng
    #  
    #return(overall)
  } else {
    return(tibble(STATION_ID = NA, ParameterSTAT = NA, Status = NA, individualColor = NA, 
                  individualScore = NA, stationOverallScore = NA, stationColor = NA, Parameter = NA)) }
  #return(tibble(STATION_ID = NA, individualColor = NA, individualScore = NA, n = NA)) }
}




# Loading screen
load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}




withinAssessmentPeriod <- function(x){
  if((range(unique(x$FDT_DATE_TIME))[1] < assessmentPeriod[1]) | 
     (range(unique(x$FDT_DATE_TIME))[2] > assessmentPeriod[2])){
    return('Data included that falls outside of assessment period. Review input data.')
  }else{return('All input data falls within the assessment period.')}
}


# change WQS for a given module
changeWQSfunction <- function(stationData,  # single station dataset
                              inputCLASS_DESCRIPTION){ # from user module)
  WQSvalues <- bind_rows(WQSvalues, 
                         tribble(
                           ~`pH Min`, ~`pH Max`, ~CLASS_DESCRIPTION, 
                           6.5, 9.5, 'SPSTDS = 6.5-9.5'))
  if(inputCLASS_DESCRIPTION != unique(stationData$CLASS_DESCRIPTION)){
    changedWQS <- filter(WQSvalues, CLASS_DESCRIPTION %in% inputCLASS_DESCRIPTION)
    return(dplyr::select(stationData, -c(`Description Of Waters`:CLASS_DESCRIPTION)) %>%
             mutate(CLASS = changedWQS$CLASS, 
                    `Description Of Waters` = changedWQS$`Description Of Waters` ) %>%
             left_join(changedWQS, by = c('CLASS', 'Description Of Waters'))) 
  } else {return(stationData)} 
}








## Old bacteria methods just hanging on for bacteria modules to calculate the old results for comparison, will phase out over time

bacteria_ExceedancesSTV_OLD <- function(x, STVlimit){                                    
  x %>% rename(parameter = !!names(.[2])) %>% # rename columns to make functions easier to apply
    mutate(limit = STVlimit, exceeds = ifelse(parameter > limit, T, F)) # Single Sample Maximum 
}

bacteria_ExceedancesGeomeanOLD <- function(x, bacteriaType, geomeanLimit){
  if(nrow(x) > 0){
    suppressWarnings(mutate(x, SampleDate = format(FDT_DATE_TIME,"%m/%d/%y"), # Separate sampling events by day
                            previousSample=lag(SampleDate,1),
                            previousSampleBacteria=lag(!! bacteriaType,1)) %>% # Line up previous sample with current sample line
                       rowwise() %>% 
                       mutate(sameSampleMonth= as.numeric(strsplit(SampleDate,'/')[[1]][1])  -  as.numeric(strsplit(previousSample,'/')[[1]][1])) %>% # See if sample months are the same, e.g. more than one sample per calendar month
                       filter(sameSampleMonth == 0 | is.na(sameSampleMonth)) %>% # keep only rows with multiple samples per calendar month  or no previous sample (NA) to then test for geomean
                       # USING CALENDAR MONTH BC THAT'S HOW WRITTEN IN GUIDANCE, rolling 4 wk windows would have been more appropriate
                       mutate(sampleMonthYear = paste(month(as.Date(SampleDate,"%m/%d/%y")),year(as.Date(SampleDate,"%m/%d/%y")),sep='/')) %>% # grab sample month and year to group_by() for next analysis
                       group_by(sampleMonthYear) %>%
                       mutate(geoMeanCalendarMonth =  EnvStats::geoMean(as.numeric(get(bacteriaType)), na.rm = TRUE), # Calculate geomean
                              limit = geomeanLimit, samplesPerMonth = n()))
  }
}
# How bacteria is assessed
bacteria_Assessment_OLD <- function(x, bacteriaType, geomeanLimit, STVlimit){
  if(nrow(x)>1){
    bacteria <- dplyr::select(x,FDT_DATE_TIME, !! bacteriaType)%>% # Just get relevant columns, 
      filter(!is.na(!!bacteriaType)) #get rid of NA's
    # Geomean Analysis (if enough n)
    if(nrow(bacteria)>0){
      bacteriaGeomean <- bacteria_ExceedancesGeomeanOLD(bacteria, bacteriaType, geomeanLimit) %>%     
        distinct(sampleMonthYear, .keep_all = T) %>%
        filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) %>% # minimum sampling rule for geomean to apply
        mutate(exceeds = TRUE) %>%
        dplyr::select(sampleMonthYear, geoMeanCalendarMonth, limit, exceeds, samplesPerMonth)
      geomeanResults <- quickStats(bacteriaGeomean, bacteriaType) %>%
        mutate(`Assessment Method` = 'Old Monthly Geomean')
      geomeanResults[,4] <- ifelse(is.na(geomeanResults[,4]),NA, dplyr::recode(geomeanResults[,4], 'Review' = paste('Review if ', bacteriaType,'_VIO > 1',sep='')))
      
      # Single Sample Maximum Analysis
      bacteriaSSM <- bacteria_ExceedancesSTV_OLD(bacteria, STVlimit) 
      SSMresults <- quickStats(bacteriaSSM, bacteriaType) %>% mutate(`Assessment Method` = 'Old Single Sample Maximum')
      return( rbind(geomeanResults, SSMresults) )
    }
  }
  
}


