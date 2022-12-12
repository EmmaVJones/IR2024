# test bacteria over all stations 

bacteriaOut <- tibble(StationID = as.character(NA), ECOLI_EXC = as.numeric(NA), ECOLI_SAMP = as.numeric(NA), ECOLI_GM_EXC = as.numeric(NA), ECOLI_GM_SAMP = as.numeric(NA),
                      ECOLI_STAT = as.character(NA), ECOLI_STATECOLI_VERBOSE = as.character(NA),
                      ENTER_EXC = as.numeric(NA), ENTER_SAMP = as.numeric(NA), ENTER_GM_EXC = as.numeric(NA), ENTER_GM_SAMP = as.numeric(NA),
                      ENTER_STAT = as.character(NA), ENTER_STATENTER_VERBOSE = as.character(NA)) 

for(i in 1:nrow(stationTable)){
  #i = 1
  print(paste('Assessing station', i, 'of', nrow(stationTable), sep=' '))
  
  # pull one station data
  stationData <- filter(conventionals, FDT_STA_ID %in% stationTable$STATION_ID[i]) %>%
    #stationData <- filter(conventionals, FDT_STA_ID == '1AACO014.57') %>% #"4ACRV006.19") %>% #"1ABIR000.76") %>%# test a specific station
    left_join(stationTable, by = c('FDT_STA_ID' = 'STATION_ID')) %>%
    # Special Standards Correction step. This is done on the actual data bc some special standards have temporal components
    pHSpecialStandardsCorrection() %>% # correct pH to special standards where necessary
    temperatureSpecialStandardsCorrection() %>% # correct temperature special standards where necessary
    # special lake steps
    {if(stationTable$STATION_ID[i] %in% lakeStations$STATION_ID)
      #{if('1AACO014.57' %in% lakeStations$STATION_ID)
      suppressWarnings(suppressMessages(
        mutate(., lakeStation = TRUE) %>%
          thermoclineDepth())) # adds thermocline information and SampleDate
      else mutate(., lakeStation = FALSE) }
 
  stationBacteria <- bacteriaAssessmentDecisionClass(stationData)
  
  bacteriaOut <- bind_rows(bacteriaOut, stationBacteria)
}

