# need to get from ammoniaAnalysisStation to dataToAnalyze format
# it's fine to leave ammoniaAnalysisStation as is so we can use in the app to unpack data with mostly the same plotly stuff

# pull one station data
stationData <- filter(conventionals, FDT_STA_ID == '1AACO014.57') %>% #"4ACRV006.19") %>% #"1ABIR000.76") %>%# test a specific station
  left_join(stationTable, by = c('FDT_STA_ID' = 'STATION_ID')) %>%
  # Special Standards Correction step. This is done on the actual data bc some special standards have temporal components
  pHSpecialStandardsCorrection() %>% # correct pH to special standards where necessary
  temperatureSpecialStandardsCorrection() %>% # correct temperature special standards where necessary
  # special lake steps
  {if('1AACO014.57' %in% lakeStations$STATION_ID)
    suppressWarnings(suppressMessages(
      mutate(., lakeStation = TRUE) %>%
        thermoclineDepth())) # adds thermocline information and SampleDate
    else mutate(., lakeStation = FALSE) }



# fake data for testing, station == "1AACO014.57"
stationData$AMMONIA_mg_L[2:12] <- rep(25, 11)
ammoniaAnalysisStation <- freshwaterNH3limit(stationData, trout = TRUE, mussels = TRUE, earlyLife = TRUE)

dataToAnalyze2 <- ammoniaAnalysisStation %>% 
  mutate(WindowDateTimeStart = FDT_STA_ID)

Value = AMMONIA_mg_L, parameterRound = ammoniaRound, 



# fake some data for testing acute
stationDataAmmonia$FDT_DATE_TIME[2] <- stationDataAmmonia$FDT_DATE_TIME[1]+ minutes(10)
stationDataAmmonia$FDT_DATE_TIME[3] <- stationDataAmmonia$FDT_DATE_TIME[1]+ minutes(20)

k = stationDataAmmonia$FDT_DATE_TIME[1]



# fake some data for testing chronic
stationDataAmmonia$FDT_DATE_TIME[2] <- stationDataAmmonia$FDT_DATE_TIME[1]+ days(10)
stationDataAmmonia$FDT_DATE_TIME[3] <- stationDataAmmonia$FDT_DATE_TIME[1]+ days(15)
stationDataAmmonia$FDT_DATE_TIME[4] <- stationDataAmmonia$FDT_DATE_TIME[1]+ days(20)
stationDataAmmonia$FDT_DATE_TIME[5] <- stationDataAmmonia$FDT_DATE_TIME[1]+ days(25)
stationDataAmmonia$FDT_DATE_TIME[6] <- stationDataAmmonia$FDT_DATE_TIME[2]+ days(10)
stationDataAmmonia$FDT_DATE_TIME[7] <- stationDataAmmonia$FDT_DATE_TIME[3]+ days(10)
stationDataAmmonia$FDT_DATE_TIME[8] <- stationDataAmmonia$FDT_DATE_TIME[4]+ days(10)
stationDataAmmonia$FDT_DATE_TIME[9] <- stationDataAmmonia$FDT_DATE_TIME[5]+ days(10)

k = stationDataAmmonia$FDT_DATE_TIME[1]



stationData$FDT_DATE_TIME[2] <- stationData$FDT_DATE_TIME[1]+ days(1)
stationData$FDT_DATE_TIME[3] <- stationData$FDT_DATE_TIME[1]+ days(3)
stationData$FDT_DATE_TIME[4] <- stationData$FDT_DATE_TIME[1]+ days(6)
stationData$FDT_DATE_TIME[5] <- stationData$FDT_DATE_TIME[1]+ days(9)
zz <- freshwaterNH3limit(stationData, trout = F, mussels = T, earlyLife  = T) 



zz <- freshwaterNH3limit(stationData, trout = F, mussels = T, earlyLife  = T) 
View(zz %>% 
       arrange(WindowDateTimeStart, `Criteria Type`)) # so can preview associatedData faster


# real parameters for comparison 
trout = ifelse(unique(stationData$CLASS) %in% c('V','VI'), TRUE, FALSE)
mussels = TRUE
earlyLife = TRUE
zz <- freshwaterNH3limit(stationData, trout = trout, mussels = T, earlyLife  = T) 
View(zz %>% 
       arrange(WindowDateTimeStart, `Criteria Type`)) # so can preview associatedData faster
