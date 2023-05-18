stationTableResults <- readRDS('template.RDS')

for(i in 1:nrow(stationTable)){
  #i = 1
  print(paste('Assessing station', i, 'of', nrow(stationTable), sep=' '))
  
  # pull one station data
  stationData <- filter(conventionals, FDT_STA_ID %in% stationTable$STATION_ID[i]) %>%
    #stationData <- filter(conventionals, FDT_STA_ID == '1ABIR000.76') %>% #'1AACO014.57') %>% #"4ACRV006.19") %>% #"1ABIR000.76") %>%# test a specific station
    left_join(stationTable, by = c('FDT_STA_ID' = 'STATION_ID')) %>%
    
    arrange(FDT_DATE_TIME, FDT_DEPTH) %>% # make sure data are in order, citmon can be all over the place
    
    # Special Standards Correction step. This is done on the actual data bc some special standards have temporal components
    pHSpecialStandardsCorrection() %>% # correct pH to special standards where necessary
    temperatureSpecialStandardsCorrection() %>% # correct temperature special standards where necessary
    # special lake steps
    {if(stationTable$STATION_ID[i] %in% lakeStations$STATION_ID)
      #{if('1ABIR000.76' %in% lakeStations$STATION_ID)
      suppressWarnings(suppressMessages(
        mutate(., lakeStation = TRUE) %>%
          thermoclineDepth())) # adds thermocline information and SampleDate
      else mutate(., lakeStation = FALSE) }
  
  # for PWS WCmetals
  WCmetalsStationPWS <- left_join(dplyr::select(stationData, FDT_STA_ID, PWS) %>% distinct(FDT_STA_ID, .keep_all = T),
                                  filter(WCmetalsForAnalysis, Station_Id %in%  stationData$FDT_STA_ID),
                                  by = c('FDT_STA_ID' = 'Station_Id'))
  
  if(nrow(stationData) > 0 | stationTable$STATION_ID[i] %in% extraSites$FDT_STA_ID){ 
    # or statement allows stations with only toxics data to pass through and get flag
    if(nrow(stationData) > 0){
      if(is.na(unique(stationData$PWS))  ){
        PWSconcat <- tibble(PWS= NA)
      } else {
        PWSconcat <- cbind(assessPWSsummary(assessPWS(stationData, NITROGEN_NITRATE_DISSOLVED_00618_mg_L, LEVEL_00618, 10), 'PWS_NitrateDissolved'),
                           assessPWSsummary(assessPWS(stationData, NITROGEN_NITRATE_TOTAL_00620_mg_L, LEVEL_00620, 10), 'PWS_NitrateTotal'),
          assessPWSsummary(assessPWS(stationData, CHLORIDE_DISSOLVED_00941_mg_L, LEVEL_00941, 250), 'PWS_ChlorideDissolved'),
                           assessPWSsummary(assessPWS(stationData, CHLORIDE_TOTAL_00940_mg_L, LEVEL_00940, 250), 'PWS_ChlorideTotal'),
                           assessPWSsummary(assessPWS(stationData, SULFATE_TOTAL_mg_L, LEVEL_SULFATE_TOTAL, 250), 'PWS_Total_Sulfate'),
                           assessPWSsummary(assessPWS(WCmetalsStationPWS, AntimonyTotal, RMK_AntimonyTotal, 5), 'PWS_AntimonyTotal'),
                           assessPWSsummary(assessPWS(WCmetalsStationPWS, ArsenicTotal, RMK_ArsenicTotal, 10), 'PWS_ArsenicTotal'),
                           assessPWSsummary(assessPWS(WCmetalsStationPWS, BariumTotal, RMK_BariumTotal, 2000), 'PWS_BariumTotal'),
                           assessPWSsummary(assessPWS(WCmetalsStationPWS, CadmiumTotal, RMK_CadmiumTotal, 5), 'PWS_CadmiumTotal'),
                           assessPWSsummary(assessPWS(WCmetalsStationPWS, ChromiumTotal, RMK_ChromiumTotal, 100), 'PWS_ChromiumIIITotal'),
                           assessPWSsummary(assessPWS(WCmetalsStationPWS, CopperTotal, RMK_CopperTotal, 1300), 'PWS_CopperTotal'),
                           assessPWSsummary(assessPWS(WCmetalsStationPWS, IronDissolved, RMK_IronDissolved, 300), 'PWS_IronDissolved'),
                           assessPWSsummary(assessPWS(WCmetalsStationPWS, IronTotal, RMK_IronTotal, 300), 'PWS_IronTotal'),
                           assessPWSsummary(assessPWS(WCmetalsStationPWS, LeadTotal, RMK_LeadTotal, 15), 'PWS_LeadTotal'),
                           assessPWSsummary(assessPWS(WCmetalsStationPWS, NickelTotal, RMK_NickelTotal, 610), 'PWS_NickelTotal'),
                           assessPWSsummary(assessPWS(WCmetalsStationPWS, SeleniumTotal, RMK_SeleniumTotal, 170), 'PWS_SeleniumTotal'),
                           assessPWSsummary(assessPWS(WCmetalsStationPWS, ThalliumTotal, RMK_ThalliumTotal, 0.24), 'PWS_ThalliumTotal'),
                           assessPWSsummary(assessPWS(WCmetalsStationPWS, UraniumTotal, RMK_UraniumTotal, 30), 'PWS_UraniumTotal')) %>%
          dplyr::select(-ends_with('exceedanceRate')) }
    } else {PWSconcat <- #tibble(PWS= NA)}
      stationTableResults[0,] %>% dplyr::select(PWS_NitrateDissolved_EXC:PWS_UraniumTotal_MedianExceedance) }
  } else { PWSconcat <- #tibble(PWS= NA)}
    stationTableResults[0,] %>% dplyr::select(PWS_NitrateDissolved_EXC:PWS_UraniumTotal_MedianExceedance) }
  results <- cbind(
    StationTableStartingData(stationData),
    PWSconcat)
  
  stationTableResults <- bind_rows(stationTableResults,results)
}
  
saveRDS(stationTableResults, 'PWSsummary.RDS')
stationTableResults <- readRDS('PWSsummary.RDS')

library(purrr)
# filter to just sites with data
stationsWithData <- stationTableResults %>%
  group_by(STATION_ID) %>% 
  dplyr::select(STATION_ID, contains('_SAMP')) %>% 
  pivot_longer(cols = -c('STATION_ID'), names_to = 'parameter', values_to = 'value') %>% 
  filter(!is.na(value)) %>% 
  distinct(STATION_ID)

stationTableResults1 <- filter(stationTableResults, STATION_ID %in% stationsWithData$STATION_ID)  %>% 
  dplyr::select(-PWS)
write.csv(stationTableResults1, 'PWSwithDissolvedNO3Cl.csv', na = '', row.names = F)    
    