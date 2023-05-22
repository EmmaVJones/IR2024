
#####################################   UPDATE EACH NEW TOOL REBUILD #############################################
# Establish Assessment Period 
assessmentPeriod <- as.POSIXct( c("2017-01-01 00:00:00 UTC", "2022-12-31 23:59:59 UTC"), tz='UTC')
assessmentCycle <- '2024'
##################################################################################################################

# WQS information for functions
# From: 9VAC25-260-50. Numerical Criteria for Dissolved Oxygen, Ph, and Maximum Temperature
# https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
WQSvalues <- tibble(CLASS_BASIN = c('I',"II","II_7","III","IV","V","VI","VII"),
                    CLASS = c('I',"II","II","III","IV","V","VI","VII"),
                    `Description Of Waters` = c('Open Ocean', 'Tidal Waters in the Chowan Basin and the Atlantic Ocean Basin',
                                                'Tidal Waters in the Chesapeake Bay and its tidal tributaries',
                                                'Nontidal Waters (Coastal and Piedmont Zone)','Mountainous Zone Waters',
                                                'Stockable Trout Waters','Natural Trout Waters','Swamp Waters'),
                    `Dissolved Oxygen Min (mg/L)` = c(5,4,NA,4,4,5,6,NA),
                    `Dissolved Oxygen Daily Avg (mg/L)` = c(NA,5,NA,5,5,6,7,NA),
                    `pH Min` = c(6,6,6.0,6.0,6.0,6.0,6.0,3.7),
                    `pH Max` = c(9.0,9.0,9.0,9.0,9.0,9.0,9.0,8.0),
                    `Max Temperature (C)` = c(NA, NA, NA, 32, 31, 21, 20, NA)) %>%
  mutate(CLASS_DESCRIPTION = paste0(CLASS, " | ", `Description Of Waters`))

# Do we need these functions? cant trace them to any usage
# concatinateUnique <- function(stuff){
#   if(length(stuff)==1){
#     if(is.na(stuff)){return(NA)
#     }else{
#       return(paste(unique(stuff), collapse= ', ')) }
#   } 
#   if(length(stuff) > 1){return(paste(unique(stuff), collapse= ', '))}
# }
# 
# changeDEQRegionName <- function(stuff){
#   # have to do this bc different places in conventionals report the assessment region over sample region
#   if(length(stuff) == 1){
#     if(is.na(stuff)){return("NA")}
#     if(stuff == "Valley"){return('VRO')}
#     if(stuff == "Northern"){return('NRO')}
#     if(stuff == "Piedmont"){return('PRO')}
#     if(stuff == "Blue Ridge"){return('BRRO')}
#     if(stuff == "Tidewater"){return('TRO')}
#     if(stuff == "Southwest" ){return('SWRO')}
#     if(stuff == 'NA'){return('NA')}
#   } else {return(concatinateUnique(stuff))}
# }

# Lake name standardization
lakeNameStandardization <- function(x){
  # flexibility to handle new and old naming conventions
  x %>% 
    {if("WaterName" %in% names(x))
      rename(x, WATER_NAME = WaterName)
      else .} %>% 
    mutate(Lake_Name = case_when(WATER_NAME %in% c('Abel Lake Reservoir (Long Branch)') ~ 'Abel Lake',
                                 WATER_NAME %in% c('Big Cherry Reservior') ~ 'Big Cherry Lake',
                                 WATER_NAME %in% c('Little Stony Creek upstream to Star Branch confluence') ~ 'Bark Camp Lake',
                                 WATER_NAME %in% c('Dan River','Buffalo Creek','Bluestone Creek') ~ 'Kerr Reservoir',
                                 WATER_NAME %in% c('Smith Lake (Aquia Reservoir)') ~ 'Aquia Reservoir (Smith Lake)',
                                 WATER_NAME %in% c('Claytor Lake (New River)', 'Claytor Lake (Peak Creek)',
                                                   'Claytor Lake Lower (New River)') ~ 'Claytor Lake',
                                 WATER_NAME %in% c('Fairystone Lake (Goblin Town Creek)') ~ 'Fairystone Lake', 
                                 WATER_NAME %in% c('Harwood Mill Reservoir (PWS)') ~ 'Harwood Mills Reservoir',
                                 WATER_NAME %in% c('Goose Creek') ~ 'Goose Creek Reservoir',
                                 WATER_NAME %in% c('Lake Anna', 'Lake Anna/Contrary Creek', 'Lake Anna/Freshwater Creek', 
                                                   'Lake Anna/Gold Mine Creek', 'Lake Anna/Pamunkey Creek', 
                                                   'Lake Anna/Plentiful Creek', 'Terrys Run/Lake Anna') ~ 'Lake Anna',
                                 WATER_NAME %in% c('Lake Cohoon (PWS)') ~ 'Lake Cohoon',     
                                 WATER_NAME %in% c('Conner Lake') ~ 'Lake Conner',
                                 WATER_NAME %in% c('Lake Kilby (PWS)') ~ 'Lake Kilby',
                                 WATER_NAME %in% c('Lake Meade (PWS)','Pitch Kettle Creek - Lake (PWS)') ~ 'Lake Meade',
                                 WATER_NAME %in% c('Lake Moomaw (Jackson River)','Lake Moomaw Middle (Jackson River)') ~ 'Lake Moomaw',
                                 WATER_NAME %in% c('Lake Prince - Reservoir (PWS)') ~ 'Lake Prince',
                                 WATER_NAME %in% c('Lake Shenandoah') ~ 'Shenandoah Lake',
                                 WATER_NAME %in% c('Lake Smith (PWS)') ~ 'Lake Smith',
                                 WATER_NAME %in% c('Lake Whitehurst (PWS)') ~ 'Lake Whitehurst',
                                 WATER_NAME %in% c('Leesville Lake', 'Leesville Lake (Pigg R.)', 
                                                   'Leesville Lake Middle (Roanoke R.)') ~ 'Leesville Lake',
                                 WATER_NAME %in% c('Lee Hall Reservoir- Upper, Middle','Lee Hall Reservoir-Lower') ~ 'Lee Hall Reservoir',
                                 WATER_NAME %in% c('Little Creek Reservoir - (PWS)') ~ 'Little Creek Reservoir (VBC)',
                                 WATER_NAME %in% c('Little Creek Reservoir (PWS)') ~ 'Little Creek Reservoir (JCC)',
                                 WATER_NAME %in% c('Lonestar Lake F (PWS)') ~ 'Lone Star Lake F (Crystal Lake)', 
                                 WATER_NAME %in% c('Lone Star Lake G (PWS)') ~ 'Lone Star Lake G (Crane Lake)', 
                                 WATER_NAME %in% c('Lone Star Lake I (PWS)') ~ 'Lone Star Lake I (Butler Lake)', 
                                 WATER_NAME %in% c('Martinsville (Beaver Creek) Reservoir',
                                                   'Martinsville Reservoir') ~ 'Martinsville Reservoir (Beaver Creek Reservoir)',
                                 WATER_NAME %in% c('Moormans River') ~ 'Sugar Hollow Reservoir',
                                 WATER_NAME %in% c('North River') ~ 'Staunton Dam Lake',
                                 WATER_NAME %in% c('Philpott Reservoir (Goblin Town Creek)', 'Philpott Reservoir Lower (Smith River)',
                                                   'Philpott Reservoir (Smith River)') ~ "Philpott Reservoir",
                                 WATER_NAME %in% c('Roanoke River','Lake Gaston') ~ 'Lake Gaston',     
                                 WATER_NAME %in% c('Roaring Fork Reservoir') ~ 'Roaring Fork',
                                 WATER_NAME %in% c('S F Rivanna River Reservoir') ~ 'Rivanna Reservoir (South Fork Rivanna Reservoir)',
                                 str_detect(WATER_NAME, 'Smith Mtn. Lake') ~ 'Smith Mountain Lake', 
                                 WATER_NAME %in% c('Speights Run - Lake (PWS)') ~ 'Speights Run Lake',
                                 WATER_NAME %in% c('Troublesome Reservoir') ~ 'Troublesome Creek Reservoir',
                                 WATER_NAME %in% c('Waller Mill Reservoir [PWS]') ~ 'Waller Mill Reservoir',
                                 WATER_NAME %in% c('Unnamed pond near Tanyard Swamp') ~ 'Tanyard Swamp',
                                 WATER_NAME %in% c('Unsegmented lakes in G03') ~ 'West Run',
                                 TRUE ~ as.character(WATER_NAME))) 
}
# regionalAUs %>% lakeNameStandardization()


quickStats <- function(parameterDataset, # dataset from one station that has been run through a parameter exceedance analysis function 
                       #                  (e.g. tempExceedances(), DOExceedances_Min(), pHexceedances())
                       parameter, # the name of the parameter as you want it to appear in final stations table output
                       drop7Q10 = FALSE # drop any records from this analysis with a 7Q10 flag, default is false but can be overridden in apps
                       ){
  # Drop any 7Q10 flagged data from exceedance analyses for appropriate parameters
  if(drop7Q10 == TRUE &
     parameter %in% c("TEMP", "DO", "DO_Daily_Avg", "PH")){
    parameterDataset <- dplyr::filter(parameterDataset, is.na(`7Q10 Flag`)) # only assess on assessable dataset
  }
  
  if(nrow(parameterDataset) > 0 & any(!is.na(parameterDataset$limit))){
    results <- data.frame(EXC = nrow(dplyr::filter(parameterDataset, exceeds == TRUE)),
                          SAMP = nrow(parameterDataset)) %>%
      # Implement Round to Even on Exceedance Frequency
      dplyr::mutate(exceedanceRate = as.numeric(round::roundAll((EXC/SAMP)*100,digits=0, "r0.C"))) # round to nearest whole number per Memo to Standardize Rounding for Assessment Guidance
    
    if(results$EXC >= 1){outcome <- 'Review'} # for Mary
    if(results$EXC >= 1 & results$exceedanceRate < 10.5){outcome <- 'Review'}
    if(results$exceedanceRate > 10.5 & results$EXC >= 2 & results$SAMP > 10){outcome <- '10.5% Exceedance'}
    if(results$EXC < 1 &results$exceedanceRate < 10.5 & results$SAMP > 10){outcome <- 'S'}
    if(results$EXC >= 1 & results$SAMP <= 10){outcome <- 'Review'}
    if(results$EXC < 1 & results$SAMP <= 10 & results$SAMP > 1){outcome <- 'S'} # & results$SAMP >1 new 12/21/2020 can't say supporting on 1 sample
    if(results$EXC < 1 & results$SAMP <= 10 & results$SAMP == 1){outcome <- 'Review'} # & results$SAMP >1 new 12/21/2020 can't say supporting on 1 sample
    
    
    results <- dplyr::mutate(results, STAT = outcome)
    names(results) <- c(paste(parameter,names(results)[1], sep = '_'),
                        paste(parameter,names(results)[2], sep = '_'),
                        paste(parameter,names(results)[3], sep = '_'),
                        paste(parameter,names(results)[4], sep = '_'))
    #rename based on parameter entered
    return(results)
  } else {
    if(nrow(parameterDataset) == 0){
      z <- data.frame(EXC = NA, SAMP= NA, exceedanceRate= NA, STAT= NA)
      names(z) <- paste(parameter,names(z), sep='_')
      return(z)
    } else {
      z <- data.frame(EXC = NA, SAMP= nrow(parameterDataset), exceedanceRate= NA, STAT= paste(parameter, 'WQS info missing from analysis'))
      names(z) <- paste(parameter,names(z), sep='_')
      return(z)
    }
    
  }
}


StationTableStartingData <- function(stationData){
  stationData %>%
    dplyr::select(FDT_STA_ID, ID305B_1:VAHU6) %>%
    dplyr::rename('STATION_ID' = 'FDT_STA_ID') %>%
    dplyr::distinct(STATION_ID, .keep_all = T)
}


stationTableComments <- function(stations, 
                                 previousStationTable, 
                                 previousStationTableCycle,
                                 previousStationTable2,
                                 previousStationTable2Cycle){
  lastComment <- filter(previousStationTable, `Station Id` %in% stations) %>%
    dplyr::select(`Station Id`, Comments)
  names(lastComment) <- c('STATION_ID', paste(previousStationTableCycle, 'IR COMMENTS'))
  lastComment2 <- filter(previousStationTable2, `Station Id` %in% stations) %>%
    dplyr::select(`Station Id`, Comments)
  names(lastComment2) <- c('STATION_ID', paste(previousStationTable2Cycle, 'IR COMMENTS'))
  return(left_join(lastComment, lastComment2, by= 'STATION_ID'))
}



# Calculate daily thermocline depth and designate Epilimnion vs Hypolimnion
thermoclineDepth <- function(stationData){
  stationData <- stationData %>%
    mutate(SampleDate = as.Date(FDT_DATE_TIME)) %>%
    group_by(FDT_STA_ID, SampleDate) 
  
  dailyThermDepth <- dplyr::select(stationData, FDT_STA_ID, SampleDate, FDT_DEPTH, FDT_TEMP_CELCIUS) %>%
    filter(!is.na(FDT_TEMP_CELCIUS)) %>% 
    filter(!is.na(FDT_DEPTH)) %>% 
    arrange(SampleDate, FDT_DEPTH) %>% # make sure data are in order
    mutate(DepthDiff = c(NA, diff(FDT_DEPTH)),
           TempDiff = c(NA, diff(FDT_TEMP_CELCIUS))) %>%
    filter(DepthDiff >= 1) # get rid of changes less than 1 meter depth
  # Alt route in case shallow lake
  if(nrow(dailyThermDepth) > 0){
    dailyThermDepth <- filter(dailyThermDepth, TempDiff <= -1)
    # one more catch if no thermocline established
    if(nrow(dailyThermDepth) > 0){
      dailyThermDepth <- summarise(dailyThermDepth, ThermoclineDepth = min(FDT_DEPTH) - 0.5) %>% ungroup() 
    } else {
      dailyThermDepth <- summarise(stationData, ThermoclineDepth = NA) %>% ungroup()  }
  } else {
    dailyThermDepth <- summarise(stationData, ThermoclineDepth = NA) %>% ungroup() }
    
  
  full_join(stationData, dailyThermDepth, by = c('FDT_STA_ID', 'SampleDate')) %>%
    mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))%>% ungroup() 
}
# stationData %>% thermoclineDepth()


#Max Temperature Exceedance Function
tempExceedances <- function(stationData){
  dplyr::select(stationData, FDT_DATE_TIME, FDT_DEPTH, tidyselect::contains('TEMP_CELCIUS'), `Max Temperature (C)`, `7Q10 Flag`) %>% # Just get relevant columns 
    dplyr::filter(! (LEVEL_FDT_TEMP_CELCIUS %in% c('Level II', 'Level I'))) %>% # get lower levels out
    dplyr::filter(! is.na(FDT_TEMP_CELCIUS)) %>% # get rid of NA's
    # rename columns to make exceedance analyses easier to apply
    dplyr::rename(parameter = !! names(.[3]),
           limit = !! names(.[6])) %>% 
    # Apply Round to Even Rule before testing for exceedances
    dplyr::mutate(parameterRound = signif(parameter, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
                  exceeds = case_when(parameterRound > limit ~ TRUE, # Identify where above max Temperature, 
                                      parameterRound <= limit ~ FALSE, # no exceedance
                                      TRUE ~ NA))
}
# tempExceedances(stationData) %>%
#  quickStats('TEMP')

# temperature special standards adjustment function
# this is applied to the actual monitoring data because there is a temporal component to these criteria
temperatureSpecialStandardsCorrection <- function(x){
  # ee. Maximum temperature for these seasonally stockable trout waters is 26°C and applies May 1 through October 31. https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section310/
  # ff. Maximum temperature for these seasonally stockable trout waters is 28°C and applies May 1 through October 31. https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section310/
  # hh. Maximum temperature for these seasonally stockable trout waters is 31°C and applies May 1 through October 31. https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section310/
  mutate(x, `Max Temperature (C)` = case_when(str_detect(as.character(SPSTDS), 'ee') & month(FDT_DATE_TIME) %in% 5:10 ~ 26,
                                              str_detect(as.character(SPSTDS), 'ff') & month(FDT_DATE_TIME) %in% 5:10 ~ 28,
                                              str_detect(as.character(SPSTDS), 'hh') & month(FDT_DATE_TIME) %in% 5:10 ~ 31,
                                              TRUE ~ `Max Temperature (C)`)) 
}


# Minimum DO Exceedance function
DOExceedances_Min <- function(stationData){
  # special step for lake stations, remove samples based on lake assessment guidance 
  if(unique(stationData$lakeStation) == TRUE){
    if(!is.na(unique(stationData$Lakes_187B)) & unique(stationData$Lakes_187B) == 'y'){
      DOdata <- dplyr::filter(stationData, LakeStratification %in% c("Epilimnion", NA)) %>% # only use epilimnion or unstratified samples for analysis
        dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, DO_mg_L, RMK_DO, LEVEL_DO, 
                      `Dissolved Oxygen Min (mg/L)`, LakeStratification, `7Q10 Flag`) # Just get relevant columns,
    } else {
      DOdata <- dplyr::select(stationData, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, DO_mg_L, RMK_DO, LEVEL_DO,
                              `Dissolved Oxygen Min (mg/L)`, LakeStratification, `7Q10 Flag`) }# Just get relevant columns,
  } else {
    DOdata <- dplyr::select(stationData, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, DO_mg_L, RMK_DO, LEVEL_DO, 
                            `Dissolved Oxygen Min (mg/L)`, `7Q10 Flag`) # Just get relevant columns, 
  }
  
  DOdata %>%
    dplyr::filter(!(LEVEL_DO %in% c('Level II', 'Level I'))) %>% # get lower levels out
    dplyr::filter(!is.na(DO_mg_L)) %>% 
    dplyr::rename(parameter = !!names(.[4]), limit = !!names(.[7])) %>% # rename columns to make functions easier to apply
    # Round to Even Rule
    dplyr::mutate(parameterRound = signif(parameter, digits = 2), # two significant figures based on  https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
                  exceeds = case_when(parameterRound < limit ~ TRUE, # Identify where below min DO 
                                      parameterRound >= limit ~ FALSE, # no exceedance
                                      TRUE ~ NA))
  }
#DOExceedances_Min(stationData) %>% quickStats('DO')


# Daily Average exceedance function
DO_Assessment_DailyAvg <- function(stationData){ 
  # special step for lake stations, remove samples based on lake assessment guidance 
  if(unique(stationData$lakeStation) == TRUE){
    if(!is.na(unique(stationData$Lakes_187B)) & unique(stationData$Lakes_187B) == 'y'){
      DOdata <- dplyr::filter(stationData, LakeStratification %in% c("Epilimnion", NA)) %>% # only use epilimnion or unstratified samples for analysis
        dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, DO_mg_L, RMK_DO, LEVEL_DO, 
                      `Dissolved Oxygen Daily Avg (mg/L)`, LakeStratification, `7Q10 Flag`) # Just get relevant columns,
    } else {
      DOdata <- dplyr::select(stationData, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, DO_mg_L, RMK_DO, LEVEL_DO,
                              `Dissolved Oxygen Daily Avg (mg/L)`, LakeStratification, `7Q10 Flag`) }# Just get relevant columns,
  } else {
    DOdata <- dplyr::select(stationData, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, DO_mg_L, RMK_DO, LEVEL_DO, 
                            `Dissolved Oxygen Daily Avg (mg/L)`, `7Q10 Flag`) # Just get relevant columns, 
  }
  
  
  DOdata %>% 
    dplyr::filter(!(LEVEL_DO %in% c('Level II', 'Level I'))) %>% # get lower levels out
    dplyr::filter(!is.na(DO_mg_L)) %>% #get rid of NA's
    dplyr::mutate(date = as.Date(FDT_DATE_TIME, format="%m/%d/%Y"), 
                  limit = `Dissolved Oxygen Daily Avg (mg/L)`) %>% 
    dplyr::group_by(date) %>%
    dplyr::mutate(n_Samples_Daily = n()) %>% # how many samples per day?
    dplyr::filter(n_Samples_Daily > 1) %>%
      # Daily average with average rounded to even
    dplyr::mutate(DO_DailyAverage = signif(mean(DO_mg_L), digits = 2),  # two significant figures based on  https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
                  exceeds =  case_when(DO_DailyAverage < limit & is.na(`7Q10 Flag`) ~ TRUE, # exceedance if above limit and no 7Q10 Flag
                                       DO_DailyAverage < limit & `7Q10 Flag` == "7Q10 Flag" ~ FALSE,  # exceedance if above limit and 7Q10 Flag
                                       DO_DailyAverage >= limit ~ FALSE, # no exceedance
                                       TRUE ~ NA)) %>% 
                    #ifelse(DO_DailyAverage < `Dissolved Oxygen Daily Avg (mg/L)`,T,F)) %>% 
    dplyr::ungroup() %>%
    distinct(date, .keep_all = T) %>% 
    dplyr::select(-c(FDT_DATE_TIME)) 
}
#DO_Assessment_DailyAvg(stationData) %>% quickStats('DO_Daily_Avg') 


pHSpecialStandardsCorrection <- function(stationData){
  mutate(stationData, `pH Min` = case_when(str_detect(as.character(SPSTDS), '6.5-9.5') ~ 6.5, TRUE ~ `pH Min`),
         `pH Max` = case_when(str_detect(as.character(SPSTDS), '6.5-9.5') ~ 9.5, TRUE ~ `pH Max`))
}
  
# pH range Exceedance Function
pHExceedances <- function(stationData){
  # special step for lake stations, remove samples based on lake assessment guidance 
  if(unique(stationData$lakeStation) == TRUE){
    if(!is.na(unique(stationData$Lakes_187B)) & unique(stationData$Lakes_187B) == 'y'){
      pHdata <- filter(stationData, LakeStratification %in% c("Epilimnion", NA)) %>% # only use epilimnion or unstratified samples for analysis
        dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_FIELD_PH, RMK_FDT_FIELD_PH, LEVEL_FDT_FIELD_PH, 
                      `pH Min`, `pH Max`, LakeStratification, `7Q10 Flag`) # Just get relevant columns,
    } else {
      pHdata <- dplyr::select(stationData, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_FIELD_PH, RMK_FDT_FIELD_PH, LEVEL_FDT_FIELD_PH,
                              `pH Min`, `pH Max`, LakeStratification, `7Q10 Flag`) }# Just get relevant columns,
  } else {
    pHdata <- dplyr::select(stationData, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_FIELD_PH, RMK_FDT_FIELD_PH, LEVEL_FDT_FIELD_PH, 
                            `pH Min`, `pH Max`, `7Q10 Flag`) }# Just get relevant columns, 
  
  pHdata <- filter(pHdata, !(LEVEL_FDT_FIELD_PH %in% c('Level II', 'Level I'))) %>% # get lower levels out
    filter(!is.na(FDT_FIELD_PH)) #get rid of NA's
    
  # only run analysis if WQS exist for station
    if(any(is.na(pHdata$`pH Min`)) | any(is.na(pHdata$`pH Max`))){
      pH <- mutate(pHdata, interval = 1, exceeds = FALSE, limit = `pH Min`) # placeholder to run quickStats() without any WQS
    } else {
      pH <- pHdata %>%
        rowwise() %>% 
        # Round to Even Rule
        mutate(parameterRound = signif(FDT_FIELD_PH, digits = 2)) %>% # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
        mutate(interval=findInterval(parameterRound,c(`pH Min`,`pH Max`), left.open=TRUE, rightmost.closed = TRUE)) %>% # Identify where pH outside of assessment range with round to even
        ungroup()%>%
        mutate(exceeds = case_when(interval != 1 ~ TRUE, #  Highlight where pH doesn't fall into criteria range
                                   interval == 1 ~ FALSE,  # no exceedance if inside limit
                                   TRUE ~ NA),
               limit = `pH Min`) # placeholder for quickStats function, carries over whether or not station has WQS attributed
    }
  return(pH)
}
#pHExceedances(stationData) %>% quickStats('PH')



# Consolidate water column metals assessment decisions calculated by Roger Stewart
# Fuction exactly the same as 2020 cycle and may need updates if dataset changes
metalsExceedances <- function(x, metalType){
  # if any data given to function
  if(nrow(x) > 0){ VIO <- length(which(x == 'NSP')) 
  }else {
    VIO <- NA  }
  
  x <- data.frame(VIO = VIO, STAT = ifelse(VIO > 0, 'Review', 'S'))
  names(x) <- paste(metalType,names(x), sep='_')
  return(x)
}


## Identify if further review needs to happen for PCB or metals data 
PCBmetalsDataExists <- function(datasetType, # any of the preorganized PCB or fish tissue datasets
                                parameterType # field name to pass through to Station Table output
                                ){
  # if any data given to function
  if(nrow(datasetType) > 0){ 
    x <- data.frame(EXC = NA, STAT = 'Review')
  }else {
    x <- data.frame(EXC = NA, STAT = NA) }
  
  names(x) <- paste(parameterType, names(x), sep='_')
  return(x)
}

# PCBmetalsDataExists(filter(markPCB, str_detect(SampleMedia, 'Water')) %>%
#                       filter(StationID %in% stationData$FDT_STA_ID), 'WAT_TOX')
# PCBmetalsDataExists(filter(fishMetals, Station_ID %in% stationData$FDT_STA_ID), 'FISH_MET')
# PCBmetalsDataExists(filter(fishPCB, `DEQ rivermile` %in%  stationData$FDT_STA_ID), 'FISH_TOX')

#### PWS Assessment Functions ---------------------------------------------------------------------------------------------------
# The app doesn't use this function for modules because you need to be able to toggle assessment on/off with WQS adjustment on the
# fly, but the automated functions need PWS filter programmed in to ease automating over thousands of sites

assessPWS <- function(stationData, fieldName, commentName, PWSlimit){
  if(unique(stationData$PWS) %in% c("Yes")){
    fieldName_ <- enquo(fieldName)
    commentName_ <- enquo(commentName)
    parameterData <- dplyr::select(stationData, FDT_DATE_TIME, !! fieldName_, !! commentName_) %>%
      filter(!( !! commentName_ %in% c('Level II', 'Level I'))) %>% # get lower levels out
      filter(!is.na(!!fieldName_ )) %>% #get rid of NA's
      # rename(parameter = !!names(.[2])) #%>% # rename columns to make functions easier to apply
      mutate(`Parameter Rounded to WQS Format` = signif(!! fieldName_, digits = 2),  # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
             `Parameter Median` = median(!! fieldName_),
             `Parameter Median Rounded to WQS Format` = signif(`Parameter Median`, digits = 2),  # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
             limit =  PWSlimit) %>%
      mutate(exceeds = ifelse(`Parameter Rounded to WQS Format` > limit, T, F),
             medianExceeds =  ifelse(`Parameter Median Rounded to WQS Format` > limit, T, F),) # Identify where above WQS limit
    return(parameterData)
  }
  return(NULL)
}


  

#assessPWS(stationData, NITRATE_mg_L, LEVEL_NITRATE, 10)
#assessPWS(stationData, CHLORIDE_mg_L, LEVEL_CHLORIDE, 250)
#assessPWS(stationData, SULFATE_TOTAL_mg_L, LEVEL_SULFATE_TOTAL, 250)
# WCmetalsStationPWS <- left_join(dplyr::select(stationData, FDT_STA_ID, PWS) %>% distinct(FDT_STA_ID, .keep_all = T),
#                                 filter(WCmetalsForAnalysis, Station_Id %in%  stationData$FDT_STA_ID),
#                                 by = c('FDT_STA_ID' = 'Station_Id'))
# assessPWS(WCmetalsStationPWS, Antimony, RMK_Antimony, 5)




assessPWSsummary <- function(assessPWSresults, 
                             outputName){
  if(is.null(assessPWSresults) ){
    z <- tibble(`_EXC` = as.numeric(NA), `_SAMP` = as.numeric(NA), `_exceedanceRate` = as.numeric(NA), 
                `_STAT` = as.character(NA), `_MedianExceedance` = NA)
    names(z) <- paste0(outputName, names(z))
  } else{
    if(nrow(assessPWSresults) == 0){
      z <- tibble(`_EXC` = as.numeric(NA), `_SAMP` = as.numeric(NA), `_exceedanceRate` = as.numeric(NA), 
                  `_STAT` = as.character(NA), `_MedianExceedance` = NA)
      names(z) <- paste0(outputName, names(z))
    } else{
      #if(nrow(assessPWSresults) > 0){
      z <- quickStats(assessPWSresults, outputName) %>% 
        mutate(MedianExceedance = unique(assessPWSresults$medianExceeds))
      names(z)[5] <- paste0(outputName, '_MedianExceedance')
    }
  }
  
    
  return(z)
}

# assessPWSsummary(assessPWS(stationData, NITROGEN_NITRATE_TOTAL_00620_mg_L, LEVEL_00620, 10), 'PWS_NitrateTotal')
# assessPWSsummary(assessPWS(stationData, CHLORIDE_TOTAL_00940_mg_L, LEVEL_00940, 250), 'PWS_ChlorideTotal')
# assessPWSsummary(assessPWS(stationData, SULFATE_TOTAL_mg_L, LEVEL_SULFATE_TOTAL, 250), 'PWS_Total_Sulfate')
# assessPWSsummary(assessPWS(WCmetalsStationPWS, AntimonyTotal, RMK_AntimonyTotal, 5), 'PWS_AntimonyTotal')
# assessPWSsummary(assessPWS(WCmetalsStationPWS, ArsenicTotal, RMK_ArsenicTotal, 10), 'PWS_ArsenicTotal')
# assessPWSsummary(assessPWS(WCmetalsStationPWS, BariumTotal, RMK_BariumTotal, 2000), 'PWS_BariumTotal')
# assessPWSsummary(assessPWS(WCmetalsStationPWS, CadmiumTotal, RMK_CadmiumTotal, 5), 'PWS_CadmiumTotal')
# assessPWSsummary(assessPWS(WCmetalsStationPWS, ChromiumTotal, RMK_ChromiumTotal, 100), 'PWS_ChromiumIIITotal')
# assessPWSsummary(assessPWS(WCmetalsStationPWS, CopperTotal, RMK_CopperTotal, 1300), 'PWS_CopperTotal')
# assessPWSsummary(assessPWS(WCmetalsStationPWS, IronDissolved, RMK_IronDissolved, 300), 'PWS_IronDissolved')
# assessPWSsummary(assessPWS(WCmetalsStationPWS, IronTotal, RMK_IronTotal, 300), 'PWS_IronTotal')
# assessPWSsummary(assessPWS(WCmetalsStationPWS, LeadTotal, RMK_LeadTotal, 15), 'PWS_LeadTotal')
# assessPWSsummary(assessPWS(WCmetalsStationPWS, NickelTotal, RMK_NickelTotal, 610), 'PWS_NickelTotal')
# assessPWSsummary(assessPWS(WCmetalsStationPWS, SeleniumTotal, RMK_SeleniumTotal, 170), 'PWS_SeleniumTotal')
# assessPWSsummary(assessPWS(WCmetalsStationPWS, ThalliumTotal, RMK_ThalliumTotal, 0.24), 'PWS_ThalliumTotal')
# assessPWSsummary(assessPWS(WCmetalsStationPWS, UraniumTotal, RMK_UraniumTotal, 30), 'PWS_UraniumTotal')




# Nutrients pseudo-assessment functions (for Riverine applications)

# Count samples
countNutrients <- function(stationData, fieldName, commentName, nutrientLimit){
  fieldName_ <- enquo(fieldName)
  commentName_ <- enquo(commentName)
  
  dplyr::select(stationData,FDT_STA_ID,FDT_DATE_TIME, !! fieldName_, !! commentName_)%>% # Just get relevant columns
    filter(!( !! commentName_ %in% c('Level II', 'Level I'))) %>% # get lower levels out
    filter(!is.na(!!fieldName_ )) %>% #get rid of NA's
    rename(parameter = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(limit = nutrientLimit, 
           exceeds = ifelse(parameter > limit, T, F)) # Identify where above WQS limit
}
#countNutrients(stationData, PHOSPHORUS_mg_L, LEVEL_PHOSPHORUS, 0.2) %>% quickStats('NUT_TP') # but no longer use 0.2 riverine flag after 12/21/2020 email with Tish/Amanda
#countNutrients(stationData, CHLOROPHYLL_A_ug_L, LEVEL_CHLOROPHYLL_A, NA)  %>% quickStats('NUT_CHLA')



#### Lake Chlorophyll a Assessment Functions ---------------------------------------------------------------------------------------------------

chlA_analysis <- function(x){
  if(!is.na(unique(x$Lakes_187B))){
    if(unique(x$Lakes_187B) == 'y'){
      x <- filter(x, LACUSTRINE == 'Y')  } }
  
  chla <- filter(x, !is.na(CHLOROPHYLL_A_ug_L)) %>%
    filter(FDT_DEPTH <= 1) %>% # Guidance calls for top meter only
    filter(!( LEVEL_CHLOROPHYLL_A %in% c('Level II', 'Level I'))) %>% # get lower levels out
    dplyr::select(FDT_STA_ID, FDT_DEPTH, FDT_DATE_TIME, SampleDate, CHLOROPHYLL_A_ug_L, `Chlorophyll a (ug/L)`, LACUSTRINE)%>%
    mutate(Year= year(FDT_DATE_TIME), Month=month(FDT_DATE_TIME)) %>%
    filter(Month %in% c(4, 5, 6, 7, 8, 9, 10)) # make sure only assess valid sample months
  if(length(unique(chla$FDT_STA_ID)) > 1){
    chlaResults <- chla %>%
      group_by(Month, Year) %>%
      summarise(samplesPerMonth = n(),
             medianCHLOROPHYLL_A_ug_L = median(CHLOROPHYLL_A_ug_L, na.rm = T),
             `Chlorophyll a (ug/L)` = unique(`Chlorophyll a (ug/L)`)) %>%
      ungroup() %>%
      group_by(Year) %>%
      summarise(samplesPerYear = n(),
                pct90 = quantile(medianCHLOROPHYLL_A_ug_L, 0.9),
                `Chlorophyll a (ug/L)` = unique(`Chlorophyll a (ug/L)`)) %>%
      mutate(`90th Percentile Rounded to WQS Format` = signif(pct90, digits = 2),  # two significant figures based on WQS https://lis.virginia.gov/cgi-bin/legp604.exe?000+reg+9VAC25-260-187&000+reg+9VAC25-260-187
             chlA_Exceedance = ifelse(`90th Percentile Rounded to WQS Format` > `Chlorophyll a (ug/L)`, T, F),
             ID305B = unique(x$ID305B_1))  %>%
      dplyr::select(ID305B, Year, samplesPerYear, pct90, `90th Percentile Rounded to WQS Format`, everything())
  } else {
    chlaResults <- chla %>%
      group_by(Year) %>%
      mutate(samplesPerYear = n(),
             pct90 = quantile(CHLOROPHYLL_A_ug_L, 0.9),
             `90th Percentile Rounded to WQS Format` = signif(pct90, digits = 2),  # two significant figures based on WQS https://lis.virginia.gov/cgi-bin/legp604.exe?000+reg+9VAC25-260-187&000+reg+9VAC25-260-187
             chlA_Exceedance = ifelse(`90th Percentile Rounded to WQS Format` > `Chlorophyll a (ug/L)`, T, F)) %>%
      dplyr::select(FDT_STA_ID, Year, samplesPerYear, pct90, `90th Percentile Rounded to WQS Format`,`Chlorophyll a (ug/L)`, chlA_Exceedance, LACUSTRINE) %>%
      distinct(Year, .keep_all=T)
  }
  return(chlaResults)
}

#chlA_analysis(stationData1)

chlA_Assessment <- function(x){
  chlA_Results <- chlA_analysis(x) %>% ungroup()
  
  if(nrow(chlA_Results) > 0){
    if(is.na(unique(chlA_Results$`Chlorophyll a (ug/L)`))){ # bail out if nutrient standards didn't join properly
      return(tibble(NUT_CHLA_EXC= NA, NUT_CHLA_SAMP = NA,	NUT_CHLA_STAT = NA))}
    validYears <- filter(chlA_Results, samplesPerYear >= 6) # need at least 6 samples per year
    mostRecent2years <- slice_max(validYears, Year, n = 2) # get most recent two years of results
    if(nrow(mostRecent2years) == 2){ 
      if(all(unique(mostRecent2years$chlA_Exceedance) == FALSE)){ # no exceedances in last two years
        return(tibble(NUT_CHLA_EXC= 0, NUT_CHLA_SAMP = nrow(mostRecent2years),	NUT_CHLA_STAT = 'S') )
      } else { # at least one chlA_Exceedance exists
        if(all(unique(mostRecent2years$chlA_Exceedance)) == TRUE){ # both years exceed
          return(tibble(NUT_CHLA_EXC= nrow(mostRecent2years), NUT_CHLA_SAMP = nrow(mostRecent2years),	NUT_CHLA_STAT = 'IM'))
        } else { # run a tiebreak with third most recent year
          mostRecent3years <- slice_max(validYears, Year, n = 3) # get most recent three years of results
          mostRecent3yearsExceed <- filter(mostRecent3years, chlA_Exceedance == TRUE)
          if(nrow(mostRecent3yearsExceed) >= 2){
            return(tibble(NUT_CHLA_EXC= nrow(mostRecent3yearsExceed), NUT_CHLA_SAMP = nrow(mostRecent3years),	NUT_CHLA_STAT = 'IM'))
          } else {
            return(tibble(NUT_CHLA_EXC= nrow(mostRecent3yearsExceed), NUT_CHLA_SAMP = nrow(mostRecent3years),	NUT_CHLA_STAT = 'Review')) }
        }}} else {return(tibble(NUT_CHLA_EXC= NA, NUT_CHLA_SAMP = nrow(validYears),	NUT_CHLA_STAT = 'IN') ) }
    } else {    return(tibble(NUT_CHLA_EXC= NA, NUT_CHLA_SAMP = NA,	NUT_CHLA_STAT = NA) )  }
}

#chlA_Assessment(stationData1)
#chlA_Assessment(AUData1)



#### Lake Total Phosphorus Assessment Functions ---------------------------------------------------------------------------------------------------

TP_analysis <- function(x){
  if(!is.na(unique(x$Lakes_187B))){
    if(unique(x$Lakes_187B) == 'y'){
      x <- filter(x, LACUSTRINE == 'Y')  }
  }
  
  
  TP <- filter(x, !is.na(PHOSPHORUS_mg_L)) %>%
    filter(FDT_DEPTH <= 1) %>% # Guidance calls for top meter only
    filter(!( LEVEL_PHOSPHORUS %in% c('Level II', 'Level I'))) %>% # get lower levels out
    dplyr::select(FDT_STA_ID, FDT_DEPTH, FDT_DATE_TIME, SampleDate, PHOSPHORUS_mg_L, `Total Phosphorus (mg/L)`, LACUSTRINE)%>%
    mutate(Year= year(FDT_DATE_TIME), Month=month(FDT_DATE_TIME)) %>%
    filter(Month %in% c(4, 5, 6, 7, 8, 9, 10)) # make sure only assess valid sample months
  if(length(unique(TP$FDT_STA_ID)) > 1){
    TPResults <- TP %>%
      group_by(Month, Year) %>%
      summarise(samplesPerMonth = n(),
                medianPHOSPHORUS_mg_L = median(PHOSPHORUS_mg_L, na.rm = T),
                `Total Phosphorus (mg/L)` = unique(`Total Phosphorus (mg/L)`)) %>%
      ungroup() %>%
      group_by(Year) %>%
      summarise(samplesPerYear = n(),
                `Annual Median TP` = median(medianPHOSPHORUS_mg_L, na.rm = TRUE),
                `Total Phosphorus (mg/L)` = unique(`Total Phosphorus (mg/L)`)) %>%
      mutate(`Annual Median TP Rounded to WQS Format` = signif(`Annual Median TP`, digits = 1),  # one significant figures based on WQS https://lis.virginia.gov/cgi-bin/legp604.exe?000+reg+9VAC25-260-187&000+reg+9VAC25-260-187
             TP_Exceedance = ifelse(`Annual Median TP Rounded to WQS Format` > `Total Phosphorus (mg/L)`, T, F),
             ID305B = unique(x$ID305B_1))  %>%
      dplyr::select(ID305B, Year, samplesPerYear, `Annual Median TP`, `Annual Median TP Rounded to WQS Format`, everything())
  } else {
    TPResults <- TP %>%
      group_by(Year) %>%
      mutate(samplesPerYear = n(),
             `Annual Median TP` = median(PHOSPHORUS_mg_L, na.rm = TRUE),
             `Annual Median TP Rounded to WQS Format` = signif(`Annual Median TP`, digits = 1),  # one significant figures based on WQS https://lis.virginia.gov/cgi-bin/legp604.exe?000+reg+9VAC25-260-187&000+reg+9VAC25-260-187
             TP_Exceedance = ifelse(`Annual Median TP Rounded to WQS Format` > `Total Phosphorus (mg/L)`, T, F)) %>%
      dplyr::select(FDT_STA_ID, Year, samplesPerYear, `Annual Median TP`, `Annual Median TP Rounded to WQS Format`,`Total Phosphorus (mg/L)`, TP_Exceedance, LACUSTRINE) %>%
      distinct(Year, .keep_all=T)
  }
  return(TPResults)
}
#TP_analysis(stationData1)
#TP_analysis(AUData11)

TP_Assessment <- function(x){
  TP_Results <- TP_analysis(x) %>% ungroup()
  
  if(nrow(TP_Results) > 0){
    if(is.na(unique(TP_Results$`Total Phosphorus (mg/L)`))){ # bail out if nutrient standards didn't join properly
      return(tibble(NUT_TP_EXC= NA, NUT_TP_SAMP = NA,	NUT_TP_STAT = NA))}
    validYears <- filter(TP_Results, samplesPerYear >= 6) # need at least 6 samples per year
    mostRecent2years <- slice_max(validYears, Year, n = 2) # get most recent two years of results
    if(nrow(mostRecent2years) == 2){ 
      if(all(unique(mostRecent2years$TP_Exceedance) == FALSE)){ # no exceedances in last two years
        return(tibble(NUT_TP_EXC= 0, NUT_TP_SAMP = nrow(mostRecent2years),	NUT_TP_STAT = 'S') )
      } else { # at least one TP_Exceedance exists
        if(all(unique(mostRecent2years$TP_Exceedance) == TRUE)){ # both years exceed
          return(tibble(NUT_TP_EXC= nrow(mostRecent2years), NUT_TP_SAMP = nrow(mostRecent2years),	NUT_TP_STAT = 'IM'))
        } else { # run a tiebreak with third most recent year
          mostRecent3years <- slice_max(validYears, Year, n = 3)  # get most recent three years of results
          mostRecent3yearsExceed <- filter(mostRecent3years, TP_Exceedance == TRUE)
          if(nrow(mostRecent3yearsExceed) >= 2){
            return(tibble(NUT_TP_EXC= nrow(mostRecent3yearsExceed), NUT_TP_SAMP = nrow(mostRecent3years),	NUT_TP_STAT = 'IM'))
          } else {
            return(tibble(NUT_TP_EXC= nrow(mostRecent3yearsExceed), NUT_TP_SAMP = nrow(mostRecent3years),	NUT_TP_STAT = 'Review')) }
        }}} else {return(tibble(NUT_TP_EXC= NA, NUT_TP_SAMP = nrow(validYears),	NUT_TP_STAT = 'IN') ) }
  } else {    return(tibble(NUT_TP_EXC= NA, NUT_TP_SAMP = NA,	NUT_TP_STAT = NA) )  }
}

#TP_Assessment(stationData1)
#TP_Assessment(AUData11)



### TSI equations
TSIcalculation <- function(stationData){
  if(unique(stationData$lakeStation) == TRUE){
    if(is.na(unique(stationData$Lakes_187B))){
      # first fill down secchi depth in case it isn't stored exactly at 0.3 meter
      secchiFix <- stationData %>%
        group_by(FDT_STA_ID, FDT_DATE_TIME) %>% 
        fill(SECCHI_DEPTH_M, .direction = "downup") %>%
        filter(FDT_DEPTH <= 0.3) %>%
        # remove all data except those from mid June through mid September (going with June 15 and Sept 15 since guidance does not specify)
        mutate(monthday = as.numeric(paste0(month(FDT_DATE_TIME), day(FDT_DATE_TIME)))) %>%
        filter(between(monthday, 615, 915 )) %>%
        dplyr::select(-monthday)
      
      # Calculate Secchi depth TSI
      SDdata <- filter(secchiFix, !is.na(SECCHI_DEPTH_M) ) %>%
          filter(!( LEVEL_SECCHI_DEPTH %in% c('Level II', 'Level I'))) %>% # get lower levels out
          dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH,SECCHI_DEPTH_M) %>%
        mutate(TSI_SD = 10*(6 - (log(SECCHI_DEPTH_M) / log(2))) )
      SD <- suppressWarnings(suppressMessages(
        SDdata %>%
          group_by(FDT_STA_ID) %>%
          summarise(meanSD = mean(SECCHI_DEPTH_M, na.rm = T), # take average of all secchi depths first
                    TSI_SD = 10*(6 - (log(meanSD) / log(2))) ) ))# log() is natural log in R
      
      # Calculate Chlorophyll a TSI
      chlaData <- filter(secchiFix, !is.na(CHLOROPHYLL_A_ug_L) ) %>%
          filter(!( LEVEL_CHLOROPHYLL_A %in% c('Level II', 'Level I'))) %>% # get lower levels out
          dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, CHLOROPHYLL_A_ug_L) %>%
        mutate(TSI_chla = 10*(6 - (2.04 - 0.68 * (log( CHLOROPHYLL_A_ug_L))) / log(2))) 
      chla <- suppressWarnings(suppressMessages(
        chlaData %>% 
          group_by(FDT_STA_ID) %>%
          summarise(meanchla = mean(CHLOROPHYLL_A_ug_L, na.rm = T), # take average of all chl a first
                    TSI_chla = 10*(6 - (2.04 - 0.68 * (log(meanchla))) / log(2)))  ))# log() is natural log in R
      
      # Calculate Total Phosphorus TSI
      TPdata <- filter(secchiFix, !is.na(PHOSPHORUS_mg_L) ) %>%
          filter(!( LEVEL_PHOSPHORUS %in% c('Level II', 'Level I'))) %>% # get lower levels out
          dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, PHOSPHORUS_mg_L) %>%
          mutate(PHOSPHORUS_ug_L = PHOSPHORUS_mg_L * 1000,# first convert mg/L to ug/L
                 TSI_TP = 10*(6 - (log( (48 / PHOSPHORUS_ug_L )) / log(2))) )   
      TP <- suppressWarnings(suppressMessages(
        TPdata %>%
          group_by(FDT_STA_ID) %>%
          summarise(meanTP = mean(PHOSPHORUS_ug_L, na.rm = T), # take average of all TP first
                    TSI_TP = 10*(6 - (log( (48 / meanTP)) / log(2))) ) ))# log() is natural log in R
      
      TSI <- full_join(SD, chla, by = c('FDT_STA_ID')) %>%
        full_join(TP, by = c('FDT_STA_ID')) %>%
        bind_cols(tibble(associatedData = list(full_join(SDdata, chlaData, by = c('FDT_STA_ID', 'FDT_DATE_TIME', 'FDT_DEPTH')) %>%
                                                 full_join(TPdata, by = c('FDT_STA_ID', 'FDT_DATE_TIME', 'FDT_DEPTH'))) ))
      
      return(TSI)
      
    } else {return(NULL)} #tibble(FDT_STA_ID = NA, meanSD = NA, TSI_SD = NA, meanchla = NA, TSI_chla = NA, meanTP = NA, TSI_TP = NA, associatedData = list(NA)))}
  } else {return(NULL)} #return(tibble(FDT_STA_ID = NA, meanSD = NA, TSI_SD = NA, meanchla = NA, TSI_chla = NA, meanTP = NA, TSI_TP = NA, associatedData = list(NA)))} 
}

#TSIcalculation(stationData)

#x <- stationData1 %>%
#  bind_rows(mutate(stationData1, FDT_STA_ID = 'FAKE'))

TSIassessment <- function(x){
  TSI <- TSIcalculation(x)
  
  if(nrow(TSI) > 0){
    if(nrow(TSI) > 1){ # if more than one station in AU, first average TSI results then assess
      return(
        TSI %>% # using mean to be consistent with mean in TSIcalculation()
          summarise(TSI_SD = as.numeric(signif(mean(TSI_SD, na.rm = T),digits = 2)), 
                    TSI_chla = as.numeric(signif(mean(TSI_chla, na.rm = T), digits = 2)),
                    TSI_TP = as.numeric(signif(mean(TSI_TP, na.rm = T), digits = 2))) %>%
          mutate(ID305B = unique(x$ID305B_1)) %>%
          dplyr::select(ID305B, everything()))
    } else {
      return(TSI %>%
               mutate(ID305B = unique(x$ID305B_1),
                      TSI_SD = as.numeric(signif(TSI_SD, digits = 2)),
                      TSI_chla = as.numeric(signif(TSI_chla, digits = 2)), 
                      TSI_TP = as.numeric(signif(TSI_TP, digits = 2))) %>%
               dplyr::select(ID305B,TSI_SD, TSI_chla, TSI_TP))}
  } else {return(tibble(ID305B = NA, TSI_SD = NA, TSI_chla = NA, TSI_TP = NA))}
}
#TSIassessment(x)




# Metals exceedances

metalsExceedances <- function(x, metalType){
  # if any data given to function
  if(nrow(x) > 0){ EXC <- length(which(x == 'NSP' | x == 'OE')) 
  }else { EXC <- NA  }
  
  x <- tibble(EXC = EXC, STAT = ifelse(EXC > 0, 'Review', 'S'))
  names(x) <- paste(metalType,names(x), sep='_')
  return(x)
}


# Benthic Data flag
benthicAssessment <- function(stationData,
                              VSCIresults # pinned dataset with all BenSamps run against just VSCI
                              ){
  # this works because the all SCI options are run on all data, so if there is a VSCI result 
  # (even if in real life that is not the correct SCI to use), then benthic data exists for
  # a given station
  benthicDataExist <- filter(VSCIresults, StationID %in% unique(stationData$FDT_STA_ID))
  if(nrow(benthicDataExist) > 0){tibble(BENTHIC_STAT = 'Review')
  } else { tibble(BENTHIC_STAT = NA)}
}
#benthicAssessment(stationData, VSCIresults)



#### Ammonia Assessment Functions ---------------------------------------------------------------------------------------------------

# Used rolling windows but opted for loops with filtering instead of roll_apply over time series so the data analyzed each window could
#  cascade outside the function and be unpacked by further analyses/visualizations if necessary


# Calculate limits and return dataframe with original data and limits 9VAC25-260-155 https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section155/
freshwaterNH3limit <- function(stationData, # dataframe with station data
                               trout, # T/F condition
                               mussels,# T/F condition
                               earlyLife# T/F condition
){
  # If no data, return nothing
  if(nrow(stationData)==0){return(NULL)}
  
  # remove any data that shouldn't be considered
  stationDataAmmonia <- filter(stationData, !(LEVEL_FDT_TEMP_CELCIUS %in% c('Level II', 'Level I')) |
                                 !(LEVEL_FDT_FIELD_PH %in% c('Level II', 'Level I'))) %>% # get lower levels out
    # # lake stations should only be surface sample
    # {if(unique(stationData$lakeStation) == TRUE)
      filter(., FDT_DEPTH <= 1) %>%  # all samples should be < 1m (this allows for 0.3m surface samples and 0m integrated samples)
      # else . } %>%
    filter(!is.na(AMMONIA_mg_L)) %>% #get rid of NA's
    dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_TEMP_CELCIUS, FDT_FIELD_PH, AMMONIA_mg_L) %>% 
    # can only run analysis if all above variable are populated for a given date/time
    filter(!is.na(FDT_TEMP_CELCIUS) & !is.na(FDT_FIELD_PH))
  # If no data, return nothing
  if(nrow(stationDataAmmonia)==0){return(NULL)}
  
  
  # make a place to store analysis results
  acuteCriteriaResults <- tibble(FDT_STA_ID = as.character(NA), WindowDateTimeStart = as.POSIXct(NA), 
                                 FDT_DEPTH = as.character(NA),# character so we can concatenate multiple depth values if needed
                                 Value = as.numeric(NA), ValueType = as.character(NA), 
                                 `Criteria Type` = as.character(NA), CriteriaValue = as.numeric(NA), 
                                 `Sample Count` = as.numeric(NA), 
                                 parameterRound = as.numeric(NA), Exceedance = as.numeric(NA),
                                 associatedData = list())
  chronicCriteriaResults <- acuteCriteriaResults
  fourDayCriteriaResults <- acuteCriteriaResults
  
  # loop through each row of data to correctly calculate acute criteria and find any chronic scenarios (and then 
  #   run a 4 day analysis if data exist)
  for(k in stationDataAmmonia$FDT_DATE_TIME){  #k = stationDataAmmonia$FDT_DATE_TIME[2]
    acuteDataWindow <- filter(stationDataAmmonia,  between(FDT_DATE_TIME, k, k + hours(1)))
    chronicDataWindow <- filter(stationDataAmmonia,  between(FDT_DATE_TIME, k, k + days(30)))
    fourDayDataWindow <- filter(stationDataAmmonia,  between(FDT_DATE_TIME, k, k + days(4)))
    
    
    # Run acute analysis if data exists
    # Acute is calculated on 1 hour windows, so we need to average temperature and pH within each 1 hour window before we can calculate an
    #  acute criteria. There are no rules on how many samples need to occur in a 1 hour window for the window to be valid, but chronic and
    #  4 day criteria require > 1 sample to be analyzed.
    
    # Acute criteria can combine multiple depths in calculation of criteria, <1m difference (which is taken care of above)
    
    if(nrow(acuteDataWindow) > 0){
      acuteDataCriteriaAnalysis <- suppressMessages( 
        acuteDataWindow %>% 
          #  group_by(FDT_STA_ID) %>% # for some reason if multiple depths but 1 stationID get 2 rows when group_by(FDT_STA_ID)
          summarise(FDT_STA_ID = paste0(unique(FDT_STA_ID), collapse = ' '), ### this is the ugly fix to problem identified above
                    FDT_DEPTH = paste0(sort(unique(FDT_DEPTH)), collapse = ' | '),
                    TempValue = mean(FDT_TEMP_CELCIUS, na.rm=T),  # get hourly average; don't round to even bc more calculations to follow with data
                    pHValue = mean(FDT_FIELD_PH, na.rm=T),  # get hourly average; don't round to even bc more calculations to follow with data
                    Value = mean(AMMONIA_mg_L, na.rm=T),  # get hourly average; don't round to even bc more calculations to follow with data
                    `Sample Count` = length(AMMONIA_mg_L)) %>%  #count sample that made up average
          mutate(ValueType = 'Hourly Average',
                 ID = paste( FDT_STA_ID, FDT_DEPTH, sep = '_'), # make a uniqueID in case >1 sample for given datetime
                 `Criteria Type` = 'Acute') %>% 
          {if(trout == TRUE & mussels == TRUE)  # Trout & mussels present scenario
            mutate(., CriteriaValue = as.numeric(signif(
              min(((0.275 / (1 + 10^(7.204 - pHValue))) + (39.0 / (1 + 10^(pHValue - 7.204)))),
                  (0.7249 * ( (0.0114 / (1 + 10^(7.204 - pHValue))) + (1.6181 / (1 + 10^(pHValue - 7.204)))) * (23.12 * 10^(0.036 * (20 - TempValue))) )), digits = 2)))
            else . } %>%
          {if(trout == TRUE & mussels == FALSE) # Trout present & mussels absent scenario
            mutate(., CriteriaValue = as.numeric(signif(
              min(((0.275 / (1 + 10^(7.204 - pHValue))) + (39.0 / (1 + 10^(pHValue - 7.204)))),
                  (0.7249 * ( (0.0114 / (1 + 10^(7.204 - pHValue))) + (1.6181 / (1 + 10^(pHValue - 7.204)))) * (62.15 * 10^(0.036 * (20 - TempValue))) )), digits = 2)))
            else . } %>%
          {if(trout == FALSE & mussels == TRUE) # Trout absent & mussels present scenario
            mutate(., CriteriaValue = as.numeric(signif(
              0.7249 * ((0.0114 / (1 + 10^(7.204 - pHValue))) + (1.6181 / (1 + 10^(pHValue - 7.204)))) * min(51.93, (23.12 * 10^(0.036 * (20 - TempValue)))), digits = 2)))
            else .} %>% 
          {if(trout == FALSE & mussels == FALSE) # Trout & mussels absent scenario
            mutate(., CriteriaValue = as.numeric(signif(
              0.7249 * ((0.0114 / (1 + 10^(7.204 - pHValue))) + (1.6181 / (1 + 10^(pHValue - 7.204)))) * min(51.93, (62.15 * 10^(0.036 * (20 - TempValue)))), digits = 2)))
            else .} %>% 
          mutate(parameterRound = signif(Value, digits = 2), # two significant figures based on 9VAC25-260-155 https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section155/
                 Exceedance = ifelse(parameterRound > CriteriaValue, 1, 0 ), # use 1/0 to easily summarize multiple results later
                 WindowDateTimeStart = min(acuteDataWindow$FDT_DATE_TIME)) %>% 
          dplyr::select(FDT_STA_ID, WindowDateTimeStart, everything(), -ID) ) 
      # now save data for later, in a format that matches with desired output
      acuteDataCriteriaAnalysis <- acuteDataCriteriaAnalysis %>% 
        dplyr::select(-c(TempValue, pHValue)) %>% 
        bind_cols(tibble(associatedData = list(left_join(acuteDataWindow,
                                                         dplyr::select(acuteDataCriteriaAnalysis, FDT_STA_ID,  WindowDateTimeStart, TempValue, pHValue, Value),
                                                         by = c('FDT_STA_ID', 'FDT_DATE_TIME' = 'WindowDateTimeStart')))) )
      
      # Save the results for viewing later
      acuteCriteriaResults <- bind_rows(acuteCriteriaResults, acuteDataCriteriaAnalysis) 
      
    } else {acuteCriteriaResults <- acuteCriteriaResults }
    

    # Run chronic analysis if enough data exists, must have > 1 sample in chronic window to run analysis
    # Chronic is calculated on 30 day windows, so we need to average temperature and pH within each 30 day window before we can calculate a
    #  chronic criteria. The chronic criteria will be associated with each sample date that starts a 30 day period, but it applies to all
    #  samples within the 30 day window. All raw data associated with each window is saved as a listcolumn for later review.
    
    # Chronic criteria can combine multiple depths in calculation of criteria, <1m difference (which is taken care of above)
    
    if(nrow(chronicDataWindow) > 1){ # need 2 or more data points to run a chronic
      chronicDataCriteriaAnalysis <- suppressMessages(
        chronicDataWindow %>% 
        #  group_by(FDT_STA_ID) %>% # for some reason if multiple depths but 1 stationID get 2 rows when group_by(FDT_STA_ID)
          summarise(FDT_STA_ID = paste0(unique(FDT_STA_ID), collapse = ' '), ### this is the ugly fix to problem identified above
                    FDT_DEPTH = paste0(sort(unique(FDT_DEPTH)), collapse = ' | '),
                    TempValue = mean(FDT_TEMP_CELCIUS, na.rm = T), # get 30 day average; don't round to even bc more calculations to follow with data
                    pHValue = mean(FDT_FIELD_PH, na.rm = T), # get 30 day average; don't round to even bc more calculations to follow with data
                    Value = mean(AMMONIA_mg_L, na.rm = T), # get 30 day average; don't round to even bc more calculations to follow with data
                    `Sample Count` = length(AMMONIA_mg_L)) %>%  #count sample that made up average
          mutate(ValueType = '30 Day Average',
                 ID = paste( FDT_STA_ID, FDT_DEPTH, sep = '_'), # make a uniqueID in case >1 sample for given datetime
                 `Criteria Type` = 'Chronic') %>%
        # Trout & mussels present scenario
        {if(trout == TRUE & mussels == TRUE & earlyLife == TRUE)  # Trout & mussels present scenario & earlyLife == TRUE
          mutate(., CriteriaValue = as.numeric(signif(
            0.8876 * ((0.0278 / (1 + 10^(7.688 - pHValue))) + (1.1994 / (1 + 10^(pHValue - 7.688)))) * (2.126 * 10^(0.028 * (20 - max(7, TempValue)))), digits = 2)))
          else .} %>%
        {if(trout == TRUE & mussels == TRUE & earlyLife == FALSE)  # Trout & mussels present scenario & earlyLife == FALSE
          mutate(., CriteriaValue = as.numeric(NA))
          else .} %>%

        # Trout present & mussels absent scenario
        {if(trout == TRUE & mussels == FALSE & earlyLife == TRUE)  # Trout present & mussels absent scenario & earlyLife == TRUE
          mutate(., CriteriaValue = as.numeric(signif(
            0.9405 * ((0.0278 / (1 + 10^(7.688 - pHValue))) + (1.1994 / (1 + 10^(pHValue - 7.688)))) * min(6.92, (7.547 * 10^(0.028 * (20 - TempValue)))), digits = 2)))
          else .} %>%
        {if(trout == TRUE & mussels == FALSE & earlyLife == FALSE)  # Trout present & mussels absent scenario & earlyLife == FALSE
          mutate(., CriteriaValue = as.numeric(signif(
            0.9405 * ((0.0278 / (1 + 10^(7.688 - pHValue))) + (1.1994 / (1 + 10^(pHValue - 7.688)))) * (7.547 * 10^(0.028 * (20 - max(TempValue, 7)))), digits = 2)))
          else .} %>%

        # Trout absent & mussels present scenario
        {if(trout == FALSE & mussels == TRUE & earlyLife == TRUE)  # Trout absent & mussels present scenario & earlyLife == TRUE
          mutate(., CriteriaValue = as.numeric(signif(
            0.8876 * ((0.0278 / (1 + 10^(7.688 - pHValue))) + (1.1994 / (1 + 10^(pHValue - 7.688)))) * (2.126 * 10^(0.028 * (20 - max(7, TempValue)))), digits = 2)))
          else .} %>%
        {if(trout == FALSE & mussels == TRUE & earlyLife == FALSE)  # Trout absent & mussels present scenario & earlyLife == FALSE
          mutate(., CriteriaValue = as.numeric(NA))
          else .} %>%

         # Trout & mussels absent scenario
        {if(trout == FALSE & mussels == FALSE & earlyLife == TRUE)  # Trout & mussels absent scenario & earlyLife == TRUE
          mutate(., CriteriaValue = as.numeric(signif(
            0.9405 * ((0.0278 / (1 + 10^(7.688 - pHValue))) + (1.1994 / (1 + 10^(pHValue - 7.688)))) * min(6.92, (7.547 * 10^(0.028 * (20 - TempValue)))), digits = 2)))
          else .} %>%
        {if(trout == FALSE & mussels == FALSE & earlyLife == FALSE)  # Trout & mussels absent scenario & earlyLife == FALSE
          mutate(., CriteriaValue = as.numeric(signif(
            0.9405 * ((0.0278 / (1 + 10^(7.688 - pHValue))) + (1.1994 / (1 + 10^(pHValue - 7.688)))) * (7.547 * 10^(0.028 * (20 - max(TempValue, 7)))), digits = 2)))
          else .} %>%

        mutate(parameterRound = signif(Value, digits = 2), # two significant figures based on 9VAC25-260-155 https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section155/
               Exceedance = ifelse(parameterRound > CriteriaValue, 1, 0 ), # use 1/0 to easily summarize multiple results later
               WindowDateTimeStart = min(acuteDataWindow$FDT_DATE_TIME)) %>%
        dplyr::select(FDT_STA_ID, WindowDateTimeStart, everything(), -ID) ) 
      # now save data for later, in a format that matches with desired output
      chronicDataCriteriaAnalysis <- chronicDataCriteriaAnalysis %>% 
        dplyr::select(-c(TempValue, pHValue)) %>% 
        bind_cols(tibble(associatedData = list(left_join(chronicDataWindow,
                                                         dplyr::select(chronicDataCriteriaAnalysis, FDT_STA_ID,  WindowDateTimeStart, TempValue, pHValue, Value),
                                                         by = c('FDT_STA_ID', 'FDT_DATE_TIME' = 'WindowDateTimeStart')))) )

      # Save the results for viewing later
      chronicCriteriaResults <- bind_rows(chronicCriteriaResults, chronicDataCriteriaAnalysis)
    } else {chronicCriteriaResults <- chronicCriteriaResults }
    
    
    # Run 4 day analysis if data exists
    # 4 day analysis is run on 4 day windows, so we need to average temperature and pH within each 4 day window before we can compare to a 2.5x 
    #  chronic criteria. The chronic criteria associated with each sample date that starts a 4 day period is used for the 4 day analysis. 
    #  All raw data associated with each window is saved as a listcolumn for later review.
    
    # 4day criteria can combine multiple depths in calculation of criteria, <1m difference (which is taken care of above)
    
    if(nrow(fourDayDataWindow) > 1){
      fourDayDataCriteriaAnalysis <- suppressMessages( 
        fourDayDataWindow %>% 
          #  group_by(FDT_STA_ID) %>% # for some reason if multiple depths but 1 stationID get 2 rows when group_by(FDT_STA_ID)
          summarise(FDT_STA_ID = paste0(unique(FDT_STA_ID), collapse = ' '), ### this is the ugly fix to problem identified above
                    FDT_DEPTH = paste0(sort(unique(FDT_DEPTH)), collapse = ' | '),
                    TempValue = mean(FDT_TEMP_CELCIUS, na.rm=T),  # get 4day average; doesn't do anything bc this doesn't go into criteria calculation but needed to match other data format
                    pHValue = mean(FDT_FIELD_PH, na.rm=T),  # get 4day average; doesn't do anything bc this doesn't go into criteria calculation but needed to match other data format
                    Value = mean(AMMONIA_mg_L, na.rm=T),  # get 4day average; don't round to even bc want to see raw and parameterRound columns in output
                    `Sample Count` = length(AMMONIA_mg_L)) %>%  #count sample that made up average
          mutate(ValueType = 'Four Day Average',
                 ID = paste( FDT_STA_ID, FDT_DEPTH, sep = '_'), # make a uniqueID in case >1 sample for given datetime
                 `Criteria Type` = 'Four Day') %>% 
          left_join(dplyr::select(chronicDataCriteriaAnalysis, FDT_STA_ID, `Chronic CriteriaValue` = CriteriaValue),
                    by = c('FDT_STA_ID')) %>% 
          mutate(CriteriaValue = as.numeric(signif(`Chronic CriteriaValue` * 2.5, digits = 2)), # 4 day criteria = 2.5x chronic criteria
                 parameterRound = signif(Value, digits = 2), # two significant figures based on 9VAC25-260-155 https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section155/
                 Exceedance = ifelse(parameterRound > CriteriaValue, 1, 0 ), # use 1/0 to easily summarize multiple results later
                 WindowDateTimeStart = min(fourDayDataWindow$FDT_DATE_TIME)) %>% 
          dplyr::select(FDT_STA_ID, WindowDateTimeStart, everything(), -c(ID, `Chronic CriteriaValue` ) ) )
      # now save data for later, in a format that matches with desired output
      fourDayDataCriteriaAnalysis <- fourDayDataCriteriaAnalysis %>% 
        dplyr::select(-c(TempValue, pHValue)) %>% 
        bind_cols(tibble(associatedData = list(left_join(fourDayDataWindow,
                                                         dplyr::select(fourDayDataCriteriaAnalysis, FDT_STA_ID,  WindowDateTimeStart, TempValue, pHValue, Value),
                                                         by = c('FDT_STA_ID', 'FDT_DATE_TIME' = 'WindowDateTimeStart')))) )
      
      # Save the results for viewing later
      fourDayCriteriaResults <- bind_rows(fourDayCriteriaResults, fourDayDataCriteriaAnalysis) 
      
    } else {fourDayCriteriaResults <- fourDayCriteriaResults }
    
          

    combinedResults <- bind_rows(acuteCriteriaResults, chronicCriteriaResults) %>%
      {if(nrow(fourDayCriteriaResults) > 0)
        bind_rows(., fourDayCriteriaResults)
        else .} %>% 
      arrange(WindowDateTimeStart, `Criteria Type`)
        
    }
  
      
      return( combinedResults)#bind_rows(acuteDataCriteriaAnalysis, chronicDataCriteriaAnalysis))
}
#ammoniaAnalysisStation <- freshwaterNH3limit(stationData, trout = TRUE, mussels = TRUE, earlyLife = TRUE)
# fake data for testing, station == "1AACO014.57"
#stationData$AMMONIA_mg_L[2:12] <- rep(25, 11)
#ammoniaAnalysisStation <- freshwaterNH3limit(stationData, trout = TRUE, mussels = TRUE, earlyLife = TRUE)


# old function that needs to be deleted
## Identify if metals data exists
metalsData <- function(stationData, metalType){
  if(nrow(stationData) > 0){
    dataOut <- tibble(`_EXC` = NA, `_STAT` = 'Review')
  } else {
    dataOut <- tibble(`_EXC` = NA, `_STAT` = NA)}
  names(dataOut) <- paste0(metalType, names(dataOut))
  return(dataOut)}

# Metals criteria analysis
metalsCriteriaFunction <- function(ID, Hardness, WER){
  # Remember: ln is really log() in R; exp() is natural antilog in R
  # Per 9VAC25-260-140, criteria to 2 sig figs #https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
  
  # If no WER supplied, use 1
  WER <- ifelse(is.na(WER), 1, WER)
  # Establish Hardness Criteria
  criteriaHardness <- ifelse(Hardness < 25, 25, ifelse(Hardness > 400, 400, Hardness))
  
  metalsCriteria <- suppressWarnings(
    tibble(ID= ID, `AntimonyDissolved PWS` = 5.6, `AntimonyDissolved All Other Surface Waters` = 640,
           `ArsenicDissolved Acute Freshwater` = 340, `ArsenicDissolved Chronic Freshwater` = 150, `ArsenicDissolved PWS` = 10,
           `ArsenicDissolved Acute Saltwater` = 69, `ArsenicDissolved Chronic Saltwater` = 36,
           `BariumDissolved PWS` = 2000,
           `CadmiumDissolved Acute Freshwater` =  signif(WER * exp(0.9789 * (log(criteriaHardness))-3.866) * (1.136672 - (log(criteriaHardness) * 0.041838)), digits = 2),
           `CadmiumDissolved Chronic Freshwater` = signif(WER * exp(0.7977 * log(criteriaHardness) - 3.909) * (1.101672 - (log(criteriaHardness) * (0.041838))), digits = 2),
           `CadmiumDissolved Acute Saltwater` = signif(33 * WER, digits = 2), `CadmiumDissolved Chronic Saltwater` = signif(7.9 * WER, digits = 2), `CadmiumDissolved PWS` = 5,
           `ChromiumIIIDissolved Acute Freshwater` = signif(WER *  (exp(0.8190 * (log(criteriaHardness)) + 3.7256)) * 0.316, digits = 2), 
           `ChromiumIIIDissolved Chronic Freshwater` = signif(WER *  (exp(0.8190 * (log(criteriaHardness))  +0.6848)) * 0.860, digits = 2), `ChromiumIIIDissolved PWS` = 100,
           `ChromiumVIDissolved Acute Freshwater` = 16, `ChromiumVIDissolved Chronic Freshwater` = 11, `ChromiumVIDissolved Acute Saltwater` = 1100, `ChromiumVIDissolved Chronic Saltwater` = 50, 
           `CopperDissolved Acute Freshwater` =  signif(WER * (exp(0.9422 * log(criteriaHardness) - 1.700)) * 0.960, digits = 2),
           `CopperDissolved Chronic Freshwater` = signif(WER * (exp(0.8545 * log(criteriaHardness) - 1.702)) * 0.960, digits = 2),
           `CopperDissolved Acute Saltwater` =  signif(9.3 * WER, digits = 2), `CopperDissolved Chronic Saltwater` =  signif(6.0 * WER, digits = 2), `CopperDissolved PWS` = 1300,
           `IronDissolved PWS` = 300, `IronTotal PWS` = 300, # new for IR2024 when iron data pulled in conventionals
           `LeadDissolved Acute Freshwater` = signif(WER * (exp(1.273 * log(criteriaHardness) - 1.084)) * (1.46203 - (log(criteriaHardness) * 0.145712)), digits = 2),
           `LeadDissolved Chronic Freshwater` = signif(WER * (exp(1.273 * log(criteriaHardness) - 3.259)) * (1.46203 - (log(criteriaHardness) * 0.145712)), digits = 2),
           `LeadDissolved Acute Saltwater` = signif(230 * WER, digits = 2), `LeadDissolved Chronic Saltwater` = signif(8.8 * WER, digits = 2), `LeadDissolved PWS` = 15,
           `Mercury Acute Freshwater` = 1.4, `Mercury Chronic Freshwater` = 0.77, `Mercury Acute Saltwater` = 1.8, `Mercury Chronic Saltwater` = 0.94,
           `NickelDissolved Acute Freshwater` = signif(WER * (exp (0.8460 * log(criteriaHardness) + 1.312)) * 0.998, digits = 2), 
           `NickelDissolved Chronic Freshwater` = signif(WER * (exp(0.8460 * log(criteriaHardness) - 0.8840)) * 0.997, digits = 2),
           `NickelDissolved Acute Saltwater` = signif(74 * WER, digits = 2), `NickelDissolved Chronic Saltwater` = signif(8.2 * WER, digits = 2), 
           `NickelDissolved PWS` = 610,  `NickelDissolved All Other Surface Waters` = 4600,
           `UraniumTotal PWS` = 30,
           `SeleniumDissolved Acute Freshwater` = 20, `SeleniumDissolved Chronic Freshwater` = 5.0, 
           `SeleniumDissolved Acute Saltwater` = signif(290 * WER, digits = 2), `SeleniumDissolved Chronic Saltwater` = signif(71 * WER, digits = 2),
           `SeleniumDissolved PWS` = 170, `SeleniumDissolved All Other Surface Waters` = 4200,
           `SilverDissolved Acute Freshwater` = signif(WER * (exp(1.72 * log(criteriaHardness) - 6.52)) * 0.85, digits = 2), `SilverDissolved Acute Saltwater` = signif(1.9 * WER, digits = 2),
           `ThalliumDissolved PWS` = 0.24, `ThalliumDissolved All Other Surface Waters` = 0.47,
           `ZincDissolved Acute Freshwater` = signif(WER * (exp(0.8473 * log(criteriaHardness) + 0.884)) * 0.978, digits = 2),
           `ZincDissolved Chronic Freshwater` = signif(WER * (exp(0.8473 * log(criteriaHardness) + 0.884)) * 0.986, digits = 2),
           `ZincDissolved Acute Saltwater` = signif(90 * WER, digits = 2), `ZincDissolved Chronic Saltwater` = signif(81 * WER, digits = 2), 
           `ZincDissolved PWS` = 7400, `ZincDissolved All Other Surface Waters` = 26000) %>% 
      pivot_longer(!ID, names_to = 'Criteria', values_to = 'CriteriaValue') %>% 
      mutate(Criteria2 = Criteria) %>%  #duplicate column to split
      separate(Criteria2, c("Metal", "Criteria Type", "Waterbody"), sep = " ") %>% 
      mutate(`Criteria Type` = ifelse(`Criteria Type` == 'All', 'All Other Waters', `Criteria Type`),
             Waterbody = ifelse(Waterbody == 'Other', NA, Waterbody)) %>% 
      dplyr::select(ID, Metal, Criteria, `Criteria Type`, Waterbody, CriteriaValue))
  return(metalsCriteria)
}

# Pre update to add to rolling aquatic life use method
# metalsAnalysis <- function(stationMetalsData, stationData, WER){
#   # If no WER supplied, use 1
#   WER <- ifelse(is.na(WER), 1, WER)
#   
#   # Get WQS from stationData so correct criteria can be applied
#   stationMetalsData <- left_join(stationMetalsData, 
#                                  dplyr::select(stationData, FDT_STA_ID, CLASS, PWS, ZONE) %>% 
#                                    distinct(FDT_STA_ID, .keep_all = TRUE), by = c('Station_Id' = 'FDT_STA_ID')) %>% 
#     mutate(`Assess As` = case_when(CLASS == "I" ~ 'Saltwater',
#                                    CLASS == "II" & ZONE == 'Estuarine' ~ 'Saltwater',
#                                    CLASS == "II" & ZONE == 'Transition' ~ 'More Stringent',
#                                    CLASS == "II" & ZONE == 'Tidal Fresh' ~ 'Freshwater',
#                                    CLASS %in% c('III', "IV","V","VI","VII") ~ 'Freshwater',
#                                    TRUE ~ as.character(NA)),
#            ChromiumIII= Chromium, RMK_ChromiumIII = RMK_Chromium, 
#            ChromiumVI= Chromium, RMK_ChromiumVI = RMK_Chromium ) %>% # add individual Chromium variables to make joining to assessment criteria easier
#     # Roger uses ChromiumIII and VI to flag any potential chromium issues, likely further lab analyses needed if either chromium criteria blown
#     dplyr::select(Station_Id:RMK_Cadmium, ChromiumIII:RMK_ChromiumVI, Cadmium:`Assess As`)
#   
#   # make a place to store raw analysis results
#   rawCriteriaResults <- tibble(Station_Id = as.character(NA), WindowDateTimeStart = as.POSIXct(NA), FDT_DEPTH = as.numeric(NA),
#                                CLASS = as.factor(NA), PWS = as.factor(NA), ZONE = as.factor(NA), `Assess As` = as.character(NA),
#                                Metal = as.character(NA), ValueType = as.character(NA), Value = as.numeric(NA), Criteria = as.character(NA), 
#                                `Criteria Type` = as.character(NA), Waterbody = as.character(NA), CriteriaValue = as.numeric(NA),
#                                parameterRound = as.numeric(NA), Exceedance = as.numeric(NA))
#   acuteCriteriaResults <- rawCriteriaResults 
#   chronicCriteriaResults <- acuteCriteriaResults 
#   
#   # loop through each row of data to correctly calculate criteria and find any chronic scenarios
#   for(k in stationMetalsData$FDT_DATE_TIME){
#     rawDataWindow <- filter(stationMetalsData, FDT_DATE_TIME == k)
#     acuteDataWindow <- filter(stationMetalsData,  between(FDT_DATE_TIME, k, k + hours(1)))
#     chronicDataWindow <- filter(stationMetalsData,  between(FDT_DATE_TIME, k, k + days(4)))
#     # Run any analyses requiring raw data if data exists
#     if(nrow(rawDataWindow) > 0){
#       rawData <- rawDataWindow %>% 
#         group_by(Station_Id, FDT_DATE_TIME, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`) %>% 
#         dplyr::select(-c(contains('RMK_'))) %>% 
#         pivot_longer(cols = Antimony:Hardness, names_to = "Metal", values_to = "Value") %>% 
#         mutate(ValueType = 'Raw Result',
#                ID = paste(Station_Id, FDT_DATE_TIME, FDT_DEPTH, sep = '_')) %>% # make a uniqueID in case >1 sample for given datetime
#         ungroup()
#       # Calculate criteria based on raw data
#       rawDataCriteria <- metalsCriteriaFunction(filter(rawData, Metal == "Hardness")$ID, filter(rawData, Metal == "Hardness")$Value, WER = 1) %>% 
#         filter(`Criteria Type` %in% c('All Other Waters', 'PWS')) %>% # don't need other criteria for acute window
#         {if(is.na(unique(rawData$PWS)))
#           filter(., `Criteria Type` != 'PWS')
#           else .}
#       # Join appropriate criteria to rawData for comparison to averaged data
#       rawDataCriteriaAnalysis <- left_join(rawData, rawDataCriteria, by = c('ID', 'Metal')) %>% 
#         mutate(parameterRound = signif(Value, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
#                Exceedance = ifelse(parameterRound > CriteriaValue, 1, 0 ),
#                WindowDateTimeStart = min(rawDataWindow$FDT_DATE_TIME)) %>%  # use 1/0 to easily summarize multiple results later
#         filter(!is.na(Criteria)) %>%  # filter out metals that don't have chronic criteria
#         dplyr::select(Station_Id, WindowDateTimeStart, everything()) %>% 
#         dplyr::select(-c(FDT_DATE_TIME, ID))
#       # Save the results for viewing later
#       rawCriteriaResults <- bind_rows(rawCriteriaResults, rawDataCriteriaAnalysis) 
#     } else {rawCriteriaResults <- rawCriteriaResults }
#     # Run acute analysis if data exists
#     if(nrow(acuteDataWindow) > 0){
#       acuteData <- suppressMessages(acuteDataWindow %>% 
#         group_by(Station_Id, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`) %>% # can't group by datetime or summary can't happen
#         dplyr::select(-c(contains('RMK_'))) %>% 
#         pivot_longer(cols = Antimony:Hardness, names_to = "Metal", values_to = "CriteriaValue") %>% 
#         ungroup() %>% group_by(Station_Id, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`, Metal) %>% 
#         summarise(Value = mean(CriteriaValue, na.rm=T)) %>%  # get hourly average
#         mutate(ValueType = 'Hourly Average',
#                ID = paste(Station_Id, FDT_DEPTH, sep = '_')) ) # make a uniqueID in case >1 sample for given datetime
#       # Calculate criteria based on hourly averaged data
#       acuteDataCriteria <- metalsCriteriaFunction(filter(acuteData, Metal == "Hardness")$ID, filter(acuteData, Metal == "Hardness")$Value, WER = 1) %>% 
#         filter(`Criteria Type` == 'Acute') %>% # don't need other criteria for acute window
#         # Keep only the criteria needed 
#         {if(unique(acuteData$`Assess As`) %in% c('Freshwater', 'Saltwater'))
#           filter(., Waterbody %in% c(NA, !!unique(acuteData$`Assess As`)))
#           # if in Transition Zone then use the more stringent standard
#           else group_by(., Metal) %>% 
#             mutate(MoreStringent = min(CriteriaValue)) %>% 
#             filter(CriteriaValue == MoreStringent) %>% 
#             dplyr::select(-MoreStringent)} 
#       # Join appropriate criteria to acuteData for comparison to averaged data
#       acuteDataCriteriaAnalysis <- left_join(acuteData, acuteDataCriteria, by = c('ID', 'Metal')) %>% 
#         mutate(parameterRound = signif(Value, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
#                Exceedance = ifelse(parameterRound > CriteriaValue, 1, 0 ), # use 1/0 to easily summarize multiple results later
#                WindowDateTimeStart = min(acuteDataWindow$FDT_DATE_TIME)) %>% 
#         filter(!is.na(Criteria)) %>% # filter out metals that don't have chronic criteria
#         dplyr::select(Station_Id, WindowDateTimeStart, everything(), -ID)
#       # Save the results for viewing later
#       acuteCriteriaResults <- bind_rows(acuteCriteriaResults, acuteDataCriteriaAnalysis) 
#     } else {acuteCriteriaResults <- acuteCriteriaResults }
#     # Run chronic analysis if data exists
#     if(nrow(chronicDataWindow) > 0){
#       chronicData <- suppressMessages(chronicDataWindow %>% 
#         group_by(Station_Id, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`) %>% # can't group by datetime or summary can't happen
#         dplyr::select(-c(contains('RMK_'))) %>% 
#         pivot_longer(cols = Antimony:Hardness, names_to = "Metal", values_to = "CriteriaValue") %>% 
#         ungroup() %>% group_by(Station_Id, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`, Metal) %>% 
#         summarise(Value = mean(CriteriaValue, na.rm=T)) %>% # get four day average
#         mutate(ValueType = 'Four Day Average',
#                ID = paste(Station_Id, FDT_DEPTH, sep = '_')) ) # make a uniqueID in case >1 sample for given datetime
#       # Calculate criteria based on hourly averaged data
#       chronicDataCriteria <- metalsCriteriaFunction(filter(chronicData, Metal == "Hardness")$ID, filter(chronicData, Metal == "Hardness")$Value, WER = 1) %>% 
#         filter(`Criteria Type` == 'Chronic') %>% # don't need other criteria for chronic window analysis
#         # Keep only the criteria needed 
#         {if(unique(chronicData$`Assess As`) %in% c('Freshwater', 'Saltwater'))
#           filter(., Waterbody %in% c(NA, !!unique(chronicData$`Assess As`)))
#           # if in Transition Zone then use the more stringent standard
#           else group_by(., Metal) %>% 
#             mutate(MoreStringent = min(CriteriaValue)) %>% 
#             filter(CriteriaValue == MoreStringent) %>% 
#             dplyr::select(-MoreStringent)} 
#       # Join appropriate criteria to chronicData for comparison to averaged data
#       chronicDataCriteriaAnalysis <- left_join(chronicData, chronicDataCriteria, by = c('ID', 'Metal')) %>% 
#         mutate(parameterRound = signif(Value, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
#                Exceedance = ifelse(parameterRound > CriteriaValue, 1, 0 ), # use 1/0 to easily summarize multiple results later
#                WindowDateTimeStart = min(chronicDataWindow$FDT_DATE_TIME)) %>% 
#         filter(!is.na(Criteria)) %>% # filter out metals that don't have chronic criteria
#         dplyr::select(Station_Id, WindowDateTimeStart, everything(), -ID)
#       # Save the results for viewing later
#       chronicCriteriaResults <- bind_rows(chronicCriteriaResults, chronicDataCriteriaAnalysis) 
#     } else {chronicCriteriaResults <- chronicCriteriaResults }
#   }
#   stationCriteriaResults <- bind_rows(rawCriteriaResults, acuteCriteriaResults, chronicCriteriaResults) %>% 
#     filter(!is.na(Station_Id)) %>% # drop placeholder rows
#     distinct(Station_Id, WindowDateTimeStart, FDT_DEPTH, Criteria, .keep_all = T) %>% # remove duplicates in case > 1 depth per datetime
#     arrange(Station_Id, WindowDateTimeStart, FDT_DEPTH, Metal)
#   return(stationCriteriaResults)
# }

# Pre aquatic life update
# # Metals Assessment function that makes sense of output from metalsAnalysis()
# metalsAssessmentFunction <- function(metalsAnalysisResults){
#   #Check to make sure that metals data exist
#   if(nrow(metalsAnalysisResults) > 0){
#     
#     #  Organize results to include Staion_Id, Criteria, and the # of Exceedances
#     metalsExceedances <- metalsAnalysisResults %>%
#       group_by(Station_Id, Criteria) %>%
#       summarise(Exceedances = sum(Exceedance, na.rm = T)) %>%
#       arrange(Criteria)%>%  # arrange on just Criteria to make column order make more sense
#       filter(Exceedances > 0)
#     
#     # If there are any exceedances, create a string that includes the metal name(s) and the number of exceedances in parentheses
#     if(nrow(metalsExceedances)>0){
#       WAT_MET_STATstring<-sapply(seq_len(nrow(metalsExceedances)), function(i) paste0(metalsExceedances$Criteria[i]," (", metalsExceedances$Exceedances[i], ")")) %>%
#         toString()
#       # Create tibble in the format of the stations table that takes the sum of all exceedances for the WAT_MET_EXC field and returns the string created above for the WAT_MET_STAT field
#       metalsResults <- tibble("WAT_MET_EXC" = sum(metalsExceedances$Exceedances), "WAT_MET_STAT" = "Review", "WAT_MET_COMMENT" = WAT_MET_STATstring)
#     }else{
#       # If metals data do exist, but there are no exceedances, return 0 and "S" for supporting
#       metalsResults <- tibble("WAT_MET_EXC" = 0, "WAT_MET_STAT" = "S", "WAT_MET_COMMENT" = NA)
#     }}else{
#       # If no metals data exist, return a tibble with NA for both fields
#       metalsResults <- tibble("WAT_MET_EXC" = NA, "WAT_MET_STAT" = NA, "WAT_MET_COMMENT" = NA)
#     }    
#   return(metalsResults)
# }


# For use with rolling aquatic life use method

metalsAnalysis <- function(stationMetalsData, stationData, WER){
  # If no WER supplied, use 1
  WER <- ifelse(is.na(WER), 1, WER)
  
  # Get WQS from stationData so correct criteria can be applied
  stationMetalsData1 <- left_join(stationMetalsData, 
                                 dplyr::select(stationData, FDT_STA_ID, CLASS, PWS, ZONE) %>% 
                                   distinct(FDT_STA_ID, .keep_all = TRUE), by = c('Station_Id' = 'FDT_STA_ID')) %>% 
    mutate(`Assess As` = case_when(CLASS == "I" ~ 'Saltwater',
                                   CLASS == "II" & ZONE == 'Estuarine' ~ 'Saltwater',
                                   CLASS == "II" & ZONE == 'Transition' ~ 'More Stringent',
                                   CLASS == "II" & ZONE == 'Tidal Fresh' ~ 'Freshwater',
                                   CLASS %in% c('III', "IV","V","VI","VII") ~ 'Freshwater',
                                   TRUE ~ as.character(NA)),
           ChromiumIIIDissolved= ChromiumDissolved, RMK_ChromiumIIIDissolved = RMK_ChromiumDissolved, 
           ChromiumVIDissolved= ChromiumDissolved, RMK_ChromiumVIDissolved = RMK_ChromiumDissolved,  # add individual Chromium variables to make joining to assessment criteria easier
           Iron = IronTotal, RMK_Iron = RMK_IronTotal,
           Uranium = UraniumTotal, RMK_Uranium = RMK_UraniumTotal,
           Hardness = HardnessTotal, RMK_Hardness = RMK_HardnessTotal) %>% # duplicate this in lower case because we want this to pass through select(-contains('Total')) later
    # Roger uses ChromiumIII and VI to flag any potential chromium issues, likely further lab analyses needed if either chromium criteria blown
    dplyr::select(-contains('Total', ignore.case = T)) %>%  # only apply criteria to dissolved metals for aquatic life use, except Uranium bc only total
    dplyr::select(Station_Id:RMK_CadmiumDissolved, ChromiumIIIDissolved:RMK_ChromiumVIDissolved, 
                  CopperDissolved:RMK_IronDissolved, 
                  IronTotal = Iron, RMK_IronTotal = RMK_Iron,
                  LeadDissolved:RMK_ThalliumDissolved,
                  UraniumTotal = Uranium, RMK_UraniumTotal = RMK_Uranium, ZincDissolved:RMK_HardnessDissolved,
                  HardnessTotal = Hardness, RMK_HardnessTotal = RMK_Hardness, 
                  HARDNESS:`Assess As`) # reorder to desired format and rename 
  
  # But there are times when there are more rows than actual data due to the structure of the original data pull. It is unnecessary
  # to analyze these empty rows. We can't do a simple drop_na() because there are legit times when we have a lot of missing metals data 
  # but still want to run the analysis. Below, pivot on parameter and date and filter out rows with NA, what's left are the dates (rows)
  # that we actually care about. Most stations this step is redundant, but for some unique situations this can prevent serious issues down the line
  desiredDates <- stationMetalsData1 %>% 
    dplyr::select(-contains("RMK_")) %>%  # drop all lab codes bc we just care about missing data now, not lab codes
    group_by(Station_Id, FDT_DATE_TIME, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`) %>% 
    pivot_longer(-c(Station_Id, FDT_DATE_TIME, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`), names_to = 'parameter', values_to = 'value') %>% 
    filter(!is.na(value))
  
  stationMetalsData <- filter(stationMetalsData1, FDT_DATE_TIME %in% desiredDates$FDT_DATE_TIME)
  
  
  # make a place to store raw analysis results
  rawCriteriaResults <- tibble(Station_Id = as.character(NA), WindowDateTimeStart = as.POSIXct(NA), FDT_DEPTH = as.numeric(NA),
                               CLASS = as.factor(NA), PWS = as.factor(NA), ZONE = as.factor(NA), `Assess As` = as.character(NA),
                               Metal = as.character(NA), Value = as.numeric(NA), ValueType = as.character(NA), Criteria = as.character(NA), 
                               `Criteria Type` = as.character(NA), Waterbody = as.character(NA), CriteriaValue = as.numeric(NA),
                               `Sample Count` = as.numeric(NA),
                               parameterRound = as.numeric(NA), Exceedance = as.numeric(NA))
  acuteCriteriaResults <- rawCriteriaResults 
  chronicCriteriaResults <- acuteCriteriaResults 
  
  # loop through each row of data to correctly calculate criteria and find any chronic scenarios
  for(k in stationMetalsData$FDT_DATE_TIME){
    rawDataWindow <- filter(stationMetalsData, FDT_DATE_TIME == k)
    acuteDataWindow <- filter(stationMetalsData,  between(FDT_DATE_TIME, k, k + hours(1)))
    chronicDataWindow <- filter(stationMetalsData,  between(FDT_DATE_TIME, k, k + days(4)))
    # Run any analyses requiring raw data if data exists
    if(nrow(rawDataWindow) > 0){
      rawData <- rawDataWindow %>% 
        group_by(Station_Id, FDT_DATE_TIME, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`) %>% 
        dplyr::select(-c(contains('RMK_'))) %>% 
        pivot_longer(cols = AntimonyDissolved:HARDNESS, names_to = "Metal", values_to = "Value") %>% 
        mutate(ValueType = 'Raw Result',
               ID = paste(Station_Id, FDT_DATE_TIME, FDT_DEPTH, sep = '_')) %>% # make a uniqueID in case >1 sample for given datetime
        ungroup()
      # Calculate criteria based on raw data
      rawDataCriteria <- metalsCriteriaFunction(filter(rawData, Metal == "HARDNESS")$ID, filter(rawData, Metal == "HARDNESS")$Value, WER = 1) %>% 
        filter(`Criteria Type` %in% c('All Other Waters', 'PWS')) %>% # don't need other criteria for acute window
        {if(is.na(unique(rawData$PWS)))
          filter(., `Criteria Type` != 'PWS')
          else .}
      # Join appropriate criteria to rawData for comparison to averaged data
      rawDataCriteriaAnalysis <- left_join(rawData, rawDataCriteria, by = c('ID', 'Metal')) %>% 
        mutate(`Sample Count` = 1, # will be 1 here
               parameterRound = signif(Value, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
               Exceedance = ifelse(parameterRound > CriteriaValue, 1, 0 ),
               WindowDateTimeStart = min(rawDataWindow$FDT_DATE_TIME)) %>%  # use 1/0 to easily summarize multiple results later
        filter(!is.na(Criteria)) %>%  # filter out metals that don't have chronic criteria
        dplyr::select(Station_Id, WindowDateTimeStart, everything()) %>% 
        dplyr::select(-c(FDT_DATE_TIME, ID))
      # Save the results for viewing later
      rawCriteriaResults <- bind_rows(rawCriteriaResults, rawDataCriteriaAnalysis) 
    } else {rawCriteriaResults <- rawCriteriaResults }
    # Run acute analysis if data exists
    if(nrow(acuteDataWindow) > 0){
      acuteData <- suppressMessages(acuteDataWindow %>% 
                                      group_by(Station_Id, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`) %>% # can't group by datetime or summary can't happen
                                      dplyr::select(-c(contains('RMK_'))) %>% 
                                      pivot_longer(cols = AntimonyDissolved:HARDNESS, names_to = "Metal", values_to = "CriteriaValue") %>% 
                                      ungroup() %>% group_by(Station_Id, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`, Metal) %>% 
                                      summarise(`Sample Count` = length(CriteriaValue),
                                                Value = mean(CriteriaValue, na.rm=T)) %>%  # get hourly average
                                      mutate(ValueType = 'Hourly Average',
                                             ID = paste(Station_Id, FDT_DEPTH, sep = '_')) ) # make a uniqueID in case >1 sample for given datetime
      # Calculate criteria based on hourly averaged data
      acuteDataCriteria <- metalsCriteriaFunction(filter(acuteData, Metal == "HARDNESS")$ID, filter(acuteData, Metal == "HARDNESS")$Value, WER = 1) %>% 
        filter(`Criteria Type` == 'Acute') %>% # don't need other criteria for acute window
        # Keep only the criteria needed 
        {if(unique(acuteData$`Assess As`) %in% c('Freshwater', 'Saltwater'))
          filter(., Waterbody %in% c(NA, !!unique(acuteData$`Assess As`)))
          # if in Transition Zone then use the more stringent standard
          else group_by(., Metal) %>% 
            mutate(MoreStringent = min(CriteriaValue)) %>% 
            filter(CriteriaValue == MoreStringent) %>% 
            dplyr::select(-MoreStringent)} 
      # Join appropriate criteria to acuteData for comparison to averaged data
      acuteDataCriteriaAnalysis <- left_join(acuteData, acuteDataCriteria, by = c('ID', 'Metal')) %>% 
        mutate(parameterRound = signif(Value, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
               Exceedance = ifelse(parameterRound > CriteriaValue, 1, 0 ), # use 1/0 to easily summarize multiple results later
               WindowDateTimeStart = min(acuteDataWindow$FDT_DATE_TIME)) %>% 
        filter(!is.na(Criteria)) %>% # filter out metals that don't have chronic criteria
        dplyr::select(names(rawDataCriteriaAnalysis))
      # dplyr::select(Station_Id, WindowDateTimeStart, everything(), -ID)      # Save the results for viewing later
      acuteCriteriaResults <- bind_rows(acuteCriteriaResults, acuteDataCriteriaAnalysis) 
    } else {acuteCriteriaResults <- acuteCriteriaResults }
    # Run chronic analysis if data exists
    if(nrow(chronicDataWindow) > 0){
      chronicData <- suppressMessages(chronicDataWindow %>% 
                                        group_by(Station_Id, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`) %>% # can't group by datetime or summary can't happen
                                        dplyr::select(-c(contains('RMK_'))) %>% 
                                        pivot_longer(cols = AntimonyDissolved:HARDNESS, names_to = "Metal", values_to = "CriteriaValue") %>% 
                                        ungroup() %>% group_by(Station_Id, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`, Metal) %>% 
                                        summarise(`Sample Count` = length(CriteriaValue),
                                                  Value = mean(CriteriaValue, na.rm=T)) %>% # get four day average
                                        mutate(ValueType = 'Four Day Average',
                                               ID = paste(Station_Id, FDT_DEPTH, sep = '_')) ) # make a uniqueID in case >1 sample for given datetime
      # Calculate criteria based on hourly averaged data
      chronicDataCriteria <- metalsCriteriaFunction(filter(chronicData, Metal == "HARDNESS")$ID, filter(chronicData, Metal == "HARDNESS")$Value, WER = 1) %>% 
        filter(`Criteria Type` == 'Chronic') %>% # don't need other criteria for chronic window analysis
        # Keep only the criteria needed 
        {if(unique(chronicData$`Assess As`) %in% c('Freshwater', 'Saltwater'))
          filter(., Waterbody %in% c(NA, !!unique(chronicData$`Assess As`)))
          # if in Transition Zone then use the more stringent standard
          else group_by(., Metal) %>% 
            mutate(MoreStringent = min(CriteriaValue)) %>% 
            filter(CriteriaValue == MoreStringent) %>% 
            dplyr::select(-MoreStringent)} 
      # Join appropriate criteria to chronicData for comparison to averaged data
      chronicDataCriteriaAnalysis <- left_join(chronicData, chronicDataCriteria, by = c('ID', 'Metal')) %>% 
        mutate(parameterRound = signif(Value, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
               Exceedance = ifelse(parameterRound > CriteriaValue, 1, 0 ), # use 1/0 to easily summarize multiple results later
               WindowDateTimeStart = min(chronicDataWindow$FDT_DATE_TIME)) %>% 
        filter(!is.na(Criteria)) %>% # filter out metals that don't have chronic criteria
        dplyr::select(names(rawDataCriteriaAnalysis))
      # dplyr::select(Station_Id, WindowDateTimeStart, everything(), -ID)
      # Save the results for viewing later
      chronicCriteriaResults <- bind_rows(chronicCriteriaResults, chronicDataCriteriaAnalysis) 
    } else {chronicCriteriaResults <- chronicCriteriaResults }
  }
  stationCriteriaResults <- bind_rows(rawCriteriaResults, acuteCriteriaResults, chronicCriteriaResults) %>% 
    filter(!is.na(Station_Id)) %>% # drop placeholder rows
    distinct(Station_Id, WindowDateTimeStart, FDT_DEPTH, Criteria, .keep_all = T) %>% # remove duplicates in case > 1 depth per datetime
    arrange(Station_Id, WindowDateTimeStart, FDT_DEPTH, Metal)
  return(stationCriteriaResults)
}



# Metals Assessment function that makes sense of output from metalsAnalysis()
metalsAssessmentFunction <- function(metalsAnalysisResults){

  #Check to make sure that metals data exist
  if(!is.null(metalsAnalysisResults)){
    if(nrow(metalsAnalysisResults) > 0){
      
      #  Organize results to include Staion_Id, Criteria, and the # of Exceedances
      metalsExceedances <- metalsAnalysisResults %>%
        rename("Exceedances" = `n Windows Exceeding`)%>%
        arrange(`Criteria Type`) %>%  # arrange on just Criteria to make column order make more sense
        filter(Exceedances > 0 & `Suggested Result` != "Supporting")
      
      # If there are any exceedances, create a string that includes the metal name(s) and the number of exceedances in parentheses
      if(nrow(metalsExceedances)>0){
        WAT_MET_STATstring<-sapply(seq_len(nrow(metalsExceedances)), function(i) paste0(metalsExceedances$`Criteria Type`[i]," (", metalsExceedances$Exceedances[i], ")")) %>%
          toString()
        # Create tibble in the format of the stations table that takes the sum of all exceedances for the WAT_MET_EXC field and returns the string created above for the WAT_MET_STAT field
        metalsResults <- tibble("WAT_MET_EXC" = sum(metalsExceedances$Exceedances), "WAT_MET_STAT" = "Review", "WAT_MET_COMMENT" = WAT_MET_STATstring)
      }else{
        # If metals data do exist, but there are no exceedances, return 0 and "S" for supporting
        metalsResults <- tibble("WAT_MET_EXC" = 0, "WAT_MET_STAT" = "S", "WAT_MET_COMMENT" = NA)
      }}else{
        # If no metals data exist, return a tibble with NA for both fields
        metalsResults <- tibble("WAT_MET_EXC" = NA, "WAT_MET_STAT" = NA, "WAT_MET_COMMENT" = NA)
      } 
  } else {
    # If no metals data exist, return a tibble with NA for both fields
    metalsResults <- tibble("WAT_MET_EXC" = NA, "WAT_MET_STAT" = NA, "WAT_MET_COMMENT" = NA)
  }
     
  return(metalsResults)
}



# Single station chloride assessment

chlorideFreshwaterAnalysis <- function(stationData){
  # doesn't apply in class II transition zone
  stationDataCHL <- filter(stationData, CLASS %in% c('III', "IV","V","VI","VII") |
                             CLASS ==  "II" & ZONE == 'Tidal Fresh') %>% 
    filter(!(LEVEL_CHLORIDE %in% c('Level II', 'Level I'))) %>% # get lower levels out
    filter(!is.na(CHLORIDE_mg_L)) #get rid of NA's
  
  if(nrow(stationDataCHL) > 0){
    
    
    # make a place to store analysis results
    acuteCriteriaResults <- tibble(FDT_STA_ID = as.character(NA), WindowDateTimeStart = as.POSIXct(NA), FDT_DEPTH = as.numeric(NA),
                                   Value = as.numeric(NA), ValueType = as.character(NA), 
                                   `Criteria Type` = as.character(NA), CriteriaValue = as.numeric(NA), 
                                   `Sample Count` = as.numeric(NA), 
                                   parameterRound = as.numeric(NA), Exceedance = as.numeric(NA),
                                   associatedData = list())
    chronicCriteriaResults <- acuteCriteriaResults
    
    # loop through each row of data to correctly calculate criteria and find any chronic scenarios
    for(k in stationDataCHL$FDT_DATE_TIME){  #k = stationDataCHL$FDT_DATE_TIME[1]
      acuteDataWindow <- filter(stationDataCHL,  between(FDT_DATE_TIME, k, k + hours(1)))
      chronicDataWindow <- filter(stationDataCHL,  between(FDT_DATE_TIME, k, k + days(4)))
      
      # Run acute analysis if data exists
      if(nrow(acuteDataWindow) > 0){
        acuteDataCriteriaAnalysis <- suppressMessages( 
          dplyr::select(acuteDataWindow, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, CHLORIDE_mg_L) %>% 
          group_by(FDT_STA_ID, FDT_DEPTH) %>% # can't group by datetime or summary can't happen
          summarise(Value = mean(CHLORIDE_mg_L, na.rm=T),  # get hourly average
                    `Sample Count` = length(CHLORIDE_mg_L)) %>%  #count sample that made up average
          mutate(ValueType = 'Hourly Average',
                 ID = paste( FDT_STA_ID, FDT_DEPTH, sep = '_'), # make a uniqueID in case >1 sample for given datetime
                 `Criteria Type` = 'Acute', 
                 CriteriaValue = 860) %>%  # 860,000ug/L criteria to mg/L
          mutate(parameterRound = signif(Value, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
                 Exceedance = ifelse(parameterRound > CriteriaValue, 1, 0 ), # use 1/0 to easily summarize multiple results later
                 WindowDateTimeStart = min(acuteDataWindow$FDT_DATE_TIME)) %>% 
          dplyr::select(FDT_STA_ID, WindowDateTimeStart, everything(), -ID) ) %>% 
          bind_cols(tibble(associatedData = list(dplyr::select(acuteDataWindow, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, CHLORIDE_mg_L) ) ))
        # Save the results for viewing later
        acuteCriteriaResults <- bind_rows(acuteCriteriaResults, acuteDataCriteriaAnalysis) 
      } else {acuteCriteriaResults <- acuteCriteriaResults }
      
      # Run chronic analysis if data exists
      if(nrow(chronicDataWindow) > 0){
        chronicDataCriteriaAnalysis <- suppressMessages( 
          dplyr::select(chronicDataWindow, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, CHLORIDE_mg_L) %>% 
          group_by(FDT_STA_ID, FDT_DEPTH) %>% # can't group by datetime or summary can't happen
          summarise(Value = mean(CHLORIDE_mg_L, na.rm=T),  # get hourly average
                    `Sample Count` = length(CHLORIDE_mg_L)) %>%  #count sample that made up average
          mutate(ValueType = 'Four Day Average',
                 ID = paste( FDT_STA_ID, FDT_DEPTH, sep = '_'), # make a uniqueID in case >1 sample for given datetime
                 `Criteria Type` = 'Chronic', 
                 CriteriaValue = 230) %>%  # 230,000ug/L criteria to mg/L
          mutate(parameterRound = signif(Value, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
                 Exceedance = ifelse(parameterRound > CriteriaValue, 1, 0 ), # use 1/0 to easily summarize multiple results later
                 WindowDateTimeStart = min(chronicDataWindow$FDT_DATE_TIME)) %>% 
          dplyr::select(FDT_STA_ID, WindowDateTimeStart, everything(), -ID) ) %>% 
          bind_cols(tibble(associatedData = list(dplyr::select(chronicDataWindow, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, CHLORIDE_mg_L) ) ))
        # Save the results for viewing later
        chronicCriteriaResults <- bind_rows(chronicCriteriaResults, chronicDataCriteriaAnalysis) 
      } else {chronicCriteriaResults <- chronicCriteriaResults }
    }
    # summarize results
    stationCriteriaResults <- bind_rows( acuteCriteriaResults, chronicCriteriaResults) %>% 
      filter(!is.na(FDT_STA_ID)) %>% # drop placeholder rows
      distinct(FDT_STA_ID, WindowDateTimeStart, FDT_DEPTH, `Criteria Type`, .keep_all = T) %>% # remove duplicates in case > 1 depth per datetime
      arrange(FDT_STA_ID, WindowDateTimeStart, FDT_DEPTH, `Criteria Type`)
    return(stationCriteriaResults)
  } else { return(NULL)}
}
# dataToAnalyze <- chlorideFreshwaterAnalysis(stationData) 

# fake data for testing
#dataToAnalyze$`Sample Count`[1:77] <- rep(3, 77)
# dataToAnalyze <- zz




# New rolling window analysis function. This calculates the number of exceedances in a X year window (not repeating windows)
#  and determines if the number of windows exceeding criteria are greater than the number of windows not exceeding criteria
#  using subsequent annualRollingExceedanceSummary()
# analyze results by 3 year windows, not a strict rolling window, roll by year
annualRollingExceedanceAnalysis <- function(dataToAnalyze,
                                            yearsToRoll, # number of years to roll analysis over
                                            aquaticLifeUse = FALSE # whether or not this function is being used for Aquatic Life Use criteria
                                            # default = FALSE, which uses the Ammonia definition for window validity for chronic and four day sampling criteria
                                            # if TRUE, two samples across the rolled window length ensures a valid window
                                            ){
  
  if(is.null(dataToAnalyze)){return(NULL)}
  if(nrow(dataToAnalyze) == 0 ){return(NULL)}
  
  # place to store results
  dataWindowResults <- tibble(FDT_STA_ID = as.character(NA), `Window Begin` = as.numeric(NA),
                              FDT_DEPTH = as.character(NA),# character so we can concatenate multiple depth values if needed
                              #FDT_DEPTH = as.numeric(NA), 
                              `Criteria Type` = as.character(NA),
                              `Years Analysis Rolled Over`= as.numeric(NA), 
                              `Exceedances in Rolled Window` = as.numeric(NA), 
                              associatedData = list())
  dataToAnalyze <- dataToAnalyze %>% 
    mutate(Year = year(WindowDateTimeStart),
           `Valid Window` = case_when(`Criteria Type` %in% c("Acute") ~ TRUE, # just to make people feel good later on
                                      `Criteria Type` %in% c("Chronic", "Four Day")  & `Sample Count` > 1 ~ TRUE,
                                      TRUE ~ NA))
  
  # First, identify all window start options
  windowOptions <- unique(year(dataToAnalyze$WindowDateTimeStart))
  # Now, stop loop from repeating windows by capping the top end by yearsToRoll-1 (so don't have 3 yr windows exceeding assessment period), then remove any
  #  windowRange options that don't actually have data in those years
  windowRange <- (min(windowOptions):(max(windowOptions)- (yearsToRoll- 1)))#[(min(windowOptions):(max(windowOptions)- (yearsToRoll- 1))) %in% windowOptions]
  # windowRange check to remove unnecessary windows that occur before the first windowRange year 
  # e.g. if only one year of data is available, window range will return: year, year - 1, and year -2 which isn't necessary, so stop that behavior
  if(length(windowRange) > 1 & windowRange[2] < windowRange[1]){    windowRange <- windowRange[1]  }

  for(i in windowRange){ #i =  windowRange[1] #min(min(windowRange):(max(windowRange)- (yearsToRoll- 1))[1])
    dataWindow <- filter(dataToAnalyze, Year %in% i:(i + yearsToRoll- 1) ) # minus 1 year for math to work
    # unfortunately have to double loop this to attach the correct dataWindow by Criteria type instead of a simple summarize
    # should be able to do something similar to this but filter the embedded dataWindow by the grouping variables but cant figure out right now
    # dataWindowAnalysis <- suppressMessages( dataWindow %>% 
    #                                           group_by(FDT_STA_ID, #FDT_DEPTH, 
    #                                                    `Criteria Type`) %>%
    #                                           summarise(`Criteria Type` = unique(`Criteria Type`),
    #                                                     `Window Begin` = year(min(WindowDateTimeStart)),
    #                                                     `Years Analysis Rolled Over` = yearsToRoll, 
    #                                                     `Exceedances in Rolled Window` = sum(Exceedance),
    #                                                     `Valid Window` = all(`Valid Window` == T)) %>% 
    #                                           bind_cols(tibble(associatedData = list(dataWindow))) )

    for(k in unique(dataWindow$`Criteria Type`)){ # this logic will skip years with no data
      dataWindowCriteria <- filter(dataWindow, `Criteria Type` %in% k)
      dataWindowAnalysis <- suppressMessages( dataWindowCriteria %>% 
                                                group_by(FDT_STA_ID, #FDT_DEPTH, 
                                                         `Criteria Type`) %>%
                                                {if(aquaticLifeUse == FALSE)
                                                  summarise(., `Criteria Type` = unique(`Criteria Type`),
                                                            `Window Begin` = i , #year(min(WindowDateTimeStart)),
                                                            `Years Analysis Rolled Over` = yearsToRoll, 
                                                            `Exceedances in Rolled Window` = sum(Exceedance),
                                                            `Valid Window` = all(`Valid Window` == T))
                                                  else summarise(., `Criteria Type` = unique(`Criteria Type`),
                                                                 `Window Begin` = i, #year(min(WindowDateTimeStart)),
                                                                 `Years Analysis Rolled Over` = yearsToRoll, 
                                                                 `Exceedances in Rolled Window` = sum(Exceedance),
                                                                 `Valid Window` = ifelse(nrow(dataWindowCriteria) >= 2, TRUE, NA))
                                                  }   %>% 
                                                bind_cols(tibble(associatedData = list(dataWindowCriteria))) )
      dataWindowResults <- bind_rows(dataWindowResults, dataWindowAnalysis) 
      }
    }
  return(dataWindowResults)
}


# Flag if more windows exceeding than not
# rolledAnalysis <- annualRollingExceedanceAnalysis(dataToAnalyze, 3, aquaticLifeUse = FALSE)
annualRollingExceedanceSummary <- function(rolledAnalysis){
  
  if(is.null(rolledAnalysis)){return(NULL)}
  
  # make a valid dataset for this analysis
  validDataForExceedanceSummary <- bind_rows(
    rolledAnalysis %>% 
      #filter(`Criteria Type` == 'Chronic' & `Valid Window` == TRUE),
      filter(str_detect(`Criteria Type`, 'Chronic') & `Valid Window` == TRUE), # more flexible so metals can pass through this function
    rolledAnalysis %>% 
      #filter(`Criteria Type` == 'Acute')  ) %>% 
      filter(str_detect(`Criteria Type`,  'Acute'))  ) %>% 
    {if("Four Day" %in% unique(rolledAnalysis$`Criteria Type`))
      bind_rows(.,
                filter(rolledAnalysis, `Criteria Type` == "Four Day"& `Valid Window` == TRUE) )
      else . } %>% 
    {if(any(grep("All Other Surface Waters", unique(rolledAnalysis$`Criteria Type`))) )
      bind_rows(.,
                filter(rolledAnalysis, str_detect(`Criteria Type`,  "All Other Surface Waters") & `Valid Window` == TRUE) )
      else . } 
  
  # Note: the PWS results are purposefully omitted from the rolling window assessment. These use median and reported on in a toxics module

  
  suppressMessages(
    validDataForExceedanceSummary %>% 
    group_by(FDT_STA_ID, FDT_DEPTH, `Criteria Type`) %>% 
    summarise(`n Windows Fine` = sum(`Exceedances in Rolled Window` < 2),
              `n Windows Exceeding` = sum(`Exceedances in Rolled Window` >= 2)) %>% 
    mutate(`Suggested Result` = case_when(`n Windows Exceeding` >=2 ~ "Impaired",
                                          `n Windows Exceeding` < 2 & `n Windows Fine` > `n Windows Exceeding` ~ "Supporting",
                                          `n Windows Exceeding` < 2 & `n Windows Fine` < `n Windows Exceeding` ~ "Review",
                                          `n Windows Exceeding` < 2 & `n Windows Fine` == `n Windows Exceeding` ~ "Review",
                                          TRUE ~ as.character(NA))))
    
}

# annualRollingExceedanceSummaryOutput <- annualRollingExceedanceSummary(annualRollingExceedanceAnalysis(chlorideFreshwaterAnalysis(stationData), 3, aquaticLifeUse = FALSE) )
# annualRollingExceedanceSummaryOutput <- annualRollingExceedanceSummary(
#   annualRollingExceedanceAnalysis(
#     freshwaterNH3limit(stationData, trout = ifelse(unique(stationData$CLASS) %in% c('V','VI'), TRUE, FALSE),
#                        mussels = TRUE, earlyLife = TRUE) , 3, aquaticLifeUse = FALSE) )
# annualRollingExceedanceSummaryOutput <- annualRollingExceedanceSummary(rolledAnalysis) # result with fake data to show alt options


# Don't use, can make more general and thus useful
# chlorideFreshwaterSummary <- function(chlorideFreshwater){
#   chlorideFreshwaterExceed <- filter(chlorideFreshwater, Exceedance == 1 & `Criteria Type` %in% "Acute" |
#                                        Exceedance == 1 & `Criteria Type` %in% "Chronic" & `Sample Count` > 1)
#   if(nrow(chlorideFreshwaterExceed) >0){
#     return(tibble(CHL_EXC = nrow(chlorideFreshwaterExceed), CHL_STAT = 'Review'))
#   } else {return(tibble(CHL_EXC = NA, CHL_STAT= NA))}
# }
# #chlorideFreshwaterSummary(chlorideFreshwaterAnalysis(stationData))


# Rolling summary for stations table
rollingWindowSummary <- function(annualRollingExceedanceSummaryOutput, parameterAbbreviation){
  
  if("Review" %in% unique(annualRollingExceedanceSummaryOutput$`Suggested Result`)){
    z <- tibble(`_EXC` = as.numeric(NA), `_STAT` = 'Review')
  } else {
    # first kick out NULL entries (no data to analyze)
    if(is.null(annualRollingExceedanceSummaryOutput)){
      z <- tibble(`_EXC` = as.numeric(NA), `_STAT` = as.character(NA))
      } else {  z <- tibble(`_EXC` = as.numeric(NA), `_STAT` = "S")} }
  names(z) <- paste0(parameterAbbreviation, names(z))
  return(z)
}
# rollingWindowSummary(annualRollingExceedanceSummaryOutput, "AMMONIA")
# rollingWindowSummary(annualRollingExceedanceSummary(annualRollingExceedanceAnalysis(chlorideFreshwaterAnalysis(stationData), 3, aquaticLifeUse = TRUE) ), "CHL")


# Function to add a column to stations table output that summarizes 7Q10 information for relevant parameters
lowFlowFlagColumn <- function(stationData){
  lowFlowFlag <- filter(stationData, !is.na(`7Q10 Flag`))
  if(nrow(lowFlowFlag) > 0){
    withParameterData <- dplyr::select(lowFlowFlag, FDT_STA_ID, FDT_DATE_TIME, FDT_TEMP_CELCIUS, DO_mg_L, FDT_FIELD_PH, `7Q10 Flag Gage`) %>% 
      pivot_longer(-c(FDT_STA_ID, FDT_DATE_TIME, `7Q10 Flag Gage`), names_to = 'parameter', values_to = 'value') %>% 
      filter(!is.na(value)) # drop rows with no data for specific parameters
    if(nrow(withParameterData) > 0){
      gageFlags <- dplyr::select(withParameterData, FDT_STA_ID, FDT_DATE_TIME,  `7Q10 Flag Gage`) %>% 
        distinct(FDT_DATE_TIME, .keep_all = T) %>% 
        summarise(STATION_ID = unique(FDT_STA_ID), 
                  `7Q10 Flag` = paste(as.Date(FDT_DATE_TIME),  `7Q10 Flag Gage`, collapse = '; ')) %>% 
        mutate(`7Q10 Flag` = paste('USGS Gages in subbasin below 7Q10: ', `7Q10 Flag`)) 
    } else {
      gageFlags <- tibble(STATION_ID = unique(lowFlowFlag$FDT_STA_ID),
                          `7Q10 Flag` = NA_character_)   }
  } else {
    gageFlags <- tibble(STATION_ID = unique(lowFlowFlag$FDT_STA_ID),
                        `7Q10 Flag` = NA_character_)
  }
  return(gageFlags)
}

#lowFlowFlagColumn(stationData)




# Function to analyze levels for stations and output a comment if data is only level I or level II
levelCommentAnalysis <- function(stationData){
  # organize levels available in dataset
  levelSummary <- dplyr::select(stationData, FDT_STA_ID, contains('LEVEL')) %>% 
    pivot_longer(-FDT_STA_ID, names_to = 'Parameter', values_to = 'Value') %>% 
    filter(!is.na(Value)) 
  
  # comment for all level II data
  if(length(unique(levelSummary$Value)) == 1 && unique(levelSummary$Value) %in% c('Level II')){
    levelComment = 'Station contains all Level II data'
  }else{
    # comment for all Level I and level II data
    if(length(unique(levelSummary$Value)) == 2 && all(unique(levelSummary$Value) %in% c("Level I",'Level II'))){
      levelComment = 'Station contains all Level I and Level II data'
      # comment for everything else
    }else{levelComment = NA} }
  return(levelComment)
}
#levelCommentAnalysis(stationData)