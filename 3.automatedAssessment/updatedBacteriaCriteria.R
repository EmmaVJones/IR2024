# Updated Bacteria Criteria based on 9VAC25-260-170 (https://lis.virginia.gov/cgi-bin/legp604.exe?000+reg+9VAC25-260-170&000+reg+9VAC25-260-170)

# Function is built to calculate both E.coli and Enterococci based on 90 day assessment windows.
# here is some test data 
# station <- '1AACO014.57'#'2-JKS023.61'#'1ABAR037.84'#
# stationData <- filter(conventionals, FDT_STA_ID %in% station)%>%
#    left_join(stationTable, by = c('FDT_STA_ID' = 'STATION_ID')) %>% 
#   # Special Standards Correction step. This is done on the actual data bc some special standards have temporal components
#   pHSpecialStandardsCorrection() %>% # correct pH to special standards where necessary
#   temperatureSpecialStandardsCorrection() %>% # correct temperature special standards where necessary
#   # special lake steps
#   {if(station %in% lakeStations$STATION_ID)
#       suppressWarnings(suppressMessages(
#         mutate(., lakeStation = TRUE) %>%
#           thermoclineDepth())) # adds thermocline information and SampleDate
#       else mutate(., lakeStation = FALSE) }

# # add some high frequency data
# stationData <- bind_rows(stationData,
#                         data.frame(FDT_STA_ID = c('2-JKS023.61', '2-JKS023.61', '2-JKS023.61', '2-JKS023.61', '2-JKS023.61', '2-JKS023.61',
#                                                  '2-JKS023.61', '2-JKS023.61', '2-JKS023.61', '2-JKS023.61', '2-JKS023.61', '2-JKS023.61', '2-JKS023.61'),
#                           FDT_DATE_TIME= as.POSIXct(c('2019-02-12 10:00:00', '2019-02-13 10:00:00', '2019-02-14 10:00:00', '2019-02-15 10:00:00', '2019-02-16 10:00:00',
#                                           '2019-02-17 10:00:00', '2019-02-18 10:00:00', '2019-02-19 10:00:00', '2019-02-20 10:00:00', '2019-02-21 10:00:00',
#                                           '2019-02-22 10:00:00','2019-02-23 10:00:00','2019-02-24 10:00:00')),
#                          ECOLI = c(22, 33, 44, 55, 66, 77, 88, 99, 100, 800, 450, 400, 430),
#                          LEVEL_ECOLI = c('Level II', 'Level II', 'Level II', 'Level I', 'Level III', NA, NA, NA, NA, NA, NA, NA, NA)))
#                          #LEVEL_ECOLI = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)))


# 
# bacteriaField <- 'ECOLI' #
# #  'ENTEROCOCCI'
# bacteriaRemark <- 'LEVEL_ECOLI' #
# #  'LEVEL_ENTEROCOCCI'
# sampleRequirement <- 10
# STV <- 410 #
# #130
# geomeanCriteria <- 126 #
# #  35
# rm(i);rm(bacteriaField);rm(bacteriaRemark); rm(sampleRequirement); rm(STV); rm(geomeanCriteria); rm(stationTableName); rm(z);rm(exceedGeomean); rm(out); rm(exceedSTVrate);rm(stationData); rm(time1);rm(timePlus89); rm(nSamples); rm(exceedSTVn);

# Function to assess on 90 day windows across input dataset
# really just a building block, one probably wouldn't run this function independently
bacteriaExceedances_NEW <- function(stationData, # input dataframe with bacteria data
                                    bacteriaField, # name of bacteria field data
                                    bacteriaRemark, # name of bacteria comment field
                                    sampleRequirement, # minimum n samples in 90 day window needed to apply geomean
                                    STV, # unique for ecoli/enter
                                    geomeanCriteria # unique for ecoli/enter
                                    ){
  # Output tibble to organize results, need list object to save associate data
  out <- tibble(`StationID` = as.character(NA),
                `Date Window Starts` = as.Date(NA), 
                `Date Window Ends` = as.Date(NA), 
                `Last Sample Date in Window` = as.Date(NA), 
                `Samples in 90 Day Window` = as.numeric(NA), 
                `STV Exceedances In Window` = as.numeric(NA), 
                `STV Exceedance Rate` = as.numeric(NA), 
                `STV Assessment` = as.character(NA),
                `Geomean In Window` = as.numeric(NA),
                `Geomean Assessment` = as.character(NA),
                associatedData = list())
  
  # Data reorg to enable both types of bacteria assessment from a single function
  stationData2 <- dplyr::select(stationData, FDT_STA_ID, FDT_DATE_TIME, !! bacteriaField, !! bacteriaRemark) %>%
    rename(Value = bacteriaField, LEVEL_Value = bacteriaRemark) %>%
    filter(! LEVEL_Value %in% c('Level II', 'Level I')) %>% # get lower levels out
    filter(!is.na(Value))
  
  if(nrow(stationData2) > 0){
    # Loop through each row of input df to test 90 day windows against assessment criteria
    for( i in 1 : nrow(stationData2)){
      time1 <- as.Date(stationData2$FDT_DATE_TIME[i])
      timePlus89 <- time1 + days(89) 
      
      # Organize prerequisites to decision process
      windowData <- filter(stationData2, as.Date(FDT_DATE_TIME) >= time1 & as.Date(FDT_DATE_TIME) <= timePlus89) %>% 
        mutate(nSamples = n(), # count number of samples in 90 day window
               STVhit = ifelse(Value > STV, TRUE, FALSE), # test values in window against STV
               geomean = ifelse(nSamples > 1, # calculate geomean of samples if nSamples>1
                                as.numeric(round::roundAll(EnvStats::geoMean(Value, na.rm = TRUE), digits=0, "r0.C")), # round to nearest whole number per Memo to Standardize Rounding for Assessment Guidance
                                NA), 
               geomeanCriteriaHit = ifelse(geomean > geomeanCriteria, TRUE, FALSE)) # test round to even geomean against geomean Criteria
      
      # First level of testing: any STV hits in dataset? Want this information for all scenarios
      nSTVhitsInWindow <- nrow(filter(windowData, STVhit == TRUE))
      # STV exceedance rate calculation with round to even math
      STVexceedanceRate <- ifelse(unique(windowData$nSamples) >= 10, 
                                  as.numeric(round::roundAll((nSTVhitsInWindow / unique(windowData$nSamples)) * 100,digits=0, "r0.C")), # round to nearest whole number per Memo to Standardize Rounding for Assessment Guidance
                                  NA) # no STV exceedance rate if < 10 samples
      if(nSTVhitsInWindow == 0){
        `STV Assessment` <- 'No STV violations within 90 day window' } 
      if(nSTVhitsInWindow == 1){
        `STV Assessment` <- paste(nSTVhitsInWindow, ' STV violation(s) with ', format(STVexceedanceRate, digits = 3), 
                                  '% exceedance rate in 90 day window | Insufficient Information (Prioritize for follow up monitoring)',sep='')}
      if(nSTVhitsInWindow >= 2){
        `STV Assessment` <- paste(nSTVhitsInWindow, ' STV violation(s) with ', format(STVexceedanceRate, digits = 3), 
                                  '% exceedance rate in 90 day window | Impaired: ', nSTVhitsInWindow,' hits in the same 90-day period',sep='') }
      
      
      # Second level of testing: only if minimum geomean sampling requirements met in 90 day period
      if(unique(windowData$nSamples) >= sampleRequirement){
        # Geomean Hit
        if(unique(windowData$geomeanCriteriaHit) == TRUE){
          `Geomean Assessment` <- paste('Geomean: ', format(unique(windowData$geomean), digits = 3), 
                                        ' | Impaired: geomean exceeds criteria in the 90-day period', sep='')  
        } else{
          `Geomean Assessment` <-  paste('Geomean: ', format(unique(windowData$geomean), digits = 3), 
                                         ' | Geomean criteria met, hold assessment decision for further testing', sep= '')} 
      } else { # minimum geomean sampling requirements NOT met in 90 day period
        `Geomean Assessment` <- 'Insufficient Information: geomean sampling criteria not met'  }
      
      out[i,] <-  tibble(`StationID` = unique(stationData2$FDT_STA_ID),
                         `Date Window Starts` = time1, 
                         `Date Window Ends` = timePlus89, 
                         `Last Sample Date in Window` = max(windowData$FDT_DATE_TIME),
                         `Samples in 90 Day Window` = unique(windowData$nSamples), 
                         `STV Exceedances In Window` = nSTVhitsInWindow, 
                         `STV Exceedance Rate` = STVexceedanceRate,
                         `STV Assessment` = `STV Assessment`,
                         `Geomean In Window` = ifelse(unique(windowData$nSamples) >= sampleRequirement, unique(windowData$geomean), NA), # avoid excitement, only give geomean result if 10+ samples
                         `Geomean Assessment` = `Geomean Assessment`,
                         associatedData = list(windowData)) 
    } #end for loop
  } else {
    out <- tibble(`StationID` = unique(stationData$FDT_STA_ID),
                  `Date Window Starts` = as.Date(NA), 
                  `Date Window Ends` = as.Date(NA), 
                  `Last Sample Date in Window` = as.Date(NA), 
                  `Samples in 90 Day Window` = as.numeric(NA), 
                  `STV Exceedances In Window` = as.numeric(NA), 
                  `STV Exceedance Rate` = as.numeric(NA), 
                  `STV Assessment` = as.character(NA),
                  `Geomean In Window` = as.numeric(NA),
                  `Geomean Assessment` = as.character(NA),
                  associatedData = list(NA))
  }
  
  # For IR2024, we only want to make assessment decisions on data windows with unique datasets. Running distinct() on `Last Sample Date in Window`
  # will keep only the rows with the first occurrence of any given sample date, allowing us to remove rolled windows that contain duplicated 
  # datasets. Joining this result back to the out dataset allows us to flag `Valid Assessment Window` appropriately. Since this function
  # feeds the app, we want to keep all records and only at later functions that make assessment decisions do we want to filter out 
  # non-unique windows.
  distinctWindows <- distinct(out, `Last Sample Date in Window`, .keep_all = T) %>% 
    mutate(`Valid Assessment Window` = TRUE)
  
  out <- left_join(out, distinctWindows,
                    by = c("StationID", "Date Window Starts", "Date Window Ends", "Last Sample Date in Window", 
                           "Samples in 90 Day Window", "STV Exceedances In Window", "STV Exceedance Rate", 
                           "STV Assessment", "Geomean In Window", "Geomean Assessment", "associatedData")) %>% # join by everything to be safe
    dplyr::select(StationID:`Last Sample Date in Window`, `Valid Assessment Window`, everything())
  
  return(out) 
}

#y <- bacteriaExceedances_NEW(stationData, 'ECOLI', 'LEVEL_ECOLI', 10, 410, 126)



#Function to see if any 90 day windows have 2+ STV exceedances
STVexceedance <- function(df, STV){
  morethan1STVexceedanceInAnyWindow <- filter(df, `STV Exceedances In Window` >= 2)
  if(nrow(morethan1STVexceedanceInAnyWindow) > 0){
    return('| 2 or more STV exceedances in a 90 day window |')
  }
}


bacteriaField <- 'ECOLI' #
bacteriaRemark <- 'LEVEL_ECOLI' #
sampleRequirement <- 10
STV <- 410 #
geomeanCriteria <- 126 #

# Function to summarize bacteria assessment results into decisions
# This function returns all potential issues with priory on geomean results IF there
# are enough samples to run geomean
# Round to even rules are applied
bacteriaAssessmentDecision <- function(stationData, # input dataframe with bacteria data
                                       bacteriaField, # name of bacteria field data
                                       bacteriaRemark, # name of bacteria comment field
                                       sampleRequirement, # minimum n samples in 90 day window needed to apply geomean
                                       STV, # unique for ecoli/enter
                                       geomeanCriteria # unique for ecoli/enter
){
    # Rename output columns based on station table template
    stationTableName <- ifelse(bacteriaField == 'ECOLI', "ECOLI", "ENTER")
    
    nSamples <- select(stationData,  Value = {{ bacteriaField }} ) %>% 
      filter(!is.na(Value)) # total n samples taken in assessment window
    
    
    if(nrow(nSamples) > 0){ # only proceed through decisions if there is data to be analyzed

    # Run assessment function
    # make two objects here because we want to base all decisions only on valid data, but we also want to output all associated data from 
      # this function, so we create a rawAnalysisForOutput to be saved in a list column for unpacking later
    rawAnalysisForOutput <- suppressWarnings(bacteriaExceedances_NEW(stationData, bacteriaField, bacteriaRemark, sampleRequirement, STV, geomeanCriteria)   )
    validForAssessment <- rawAnalysisForOutput %>% 
      filter(`Valid Assessment Window` == TRUE)
    
    # bail out if no data to analyze bc all Level II or Level I, OR (new for IR2024) no valid windows for assessment
    if(nrow(filter(validForAssessment, !is.na(`Date Window Starts`))) == 0 ){
      return(tibble(StationID = unique(stationData$FDT_STA_ID),
                    `_EXC` = NA, # right now this is set to # total STV exceedances, not the # STV exceedances in a 90-day period with 10+ samples
                    #`_IMPAIREDWINDOWS` = NA,
                    `_SAMP` = NA, 
                    `_GM.EXC` = NA,
                    `_GM.SAMP` = NA,
                    `_STAT` = NA, # is this the right code???
                    `_STAT_VERBOSE` = NA, 
                    `BACTERIADECISION` = NA,
                    `BACTERIASTATS` = NA,
                    associatedDecisionData = list(NA)) %>%
               rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE)) %>%  # fix names to match station table format
               rename_with( ~ gsub(".", "_", .x, fixed = TRUE)) ) # special step to get around accidentally replacing _GM with station table name
    }
    
    # number of STV exceedances, reported in bacteria_EXC field in stations table and useful for logic testing
    # don't want to do this analysis on validForAssessment bc sum(validForAssessment$`STV Exceedances In Window`) will overshoot real n
    exceedSTVn <- select(stationData,  Value = {{ bacteriaField }} ) %>%
      filter(Value > STV) # total STV exceedances in dataset
    # # Windows with impairments, reported outside stations table bulk upload template
    # zz <- mutate(validForAssessment, UID = 1:n())
    # impairedWindow <- bind_rows(filter(zz, str_detect(`STV Assessment`, 'Impaired')), 
    #                             filter(zz, str_detect(`Geomean Assessment`, 'Impaired')) ) %>% 
    #   distinct(UID, .keep_all = T)
    # # Windows with STV exceedances, combo small and large window logic
    # exceedSTVwindow <- bind_rows(filter(zz, `STV Exceedance Rate` > 10), # `STV Exceedance Rate` only appears when 10+ samples in window
    #                         filter(zz, `STV Exceedances In Window` > 0 & `Samples in 90 Day Window` < 10)) %>% 
    #   distinct(UID, .keep_all = T)
    # Windows with > 10% STV rate, these can only be calculated on windows with 10 or more samples
    exceedSTVrate <- filter(validForAssessment, `STV Exceedance Rate` > 10)
    # windows with geomean exceedances, these can only be calculated on windows with 10 or more samples
    exceedGeomean <- filter(validForAssessment, `Geomean In Window` > geomeanCriteria)
    
    # Decision logic time, work through geomean first and if there is no appropriate geomean data (no windows with 10+ samples)
    #   then go to STV assessment
    
    # Were at least 10 samples taken within any 90-day period of the assessment window?
    if( any(!is.na(validForAssessment$`Geomean In Window`)) ){ # Were at least 10 samples taken within any 90-day period of the assessment window? - Yes
      # Do the geometric means calculated for the 90-day periods represented by 10+ samples meet the GM criterion?
      if( nrow(exceedGeomean) == 0){ # Do the geometric means calculated for the 90-day periods represented by 10+ samples meet the GM criterion? - Yes
        # Do any of the 90-day periods of the assessment window represented in the dataset exceed the 10% STV Exceedance Rate?
        if( nrow(exceedSTVn) > 0){ # Do any of the 90-day periods of the assessment window represented in the dataset exceed the 10% STV Exceedance Rate? - Yes
          
          # Yes, in a 90-day period represented by 10+ samples
          if(nrow(filter(exceedSTVrate, `Samples in 90 Day Window` >= 10 & `STV Exceedance Rate` > 10)) > 0){ # STV exceedances in a 90-day period represented by >= 10 samples
            return(tibble(StationID = unique(validForAssessment$StationID),
                          `_EXC` = nrow(exceedSTVn), # right now this is set to # total STV exceedances, not the # STV exceedances in a 90-day period with 10+ samples
                          `_SAMP` = nrow(nSamples), 
                          `_GM.EXC` = nrow(exceedGeomean),
                          `_GM.SAMP` = nrow(filter(validForAssessment, !is.na(`Geomean In Window`))),
                          `_STAT` = "IM",
                          `_STAT_VERBOSE` = "Impaired - 2 or more STV exceedances in the same 90-day period represented by 10+ samples, no geomean exceedances.",#STV exceedances in a 90-day period represented by >= 10 samples after verifying geomean passes where applicable.",
                          `BACTERIADECISION` = paste0(stationTableName, ": ",`_STAT_VERBOSE`),
                          `BACTERIASTATS` = paste0(stationTableName, ": Number of 90 day windows with > 10% STV exceedance rate: ", nrow(exceedSTVrate)),
                          associatedDecisionData = list(rawAnalysisForOutput) ) %>%
                     rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE)) %>%  # fix names to match station table format
                     rename_with( ~ gsub(".", "_", .x, fixed = TRUE)) ) # special step to get around accidentally replacing _GM with station table name
            
          } else {  # STV exceedances in a 90-day period represented by < 10 samples
            
            # 2 or more hits in the same 90-day period?
            if(any(validForAssessment$`STV Exceedances In Window` >= 2) ){
              return(tibble(StationID = unique(validForAssessment$StationID),
                            `_EXC` = nrow(exceedSTVn), # right now this is set to # total STV exceedances, not the # STV exceedances in a 90-day period with 10+ samples
                            `_SAMP` = nrow(nSamples), 
                            `_GM.EXC` = nrow(exceedGeomean),
                            `_GM.SAMP` = nrow(filter(validForAssessment, !is.na(`Geomean In Window`))),
                            `_STAT` = "IM",
                            `_STAT_VERBOSE` = "Impaired- 2 or more STV exceedances in the same 90-day period with < 10 samples, no geomean exceedances.", #2 or more STV hits in the same 90-day period with < 10 samples after verifying geomean passes where applicable.",
                            `BACTERIADECISION` = paste0(stationTableName, ": ",`_STAT_VERBOSE`),
                            `BACTERIASTATS` = paste0(stationTableName, ": Number of 90 day windows with > 10% STV exceedance rate: ", nrow(exceedSTVrate)),
                            associatedDecisionData = list(rawAnalysisForOutput) ) %>%
                       rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE)) %>%  # fix names to match station table format
                       rename_with( ~ gsub(".", "_", .x, fixed = TRUE)) ) # special step to get around accidentally replacing _GM with station table name
              } else { 
                
                # did the STV exceedance(s) occur in windows with 10+ samples?
                if(all(filter(validForAssessment, `STV Exceedances In Window` > 0)$`Samples in 90 Day Window` >= 10)){
                  return(tibble(StationID = unique(validForAssessment$StationID),
                                `_EXC` = nrow(exceedSTVn), # right now this is set to # total STV exceedances, not the # STV exceedances in a 90-day period with 10+ samples
                                `_SAMP` = nrow(nSamples), 
                                `_GM.EXC` = nrow(exceedGeomean),
                                `_GM.SAMP` = nrow(filter(validForAssessment, !is.na(`Geomean In Window`))),
                                `_STAT` = "S",
                                `_STAT_VERBOSE` = "Fully Supporting - No STV exceedance rates >10% or geomean exceedances in any 90-day period represented by 10+ samples.",# No geomean exceedances and STV exceedance(s) in one or multiple 90-day periods represented by 10+ samples.", # previous language: 1 STV hit in one or multiple 90-day periods with < 10 samples after verifying geomean passes where applicable.",
                                `BACTERIADECISION` = paste0(stationTableName, ": ",`_STAT_VERBOSE`),
                                `BACTERIASTATS` = paste0(stationTableName, ": Number of 90 day windows with > 10% STV exceedance rate: ", nrow(exceedSTVrate)),
                                associatedDecisionData = list(rawAnalysisForOutput) ) %>%
                           rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE)) %>%  # fix names to match station table format
                           rename_with( ~ gsub(".", "_", .x, fixed = TRUE)) ) # special step to get around accidentally replacing _GM with station table name
                  
                } else {# STV exceedance(s) occured in windows with < 10 samples
                
                # 1 hit in one or multiple 90-day periods after verifying geomean passes where applicable
                return(tibble(StationID = unique(validForAssessment$StationID),
                              `_EXC` = nrow(exceedSTVn), # right now this is set to # total STV exceedances, not the # STV exceedances in a 90-day period with 10+ samples
                              `_SAMP` = nrow(nSamples), 
                              `_GM.EXC` = nrow(exceedGeomean),
                              `_GM.SAMP` = nrow(filter(validForAssessment, !is.na(`Geomean In Window`))),
                              `_STAT` = "O",
                              `_STAT_VERBOSE` = "Fully Supporting - No geomean exceedances and only 1 STV exceedance in one or multiple 90-day periods represented by < 10 samples.", # previous language: 1 STV hit in one or multiple 90-day periods with < 10 samples after verifying geomean passes where applicable.",
                              `BACTERIADECISION` = paste0(stationTableName, ": ",`_STAT_VERBOSE`),
                              `BACTERIASTATS` = paste0(stationTableName, ": Number of 90 day windows with > 10% STV exceedance rate: ", nrow(exceedSTVrate)),
                              associatedDecisionData = list(rawAnalysisForOutput) ) %>%
                         rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE)) %>%  # fix names to match station table format
                         rename_with( ~ gsub(".", "_", .x, fixed = TRUE)) ) # special step to get around accidentally replacing _GM with station table name
                } 
                }
          }
          
        } else {  # Do any of the 90-day periods of the assessment window represented in the dataset exceed the 10% STV Exceedance Rate? - No
          return(tibble(StationID = unique(validForAssessment$StationID),
                        `_EXC` = nrow(exceedSTVn), # right now this is set to # total STV exceedances, not the # STV exceedances in a 90-day period with 10+ samples
                        `_SAMP` = nrow(nSamples), 
                        `_GM.EXC` = nrow(exceedGeomean),
                        `_GM.SAMP` = nrow(filter(validForAssessment, !is.na(`Geomean In Window`))),
                        `_STAT` = "S",
                        `_STAT_VERBOSE` = "Fully Supporting - No STV exceedance rates >10% or geomean exceedances in any 90-day period represented by 10+ samples.", #No STV exceedances or geomean exceedances in any 90-day period.",
                        `BACTERIADECISION` = paste0(stationTableName, ": ",`_STAT_VERBOSE`),
                        `BACTERIASTATS` = paste0(stationTableName, ": Number of 90 day windows with > 10% STV exceedance rate: ", nrow(exceedSTVrate)),
                        associatedDecisionData = list(rawAnalysisForOutput) ) %>%
                   rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE)) %>%  # fix names to match station table format
                   rename_with( ~ gsub(".", "_", .x, fixed = TRUE)) ) # special step to get around accidentally replacing _GM with station table name
          }
        
      } else { # Do the geometric means calculated for the 90-day periods represented by 10+ samples meet the GM criterion? - No
        return(tibble(StationID = unique(validForAssessment$StationID),
                      `_EXC` = nrow(exceedSTVn), # right now this is set to # total STV exceedances, not the # STV exceedances in a 90-day period with 10+ samples
                      `_SAMP` = nrow(nSamples), 
                      `_GM.EXC` = nrow(exceedGeomean),
                      `_GM.SAMP` = nrow(filter(validForAssessment, !is.na(`Geomean In Window`))),
                      `_STAT` = "IM",
                      `_STAT_VERBOSE` = "Impaired- geomean exceedance in any 90-day period.", #geomean exceedance(s) in any 90-day period with >= 10 samples.",
                      `BACTERIADECISION` = paste0(stationTableName, ": ",`_STAT_VERBOSE`),
                      `BACTERIASTATS` = paste0(stationTableName, ": Number of 90 day windows with > 10% STV exceedance rate: ", nrow(exceedSTVrate)),
                      associatedDecisionData = list(rawAnalysisForOutput) ) %>%
                 rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE)) %>%  # fix names to match station table format
                 rename_with( ~ gsub(".", "_", .x, fixed = TRUE)) ) # special step to get around accidentally replacing _GM with station table name
        }
      
    } else { # Were at least 10 samples taken within any 90-day period of the assessment window? - No
      # Were there any hits of the STV during the dataset?
      if( nrow(exceedSTVn) == 0){ # Were there any hits of the STV during the dataset? - No
        return(tibble(StationID = unique(validForAssessment$StationID),
                      `_EXC` = nrow(exceedSTVn), # right now this is set to # total STV exceedances, not the # STV exceedances in a 90-day period with 10+ samples
                      `_SAMP` = nrow(nSamples), 
                      `_GM.EXC` = as.numeric(NA), #nrow(exceedGeomean), # Data Entry manual updated to require NA instead of 0 if < 10 samples per 90 day window
                      `_GM.SAMP` = as.numeric(NA), #nrow(filter(validForAssessment, !is.na(`Geomean In Window`))), # Data Entry manual updated to require NA instead of 0 if < 10 samples per 90 day window
                      `_STAT` = "IN", # is this the right code???
                      `_STAT_VERBOSE` = "Insufficient Information (Prioritize for follow up monitoring)- No STV exceedances but insufficient data to analyze geomean.", #0 STV hits but insufficient data to analyze geomean.",
                      `BACTERIADECISION` = paste0(stationTableName, ": ",`_STAT_VERBOSE`),
                      `BACTERIASTATS` = paste0(stationTableName, ": Number of 90 day windows with > 10% STV exceedance rate: ", nrow(exceedSTVrate)),
                      associatedDecisionData = list(rawAnalysisForOutput) ) %>%
                 rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE))%>%  # fix names to match station table format
                 rename_with( ~ gsub(".", "_", .x, fixed = TRUE)) ) # special step to get around accidentally replacing _GM with station table name
        } else { # Were there any hits of the STV during the dataset? - Yes
          # 2 or more hits in the same 90-day period
          if(any(validForAssessment$`STV Exceedances In Window` >= 2) ){
            return(tibble(StationID = unique(validForAssessment$StationID),
                          # not quite right yet
                          `_EXC` = nrow(exceedSTVn), # right now this is set to # total STV exceedances, not the number of STV exceedances in a 90-day period with 10+ samples
                          `_SAMP` = nrow(nSamples), 
                          `_GM.EXC` = as.numeric(NA), #nrow(exceedGeomean), # Data Entry manual updated to require NA instead of 0 if < 10 samples per 90 day window
                          `_GM.SAMP` = as.numeric(NA), #nrow(filter(validForAssessment, !is.na(`Geomean In Window`))), # Data Entry manual updated to require NA instead of 0 if < 10 samples per 90 day window
                          `_STAT` = "IM", # is this the right code???
                          `_STAT_VERBOSE` = "Impaired - 2 or more STV hits in the same 90-day period with < 10 samples.",
                          `BACTERIADECISION` = paste0(stationTableName, ": ",`_STAT_VERBOSE`),
                          `BACTERIASTATS` = paste0(stationTableName, ": Number of 90 day windows with > 10% STV exceedance rate: ", nrow(exceedSTVrate)),
                          associatedDecisionData = list(rawAnalysisForOutput) ) %>%
                     rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE))%>%  # fix names to match station table format
                     rename_with( ~ gsub(".", "_", .x, fixed = TRUE)) ) # special step to get around accidentally replacing _GM with station table name
          } else { 
            # 1 hit in one or multiple 90-day periods
            return(tibble(StationID = unique(validForAssessment$StationID),
                          `_EXC` = nrow(exceedSTVn), # right now this is set to # total STV exceedances, not the # STV exceedances in a 90-day period with 10+ samples
                          `_SAMP` = nrow(nSamples), 
                          `_GM.EXC` = as.numeric(NA), #nrow(exceedGeomean), # Data Entry manual updated to require NA instead of 0 if < 10 samples per 90 day window
                          `_GM.SAMP` = as.numeric(NA), #nrow(filter(validForAssessment, !is.na(`Geomean In Window`))), # Data Entry manual updated to require NA instead of 0 if < 10 samples per 90 day window
                          `_STAT` = "IN", # is this the right code???
                          `_STAT_VERBOSE` = "Insufficient Information (Prioritize for follow up monitoring)- One STV exceedance in one or multiple 90-day periods but insufficient data to analyze geomean.",#1 STV hit in one or multiple 90-day periods but insufficient data to analyze geomean.",
                          `BACTERIADECISION` = paste0(stationTableName, ": ",`_STAT_VERBOSE`),
                          `BACTERIASTATS` = paste0(stationTableName, ": Number of 90 day windows with > 10% STV exceedance rate: ", nrow(exceedSTVrate)),
                          associatedDecisionData = list(rawAnalysisForOutput) ) %>%
                     rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE)) %>%  # fix names to match station table format
                     rename_with( ~ gsub(".", "_", .x, fixed = TRUE)) ) # special step to get around accidentally replacing _GM with station table name
            }
        }
      }
    # No bacteria data to analyze
    } else {
      return(tibble(StationID = unique(stationData$FDT_STA_ID),
                    `_EXC` = NA, # right now this is set to # total STV exceedances, not the # STV exceedances in a 90-day period with 10+ samples
                    `_SAMP` = NA, 
                    `_GM.EXC` = NA,
                    `_GM.SAMP` = NA,
                    `_STAT` = NA, # is this the right code???
                    `_STAT_VERBOSE` = NA, 
                    `BACTERIADECISION` = NA,
                    `BACTERIASTATS` = NA,
                    associatedDecisionData = list(NA)) %>%
               rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE)) %>%  # fix names to match station table format
               rename_with( ~ gsub(".", "_", .x, fixed = TRUE)) ) # special step to get around accidentally replacing _GM with station table name
    }
  }

# To get just info for station table  
#xxx <- bacteriaAssessmentDecision(stationData, 'ECOLI', 'LEVEL_ECOLI', 10, 410, 126) %>%
#  dplyr::select(StationID:ECOLI_STAT)
#xxx <- bacteriaAssessmentDecision(stationData, 'ENTEROCOCCI', 'LEVEL_ENTEROCOCCI', 10, 130, 35) %>%
#  dplyr::select(StationID:ENTER_STAT)




## outermost function to decide which bacteria should be assessed based on WQS Class
bacteriaAssessmentDecisionClass <- function(stationData){ # input dataframe with bacteria data
  uniqueStationName <- unique(stationData$FDT_STA_ID) # just in case
  
  # # quick out if all bacteria data level II or I
  # if(any( c('CMON', 'NONA') %in% unique(dplyr::select(stationData, contains('TYPE_')) %>% summarize(unique(.)) %>%
  #                                       pivot_longer(cols = everything(), names_to = 'name', values_to = 'value') %>% pull(value)) ) ){
  #   # quick out if all bacteria data level II or I
  #   if(all(c(filter(stationData, !is.na(LEVEL_ECOLI)) %>% dplyr::select(LEVEL_ECOLI) %>% pull(),
  #            filter(stationData, !is.na(LEVEL_ENTEROCOCCI)) %>% dplyr::select(LEVEL_ENTEROCOCCI) %>% pull() )  %in% c('Level II', 'Level I')) ){
  #     return(
  #       tibble(StationID = z, ECOLI_EXC = as.numeric(NA), ECOLI_SAMP = as.numeric(NA), ECOLI_GM_EXC = as.numeric(NA), ECOLI_GM_SAMP = as.numeric(NA),
  #              ECOLI_STAT = as.character(NA), ECOLI_STATECOLI_VERBOSE = as.character(NA),
  #              ENTER_EXC = as.numeric(NA), ENTER_SAMP = as.numeric(NA), ENTER_GM_EXC = as.numeric(NA), ENTER_GM_SAMP = as.numeric(NA),
  #              ENTER_STAT = as.character(NA), ENTER_STATENTER_VERBOSE = as.character(NA)) ) }
  #   if(any(c(filter(stationData, !is.na(LEVEL_ECOLI)) %>% dplyr::select(LEVEL_ECOLI) %>% pull(),
  #            filter(stationData, !is.na(LEVEL_ENTEROCOCCI)) %>% dplyr::select(LEVEL_ENTEROCOCCI) %>% pull() )  %in% c('Level III')) ){
  #     # run both bacteria methods if level III data exists to be most inclusive
  #     return(
  #       left_join(bacteriaAssessmentDecision(stationData, 'ECOLI', 'LEVEL_ECOLI', 10, 410, 126), 
  #                 bacteriaAssessmentDecision(stationData, 'ENTEROCOCCI', 'LEVEL_ENTEROCOCCI', 10, 130, 35), by = 'StationID') %>% 
  #         dplyr::select(StationID, ECOLI_EXC, ECOLI_SAMP, ECOLI_GM_EXC, ECOLI_GM_SAMP, ECOLI_STAT, ECOLI_STATECOLI_VERBOSE,
  #                       ENTER_EXC, ENTER_SAMP, ENTER_GM_EXC, ENTER_GM_SAMP, ENTER_STAT, ENTER_STATENTER_VERBOSE) ) } }
    
    
  # stop everything if no data to analyze in stationData
  if(nrow(stationData) == 0){
    return(
      tibble(StationID = uniqueStationName, ECOLI_EXC = as.numeric(NA), ECOLI_SAMP = as.numeric(NA), ECOLI_GM_EXC = as.numeric(NA), ECOLI_GM_SAMP = as.numeric(NA),
             ECOLI_STAT = as.character(NA), ECOLI_STATECOLI_VERBOSE = as.character(NA),
             ENTER_EXC = as.numeric(NA), ENTER_SAMP = as.numeric(NA), ENTER_GM_EXC = as.numeric(NA), ENTER_GM_SAMP = as.numeric(NA),
             ENTER_STAT = as.character(NA), ENTER_STATENTER_VERBOSE = as.character(NA)) )}
  
  # lake stations should only be surface sample
  if(unique(stationData$lakeStation) == TRUE){
    stationData <- filter(stationData, FDT_DEPTH <= 0.3) }
  
  if(nrow(stationData) > 0){
    if(unique(stationData$CLASS) %in% c('I', 'II')){
      return(
        left_join(bacteriaAssessmentDecision(stationData, 'ECOLI', 'LEVEL_ECOLI', 10, 410, 126),
                  bacteriaAssessmentDecision(stationData, 'ENTEROCOCCI', 'LEVEL_ENTEROCOCCI', 10, 130, 35),
                  by = 'StationID') %>% 
          mutate(BACTERIADECISION = paste0(BACTERIADECISION.x, ' | ', BACTERIADECISION.y),
                 BACTERIASTATS = paste0(BACTERIASTATS.x, ' | ', BACTERIASTATS.y)) %>% 
          dplyr::select(-c(BACTERIADECISION.x, BACTERIADECISION.y, BACTERIASTATS.x, BACTERIASTATS.y,
                           associatedDecisionData.x, associatedDecisionData.y)) )
      # previously this was programmed to only output enterococci results for these classes, but assessors requested both outputs to be conservative for
      # transitional sites
        # bacteriaAssessmentDecision(stationData, 'ENTEROCOCCI', 'LEVEL_ENTEROCOCCI', 10, 130, 35) %>%
        #   mutate(ECOLI_EXC = as.numeric(NA), ECOLI_SAMP = as.numeric(NA), ECOLI_GM_EXC = as.numeric(NA), ECOLI_GM_SAMP = as.numeric(NA),
        #          ECOLI_STAT = as.character(NA), ECOLI_STATECOLI_VERBOSE = as.character(NA)) %>%
        #   dplyr::select(StationID, ECOLI_EXC, ECOLI_SAMP, ECOLI_GM_EXC, ECOLI_GM_SAMP, ECOLI_STAT, ECOLI_STATECOLI_VERBOSE, ENTER_EXC, 
        #                 ENTER_SAMP, ENTER_GM_EXC, ENTER_GM_SAMP, ENTER_STAT, ENTER_STATENTER_VERBOSE, BACTERIADECISION, BACTERIASTATS) )
    } else {
      return(
        bacteriaAssessmentDecision(stationData, 'ECOLI', 'LEVEL_ECOLI', 10, 410, 126) %>%
          dplyr::select(StationID:BACTERIASTATS) %>% #ECOLI_STATECOLI_VERBOSE) %>%
          mutate(ENTER_EXC = as.numeric(NA), ENTER_SAMP = as.numeric(NA), ENTER_GM_EXC = as.numeric(NA), ENTER_GM_SAMP = as.numeric(NA),
                 ENTER_STAT = as.character(NA), ENTER_STATENTER_VERBOSE = as.character(NA)) ) }
  } else {
    return(
      tibble(StationID = uniqueStationName, ECOLI_EXC = as.numeric(NA), ECOLI_SAMP = as.numeric(NA), ECOLI_GM_EXC = as.numeric(NA), ECOLI_GM_SAMP = as.numeric(NA),
             ECOLI_STAT = as.character(NA), ECOLI_STATECOLI_VERBOSE = as.character(NA),
             ENTER_EXC = as.numeric(NA), ENTER_SAMP = as.numeric(NA), ENTER_GM_EXC = as.numeric(NA), ENTER_GM_SAMP = as.numeric(NA),
             ENTER_STAT = as.character(NA), ENTER_STATENTER_VERBOSE = as.character(NA)) )}
}
#bacteriaAssessmentDecisionClass(stationData)
#bacteriaAssessmentDecisionClass(stationData)
