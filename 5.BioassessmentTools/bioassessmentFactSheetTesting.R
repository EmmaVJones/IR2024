
source('global.R')


previousIRassessmentDecisions <- pin_get('ejones/PreviousIRbioassessmentDecisions', board = 'rsconnect')


# IR Bioassessment Tab

## User just wants to generate a fact sheet from a specific IR window

inputFile <- NULL

if(is.null(inputFile)){
  pinnedDecisions <- bind_rows(previousIRassessmentDecisions,
                               pin_get('ejones/CurrentIRbioassessmentDecisions', board = 'rsconnect'))
} else {
  pinCheck('ejones/CurrentIRbioassessmentDecisions', validInputData) 
  # pull new pin
  pinnedDecisions <- pin_get('ejones/CurrentIRbioassessmentDecisions', board = 'rsconnect')
  pinnedDecisions <- bind_rows(previousIRassessmentDecisions, 
                               pinnedDecisions)
} 

# User chooses a window
userIRwindows <- sort(unique(pinnedDecisions$IRYear), decreasing = TRUE)[1]

# user chooses a station
stations <- filter(pinnedDecisions, IRYear %in% userIRwindows) %>% 
  distinct(StationID) %>% 
  arrange(StationID) %>% 
  pull(StationID)

userStations <- stations[1]#'2-JKS023.61'#stations[1]

# pull data for user selection
assessmentDecision_UserSelection <- filter(pinnedDecisions, StationID %in% userStations &
                                             IRYear %in% userIRwindows) 

SCI_UserSelection <- filter(VSCIresultsAll, StationID %in% filter(assessmentDecision_UserSelection, AssessmentMethod == 'VSCI')$StationID) %>%
  bind_rows(filter(VCPMI63resultsAll, StationID %in% filter(assessmentDecision_UserSelection, AssessmentMethod == 'VCPMI63 + Chowan')$StationID)  ) %>%
  bind_rows(
    filter(VCPMI65resultsAll, StationID %in% filter(assessmentDecision_UserSelection, AssessmentMethod == 'VCPMI65 - Chowan')$StationID)  ) %>%
  filter(between(`Collection Date`, filter(assessmentPeriodLookup, IRYear %in% userIRwindows)$PeriodStart,
                 filter(assessmentPeriodLookup, IRYear %in% userIRwindows)$PeriodEnd)) %>%# limit data to assessment window of interest
  filter(RepNum %in% c('1', '2')) %>% # drop QA and wonky rep numbers
  filter(`Target Count` == 110) %>% # only assess rarified data
  filter(Gradient != "Boatable") %>%  # don't assess where no SCI not validated
  # add back in description information
  left_join(filter(benSampsAll, StationID %in% userStations) %>%
              dplyr::select(StationID, Sta_Desc, BenSampID,US_L3CODE, US_L3NAME, HUC_12, VAHU6, Basin, Basin_Code),
            by = c('StationID', 'BenSampID')) %>%
  dplyr::select(StationID, Sta_Desc, BenSampID, `Collection Date`, RepNum, everything())

habSampsIR <- filter(habSampsAll, between(`Collection Date`, 
                                        filter(assessmentPeriodLookup, IRYear %in% userIRwindows)$PeriodStart, 
                                        filter(assessmentPeriodLookup, IRYear %in% userIRwindows)$PeriodEnd))# limit data to assessment window
habValuesIR <- filter(habValuesAll, HabSampID %in% habSampsIR$HabSampID)
habitatUserSelection <- habitatConsolidation( userStations, habSampsIR, habValuesIR)  


# have to make separate reactive object in order to send appropriate station name to the download title
fileNameForReport <- paste("IR",assessmentCycle," ", as.character(unique(userStations))," Benthic Assessment Fact Sheet.html", sep = "")





## User uploads decisions and then decides to make fact sheet

inputFile <- read_excel("data/BioassessmentRegionalResultsTemplate_EVJtest.xlsx")

# Validate stations
userUploadCheck <- stationValidation(inputFile)

validInputData <- userUploadCheck$validStations
userUploadCheck$invalidStations

if(is.null(inputFile)){
  pinnedDecisions <- bind_rows(previousIRassessmentDecisions,
                               pin_get('ejones/CurrentIRbioassessmentDecisions', board = 'rsconnect'))
} else {
  pinCheck('ejones/CurrentIRbioassessmentDecisions', validInputData) 
  # pull new pin
  pinnedDecisions <- pin_get('ejones/CurrentIRbioassessmentDecisions', board = 'rsconnect')
  pinnedDecisions <- bind_rows(previousIRassessmentDecisions, 
                               pinnedDecisions)
} 

# User chooses a window
userIRwindows <- sort(unique(pinnedDecisions$IRYear))[3]

# user chooses a station
stations <- filter(pinnedDecisions, IRYear %in% userIRwindows) %>% 
  distinct(StationID) %>% 
  arrange(StationID) %>% 
  pull(StationID)

userStations <- stations[1]

# pull data for user selection
assessmentDecision_UserSelection <- filter(pinnedDecisions, StationID %in% userStations &
                                             IRYear %in% userIRwindows) 

SCI_UserSelection <- filter(VSCIresultsAll, StationID %in% filter(assessmentDecision_UserSelection, AssessmentMethod == 'VSCI')$StationID) %>%
  bind_rows(filter(VCPMI63resultsAll, StationID %in% filter(assessmentDecision_UserSelection, AssessmentMethod == 'VCPMI63 + Chowan')$StationID)  ) %>%
  bind_rows(
    filter(VCPMI65resultsAll, StationID %in% filter(assessmentDecision_UserSelection, AssessmentMethod == 'VCPMI65 - Chowan')$StationID)  ) %>%
  filter(between(`Collection Date`, filter(assessmentPeriodLookup, IRYear %in% userIRwindows)$PeriodStart,
                 filter(assessmentPeriodLookup, IRYear %in% userIRwindows)$PeriodEnd)) %>%# limit data to assessment window of interest
  filter(RepNum %in% c('1', '2')) %>% # drop QA and wonky rep numbers
  filter(`Target Count` == 110) %>% # only assess rarified data
  filter(Gradient != "Boatable") %>%  # don't assess where no SCI not validated
  # add back in description information
  left_join(filter(benSampsAll, StationID %in% userStations) %>%
              dplyr::select(StationID, Sta_Desc, BenSampID,US_L3CODE, US_L3NAME, HUC_12, VAHU6, Basin, Basin_Code),
            by = c('StationID', 'BenSampID')) %>%
  dplyr::select(StationID, Sta_Desc, BenSampID, `Collection Date`, RepNum, everything())

habSampsIR <- filter(habSampsAll, between(`Collection Date`, 
                                          filter(assessmentPeriodLookup, IRYear %in% userIRwindows)$PeriodStart, 
                                          filter(assessmentPeriodLookup, IRYear %in% userIRwindows)$PeriodEnd))# limit data to assessment window
habValuesIR <- filter(habValuesAll, HabSampID %in% habSampsIR$HabSampID)
habitatUserSelection <- habitatConsolidation( userStations, habSampsIR, habValuesIR)  








# Get data from bio
IR2020 <- read_excel('data/BioassessmentRegionalResultsIR2020.xlsx') %>%
  filter(!is.na(FinalAssessmentRating))

###########IR2022 <- read_excel('data/BioassessmentRegionalResultsIR2022test.xlsx')
#IR2022 <- read_excel('data/BioassessmentRegionalResultsIR2022test.xlsx')
# Brett data test
#IR2022 <- read_excel('data/BioassessmentRegionalResultsTemplate_2022_BDS.xlsx')
#IR2022 <- read_excel('data/BioassessmentRegionalResultsTemplate_2022_BDS_EVJ.xlsx') # removed duplicate row
IR2022 <- read_excel('data/BioassessmentRegionalResultsTemplate_2022_BDS_Final.xlsx')

# check against pinned data, overwrite if necessary
# this is the original pin in case it needs to be reset during testing
#pin(IR2022[1:3,], name = 'IR2022bioassessmentDecisions_test', description = 'Test dataset for developing IR2022 bioassessment fact sheet tool', board = 'rsconnect') # original pin

# original list of what's available on the server
#OGpinnedDecisions <- pin_get('IR2022bioassessmentDecisions_test', board = 'rsconnect')

# what user uploads
userUpload <- IR2022#[2:5,]
userUploadFail <- mutate(userUpload, StationID = case_when(StationID == "2-CAT026.29" ~ "2-CAT026.00", 
                                                           TRUE ~ as.character(StationID)))

# make sure all uploaded stations are valid
userUploadCheck <- stationValidation(userUpload)#Fail)

userUploadValid <- userUploadCheck$validStations#stationValidation(userUploadFail)

# display any stations that cannot undergo more work
invalidInputData <- userUploadCheck$invalidStations#filter(userUpload, ! StationID %in% userUploadValid$StationID)

DT::datatable(mutate(userUpload, Validated = case_when(StationID %in% invalidInputData$StationID ~ FALSE,
                                                       TRUE ~ TRUE), # for pretty viz
                     ValidatedColor = case_when(StationID %in% invalidInputData$StationID ~ 0,
                                                TRUE ~ 1)) %>% # for color coding with datatables, styleEqual needs numeric not boolean
                dplyr::select(Validated, everything()) %>% 
                arrange(Validated),
              escape=F, rownames = F, options=list(scrollX = TRUE, scrollY = "800px",pageLength=nrow(userUpload),
                                                   columnDefs = list(list(visible=FALSE, targets=14)))) %>%
  formatStyle('StationID', 'ValidatedColor', backgroundColor = styleEqual(c(0, 1), c('yellow',NA))  )

pinCheck('IR2022bioassessmentDecisions_test', userUploadValid) # can change to real deal pin in time
