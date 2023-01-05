#previousIRassessmentDecisions <- 
IR2020assessmentDecisions <- read_excel('data/BioassessmentRegionalResultsIR2020.xlsx') # not fully filled out but will help the bios who participated last cycle
pinnedDecisions <- pin_get('IR2022bioassessmentDecisions_test', board = 'rsconnect') %>%
  dplyr::select(IRYear:FinalAssessmentRating)
glimpse(pinnedDecisions)
glimpse(IR2020assessmentDecisions)


eco4 <- st_read('C:/HardDriveBackup/R/GitHub/BenthicDataQueryApp/data/GIS/VA_level4ecoregion_WGS84.shp') %>% 
  st_drop_geometry() %>% 
  dplyr::select(US_L3NAME, US_L4CODE)

oldCleanup <- dplyr::select(IR2020assessmentDecisions, IRYear, StationID, CollectorID, Region, EcoRegion,
                            AssessmentMethod, Reviewer, SeasonalDifferences, SummaryAssessment,
                            FactorsWatershed, RecentChange, FinalAssessmentRating) %>% 
  filter(!is.na(Reviewer)) %>% 
  left_join(eco4, by = c('EcoRegion' = 'US_L4CODE')) %>% 
  mutate(EcoRegion = US_L3NAME) %>% 
  dplyr::select(-US_L3NAME) %>% 
  group_by(IRYear, StationID) %>% 
  mutate(n = 1:n()) %>% 
  filter(n == 1) %>% 
  dplyr::select(-n)


previousIRassessmentDecisions <- bind_rows(pinnedDecisions,
                                           oldCleanup )
glimpse(previousIRassessmentDecisions)

# pin to server
pin(previousIRassessmentDecisions, name = 'PreviousIRbioassessmentDecisions',
    description = 'This dataset archives bioassessment decisions from previous cycles in one standardized format for use in the IR tools.',
    board = 'rsconnect')

# pin blank dataset to server for current assessment cycle
currentIRassessmentDecisions <- previousIRassessmentDecisions[1,] %>% 
  mutate(IRYear = NA_integer_,
         StationID = NA_character_,
         StreamName = NA_character_,
         CollectorID = NA_character_,
         Region = NA_character_,
         EcoRegion = NA_character_,
         AssessmentMethod = NA_character_,
         Reviewer = NA_character_,
         SeasonalDifferences = NA_character_, 
         SummaryAssessment = NA_character_, 
         FactorsWatershed = NA_character_, 
         RecentChange = NA_character_, 
         FinalAssessmentRating = NA_character_)

# double check it will work:
#bind_rows(currentIRassessmentDecisions, previousIRassessmentDecisions[1:10,])

pin(currentIRassessmentDecisions, name = 'CurrentIRbioassessmentDecisions',
    description = 'This dataset is the working dataset of bioassessment decisions for the current IR cycle using one standardized format for use in the IR tools.',
    board = 'rsconnect')
