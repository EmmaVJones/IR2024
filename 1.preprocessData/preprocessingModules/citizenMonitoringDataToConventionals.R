# This script takes the citizen monitoring data provided by Reid and ensures it is in the same schema as 
# conventionals for automated assessment purposes



# first, we need FDT_STA_ID filled out for each station
stationOptions <- readxl::read_excel('data/CitizenNonAgencyStations.xlsx',
                                     sheet = 'Citizen Monitoring') %>% 
  bind_rows(readxl::read_excel('data/CitizenNonAgencyStations.xlsx',
                               sheet = 'NonAgency'))


# Fix any available station names
citmonNonAgency0 <- citmonNonAgency %>% 
  filter(is.na(FDT_STA_ID) | FDT_STA_ID== '') %>% 
  # elaborate way to run distinct without losing sites
  group_by(FDT_STA_ID, GROUP_STA_ID) %>% 
  mutate(n = 1:n()) %>% 
  dplyr::select(n, everything())


citmonNonAgencyLookup <- citmonNonAgency0 %>% 
  dplyr::select(n, FDT_STA_ID:Deq_Region, Data_Source, Latitude, Longitude) %>% 
  filter(n == 1) %>% 
  left_join(stationOptions, by = c('GROUP_STA_ID' = 'Group Station ID')) %>% 
  ungroup() %>% group_by(FDT_STA_ID, GROUP_STA_ID) %>% 
  mutate(n = 1:n()) %>% 
  dplyr::select(n, everything())

issues <- filter(citmonNonAgencyLookup, GROUP_STA_ID %in% filter(citmonNonAgencyLookup, n >1)$GROUP_STA_ID)

citmonNonAgencyLookup <- citmonNonAgencyLookup %>% 
  mutate(FDT_STA_ID = case_when(GROUP_STA_ID == 'ACB.RUSRIV5.4'  ~ '3RUS-5.4-ALL',
                                TRUE ~ as.character(`DEQ Station ID`))) %>% 
  filter(n == 1) # get rid of extra

# this site is the only issue, can join on lookup if this site is fixed

citmonNonAgencyFine <- citmonNonAgency %>% 
  filter(! is.na(FDT_STA_ID)) %>% 
  filter(FDT_STA_ID != '') 

citmonNonAgencyFixed <- citmonNonAgency %>% 
  filter(is.na(FDT_STA_ID) | FDT_STA_ID== '') %>% 
  left_join(dplyr::select(citmonNonAgencyLookup, FDT_STA_ID, GROUP_STA_ID, `DEQ Station ID`), 
            by =c( 'GROUP_STA_ID')) %>% 
  dplyr::select(FDT_STA_ID.x, FDT_STA_ID.y, GROUP_STA_ID, `DEQ Station ID`, everything()) %>% 
  mutate(FDT_STA_ID.x = ifelse(FDT_STA_ID.x =='', NA, FDT_STA_ID.x),
         FDT_STA_ID.y = ifelse(FDT_STA_ID.y =='', NA, FDT_STA_ID.y)) %>% 
  mutate(FDT_STA_ID = coalesce(FDT_STA_ID.x, FDT_STA_ID.y,`DEQ Station ID`)) %>% 
  dplyr::select(FDT_STA_ID, everything()) %>% 
  dplyr::select(names(citmonNonAgency)) %>% 
  # add back in data that was fine to begin with
  bind_rows(citmonNonAgencyFine)

# double check same size as original
nrow(citmonNonAgencyFixed)== nrow(citmonNonAgency)
  

# There are still a number of sites that don't have FDT_STA_ID, smash the entire dataset
# together and then write to csv for manual fixing
#write.csv(citmonNonAgencyFixed, 'data/draftData/CMC_FOSR_19_20_EVJ02232023.csv', row.names = F, na='')

# not making headway manually. going to export to csv for assessor review

forReview <- citmonNonAgencyFixed %>% 
  filter(is.na(FDT_STA_ID)) %>% 
  distinct(GROUP_STA_ID, .keep_all = T) %>% 
  dplyr::select(FDT_STA_ID:Deq_Region, Latitude, Longitude, Data_Source) %>% 
  mutate(WQS_ID= NA_character_,
         `WQS_ID Comments` = NA_character_,
         ID305B_1= NA_character_,
         ID305B_2= NA_character_,
         ID305B_3= NA_character_,
         ID305B_4= NA_character_,
         ID305B_5= NA_character_,
         ID305B_6= NA_character_,
         ID305B_7= NA_character_,
         ID305B_8= NA_character_,
         ID305B_9= NA_character_,
         ID305B_10= NA_character_)

# #write.csv(forReview, 'data/draftData/REVIEW_NEEDED_CMC_FOSR_19_2002232023.csv', row.names = F, na='')
# This was converted to a .xlsx named  'data/draftData/REVIEW_NEEDED_CMC_FOSR_19_2002232023.xlsx' and placed in onedrive/WQA
# for review by each regional assessor.


# in the meantime moving forward with just stations that have appropriate station information


citmonNonAgency <- filter(citmonNonAgencyFixed, ! is.na(FDT_STA_ID))
rm(list=setdiff(ls(), c( "citmonNonAgency", 'conventionals', 'missingCoordinates', 'distinctSites_sf',
                'conn', 'distinctSites')))



# conventionals reorganization bit

conventionalsSchema <- pin_get('ejones/conventionals2024draft', board = 'rsconnect')

names(citmonNonAgency) == names(conventionalsSchema)


citmonNonAgency1 <- citmonNonAgency %>% 
  rename(VADEQ_ADMIN_REGION = Deq_Region,
         MONITORING_REGION = STA_REC_CODE,
         `NITRITE+NITRATE_TOTAL_00630_mg_L` = NITRITE.NITRATE_TOTAL_00630_mg_L,
         `NITRITE+NITRATE_DISSOLVED_00631_mg_L` = NITRITE.NITRATE_DISSOLVED_00631_mg_L, 
         RMK_HARDNESS_TOTAL = RMK_00900,
         LEVEL_HARDNESS_TOTAL = LEVEL_00900
  ) %>% 
  mutate(OTHER_CITMON_NONAGENCY_INFO = NA_character_,   # nothing here yet but maybe next version
         # fix datetime
         FDT_DATE_TIME = as.POSIXct(FDT_DATE_TIME, format = '%m/%d/%Y %H:%M')
         
  ) %>%
  # adjust level system
  mutate_if(is.integer, as.character) %>%
  mutate_if(is.logical, as.character) %>%
  #mutate(across(starts_with("LEVEL_"), levelChange))
  mutate(across(starts_with("LEVEL_"),
                ~case_when(. == '3' ~ 'Level III',
                           . == '2' ~ 'Level II',
                           . == '1' ~ 'Level I',
                           is.na(.) ~ .))) %>% 
  # correct issues with data formats so these will smash together
  mutate_at(c('FDT_PERCENT_FRB', 'DISSOLVED_OXYGEN_00300_mg_L', 'DISSOLVED_OXYGEN_DOOPT_mg_L',
              'DISSOLVED_OXYGEN_WINK_mg_L', 'NOX_mg_L', 'NITROGEN_TOTAL_00600_mg_L', 
              'NITROGEN_AMMONIA_DISSOLVED_00608_mg_L', 'NITROGEN_AMMONIA_TOTAL_00610_mg_L', 
              'NITROGEN_NITRITE_DISSOLVED_00613_mg_L','NITROGEN_NITRITE_TOTAL_00615_mg_L',
              'NITROGEN_NITRATE_DISSOLVED_00618_mg_L', 'NITROGEN_NITRATE_TOTAL_00620_mg_L',
              'NITROGEN_KJELDAHL_TOTAL_00625_mg_L', 'NITRITE+NITRATE_DISSOLVED_00631_mg_L',
              'NITROGEN_PARTICULATE_49570_mg_L', 'NITROGEN_TOTAL_DISSOLVED_49571_mg_L', 
              'NITROGEN_TOTAL_DISSOLVED_TDNLF_mg_L', 'PHOSPHORUS_TOTAL_00665_mg_L',
              'PHOSPHORUS_DISSOLVED_00666_mg_L', 'PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg_L',
              'PHOSPHOROUS_PARTICULATE_49567_mg_L', 'PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L', 
              'ORTHOPHOSPHATE_DISSOLVED_OPWLF_mg_L', 'PHOSPHORUS_SUSPENDED_INORGANIC_PIPLF_mg_L',
              'PHOSPHORUS_PARTICULATE_PPWLF_mg_L', 'PHOSPHORUS_TOTAL_DISSOLVED_TDPLF_mg_L', 
              'HARDNESS_TOTAL_00900_mg_L', 'CHLORIDE_mg_L', 'CHLORIDE_TOTAL_00940_mg_L', 
              'CHLORIDE_DISSOLVED_00941_mg_L', 'SULFATE_mg_L', 'SULFATE_TOTAL_mg_L', 
              'SULFATE_DISS_mg_L', 'ECOLI_31648_NO_100mL', 'ENTEROCOCCI', 'FECAL_COLI',
              'TOTAL_SUSPENDED_SOLIDS_00530_mg_L', 'SSC_mg_L', 'TOTAL_SUSPENDED_SOLIDS_TSS45_mg_L'),
            as.numeric) %>% 
  mutate_at(c('RMK_49567', 'RMK_PIPLF'),
            as.logical) %>%
  mutate_at(c("Huc6_Huc_12"),
            as.character) %>% 
  
  # new for IR 2024, no CBP name provided
  dplyr::select(-STA_CBP_NAME) %>% 
  # reorder columns to conventionals schema
  dplyr::select(names(conventionalsSchema))
         
# double checks
names(citmonNonAgency1)[!names(citmonNonAgency1) %in%  names(conventionalsSchema)]
names(conventionalsSchema)[!names(conventionalsSchema) %in%  names(citmonNonAgency1)]

names(citmonNonAgency1) == names(conventionalsSchema)



# combine into a single dataset
conventionalsSchema <- bind_rows(conventionalsSchema,
                                 citmonNonAgency1)

# pin this versions
pin(conventionalsSchema, name = 'ejones/conventionals2024draft',
    description = 'Working IR2024 conventionals dataset, EVJ reorganized from Roger 2/23/2023 with citmon with FDT_STA_ID from 2019-2020',
    board = 'rsconnect')

#totally clear environment except citmon we need to work through back in larger RMD
citmonNonAgency <- citmonNonAgency1

rm(list=setdiff(ls(), c('citmonNonAgency', 'conventionals', 'missingCoordinates', 'distinctSites_sf',
                'conn', 'distinctSites')))

