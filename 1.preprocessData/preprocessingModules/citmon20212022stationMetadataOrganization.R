# This script pulls the manually reviewed citmon 2021 2022 stations from CMC including WQS and AU information


library(tidyverse)
library(readxl)


# WQS Bring in data from server
WQStableExisting <- pin_get('ejones/citmonStationsWithWQSFinal', board = 'rsconnect')



# Bring in BRRO work
BRRO <- read_excel('C:/Users/wmu43954/OneDrive - Commonwealth of Virginia/WQA/ForReview.xlsx',
                   sheet = 'BRRO') %>% 
  dplyr::select(FDT_STA_ID, STA_DESC, Data_Source, GROUP_STA_ID, ASSESS_REG, VAHU6, BasinName, SUBBASIN,
                WQS_ID, `WQS Buffer Distance`, `WQS Comments`, GNIS_ID:OBJECTID, 
                ID305B_1, ID305B_2,  ID305B_3,  ID305B_4,  ID305B_5,  ID305B_6,  ID305B_7,  ID305B_8,  ID305B_9,  ID305B_10, 
                `AU Buffer Distance`, 
                Lacustrine, TYPE_1)

# Purposefully skipping CO because the only station that fell in CO AU polygon is actually a TRO site that has the wrong coordinates,
#  has no QAPP (below level 1), and is sampling a storm drain according to the station description, so for IR2024 we will not use data 
#  from that site

# Bring in NRO work
# note, there were a number of stations that Rebecca updated the coordinates in this spreadsheet. Coordinates need to be updated in
#  CMC to make it through to conventionals dataset, so the metadata for these updated coordinates are reflected in subsequent steps, but not
#  the changed location. It is up to the citizen groups to communicate their sampling locations, we are merely data end users
NRObad <- read_excel('C:/Users/wmu43954/OneDrive - Commonwealth of Virginia/WQA/ForReview.xlsx',
                   sheet = 'NRO') %>% 
  # first filter sites Rebecca doesn't want
  filter(str_detect(`WQS Comments`, 'EXCLUDE' ) |  str_detect(`WQS Comments`, 'Excluded') |
           str_detect(`WQS Comments`, 'EXLCUDE'))
# something is up with str_detect and this cell, had to do this removal process in two steps bc the negative of each statement
#  produced wonky results
NRO <- read_excel('C:/Users/wmu43954/OneDrive - Commonwealth of Virginia/WQA/ForReview.xlsx',
                     sheet = 'NRO') %>% 
  filter(! FDT_STA_ID %in% NRObad$FDT_STA_ID) %>% 
  dplyr::select(FDT_STA_ID, `NRO CORRECTED STATION ID`, STA_DESC, Data_Source, GROUP_STA_ID, ASSESS_REG, VAHU6, BasinName, SUBBASIN,
                WQS_ID, `WQS Buffer Distance`, `WQS Comments`, GNIS_ID:OBJECTID, 
                ID305B_1, ID305B_2,  ID305B_3,  ID305B_4,  ID305B_5,  ID305B_6,  ID305B_7,  ID305B_8,  ID305B_9,  ID305B_10, 
                `AU Buffer Distance`, 
                Lacustrine, TYPE_1) %>% 
  # Use Rebecca's updated FDT_STA_ID information where available
  mutate(FDT_STA_ID = ifelse(is.na(`NRO CORRECTED STATION ID`), FDT_STA_ID, `NRO CORRECTED STATION ID`)) %>% 
  dplyr::select(-`NRO CORRECTED STATION ID`)
rm(NRObad)

# add in difficult CMC sites
NROdiff <- read_excel('C:/Users/wmu43954/OneDrive - Commonwealth of Virginia/WQA/Difficult_CMC_Sites.xlsx',
                      sheet = 'NRO') %>% 
  filter(!is.na(FDT_STA_ID)) %>% 
  dplyr::select(FDT_STA_ID,  STA_DESC, Data_Source, GROUP_STA_ID, ASSESS_REG = Region, 
                WQS_ID, `WQS Comments`,
                ID305B_1, ID305B_2, TYPE_1, Lacustrine)



# Bring in PRO work
PRO <- read_excel('C:/Users/wmu43954/OneDrive - Commonwealth of Virginia/WQA/ForReview.xlsx',
                   sheet = 'PRO') %>% 
  dplyr::select(FDT_STA_ID, STA_DESC, Data_Source, GROUP_STA_ID, ASSESS_REG, VAHU6, BasinName, SUBBASIN,
                WQS_ID, `WQS Buffer Distance`, `WQS Comments`,GNIS_ID:OBJECTID, 
                ID305B_1, ID305B_2,  ID305B_3,  ID305B_4,  ID305B_5,  ID305B_6,  ID305B_7,  ID305B_8,  ID305B_9,  ID305B_10, 
                `AU Buffer Distance`, 
                Lacustrine, TYPE_1)

# add in difficult CMC sites
PROdiff <- read_excel('C:/Users/wmu43954/OneDrive - Commonwealth of Virginia/WQA/Difficult_CMC_Sites.xlsx',
                      sheet = 'PRO') %>% 
  filter(!is.na(FDT_STA_ID)) %>% 
  dplyr::select(FDT_STA_ID,  STA_DESC, Data_Source, GROUP_STA_ID, ASSESS_REG = Region, 
                WQS_ID, `WQS Comments`,
                ID305B_1, ID305B_2, TYPE_1, Lacustrine) 




# Bring in TRO work
TRO <- read_excel('C:/Users/wmu43954/OneDrive - Commonwealth of Virginia/WQA/ForReview.xlsx',
                  sheet = 'TRO') %>% 
  dplyr::select(FDT_STA_ID, STA_DESC, Data_Source, GROUP_STA_ID, ASSESS_REG, VAHU6, BasinName, SUBBASIN,
                WQS_ID, `WQS Buffer Distance`, `WQS Comments`,GNIS_ID:OBJECTID, 
                ID305B_1, ID305B_2,  ID305B_3,  ID305B_4,  ID305B_5,  ID305B_6,  ID305B_7,  ID305B_8,  ID305B_9,  ID305B_10, 
                `AU Buffer Distance`, 
                Lacustrine, TYPE_1)

# Bring in VRO work
VRO <- read_excel('C:/Users/wmu43954/OneDrive - Commonwealth of Virginia/WQA/ForReview.xlsx',
                  sheet = 'VRO') %>% 
  dplyr::select(FDT_STA_ID, STA_DESC, Data_Source, GROUP_STA_ID, ASSESS_REG, VAHU6, BasinName, SUBBASIN,
                WQS_ID, `WQS Buffer Distance`, `WQS Comments`,GNIS_ID:OBJECTID, 
                ID305B_1, ID305B_2,  ID305B_3,  ID305B_4,  ID305B_5,  ID305B_6,  ID305B_7,  ID305B_8,  ID305B_9,  ID305B_10, 
                `AU Buffer Distance`, 
                Lacustrine, TYPE_1)

# add in difficult CMC sites
VROdiff <- read_excel('C:/Users/wmu43954/OneDrive - Commonwealth of Virginia/WQA/Difficult_CMC_Sites.xlsx',
                      sheet = 'VRO') %>% 
  filter(!is.na(FDT_STA_ID)) %>% 
  dplyr::select(FDT_STA_ID,  STA_DESC, Data_Source, GROUP_STA_ID, ASSESS_REG = Region, 
                WQS_ID, `WQS Comments`,
                ID305B_1, ID305B_2, TYPE_1, Lacustrine) 



# smash all the approved regional data together
together <- bind_rows(BRRO, NRO) %>% 
  bind_rows(NROdiff) %>% 
  bind_rows(PRO) %>% 
  bind_rows(PROdiff) %>% 
  bind_rows(TRO) %>% 
  bind_rows(VRO) %>% 
  bind_rows(VROdiff)  %>% 
  distinct(FDT_STA_ID, .keep_all = T)
# clean up workspace
rm(BRRO, NRO, NROdiff, PRO, PROdiff, TRO, VRO, VROdiff)



# Now add the WQS information to the pinned dataset

# Where new WQS_ID information is attached to a station, join in appropriate WQS metadata
WQSlookupToDo <- filter(together, !is.na(WQS_ID)) %>% 
  dplyr::select(StationID = FDT_STA_ID, WQS_ID, `Buffer Distance` = `WQS Buffer Distance`, Comments = `WQS Comments`) %>% 
  #sometimes there are wonky characters embedded into WQS_ID, fix that
  mutate(WQS_ID = str_remove(WQS_ID, '\r\n'))
stationsWithOutWQS_ID <- filter(together, ! FDT_STA_ID %in% WQSlookupToDo$StationID) %>% 
  rename(StationID = FDT_STA_ID,  `Buffer Distance` = `WQS Buffer Distance`, Comments = `WQS Comments`) 



#bring in Riverine layers, valid for the assessment window
riverine <- st_read('C:/HardDriveBackup/R/GitHub/IR2024/GIS/draft_WQS_shapefiles_2022/riverine_draft2022.shp') %>%
  st_drop_geometry() # only care about data not geometry

WQSlookupFull <- left_join(WQSlookupToDo, riverine) %>%
  filter(!is.na(CLASS))
rm(riverine)

lacustrine <- st_read('C:/HardDriveBackup/R/GitHub/IR2024/GIS/draft_WQS_shapefiles_2022/lakes_reservoirs_draft2022.shp') %>%
  st_drop_geometry() # only care about data not geometry

WQSlookupFull <- left_join(WQSlookupToDo, lacustrine) %>%
  filter(!is.na(CLASS)) %>%
  bind_rows(WQSlookupFull)
rm(lacustrine)

estuarineLines <- st_read('C:/HardDriveBackup/R/GitHub/IR2024/GIS/draft_WQS_shapefiles_2022/estuarinelines_draft2022.shp') %>%
  st_drop_geometry() # only care about data not geometry

WQSlookupFull <- left_join(WQSlookupToDo, estuarineLines) %>%
  filter(!is.na(CLASS)) %>%
  bind_rows(WQSlookupFull)
rm(estuarineLines)

estuarinePolys <- st_read('C:/HardDriveBackup/R/GitHub/IR2024/GIS/draft_WQS_shapefiles_2022/estuarinepolygons_draft2022.shp') %>%
  st_drop_geometry() # only care about data not geometry

WQSlookupFull <- left_join(WQSlookupToDo, estuarinePolys) %>%
  filter(!is.na(CLASS)) %>%
  bind_rows(WQSlookupFull)
rm(estuarinePolys)

# what stations are still missing info?
#View(filter(WQSlookupToDo, !StationID %in% WQSlookupFull$StationID) )
# Find duplicates 
# WQSlookupFull %>% 
#   group_by(StationID) %>% 
#   mutate(totCount = n()) %>% 
#   filter(totCount > 1) 

# Smash back in stations that still don't have WQS_ID
WQSlookupFull1 <- bind_rows(WQSlookupFull %>% 
                              mutate(GNIS_ID = as.integer(GNIS_ID)), 
                            stationsWithOutWQS_ID %>% 
                              mutate(GNIS_ID = as.integer(GNIS_ID))) %>% 
  dplyr::select(names(WQStableExisting))



# find any duplicates before binding
z <- filter(WQStableExisting, StationID %in% WQSlookupFull1$StationID) %>% 
  filter(is.na(WQS_ID) | WQS_ID =='')
y <- filter(WQSlookupFull1, StationID %in% WQStableExisting$StationID)%>% 
  filter(is.na(WQS_ID) | WQS_ID =='')
# in general, better to take the data from WQSlookupFull1
rm(z, y)

WQStableExistingUnique <- filter(WQStableExisting, ! StationID %in% WQSlookupFull1$StationID) 



WQSlookup_withStandards_pin <- bind_rows(WQStableExistingUnique, WQSlookupFull1) # for pin

# pin to server
# pin(WQSlookup_withStandards_pin, name = 'ejones/citmonStationsWithWQSFinal', 
#     description = 'WQS lookup table with Standards for Citmon/Non Agency Stations',
#     board = 'rsconnect')

rm(stationsWithOutWQS_ID, WQSlookup_withStandards_pin, WQSlookupFull, WQSlookupFull1, WQSlookupToDo, WQStableExisting, WQStableExistingUnique)









# now work on AUs

# Bring in existing pinned data
AUlookup <- pin_get("ejones/AUlookup", board = "rsconnect")

# reformat data from above to match this template. Unfortunately, add all 10 AU fields to this version
stationsWithAUinfo <- together %>% 
  filter(!is.na(ID305B_1)) %>% 
  mutate(CYCLE = 2024,
         `Spatially Snapped` = FALSE) %>% 
  dplyr::select(FDT_STA_ID, contains("ID305B"), 
                `Buffer Distance` = `AU Buffer Distance`,
                Source_Sta_Id = Data_Source)
# find duplicates
View(
  filter(AUlookup, CYCLE == 2024) %>% 
    filter(FDT_STA_ID %in% stationsWithAUinfo$FDT_STA_ID)
)
# we want to take data from most recent assessor effort, so drop dupes and combine to a single dataset
dropSites <- filter(AUlookup, CYCLE == 2024) %>% 
  filter( FDT_STA_ID %in% stationsWithAUinfo$FDT_STA_ID)
AUlookup_pin <- bind_rows(
  filter(AUlookup, !  FDT_STA_ID %in% dropSites$FDT_STA_ID), # all these sites are in IR2024, so this works but keep in mind multiple cycles in the future!
  stationsWithAUinfo)

# pin to server
# pin(AUlookup, name = 'ejones/citmonStationsWithWQSFinal',
#     description = 'AUlookup table, draft until data are submitted to CEDS and cycle is approved (use that data instead). Last run by EVJ on 04/03/2023',
#     board = 'rsconnect')

