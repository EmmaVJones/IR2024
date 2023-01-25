# This script is a companion to the server.R and allows you to test individual portions of the application


source('global.R')

# # Pinned to server, done for each region in C:\HardDriveBackup\R\GitHub\IR2022\1.preprocessData\preprocessingWorkflow\ReworkingDataPreprocessingMethod.Rmd
# #regionalAUs <- st_read('userDataToUpload/AU_working/va_2020_aus_riverine_DRAFT_BRRO.shp') %>%
# #  st_transform(4326)   # transform to WQS84 for spatial intersection
# #pin(regionalAUs, name = 'BRROworkingAUriverine', description = "BRRO working AU riverine", board = "rsconnect")
# 
# 
# 
# Pull data from server
conventionals <- pin_get('ejones/conventionals2024draft_with7Q10flag', board = 'rsconnect') # version with precompiled 7Q10 information to save rendering time, only used by apps
vahu6 <- st_as_sf(pin_get("vahu6", board = "rsconnect")) # bring in as sf object
WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
WQMstationFull <- pin_get("WQM-Station-Full", board = "rsconnect")
stationsTemplate <- pin_get('ejones/stationsTable2024begin', board = 'rsconnect')[0,] %>% 
  mutate(across(matches(c("LATITUDE", "LONGITUDE", "EXC", "SAMP")), as.numeric)) 
assessmentWindowLowFlows <- pin_get('ejones/AssessmentWindowLowFlows', board = 'rsconnect')


# Read in local data, don't want this saved on the server in case someone pulls and uses for unintended purposes
template <- read_csv('userDataToUpload/stationTableResults.csv')
lastUpdated <- as.Date(file.info('userDataToUpload/stationTableResults.csv')$mtime)
historicalStationsTable <- readRDS('data/stationsTable2022.RDS')# last cycle stations table (forced into new station table format)
historicalStationsTable2 <- readRDS('data/stationsTable2020.RDS') # two cycle ago stations table
intakeSites <- readRDS('data/sites100mFromVDHintakes.RDS')


WCmetals <- pin_get("ejones/WCmetalsForAnalysis",  board = "rsconnect")
# Separate object for analysis, tack on METALS and RMK designation to make the filtering of certain lab comment codes easier
WCmetalsForAnalysis <- readRDS('data/WCmetalsForApp.RDS')
Smetals <- pin_get("Smetals-2022IRfinal",  board = "rsconnect")
# IR2020WCmetals <- pin_get("WCmetals-2020IRfinal",  board = "rsconnect")
# IR2020Smetals <- pin_get("Smetals-2020IRfinal",  board = "rsconnect")
VSCIresults <- pin_get("VSCIresults", board = "rsconnect") %>%
  filter( between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]) )
VCPMI63results <- pin_get("VCPMI63results", board = "rsconnect") %>%
  filter( between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]) )
VCPMI65results <- pin_get("VCPMI65results", board = "rsconnect") %>%
  filter( between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]) )
benSampsStations <- st_as_sf(pin_get("ejones/benSampsStations", board = "rsconnect")) #%>%
benSamps <- pin_get("ejones/benSamps", board = "rsconnect") %>%
  filter(between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2])) %>%# limit data to assessment window
  filter(RepNum %in% c('1', '2')) %>% # drop QA and wonky rep numbers
  filter(`Target Count` == 110) %>% # only assess rarified data
  left_join(benSampsStations, by = 'StationID') %>% # update with spatial, assess reg, vahu6, basin/subbasin, & ecoregion info
  dplyr::select(StationID, Sta_Desc, everything()) %>%
  arrange(StationID)
habSamps <- pin_get("ejones/habSamps", board = "rsconnect") %>%
  filter(between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]))# limit data to assessment window
habValues <- pin_get("ejones/habValues", board = "rsconnect")  %>%
  filter(HabSampID %in% habSamps$HabSampID)
habObs <- pin_get("ejones/habObs", board = "rsconnect") %>%
  filter(HabSampID %in% habSamps$HabSampID)
pinnedDecisions <- pin_get('ejones/CurrentIRbioassessmentDecisions', board = 'rsconnect') %>% 
  dplyr::select(IRYear:FinalAssessmentRating)


# Bring in local data (for now)
ammoniaAnalysis <- readRDS('userDataToUpload/ammoniaAnalysis.RDS') # by having this locally and pre-analyzed it speeds up app rendering significantly
markPCB <- read_excel('data/oldData/2022 IR PCBDatapull_EVJ.xlsx', sheet = '2022IR Datapull EVJ') %>%
  mutate(SampleDate = as.Date(SampleDate),
         `Parameter Rounded to WQS Format` = as.numeric(signif(`Total Of Concentration`, digits = 2))) %>% # round to even for comparison to chronic criteria)
  dplyr::select(StationID: `Total Of Concentration`,`Parameter Rounded to WQS Format`, StationID_join)
fishPCB <- read_excel('data/oldData/FishTissuePCBsMetals_EVJ.xlsx', sheet= 'PCBs') %>%
  mutate(`Parameter Rounded to WQS Format` = as.numeric(signif(`Total PCBs`, digits = 2))) %>% # round to even for comparison to chronic criteria)
  dplyr::select(WBID:`Weight (g)`, `Water %`:`Total PCBs`, `Parameter Rounded to WQS Format`, uncorrected, `recovery corrected`, comment3, Latitude, Longitude)
fishMetals <- read_excel('data/oldData/FishTissuePCBsMetals_EVJ.xlsx', sheet= 'Metals') %>%
  rename("# of Fish" = "# of fish...4", "Species_Name"  = "Species_Name...5",
         "species_name" = "Species_Name...47", "number of fish" = "# of fish...48")#,
#         "Beryllium"= "Be",  "Aluminum" = "Al",  "Vanadium" = "V", "Chromium"= "Cr",
#         "Manganese" = "Mn", "Nickel" = "Ni", "Copper" = "Cu" ,"Zinc"=  "Zn",  "Arsenic" = "As" , "Selenium" = "Se" ,
#         "Silver" = "Ag" , "Cadmium" = "Cd",
#         "Antimony" = "Sb", "Barium"=  "Ba" , "Mercury" =  "Hg", "Thallium"  = "Tl", "Lead" = "Pb"  )
fishMetalsScreeningValues <- read_csv('data/FishMetalsScreeningValues.csv') %>%
  group_by(`Screening Method`) %>%
  pivot_longer(cols = -`Screening Method`, names_to = 'Metal', values_to = 'Screening Value') %>%
  arrange(Metal)


## Data Upload Tab

# Pull together stationTable, this happens once per user upload
stationTable <- read_csv('userDataToUpload/stationTableResults.csv',
                         col_types = cols(COMMENTS = col_character())) %>% # force to character bc parsing can incorrectly guess logical based on top 1000 rows
  as_tibble()
# Remove stations that don't apply to application
lakeStations <- filter_at(stationTable, vars(starts_with('TYPE')), any_vars(. == 'L'))
estuarineStations <- filter(stationTable, str_detect(ID305B_1, 'E_'))


stationTable <- filter(stationTable, !STATION_ID %in% lakeStations$STATION_ID) %>%
  filter(!STATION_ID %in% estuarineStations$STATION_ID) %>%
  
  
  #      # Citmon addition
  #      # Special CitMon/Non Agency step until full WQS_ID inplementation in IR2028
  #      left_join(citmonWQS, by = c('STATION_ID' = 'StationID')) %>% # (1)
  
  # Join to real WQS_ID's (do this second in case citmon station double listed, want proper WQS_ID if available) (1)
  left_join(WQSlookup, by = c('STATION_ID' = 'StationID')) %>%
  
  #      # coalesce these similar fields together, taking WQS_ID info before citmon method
  #      mutate(CLASS = coalesce(CLASS, `WQS Class`),
  #             SEC = coalesce(SEC, `WQS Section`),
  #             SPSTDS = coalesce(SPSTDS, `WQS Special Standard`)) %>% 
  #      dplyr::select(-c(`WQS Section`, `WQS Class`, `WQS Special Standard`)) %>% 
  
  # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
  mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
  mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", as.character(CLASS))) %>%
  # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
  left_join(WQSvalues, by = 'CLASS_BASIN') %>%
  # data cleanup
  dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
  rename('CLASS' = 'CLASS.x') %>%
  
  # As of 1/5/23, confirmed that water temperature criteria for class VII waters is determined by the former 
  #  class of the water. Also confirmed that all class VII waters in TRO, PRO, and NRO were formerly class III,  
  #  which means that these waters have a maximum temperature criteria of 32 degrees C.
  mutate(`Max Temperature (C)` = case_when(
    CLASS == "VII" & REGION == "TRO" ~ 32,
    CLASS == "VII" & REGION == "PRO" ~ 32,
    CLASS == "VII" & REGION == "NRO" ~ 32,
    TRUE ~ as.numeric(`Max Temperature (C)`) )) %>% 
  
  # Join station ecoregion information (for benthic analyses)
  left_join(dplyr::select(WQMstationFull, WQM_STA_ID, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME) %>%
              distinct(WQM_STA_ID, .keep_all = TRUE), by = c('STATION_ID' = 'WQM_STA_ID')) %>%
  mutate(lakeStation = FALSE)




## Watershed selection Tab
# side panel arguments
DEQregionSelection <- "BRRO"#"NRO"#"NRO"#"VRO"#"PRO"#"NRO"#'BRRO'#"PRO"#'BRRO'
basinSelection <- "James-Upper"#"James-Middle"#"Potomac-Lower"#"Appomattox"#"Potomac-Lower"#"James-Upper"#"James-Middle"#"James-Upper"#"Chowan-Dismal"#'Roanoke'#'James-Upper'#'Roanoke'#"Small Coastal" ##"Roanoke"#"Roanoke"#'James-Upper'#
HUC6Selection <- "JU11"#"JM01"#"PL30"#"PU10"#"JA42"#"PL56"#"JU44"#JM01"#"JU41"#"CM01"#"RD15"#"RU24"#"JM01"#'JU21'#"RU14"#"CB47"#'JM16'#'RU09'#'RL12'#

# pull together data based on user input on side panel
# Pull AU data from server
regionalAUs <- st_zm(st_as_sf(pin_get(paste0(DEQregionSelection, '_AUriverine'), board = 'rsconnect')))   


the_data <- filter(vahu6, ASSESS_REG %in% DEQregionSelection) %>%
  left_join(dplyr::select(subbasinToVAHU6, VAHU6, Basin, BASIN_CODE, Basin_Code))
basin_filter <- filter(the_data, Basin_Code %in% basinSelection)
huc6_filter <- filter(basin_filter, VAHU6 %in% HUC6Selection)


# Spatially intersect chosen VAHU6 with regionalAUs
AUs <- suppressWarnings(st_intersection(regionalAUs,  huc6_filter)) 

### ------- Just for testing, skip front matter and jump to picking station for module testing---------------------------------------------------------
# Pull Conventionals data for selected AU on click
conventionals_HUC <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter$VAHU6) %>%
  left_join(dplyr::select(stationTable, STATION_ID:VAHU6,lakeStation,
                          WQS_ID:EPA_ECO_US_L3NAME),
            by = c('FDT_STA_ID' = 'STATION_ID')) %>%
  filter(!is.na(ID305B_1)) %>%
  pHSpecialStandardsCorrection() %>% #correct pH to special standards where necessary
  temperatureSpecialStandardsCorrection()  # correct temperature special standards where necessary

carryoverStations <- filter(stationTable, VAHU6 %in% huc6_filter$VAHU6 & str_detect(COMMENTS, "This station has no data"))


# AUs from data in conventionals and carryoverStations
AUselectionOptions <- unique(c(conventionals_HUC$ID305B_1, 
                               #dplyr::select(carryoverStations(), ID305B_1:ID305B_10) %>% as.character()))
                               # this method allows for multiple carryover stations to be concatinated correctly
                               dplyr::select(carryoverStations, ID305B_1:ID305B_10) %>% 
                                 mutate_at(vars(starts_with("ID305B")), as.character) %>%
                                 pivot_longer(ID305B_1:ID305B_10, names_to = 'ID305B', values_to = 'keep') %>%
                                 filter(!is.na(keep)) %>% 
                                 pull(keep) ))
AUselectionOptions <- AUselectionOptions[!is.na(AUselectionOptions) & !(AUselectionOptions %in% c("NA", "character(0)", "logical(0)"))] # double check nothing wonky in there before proceeding

# user selection
AUselection <- AUselectionOptions[1]# "VAN-A15R_ACO02A00"#"VAW-I04R_JKS03A00"##"VAV-B05R_BAR03A10"#"VAP-J17R_SFT01B98"#AUselectionOptions[1]#"VAN-A27R_AUA01A00"#

# Allow user to select from available stations in chosen AU to investigate further
stationSelection_ <- filter(conventionals_HUC, ID305B_1 %in% AUselection | ID305B_2 %in% AUselection | 
                              ID305B_3 %in% AUselection | ID305B_4 %in% AUselection | ID305B_5 %in% AUselection | 
                              ID305B_6 %in% AUselection | ID305B_7 %in% AUselection | ID305B_8 %in% AUselection | 
                              ID305B_9 %in% AUselection | ID305B_10 %in% AUselection) %>%
  distinct(FDT_STA_ID) %>%
  pull()
# add in carryover stations
if(nrow(carryoverStations) > 0){
  carryoverStationsInAU <- filter(carryoverStations,  ID305B_1 %in% AUselection | ID305B_2 %in% AUselection | 
                                    ID305B_3 %in% AUselection | ID305B_4 %in% AUselection | ID305B_5 %in% AUselection | 
                                    ID305B_6 %in% AUselection | ID305B_7 %in% AUselection | ID305B_8 %in% AUselection | 
                                    ID305B_9 %in% AUselection | ID305B_10 %in% AUselection) %>%
    distinct(STATION_ID) %>%
    pull()
  if(length(carryoverStationsInAU) > 0){
    stationSelection_  <- c(stationSelection_ , carryoverStationsInAU)  } }

# user selection
stationSelection <- stationSelection_[2]


# Pull conventionals data for just selected AU
AUData <- filter_at(conventionals_HUC, vars(starts_with("ID305B")), any_vars(. %in% AUselection) ) 

# Pull conventionals data for just selected station
stationData <- filter(AUData, FDT_STA_ID %in% stationSelection)
### ------- End Just for testing, skip front matter and jump to picking station for module testing---------------------------------------------------------





# Watershed Map, main panel
m <- mapview(basin_filter,label= basin_filter$VAHU6, layer.name = 'Basin Chosen',
               popup= leafpop::popupTable(basin_filter, zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")), legend= FALSE) + 
    mapview(huc6_filter, color = 'yellow',lwd= 5, label= huc6_filter$VAHU6, layer.name = c('Selected HUC6'),
            popup= leafpop::popupTable(huc6_filter, zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")), legend= FALSE)
m@map %>% setView(st_bbox(huc6_filter)$xmax[[1]],st_bbox(huc6_filter)$ymax[[1]],zoom = 9) 

# Table of AUs within Selected VAHU6, main panel
DT::datatable(AUs %>% st_drop_geometry(), rownames = FALSE, 
                options= list(scrollX = TRUE, pageLength = nrow(AUs), scrollY = "300px", dom='Bti'),
                selection = 'none') 

# Table of Stations within Selected VAHU6, main panel
stationSummary <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter$VAHU6) %>%
  distinct(FDT_STA_ID, .keep_all = TRUE)  %>% 
  dplyr::select(FDT_STA_ID:FDT_SPG_CODE, STA_LV1_CODE:Data_Source, Latitude, Longitude, Huc6_Huc_8, Huc6_Huc_8_Name, Huc6_Name) %>% 
  dplyr::select(FDT_STA_ID, GROUP_STA_ID, STA_DESC, Latitude, Longitude, everything()) %>% 
  dplyr::select(-c(FDT_DATE_TIME, FDT_DEPTH, FDT_DEPTH_DESC, FDT_PERCENT_FRB)) %>% # drop date time bc confusing to users and other junk
  mutate(`Analyzed By App` = ifelse(FDT_STA_ID %in% unique(stationTable$STATION_ID), 'yes','no')) 


DT::datatable(stationSummary, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(stationSummary), 
                                                                  scrollY = "300px", dom='Bti'),
                selection = 'none') %>%
    DT::formatStyle('Analyzed By App', target = 'row', backgroundColor = styleEqual(c('yes','no'), c('lightgray', 'yellow'))) 


# Table of stations that were carried over from last cycle that have no data in current window, main panel
carryoverStations <- filter(stationTable, VAHU6 %in% huc6_filter$VAHU6 & str_detect(COMMENTS, "This station has no data"))
z <- carryoverStations %>%  dplyr::select(STATION_ID:VAHU6, COMMENTS)
DT::datatable(z, rownames = FALSE, 
                options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "300px", dom='Bti',
                              autoWidth = TRUE, columnDefs = list(list(width = '400px', targets = c(29)))),
                selection = 'none') 


## Review AU Modal Modal--------------------------------------------------------------------------------------------------------------

# AU modal map
stations <- dplyr::select(stationSummary, STATION_ID = FDT_STA_ID, LATITUDE = Latitude, LONGITUDE = Longitude, `Analyzed By App`) %>%
  bind_rows(filter(stationTable, VAHU6 %in% huc6_filter$VAHU6 & !STATION_ID %in% stationSummary$FDT_STA_ID) %>%
              dplyr::select(STATION_ID, LATITUDE, LONGITUDE) %>%
              mutate(`Analyzed By App` = 'IM carryover with no data in window')) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
           remove = F, # don't remove these lat/lon cols from df
           crs = 4269) # add projection, needs to be geographic for now bc entering lat/lng

z <- AUs
z$ID305B <- factor(z$ID305B) # drop extra factor levels so colors come out right

m <- mapview(huc6_filter, color = 'yellow',lwd= 5, label= NULL, layer.name = c('Selected HUC6'),
             popup= leafpop::popupTable(huc6_filter, zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")), legend= FALSE) + 
  mapview(z, label= z$ID305B, layer.name = c('AUs in Selected HUC6'), zcol = "ID305B", 
          popup= leafpop::popupTable(z, zcol=c("ID305B","MILES","CYCLE","WaterName","Location" )), legend= FALSE) +
  mapview(stations, label = stations$STATION_ID, layer.name = c('Stations in Selected HUC6'), zcol = "Analyzed By App", 
          popup= leafpop::popupTable(stations, zcol=c("STATION_ID", "Analyzed By App")), legend= FALSE) 
m@map 


## End Review AU Modal Modal--------------------------------------------------------------------------------------------------------------

## Status Overview Modal--------------------------------------------------------------------------------------------------------------
  
chooseStatusParameter <- c('Overall Status', unique(parameterSTATcrosswalk$Parameter))[1]

# VAHU6 station status 
stationStatus <- VAHU6stationSummary(stationTable, huc6_filter, parameterSTATcrosswalk) 

# Station Status modal map
indStatusMap(chooseStatusParameter, stationStatus) 

## End Status Overview Modal--------------------------------------------------------------------------------------------------------------


################################ Assessment Unit Review Tab ########################################

# Show selected VAHU6
DT::datatable(huc6_filter %>% st_set_geometry(NULL) %>% select(VAHU6, VaName, Basin),
            rownames = FALSE, options= list(pageLength = 1, scrollY = "35px", dom='t'),
            selection = 'none')

# Pull Conventionals data for selected AU on click
conventionals_HUC <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter$VAHU6) %>%
    left_join(dplyr::select(stationTable, STATION_ID:VAHU6,lakeStation,
                            WQS_ID:EPA_ECO_US_L3NAME),
              by = c('FDT_STA_ID' = 'STATION_ID')) %>%
    filter(!is.na(ID305B_1)) %>%
    pHSpecialStandardsCorrection() %>% #correct pH to special standards where necessary
    temperatureSpecialStandardsCorrection()  # correct temperature special standards where necessary


# Allow user to select from available AUs to investigate further

# AUs from data in conventionals and carryoverStations
AUselectionOptions <- unique(c(conventionals_HUC$ID305B_1, 
                               #dplyr::select(carryoverStations(), ID305B_1:ID305B_10) %>% as.character()))
                               # this method allows for multiple carryover stations to be concatinated correctly
                               dplyr::select(carryoverStations, ID305B_1:ID305B_10) %>% 
                                 mutate_at(vars(starts_with("ID305B")), as.character) %>%
                                 pivot_longer(ID305B_1:ID305B_10, names_to = 'ID305B', values_to = 'keep') %>%
                                 filter(!is.na(keep)) %>% 
                                 pull(keep) ))
AUselectionOptions <- AUselectionOptions[!is.na(AUselectionOptions) & !(AUselectionOptions %in% c("NA", "character(0)", "logical(0)"))] # double check nothing wonky in there before proceeding

# user selection
AUselection <- AUselectionOptions[2]

# Table with AU information from last cycle
z <- filter(regionalAUs, ID305B %in% AUselection) %>% st_set_geometry(NULL) %>% as.data.frame()
datatable(z, rownames = FALSE, 
          options= list(pageLength = nrow(z),scrollX = TRUE, scrollY = "300px", dom='t'),
          selection = 'none')


# Allow user to select from available stations in chosen AU to investigate further
stationSelection_ <- filter(conventionals_HUC, ID305B_1 %in% AUselection | ID305B_2 %in% AUselection | 
                ID305B_3 %in% AUselection | ID305B_4 %in% AUselection | ID305B_5 %in% AUselection | 
                ID305B_6 %in% AUselection | ID305B_7 %in% AUselection | ID305B_8 %in% AUselection | 
                ID305B_9 %in% AUselection | ID305B_10 %in% AUselection) %>%
    distinct(FDT_STA_ID) %>%
    pull()
# add in carryover stations
if(nrow(carryoverStations) > 0){
  carryoverStationsInAU <- filter(carryoverStations,  ID305B_1 %in% AUselection | ID305B_2 %in% AUselection | 
                                    ID305B_3 %in% AUselection | ID305B_4 %in% AUselection | ID305B_5 %in% AUselection | 
                                    ID305B_6 %in% AUselection | ID305B_7 %in% AUselection | ID305B_8 %in% AUselection | 
                                    ID305B_9 %in% AUselection | ID305B_10 %in% AUselection) %>%
    distinct(STATION_ID) %>%
    pull()
  if(length(carryoverStationsInAU) > 0){
    stationSelection_  <- c(stationSelection_ , carryoverStationsInAU)  } }

# user selection
stationSelection <- stationSelection_[1]


# Pull conventionals data for just selected AU
AUData <- filter_at(conventionals_HUC, vars(starts_with("ID305B")), any_vars(. %in% AUselection) ) 

# Pull conventionals data for just selected station
stationData <- filter(AUData, FDT_STA_ID %in% stationSelection)

# Organize station metadata to report to user on stationInfo DT::datatable
stationInfo <- filter(stationTable, STATION_ID == stationSelection) %>% 
    select(STATION_ID:VAHU6, WQS_ID:Trout)

# Table display of stationInfo object
z <- stationInfo %>%
    t() %>% as.data.frame() %>% rename(`Station and WQS Information` = 1)
DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'),
                selection = 'none')  

# Thumbnail map of station and AU
point <- dplyr::select(stationInfo,  STATION_ID, starts_with('ID305B'), LATITUDE, LONGITUDE ) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
           remove = F, # don't remove these lat/lon cols from df
           crs = 4269) # add projection, needs to be geographic for now bc entering lat/lng
segmentChoices <- dplyr::select(point, starts_with('ID305B')) %>% st_drop_geometry() %>% as.character()  
segment <- filter(regionalAUs, ID305B %in% segmentChoices)
map1 <- mapview(segment,zcol = 'ID305B', label= segment$ID305B, layer.name = 'Assessment Unit (ID305B_1)',
                popup= leafpop::popupTable(segment, zcol=c("ID305B","MILES","CYCLE","WaterName")), legend= FALSE) + 
  mapview(point, color = 'yellow', lwd = 5, label= point$STATION_ID, layer.name = c('Selected Station'),
          popup=NULL, legend= FALSE)
map1@map %>% setView(point$LONGITUDE, point$LATITUDE, zoom = 12) 

# Historical Station Table Information 
z <- suppressWarnings(filter(historicalStationsTable, `Station Id` %in% stationSelection) %>% 
                        select(`Station Id`:`Modified Date`) %>%
                        t() %>% as.data.frame()) #%>% rename(`Station Information From 2022 Cycle` = 'V1')) # need to update each rebuild
names(z) <- paste0('Station Information From ', as.numeric(assessmentCycle) - 2, ' Cycle')
DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'),
              selection = 'none')  

z <- suppressWarnings(filter(historicalStationsTable2, `Station Id` %in% stationSelection) %>% 
                        select(`Station Id`:`Modified Date`) %>%
                        t() %>% as.data.frame()) #%>% rename(`Station Information From 2020 Cycle` = 'V1')) # need to update each rebuild
names(z) <- paste0('Station Information From ', as.numeric(assessmentCycle) - 4, ' Cycle')
DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'),
              selection = 'none') 




## Station Table View Section

# Run longer analyses first
# still running both of these on entire dataset for visualization purposes and so assessors can understand previous assessment decisions
ecoli <- bacteriaAssessmentDecision(stationData, 'ECOLI', 'LEVEL_ECOLI', 10, 410, 126)
enter <- bacteriaAssessmentDecision(stationData, 'ENTEROCOCCI', 'LEVEL_ENTEROCOCCI', 10, 130, 35)
z <- filter(ammoniaAnalysis, StationID %in% unique(stationData$FDT_STA_ID)) %>%
  map(1) 
ammoniaAnalysisStation <- z$AmmoniaAnalysis 
WCmetalsStationAnalysisStation <- filter(WCmetalsForAnalysis, StationID %in% unique(stationData$FDT_STA_ID)) %>%
    map(1)  

WCmetalsStationAnalysisStation$WCmetalsExceedanceSummary

  # PWS stuff
  if(nrow(stationData) > 0){
    if(is.na(unique(stationData$PWS))  ){
      PWSconcat <- tibble(#STATION_ID = unique(stationData$FDT_STA_ID),
        PWS= NA)
    } else {
      PWSconcat <- cbind(#tibble(STATION_ID = unique(stationData()$FDT_STA_ID)),
        assessPWSsummary(assessPWS(stationData, NITRATE_mg_L, LEVEL_NITRATE, 10), 'PWS_Nitrate'),
        assessPWSsummary(assessPWS(stationData, CHLORIDE_mg_L, LEVEL_CHLORIDE, 250), 'PWS_Chloride'),
        assessPWSsummary(assessPWS(stationData, SULFATE_TOTAL_mg_L, LEVEL_SULFATE_TOTAL, 250), 'PWS_Total_Sulfate')) %>%
        dplyr::select(-ends_with('exceedanceRate')) }
    
    # chloride assessment if data exists
    if(nrow(filter(stationData, !is.na(CHLORIDE_mg_L)))){
      chlorideFreshwater <- rollingWindowSummary(
        annualRollingExceedanceSummary(
          annualRollingExceedanceAnalysis(chlorideFreshwaterAnalysis(stationData), 3, aquaticLifeUse = TRUE) ), "CHL")
    } else {
      chlorideFreshwater <- tibble(CHL_EXC = NA, CHL_STAT= NA)}
    
    # Water toxics combination with PWS, Chloride Freshwater, and water column PCB data
    if(nrow(bind_cols(PWSconcat,
                      chlorideFreshwater,
                      PCBmetalsDataExists(filter(markPCB, str_detect(SampleMedia, 'Water')) %>%
                                          filter(StationID %in% stationData$FDT_STA_ID), 'WAT_TOX')) %>%
            dplyr::select(contains(c('_EXC','_STAT'))) %>%
            mutate(across( everything(),  as.character)) %>%
            pivot_longer(cols = contains(c('_EXC','_STAT')), names_to = 'parameter', values_to = 'values', values_drop_na = TRUE) ) > 1) {
      WCtoxics <- tibble(WAT_TOX_EXC = NA, WAT_TOX_STAT = 'Review',
                         PWSinfo = list(PWSconcat))# add in PWS information so you don't need to run this analysis again
    } else { WCtoxics <- tibble(WAT_TOX_EXC = NA, WAT_TOX_STAT = NA,
                                PWSinfo = list(PWSconcat))}# add in PWS information so you don't need to run this analysis again

  } else { WCtoxics <- tibble(WAT_TOX_EXC = NA, WAT_TOX_STAT = NA,
                              PWSinfo = list(PWSconcat))}# add in PWS information so you don't need to run this analysis again

waterToxics <- WCtoxics

# Create station table row for each site
stationTableOutput <- bind_rows(stationsTemplate,
                                
                                cbind(StationTableStartingData(stationData),
                                      tempExceedances(stationData) %>% quickStats('TEMP'),
                                      DOExceedances_Min(stationData) %>% quickStats('DO'), 
                                      pHExceedances(stationData) %>% quickStats('PH'),
                                      
                                      # this runs the bacteria assessment again (unfortunately), but it suppresses 
                                      # any unnecessary bacteria fields for the stations table to avoid unnecessary flags
                                      # and it only runs the 2 year analysis per IR2024 rules
                                      bacteriaAssessmentDecisionClass( # NEW for IR2024, bacteria only assessed in two most recent years of assessment period
                                        filter(stationData, between(FDT_DATE_TIME, assessmentPeriod[1] + years(4), assessmentPeriod[2])),
                                        uniqueStationName = unique(stationData$FDT_STA_ID)),
                                      
                                      rollingWindowSummary(
                                        annualRollingExceedanceSummary(
                                          annualRollingExceedanceAnalysis(ammoniaAnalysisStation, yearsToRoll = 3)), parameterAbbreviation = "AMMONIA"),
                                      
                                      # Water Column Metals
                                      metalsAssessmentFunction(WCmetalsStationAnalysisStation$WCmetalsExceedanceSummary),
                                      
                                      # Water Toxics combo fields
                                      waterToxics, 
                                      
                                      
                                      # Roger's sediment metals analysis, transcribed
                                      metalsData(filter(Smetals, Station_Id %in% stationData$FDT_STA_ID), 'SED_MET'),
                                      # Mark's sediment PCB results, flagged
                                      PCBmetalsDataExists(filter(markPCB, str_detect(SampleMedia, 'Sediment')) %>%
                                                            filter(StationID %in%  stationData$FDT_STA_ID), 'SED_TOX'),
                                      # Gabe's fish metals results, flagged
                                      PCBmetalsDataExists(filter(fishMetals, Station_ID %in% stationData$FDT_STA_ID), 'FISH_MET'),
                                      # Gabe's fish PCB results, flagged
                                      PCBmetalsDataExists(filter(fishPCB, `DEQ rivermile` %in%  stationData$FDT_STA_ID), 'FISH_TOX'),
                                      benthicAssessment(stationData, VSCIresults),
                                      countNutrients(stationData, PHOSPHORUS_mg_L, LEVEL_PHOSPHORUS, NA) %>% quickStats('NUT_TP') %>% 
                                        mutate(NUT_TP_STAT = ifelse(NUT_TP_STAT != "S", "Review", NA)), # flag OE but don't show a real assessment decision
                                      countNutrients(stationData, CHLOROPHYLL_A_ug_L, LEVEL_CHLOROPHYLL_A, NA) %>% quickStats('NUT_CHLA') %>%
                                        mutate(NUT_CHLA_STAT = NA)) %>% # don't show a real assessment decision) %>%
                                  #mutate(COMMENTS = NA) %>%
                                  # add in real comments from uploaded station table
                                  left_join(dplyr::select(stationTable, STATION_ID, COMMENTS),
                                            by = 'STATION_ID') %>% 
                                  dplyr::select(-ends_with(c('exceedanceRate','Assessment Decision', 'VERBOSE', 'StationID', "PWSinfo",
                                                             'BACTERIADECISION', 'BACTERIASTATS')))) %>% 
  filter(!is.na(STATION_ID)) 

# Display marked up station table row for each site
datatable(stationTableOutput, extensions = 'Buttons', escape=F, rownames = F, editable = TRUE,
            options= list(scrollX = TRUE, pageLength = nrow(stationTableOutput),
                          # adjust COMMENTS column width
                          autoWidth = TRUE, columnDefs = list(list(width = '400px', targets = c(71))), # DT starts counting at 0
                          dom='Bt', buttons=list('copy',
                                                 list(extend='csv',filename=paste('AssessmentResults_',paste(assessmentCycle,stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                                 list(extend='excel',filename=paste('AssessmentResults_',paste(assessmentCycle,stationSelection, collapse = "_"),Sys.Date(),sep='')))),
            selection = 'none') %>% 
    formatStyle(c('TEMP_EXC','TEMP_SAMP','TEMP_STAT'), 'TEMP_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
    formatStyle(c('DO_EXC','DO_SAMP','DO_STAT'), 'DO_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
    formatStyle(c('PH_EXC','PH_SAMP','PH_STAT'), 'PH_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
    formatStyle(c('ECOLI_EXC','ECOLI_SAMP','ECOLI_GM_EXC','ECOLI_GM_SAMP','ECOLI_STAT'), 'ECOLI_STAT', backgroundColor = styleEqual(c('IM'), c('red'))) %>%
    formatStyle(c('ENTER_SAMP','ENTER_EXC',"ENTER_GM_EXC","ENTER_GM_SAMP",'ENTER_STAT'), 'ENTER_STAT', backgroundColor = styleEqual(c('IM'), c('red'))) %>%
    formatStyle(c('AMMONIA_EXC','AMMONIA_STAT'), 'AMMONIA_STAT', backgroundColor = styleEqual(c('Review', 'IM'), c('yellow','red'))) %>%
    formatStyle(c('WAT_MET_EXC','WAT_MET_STAT'), 'WAT_MET_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
    formatStyle(c('WAT_TOX_EXC','WAT_TOX_STAT'), 'WAT_TOX_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
    formatStyle(c('SED_MET_EXC','SED_MET_STAT'), 'SED_MET_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%   
    formatStyle(c('SED_TOX_EXC','SED_TOX_STAT'), 'SED_TOX_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
    formatStyle(c('FISH_MET_EXC','FISH_MET_STAT'), 'FISH_MET_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%   
    formatStyle(c('FISH_TOX_EXC','FISH_TOX_STAT'), 'FISH_TOX_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
    formatStyle(c('BENTHIC_STAT'), 'BENTHIC_STAT', backgroundColor = styleEqual(c('Review'), c('yellow'))) %>%
    formatStyle(c('NUT_TP_EXC','NUT_TP_SAMP'), 'NUT_TP_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) 
  


## PWS table output marked up
if(is.na(unique(stationData$PWS))){
    PWSconcat <- tibble(STATION_ID = unique(stationData$FDT_STA_ID),
                        PWS= 'PWS Standards Do Not Apply To Station')
    DT::datatable(PWSconcat, escape=F, rownames = F, options= list(scrollX = TRUE, pageLength = nrow(PWSconcat), dom='t'),
                  selection = 'none')
    
  } else {
    PWSconcat <- waterToxics %>% 
      dplyr::select(PWSinfo) %>% 
      unnest(cols = c(PWSinfo)) %>% 
      dplyr::select(-ends_with('exceedanceRate')) 
    
    DT::datatable(PWSconcat, escape=F, rownames = F, options= list(scrollX = TRUE, pageLength = nrow(PWSconcat), dom='t'),
                  selection = 'none') %>% 
      formatStyle(c("PWS_Nitrate_EXC","PWS_Nitrate_SAMP","PWS_Nitrate_STAT"), "PWS_Nitrate_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>%
      formatStyle(c("PWS_Chloride_EXC","PWS_Chloride_SAMP","PWS_Chloride_STAT"), "PWS_Chloride_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>%
      formatStyle(c("PWS_Total_Sulfate_EXC","PWS_Total_Sulfate_SAMP","PWS_Total_Sulfate_STAT"), "PWS_Total_Sulfate_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) } 

# show text flag if a station is within 100m of a VDH intake, analysis conducted in 2.oragnizeMetadata/waterIntakeAnalysis.Rmd
if(unique(stationData$FDT_STA_ID) %in% intakeSites$FDT_STA_ID){
  print('This station is within 100 meters of a drinking water intake. Please review whether the station should be assessed for secondary human health criteria.')
}

#### Data Sub Tab ####---------------------------------------------------------------------------------------------------

# Display Data 
DT::datatable(AUData, extensions = 'Buttons', escape=F, rownames = F, 
                options= list(scrollX = TRUE, pageLength = nrow(AUData), scrollY = "300px", 
                              dom='Btf', buttons=list('copy',
                                                      list(extend='csv',filename=paste('AUData_',paste(stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                                      list(extend='excel',filename=paste('AUData_',paste(stationSelection, collapse = "_"),Sys.Date(),sep='')))),
                selection = 'none')
# Summarize data
paste(nrow(AUData), 'records were retrieved for',as.character(AUselection),sep=' ')
AUData %>% 
  group_by(FDT_STA_ID) %>% 
  count() %>% dplyr::rename('Number of Records'='n')
withinAssessmentPeriod(AUData)





#Benthic tab
# the Benthics module handles data plotting and SCI choice stuff
# this part of app that pulls together biologist assessment decisions and makes benthic fact sheets is 
# purposely left at the server level of the app bc it was a pain to get the .Rmd to come out of a module correctly

assessmentDecision_UserSelection <- filter(pinnedDecisions, StationID %in% stationSelection) 

# Bioassesment information from current cycle
DT::datatable(assessmentDecision_UserSelection,  escape=F, rownames = F,
                options= list(dom= 't' , pageLength = nrow(assessmentDecision_UserSelection),scrollX = TRUE, scrollY = "250px"),
                selection = 'none')

SCI_UserSelection <- filter(VSCIresults, StationID %in% filter(assessmentDecision_UserSelection, AssessmentMethod == 'VSCI')$StationID) %>%
    bind_rows(
      filter(VCPMI63results, StationID %in% filter(assessmentDecision_UserSelection, AssessmentMethod == 'VCPMI63 + Chowan')$StationID)  ) %>%
    bind_rows(
      filter(VCPMI65results, StationID %in% filter(assessmentDecision_UserSelection, AssessmentMethod == 'VCPMI65 - Chowan')$StationID)  ) %>%
  # only Current IR data
  filter(between(`Collection Date`,assessmentPeriod[1], assessmentPeriod[2])) %>% # limit data to assessment window
  filter(RepNum %in% c('1', '2')) %>% # drop QA and wonky rep numbers
  filter(`Target Count` == 110) %>% # only use family level rarified data
  filter(Gradient != "Boatable") %>%  # don't assess where no SCI not validated
    # add back in description information
    left_join(filter(benSamps, StationID %in% stationSelection) %>%
                dplyr::select(StationID, Sta_Desc, BenSampID,US_L3CODE, US_L3NAME, HUC_12, VAHU6, Basin, Basin_Code),
              by = c('StationID', 'BenSampID')) %>%
    dplyr::select(StationID, Sta_Desc, BenSampID, `Collection Date`, RepNum, everything())
benSampsFilter <- filter(benSamps, BenSampID %in% SCI_UserSelection$BenSampID)
habitatUserSelection <- habitatConsolidation(stationSelection, habSamps, habValues)  






