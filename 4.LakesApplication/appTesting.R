# This script is a companion to the server.R and allows you to test individual portions of the application


source('global.R')

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
WCmetals <- pin_get("WCmetals-2022IRfinal",  board = "rsconnect")
# Separate object for analysis, tack on METALS and RMK designation to make the filtering of certain lab comment codes easier
WCmetalsForAnalysis <- WCmetals %>%
  dplyr::select(Station_Id, FDT_DATE_TIME, FDT_DEPTH, # include depth bc a few samples taken same datetime but different depths
                METAL_Antimony = `STORET_01095_ANTIMONY, DISSOLVED (UG/L AS SB)`, RMK_Antimony = RMK_01097,
                METAL_Arsenic = `STORET_01000_ARSENIC, DISSOLVED  (UG/L AS AS)`, RMK_Arsenic = RMK_01002,
                METAL_Barium = `STORET_01005_BARIUM, DISSOLVED (UG/L AS BA)`, RMK_Barium = RMK_01005,
                METAL_Cadmium = `STORET_01025_CADMIUM, DISSOLVED (UG/L AS CD)`, RMK_Cadmium = RMK_01025,
                METAL_Chromium = `STORET_01030_CHROMIUM, DISSOLVED (UG/L AS CR)`, RMK_Chromium = RMK_01030,
                # Chromium III and ChromiumVI dealt with inside metalsAnalysis()
                METAL_Copper = `STORET_01040_COPPER, DISSOLVED (UG/L AS CU)`, RMK_Copper = RMK_01040,
                METAL_Lead = `STORET_01049_LEAD, DISSOLVED (UG/L AS PB)`, RMK_Lead = RMK_01049,
                METAL_Mercury = `STORET_50091_MERCURY-TL,FILTERED WATER,ULTRATRACE METHOD UG/L`, RMK_Mercury = RMK_50091,
                METAL_Nickel = `STORET_01065_NICKEL, DISSOLVED (UG/L AS NI)`, RMK_Nickel = RMK_01067,
                METAL_Uranium = `URANIUM_TOT`, RMK_Uranium = `RMK_7440-61-1T`,
                METAL_Selenium = `STORET_01145_SELENIUM, DISSOLVED (UG/L AS SE)`, RMK_Selenium = RMK_01145,
                METAL_Silver = `STORET_01075_SILVER, DISSOLVED (UG/L AS AG)`, RMK_Silver = RMK_01075,
                METAL_Thallium = `STORET_01057_THALLIUM, DISSOLVED (UG/L AS TL)`, RMK_Thallium = RMK_01057,
                METAL_Zinc = `STORET_01090_ZINC, DISSOLVED (UG/L AS ZN)`, RMK_Zinc = RMK_01092,
                METAL_Hardness = `STORET_DHARD_HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED`, RMK_Hardness = RMK_DHARD) %>%
  group_by(Station_Id, FDT_DATE_TIME, FDT_DEPTH) %>%
  mutate_if(is.numeric, as.character) %>%
  pivot_longer(cols = METAL_Antimony:RMK_Hardness, #RMK_Antimony:RMK_Hardness,
               names_to = c('Type', 'Metal'),
               names_sep = "_",
               values_to = 'Value') %>%
  ungroup() %>% group_by(Station_Id, FDT_DATE_TIME, FDT_DEPTH, Metal) %>%
  pivot_wider(id_cols = c(Station_Id, FDT_DATE_TIME, FDT_DEPTH, Metal), names_from = Type, values_from = Value) %>% # pivot remark wider so the appropriate metal value is dropped when filtering on lab comment codes
  filter(! RMK %in% c('IF', 'J', 'O', 'QF', 'V')) %>% # lab codes dropped from further analysis
  pivot_longer(cols= METAL:RMK, names_to = 'Type', values_to = 'Value') %>% # get in appropriate format to flip wide again
  pivot_wider(id_cols = c(Station_Id, FDT_DATE_TIME, FDT_DEPTH), names_from = c(Type, Metal), names_sep = "_", values_from = Value) %>%
  mutate_at(vars(contains('METAL')), as.numeric) %>%# change metals values back to numeric
  rename_with(~str_remove(., 'METAL_')) # drop METAL_ prefix for easier analyses
Smetals <- pin_get("Smetals-2022IRfinal",  board = "rsconnect")


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
lakeNutStandards <- read_csv('data/9VAC25-260-187lakeNutrientStandards.csv')




## Data Upload Tab

# Pull together stationTable, this happens once per user upload
stationTable <- read_csv('userDataToUpload/stationTableResults.csv',
                         col_types = cols(COMMENTS = col_character(),
                                          LACUSTRINE = col_character())) %>% # force to character bc parsing can incorrectly guess logical based on top 1000 rows
  #fix periods in column names from excel
  as_tibble() %>%
  
  filter_at(vars(starts_with('TYPE')), any_vars(. == 'L')) %>% # keep only lake stations
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
  # Join actual WQS criteria to each StationID
  left_join(WQSvalues, by = 'CLASS_BASIN') %>%
  # data cleanup
  dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
  rename('CLASS' = 'CLASS.x') %>%
  # # Don't need this for lakes
  # # As of 1/5/23, confirmed that water temperature criteria for class VII waters is determined by the former 
  # #  class of the water. Also confirmed that all class VII waters in TRO, PRO, and NRO were formerly class III,  
  # #  which means that these waters have a maximum temperature criteria of 32 degrees C.
  # mutate(`Max Temperature (C)` = case_when(
  #   CLASS == "VII" & REGION == "TRO" ~ 32,
  #   CLASS == "VII" & REGION == "PRO" ~ 32,
  #   CLASS == "VII" & REGION == "NRO" ~ 32,
  #   TRUE ~ as.numeric(`Max Temperature (C)`) )) %>% 
  
  # Join station ecoregion information (for benthic analyses)
left_join(dplyr::select(WQMstationFull, WQM_STA_ID, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME) %>%
            distinct(WQM_STA_ID, .keep_all = TRUE), by = c('STATION_ID' = 'WQM_STA_ID')) %>% # last cycle had code to fix Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard) but not sure if necessary
  lakeNameStandardization() %>% # standardize lake names
  
  # extra special step
  mutate(Lake_Name = case_when(STATION_ID %in% c('2-TRH000.40') ~ 'Thrashers Creek Reservoir',
                               STATION_ID %in% c('2-LSL000.16') ~ 'Lone Star Lake F (Crystal Lake)',
                               STATION_ID %in% c('2-LSL000.04') ~ 'Lone Star Lake G (Crane Lake)',
                               STATION_ID %in% c('2-LSL000.20') ~ 'Lone Star Lake I (Butler Lake)',
                               STATION_ID %in% c('2-NWB002.93','2-NWB004.67', '2-NWB006.06') ~ 'Western Branch Reservoir',
                               STATION_ID %in% c('2-LDJ000.60') ~ 'Lake Nottoway (Lee Lake)',
                               TRUE ~ as.character(Lake_Name))) %>%
  
  # special step for 187 lakes missing designation
  #mutate(Lakes_187B = case_when(STATION_ID == '1BNTH043.48' ~ 'y',
  #                              TRUE ~ as.character(Lakes_187B))) %>% 
  
  
  left_join(lakeNutStandards %>% 
              mutate(Lakes_187B = 'y'),  # special step to make sure the WQS designation for 187 are correct even when not
            by = c('Lake_Name')) %>%
  # lake drummond special standards
  mutate(Lakes_187B = ifelse(is.na(Lakes_187B.y ), Lakes_187B.x, Lakes_187B.y), 
         `Chlorophyll a (ug/L)` = case_when(Lake_Name %in% c('Lake Drummond') ~ 35,
                                            TRUE ~ as.numeric(`Chlorophyll a (ug/L)`)),
         `Total Phosphorus (ug/L)` = case_when(Lake_Name %in% c('Lake Drummond') ~ 40,
                                               TRUE ~ as.numeric(`Total Phosphorus (ug/L)`))) %>% 
  dplyr::select(STATION_ID:StreamType, Lakes_187B, `Description Of Waters`:`Total Phosphorus (ug/L)`) %>%
  # match lake limit to TP data unit
  mutate(`Total Phosphorus (mg/L)` = `Total Phosphorus (ug/L)` / 1000) %>% 
  mutate(lakeStation = TRUE)





################################ Lake Selection Tab ########################################

# side panel arguments
DEQregionSelection <- "BRRO"#"NRO"# "BRRO"

# User clicks region and brings back entire state (this just expedites original app rendering and spreads
# out data requests)
regionalAUs <- st_zm(st_as_sf(pin_get('AUreservoir', board = 'rsconnect'))) %>%
  lakeNameStandardization()

  
# Query lakes in region By Selectize arguments
lakeSelection_ <- regionalAUs %>% 
  st_drop_geometry() %>% 
  filter( ASSESS_REG %in% DEQregionSelection) %>% 
  distinct(Lake_Name) %>% 
  arrange(Lake_Name) %>% 
  pull()
lakeSelection <- "Banister Lake"#"Cherrystone Reservoir"#"Aquia Reservoir (Smith Lake)"# "Claytor Lake"# "Cherrystone Reservoir"#lakeSelection_[7]

AUs <- filter(regionalAUs, Lake_Name %in% lakeSelection & ASSESS_REG %in% DEQregionSelection)
lake_filter <- filter_at(stationTable, vars(starts_with('ID305B')), any_vars(. %in% AUs$ID305B)) 
lakeStations <- lake_filter %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326)  # add projection, needs to be geographic for now bc entering lat/lng


### ------- Just for testing, skip front matter and jump to picking station for module testing---------------------------------------------------------
# Pull Conventionals data for selected lake on click
conventionalsLake <- filter(conventionals, FDT_STA_ID %in% lake_filter$STATION_ID) %>%
  left_join(dplyr::select(stationTable, STATION_ID:VAHU6, lakeStation,
                          WQS_ID:`Total Phosphorus (mg/L)`),
            #WQS_ID:`Max Temperature (C)`), 
            by = c('FDT_STA_ID' = 'STATION_ID')) %>%
  filter(!is.na(ID305B_1)) %>%
  # Special Standards Correction step. This is done on the actual data bc some special standards have temporal components
  pHSpecialStandardsCorrection() %>% # correct pH to special standards where necessary
  temperatureSpecialStandardsCorrection() %>% # correct temperature special standards where necessary
  thermoclineDepth()  # adds thermocline information and SampleDate


# Allow user to select from available AUs to investigate further
AUselectionOptions <- unique(dplyr::select(lake_filter, ID305B_1:ID305B_10) %>% 
                               mutate_at(vars(starts_with("ID305B")), as.character) %>%
                               pivot_longer(ID305B_1:ID305B_10, names_to = 'ID305B', values_to = 'keep') %>%
                               pull(keep) )
AUselectionOptions <- AUselectionOptions[!is.na(AUselectionOptions) & !(AUselectionOptions %in% c("NA", "character(0)", "logical(0)"))]

inputAUselection <- AUselectionOptions[1]
AUselection <- filter(regionalAUs, ID305B %in% inputAUselection) %>% st_set_geometry(NULL) %>% as.data.frame()


# Allow user to select from available stations in chosen AU to investigate further
stationSelectionOptions <- filter_at(lake_filter, vars(starts_with("ID305B")), any_vars(. %in% inputAUselection)) %>%
  distinct(STATION_ID) %>% arrange(STATION_ID) %>%  pull()
stationSelection <- stationSelectionOptions[1]

# Pull conventionals data for just selected AU
AUData <- filter_at(conventionalsLake, vars(starts_with("ID305B")), any_vars(. %in% inputAUselection) ) 

# Pull conventionals data for just selected station
stationData <- filter(AUData, FDT_STA_ID %in% stationSelection) 
### ------- End Just for testing, skip front matter and jump to picking station for module testing---------------------------------------------------------








  
# Lake Map

z <- suppressWarnings(st_coordinates(sf::st_centroid(AUs %>% group_by(Lake_Name) %>% summarise())))
  
CreateWebMap(maps = c("Topo","Imagery"), collapsed = TRUE) %>%
  {if(nrow(AUs)>1)
    setView(., z[1], z[2], zoom = 10) 
    else setView(., z[1], z[2], zoom = 12) } %>%
  addPolygons(data= AUs, group = 'Selected Lake',
              popup=leafpop::popupTable(AUs, zcol=c('Lake_Name',"ID305B","ASSESS_REG"))) %>%
  {if(nrow(lakeStations) > 0)
    addCircleMarkers(., data = lakeStations, color='black', fillColor='yellow', radius = 4,
                     fillOpacity = 0.5,opacity=0.8,weight = 1,stroke=T, group="Monitored Stations",
                     label = ~STATION_ID, layerId = ~STATION_ID,
                     popup=leafpop::popupTable(lakeStations, zcol=c('STATION_ID',"ID305B_1","ID305B_2","ID305B_3"))) 
    else . } %>%
  addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                   overlayGroups = c('Monitored Stations', 'Selected Lake'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft')       

# Table of AUs within Selected Lake
DT::datatable(AUs %>% st_drop_geometry(), rownames = FALSE, 
                options= list(scrollX = TRUE, pageLength = nrow(AUs), scrollY = "300px", dom='Bti'),
                selection = 'none')   

# Table of Stations within Selected Lake
stationSummary <- filter(conventionals, FDT_STA_ID %in% lake_filter$STATION_ID) %>%
    distinct(FDT_STA_ID, .keep_all = TRUE)  %>% 
    dplyr::select(FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:Data_Source, Latitude, Longitude) %>% 
    dplyr::select(-FDT_DATE_TIME) # drop date time bc confusing to users 

DT::datatable(stationSummary, rownames = FALSE, 
                options= list(scrollX = TRUE, pageLength = nrow(stationSummary), scrollY = "300px", dom='Bti'),
                selection = 'none') 


# Table of stations that were carried over from last cycle that have no data in current window
carryoverStations <- filter(lake_filter, str_detect(COMMENTS, "This station has no data")) 

z <- carryoverStations %>%  dplyr::select(STATION_ID:VAHU6, COMMENTS)
DT::datatable(z, rownames = FALSE, 
              options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "300px", dom='Bti',
                            autoWidth = TRUE, columnDefs = list(list(width = '400px', targets = c(29)))),
              selection = 'none')   






################################ Assessment Unit Review Tab ########################################

# Show selected Lake 
z <- dplyr::select(lake_filter, Lake_Name, VAHU6, Lakes_187B) %>%
    group_by(Lake_Name) %>%
    summarise(VAHU6 = toString(sort(unique(VAHU6))),
              `Section 187` = toString(sort(unique(Lakes_187B))))
datatable(z, rownames = FALSE, options= list(pageLength = 1, scrollY = "35px", dom='t'), selection = 'none')

# Pull Conventionals data for selected lake on click
conventionalsLake <- filter(conventionals, FDT_STA_ID %in% lake_filter$STATION_ID) %>%
    left_join(dplyr::select(stationTable, STATION_ID:VAHU6, lakeStation,
                            WQS_ID:`Total Phosphorus (mg/L)`),
              #WQS_ID:`Max Temperature (C)`), 
              by = c('FDT_STA_ID' = 'STATION_ID')) %>%
    filter(!is.na(ID305B_1)) %>%
  # Special Standards Correction step. This is done on the actual data bc some special standards have temporal components
  pHSpecialStandardsCorrection() %>% # correct pH to special standards where necessary
  temperatureSpecialStandardsCorrection() %>% # correct temperature special standards where necessary
    thermoclineDepth()  # adds thermocline information and SampleDate


# Allow user to select from available AUs to investigate further
AUselectionOptions <- unique(dplyr::select(lake_filter, ID305B_1:ID305B_10) %>% 
                                 mutate_at(vars(starts_with("ID305B")), as.character) %>%
                                 pivot_longer(ID305B_1:ID305B_10, names_to = 'ID305B', values_to = 'keep') %>%
                                 pull(keep) )
AUselectionOptions <- AUselectionOptions[!is.na(AUselectionOptions) & !(AUselectionOptions %in% c("NA", "character(0)", "logical(0)"))]

inputAUselection <- AUselectionOptions[1]
AUselection <- filter(regionalAUs, ID305B %in% inputAUselection) %>% st_set_geometry(NULL) %>% as.data.frame()
datatable(AUselection, rownames = FALSE, 
          options= list(pageLength = nrow(AUselection),scrollX = TRUE, scrollY = "300px", dom='t'),
          selection = 'none')
  

# Allow user to select from available stations in chosen AU to investigate further
stationSelectionOptions <- filter_at(lake_filter, vars(starts_with("ID305B")), any_vars(. %in% inputAUselection)) %>%
    distinct(STATION_ID) %>% arrange(STATION_ID) %>%  pull()
stationSelection <- stationSelectionOptions[1]

# Pull conventionals data for just selected AU
AUData <- filter_at(conventionalsLake, vars(starts_with("ID305B")), any_vars(. %in% inputAUselection) ) 

# Pull conventionals data for just selected station
stationData <- filter(AUData, FDT_STA_ID %in% stationSelection) 

# Organize station metadata to report to user on stationInfo DT::datatable
stationInfo <- filter(stationTable, STATION_ID == stationSelection) %>% 
    select(STATION_ID:VAHU6, WQS_ID:`Total Phosphorus (mg/L)`)

# Table display of stationInfo object
z <- stationInfo %>%
    t() %>% as.data.frame() %>% rename(`Station and WQS Information` = 1)
DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'),
              selection = 'none')  
  
# Thumbnail map of station and AU
point <- dplyr::select(stationInfo,  STATION_ID, starts_with('ID305B'), LATITUDE, LONGITUDE ) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) # add projection, needs to be geographic for now bc entering lat/lng
segmentChoices <- dplyr::select(point, starts_with('ID305B')) %>% st_drop_geometry() %>% as.character()  
segment <- filter(regionalAUs, ID305B %in% segmentChoices)
map1 <- mapview(segment,zcol = 'ID305B', label= segment$ID305B, layer.name = 'Assessment Unit (ID305B_1)',
                popup= leafpop::popupTable(segment, zcol=c("ID305B","Acres","CYCLE","WATER_NAME")), legend= FALSE) + 
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
# save individual ecoli results for later
AUmedians <- AUData %>%
    filter(ID305B_1 %in% inputAUselection) %>%# run ecoli by only 1 AU at a time
    group_by(SampleDate, FDT_DEPTH) %>%
    filter(!is.na(ECOLI)) %>%
    mutate(EcoliDailyMedian = median(ECOLI, na.rm = TRUE)) %>%
    dplyr::select(ID305B_1, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, SampleDate, EcoliDailyMedian, ECOLI, RMK_ECOLI, LEVEL_ECOLI) %>%
    arrange(SampleDate) %>% ungroup() 

# need to run analysis on only one point per day
AUmediansForAnalysis <- AUmedians %>% 
    filter(! LEVEL_ECOLI %in% c('Level I', 'Level II')) %>%
    mutate(ECOLI_Station = ECOLI,
           StationName = FDT_STA_ID,
           ECOLI = EcoliDailyMedian,
           FDT_STA_ID = unique(ID305B_1),
           FDT_DATE_TIME = SampleDate) %>%
    dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, SampleDate, ECOLI, StationName, ECOLI_Station, RMK_ECOLI, LEVEL_ECOLI) # dont run distinct here so extra station data can go to ecoli module

ecoliAU <-  bacteriaAssessmentDecision(AUmediansForAnalysis %>%
                                         distinct(SampleDate, .keep_all = T) , 'ECOLI', 'LEVEL_ECOLI', 10, 410, 126)


z <- filter(ammoniaAnalysis, StationID %in% unique(stationData$FDT_STA_ID)) %>%
  map(1) 
ammoniaAnalysisStation <- z$AmmoniaAnalysis   

# Water Toxics
# PWS stuff
if(nrow(stationData) > 0){
  if(is.na(unique(stationData$PWS))  ){
    PWSconcat <- tibble(#STATION_ID = unique(stationData$FDT_STA_ID),
      PWS= NA)
  } else {
    PWSconcat <- cbind(#tibble(STATION_ID = unique(stationData$FDT_STA_ID)),
      assessPWSsummary(assessPWS(stationData, NITRATE_mg_L, LEVEL_NITRATE, 10), 'PWS_Nitrate'),
      assessPWSsummary(assessPWS(stationData, CHLORIDE_mg_L, LEVEL_CHLORIDE, 250), 'PWS_Chloride'),
      assessPWSsummary(assessPWS(stationData, SULFATE_TOTAL_mg_L, LEVEL_SULFATE_TOTAL, 250), 'PWS_Total_Sulfate')) %>%
      dplyr::select(-ends_with('exceedanceRate')) }
  
  # chloride assessment if data exists
  if(nrow(filter(stationData, !is.na(CHLORIDE_mg_L)))){
    chlorideFreshwater <- rollingWindowSummary(
      annualRollingExceedanceSummary(
        annualRollingExceedanceAnalysis(chlorideFreshwaterAnalysis(stationData), yearsToRoll = 3, aquaticLifeUse = TRUE) ), "CHL")
  } else {
    chlorideFreshwater <- tibble(CHL_EXC = NA, CHL_STAT= NA)}
  
  # Water toxics combination with PWS, Chloride Freshwater, and water column PCB data
  if(nrow(bind_cols(PWSconcat,
                    chlorideFreshwater,
                    PCBmetalsDataExists(filter(markPCB, str_detect(SampleMedia, 'Water')) %>%
                                        filter(StationID %in% stationData$FDT_STA_ID), 'WAT_TOX')) %>%
          dplyr::select(contains(c('_EXC','_STAT'))) %>%
          mutate(across( everything(),  as.character)) %>%
          pivot_longer(cols = contains(c('_EXC','_STAT')), names_to = 'parameter', values_to = 'values', values_drop_na = TRUE) ) >= 1) {
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
                                          annualRollingExceedanceAnalysis(ammoniaAnalysisStation, yearsToRoll = 3, aquaticLifeUse = FALSE)), parameterAbbreviation = "AMMONIA"),
                                      
                                      # Water Column Metals
                                      filter(WCmetalsForAnalysis, Station_Id %in%  stationData$FDT_STA_ID) %>% 
                                        metalsAnalysis(stationData, WER= 1) %>% 
                                        metalsAssessmentFunction(), 
                                      
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
                                      # add in benthic placeholders
                                      tibble(BENTHIC_STAT = NA, BENTHIC_WOE_CAT= NA, BIBI_SCORE = NA),
                                      TP_Assessment(stationData),
                                      chlA_Assessment(stationData) ) %>%
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
    formatStyle(c('AMMONIA_EXC','AMMONIA_STAT'), 'AMMONIA_STAT', backgroundColor = styleEqual(c('Review', 'IM'), c('yellow','red'))) %>%
    formatStyle(c('WAT_MET_EXC','WAT_MET_STAT'), 'WAT_MET_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
    formatStyle(c('WAT_TOX_EXC','WAT_TOX_STAT'), 'WAT_TOX_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
    formatStyle(c('SED_MET_EXC','SED_MET_STAT'), 'SED_MET_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%   
    formatStyle(c('SED_TOX_EXC','SED_TOX_STAT'), 'SED_TOX_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
    formatStyle(c('FISH_MET_EXC','FISH_MET_STAT'), 'FISH_MET_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%   
    formatStyle(c('FISH_TOX_EXC','FISH_TOX_STAT'), 'FISH_TOX_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
    formatStyle(c('BENTHIC_STAT'), 'BENTHIC_STAT', backgroundColor = styleEqual(c('Review'), c('yellow'))) %>%
    formatStyle(c('NUT_TP_EXC','NUT_TP_SAMP'), 'NUT_TP_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>% 
    formatStyle(c('NUT_CHLA_EXC','NUT_CHLA_SAMP'), 'NUT_CHLA_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red')))  


## Water Intake proximity flag for station
if(unique(stationData$FDT_STA_ID) %in% intakeSites$FDT_STA_ID){
    print('This station is within 100 meters of a drinking water intake. Please review whether the station 
                should be assessed for secondary human health criteria.' ) } 

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



#### Data Sub Tab ####---------------------------------------------------------------------------------------------------


# Display Data 
DT::datatable(AUData, extensions = 'Buttons', escape=F, rownames = F, 
              options= list(scrollX = TRUE, pageLength = nrow(AUData), scrollY = "300px", 
                            dom='Btf', buttons=list('copy',
                                                    list(extend='csv',filename=paste('AUData_',paste(stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                                    list(extend='excel',filename=paste('AUData_',paste(stationSelection, collapse = "_"),Sys.Date(),sep='')))),
              selection = 'none')
# Summarize data
paste(nrow(AUData), 'records were retrieved for',as.character(inputAUselection),sep=' ')
plyr::count(AUData, vars = c("FDT_STA_ID")) %>% dplyr::rename('Number of Records'='freq')
withinAssessmentPeriod(AUData)





# Ecoli median data testing
