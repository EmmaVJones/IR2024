source('global.R')

# # Pinned to server, done for each region in C:\HardDriveBackup\R\GitHub\IR2022\1.preprocessData\preprocessingWorkflow\ReworkingDataPreprocessingMethod.Rmd
# #regionalAUs <- st_read('userDataToUpload/AU_working/va_2020_aus_riverine_DRAFT_BRRO.shp') %>%
# #  st_transform(4326)   # transform to WQS84 for spatial intersection
# #pin(regionalAUs, name = 'BRROworkingAUriverine', description = "BRRO working AU riverine", board = "rsconnect")
# 
# 
# 
# # Pull data from server
# conventionals <- pin_get("conventionals2022IRfinalWithSecchi", board = "rsconnect")
# #pin_get("conventionals2022IRdraft", board = "rsconnect") %>%
# #filter(FDT_DATE_TIME >= "2015-01-01 00:00:00 UTC" )
# vahu6 <- st_as_sf(pin_get("vahu6", board = "rsconnect")) #%>%  # bring in as sf object
# #  # change all CO to TRO for riverine app
# #  mutate(ASSESS_REG = ifelse(ASSESS_REG == as.character('CO'), as.character('TRO'), as.character(ASSESS_REG))) %>%
# #  mutate(ASSESS_REG = as.factor(ASSESS_REG))
# WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
# # placeholder for now, shouldn't be a spatial file
# historicalStationsTable <- st_read('data/GIS/va20ir_wqms.shp') %>%
#   st_drop_geometry()#read_csv('data/stationsTable2022begin.csv') # last cycle stations table (forced into new station table format)
# WCmetals <- pin_get("WCmetals-2022IRfinal",  board = "rsconnect")
# # Separate object for analysis, tack on METALS and RMK designation to make the filtering of certain lab comment codes easier
# WCmetalsForAnalysis <- WCmetals %>%
#   dplyr::select(Station_Id, FDT_DATE_TIME, FDT_DEPTH, # include depth bc a few samples taken same datetime but different depths
#                 METAL_Antimony = `STORET_01095_ANTIMONY, DISSOLVED (UG/L AS SB)`, RMK_Antimony = RMK_01097,
#                 METAL_Arsenic = `STORET_01000_ARSENIC, DISSOLVED  (UG/L AS AS)`, RMK_Arsenic = RMK_01002,
#                 METAL_Barium = `STORET_01005_BARIUM, DISSOLVED (UG/L AS BA)`, RMK_Barium = RMK_01005,
#                 METAL_Cadmium = `STORET_01025_CADMIUM, DISSOLVED (UG/L AS CD)`, RMK_Cadmium = RMK_01025,
#                 METAL_Chromium = `STORET_01030_CHROMIUM, DISSOLVED (UG/L AS CR)`, RMK_Chromium = RMK_01030,
#                 # Chromium III and ChromiumVI dealt with inside metalsAnalysis()
#                 METAL_Copper = `STORET_01040_COPPER, DISSOLVED (UG/L AS CU)`, RMK_Copper = RMK_01040,
#                 METAL_Lead = `STORET_01049_LEAD, DISSOLVED (UG/L AS PB)`, RMK_Lead = RMK_01049,
#                 METAL_Mercury = `STORET_50091_MERCURY-TL,FILTERED WATER,ULTRATRACE METHOD UG/L`, RMK_Mercury = RMK_50091,
#                 METAL_Nickel = `STORET_01065_NICKEL, DISSOLVED (UG/L AS NI)`, RMK_Nickel = RMK_01067,
#                 METAL_Uranium = `URANIUM_TOT`, RMK_Uranium = `RMK_7440-61-1T`,
#                 METAL_Selenium = `STORET_01145_SELENIUM, DISSOLVED (UG/L AS SE)`, RMK_Selenium = RMK_01145,
#                 METAL_Silver = `STORET_01075_SILVER, DISSOLVED (UG/L AS AG)`, RMK_Silver = RMK_01075,
#                 METAL_Thallium = `STORET_01057_THALLIUM, DISSOLVED (UG/L AS TL)`, RMK_Thallium = RMK_01057,
#                 METAL_Zinc = `STORET_01090_ZINC, DISSOLVED (UG/L AS ZN)`, RMK_Zinc = RMK_01092,
#                 METAL_Hardness = `STORET_DHARD_HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED`, RMK_Hardness = RMK_DHARD) %>%
#   group_by(Station_Id, FDT_DATE_TIME, FDT_DEPTH) %>%
#   mutate_if(is.numeric, as.character) %>%
#   pivot_longer(cols = METAL_Antimony:RMK_Hardness, #RMK_Antimony:RMK_Hardness,
#                names_to = c('Type', 'Metal'),
#                names_sep = "_",
#                values_to = 'Value') %>%
#   ungroup() %>% group_by(Station_Id, FDT_DATE_TIME, FDT_DEPTH, Metal) %>%
#   pivot_wider(id_cols = c(Station_Id, FDT_DATE_TIME, FDT_DEPTH, Metal), names_from = Type, values_from = Value) %>% # pivot remark wider so the appropriate metal value is dropped when filtering on lab comment codes
#   filter(! RMK %in% c('IF', 'J', 'O', 'QF', 'V')) %>% # lab codes dropped from further analysis
#   pivot_longer(cols= METAL:RMK, names_to = 'Type', values_to = 'Value') %>% # get in appropriate format to flip wide again
#   pivot_wider(id_cols = c(Station_Id, FDT_DATE_TIME, FDT_DEPTH), names_from = c(Type, Metal), names_sep = "_", values_from = Value) %>%
#   mutate_at(vars(contains('METAL')), as.numeric) %>%# change metals values back to numeric
#   rename_with(~str_remove(., 'METAL_')) # drop METAL_ prefix for easier analyses
# Smetals <- pin_get("Smetals-2022IRfinal",  board = "rsconnect")
# IR2020WCmetals <- pin_get("WCmetals-2020IRfinal",  board = "rsconnect")
# IR2020Smetals <- pin_get("Smetals-2020IRfinal",  board = "rsconnect")
# WQMstationFull <- pin_get("WQM-Station-Full", board = "rsconnect")
# VSCIresults <- pin_get("VSCIresults", board = "rsconnect") %>%
#   filter( between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]) )
# VCPMI63results <- pin_get("VCPMI63results", board = "rsconnect") %>%
#   filter( between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]) )
# VCPMI65results <- pin_get("VCPMI65results", board = "rsconnect") %>%
#   filter( between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]) )
# benSampsStations <- st_as_sf(pin_get("ejones/benSampsStations", board = "rsconnect")) #%>%
# benSamps <- pin_get("ejones/benSamps", board = "rsconnect") %>%
#   filter(between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2])) %>%# limit data to assessment window
#   filter(RepNum %in% c('1', '2')) %>% # drop QA and wonky rep numbers
#   filter(`Target Count` == 110) %>% # only assess rarified data
#   left_join(benSampsStations, by = 'StationID') %>% # update with spatial, assess reg, vahu6, basin/subbasin, & ecoregion info
#   dplyr::select(StationID, Sta_Desc, everything()) %>%
#   arrange(StationID)
# habSamps <- pin_get("ejones/habSamps", board = "rsconnect") %>%
#   filter(between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]))# limit data to assessment window
# habValues <- pin_get("ejones/habValues", board = "rsconnect")  %>%
#   filter(HabSampID %in% habSamps$HabSampID)
# habObs <- pin_get("ejones/habObs", board = "rsconnect") %>%
#   filter(HabSampID %in% habSamps$HabSampID)
# pinnedDecisions <- pin_get('IR2022bioassessmentDecisions_test', board = 'rsconnect') %>% ####################change to real when available
#   dplyr::select(IRYear:FinalAssessmentRating)
# 
# 
# # Bring in local data (for now)
# ammoniaAnalysis <- readRDS('userDataToUpload/processedStationData/ammoniaAnalysis.RDS')
# markPCB <- read_excel('data/2022 IR PCBDatapull_EVJ.xlsx', sheet = '2022IR Datapull EVJ') %>%
#   mutate(SampleDate = as.Date(SampleDate),
#          `Parameter Rounded to WQS Format` = as.numeric(signif(`Total Of Concentration`, digits = 2))) %>% # round to even for comparison to chronic criteria)
#   dplyr::select(StationID: `Total Of Concentration`,`Parameter Rounded to WQS Format`, StationID_join)
# fishPCB <- read_excel('data/FishTissuePCBsMetals_EVJ.xlsx', sheet= 'PCBs') %>%
#   mutate(`Parameter Rounded to WQS Format` = as.numeric(signif(`Total PCBs`, digits = 2))) %>% # round to even for comparison to chronic criteria)
#   dplyr::select(WBID:`Weight (g)`, `Water %`:`Total PCBs`, `Parameter Rounded to WQS Format`, uncorrected, `recovery corrected`, comment3, Latitude, Longitude)
# fishMetals <- read_excel('data/FishTissuePCBsMetals_EVJ.xlsx', sheet= 'Metals') %>%
#   rename("# of Fish" = "# of fish...4", "Species_Name"  = "Species_Name...5",
#          "species_name" = "Species_Name...47", "number of fish" = "# of fish...48")#,
# #         "Beryllium"= "Be",  "Aluminum" = "Al",  "Vanadium" = "V", "Chromium"= "Cr",
# #         "Manganese" = "Mn", "Nickel" = "Ni", "Copper" = "Cu" ,"Zinc"=  "Zn",  "Arsenic" = "As" , "Selenium" = "Se" ,
# #         "Silver" = "Ag" , "Cadmium" = "Cd",
# #         "Antimony" = "Sb", "Barium"=  "Ba" , "Mercury" =  "Hg", "Thallium"  = "Tl", "Lead" = "Pb"  )
# fishMetalsScreeningValues <- read_csv('data/FishMetalsScreeningValues.csv') %>%
#   group_by(`Screening Method`) %>%
#   pivot_longer(cols = -`Screening Method`, names_to = 'Metal', values_to = 'Screening Value') %>%
#   arrange(Metal)



shinyServer(function(input, output, session) {
  
  # display the loading feature until data loads into app
  load_data()
  
})