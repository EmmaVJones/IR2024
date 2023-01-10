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
