source('global.R')

# # Pinned to server, done for each region in C:\HardDriveBackup\R\GitHub\IR2022\1.preprocessData\preprocessingWorkflow\ReworkingDataPreprocessingMethod.Rmd
# #regionalAUs <- st_read('userDataToUpload/AU_working/va_2020_aus_riverine_DRAFT_BRRO.shp') %>%
# #  st_transform(4326)   # transform to WQS84 for spatial intersection
# #pin(regionalAUs, name = 'BRROworkingAUriverine', description = "BRRO working AU riverine", board = "rsconnect")
# 
# 
# 
# Pull data from server
conventionals <- pin_get('ejones/conventionals2024final_with7Q10flag', board = 'rsconnect') # version with precompiled 7Q10 information to save rendering time, only used by apps
  #pin_get('ejones/conventionals2024draft_with7Q10flag', board = 'rsconnect') # version with precompiled 7Q10 information to save rendering time, only used by apps
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


WCmetals <- pin_get("WCmetalsIR2024",  board = "rsconnect")#pin_get("WCmetalsIR2024",  board = "rsconnect") 
WCmetalsForAnalysis <- pin_get("ejones/WCmetalsForAnalysisIR2024",  board = "rsconnect") # this is included in local data above
WCmetalsAnalyzed <- readRDS('userDataToUpload/WCmetalsForApp.RDS')
Smetals <- pin_get("SmetalsIR2024",  board = "rsconnect")
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
pinnedDecisions <- bind_rows(pin_get('ejones/CurrentIRbioassessmentDecisions', board = 'rsconnect'),
                             pin_get('ejones/PreviousIRbioassessmentDecisions', board = 'rsconnect')) %>% 
  dplyr::select(IRYear:FinalAssessmentRating)


# Bring in local data (for now)
ammoniaAnalysis <- readRDS('userDataToUpload/ammoniaAnalysis.RDS') # by having this locally and pre-analyzed it speeds up app rendering significantly
markPCB <- pin_get("ejones/PCBIR2024", board = 'rsconnect') %>% #read_excel('data/oldData/2022 IR PCBDatapull_EVJ.xlsx', sheet = '2022IR Datapull EVJ')
  mutate(SampleDate = as.Date(SampleDate),
         `Parameter Rounded to WQS Format` = as.numeric(signif(`Total Of Concentration`, digits = 2))) %>% # round to even for comparison to chronic criteria)
  dplyr::select(StationID: `Total Of Concentration`,`Parameter Rounded to WQS Format`, StationID_join)
fishPCB <- pin_get("ejones/fishPCBIR2024", board = "rsconnect") %>% #read_excel('data/oldData/FishTissuePCBsMetals_EVJ.xlsx', sheet= 'PCBs')
  mutate(`Parameter Rounded to WQS Format` = as.numeric(signif(`Total PCBs`, digits = 2))) %>% # round to even for comparison to chronic criteria)
  dplyr::select(WBID:`Weight (g)`, `Water %`:`Total PCBs`, `Parameter Rounded to WQS Format`, uncorrected, `recovery corrected`, comment3, Latitude, Longitude)
fishMetals <- pin_get("ejones/fishMetalsIR2024", board = "rsconnect") %>% #read_excel('data/oldData/FishTissuePCBsMetals_EVJ.xlsx', sheet= 'Metals')
  rename('# of Fish' = '# of fish...4', 'Species_Name' = "Species_Name...5",
         "Beryllium"= "Be",  "Aluminum" = "Al",  "Vanadium" = "V", "Chromium"= "Cr",
         "Manganese" = "Mn", "Nickel" = "Ni", "Copper" = "Cu" ,"Zinc"=  "Zn",  "Arsenic" = "As" , "Selenium" = "Se" ,
         "Silver" = "Ag" , "Cadmium" = "Cd",
         "Antimony" = "Sb", "Barium"=  "Ba" , "Mercury" =  "Hg", "Thallium"  = "Tl", "Lead" = "Pb"  )



# markPCB <- read_excel('data/oldData/2022 IR PCBDatapull_EVJ.xlsx', sheet = '2022IR Datapull EVJ') %>%
#   mutate(SampleDate = as.Date(SampleDate),
#          `Parameter Rounded to WQS Format` = as.numeric(signif(`Total Of Concentration`, digits = 2))) %>% # round to even for comparison to chronic criteria)
#   dplyr::select(StationID: `Total Of Concentration`,`Parameter Rounded to WQS Format`, StationID_join)
# fishPCB <- pin_get('ejones/fishPCBIR2024', board = 'rsconnect') %>%
#   mutate(`Parameter Rounded to WQS Format` = as.numeric(signif(`Total PCBs`, digits = 2))) %>% # round to even for comparison to chronic criteria)
#   dplyr::select(WBID:`Weight (g)`, `Water %`:`Total PCBs`, `Parameter Rounded to WQS Format`, uncorrected, `recovery corrected`, comment3, Latitude, Longitude)
# read_excel('data/oldData/FishTissuePCBsMetals_EVJ.xlsx', sheet= 'PCBs') %>%
# mutate(`Parameter Rounded to WQS Format` = as.numeric(signif(`Total PCBs`, digits = 2))) %>% # round to even for comparison to chronic criteria)
# dplyr::select(WBID:`Weight (g)`, `Water %`:`Total PCBs`, `Parameter Rounded to WQS Format`, uncorrected, `recovery corrected`, comment3, Latitude, Longitude)
# fishMetals <- pin_get('ejones/fishMetalsIR2024', board = 'rsconnect') %>% 
#   # read_excel('data/oldData/FishTissuePCBsMetals_EVJ.xlsx', sheet= 'Metals') %>%
#   rename("Beryllium"= "Be",  "Aluminum" = "Al",  "Vanadium" = "V", "Chromium"= "Cr",
#          "Manganese" = "Mn", "Nickel" = "Ni", "Copper" = "Cu" ,"Zinc"=  "Zn",  "Arsenic" = "As" , "Selenium" = "Se" ,
#          "Silver" = "Ag" , "Cadmium" = "Cd",
#          "Antimony" = "Sb", "Barium"=  "Ba" , "Mercury" =  "Hg", "Thallium"  = "Tl", "Lead" = "Pb"  )
fishMetalsScreeningValues <- read_csv('data/FishMetalsScreeningValues.csv') %>%
  group_by(`Screening Method`) %>%
  pivot_longer(cols = -`Screening Method`, names_to = 'Metal', values_to = 'Screening Value') %>%
  arrange(Metal)



shinyServer(function(input, output, session) {
  
  # display the loading feature until data loads into app
  load_data()
  
  # Reactive Value to store all site data
  siteData <- reactiveValues()
  
  
  ################################ Data Upload Tab ################################################# 
  
  # Download data template
  output$downloadTemplate <- downloadHandler(filename=function(){'stationTableStatewideExample.csv'},
                                             content=function(file){write_csv(template,file,na = "")})
  
  output$templateLastUpdated_ <- renderUI({
    helpText(paste0('Template last updated: ', lastUpdated))  })
  
  
  # read in stationTable that user uploads
  stationTable <- reactive({  req(input$stationsTable)
    inFile <- input$stationsTable
    stationTable <- read_csv(inFile$datapath,
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
    
    return(stationTable)
  })
  
  
  
  
  ################################ Watershed Selection Tab ########################################
  
  output$DEQregionSelectionUI <- renderUI({ req(vahu6)
    selectInput("DEQregionSelection", "Select DEQ Assessment Region", choices = unique(vahu6$ASSESS_REG))})
  
  
  # Pull AU data from server
  regionalAUs <- reactive({ req(input$pullAUs)
    withProgress(message = 'Reading in Large Spatial File',
                 st_zm(st_as_sf(pin_get(paste0(input$DEQregionSelection, '_AUriverine'), board = 'rsconnect')) )) })     # change to final
  
  
  
  # Query VAHUC6's By Selectize arguments
  the_data <- reactive({ req(regionalAUs(), input$DEQregionSelection)
    filter(vahu6, ASSESS_REG %in% input$DEQregionSelection) %>%
      left_join(dplyr::select(subbasinToVAHU6, VAHU6, Basin, BASIN_CODE, Basin_Code))})
  basin_filter <- shiny::callModule(dynamicSelect, "basinSelection", the_data, "Basin_Code" )
  huc6_filter <- shiny::callModule(dynamicSelect, "HUC6Selection", basin_filter, "VAHU6" )
  
  # Spatially intersect chosen VAHU6 with regionalAUs
  AUs <- reactive({req(huc6_filter(), regionalAUs())
    suppressWarnings(st_intersection(regionalAUs(),  huc6_filter())) }) #filter(vahu6, VAHU6 %in% huc6_filter()$VAHU6)))})
  
  
  # Watershed Map
  output$VAmap <- renderLeaflet({    req(basin_filter(), huc6_filter(), regionalAUs())
    m <- mapview(basin_filter(),label= basin_filter()$VAHU6, layer.name = 'Basin Chosen',
                 popup= leafpop::popupTable(basin_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")), legend= FALSE) + 
      mapview(huc6_filter(), color = 'yellow',lwd= 5, label= huc6_filter()$VAHU6, layer.name = c('Selected HUC6'),
              popup= leafpop::popupTable(huc6_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")), legend= FALSE)
    m@map %>% setView(st_bbox(huc6_filter())$xmax[[1]],st_bbox(huc6_filter())$ymax[[1]],zoom = 9) })
  
  # Table of AUs within Selected VAHU6
  output$AUSummary <-  DT::renderDataTable({ req(AUs())
    DT::datatable(AUs() %>% st_drop_geometry(), rownames = FALSE, 
                  options= list(scrollX = TRUE, pageLength = nrow(AUs()), scrollY = "300px", dom='Bti'),
                  selection = 'none')   })
  
  # Table of Stations within Selected VAHU6
  stationSummary <- reactive({ req(huc6_filter())
    filter(conventionals, Huc6_Vahu6 %in% huc6_filter()$VAHU6) %>%
      distinct(FDT_STA_ID, .keep_all = TRUE)  %>% 
      dplyr::select(FDT_STA_ID:FDT_SPG_CODE, STA_LV1_CODE:Data_Source, Latitude, Longitude, Huc6_Huc_8, Huc6_Huc_8_Name, Huc6_Name) %>% 
      dplyr::select(FDT_STA_ID, GROUP_STA_ID, STA_DESC, Latitude, Longitude, everything()) %>% 
      dplyr::select(-c(FDT_DATE_TIME, FDT_DEPTH, FDT_DEPTH_DESC, FDT_PERCENT_FRB)) %>% # drop date time bc confusing to users and other junk
      mutate(`Analyzed By App` = ifelse(FDT_STA_ID %in% unique(stationTable()$STATION_ID), 'yes','no')) })
  
  output$stationSummary <- DT::renderDataTable({req(stationSummary())
    DT::datatable(stationSummary(), rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(stationSummary()), 
                                                                    scrollY = "300px", dom='Bti'),
                  selection = 'none') %>%
      DT::formatStyle('Analyzed By App', target = 'row', backgroundColor = styleEqual(c('yes','no'), c('lightgray', 'yellow'))) })
  
  
  # Table of stations that were carried over from last cycle that have no data in current window
  carryoverStations <- reactive({ req(huc6_filter(), stationTable())
    filter(stationTable(), VAHU6 %in% huc6_filter()$VAHU6 & str_detect(COMMENTS, "This station has no data")) })
  
  
  # Flag stations that don't have ID305B_1 in uploaded stations table
  stationsWithoutID305B_1 <-  reactive({ req(huc6_filter(), stationTable())
    filter(stationTable(), VAHU6 %in% huc6_filter()$VAHU6) %>% 
      filter(is.na(ID305B_1)) })
  
  
  output$carryoverStationSummary <- DT::renderDataTable({   req(carryoverStations())
    z <- carryoverStations() %>%  dplyr::select(STATION_ID:VAHU6, COMMENTS)
    DT::datatable(z, rownames = FALSE, 
                  options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "200px", dom='Bti',
                                autoWidth = TRUE, columnDefs = list(list(width = '400px', targets = c(29)))),
                  selection = 'none') })
  
  output$stationsWithoutID305B_1Summary <- DT::renderDataTable({   req(stationsWithoutID305B_1())
    z <- stationsWithoutID305B_1() %>%  dplyr::select(STATION_ID:VAHU6, COMMENTS)
    DT::datatable(z, rownames = FALSE, 
                  options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "200px", dom='Bti',
                                autoWidth = TRUE, columnDefs = list(list(width = '400px', targets = c(29)))),
                  selection = 'none') })
  
  
  ## Review AU Modal Modal--------------------------------------------------------------------------------------------------------------
  
  # Button to visualize modal map of AUs in selected VAHU6
  observeEvent(input$reviewAUs,{
    showModal(modalDialog(
      title="Spatially Preview Assessment Units for Selected VAHU6",
      leafletOutput('AUmap'),
      easyClose = TRUE))  })
  
  # AU modal map
  output$AUmap <- renderLeaflet({   req(AUs(), huc6_filter())
    stations <- dplyr::select(stationSummary(), STATION_ID = FDT_STA_ID, LATITUDE = Latitude, LONGITUDE = Longitude, `Analyzed By App`) %>%
      bind_rows(filter(stationTable(), VAHU6 %in% huc6_filter()$VAHU6 & !STATION_ID %in% stationSummary()$FDT_STA_ID) %>%
                  dplyr::select(STATION_ID, LATITUDE, LONGITUDE) %>%
                  mutate(`Analyzed By App` = 'IM carryover with no data in window')) %>%
      st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
               remove = F, # don't remove these lat/lon cols from df
               crs = 4269) # add projection, needs to be geographic for now bc entering lat/lng
    
    z <- AUs()
    z$ID305B <- factor(z$ID305B) # drop extra factor levels so colors come out right
    
    m <- mapview(huc6_filter(), color = 'yellow',lwd= 5, label= NULL, layer.name = c('Selected HUC6'),
                 popup= leafpop::popupTable(huc6_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")), legend= FALSE) + 
      mapview(z, label= z$ID305B, layer.name = c('AUs in Selected HUC6'), zcol = "ID305B", 
              popup= leafpop::popupTable(z, zcol=c("ID305B","MILES","CYCLE","WaterName","Location" )), legend= FALSE) +
      mapview(stations, label = stations$STATION_ID, layer.name = c('Stations in Selected HUC6'), zcol = "Analyzed By App", 
              popup= leafpop::popupTable(stations, zcol=c("STATION_ID", "Analyzed By App")), legend= FALSE) 
    m@map })
  
  ## End Review AU Modal Modal--------------------------------------------------------------------------------------------------------------
  
  
  ## Status Overview Modal--------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$statusOverview,{
    showModal(modalDialog(
      title="Spatially Preview Station Statuses",
      helpText('This map allows users to quickly preview station status results. The map presents an overall station overview where 
               the station is colored based on the most harmful status category (e.g. if a station has 8 Supporting parameter statuses, 
               2 Insufficient parameter statuses, and 1 Impaired status, the station will be colored red to reflect the Impaired status). 
               The number of statuses in the most harmful category is reported for each station in the popup window (accessed by clicking
               the station in the map).'),
      helpText("Additionally, users may use the selection option below to investigate individual parameter statuses across the watershed."),
      helpText(strong('All statuses are based on the station table dataset uploaded to the application.')),
      fluidRow(column(4), # make sure drop down doesn't cover up map navigation features
               column(8,selectInput('chooseStatusParameter','Choose parameter to report station status.', 
                                    choices = c('Overall Status', unique(parameterSTATcrosswalk$Parameter))))),
      leafletOutput('statusMap'),
      size = 'l',  easyClose = TRUE))  })
  
  # VAHU6 station status 
  stationStatus <- reactive({req(input$statusOverview, huc6_filter(), stationTable())
    VAHU6stationSummary(stationTable(), huc6_filter(), parameterSTATcrosswalk) })
  
  # Station Status modal map
  output$statusMap <- renderLeaflet({ req(input$chooseStatusParameter, nrow(stationStatus()) >0)
    indStatusMap(input$chooseStatusParameter, stationStatus())  })
  
  ## End Status Overview Modal--------------------------------------------------------------------------------------------------------------
  
  
  
  
  ################################ Assessment Unit Review Tab ########################################
  
  #  output$test <- renderPrint({#req(regionalAUs)
  #    glimpse(AUs())})
  
  
  # Show selected VAHU6
  output$selectedVAHU6 <- DT::renderDataTable({
    datatable(huc6_filter() %>% st_set_geometry(NULL) %>% select(VAHU6, VaName, Basin),
              rownames = FALSE, options= list(pageLength = 1, scrollY = "35px", dom='t'),
              selection = 'none')})
  
  # Pull Conventionals data for selected AU on click
  conventionals_HUC <- reactive({ req(huc6_filter())
    filter(conventionals, Huc6_Vahu6 %in% huc6_filter()$VAHU6) %>%
      left_join(dplyr::select(stationTable(), STATION_ID:VAHU6,lakeStation,
                              WQS_ID:EPA_ECO_US_L3NAME),
                #WQS_ID:`Max Temperature (C)`), 
                by = c('FDT_STA_ID' = 'STATION_ID')) %>%
      filter(!is.na(ID305B_1)) %>%
      pHSpecialStandardsCorrection() %>% #correct pH to special standards where necessary
      temperatureSpecialStandardsCorrection() }) # correct temperature special standards where necessary
      
      
  # Allow user to select from available AUs to investigate further
  output$AUselection_ <- renderUI({ req(conventionals_HUC())
    # AUs from data in conventionals and carryoverStations
    AUselectionOptions <- unique(c(conventionals_HUC()$ID305B_1, 
                                   #dplyr::select(carryoverStations(), ID305B_1:ID305B_10) %>% as.character()))
                                   # this method allows for multiple carryover stations to be concatinated correctly
                                   dplyr::select(carryoverStations(), ID305B_1:ID305B_10) %>% 
                                     mutate_at(vars(starts_with("ID305B")), as.character) %>%
                                     pivot_longer(ID305B_1:ID305B_10, names_to = 'ID305B', values_to = 'keep') %>%
                                     filter(!is.na(keep)) %>% 
                                     pull(keep) ))
    AUselectionOptions <- AUselectionOptions[!is.na(AUselectionOptions) & !(AUselectionOptions %in% c("NA", "character(0)", "logical(0)"))] # double check nothing wonky in there before proceeding
    selectInput('AUselection', 'Assessment Unit Selection', choices = AUselectionOptions)  })
  
  output$selectedAU <- DT::renderDataTable({req(conventionals_HUC(),input$AUselection)
    z <- filter(regionalAUs(), ID305B %in% input$AUselection) %>% st_set_geometry(NULL) %>% as.data.frame()
    datatable(z, rownames = FALSE, 
              options= list(pageLength = nrow(z),scrollX = TRUE, scrollY = "300px", dom='t'),
              selection = 'none')})
  
  # Allow user to select from available stations in chosen AU to investigate further
  output$stationSelection_ <- renderUI({ req(conventionals_HUC(), input$AUselection)
    z <- filter(conventionals_HUC(), ID305B_1 %in% input$AUselection | ID305B_2 %in% input$AUselection | 
                  ID305B_3 %in% input$AUselection | ID305B_4 %in% input$AUselection | ID305B_5 %in% input$AUselection | 
                  ID305B_6 %in% input$AUselection | ID305B_7 %in% input$AUselection | ID305B_8 %in% input$AUselection | 
                  ID305B_9 %in% input$AUselection | ID305B_10 %in% input$AUselection) %>%
      distinct(FDT_STA_ID) %>%
      pull()
    # add in carryover stations
    if(nrow(carryoverStations()) > 0){
      carryoverStationsInAU <- filter(carryoverStations(),  ID305B_1 %in% input$AUselection | ID305B_2 %in% input$AUselection | 
                                        ID305B_3 %in% input$AUselection | ID305B_4 %in% input$AUselection | ID305B_5 %in% input$AUselection | 
                                        ID305B_6 %in% input$AUselection | ID305B_7 %in% input$AUselection | ID305B_8 %in% input$AUselection | 
                                        ID305B_9 %in% input$AUselection | ID305B_10 %in% input$AUselection) %>%
        distinct(STATION_ID) %>%
        pull()
      if(length(carryoverStationsInAU) > 0){
        z <- c(z, carryoverStationsInAU)  } }
    
    fluidRow(selectInput('stationSelection', 'Station Selection', choices = sort(unique(z))),
             helpText("The stations available in the drop down are limited to stations with an ID305B_1:ID305B_10 designation equal 
                      to the selected AU. All AU's associated with the selected station can be viewed in the map below."))})
  
  # Pull conventionals data for just selected AU
  AUData <- reactive({req(input$AUselection)
    filter_at(conventionals_HUC(), vars(starts_with("ID305B")), any_vars(. %in% input$AUselection) ) }) 
  
  # Pull conventionals data for just selected station
  stationData <- reactive({req(input$stationSelection)#eventReactive( input$stationSelection, {
    filter(AUData(), FDT_STA_ID %in% input$stationSelection) })
  
  # Organize station metadata to report to user on stationInfo DT::datatable
  stationInfo <- reactive({req(input$stationSelection, AUData())
    filter(stationTable(), STATION_ID == input$stationSelection) %>% 
      select(STATION_ID:VAHU6, WQS_ID:Trout)})
  
  # Table display of stationInfo object
  output$stationInfo <- DT::renderDataTable({ req(stationInfo())#req(stationData())
    z <- stationInfo() %>%
      t() %>% as.data.frame() %>% rename(`Station and WQS Information` = 1)
    DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'),
                  selection = 'none')  })
  
  # Thumbnail map of station and AU
  output$stationMap <- renderLeaflet({req(nrow(stationInfo()) >0) # to prevent having no lat/lng data for that half second app is catching up after station change
    point <- dplyr::select(stationInfo(),  STATION_ID, starts_with('ID305B'), LATITUDE, LONGITUDE ) %>%
      st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
               remove = F, # don't remove these lat/lon cols from df
               crs = 4269) # add projection, needs to be geographic for now bc entering lat/lng
    segmentChoices <- dplyr::select(point, starts_with('ID305B')) %>% st_drop_geometry() %>% as.character()  
    segment <- filter(regionalAUs(), ID305B %in% segmentChoices)
    map1 <- mapview(segment,zcol = 'ID305B', label= segment$ID305B, layer.name = 'Assessment Unit (ID305B_1)',
                    popup= leafpop::popupTable(segment, zcol=c("ID305B","MILES","CYCLE","WaterName")), legend= FALSE) + 
      mapview(point, color = 'yellow', lwd = 5, label= point$STATION_ID, layer.name = c('Selected Station'),
              popup=NULL, legend= FALSE)
    map1@map %>% setView(point$LONGITUDE, point$LATITUDE, zoom = 12) })
  
  
  # Historical Station Table Information 
  output$stationHistoricalInfo1 <- DT::renderDataTable({ req(nrow(stationInfo()) >0)
    z <- suppressWarnings(filter(historicalStationsTable, `Station Id` %in% input$stationSelection) %>% 
                            select(`Station Id`:`Modified Date`) %>%
                            t() %>% as.data.frame()) #%>% rename(`Station Information From 2022 Cycle` = 'V1')) # need to update each rebuild
    names(z) <- paste0('Station Information From ', as.numeric(assessmentCycle) - 2, ' Cycle')
    DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'),
                  selection = 'none')  })
  
  output$stationHistoricalInfo2 <- DT::renderDataTable({ req(nrow(stationInfo()) >0)
    z <- suppressWarnings(filter(historicalStationsTable2, `Station Id` %in% input$stationSelection) %>% 
                            select(`Station Id`:`Modified Date`) %>%
                            t() %>% as.data.frame()) #%>% rename(`Station Information From 2020 Cycle` = 'V1')) # need to update each rebuild
    names(z) <- paste0('Station Information From ', as.numeric(assessmentCycle) - 4, ' Cycle')
    DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'),
                  selection = 'none')  })
  
  
  
  
  
  ## Station Table View Section
  
  # Run longer analyses first
  # still running both of these on entire dataset for visualization purposes and so assessors can understand previous assessment decisions
  ecoli <- reactive({req(stationData())
    bacteriaAssessmentDecision(stationData(), 'ECOLI', 'LEVEL_ECOLI', 10, 410, 126)})
  enter <- reactive({req(stationData())
    bacteriaAssessmentDecision(stationData(), 'ENTEROCOCCI', 'LEVEL_ENTEROCOCCI', 10, 130, 35)})
  ammoniaAnalysisStation <- reactive({req(stationData())
    z <- filter(ammoniaAnalysis, StationID %in% unique(stationData()$FDT_STA_ID)) %>%
      map(1) 
    z$AmmoniaAnalysis })
  
  
  WCmetalsStationAnalysisStation <- reactive({req(stationData())
    filter(WCmetalsAnalyzed, StationID %in% unique(stationData()$FDT_STA_ID)) %>%
      map(1)  })
  
  # for PWS WCmetals
  WCmetalsStationPWS <- reactive({req(stationData())
    left_join(dplyr::select(stationData(), FDT_STA_ID, PWS) %>% distinct(FDT_STA_ID, .keep_all = T),
              filter(WCmetalsForAnalysis, Station_Id %in%  stationData()$FDT_STA_ID),
              by = c('FDT_STA_ID' = 'Station_Id')) })



  waterToxics <- reactive({ req(stationData())
    # PWS stuff
    if(nrow(stationData()) > 0){
      if(is.na(unique(stationData()$PWS))  ){
        PWSconcat <- tibble(#STATION_ID = unique(stationData()$FDT_STA_ID),
          PWS= NA)
      } else {
        PWSconcat <- cbind(#tibble(STATION_ID = unique(stationData()$FDT_STA_ID)),
          assessPWSsummary(assessPWS(stationData(), NITROGEN_NITRATE_TOTAL_00620_mg_L, LEVEL_00620, 10), 'PWS_NitrateTotal'),
          assessPWSsummary(assessPWS(stationData(), CHLORIDE_TOTAL_00940_mg_L, LEVEL_00940, 250), 'PWS_ChlorideTotal'),
          assessPWSsummary(assessPWS(stationData(), SULFATE_TOTAL_mg_L, LEVEL_SULFATE_TOTAL, 250), 'PWS_Total_Sulfate'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), AntimonyTotal, RMK_AntimonyTotal, 5), 'PWS_AntimonyTotal'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), ArsenicTotal, RMK_ArsenicTotal, 10), 'PWS_ArsenicTotal'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), BariumTotal, RMK_BariumTotal, 2000), 'PWS_BariumTotal'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), CadmiumTotal, RMK_CadmiumTotal, 5), 'PWS_CadmiumTotal'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), ChromiumTotal, RMK_ChromiumTotal, 100), 'PWS_ChromiumTotal'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), CopperTotal, RMK_CopperTotal, 1300), 'PWS_CopperTotal'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), IronDissolved, RMK_IronDissolved, 300), 'PWS_IronDissolved'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), IronTotal, RMK_IronTotal, 300), 'PWS_IronTotal'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), LeadTotal, RMK_LeadTotal, 15), 'PWS_LeadTotal'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), NickelTotal, RMK_NickelTotal, 610), 'PWS_NickelTotal'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), SeleniumTotal, RMK_SeleniumTotal, 170), 'PWS_SeleniumTotal'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), ThalliumTotal, RMK_ThalliumTotal, 0.24), 'PWS_ThalliumTotal'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), UraniumTotal, RMK_UraniumTotal, 30), 'PWS_UraniumTotal')) %>%
          dplyr::select(-ends_with('exceedanceRate')) }


      # chloride assessment if data exists
      if(nrow(filter(stationData(), !is.na(CHLORIDE_mg_L)))){
        chlorideFreshwater <- rollingWindowSummary(
          annualRollingExceedanceSummary(
            annualRollingExceedanceAnalysis(chlorideFreshwaterAnalysis(stationData()), yearsToRoll = 3, aquaticLifeUse = TRUE) ), "CHL")
      } else {
        chlorideFreshwater <- tibble(CHL_EXC = NA, CHL_STAT= NA)}

      # Water toxics combination with PWS, Chloride Freshwater, and water column PCB data
      if(nrow(bind_cols(PWSconcat,
                        chlorideFreshwater,
                        PCBmetalsDataExists(filter(markPCB, str_detect(SampleMedia, 'Water')) %>%
                                            filter(StationID %in% stationData()$FDT_STA_ID), 'WAT_TOX')) %>%
              dplyr::select(contains(c('_EXC','_STAT'))) %>%
              mutate(across( everything(),  as.character)) %>%
              pivot_longer(cols = contains(c('_EXC','_STAT')), names_to = 'parameter', values_to = 'values', values_drop_na = TRUE) %>%
              filter(! str_detect(values, 'WQS info missing from analysis')) %>%
              filter(! values %in% c("S", 0))) >= 1  ) {
        WCtoxics <- tibble(WAT_TOX_EXC = NA, WAT_TOX_STAT = 'Review',
                           PWSinfo = list(PWSconcat))# add in PWS information so you don't need to run this analysis again
      } else { WCtoxics <- tibble(WAT_TOX_EXC = NA, WAT_TOX_STAT = NA,
                                  PWSinfo = list(PWSconcat))}# add in PWS information so you don't need to run this analysis again
    } else { WCtoxics <- tibble(WAT_TOX_EXC = NA, WAT_TOX_STAT = NA,
                                PWSinfo = list(PWSconcat))}# add in PWS information so you don't need to run this analysis again
    return(WCtoxics) })


# Create station table row for each site
observe({
  req(nrow(ecoli()) > 0, nrow(enter()) > 0)# need to tell the app to wait for data to exist in these objects before smashing data together or will bomb out when switching between VAHU6's on the Watershed Selection Page
  siteData$stationTableOutput <- bind_rows(stationsTemplate,
                                           cbind(StationTableStartingData(stationData()),
                                                 tempExceedances(stationData()) %>% quickStats('TEMP'),
                                                 DOExceedances_Min(stationData()) %>% quickStats('DO'),
                                                 pHExceedances(stationData()) %>% quickStats('PH'),

                                                 # this runs the bacteria assessment again (unfortunately), but it suppresses
                                                 # any unnecessary bacteria fields for the stations table to avoid unnecessary flags
                                                 # and it only runs the 2 year analysis per IR2024 rules
                                                 bacteriaAssessmentDecisionClass( # NEW for IR2024, bacteria only assessed in two most recent years of assessment period
                                                   filter(stationData(), between(FDT_DATE_TIME, assessmentPeriod[1] + years(4), assessmentPeriod[2])),
                                                   uniqueStationName = unique(stationData()$FDT_STA_ID)),

                                                 rollingWindowSummary(
                                                   annualRollingExceedanceSummary(
                                                     annualRollingExceedanceAnalysis(ammoniaAnalysisStation(), yearsToRoll = 3, aquaticLifeUse = FALSE)), parameterAbbreviation = "AMMONIA"),

                                                 # Water Column Metals
                                                 metalsAssessmentFunction(WCmetalsStationAnalysisStation()$WCmetalsExceedanceSummary),

                                                 # takes too long to run this in app form, sourced from pre-analyzed data to expedite rendering time
                                                 # filter(WCmetalsForAnalysis, Station_Id %in%  stationData()$FDT_STA_ID) %>%
                                                 #   metalsAnalysis( stationData(), WER = 1) %>%
                                                 #   rename(FDT_STA_ID = Station_Id) %>%
                                                 #   mutate(`Criteria Type` = Criteria) %>%
                                                 #   annualRollingExceedanceAnalysis(yearsToRoll = 3, aquaticLifeUse = TRUE) %>%
                                                 #   annualRollingExceedanceSummary() %>%
                                                 #   metalsAssessmentFunction(),

                                                 # Water Toxics combo fields
                                                 waterToxics(),

                                                 # Roger's sediment metals analysis, transcribed
                                                 metalsData(filter(Smetals, Station_Id %in% stationData()$FDT_STA_ID), 'SED_MET'),
                                                 # Mark's sediment PCB results, flagged
                                                 PCBmetalsDataExists(filter(markPCB, str_detect(SampleMedia, 'Sediment')) %>%
                                                                       filter(StationID %in%  stationData()$FDT_STA_ID), 'SED_TOX'),
                                                 # Gabe's fish metals results, flagged
                                                 PCBmetalsDataExists(filter(fishMetals, Station_ID %in% stationData()$FDT_STA_ID), 'FISH_MET'),
                                                 # Gabe's fish PCB results, flagged
                                                 PCBmetalsDataExists(filter(fishPCB, `DEQ rivermile` %in%  stationData()$FDT_STA_ID), 'FISH_TOX'),
                                                 benthicAssessment(stationData(), VSCIresults),
                                                 countNutrients(stationData(), PHOSPHORUS_mg_L, LEVEL_PHOSPHORUS, NA) %>% quickStats('NUT_TP') %>%
                                                   mutate(NUT_TP_STAT = ifelse(NUT_TP_STAT != "S", "Review", NA)), # flag OE but don't show a real assessment decision
                                                 countNutrients(stationData(), CHLOROPHYLL_A_ug_L, LEVEL_CHLOROPHYLL_A, NA) %>% quickStats('NUT_CHLA') %>%
                                                   mutate(NUT_CHLA_STAT = NA)) %>% # don't show a real assessment decision) %>%
                                             left_join(dplyr::select(stationTable(), STATION_ID, COMMENTS),
                                                       by = 'STATION_ID') %>%
                                             dplyr::select(-ends_with(c('exceedanceRate','Assessment Decision', 'VERBOSE', 'StationID', "PWSinfo",
                                                                        'BACTERIADECISION', 'BACTERIASTATS')))) %>%
    filter(!is.na(STATION_ID))
  })

  # Display marked up station table row for each site
  output$stationTableDataSummary <- DT::renderDataTable({    req(stationData()$FDT_STA_ID == input$stationSelection, siteData$stationTableOutput)
    datatable(siteData$stationTableOutput, extensions = 'Buttons', escape=F, rownames = F, editable = TRUE,
              options= list(scrollX = TRUE, pageLength = nrow(siteData$stationTableOutput),
                            # adjust COMMENTS column width
                            autoWidth = TRUE, columnDefs = list(list(width = '400px', targets = c(71))), # DT starts counting at 0
                            dom='Bt', buttons=list('copy',
                                                   list(extend='csv',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                                   list(extend='excel',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep='')))),
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


  })

  ## Water Intake proximity flag for station
  output$intakeProximityFlag <- renderUI({req(stationData())
    if(unique(stationData()$FDT_STA_ID) %in% intakeSites$FDT_STA_ID){
      wellPanel(h5(strong('This station is within 100 meters of a drinking water intake. Please review whether the station
                should be assessed for secondary human health criteria.', style = "color:red")) ) }    })



  #### Data Sub Tab ####---------------------------------------------------------------------------------------------------

  # Display Data
  output$AURawData <- DT::renderDataTable({ req(AUData())
    DT::datatable(AUData(), extensions = 'Buttons', escape=F, rownames = F,
                  options= list(scrollX = TRUE, pageLength = nrow(AUData()), scrollY = "300px",
                                dom='Btf', buttons=list('copy',
                                                        list(extend='csv',filename=paste('AUData_',paste(input$stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                                        list(extend='excel',filename=paste('AUData_',paste(input$stationSelection, collapse = "_"),Sys.Date(),sep='')))),
                  selection = 'none')})
  # Summarize data
  output$stationDataTableRecords <- renderText({req(AUData())
    paste(nrow(AUData()), 'records were retrieved for',as.character(input$AUselection),sep=' ')})
  output$uniqueStationDataTableRecords <- renderTable({req(AUData())
    AUData() %>%
      group_by(FDT_STA_ID) %>%
      count() %>% dplyr::rename('Number of Records'='n') })
  output$stationDataTableAssessmentWindow <- renderText({req(AUData())
    withinAssessmentPeriod(AUData())})


  # Need this as a reactive to regenerate below modules when user changes station
  stationSelected <- reactive({input$stationSelection})


  ## Temperature Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(temperaturePlotlySingleStation, 'temperature', AUData, stationSelected, reactive(assessmentWindowLowFlows))

  ## pH Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(pHPlotlySingleStation,'pH', AUData, stationSelected, reactive(assessmentWindowLowFlows))

  ## DO Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(DOPlotlySingleStation,'DO', AUData, stationSelected, reactive(assessmentWindowLowFlows))

  ## Specific Conductivity Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(SpCondPlotlySingleStation,'SpCond', AUData, stationSelected)

  ## Salinity Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(salinityPlotlySingleStation,'salinity', AUData, stationSelected)

  ## Total Nitrogen Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(TNPlotlySingleStation,'TN', AUData, stationSelected)

  ## Ammonia Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(AmmoniaPlotlySingleStation,'Ammonia', AUData, stationSelected, ammoniaAnalysis)

  ## Total Phosphorus Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(TPPlotlySingleStation,'TP', AUData, stationSelected)

  ## Fecal Coliform Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(fecalPlotlySingleStation,'fecal', AUData, stationSelected)

  ## ECOLI Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(EcoliPlotlySingleStation,'Ecoli', AUData, stationSelected, ecoli)#siteData$ecoli)

  ## Enteroccoci Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(EnteroPlotlySingleStation,'Entero', AUData, stationSelected, enter)#siteData$enter)

  ## Chlorophyll a Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(chlAPlotlySingleStation,'chlA', AUData, stationSelected)

  ## Suspended Sediments Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(SSCPlotlySingleStation,'SSC', AUData, stationSelected)

  ## Nitrate Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(NitratePlotlySingleStation,'Nitrate', AUData, stationSelected)

  ## Chloride Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(ClPlotlySingleStation,'Cl', AUData, stationSelected)

  ## Sulfate Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(DSulfatePlotlySingleStation,'DSulfate', AUData, stationSelected)


  # Other Data Sources

  #### Benthics Sub Tab ####---------------------------------------------------------------------------------------------------
  callModule(BenthicsPlotlySingleStation,'Benthics', AUData, stationSelected, VSCIresults, VCPMI63results, VCPMI65results)

  # Bioassessment Tab- This is included at this level because couldn't figure the download piece out inside a module
  # empty reactive objects list
  reactive_objects = reactiveValues()

  assessmentDecision_UserSelection <- reactive({req(pinnedDecisions)
    filter(pinnedDecisions, StationID %in% input$stationSelection) %>%
      filter(IRYear == assessmentCycle) })  # only show information from current cycle here

  # Bioassesment information from current cycle
  output$bioassessmentInfo <- DT::renderDataTable({req(nrow(assessmentDecision_UserSelection())> 0)
    DT::datatable(assessmentDecision_UserSelection(),  escape=F, rownames = F,
                  options= list(dom= 't' , pageLength = nrow(assessmentDecision_UserSelection()),scrollX = TRUE, scrollY = "250px"),
                  selection = 'none')})

  observe({req(nrow(assessmentDecision_UserSelection())> 0)
    reactive_objects$SCI_UserSelection <- filter(VSCIresults, StationID %in% filter(assessmentDecision_UserSelection(), AssessmentMethod == 'VSCI')$StationID) %>%
      bind_rows(
        filter(VCPMI63results, StationID %in% filter(assessmentDecision_UserSelection(), AssessmentMethod == 'VCPMI63 + Chowan')$StationID)  ) %>%
      bind_rows(
        filter(VCPMI65results, StationID %in% filter(assessmentDecision_UserSelection(), AssessmentMethod == 'VCPMI65 - Chowan')$StationID)  ) %>%
      # only Current IR data
      filter(between(`Collection Date`,assessmentPeriod[1], assessmentPeriod[2])) %>% # limit data to assessment window
      # only use family level rarified data
      filter(`Target Count` == 110) %>%
      filter(RepNum %in% c('1', '2')) %>% # drop QA and wonky rep numbers
      filter(Gradient != "Boatable") %>%  # don't assess where no SCI not validated
      # add back in description information
      left_join(filter(benSamps, StationID %in% input$stationSelection) %>%
                  dplyr::select(StationID, Sta_Desc, BenSampID,US_L3CODE, US_L3NAME, HUC_12, VAHU6, Basin, Basin_Code),
                by = c('StationID', 'BenSampID')) %>%
      dplyr::select(StationID, Sta_Desc, BenSampID, `Collection Date`, RepNum, everything())

    reactive_objects$benSampsFilter <- filter(benSamps, BenSampID %in% reactive_objects$SCI_UserSelection$BenSampID)

    reactive_objects$habitatUserSelection <- habitatConsolidation( input$stationSelection, habSamps, habValues)  })


  # have to make separate reactive object in order to send appropriate station name to the download title
  fileNameForReport <- reactive({paste("IR",assessmentCycle," ", as.character(unique(input$stationSelection))," Benthic Assessment Fact Sheet.html", sep = "")})


  output$downloadReport_ <- renderUI({req(nrow(assessmentDecision_UserSelection())> 0)
    list(helpText('If you would like to download a copy of the Bioassessment Fact Sheet for the selected station,
                  click the Generate Report button below.'),
         downloadButton('downloadReport', 'Generate Report'))})

  output$downloadReport <- downloadHandler(
    filename = fileNameForReport,
    content= function(file){
      tempReport <- normalizePath('bioassessmentFactSheet.Rmd')
      imageToSend1 <- normalizePath('images/riskCategories.PNG') #NEW
      imageToSend2 <- normalizePath('images/HabitatColor.jpg') #NEW

      owd <- setwd(tempdir())
      on.exit(setwd(owd))

      file.copy(tempReport, 'bioassessmentFactSheet.Rmd')
      file.copy(imageToSend1, 'images/riskCategories.PNG') #NEW
      file.copy(imageToSend2, 'images/HabitatColor.jpg') #NEW

      params <- list(assessmentDecision =  assessmentDecision_UserSelection(),
                     SCI = reactive_objects$SCI_UserSelection,
                     benSampsFilter = reactive_objects$benSampsFilter,
                     habitat = reactive_objects$habitatUserSelection,
                     assessmentCycle = assessmentCycle)

      rmarkdown::render(tempReport,output_file = file,
                        params=params,envir=new.env(parent = globalenv()))})



  # Bioassesment information from previous cycles
  output$historicalBioassessmentInfo <- DT::renderDataTable({req(pinnedDecisions)
    z <- filter(pinnedDecisions, StationID %in% input$stationSelection)   %>%  # only show information from not current cycle here
      filter(IRYear != assessmentCycle) %>%
      arrange(IRYear)
    DT::datatable(z,  escape=F, rownames = F,
                  options= list(dom= 't' , pageLength = nrow(assessmentDecision_UserSelection()),scrollX = TRUE, scrollY = "250px"),
                  selection = 'none')})





  ### Metals Sub Tab ####---------------------------------------------------------------------------------------------------
  callModule(metalsTableSingleStation,'metals', AUData,  WCmetals, WCmetalsAnalyzed, Smetals, 
             fishMetals, fishMetalsScreeningValues, stationSelected, staticLimit)

 ### Toxics Sub Tab ####---------------------------------------------------------------------------------------------------
 callModule(toxicsSingleStation,'PBC',  AUData,  stationData, waterToxics, WCmetalsStationPWS,  reactive(intakeSites), markPCB, fishPCB, stationSelected)

})