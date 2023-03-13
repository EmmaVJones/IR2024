# work through appTesting.R through the creation of stationData object

TPPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(3,uiOutput(ns('oneStationSelectionUI'))),
               column(1),
               column(2,uiOutput(ns('changeLacustrineUI'))),
               column(2,helpText('The default Lacustrine Zone selection is what is designated in the user uploaded stations table.')),
               column(1),
               column(3,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText('All data presented in the interactive plot is raw data. Rounding rules are appropriately applied to the 
               assessment functions utilized by the application.'),
      plotlyOutput(ns('plotly')) ,
      br(), hr(), br(),
      fluidRow(
        column(7, h5('Annual Total Phosphorus median values for the ',span(strong('selected site')),' are displayed below.'),
               helpText('Note: Assessment is based on the two most recent monitoring years that data are available within the 
                        assessment window. A third sampling year may be needed if the two previous years result in differing 
                        assessment statuses. '),
               dataTableOutput(ns("annualMedianTableSingleSite"))),
        column(1),
        column(4, 
               h5('Total Phosphorus exceedance statistics for the ',span(strong('selected site')),' are highlighted below.
                  These are the results reflected in the stations table above.'),
               dataTableOutput(ns("stationExceedanceRate")))),
      br(),
      wellPanel(
        h4(strong('AU Assessment')),
        helpText("The guidance states: 'For lake or reservoir assessment units with multiple stations, 
        chlorophyll-a and total phosphorous data should be pooled by calculating the median of same-month 
        observations from April to October of any given year in a homogenous unit. The 90th percentile of monthly 
        chlorophyll-a medians is the value used to compare to the chlorophyll-a criterion for a particular 
        lake/reservoir.  The median of monthly TP medians should be used to assess against the applicable total 
        phosphorous criterion. '"),
        helpText("Manual adjustment of the `Lacustrine Zone Designation For Analysis` button above does not affect this
                          calculation. To change the designation of a station to Lacustrine for AU calculation purposes, adjust the
                          LACUSTRINE field in the Station Table uploaded to the application."),
        helpText(strong('This application provides an AU level assessment, but it is up to the assessor to determine 
                        if algaecides have been used in any part of the lake.')),
        fluidRow(
          column(7, h5('Annual Total Phosphorus median values for only stations that have an ID305B_1 matching
                       the ',span(strong('selected Assessment Unit')),' are displayed below.'),
                 dataTableOutput(ns('annualMedianTableAU'))),
          column(1),
          column(4, h5('Total Phosphorus exceedance statistics for the ',span(strong('Assessment Unit')),' are highlighted below.'),
                 dataTableOutput(ns("AUExceedanceRate"))))
      )
    ) )
}


TPPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove, AUselectionFromOutsideModal){
  ns <- session$ns
  
  # Select One station for individual review
  output$oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='200px', selected = stationSelectedAbove())})
  
  oneStation_original <- reactive({
    req(ns(input$oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$oneStationSelection) %>%
      filter(!is.na(PHOSPHORUS_mg_L))})
  
  # Option to change lacustrine designation and force function to calculate resutls
  output$changeLacustrineUI <- renderUI({
    req(oneStation_original())
    selectInput(ns('changeLacustrine'),strong('Lacustrine Zone Designation For Analysis'),
                choices= c('NA', 'Y'),
                width='400px', selected = unique(oneStation_original()$LACUSTRINE)) })
  
  # change WQS for rest of module if user chooses to do so
  oneStation <- reactive({req(oneStation_original(), input$changeLacustrine)
    mutate(oneStation_original(), LACUSTRINE = input$changeLacustrine) })
  
  # Button to visualize modal table of available parameter data
  observeEvent(input$reviewData,{
    showModal(modalDialog(
      title="Review Raw Data for Selected Station and Parameter",
      helpText('This table subsets the conventionals raw data by station selected in Single Station Visualization Section drop down and
               parameter currently reviewing. Scroll right to see the raw parameter values and any data collection comments. Data analyzed
               by app is highlighted in gray (all DEQ data and non agency/citizen monitoring Level III), data counted by app and noted in
               comment fields is highlighed in yellow (non agency/citizen monitoring Level II), and data NOT CONSIDERED in app is noted in
               orange (non agency/citizen monitoring Level I).'),
      DT::dataTableOutput(ns('parameterData')),
      size = 'l', easyClose = TRUE))  })
  
  # modal parameter data
  output$parameterData <- DT::renderDataTable({    req(oneStation_original())
    parameterFilter <- dplyr::select(oneStation_original(), FDT_STA_ID, GROUP_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_COMMENT,
                                     PHOSPHORUS_mg_L, RMK_PHOSPHORUS, LEVEL_PHOSPHORUS, LACUSTRINE, ThermoclineDepth, LakeStratification)
    
    DT::datatable(parameterFilter, rownames = FALSE, extensions = c('Buttons',  'FixedColumns'),
                  options= list(dom= 'Bt', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px",
                                fixedColumns = list(leftColumns = 3), buttons=list('copy')),
                  selection = 'none') %>%
      formatStyle(c('PHOSPHORUS_mg_L','RMK_PHOSPHORUS', 'LEVEL_PHOSPHORUS'), 'LEVEL_PHOSPHORUS', 
                  backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  output$plotly <- renderPlotly({
    req(input$oneStationSelection, oneStation())
    dat <- mutate(oneStation(), top = `Total Phosphorus (mg/L)`,
                  LakeStratification = replace_na(LakeStratification,"NA")) %>%
      mutate(LakeStratification = factor(LakeStratification,levels=c("Epilimnion",'NA',"Hypolimnion")))#,ordered=T)
    
    
    # Fix look of single measure
    if(nrow(dat) == 1){
      print('yes')
      dat <- bind_rows(dat,
                       tibble(SampleDate = c(dat$SampleDate- days(5), dat$SampleDate + days(5))))    }
    
    plot_ly(data=dat) %>%
      add_lines(data=dat, x=~SampleDate,y=~top, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text="Total Phosphorus Limit", name="Total Phosphorus Limit") %>%
      add_markers(data=dat, x= ~SampleDate, y= ~PHOSPHORUS_mg_L,mode = 'scatter', name="Total Phosphorus (mg/L)",
                  color=~LakeStratification, #marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Total Phosphorus: ",PHOSPHORUS_mg_L,"mg/L"),
                                               paste("Total Phosphorus Level: ",LEVEL_PHOSPHORUS),
                                               paste("LakeStratification: ",LakeStratification)))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Total Phosphorus (mg/L)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) })
  
  output$annualMedianTableSingleSite <- renderDataTable({req(oneStation())
    z <- TP_analysis(oneStation()) %>%
      rename('Total Phosphorus Limit' = 'Total Phosphorus (mg/L)')
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'),
              selection = 'none') })
  
  output$stationExceedanceRate <- renderDataTable({req(oneStation())
    z <- TP_Assessment(oneStation())
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "70px", dom='t'),
              selection = 'none') })
  
  
  # AU Assessment Section
  justAUID305B_1 <- reactive({
    filter(AUdata(), ID305B_1 %in% AUselectionFromOutsideModal()) %>%
      filter(!is.na(PHOSPHORUS_mg_L))})
  
  output$annualMedianTableAU <- renderDataTable({req(justAUID305B_1())
    z <- TP_analysis(justAUID305B_1()) %>%
      rename('Total Phosphorus Limit' = 'Total Phosphorus (mg/L)')
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'),
              selection = 'none') })
  
  output$AUExceedanceRate <- renderDataTable({req(justAUID305B_1())
    z <- TP_Assessment(justAUID305B_1())
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "70px", dom='t'),
              selection = 'none') })
  
}






ui <- fluidPage(
  uiOutput('AUselection_'),
  h5(strong('AU information from last cycle')),
  #  DT::dataTableOutput('selectedAU'),br(),
  uiOutput('stationSelection_'),
  TPPlotlySingleStationUI('TP'))

server <- function(input,output,session){
  # for testing
  stationTable <- reactive({  
    
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
    return(stationTable)  })
  
  
  
  # Pull AU data from server
  # for testing
  # Pull AU data from server
  regionalAUs <- reactive({# req(input$pullAUs)
    withProgress(message = 'Reading in Large Spatial File',
                 st_zm(st_as_sf(pin_get('AUreservoir', board = 'rsconnect')) ) %>%
                   lakeNameStandardization()) })  
  
  
  # Pull Conventionals data for selected lake on click
  conventionalsLake <- reactive({#eventReactive( input$pullHUCdata, {
    filter(conventionals, FDT_STA_ID %in% stationSelectionOptions) %>% #lake_filter1$STATION_ID) %>%
      left_join(dplyr::select(stationTable(), STATION_ID:VAHU6, lakeStation,
                              WQS_ID:`Total Phosphorus (mg/L)`),
                #WQS_ID:`Max Temperature (C)`), 
                by = c('FDT_STA_ID' = 'STATION_ID')) %>%
      filter(!is.na(ID305B_1)) %>%
      # Special Standards Correction step. This is done on the actual data bc some special standards have temporal components
      pHSpecialStandardsCorrection() %>% # correct pH to special standards where necessary
      temperatureSpecialStandardsCorrection() %>% # correct temperature special standards where necessary
      thermoclineDepth() }) # adds thermocline information and SampleDate
  
  output$AUselection_ <- renderUI({ req(conventionalsLake())
    selectInput('AUselection', 'Assessment Unit Selection', choices = unique(conventionalsLake()$ID305B_1))  })
  
  output$selectedAU <- DT::renderDataTable({req(conventionalsLake(),input$AUselection)
    z <- filter(regionalAUs(), ID305B %in% input$AUselection) %>% st_set_geometry(NULL) %>% as.data.frame()
    datatable(z, rownames = FALSE, 
              options= list(pageLength = nrow(z),scrollX = TRUE, scrollY = "300px", dom='t'))})
  
  output$stationSelection_ <- renderUI({ req(conventionalsLake(), input$AUselection)
    z <- filter(conventionalsLake(), ID305B_1 %in% input$AUselection | ID305B_2 %in% input$AUselection | 
                  ID305B_3 %in% input$AUselection | ID305B_4 %in% input$AUselection | ID305B_5 %in% input$AUselection | 
                  ID305B_6 %in% input$AUselection | ID305B_7 %in% input$AUselection | ID305B_8 %in% input$AUselection | 
                  ID305B_9 %in% input$AUselection | ID305B_10 %in% input$AUselection) %>%
      distinct(FDT_STA_ID)
    fluidRow(selectInput('stationSelection', 'Station Selection', choices = unique(z$FDT_STA_ID)),
             helpText("The stations available in the drop down are limited to stations with an ID305B_1:ID305B_10 designation equal 
                      to the selected AU. All AU's associated with the selected station can be viewed in the map below."))})
  
  
  
  AUData <- eventReactive( input$AUselection, {
    filter_at(conventionalsLake(), vars(starts_with("ID305B")), any_vars(. %in% input$AUselection) ) }) 
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData(), FDT_STA_ID %in% input$stationSelection) })
  
  stationSelected <- reactive({input$stationSelection})
  
  
  ## For Nutrients
  AUselection <- reactive({as.character(input$AUselection)})
  
  
  ## Total Phosphorus Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(TPPlotlySingleStation,'TP', AUData, stationSelected, AUselection)
  
}

shinyApp(ui,server)
