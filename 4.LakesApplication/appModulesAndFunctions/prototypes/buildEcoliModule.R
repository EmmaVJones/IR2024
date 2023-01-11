# work through appTesting.R through the creation of stationData object



EcoliPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(6, helpText('Station used for this module is the station selected above to expedite app rendering.')),#uiOutput(ns('oneStationSelectionUI'))),
               column(6,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText('All data presented in the interactive plot is raw data. Rounding rules are appropriately applied to the 
               assessment functions utilized by the application. The gray box indicates the data window valid for recreational use assessment for the current 
               assessment period.'),
      plotlyOutput(ns('plotly')),
      br(),hr(),br(),
      h4(strong('New Standard (STV= 410 CFU / 100 mL, geomean = 126 CFU / 100 mL with additional sampling requirements)')), 
      h5('All E. coli records that are ',span(strong('above the criteria')),' for the ',
         span(strong('selected site collected during the two most recent calendar years of the assessment period')),' are highlighted below.',
         span(strong('If no data are reflected in below tables then no data are available for analysis in the most recent two years
                       of the assessment period.'))),
      fluidRow(
        column(6, helpText('The below table highlights all analyzed windows ',
                           span(strong('within the two most recent calendar years of the assessment period')),
                           ' that have either STV violations OR geomean violations. Note
                        the number of samples in the window, STV Assessment, and Geomean Assessment columns for context. These violations
                        are important to understand the dataset, but the verbose assessment decision in the table to the right is where one should look
                        for assistance choosing which of the potential violations are driving the decision. Explore the dataset in 
                        90 day windows in the interactive graph below and the full dataset with assessment decisions paired with each window
                        in the table below.'),
               DT::dataTableOutput(ns('exceedancesNEWStdTableSingleSite')),
               br()),
        column(6, #br(), br(),br(), br(),br(), br(),br(), br(),
               h5('Individual E. coli exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               h5("Note: This analysis is based only on data collected during the ",span(strong('two most recent calendar years of the six-year assessment window')),". For the ",
                  assessmentCycle, " IR cycle this would mean only bacteria data collected between January 1, ",
                  year(assessmentPeriod[1] + years(4)),
                  " and  December 31, ", year(assessmentPeriod[2]), " is assessed for recreation use support."),
               DT::dataTableOutput(ns("newStdTableSingleSite")),
               h4(strong('See below section for detailed analysis with new recreation standard.')),
               br())),
      br(),hr(),
      h4(strong('New Recreation Standard In Depth Analysis')),
      helpText('Review the 90 day windows (identified by each sample date) for STV and geomean exceedances.
               Comments are specific to each row of data. To view the dataset within each 90 day window, use
               the drop down box to select the start of the window in question.'),
      fluidRow(
        #verbatimTextOutput(ns('test')),
        column(4, helpText('Below is the raw data associated with the ',span('selected site'),'. Click on a row to reveal the
                           data included in the selected 90 day window in the plot to the right and to highlight the specific 
                           assessment logic in the table below the plot. ',
                           span(strong('Data valid for recreational use assessment are colored in gray.'))), 
               h5(strong('Raw Data')),DT::dataTableOutput(ns('rawData'))),
        column(8, helpText('Click a row on the table to left to reveal a detailed interactive plot of the data
                           included in the selected 90 day window. The orange line corresponds to the window geomean; wide black dashed line
                         corresponds to the geomean criteria; thin black dashed line corresponds to the STV limit. Below the plot is a
                           table with specific assessment logic regarding the data included in the selected 90 day window.'),
               plotlyOutput(ns('plotlyZoom')),
               DT::dataTableOutput(ns("analysisTableZoom")))),
      br(), br(),
      h5(strong('Analyzed Data (Each window with an individual STV and geomean assessment decisions)')),
      helpText('This dataset shows all assessment logic for each 90 day window assessment.'),
      helpText('Per assessment guidance, each 90-day period that is assessed should be evaluated using a dataset containing at least one sample that is not 
               used in a preceding or subsequent 90-day period. In practice, all data collected begin a rolling 90-day period; however, the field called 
               `Valid Assessment Window` indicates whether a given window contains unique data for analysis. All suggested assessment decisions are only based
               upon data where `Valid Assessment Window` are true.'),
      DT::dataTableOutput(ns('analysisTable'))),
    hr(), br(),
    fluidRow(
      h4(strong('Old Standard (Single Sample Maximum = 235 CFU / 100 mL, Monthly Geomean = 126 CFU / 100 mL)')),
      column(6,
             h5('All E. coli records that are ',span(strong('above the criteria')),' for the ',
                span(strong('selected site')),' are highlighted below.',
                span(strong('If no data are reflected in below tables then no data exceeded the respective criteria.'))),
             h4(strong('Single Sample Maximum = 235 CFU / 100 mL')),
             DT::dataTableOutput(ns('exceedancesOldStdTableSingleSiteSTV')), 
             h4(strong('Monthly Geomean = 126 CFU / 100 mL')),
             DT::dataTableOutput(ns('exceedancesOldStdTableSingleSitegeomean')),
             br()),
      column(6,
             br(),br(),br(),
             h5('Individual E. coli exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
             DT::dataTableOutput(ns("oldStdTableSingleSite"))))
  )
}


EcoliPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove, analyzedData){
  ns <- session$ns
 
  oneStation <- reactive({
    filter(AUdata(), FDT_STA_ID %in% as.character(stationSelectedAbove())) %>% #input$oneStationSelection) %>%
      filter(!is.na(ECOLI))})
  
  # Bring in pre analyzed data to expedite process
  oneStationAnalysis <- reactive({analyzedData()})# bc not updating in full app unless this is reactive
  oneStationDecisionData <- reactive({oneStationAnalysis()[['associatedDecisionData']][[1]]}) # bc not updating in full app unless this is reactive
  oneStationRecreationDecisionData <- reactive({req(oneStation())
    bacteriaAssessmentDecision( # NEW for IR2024, bacteria only assessed in two most recent years of assessment period
      filter(oneStation(), between(FDT_DATE_TIME, assessmentPeriod[1] + years(4), assessmentPeriod[2])), 'ECOLI', 'LEVEL_ECOLI', 10, 410, 126)     })
  
  
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
  output$parameterData <- DT::renderDataTable({
    req(oneStation())
    parameterFilter <- dplyr::select(oneStation(), FDT_STA_ID, GROUP_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_COMMENT,
                                     ECOLI, RMK_ECOLI, LEVEL_ECOLI,
                                     ThermoclineDepth, LakeStratification)
    
    DT::datatable(parameterFilter, rownames = FALSE, extensions = c('Buttons',  'FixedColumns'),
                  options= list(dom= 'Bt', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px",
                                fixedColumns = list(leftColumns = 3), buttons=list('copy')),
                  selection = 'none') %>%
      formatStyle(c('ECOLI','RMK_ECOLI', 'LEVEL_ECOLI'), 'LEVEL_ECOLI', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  output$plotly <- renderPlotly({    req(oneStation())
    dat <- oneStation() %>%
      mutate(newSTV = 410, geomean = 126, oldSTV = 235)
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y")
    
    if(max(dat$ECOLI, na.rm = T) * 1.1 > 410){boxHeight <- max(dat$ECOLI, na.rm = T) * 1.1}else{boxHeight <- 410}
    box1 <- data.frame(x = c(assessmentPeriod[1]+ years(4), assessmentPeriod[1]+ years(4), assessmentPeriod[2],assessmentPeriod[2]),
                       y = c(0, boxHeight, boxHeight, 0))
    
    plot_ly(data=dat) %>%
      add_polygons(data = box1, x = ~x, y = ~y, fillcolor = "#949391",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Most Recent Two Years of Assessment Period')) %>%
      add_markers(data=dat, x= ~SampleDate, y= ~ECOLI,mode = 'scatter', name="E. coli (CFU / 100 mL)", marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("E. coli: ",ECOLI,"CFU / 100 mL")))%>%
      add_lines(data=dat, x=~SampleDate,y=~newSTV, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                hoverinfo = "text", text= "New STV: 410 CFU / 100 mL", name="New STV: 410 CFU / 100 mL") %>%
      add_lines(data=dat, x=~SampleDate,y=~oldSTV, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text= "Old SSM: 235 CFU / 100 mL", name="Old SSM: 235 CFU / 100 mL") %>%
      add_lines(data=dat, x=~SampleDate,y=~geomean, mode='line', line = list(color = 'black', dash= 'dash'),
                hoverinfo = "text", text= "Geomean Criteria: 126 CFU / 100 mL", name="Geomean Criteria: 126 CFU / 100 mL") %>%
      layout(showlegend=FALSE,
             yaxis=list(title="E. coli (CFU / 100 mL)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
  })
  
  
  ### New standard ----------------------------------------------------------------------------------
  
  output$exceedancesNEWStdTableSingleSite <- DT::renderDataTable({ req(oneStation(),!is.na(oneStationRecreationDecisionData()))
    z <- oneStationRecreationDecisionData()[['associatedDecisionData']][[1]] %>% 
      filter(`STV Exceedances In Window` > 0 | `Geomean In Window` > 126) %>%
      dplyr::select(-associatedData) %>% # remove embedded tibble to make table work
      mutate(`Date Window Starts` = as.Date(`Date Window Starts`),
             `Date Window Ends` = as.Date(`Date Window Ends`))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='ti'), selection = 'none')  })
  
  
  output$newStdTableSingleSite <- DT::renderDataTable({  req(oneStationRecreationDecisionData())
    z <- oneStationRecreationDecisionData() %>%
      dplyr::select(ECOLI_EXC:ECOLI_GM_SAMP, 'Verbose Assessment Decision' = ECOLI_STATECOLI_VERBOSE) #only grab decision
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'), selection = 'none') })
  
  
 
  ### Raw Data and Individual window analysis
  
  output$rawData <- DT::renderDataTable({    req(oneStation())
    z <- dplyr::select(oneStation(), FDT_STA_ID, FDT_DATE_TIME, ECOLI, RMK_ECOLI, LEVEL_ECOLI)  %>% 
      mutate(FDT_DATE_TIME = as.Date(FDT_DATE_TIME, format = '%Y-%m-%D %H:%M:S'),
             RecValid = ifelse(FDT_DATE_TIME >= assessmentPeriod[1] + years(4), 1, NA))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "400px", dom='ti',
                                                     columnDefs = list(list(targets = 5, visible = FALSE))), # hide RecValid field
                  selection = 'single')  %>% 
      formatStyle('RecValid',  target = 'row', backgroundColor = styleEqual(c(1), c('lightgray')))})
  
  
  windowData <- reactive({ req(oneStation(), input$rawData_rows_selected, !is.na(oneStationDecisionData()))
    windowDat <- filter(oneStationDecisionData(), as.character(`Date Window Starts`) %in% as.character(as.Date(oneStation()$FDT_DATE_TIME[input$rawData_rows_selected]))) %>% #input$windowChoice_) %>%
      dplyr::select( associatedData) %>%
      unnest(cols = c(associatedData)) %>%
      mutate(newSTV = 410, geomeanLimit = 126,
             `Date Time` = as.POSIXct(strptime(FDT_DATE_TIME, format="%Y-%m-%d")))
    bind_rows(windowDat,
              tibble(`Date Time` = c(min(windowDat$`Date Time`)- days(5), max(windowDat$`Date Time`) + days(5)),
                     newSTV = 410, geomeanLimit = 126))  })
  

  output$plotlyZoom <- renderPlotly({ req(windowData(), oneStation(), !is.na(oneStationDecisionData()))
    
    windowData <- windowData()
    
    plot_ly(data=windowData) %>%
      add_markers(x= ~`Date Time`, y= ~Value,mode = 'scatter', name="E. coli (CFU / 100 mL)", marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",`Date Time`),
                                               paste("E. coli: ",Value,"CFU / 100 mL"))) %>%
      add_lines(data=windowData, x=~`Date Time`, y=~geomean, mode='line', line = list(color = 'orange', dash= 'dash'),
                hoverinfo = "text", text= ~paste("Window Geomean: ", format(geomean,digits=3)," CFU / 100 mL", sep=''), 
                name="Window Geomean") %>%
      add_lines(data=windowData, x=~`Date Time`,y=~newSTV, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                hoverinfo = "text", text= "New STV: 410 CFU / 100 mL", name="New STV: 410 CFU / 100 mL") %>%
      add_lines(data=windowData, x=~`Date Time`,y=~geomeanLimit, mode='line', line = list(color = 'black', dash= 'dash'),
                hoverinfo = "text", text= "Geomean Criteria: 126 CFU / 100 mL", name="Geomean Criteria: 126 CFU / 100 mL") %>%
      layout(showlegend=FALSE,
             yaxis=list(title="E. coli (CFU / 100 mL)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
  })
  
  output$analysisTableZoom <- DT::renderDataTable({    req(!is.na(oneStationDecisionData()), nrow(windowData()) > 0)
    z <- oneStationDecisionData()[input$rawData_rows_selected,] %>%
      dplyr::select(-associatedData) # remove embedded tibble to make table work
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "140px", dom='t'), selection = 'none')  })
  
  
  
  output$analysisTable <- DT::renderDataTable({    req(!is.na(oneStationDecisionData()))
    z <- oneStationDecisionData() %>%
      dplyr::select(-associatedData) %>% # remove embedded tibble to make table work
      mutate(RecValid = ifelse(`Date Window Starts` >= assessmentPeriod[1] + years(4), 1, NA))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "400px", dom='ti'), 
                  selection = 'none')   %>% 
      formatStyle('RecValid',  target = 'row', backgroundColor = styleEqual(c(1), c('lightgray')))})
  
  
  #### Old Standard ---------------------------------------------------------------------------------
  output$exceedancesOldStdTableSingleSiteSTV <- DT::renderDataTable({req(oneStation())
    z <- bacteria_ExceedancesSTV_OLD(oneStation() %>%
                                       dplyr::select(FDT_DATE_TIME, ECOLI)%>% # Just get relevant columns, 
                                       filter(!is.na(ECOLI)) #get rid of NA's
                                     , 235 ) %>%
      filter(exceeds == T) %>%
      mutate(FDT_DATE_TIME = as.Date(FDT_DATE_TIME), ECOLI = parameter) %>%
      dplyr::select(FDT_DATE_TIME, ECOLI, limit, exceeds)
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "150px", dom='ti'), selection = 'none') })
  
  output$exceedancesOldStdTableSingleSitegeomean <- DT::renderDataTable({    req(oneStation())
    z <- bacteria_ExceedancesGeomeanOLD(oneStation() %>% 
                                          dplyr::select(FDT_DATE_TIME,ECOLI) %>% # Just get relevant columns, 
                                          filter(!is.na(ECOLI)), #get rid of NA's
                                        'ECOLI', 126) 
    if(!is.null(z)){
      z <- z %>%
        dplyr::select(FDT_DATE_TIME, ECOLI, sampleMonthYear, geoMeanCalendarMonth, limit, samplesPerMonth) %>%
        filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) # minimum sampling rule for geomean to apply
    } else {z <- tibble(FDT_DATE_TIME = NA, `ECOLI` = NA, sampleMonthYear= NA, geoMeanCalendarMonth= NA, limit= NA, samplesPerMonth= NA)}
    
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "100px", dom='t'), selection = 'none') })
  
  output$oldStdTableSingleSite <- DT::renderDataTable({req(oneStation())
    #get rid of citizen data
    z1 <- filter(oneStation(), !(LEVEL_ECOLI %in% c('Level II', 'Level I')))
    if(nrow(z1) > 1){
      z <- bacteria_Assessment_OLD(z1,  'ECOLI', 126, 235)
      if(nrow(z) > 0 ){
        z <- dplyr::select(z, `Assessment Method`,everything()) }
      DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'), selection = 'none')
    }
  })
  
  
}





ui <- fluidPage(
  #verbatimTextOutput('testtest'),
  helpText('Review each site using the single site visualization section. There are no WQS for Total Nitrogen.'),
  uiOutput('AUselection_'),
  h5(strong('AU information from last cycle')),
  #  DT::dataTableOutput('selectedAU'),br(),
  uiOutput('stationSelection_'),
  tabsetPanel(
    
    tabPanel('Assessment Unit Analysis',
             EcoliPlotlyAUUI('EcoliAU')),
    # really should be first but second for testing
    tabPanel('Single Station Analysis',
             EcoliPlotlySingleStationUI('Ecoli')))
)

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
  
  ecoli <- reactive({req(stationData())
    bacteriaAssessmentDecision(stationData(), 'ECOLI', 'LEVEL_ECOLI', 10, 410, 126)})
  
  # save individual ecoli results for later
  AUmedians <- reactive({ req(AUData(), input$AUselection)
    AUData() %>%
      filter(ID305B_1 %in% input$AUselection) %>%# run ecoli by only 1 AU at a time
      group_by(SampleDate) %>%
      filter(!is.na(ECOLI)) %>%
      mutate(EcoliDailyMedian = median(ECOLI, na.rm = TRUE)) %>%
      dplyr::select(ID305B_1, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, SampleDate, EcoliDailyMedian, ECOLI, RMK_ECOLI, LEVEL_ECOLI) %>%
      arrange(SampleDate) %>% ungroup() })
  
  # need to run analysis on only one point per day
  AUmediansForAnalysis <- reactive({req(AUmedians())
    AUmedians() %>% 
      filter(! LEVEL_ECOLI %in% c('Level I', 'Level II')) %>%
      mutate(ECOLI_Station = ECOLI,
             StationName = FDT_STA_ID,
             ECOLI = EcoliDailyMedian,
             FDT_STA_ID = unique(ID305B_1),
             FDT_DATE_TIME = SampleDate) %>%
      dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, SampleDate, ECOLI, StationName, ECOLI_Station, RMK_ECOLI, LEVEL_ECOLI) }) # dont run distinct here so extra station data can go to ecoli module

  
  #output$testtest <- renderPrint({AUmedians()})
  
  ecoliAU <- reactive({req(AUmediansForAnalysis())
    bacteriaAssessmentDecision(AUmediansForAnalysis() %>%
                                 distinct(SampleDate, .keep_all = T) , 'ECOLI', 'LEVEL_ECOLI', 10, 410, 126)})
  
  # single station tab
  callModule(EcoliPlotlySingleStation,'Ecoli', AUData, stationSelected, ecoli)
  
  callModule(EcoliPlotlyAU,'EcoliAU', AUData, AUmedians, AUmediansForAnalysis, ecoliAU)
  
  
  
    
}

shinyApp(ui,server)



