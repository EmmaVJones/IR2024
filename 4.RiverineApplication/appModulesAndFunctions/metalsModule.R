
# work through appTesting.R through the creation of stationData object

metalsTableSingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel('Water Column Metals',
               wellPanel(
                 h4(strong('Single Station Data Visualization')),
                 uiOutput(ns('WCmetals_oneStationSelectionUI')),
                 tabsetPanel(
                   tabPanel('Analyzed Data',
                            br(),
                            fluidRow(
                              column(2, numericInput(ns('WER'), strong('Water Effects Ratio (WER) used for module analysis'), value = 1)),
                              column(1),
                              column(9,  h5('A summary of the exceedances of water column metals criteria available for the ',span(strong('selected site')),' and user
                                            input WER are available below. If no data is presented, then the station does not have any water 
                                            column metals data available. Hardness based criteria are calculated as applicable.'),
                                     h5('Dissolved metal values compared to PWS criteria can be viewed in the lower two tables if a station is attributed to a PWS
                                        segment. See the Toxics tab for more information on total metal values compared to PWS criteria.')) ),
                            DT::dataTableOutput(ns('WCmetalsSingleSiteSummary')),
                            # verbatimTextOutput(ns('testtest')),
                            hr(), 
                            h5('All 3 year rolled window water column metals results for the for the ',span(strong('selected site')),' are highlighted below. 
                               Click on a row to show all the data contained within the chosen window in the table to the right.'),
                            helpText("For toxic pollutant assessment of Aquatic Life Designated Use in free-flowing streams, both chronic and acute criteria can be assessed whenever 
                                     sufficient data are available as applicable.  Chronic criteria are to be assessed when multiple grab samples are collected within two separate 
                                     four-day periods within a three-year period, or when there are two or more separate 30-day SPMD deployments within a three-year period.  
                                     Two samples (either grab or SPMD) taken within three consecutive years are sufficient to assess acute criteria."),
                            fluidRow(column(6, h5("Three year window summaries"),
                                            dataTableOutput(ns('stationRolledExceedanceRate'))),
                                     column(6, h5("All data within chosen three year window. Select a row to your left to reveal data analyzed within the chosen window/criteria combination."),
                                            dataTableOutput(ns('detailedStationRolledExceedanceRate')))),
                            br()),
                   tabPanel('Raw Data',
                            h5('All water column metals data available for the ',span(strong('selected site')),' are available below. 
                               If no data is presented, then the station does not have any water column metals data available.'),
                            DT::dataTableOutput(ns('WCmetalsRangeTableSingleSite')))))),
      tabPanel('Sediment Metals',
               wellPanel(
                 h4(strong('Single Station Data Visualization')),
                 uiOutput(ns('Smetals_oneStationSelectionUI')),
                 tabsetPanel(
                   tabPanel('Raw Data',
                            h5('All sediment metals data available for the ',span(strong('selected site')),' are available below. 
                               If no data is presented, then the station does not have any sediment metals data available.'),
                            DT::dataTableOutput(ns('SmetalsRangeTableSingleSite')))))),
      tabPanel('Fish Tissue Metals',
               wellPanel(
                 h4(strong('Single Station Data Visualization')),
                 uiOutput(ns('Fmetals_oneStationSelectionUI')),
                 #verbatimTextOutput(ns('test')),
                 h5('All fish tissue metals exceedances for the ',span(strong('selected site')),' are available below. 
                    If no data is presented, then the station does not have any fish tissue metals exceedances.'),
                 DT::dataTableOutput(ns('Fmetals_exceedance')),br(),
                 h5('All fish tissue metals data available for the ',span(strong('selected site')),' are available below. 
                    If no data is presented, then the station does not have any fish tissue metals data available.'),
                 helpText('All concentrations expressed as ppm (mg/kg), wet weight, in edible fish tissue fillet'),
                 DT::dataTableOutput(ns('FmetalsRangeTableSingleSite'))) )
      
    ))
}


metalsTableSingleStation <- function(input,output,session, AUdata, WCmetals , WCmetalsAnalyzed, Smetals,  Fmetals, 
                                     metalsSV, stationSelectedAbove, staticLimit){
  ns <- session$ns
  
  ## Water Column Metals
  
  # Select One station for individual review
  output$WCmetals_oneStationSelectionUI <- renderUI({    req(stationSelectedAbove)
    selectInput(ns('WCmetals_oneStationSelection'),strong('Select Station to Review') ,choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),
                width='300px', selected = stationSelectedAbove())})
  
  WCmetals_oneStation <- reactive({req(ns(input$WCmetals_oneStationSelection))
    filter(WCmetals, Station_Id %in% input$WCmetals_oneStationSelection)})
  
  WCmetals_oneStationForAnalysis <- reactive({req(ns(input$WCmetals_oneStationSelection), nrow(WCmetals_oneStation()) > 0)
    filter(WCmetalsAnalyzed, StationID %in% input$WCmetals_oneStationSelection) %>%
      map(1)  })
  
  # Extract metals analysis or calculate based on user input
  WCmetals_oneStationAnalysis <- reactive({req(WCmetals_oneStationForAnalysis(), input$WER, nrow(WCmetals_oneStation()) > 0)
    if(input$WER == 1){
      WCmetals_oneStationForAnalysis()$WCmetalsExceedanceAnalysis
    } else {
      stationData <- filter(AUdata(), FDT_STA_ID %in% input$WCmetals_oneStationSelection)
      WCmetals_oneStation() %>% 
        metalsAnalysis(stationData , WER = input$WER) %>% 
        rename(FDT_STA_ID = Station_Id) %>% 
        mutate(`Criteria Type` = Criteria) %>% 
        annualRollingExceedanceAnalysis(yearsToRoll = 3, aquaticLifeUse = TRUE)
    } })
  
  # output$testtest <- renderPrint({WCmetals_oneStationAnalysis()})
  
  
  
  WCmetals_oneStationAssessment <- reactive({req(WCmetals_oneStationForAnalysis(), input$WER)
    if(input$WER == 1){
      WCmetals_oneStationForAnalysis()$WCmetalsExceedanceSummary
    } else {
      annualRollingExceedanceSummary(WCmetals_oneStationAnalysis())     } })
  
  output$WCmetalsSingleSiteSummary <- DT::renderDataTable({req(WCmetals_oneStationAssessment())
    DT::datatable(WCmetals_oneStationAssessment() %>% dplyr::select(-FDT_DEPTH),
                  rownames = FALSE,extensions = 'Buttons',
                  options= list(scrollX = TRUE, pageLength = nrow(WCmetals_oneStationAssessment()), scrollY = "400px", dom='Bt',
                                buttons=list('copy',
                                             list(extend='csv',filename=paste('WCmetalsSummary_',paste(assessmentCycle,input$WCmetals_oneStationSelection, collapse = "_"),Sys.Date(),sep='')),
                                             list(extend='excel',filename=paste('WCmetalsSummary_',paste(assessmentCycle,input$WCmetals_oneStationSelection, collapse = "_"),Sys.Date(),sep='')))),
                  selection = 'none')     })
  
  
  
  ## 3 year window summaries by criteria
  output$stationRolledExceedanceRate <- renderDataTable({   req(nrow(WCmetals_oneStation()) > 0,  WCmetals_oneStationAnalysis() )
    z <- WCmetals_oneStationAnalysis() %>% 
      dplyr::select(-c(associatedData))
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'),
              selection = 'single') })
  
  ## Data from selected window for detailed table
  output$detailedStationRolledExceedanceRate <- renderDataTable({   req(nrow(WCmetals_oneStation()) > 0, input$stationRolledExceedanceRate_rows_selected)
    z <- WCmetals_oneStationAnalysis()[input$stationRolledExceedanceRate_rows_selected, ] %>%
      map_df(1)
    z <- z$associatedData %>%
      #dplyr::select(-c(associatedData)) %>%
      rename(#"Chloride Average Value"  = "Value",
        'Parameter Rounded to WQS Format' = parameterRound) %>%
      dplyr::select(-`Valid Window`)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'),
              selection = 'none') %>%
      formatSignif(columns=c('CriteriaValue', 'Parameter Rounded to WQS Format'), digits=2)})
  
  
  
  
  # Raw data
  output$WCmetalsRangeTableSingleSite <- DT::renderDataTable({req(WCmetals_oneStation())
    z <- WCmetals_oneStation()
    #z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(z, rownames = FALSE,extensions = 'Buttons',
                  options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='Bt',
                                buttons=list('copy',
                                             list(extend='csv',filename=paste('WCmetalsRaw_',paste(assessmentCycle,input$WCmetals_oneStationSelection, collapse = "_"),Sys.Date(),sep='')),
                                             list(extend='excel',filename=paste('WCmetalsRaw_',paste(assessmentCycle,input$WCmetals_oneStationSelection, collapse = "_"),Sys.Date(),sep='')))),
                  selection = 'none')     })
  
  
  
  
  
  
  ## Sediment Metals
  
  # Select One station for individual review
  output$Smetals_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('Smetals_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  output$test <- renderPrint({stationSelectedAbove()})
  
  Smetals_oneStation <- reactive({
    req(ns(input$Smetals_oneStationSelection))
    filter(Smetals, Station_Id %in% input$Smetals_oneStationSelection)})
  
  output$SmetalsRangeTableSingleSite <- DT::renderDataTable({req(Smetals_oneStation())
    z <- Smetals_oneStation()
    z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'),
                  selection = 'none')     })
  
  
  
  
  
  ## Fish Tissue Metals
  
  output$Fmetals_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('Fmetals_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  
  Fmetals_oneStation <- reactive({req(ns(input$Fmetals_oneStationSelection))
    filter(Fmetals, Station_ID %in% input$Fmetals_oneStationSelection)})
  
  output$Fmetals_exceedance <- DT::renderDataTable({req(Fmetals_oneStation())
    FmetalsSV <- dplyr::select(Fmetals_oneStation(), Station_ID, Collection_Date_Time, Sample_ID,  `# of Fish`, Species_Name, length, weight, Beryllium:Lead) %>%
      dplyr::select(-contains('RMK_')) %>%
      group_by( Station_ID, Collection_Date_Time, Sample_ID, `# of Fish`, Species_Name, length, weight) %>%
      pivot_longer(cols= Beryllium:Lead, names_to = "Metal", values_to = 'Measure') %>%
      left_join(filter(metalsSV, !str_detect(`Screening Method`, 'Practical')),
                by = 'Metal') %>% 
      filter(Measure > `Screening Value`) %>%
      arrange(Metal)
    DT::datatable(FmetalsSV, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(FmetalsSV),
                                                             scrollY = "250px", dom='Bti', buttons=list('copy')), selection = 'none') %>%
      formatSignif(columns=c('Screening Value'), digits=2)      })
  
  
  output$FmetalsRangeTableSingleSite <- DT::renderDataTable({ req(input$Fmetals_oneStationSelection, Fmetals_oneStation())
    # z <- dplyr::select(Smetals_oneStation(), FDT_STA_ID, `FDT_DATE_TIME`,ARSENIC:COMMENT)
    # z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(Fmetals_oneStation(), rownames = FALSE,
                  options= list(scrollX = TRUE, pageLength = nrow(Fmetals_oneStation()), scrollY = "250px", dom='Bti', buttons=list('copy')),
                  selection = 'none') #%>%
    #formatStyle(names(z), backgroundColor = styleEqual(c('OE'), c('red'))) # highlight cells red if not supporting
  })
  
  
}




