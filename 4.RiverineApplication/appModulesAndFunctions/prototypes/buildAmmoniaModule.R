# work through appTesting.R through the creation of stationData object


AmmoniaPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(2,uiOutput(ns('oneStationSelectionUI'))),
               column(1),
               column(4,
                      h4('Freshwater Criteria Default Analysis Settings'),
                      helpText("The below settings are applied to the station based on the WQS Class attributed to the station. All
                               analyses presented reflect these conditions."),
                      uiOutput(ns('optionsUI_'))),
               column(2, helpText('The default settings are specified to expedite application rendering time. If the default analysis 
                                  settings do not meet your needs, please contact Emma Jones (emma.jones@deq.virginia.gov)
                                  to add more interactive analysis on the fly.')),
               column(2,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText('The default design flow for calculating steady state wasteload allocations for the acute ammonia criterion for 
               freshwater is the 1Q10 (see 9VAC25-260-140 B footnote 6) unless statistically valid methods are employed that 
               demonstrate compliance with the duration and return frequency of the water quality criteria. The default design 
               flow for calculating steady state wasteload allocations for the chronic ammonia criterion for freshwater is the 
               30Q10 (see 9VAC25-260-140 B footnote 6) unless statistically valid methods are employed which demonstrate 
               compliance with the duration and return frequency of the water quality criteria.'),
      helpText(strong('This assessment application does not currently support the autocalculation of flow statistics. Regional
                      assessors are responsible for validating suggested assessment results against flow statistics.')),
      plotlyOutput(ns('plotly')),
      br(),hr(),br(),
      h4(strong("Combined Ammonia Criteria Analysis Results")),
      h5('All ammonia records that are above the ',span(strong('acute, chronic, or four day criteria')),' for the ',span(strong('selected site')),' are highlighted below.'),
      helpText('For chronic and four day criteria to apply, there must be > 1 sample to evaluate in each window. The `Valid Window` field identifies whether these criteria 
               results contain valid windows.'),
      dataTableOutput(ns('rangeTableSingleSite')),
      br(),
      h5('All 3 year rolled window results for the ',span(strong('acute, chronic, or four day criteria')),' for the ',span(strong('selected site')),' are highlighted below. 
         Click on a row to show all the data contained within the chosen window in the table to the right.'),
      fluidRow(column(6, h5("Three year window summaries"),
                      dataTableOutput(ns('stationRolledExceedanceRate'))),
               column(6, h5("All data within chosen three year window. Select a row to your left to reveal data analyzed within the chosen window/criteria combination."),
                      dataTableOutput(ns('detailedStationRolledExceedanceRate')))),
      br(),
      helpText("For wildlife and aquatic life designated uses, ammonia in free-flowing and tidal waters, acute criteria are a one-hour average concentration not to be 
               exceeded more than once every three years on the average, and chronic criteria are 30-day average concentrations not to be exceeded more than once every 
               three years on the average. In addition, the four-day average concentration of total ammonia nitrogen (in mg N/L) shall not exceed 2.5 times the chronic 
               criterion within a 30-day period more than once every three years on the average in free-flowing streams."),
      h5(span(strong('Combined Ammonia Criteria')), 'exceedance statistics calculated across three year windows for the ',span(strong('selected site')),' are highlighted below.
         The three year window results are presented as either not exceeding or exceeding, along with a suggested result. The suggested result identifies whether
         the number of exceeding windows are higher than the number of windows not exceeding by criteria type.'),
      dataTableOutput(ns("stationExceedanceRate")),
      
      br(),hr(),br(),
      
  
      h4(strong('Ammonia Criteria In Depth Analysis')),
      helpText('Review the data windows (identified by each sample date) for each criteria analysis.
               To view the dataset within any window, click the row in the table below to plot data within the window selected.'),
      fluidRow(
        column(8, helpText('Below is the data analyzed by each criteria and the calculated ammonia criteria.'), 
               h5(strong('Criteria Windows')),
               DT::dataTableOutput(ns('criteriaData'))),
        column(4, helpText('Click a row on the table to left to reveal a detailed interactive plot of the data
                           included in the selected data window. The orange dashed line is the ammonia 
                           averaged across the chosen window. The black dashed line is the criteria
                           calculated from the averaged temperature and pH measures in the chosen window.
                           For acute windows with only one measure, the plot draws the criteria lines +/- 1 day
                           in order to ensure they are visible on the plot; however, acute criteria are only 
                           applicable in the one hour window.'),
               plotlyOutput(ns('windowPlotlyZoom'))))
     #      verbatimTextOutput(ns('test'))
      
    )
  )
}


AmmoniaPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove, ammoniaAnalysis){
  ns <- session$ns
  
  # Select One station for individual review
  output$oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='200px', selected = stationSelectedAbove())})
  
  oneStation <- reactive({
    req(ns(input$oneStationSelection))
    filter(AUdata(), FDT_STA_ID %in% input$oneStationSelection) %>%
      filter(!is.na(AMMONIA_mg_L))})
  
  output$optionsUI_ <- renderUI({req(nrow(oneStation()) > 0)
    defaultTrout <- ifelse(unique(oneStation()$CLASS) %in% c('V','VI'), TRUE, FALSE)
    list(
      disabled(checkboxInput(ns('trout'), 'Trout Present', value = defaultTrout)),
      disabled(checkboxInput(ns('mussels'), 'Mussels Present', value = TRUE)),
      disabled(checkboxInput(ns('earlyLife'), 'Early Life Stages of Fish Present', value = TRUE)))})
  
  
  oneStationAnalysis <- reactive({req(nrow(oneStation()) > 0)
    ## extract pre run ammonia analysis
    dat <- filter(ammoniaAnalysis, StationID %in% unique(oneStation()$FDT_STA_ID)) %>%
      map_df(1) 
    dat$AmmoniaAnalysis })
  
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
    parameterFilter <- dplyr::select(oneStation(),  FDT_STA_ID, GROUP_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_COMMENT,
                                     AMMONIA_mg_L, RMK_AMMONIA, LEVEL_AMMONIA, `7Q10 Flag Gage`, `7Q10 Flag`)
    
    DT::datatable(parameterFilter, rownames = FALSE, extensions = c('Buttons',  'FixedColumns'),
                  options= list(dom= 'Bt', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", 
                                fixedColumns = list(leftColumns = 3), buttons=list('copy')),
                  selection = 'none') %>%
      formatStyle(c('AMMONIA_mg_L','RMK_AMMONIA', 'LEVEL_AMMONIA'), 'LEVEL_AMMONIA', 
                  backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  
  output$plotly <- renderPlotly({    req(oneStation())
    if(nrow(oneStation()) > 0){
      dat <- oneStation()
      dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y")
      if(nrow(dat) > 0){ 
        plot_ly(data=dat)%>%
          add_markers(data=dat, x= ~SampleDate, y= ~AMMONIA_mg_L,mode = 'scatter', name="Ammonia (mg/L as N)", marker = list(color= '#535559'),#marker = list(color= ~over),#list(color = '#D11814'),#for testing
                      hoverinfo="text",text=~paste(sep="<br>",
                                                   paste("Date: ",SampleDate),
                                                   paste("Depth: ",FDT_DEPTH, "m"),
                                                   paste("Ammonia: ", AMMONIA_mg_L,"mg/L as N"),
                                                   #paste('Acute Ammonia Limit: ',format(acuteNH3limit, digits=3), "mg/L as N"),
                                                   paste('Temperature: ', FDT_TEMP_CELCIUS, '(Celsius)'),
                                                   paste('pH: ', FDT_FIELD_PH, '(unitless)')))%>%
          layout(showlegend=FALSE,
                 yaxis=list(title="Ammonia (mg/L as N)"),
                 xaxis=list(title="Sample Date",tickfont = list(size = 10)))
      }
    } 
  })
  
  ## Combined Results
  output$rangeTableSingleSite <- renderDataTable({  req(nrow(oneStation()) > 0)
    z <- filter(oneStationAnalysis(), Exceedance == TRUE) %>%
      rename("Ammonia Average Value"  = "Value",
             'Ammonia Rounded to WQS Format' = parameterRound) %>% 
      dplyr::select(-associatedData)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'),
              selection = 'none') %>%
      formatSignif(columns=c('CriteriaValue', 'Ammonia Rounded to WQS Format'), digits=2)
      })
  
  
  # Rolled analysis by 3 year result
  rolledAnalysis <- reactive({ req(nrow(oneStation())> 0)
    annualRollingExceedanceAnalysis(oneStationAnalysis(), yearsToRoll = 3, aquaticLifeUse = FALSE)   })
  
  ## 3 year window summaries by criteria
  output$stationRolledExceedanceRate <- renderDataTable({   req(nrow(oneStation())> 0, rolledAnalysis())
    z <- rolledAnalysis() %>% 
      dplyr::select(-c(associatedData))
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'),
              selection = 'single') })
  
  ## Data from selected window for detailed table
  output$detailedStationRolledExceedanceRate <- renderDataTable({   req(nrow(oneStation())> 0, rolledAnalysis(), input$stationRolledExceedanceRate_rows_selected)
    z <- rolledAnalysis()[input$stationRolledExceedanceRate_rows_selected, ] %>%
      map_df(1)
    z <- z$associatedData %>% 
      dplyr::select(-c(associatedData))
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'),
              selection = 'none')    })
  
  
  # rolled analysis 3 year summary result
  output$stationExceedanceRate <- renderDataTable({    req(nrow(oneStation())> 0, rolledAnalysis())
    z <- annualRollingExceedanceSummary(rolledAnalysis()) %>% 
      rename("Number of Windows Not Exceeding" = "n Windows Fine",
             "Number of Windows Exceeding" ="n Windows Exceeding")
    datatable(z, rownames = FALSE, extensions = 'Buttons', 
              options= list(dom = 'Bt', pageLength = nrow(z), scrollX = TRUE, scrollY = "100px",
                            buttons=list('copy')),
              selection = 'none') })
  
  ### Individual window analysis
  output$criteriaData <- DT::renderDataTable({  req(oneStationAnalysis())
    z <- dplyr::select(oneStationAnalysis(), "Window Begin Date" = WindowDateTimeStart,
                       FDT_DEPTH, 
                       "Ammonia Average Value" = "Value", 
                       ValueType, `Criteria Type`, CriteriaValue, `Sample Count`,
                       'Ammonia Rounded to WQS Format' = parameterRound,
                       "Criteria Exceedance" = "Exceedance") 
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "300px", dom='ti'),
                  selection = 'single') %>% 
      formatSignif(columns=c('CriteriaValue', 'Ammonia Average Value'), digits=2) })

  windowData <-  reactive({req(oneStationAnalysis(), input$criteriaData_rows_selected)
    windowSelection <- oneStationAnalysis()[input$criteriaData_rows_selected, ]
    
    windowData <- dplyr::select(windowSelection, associatedData) %>%
      unnest(cols = c(associatedData)) %>%
      mutate(`Ammonia Average (Rounded)` = windowSelection$parameterRound,
             `Criteria Value` = windowSelection$CriteriaValue)
    windowData$`Date Time` <- as.Date(windowData$FDT_DATE_TIME, format="%m/%d/%y")
    
    # add some empty time data to make lines appear for single points
    if(windowSelection$`Criteria Type` == "Acute"){
      windowData <- windowData %>% 
        bind_rows(tibble(`Date Time` = windowData$`Date Time`  + days(1),
                         `Ammonia Average (Rounded)` = windowData$`Ammonia Average (Rounded)`,
                         `Criteria Value` = windowData$`Criteria Value`)) %>% 
        bind_rows(tibble(`Date Time` = windowData$`Date Time`  - days(1),
                         `Ammonia Average (Rounded)` = windowData$`Ammonia Average (Rounded)`,
                         `Criteria Value` = windowData$`Criteria Value`))    }
    return(windowData)  })
  


  output$windowPlotlyZoom <- renderPlotly({ req(windowData())

    plot_ly(data=windowData()) %>%
      add_markers(x= ~`Date Time`, y= ~AMMONIA_mg_L, mode = 'scatter', name="Ammonia (mg/L as N)", marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",`Date Time`),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Ammonia: ",AMMONIA_mg_L,"mg/L as N"))) %>%
      add_lines(data=windowData(), x=~`Date Time`, y=~`Ammonia Average (Rounded)`, mode='line', line = list(color = 'orange', dash= 'dash'),
                hoverinfo = "text", text= ~paste("Window Ammonia Average: ", `Ammonia Average (Rounded)`," mg/L as N", sep=''),
                name = "Window Ammonia Average") %>%
      add_lines(data = windowData(), x=~`Date Time`,y=~`Criteria Value`, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                hoverinfo = "text", text= ~paste("Window Ammonia Criteria: ", `Criteria Value`," mg/L as N", sep=''),
                name="Window Ammonia Criteria") %>%
      layout(showlegend=FALSE,
             yaxis=list(title="Ammonia (mg/L as N)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))  })
  
  output$test <- renderPrint({ oneStationAnalysis()[input$criteriaData_rows_selected, ]})
  
}






ui <- fluidPage(
  useShinyjs(),
  helpText('Review each site using the single site visualization section. There are no WQS for Specific Conductivity.'),
  AmmoniaPlotlySingleStationUI('Ammonia'))

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({filter_at(conventionals_HUC, vars(starts_with("ID305B")), any_vars(. %in% AUselection) ) })
  # for testing 30 day
  #filter(conventionals, FDT_STA_ID == '2-XDD000.40') %>%
  #  left_join(dplyr::select(stationTable, STATION_ID:VAHU6,
  #                          WQS_ID:EPA_ECO_US_L3NAME),
  #            #WQS_ID:`Max Temperature (C)`), 
  #            by = c('FDT_STA_ID' = 'STATION_ID')) %>%
  #  filter(!is.na(ID305B_1)) %>%
  #  pHSpecialStandardsCorrection() })
  
  # for testing 4 day
  #stationData <- filter(conventionals, FDT_STA_ID %in% '4ABSA000.62') %>% # good example with lots of data, lake station so depth is important and hourly averages
  #  left_join(dplyr::select(stationTable, STATION_ID:VAHU6,
  #                          WQS_ID:EPA_ECO_US_L3NAME),
  #            #WQS_ID:`Max Temperature (C)`), 
  #            by = c('FDT_STA_ID' = 'STATION_ID')) %>%
  #  #filter(!is.na(ID305B_1)) %>% # 4ABSA000.62 doesn't have an AU?????
  #  pHSpecialStandardsCorrection()
  #stationData <- filter(stationData, !is.na(AMMONIA))
  #stationData$FDT_DATE_TIME[c(2, 4, 6, 8)] <- as.POSIXct(c("2015-04-28 11:50:00 EDT", "2015-05-12 11:30:00 EDT", "2015-06-11 11:10:00 EDT", "2015-07-22 11:00:00 EDT"))
  #stationData <- stationData[1:9,]
  #return(stationData) })
  
  
  
  
  
  callModule(AmmoniaPlotlySingleStation,'Ammonia', AUData, stationSelected, ammoniaAnalysis)
  
}

shinyApp(ui,server)


