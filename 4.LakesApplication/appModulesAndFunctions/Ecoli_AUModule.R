#source('appTestingData.R')

EcoliPlotlyAUUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Assessment Unit Data Visualization')),
      fluidRow(column(6, 
                      helpText("The guidance states: 'When individual stations (whether sampled by DEQ or 
                               non-agency/citizen groups) are located in the same assessment unit and they are 
                               sampled on the same day, the median of same-day bacteria measurements should be calculated.'"),
                      helpText('Stations used for this module limited to the assessment unit in the ID305B_1 field of the 
                                  uploaded stations table.')),#uiOutput(ns('oneStationSelectionUI'))),
               column(6,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText('All data presented in the interactive plot is raw data. Rounding rules are appropriately applied to the 
               assessment functions utilized by the application. Each unique station is colored a different color on the 
               plot below. The gray box indicates the data window valid for recreational use assessment for the current 
               assessment period.'),
      #verbatimTextOutput(ns('test')),
      plotlyOutput(ns('plotly')),
      br(),hr(),br(),
      h4(strong('New Standard (STV= 410 CFU / 100 mL, geomean = 126 CFU / 100 mL with additional sampling requirements)')), 
      h5('All E. coli records that are ',span(strong('above the criteria')),' for the ',
         span(strong('selected Assessment Unit collected during the two most recent calendar years of the assessment period')),' are highlighted below.',
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
               DT::dataTableOutput(ns('exceedancesNEWStdTableAU')),
               br()),
        column(6, #br(), br(),br(), br(),br(), br(),br(), br(),
               h5('E. coli exceedance statistics for the ',span(strong('selected Assessment Unit')),' are highlighted below.'),
               h5("Note: This analysis is based only on data collected during the ",span(strong('two most recent calendar years of the six-year assessment window')),". For the ",
                  assessmentCycle, " IR cycle this would mean only bacteria data collected between January 1, ",
                  year(assessmentPeriod[1] + years(4)),
                  " and  December 31, ", year(assessmentPeriod[2]), " is assessed for recreation use support."),
               DT::dataTableOutput(ns("newStdTableAU")),
               h4(strong('See below section for detailed analysis with new recreation standard.')),
               br())),
      hr(),br(),
      h4(strong('New Recreation Standard In Depth Analysis')),
      helpText('Review the 90 day windows (identified by each sample date) for STV and geomean exceedances.
               Comments are specific to each row of data. To view the dataset within each 90 day window, use
               the drop down box to select the start of the window in question.'),
      fluidRow(
        column(4, helpText('Below is the raw daily median calculation associated with the ',span('selected AU.'),' Click on a row to reveal the
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
      DT::dataTableOutput(ns('analysisTable')))
  )
}


EcoliPlotlyAU <- function(input,output,session, AUdata, medianAUdistinct, medianAUForAnalysis, analyzedData){
  ns <- session$ns
  
  AU <- reactive({filter(AUdata(), !is.na(ECOLI))})
  
  # Bring in pre analyzed data to expedite process
  oneAUAnalysis <- reactive({analyzedData()})# bc not updating in full app unless this is reactive
  oneAUDecisionData <- reactive({ oneAUAnalysis()[['associatedDecisionData']][[1]]}) # bc not updating in full app unless this is reactive
  oneAURecreationDecisionData <- reactive({req(AU())
    bacteriaAssessmentDecision( # NEW for IR2024, bacteria only assessed in two most recent years of assessment period
      filter(medianAUForAnalysis() %>% distinct(SampleDate, .keep_all = T), 
             between(FDT_DATE_TIME, as.Date(assessmentPeriod[1] + years(4)), as.Date(assessmentPeriod[2]))),  # need to add date conversion since FDT_DATE_TIME holds data in Date format
      'ECOLI', 'LEVEL_ECOLI', 10, 410, 126)     })
  
  
  
  # Button to visualize modal table of available parameter data
  observeEvent(input$reviewData,{
    showModal(modalDialog(
      title="Review Raw Data for Selected Station and Parameter",
      helpText('This table subsets the conventionals raw data by the Assessment Unit selected in drop down above and
               parameter currently reviewing. Scroll right to see the raw parameter values and any data collection comments. Data analyzed
               by app is highlighted in gray (all DEQ data and non agency/citizen monitoring Level III), data counted by app and noted in
               comment fields is highlighed in yellow (non agency/citizen monitoring Level II), and data NOT CONSIDERED in app is noted in
               orange (non agency/citizen monitoring Level I).'),
      helpText(strong('Level I and Level II data are NOT CONSIDERED in the daily median calculation.')),
      DT::dataTableOutput(ns('parameterData')),
      size = 'l', easyClose = TRUE))  })
  
  # modal parameter data
  output$parameterData <- DT::renderDataTable({ req(medianAUdistinct())
    DT::datatable(medianAUdistinct(), rownames = FALSE, extensions = c('Buttons',  'FixedColumns'),
                  options= list(dom= 'Bt', pageLength = nrow(medianAUdistinct()), scrollX = TRUE, scrollY = "400px",
                                fixedColumns = list(leftColumns = 3), buttons=list('copy')),
                  selection = 'none') %>%
      formatStyle(c('ECOLI','RMK_ECOLI', 'LEVEL_ECOLI'), 'LEVEL_ECOLI', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))  })
  
  
  output$plotly <- renderPlotly({    req(medianAUdistinct())
    dat <- medianAUdistinct() %>%
      mutate(newSTV = 410, geomean = 126, oldSTV = 235)
    
    if(max(dat$ECOLI, na.rm = T) * 1.1 > 410){boxHeight <- max(dat$ECOLI, na.rm = T) * 1.1}else{boxHeight <- 410}
    box1 <- data.frame(x = c(assessmentPeriod[1]+ years(4), assessmentPeriod[1]+ years(4), assessmentPeriod[2],assessmentPeriod[2]),
                       y = c(0, boxHeight, boxHeight, 0))
    
    
    plot_ly(data=dat) %>%
      add_polygons(data = box1, x = ~x, y = ~y, fillcolor = "#949391",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Most Recent Two Years of Assessment Period')) %>%
      add_markers(data=dat, x= ~SampleDate, y= ~ECOLI,mode = 'scatter', name="E. coli (CFU / 100 mL)", 
                  color = ~FDT_STA_ID, #list(color= '#535559'),
                  colors = "Dark2",
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("FDT_STA_ID: ",FDT_STA_ID),
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("E. coli: ",ECOLI,"CFU / 100 mL"),
                                               paste("Level: ",LEVEL_ECOLI)))%>%
      add_lines(data=dat, x=~SampleDate,y=~newSTV, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                hoverinfo = "text", text= "New STV: 410 CFU / 100 mL", name="New STV: 410 CFU / 100 mL") %>%
      add_lines(data=dat, x=~SampleDate,y=~oldSTV, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text= "Old SSM: 235 CFU / 100 mL", name="Old SSM: 235 CFU / 100 mL") %>%
      add_lines(data=dat, x=~SampleDate,y=~geomean, mode='line', line = list(color = 'black', dash= 'dash'),
                hoverinfo = "text", text= "Geomean Criteria: 126 CFU / 100 mL", name="Geomean Criteria: 126 CFU / 100 mL") %>%
      layout(showlegend=FALSE,
             yaxis=list(title="E. coli (CFU / 100 mL)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))  })
  
  ### New standard ----------------------------------------------------------------------------------
  
  output$exceedancesNEWStdTableAU <- DT::renderDataTable({    req(AU(), !is.na(oneAURecreationDecisionData()))
    z <- oneAURecreationDecisionData()[['associatedDecisionData']][[1]] %>% 
      filter(`STV Exceedances In Window` > 0 | `Geomean In Window` > 126) %>%
      dplyr::select(-associatedData) %>% # remove embedded tibble to make table work
      mutate(`Date Window Starts` = as.Date(`Date Window Starts`),
             `Date Window Ends` = as.Date(`Date Window Ends`))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'), selection = 'none')  })
  
  
  output$newStdTableAU <- DT::renderDataTable({    req(AU(), oneAURecreationDecisionData())
    z <- oneAURecreationDecisionData() %>% 
      dplyr::select(ECOLI_EXC:ECOLI_GM_SAMP, 'Verbose Assessment Decision' = ECOLI_STATECOLI_VERBOSE) #only grab decision
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'), selection = 'none') })
  
  ### Raw Data and Individual window analysis
  
  output$rawData <- DT::renderDataTable({    req(medianAUForAnalysis())
    z <- medianAUForAnalysis() %>% 
      distinct(SampleDate, .keep_all = T) %>% 
      dplyr::select(SampleDate, ECOLI) %>% 
      rename('Daily E.coli Median' = 'ECOLI')  %>% 
      mutate(RecValid = ifelse(SampleDate >= as.Date(assessmentPeriod[1] + years(4)), 1, NA))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "400px", dom='ti',
                                                     columnDefs = list(list(targets = 2, visible = FALSE))), # hide RecValid field
                  selection = 'single')   %>% 
      formatStyle('RecValid',  target = 'row', backgroundColor = styleEqual(c(1), c('lightgray')))})
  
  windowData <- reactive({ req(AU(), input$rawData_rows_selected, !is.na(oneAUDecisionData()))
    windowDat <- filter(oneAUDecisionData(), as.character(`Date Window Starts`) %in% as.character(as.Date(medianAUForAnalysis()$SampleDate[input$rawData_rows_selected]))) %>% #input$windowChoice_) %>%
      dplyr::select( associatedData) %>%
      unnest(cols = c(associatedData)) %>%
      mutate(newSTV = 410, geomeanLimit = 126,
             `Date Time` = as.POSIXct(strptime(FDT_DATE_TIME, format="%Y-%m-%d")))
    bind_rows(windowDat,
              tibble(`Date Time` = c(min(windowDat$`Date Time`)- days(5), max(windowDat$`Date Time`) + days(5)),
                     newSTV = 410, geomeanLimit = 126))  })
  
  output$plotlyZoom <- renderPlotly({
    req(windowData(), !is.na(oneAUDecisionData()))
    
    windowData <- windowData()
    
    plot_ly(data=windowData) %>%
      add_markers(x= ~`Date Time`, y= ~Value,mode = 'scatter', name="E. coli (CFU / 100 mL)", marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",`Date Time`),
                                               paste("Daily Median E. coli: ",Value,"CFU / 100 mL"))) %>%
      add_lines(data=windowData, x=~`Date Time`, y=~geomean, mode='line', line = list(color = 'orange', dash= 'dash'),
                hoverinfo = "text", text= ~paste("Window Geomean: ", format(geomean,digits=3)," CFU / 100 mL", sep=''), 
                name="Window Geomean") %>%
      add_lines(data=windowData, x=~`Date Time`,y=~newSTV, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                hoverinfo = "text", text= "New STV: 410 CFU / 100 mL", name="New STV: 410 CFU / 100 mL") %>%
      add_lines(data=windowData, x=~`Date Time`,y=~geomeanLimit, mode='line', line = list(color = 'black', dash= 'dash'),
                hoverinfo = "text", text= "Geomean Criteria: 126 CFU / 100 mL", name="Geomean Criteria: 126 CFU / 100 mL") %>%
      layout(showlegend=FALSE,
             yaxis=list(title="E. coli (CFU / 100 mL)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))   })
  
  output$analysisTableZoom <- DT::renderDataTable({
    req(!is.na(oneAUDecisionData()), nrow(windowData()) > 0)
    z <- oneAUDecisionData()[input$rawData_rows_selected,] %>%
      dplyr::select(-associatedData) %>% # remove embedded tibble to make table work
      rename('Assessment Unit' = 'StationID')
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "140px", dom='t'), selection = 'none')  })
  
  output$analysisTable <- DT::renderDataTable({
    req(!is.na(oneAUDecisionData()))
    z <- oneAUDecisionData() %>%
      dplyr::select(-associatedData) %>% # remove embedded tibble to make table work
      rename('Assessment Unit' = 'StationID')
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "400px", dom='t'), selection = 'none')  })
  
  
  #output$test <- renderPrint({medianAUdistinct()})
  
}


