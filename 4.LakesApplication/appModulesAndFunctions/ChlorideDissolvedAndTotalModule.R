
ClPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(3,uiOutput(ns('oneStationSelectionUI'))),
               column(1),
               column(3,br(), uiOutput(ns('changeWQSUI'))),
               column(1),
               column(3,br(),actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText('All data presented in the interactive plot is raw data. Rounding rules are appropriately applied to the
               assessment functions utilized by the application.The orange dashed line is CHLORIDE_mg_L averaged across the assessment window 
               (visible if the `Apply Public Water Supply Water Quality Standards` checkbox is selected).
               Please investigate the raw parameter data to understand whether CHLORIDE_mg_L describes total or dissolved chloride. Where total and dissolved chloride
               data are available, CHLORIDE_mg_L will prioritize whichever chloride result is greater.'),
      plotlyOutput(ns('plotly')),
      fluidRow(
        column(8, h5('All chloride records that are above the PWS criteria (where applicable) for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:150px;overflow-y: scroll', dataTableOutput(ns('PWSrangeTableSingleSite')))),
        column(4, h5('Six year window average chloride exceedance statistics for the ',span(strong('selected site')),' are highlighted below.
                     If no data is presented, then the PWS criteria is not applicable to the station.'),
               dataTableOutput(ns("PWSstationExceedanceRate")))),
      br(),hr(),br(),
      h4('Freshwater Chloride Criteria'),
      helpText('Below are the results of the chloride freshwater acute and chronic criteria analysis. These results apply to all stations with
               CLASS II (Tidal Fresh Zone only) and III - VII. The acute and chronic criteria for each data window are presented on the plot below. 
               Turn the layers on/off to visualize the raw data averaged across each window.'),
      p(strong('The Freshwater Chloride analysis was analyzed using the CHLORIDE_mg_L field.')),
      h4(strong("Combined Chloride Criteria Analysis Results")),
      h5('All chloride records that are ',span(strong('above the acute or chronic criteria')),' for the ',span(strong('selected site')),' are highlighted below.'),
      # helpText('For chronic criteria to apply, there must be > 1 sample to evaluate in each window. The `Valid Window` field identifies whether these criteria 
      #          results contain valid windows.'),
      dataTableOutput(ns('rangeTableSingleSite')),
      br(),
      h5('All 3 year rolled window results for the ',span(strong('acute or chronic criteria')),' for the ',span(strong('selected site')),' are highlighted below. 
         Click on a row to show all the data contained within the chosen window in the table to the right.'),
      fluidRow(column(6, h5("Three year window summaries"),
                      dataTableOutput(ns('stationRolledExceedanceRate'))),
               column(6, h5("All data within chosen three year window. Select a row to your left to reveal data analyzed within the chosen window/criteria combination."),
                      dataTableOutput(ns('detailedStationRolledExceedanceRate')))),
      br(),
      helpText("For toxic pollutant assessment of Aquatic Life Designated Use in free-flowing streams, both chronic and acute criteria can be assessed whenever 
               sufficient data are available as applicable.  Chronic criteria are to be assessed when multiple grab samples are collected within two separate 
               four-day periods within a three-year period, or when there are two or more separate 30-day SPMD deployments within a three-year period.  
               Two samples (either grab or SPMD) taken within three consecutive years are sufficient to assess acute criteria."),
      h5(span(strong('Combined Chloride Criteria')), 'exceedance statistics calculated across three year windows for the ',span(strong('selected site')),' are highlighted below.
         The three year window results are presented as either not exceeding or exceeding, along with a suggested result. The suggested result identifies whether
         the number of exceeding windows are higher than the number of windows not exceeding by criteria type.'),
      dataTableOutput(ns("stationExceedanceRate")),
      
      br(),hr(),br()
    )
  )
}


ClPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  output$oneStationSelectionUI <- renderUI({    req(AUdata)
    selectInput(ns('oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='200px', selected = stationSelectedAbove())})
  
  oneStation <- reactive({    req(ns(input$oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$oneStationSelection) %>%
      filter(!is.na(CHLORIDE_mg_L))})
  
  oneStationAssessment <- reactive({req(oneStation())
    if(input$changeWQS == TRUE){
      return(assessPWS(oneStation() %>% mutate(PWS = "Yes"), # must override function default behavior 
                       CHLORIDE_mg_L, LEVEL_CHLORIDE, 250))
    } else {return( NULL )}    })
  
  # Option to change WQS used for modal
  output$changeWQSUI <- renderUI({    req(oneStation())
    if(nrow(oneStation()) > 0){
      defaultPWS <- unique(oneStation()$PWS) %in% c("Yes")
    } else { defaultPWS <- FALSE}
    checkboxInput(ns('changeWQS'),'Apply Public Water Supply Water Quality Standards (Automatically selected if PWS standards apply to the selected station)', value = defaultPWS) })
  
  
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
  output$parameterData <- DT::renderDataTable({    req(oneStation())
    parameterFilter <- dplyr::select(oneStation(),FDT_STA_ID, GROUP_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_COMMENT,
                                     CHLORIDE_mg_L, RMK_CHLORIDE, LEVEL_CHLORIDE, 
                                     CHLORIDE_TOTAL_00940_mg_L, RMK_00940, LEVEL_00940,
                                     CHLORIDE_DISSOLVED_00941_mg_L, RMK_00941, LEVEL_00941,
                                     ThermoclineDepth, LakeStratification)
    
    DT::datatable(parameterFilter, rownames = FALSE,  extensions = c('Buttons',  'FixedColumns'),
                  options= list(dom= 'Bt', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", 
                                fixedColumns = list(leftColumns = 3), buttons=list('copy')),
                  selection = 'none') %>%
      formatStyle(c('CHLORIDE_mg_L','RMK_CHLORIDE', 'LEVEL_CHLORIDE'), 'LEVEL_CHLORIDE', 
                  backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray')) %>% 
      formatStyle(c('CHLORIDE_TOTAL_00940_mg_L', 'RMK_00940', 'LEVEL_00940'), 'LEVEL_00940', 
                  backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray')) %>% 
      formatStyle(c('CHLORIDE_DISSOLVED_00941_mg_L', 'RMK_00941', 'LEVEL_00941'), 'LEVEL_00941', 
                  backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))   })
  
  output$plotly <- renderPlotly({    req(input$oneStationSelection, oneStation())
    if(is.null(oneStationAssessment())){
      dat <- oneStation() %>% 
        mutate(`Parameter Median` = NA, 
               PWSlimit = NA)
    }else{
      dat <- oneStation() %>% 
        left_join(dplyr::select(oneStationAssessment(), FDT_DATE_TIME, `Parameter Median`),
                  by = 'FDT_DATE_TIME') %>% 
        mutate(PWSlimit = 250)
    }
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y")
    
    # Fix look of single measure
    if(nrow(dat) == 1){
      dat <- bind_rows(dat,
                       tibble(SampleDate = c(dat$SampleDate- days(5), dat$SampleDate + days(5)),
                              PWSlimit = c(250, 250))) %>%
        fill(`Parameter Median`)  } # fill average down so line will plot
    
    maxheight <- ifelse(max(dat$CHLORIDE_mg_L, na.rm=T) < 50, 55, max(dat$CHLORIDE_mg_L, na.rm=T)* 1.2)
    
    if(input$changeWQS == TRUE){
      plot_ly(data=dat)%>%
        add_lines(data=dat, x=~SampleDate,y=~PWSlimit, mode='line', line = list(color = 'black'),
                  hoverinfo = "text", text= "PWS Criteria (250 mg/L)", name="PWS Criteria (250 mg/L)") %>%
        add_lines(data=dat, x=~SampleDate,y=~`Parameter Median`, mode='line', line = list(color = 'orange', dash= 'dash'), name="CHLORIDE_mg_L six year average",
                  hoverinfo = "text", text= ~paste(sep="<br>",
                                                   paste("CHLORIDE_mg_L six year average: ", `Parameter Median`, "mg/L"))) %>%
        # old method  
        # add_markers(data=dat, x= ~SampleDate, y= ~CHLORIDE_mg_L,mode = 'scatter', name="Dissolved Chloride (mg/L)",marker = list(color= '#535559'),
        #             hoverinfo="text",text=~paste(sep="<br>",
        #                                          paste("Date: ",SampleDate),
        #                                          paste("Depth: ",FDT_DEPTH, "m"),
        #                                          paste("Dissolved Chloride: ",CHLORIDE_mg_L,"mg/L"),
        #                                          paste("Dissolved Chloride Level: ",LEVEL_CHLORIDE)))%>%
        add_markers(data=dat, x= ~SampleDate, y= ~CHLORIDE_DISSOLVED_00941_mg_L, mode = 'scatter', name="Dissolved Chloride (mg/L)",marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Dissolved Chloride: ", CHLORIDE_DISSOLVED_00941_mg_L,"mg/L"),
                                                 paste("Dissolved Chloride Level: ", LEVEL_00941)))%>%
        add_markers(data=dat, x= ~SampleDate, y= ~CHLORIDE_TOTAL_00940_mg_L, mode = 'scatter', name="Total Chloride (mg/L)",marker = list(color= '#D3D3D3'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Total Chloride: ",CHLORIDE_TOTAL_00940_mg_L,"mg/L"),
                                                 paste("Total Chloride Level: ",LEVEL_00940)))%>%
        layout(showlegend=TRUE,
               yaxis=list(title="Chloride (mg/L)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))
    } else {
      plot_ly(data=dat)%>%
        add_lines(data=dat, x=~SampleDate,y=~`Parameter Median`, mode='line', line = list(color = 'orange', dash= 'dash'), name="CHLORIDE_mg_L six year average",
                  hoverinfo = "text", text= ~paste(sep="<br>",
                                                   paste("CHLORIDE_mg_L six year average: ", `Parameter Median`, "mg/L"))) %>%
        # old method
        # add_markers(data=dat, x= ~SampleDate, y= ~CHLORIDE_mg_L,mode = 'scatter', name="Dissolved Chloride (mg/L)",marker = list(color= '#535559'),
        #             hoverinfo="text",text=~paste(sep="<br>",
        #                                          paste("Date: ",SampleDate),
        #                                          paste("Depth: ",FDT_DEPTH, "m"),
        #                                          paste("Dissolved Chloride: ",CHLORIDE_mg_L,"mg/L")))%>%
        add_markers(data=dat, x= ~SampleDate, y= ~CHLORIDE_DISSOLVED_00941_mg_L, mode = 'scatter', name="Dissolved Chloride (mg/L)",marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Dissolved Chloride: ", CHLORIDE_DISSOLVED_00941_mg_L,"mg/L"),
                                                 paste("Dissolved Chloride Level: ", LEVEL_00941)))%>%
        add_markers(data=dat, x= ~SampleDate, y= ~CHLORIDE_TOTAL_00940_mg_L, mode = 'scatter', name="Total Chloride (mg/L)",marker = list(color= '#D3D3D3'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Total Chloride: ",CHLORIDE_TOTAL_00940_mg_L,"mg/L"),
                                                 paste("Total Chloride Level: ",LEVEL_00940)))%>%
        layout(showlegend=TRUE,
               yaxis=list(title="Chloride (mg/L)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))
      
    } 
  })
  
  
  output$PWSrangeTableSingleSite <- renderDataTable({    req(oneStation(), oneStationAssessment())
    if(input$changeWQS == TRUE){
      z <-  oneStationAssessment() %>%
        filter(exceeds == TRUE)
    } else { z <- NULL}
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t'),
              selection = 'none') })
  
  
  output$PWSstationExceedanceRate <- renderDataTable({    req(oneStationAssessment())
    if(input$changeWQS == TRUE){
      z <- assessPWSsummary(oneStationAssessment(), 'PWS_Chloride') %>% dplyr::select(-PWS_Chloride_STAT) 
      datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t'),
                selection = 'none') }}) 
  
  
  
  
  # Freshwater Chloride Analysis
  chlorideFreshwaterAnalysisResults <- reactive({ req(nrow(oneStation())> 0 )
    chlorideFreshwaterAnalysis(oneStation())    })
  
  ## Combined Results
  output$rangeTableSingleSite <- renderDataTable({  req(!is.null(chlorideFreshwaterAnalysisResults()))
    z <- chlorideFreshwaterAnalysisResults()  %>% 
      filter(Exceedance == TRUE) %>%
      rename("Chloride Average Value"  = "Value",
             'Chloride Rounded to WQS Format' = parameterRound) %>% 
      dplyr::select(-associatedData)
    
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'),
              selection = 'none') %>%
      formatSignif(columns=c('CriteriaValue', 'Chloride Rounded to WQS Format'), digits=2)
  })
  
  
  # Rolled analysis by 3 year result
  rolledAnalysis <- reactive({ req(!is.null(chlorideFreshwaterAnalysisResults()))
    annualRollingExceedanceAnalysis(chlorideFreshwaterAnalysisResults(), yearsToRoll = 3, aquaticLifeUse = TRUE)   })
  
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
      dplyr::select(-c(associatedData)) %>% 
      rename("Chloride Average Value"  = "Value",
             'Chloride Rounded to WQS Format' = parameterRound) %>% 
      dplyr::select(-`Valid Window`)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'),
              selection = 'none') %>% 
      formatSignif(columns=c('CriteriaValue', 'Chloride Rounded to WQS Format'), digits=2)})
  
  
  # rolled analysis 3 year summary result
  output$stationExceedanceRate <- renderDataTable({    req(nrow(oneStation())> 0, rolledAnalysis())
    z <- annualRollingExceedanceSummary(rolledAnalysis()) %>% 
      rename("Number of Windows Not Exceeding" = "n Windows Fine",
             "Number of Windows Exceeding" ="n Windows Exceeding")
    datatable(z, rownames = FALSE, extensions = 'Buttons', 
              options= list(dom = 'Bt', pageLength = nrow(z), scrollX = TRUE, scrollY = "100px",
                            buttons=list('copy')),
              selection = 'none') })
}




