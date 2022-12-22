# work through appTesting.R through the creation of stationData object


temperaturePlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(4,uiOutput(ns('oneStationSelectionUI'))),
               column(4,uiOutput(ns('changeWQSUI'))),
               column(4,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px')),
               column(4, actionButton(ns('reviewLowFlow'), 'Review Low Flow Information'))),
      uiOutput(ns('lowFlowFlagUI')),
      helpText('All data presented in the interactive plot is raw data. Rounding rules are appropriately applied to the 
               assessment functions utilized by the application.'),
      plotlyOutput(ns('plotly')),
      br(),hr(),br(),
      fluidRow(
        column(8, h5('All temperature records that are above the criteria for the ',span(strong('selected site')),' are highlighted below.'),
               dataTableOutput(ns('rangeTableSingleSite'))),
        column(4, h5('Individual temperature exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               dataTableOutput(ns("stationExceedanceRate")) ) 
      )
    )
  )
}

temperaturePlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove, assessmentWindowLowFlowsModal){
  ns <- session$ns
  
  # Select One station for individual review
  output$oneStationSelectionUI <- renderUI({req(AUdata)
    selectInput(ns('oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='200px', selected = stationSelectedAbove())})
  
  oneStation_original <- reactive({   req(ns(input$oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$oneStationSelection) %>%
      filter(!is.na(FDT_TEMP_CELCIUS))})
  
  # Option to change WQS used for modal
  output$changeWQSUI <- renderUI({  req(oneStation_original())
    selectInput(ns('changeWQS'),strong('WQS For Analysis'),
                choices= WQSvalues$CLASS_DESCRIPTION,
                width='400px', selected = unique(oneStation_original()$CLASS_DESCRIPTION)) })
  
  # change WQS for rest of module if user chooses to do so
  oneStation <- reactive({req(nrow(oneStation_original()) > 0, input$changeWQS)
    changeWQSfunction(oneStation_original(), input$changeWQS) })
  
  
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
    parameterFilter <- dplyr::select(oneStation(), FDT_STA_ID, GROUP_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_COMMENT, 
                                     FDT_TEMP_CELCIUS, RMK_FDT_TEMP_CELCIUS, LEVEL_FDT_TEMP_CELCIUS, `7Q10 Flag Gage`, `7Q10 Flag`)
    
    DT::datatable(parameterFilter, rownames = FALSE, extensions = 'FixedColumns',
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t',
                                fixedColumns = list(leftColumns = 3)),
                  selection = 'none') %>%
      formatStyle(c( 'FDT_TEMP_CELCIUS', 'RMK_FDT_TEMP_CELCIUS', 'LEVEL_FDT_TEMP_CELCIUS'), 'LEVEL_FDT_TEMP_CELCIUS', 
                  backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))  })
  
  # Low flow flag
  lowFlowData <- reactive({  req(nrow(oneStation()) > 0) 
    lowFlowData <- filter(oneStation(), !is.na(`7Q10 Flag`)) 
    lowFlowData$SampleDate <- as.POSIXct(lowFlowData$FDT_DATE_TIME, format="%m/%d/%y")
    return(lowFlowData)  })
  
  output$lowFlowFlagUI <- renderUI({  req(oneStation(), lowFlowData())
    if(nrow(lowFlowData()) > 0){ 
      tagList(
        fluidRow(
          column(8, h5(strong('This station contains data collected during a potential low flow event. The potentially affected data are plotted
                below with a red halo. Please investigate the `Review Raw Parameter Data` and `Review Low Flow Information` buttons above to better understand the data that
                were flagged.', style="color:red")) ) ) ) } })
  
  #output$test <- renderPrint({ nrow(lowFlowData())})
  
  # This did not work when threw module into app for some reason, so added hacky solution to modal dialog box below
  # observe({  toggleState("reviewLowFlow", nrow(lowFlowData()) > 0)  }) # don't show button unless there is something to look at
  
  # Button to visualize modal map of station proximity to low flow gage, table with low flow gage dates, and excceedance statistics recalculated
  observeEvent(input$reviewLowFlow,{
    if(nrow(lowFlowData()) > 0){ # hacky solution bc toggleState didnt work when put in whole shiny app
      showModal(modalDialog(
        title="Review Low Flow Information",
        helpText('This modal displays low flow information for gages that fell below 7Q10 in the same major river basin as
               the selected station. The map displays the selected station and any gages that fell below 7Q10 when
               data was collected at the station. The table below the map details all 7Q10 flags for the selected gages.
               See detailed gage information using the embedded hyperlink to the USGS website.'),
        helpText('Lastly, should you agree that the data collected during a flagged Low flow event are not suitable for
               assessment, the station exceedances are recaluated, dropping all sample events that fall within
               a low flow event.'),
        helpText(span('For more information on how low flow statistics are calculated and applied to the assessment process,
                 please see the ',
                 a('Low Flow (7Q10) Data section of the DEQ Water Quality Automated Assessment User Guide.',
                   href = "https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/low-flow-7q10-data.html#low-flow-7q10-data)",
                   target="_blank"))),
        fluidRow(column(2), column(10, leafletOutput(ns('gageMap'), height = 400, width = 600))), # center map..ish
        DT::dataTableOutput(ns('gageLowFlowData')),
        br(),
        h5('Individual temperature exceedance statistics for the ',span(strong('selected site with 7Q10 flagged data removed')),' are highlighted below.'),
        dataTableOutput(ns("stationExceedanceRate7Q10")),
        size = 'l', easyClose = TRUE) )
    } else {
      showModal(modalDialog(
        title="Review Low Flow Information",
        helpText('No low flow information to review for this site'),
        size = 'l', easyClose = TRUE) )
    }
    
  })
  
  # Modal map of station and low flow gage
  output$gageMap <- renderLeaflet({req(nrow(lowFlowData()) > 0)
    point <- dplyr::select(lowFlowData(),  FDT_STA_ID, Longitude, Latitude, SampleDate, `7Q10 Flag Gage` ) %>%
      st_as_sf(coords = c("Longitude", "Latitude"),
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326) # add projection
    uniqueGageID <- unlist(strsplit(point$`7Q10 Flag Gage`, ", "))
    gagePoint <- filter(assessmentWindowLowFlowsModal(),  `Gage ID` %in% uniqueGageID) %>% # pull all unique gages
      filter(Date %in% as.Date(point$SampleDate)) %>% #just the sample dates of interest
      distinct(`Gage ID`, .keep_all = T) %>% # dont need more than one point to plot on map
      st_as_sf(coords = c("Longitude", "Latitude"),
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326) # add projection
    
    map1 <- mapview(point, color = 'yellow', lwd = 5, label= point$FDT_STA_ID, layer.name = c('Selected Station'),
                    popup=NULL, legend= FALSE) +
      mapview(gagePoint, color = 'blue', lwd = 5, label= gagePoint$`Gage ID`, layer.name = c('Gages below 7Q10 in major river basin'),
              popup= leafpop::popupTable(gagePoint,  zcol=c("Agency", "Gage ID", "Station Name", "Site Type",
                                                            "n7Q10", "7Q10 Flag", 
                                                            "BASIN_CODE", "BASIN_NAME")), legend= FALSE) 
    map1@map  })
  
  # Low flow gage information for that water year
  output$gageLowFlowData <- renderDataTable({  req(ns(input$reviewLowFlow), oneStation(), nrow(lowFlowData()) > 0)
    uniqueGageID <- unlist(strsplit(lowFlowData()$`7Q10 Flag Gage`, ", "))
    z <- filter(assessmentWindowLowFlowsModal(),  `Gage ID` %in% uniqueGageID)  %>% # pull all unique gages
      dplyr::select(Agency:`Site Type`, Date:BASIN_NAME, Latitude, Longitude) %>%  #reformat for easier reading
      # first build the weblink
      mutate(webLink1 = paste0('https://waterdata.usgs.gov/monitoring-location/',
                               uniqueGageID,
                               "#parameterCode=00060",
                               "&startDT=", Date - 7,
                               "&endDT=", Date + 7)) %>%
      # then convert to a html format DT can handle in pretty way
      mutate(`See gage info` = paste0("<a href='",webLink1, "' target='_blank'>View Flow Data On USGS Website</a>")) %>%
      dplyr::select(-c(webLink1)) %>%
      dplyr::select(Agency:n7Q10, `See gage info`, everything() )#reformat for easier reading
    
    datatable(z, rownames = FALSE, escape=F, # escape=F important to allow html link to passthrough DT as link
              options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "300px", dom='t'),
              selection = 'none') })
  
  
  
  # Temperature Station Exceedance Rate 7Q10 flagged data removed
  output$stationExceedanceRate7Q10 <- renderDataTable({  req(ns(input$oneStationSelection), oneStation(), nrow(lowFlowData()) > 0)
    z <- tempExceedances(oneStation()) %>% quickStats('TEMP', drop7Q10 = TRUE) %>% dplyr::select(-TEMP_STAT)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "100px", dom='t'),
              selection = 'none') })
  
  
  # modal parameter data
  output$parameterData <- DT::renderDataTable({    req(oneStation())
    parameterFilter <- dplyr::select(oneStation(), FDT_STA_ID, GROUP_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_COMMENT, 
                                     FDT_TEMP_CELCIUS, RMK_FDT_TEMP_CELCIUS, LEVEL_FDT_TEMP_CELCIUS, `7Q10 Flag Gage`, `7Q10 Flag`)
    
    DT::datatable(parameterFilter, rownames = FALSE, extensions = 'FixedColumns',
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t',
                                fixedColumns = list(leftColumns = 3)),
                  selection = 'none') %>%
      formatStyle(c( 'FDT_TEMP_CELCIUS', 'RMK_FDT_TEMP_CELCIUS', 'LEVEL_FDT_TEMP_CELCIUS'), 'LEVEL_FDT_TEMP_CELCIUS', 
                  backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))  })
  
  
  
  
  
  
  
  output$plotly <- renderPlotly({   req(input$oneStationSelection, nrow(oneStation()) > 0)
    dat <- mutate(oneStation(),top = `Max Temperature (C)`)
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y")
    plot_ly(data=dat)%>%
      add_lines(x=~SampleDate,y=~top, mode='line',line = list(color = 'black'),
                hoverinfo = "text", text="Temperature Standard", name="Temperature Standard") %>%
      add_markers(., data = lowFlowData(),
                  x= ~SampleDate, y= ~FDT_TEMP_CELCIUS, mode = 'scatter', name="Temperature (Celsius)",
                  marker = list(color= 'red'), size = 250,
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Temperature: ",FDT_TEMP_CELCIUS,"C"),
                                               paste("7Q10 Flag Gage: ",`7Q10 Flag Gage`))) %>%
      
      add_markers(., data = dat, x= ~SampleDate, y= ~FDT_TEMP_CELCIUS,mode = 'scatter', name="Temperature (Celsius)",marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Temperature: ",FDT_TEMP_CELCIUS,"C"))) %>%
      layout(showlegend=FALSE,
             yaxis=list(title="Temperature (Celsius)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
  output$rangeTableSingleSite <- renderDataTable({    req(oneStation())
    z <- tempExceedances(oneStation()) %>%
      rename("FDT_TEMP" = 'parameter', 'Criteria' = 'limit', 'Parameter Rounded to WQS Format' = 'parameterRound') %>%
      filter(exceeds == TRUE) %>%
      dplyr::select(-exceeds)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "300px", dom='t'),
              selection = 'none') %>%
      formatSignif(columns=c('Criteria', 'Parameter Rounded to WQS Format'), digits=2) })
  
  # Temperature Station Exceedance Rate
  output$stationExceedanceRate <- renderDataTable({ req(ns(input$oneStationSelection), oneStation())
    z <- tempExceedances(oneStation()) %>% quickStats('TEMP') %>% dplyr::select(-TEMP_STAT)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "100px", dom='t'),
              selection = 'none') })
}




ui <- fluidPage(
  helpText('Review each site using the single site visualization section, then 
           proceed to the bottom of the page to find exceedance rate for the entire assessment unit.',br(),
           span(strong('NOTE: The temperature exceedance analysis results at the bottom of the page include data
                       from ALL stations within the assessment unit.'))),
  temperaturePlotlySingleStationUI('temperature'))

server <- function(input,output,session){
  
  assessmentWindowLowFlowsToModal <- reactive({assessmentWindowLowFlows})
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({filter_at(conventionals_HUC, vars(starts_with("ID305B")), any_vars(. %in% AUselection) ) })
  
  callModule(temperaturePlotlySingleStation,'temperature', AUData, stationSelected, assessmentWindowLowFlowsToModal)
}

shinyApp(ui,server)




