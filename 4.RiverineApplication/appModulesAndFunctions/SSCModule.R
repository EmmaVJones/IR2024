
SSCPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(3,uiOutput(ns('oneStationSelectionUI'))),
               column(1),
               column(3,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText('All data presented in the interactive plot is raw data. Rounding rules are appropriately applied to the 
               assessment functions utilized by the application.'),
      plotlyOutput(ns('plotly')) )
  )
}


SSCPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  output$oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='200px', selected = stationSelectedAbove())})
  
  oneStation <- reactive({
    req(ns(input$oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$oneStationSelection) %>%
      filter(!is.na(SSC_mg_L))})
  
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
                                     SSC_mg_L, RMK_SSC, LEVEL_SSC, `7Q10 Flag Gage`, `7Q10 Flag`)
    
    DT::datatable(parameterFilter, rownames = FALSE,  extensions = c('Buttons',  'FixedColumns'),
                  options= list(dom= 'Bt', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px",
                                fixedColumns = list(leftColumns = 3), buttons=list('copy')),
                  selection = 'none') %>%
      formatStyle(c('SSC_mg_L','RMK_SSC', 'LEVEL_SSC'), 'LEVEL_SSC', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  output$plotly <- renderPlotly({
    req(input$oneStationSelection, oneStation())
    dat <- oneStation()
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y")
    
    
    plot_ly(data=dat)%>%
      add_markers(data=dat, x= ~SampleDate, y= ~SSC_mg_L,mode = 'scatter', name="Suspended Sediment Concentration (units) ",marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Suspended Sediment Concentration: ",SSC_mg_L,"units"),
                                               paste("Suspended Sediment Concentration Level: ",LEVEL_SSC)))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Suspended Sediment Concentration (units)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
}
