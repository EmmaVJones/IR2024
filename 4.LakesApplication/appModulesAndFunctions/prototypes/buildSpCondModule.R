# work through appTesting.R through the creation of stationData object


SpCondPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(3,uiOutput(ns('oneStationSelectionUI'))),
               column(1),
               #column(3,br(),checkboxInput(ns('displayBSAcolors'), 'Display Benthic Stressor Analysis Colors on Plot', value = FALSE)),
               column(1),
               column(3,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText('All data presented in the interactive plot is raw data. Rounding rules are appropriately applied to the 
               assessment functions utilized by the application.'),
      plotlyOutput(ns('plotly'))  )
  )
}


SpCondPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='200px', selected = stationSelectedAbove())})
  
  oneStation <- reactive({
    req(ns(input$oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$oneStationSelection) %>%
      filter(!is.na(FDT_SPECIFIC_CONDUCTANCE))})
  
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
                                     FDT_SPECIFIC_CONDUCTANCE, RMK_FDT_SPECIFIC_CONDUCTANCE,
                                     LEVEL_FDT_SPECIFIC_CONDUCTANCE)#, `7Q10 Flag Gage`, `7Q10 Flag`)
    
    DT::datatable(parameterFilter, rownames = FALSE, extensions = c('Buttons',  'FixedColumns'),
                  options= list(dom='Bt',  pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", 
                                fixedColumns = list(leftColumns = 3), buttons=list('copy')),
                  selection = 'none') %>%
      formatStyle(c('FDT_SPECIFIC_CONDUCTANCE','RMK_FDT_SPECIFIC_CONDUCTANCE','LEVEL_FDT_SPECIFIC_CONDUCTANCE'), 'LEVEL_FDT_SPECIFIC_CONDUCTANCE', 
                  backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  
  output$plotly <- renderPlotly({
    req(input$oneStationSelection, oneStation())
    dat <- mutate(oneStation(),top = `pH Max`, bottom = `pH Min`,
                  LakeStratification = replace_na(LakeStratification,"Unstratified")) %>%
      mutate(LakeStratification = factor(LakeStratification,levels=c("Epilimnion",'Unstratified',"Hypolimnion")))#,ordered=T)
    
    # Fix look of single measure
    if(nrow(dat) == 1){
      print('yes')
      dat <- bind_rows(dat,
                       tibble(SampleDate = c(dat$SampleDate- days(5), dat$SampleDate + days(5))))    }
    
    
    # Fix look of single measure
    if(nrow(dat) == 1){
      print('yes')
      dat <- bind_rows(dat,
                       tibble(SampleDate = c(dat$SampleDate- days(5), dat$SampleDate + days(5))))
    }
    
    maxheight <- ifelse(max(dat$FDT_SPECIFIC_CONDUCTANCE, na.rm=T) < 500, 600, max(dat$FDT_SPECIFIC_CONDUCTANCE, na.rm=T)* 1.2)
    
    
      plot_ly(data=dat)%>%
        add_markers(data=dat, x= ~SampleDate, y= ~FDT_SPECIFIC_CONDUCTANCE,mode = 'scatter', #name="Specific Conductivity (uS/cm)",marker = list(color= '#535559'),
                    color=~LakeStratification, #marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Specific Conductivity: ",FDT_SPECIFIC_CONDUCTANCE,"uS/cm"),
                                                 paste("Specific Conductivity Level: ",LEVEL_FDT_SPECIFIC_CONDUCTANCE)))%>%
        layout(showlegend=TRUE,
               yaxis=list(title="Specific Conductivity (uS/cm)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))
    #}
    
  })
  
}









ui <- fluidPage(
  SpCondPlotlySingleStationUI('SpCond'))

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({filter_at(conventionalsLake, vars(starts_with("ID305B")), any_vars(. %in% inputAUselection) ) })
  
  callModule(SpCondPlotlySingleStation,'SpCond', AUData, stationSelected)
}

shinyApp(ui,server)


