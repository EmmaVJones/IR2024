
NitratePlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(2,uiOutput(ns('oneStationSelectionUI'))),
               column(1),
               column(2),
               column(1),
               column(2,br(), uiOutput(ns('changeWQSUI'))),
               column(1),
               column(2,br(),actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText('All data presented in the interactive plot is raw data. Rounding rules are appropriately applied to the 
               assessment functions utilized by the application. The orange dashed line is the NITRATE_mg_L averaged across the assessment window 
               (visible if the `Apply Public Water Supply Water Quality Standards` checkbox is selected).
               Please investigate the raw parameter data to understand whether NITRATE_mg_L describes total or dissolved nitrate. Where total and dissolved nitrate
               data are available, NITRATE_mg_L will prioritize total nitrate.'),
      plotlyOutput(ns('plotly')),
      fluidRow(
        column(8, h5('All nitrate records that are above the PWS criteria (where applicable) for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:150px;overflow-y: scroll', dataTableOutput(ns('rangeTableSingleSite')))),
        column(4, h5('Individual nitrate exceedance statistics for the ',span(strong('selected site')),' are highlighted below.
                     If no data is presented, then the PWS criteria is not applicable to the station.'),
               dataTableOutput(ns("stationExceedanceRate"))))
    )
  )
}


NitratePlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  output$oneStationSelectionUI <- renderUI({    req(AUdata)
    selectInput(ns('oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='200px', selected = stationSelectedAbove())})
  
  oneStation <- reactive({    req(ns(input$oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$oneStationSelection) %>%
      filter(!is.na(NITRATE_mg_L)) })
  
  oneStationAssessment <- reactive({req(oneStation())
    if(input$changeWQS == TRUE){
      return(assessPWS(oneStation() %>% mutate(PWS = "Yes"), # must override function default behavior 
                       NITRATE_mg_L, LEVEL_NITRATE, 10))
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
                                     NITRATE_mg_L, RMK_NITRATE, LEVEL_NITRATE, 
                                     NITROGEN_NITRATE_DISSOLVED_00618_mg_L, RMK_00618, LEVEL_00618,
                                     NITROGEN_NITRATE_TOTAL_00620_mg_L, RMK_00620, LEVEL_00620,
                                     `7Q10 Flag Gage`, `7Q10 Flag`)
    
    DT::datatable(parameterFilter, rownames = FALSE, extensions = c('Buttons',  'FixedColumns'),
                  options= list(dom= 'Bt', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", 
                                fixedColumns = list(leftColumns = 3), buttons=list('copy')),
                  selection = 'none') %>%
      formatStyle(c('NITRATE_mg_L','RMK_NITRATE', 'LEVEL_NITRATE'), 'LEVEL_NITRATE', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray')) %>% 
      formatStyle(c('NITROGEN_NITRATE_DISSOLVED_00618_mg_L', 'RMK_00618', 'LEVEL_00618'), 'LEVEL_00618', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray')) %>% 
      formatStyle(c('NITROGEN_NITRATE_TOTAL_00620_mg_L', 'RMK_00620', 'LEVEL_00620'), 'LEVEL_00620', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  output$plotly <- renderPlotly({  req(input$oneStationSelection, oneStation())
    if(is.null(oneStationAssessment())){
      dat <- oneStation() %>% 
        mutate(`Parameter Median` = NA, 
               PWSlimit = NA)
    }else{
      dat <- oneStation() %>% 
        left_join(dplyr::select(oneStationAssessment(), FDT_DATE_TIME, `Parameter Median`),
                  by = 'FDT_DATE_TIME') %>% 
        mutate(PWSlimit = 10)
    }
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y")
    
    
    # Fix look of single measure
    if(nrow(dat) == 1){
      print('yes')
      dat <- bind_rows(dat,
                       tibble(SampleDate = c(dat$SampleDate- days(5), dat$SampleDate + days(5)),
                              PWSlimit = c(10, 10)))%>%
        fill(`Parameter Median`)  } # fill average down so line will plot
    
    
    maxheight <- ifelse(max(dat$NITRATE_mg_L, na.rm=T) < 50, 55, max(dat$NITRATE_mg_L, na.rm=T)* 1.2)
    
      if(input$changeWQS == TRUE){
        plot_ly(data=dat)%>%
          add_lines(data=dat, x=~SampleDate,y=~PWSlimit, mode='line', line = list(color = 'black'),
                    hoverinfo = "text", text= "PWS Criteria (10 mg/L)", name="PWS Criteria (10 mg/L)") %>%
          add_lines(data=dat, x=~SampleDate,y=~`Parameter Median`, mode='line', line = list(color = 'orange', dash= 'dash'), name="NITRATE_mg_L six year average",
                    hoverinfo = "text", text= ~paste(sep="<br>",
                                                     paste("NITRATE_mg_L six year average: ", `Parameter Median`, "mg/L"))) %>%
          # add_markers(data=dat, x= ~SampleDate, y= ~NITRATE_mg_L,mode = 'scatter', name="Dissolved Nitrate (mg/L)",marker = list(color= '#535559'),
          #             hoverinfo="text",text=~paste(sep="<br>",
          #                                          paste("Date: ",SampleDate),
          #                                          paste("Depth: ",FDT_DEPTH, "m"),
          #                                          paste("Dissolved Nitrate: ",NITRATE_mg_L,"mg/L"),
          #                                          paste("Dissolved Nitrate Level: ",LEVEL_NITRATE)))%>%
          add_markers(data=dat, x= ~SampleDate, y= ~NITROGEN_NITRATE_DISSOLVED_00618_mg_L,mode = 'scatter', name="Dissolved Nitrate (mg/L)",marker = list(color= '#535559'),
                      hoverinfo="text",text=~paste(sep="<br>",
                                                   paste("Date: ",SampleDate),
                                                   paste("Depth: ",FDT_DEPTH, "m"),
                                                   paste("Dissolved Nitrate: ",NITROGEN_NITRATE_DISSOLVED_00618_mg_L,"mg/L"),
                                                   paste("Dissolved Nitrate Level: ",LEVEL_00618)))%>%
          add_markers(data=dat, x= ~SampleDate, y= ~NITROGEN_NITRATE_TOTAL_00620_mg_L,mode = 'scatter', name="Total Nitrate (mg/L)",marker = list(color= '#D3D3D3'),
                      hoverinfo="text",text=~paste(sep="<br>",
                                                   paste("Date: ",SampleDate),
                                                   paste("Depth: ",FDT_DEPTH, "m"),
                                                   paste("Total Nitrate: ",NITROGEN_NITRATE_TOTAL_00620_mg_L,"mg/L"),
                                                   paste("Total Nitrate Level: ",LEVEL_00620)))%>%
          layout(showlegend=TRUE,
                 yaxis=list(title="Nitrate (mg/L)"),
                 xaxis=list(title="Sample Date",tickfont = list(size = 10)))
      } else {
        plot_ly(data=dat)%>%
          add_lines(data=dat, x=~SampleDate,y=~`Parameter Median`, mode='line', line = list(color = 'orange', dash= 'dash'), name="NITRATE_mg_L six year average",
                    hoverinfo = "text", text= ~paste(sep="<br>",
                                                     paste("NITRATE_mg_L six year average: ", `Parameter Median`, "mg/L"))) %>%
          # add_markers(data=dat, x= ~SampleDate, y= ~NITRATE_mg_L,mode = 'scatter', name="Dissolved Nitrate (mg/L)",marker = list(color= '#535559'),
          #             hoverinfo="text",text=~paste(sep="<br>",
          #                                          paste("Date: ",SampleDate),
          #                                          paste("Depth: ",FDT_DEPTH, "m"),
          #                                          paste("Dissolved Nitrate: ",NITRATE_mg_L,"mg/L"),
          #                                          paste("Dissolved Nitrate Level: ",LEVEL_NITRATE)))%>%
          add_markers(data=dat, x= ~SampleDate, y= ~NITROGEN_NITRATE_DISSOLVED_00618_mg_L,mode = 'scatter', name="Dissolved Nitrate (mg/L)",marker = list(color= '#535559'),
                      hoverinfo="text",text=~paste(sep="<br>",
                                                   paste("Date: ",SampleDate),
                                                   paste("Depth: ",FDT_DEPTH, "m"),
                                                   paste("Dissolved Nitrate: ",NITROGEN_NITRATE_DISSOLVED_00618_mg_L,"mg/L"),
                                                   paste("Dissolved Nitrate Level: ",LEVEL_00618)))%>%
          add_markers(data=dat, x= ~SampleDate, y= ~NITROGEN_NITRATE_TOTAL_00620_mg_L,mode = 'scatter', name="Total Nitrate (mg/L)",marker = list(color= '#D3D3D3'),
                      hoverinfo="text",text=~paste(sep="<br>",
                                                   paste("Date: ",SampleDate),
                                                   paste("Depth: ",FDT_DEPTH, "m"),
                                                   paste("Total Nitrate: ",NITROGEN_NITRATE_TOTAL_00620_mg_L,"mg/L"),
                                                   paste("Total Nitrate Level: ",LEVEL_00620)))%>%
          layout(showlegend=TRUE,
                 yaxis=list(title="Nitrate (mg/L)"),
                 xaxis=list(title="Sample Date",tickfont = list(size = 10)))
        
      } 
  })
  
  
  output$rangeTableSingleSite <- renderDataTable({   req(oneStation(), oneStationAssessment())
    if(input$changeWQS == TRUE){
      z <-  oneStationAssessment() %>%
        filter(exceeds == TRUE)
    } else { z <- NULL}
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t'),
              selection = 'none') })
  
  output$stationExceedanceRate <- renderDataTable({ req(oneStationAssessment())
    if(input$changeWQS == TRUE){
      z <- assessPWSsummary(oneStationAssessment(), 'PWS_Nitrate') %>% dplyr::select(-PWS_Nitrate_STAT) 
      datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t'),
                selection = 'none') }}) 
}