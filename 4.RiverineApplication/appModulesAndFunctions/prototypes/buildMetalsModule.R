
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
                                     h5(strong("These analyses are still undergoing rigorous review. Please verify all assessment decisions 
                                            based off these results until further notice.")) ) ),
                            DT::dataTableOutput(ns('WCmetalsSingleSiteSummary')),
                            verbatimTextOutput(ns('testtest')),
                            hr(), 
                            fluidRow(
                              column(7, helpText('Below are the calculated results associated with the ',span('selected site'),". You can view all the
                                                 analyzed data associated with the site by using the 'All' selection, or you may choose a particular 
                                                 criteria to investigate further by choosing that in the drop down. The criteria chosen in the drop down
                                                 will filter the table to just that selected criteria."),# Any selection beside 'All' will
                                     #generate a plot to the right with the associated data plotted."),#  Click on a row to reveal the data included in the selected criteria window in the plot to the right.'), 
                                     h5(strong('Analyzed Data')),
                                     uiOutput(ns('criteriaChoice_')),
                                     DT::dataTableOutput(ns('analyzedData'))),
                              column(5, helpText('Choose a specific criteria using the drop down to left to reveal a detailed interactive plot of the data
                                                 included in the selected criteria window. When criteria with static limits are selected, a 
                                                 black dashed line appears corresponding to the appropriate criteria. When criteria with
                                                 hardness based limits are selected, measured values appear as gray if they fall below the 
                                                 calculated criteria and red if they exceed the calculated criteria.'),
                                     # helpText('Click a row on the table to left to reveal a detailed interactive plot of the data
                                     # included in the selected criteria window. When criteria with static limits are selected, a 
                                     # black dashed line appears corresponding to the appropriate criteria. When criteria with
                                     # hardness based limits are selected, measured values appear as gray if they fall below the 
                                     # calculated criteria and red if they exceed the calculated criteria.'),
                                     #verbatimTextOutput(ns('test')),
                                     plotlyOutput(ns('plotlyZoom'))))),
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


metalsTableSingleStation <- function(input,output,session, AUdata, stationData, WCmetals , WCmetalsForAnalysis, Smetals,  Fmetals, 
                                     metalsSV, stationSelectedAbove, staticLimit){
  ns <- session$ns
  
  ## Water Column Metals
  
  # Select One station for individual review
  output$WCmetals_oneStationSelectionUI <- renderUI({    req(stationSelectedAbove)
    selectInput(ns('WCmetals_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  WCmetals_oneStation <- reactive({req(ns(input$WCmetals_oneStationSelection))
    filter(WCmetals, Station_Id %in% input$WCmetals_oneStationSelection)})
  
  WCmetals_oneStationForAnalysis <- reactive({req(ns(input$WCmetals_oneStationSelection), nrow(WCmetals_oneStation()) > 0)
    filter(WCmetalsForAnalysis, StationID %in% input$WCmetals_oneStationSelection) %>%
      map(1)  })
  
  # Extract metals analysis or calculate based on user input
  WCmetals_oneStationAnalysis <- reactive({req(WCmetals_oneStationForAnalysis(), input$WER, nrow(WCmetals_oneStation()) > 0)
    if(input$WER == 1){
      WCmetals_oneStationForAnalysis()$WCmetalsExceedanceSummary
    } else {
      WCmetals_oneStation() %>% 
        metalsAnalysis( stationData(), WER = input$WER) #%>% 
        #rename(FDT_STA_ID = Station_Id) %>% 
        #mutate(`Criteria Type` = Criteria)    
      } })
   # metalsAnalysis(WCmetals_oneStationForAnalysis(), AUdata(), WER= input$WER)    })
  
  output$testtest <- renderPrint({nrow(stationData())}) #WCmetals_oneStationAnalysis()})
  
  WCmetals_oneStationAssessment <- reactive({req(WCmetals_oneStationForAnalysis(), input$WER)
    if(input$WER == 1){
      WCmetals_oneStationForAnalysis()$WCmetalsExceedanceSummary
    } else {
      annualRollingExceedanceAnalysis(WCmetals_oneStationAnalysis(), yearsToRoll = 3, aquaticLifeUse = TRUE) %>% 
        annualRollingExceedanceSummary()     } })
    #metalsAssessmentFunction(WCmetals_oneStationAnalysis())})
  
  output$WCmetalsSingleSiteSummary <- DT::renderDataTable({req(WCmetals_oneStationAssessment())
    DT::datatable(WCmetals_oneStationAssessment(), rownames = FALSE,extensions = 'Buttons',
                  options= list(scrollX = TRUE, pageLength = nrow(WCmetals_oneStationAssessment()), scrollY = "400px", dom='Bt',
                                buttons=list('copy',
                                             list(extend='csv',filename=paste('WCmetalsSummary_',paste(assessmentCycle,input$WCmetals_oneStationSelection, collapse = "_"),Sys.Date(),sep='')),
                                             list(extend='excel',filename=paste('WCmetalsSummary_',paste(assessmentCycle,input$WCmetals_oneStationSelection, collapse = "_"),Sys.Date(),sep='')))),
                  selection = 'none')     })
  
  # # Zoomed plot section
  # output$criteriaChoice_ <- renderUI({req(WCmetals_oneStationAnalysis())
  #   selectizeInput(ns("criteriaChoice"), "Choose Criteria to visualize", choices = c('All', unique(WCmetals_oneStationAnalysis()$Criteria)), width = '40%')})
  # 
  # output$analyzedData <- DT::renderDataTable({req(WCmetals_oneStationAnalysis(), input$criteriaChoice)
  #   z <- WCmetals_oneStationAnalysis() %>% 
  #     {if(input$criteriaChoice != 'All')
  #       filter(., Criteria %in% input$criteriaChoice)
  #       else . }
  #   DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "300px", dom='Bti', buttons=list('copy')),
  #                 selection = 'none') })
  # #output$test <- renderPrint({input$criteriaChoice    })
  # 
  # output$plotlyZoom <- renderPlotly({ req(WCmetals_oneStationAnalysis(), input$criteriaChoice !='All' )#input$analyzedData_rows_selected, 
  #   criteriaSelection <- input$criteriaChoice #WCmetals_oneStationAnalysis()[input$analyzedData_rows_selected, ]$Criteria
  #   dat <- filter(WCmetals_oneStationAnalysis(), Criteria %in%  criteriaSelection) %>%
  #     filter(Value != 'NaN') # drop any unmeasured values
  #   print(dat)
  #   dat$SampleDate <- as.POSIXct(dat$WindowDateTimeStart, format="%m/%d/%y")
  #   
  #   plot_ly(data=dat) %>%
  #     {if(criteriaSelection %in% staticLimit)
  #       add_markers(., x= ~SampleDate, y= ~Value,mode = 'scatter', name=~Metal, marker = list(color= '#535559'),
  #                   hoverinfo="text",text=~paste(sep="<br>",
  #                                                paste("StationID: ",Station_Id),
  #                                                paste("Date: ",SampleDate),
  #                                                paste("Depth: ",FDT_DEPTH, "m"),
  #                                                paste(Metal,":",Value, "ug/L"),
  #                                                paste('Static Criteria:', CriteriaValue, "ug/L"))) %>%
  #         add_lines(data=dat, x=~SampleDate,y=~CriteriaValue, mode='line', line = list(color = '#484a4c',dash = 'dot'),
  #                   hoverinfo = "text", text= ~paste(criteriaSelection, "Criteria:",  CriteriaValue, "ug/L"), name="Static Criteria")
  #       else add_markers(., data=dat, x= ~SampleDate, y= ~Value, mode = 'scatter', name=~Metal, marker = list(color= ~Exceedance), colors = c('#535559', 'red'), #color= ~Exceedance, #colors = c('#535559', 'red'),#marker = list(color= '#535559'),
  #                        symbol =  ~Exceedance, symbols = c(16,15),
  #                        hoverinfo="text",text=~paste(sep="<br>",
  #                                                     paste("StationID: ",Station_Id),
  #                                                     paste("Date: ",SampleDate),
  #                                                     paste("Depth: ",FDT_DEPTH, "m"),
  #                                                     paste(Metal,":",Value, "ug/L"),
  #                                                     paste('Hardness Based Criteria:', CriteriaValue, "ug/L")))       } %>%
  #     layout(showlegend=FALSE,
  #            yaxis=list(title=paste(stringr::word(criteriaSelection, 1), "ug/L")),
  #            xaxis=list(title="Sample Date",tickfont = list(size = 10))) })
  # 
  # 
  # 
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


  # 
  # 
 
  # 
  # ## Sediment Metals
  # 
  # # Select One station for individual review
  # output$Smetals_oneStationSelectionUI <- renderUI({
  #   req(stationSelectedAbove)
  #   selectInput(ns('Smetals_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
  #               width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  # 
  # Smetals_oneStation <- reactive({
  #   req(ns(input$Smetals_oneStationSelection))
  #   filter(Smetals, Station_Id %in% input$Smetals_oneStationSelection)})
  # 
  # output$SmetalsRangeTableSingleSite <- DT::renderDataTable({req(Smetals_oneStation())
  #   z <- Smetals_oneStation()
  #   z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
  #   DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'),
  #                 selection = 'none')     })
  # 
  #  
  # ## Fish Tissue Metals
  # 
  # output$Fmetals_oneStationSelectionUI <- renderUI({
  #   req(stationSelectedAbove)
  #   selectInput(ns('Fmetals_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
  #               width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  # 
  # 
  # Fmetals_oneStation <- reactive({req(ns(input$Fmetals_oneStationSelection))
  #   filter(Fmetals, Station_ID %in% input$Fmetals_oneStationSelection)})
  # 
  # output$Fmetals_exceedance <- DT::renderDataTable({req(Fmetals_oneStation())
  #   FmetalsSV <- dplyr::select(Fmetals_oneStation(), Station_ID, Collection_Date_Time, Sample_ID,  `# of Fish`, Species_Name, length, weight, Beryllium:Lead) %>%
  #     dplyr::select(-contains('RMK_')) %>%
  #     group_by( Station_ID, Collection_Date_Time, Sample_ID, `# of Fish`, Species_Name, length, weight) %>%
  #     pivot_longer(cols= Beryllium:Lead, names_to = "Metal", values_to = 'Measure') %>%
  #     left_join(metalsSV, by = 'Metal') %>%
  #     filter(Measure > `Screening Value`) %>%
  #     arrange(Metal)
  #   DT::datatable(FmetalsSV, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(FmetalsSV),
  #                                                            scrollY = "250px", dom='Bti', buttons=list('copy')), selection = 'none') })
  # 
  # 
  # output$FmetalsRangeTableSingleSite <- DT::renderDataTable({ req(input$Fmetals_oneStationSelection, Fmetals_oneStation())
  #   # z <- dplyr::select(Smetals_oneStation(), FDT_STA_ID, `FDT_DATE_TIME`,ARSENIC:COMMENT)
  #   # z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
  #   DT::datatable(Fmetals_oneStation(), rownames = FALSE,
  #                 options= list(scrollX = TRUE, pageLength = nrow(Fmetals_oneStation()), scrollY = "250px", dom='Bti', buttons=list('copy')),
  #                 selection = 'none') #%>%
  #   #formatStyle(names(z), backgroundColor = styleEqual(c('OE'), c('red'))) # highlight cells red if not supporting
  # })
  # 
  
}





ui <- fluidPage(
  helpText('Review each site using the single site visualization section. There are no WQS for Specific Conductivity.'),
  metalsTableSingleStationUI('metals'),
  verbatimTextOutput('test')
)

server <- function(input,output,session){
  stationData1 <- eventReactive( input$stationSelection, {
    filter(AUData(), FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  #AUData <- reactive({filter_at(conventionals_HUC, vars(starts_with("ID305B")), any_vars(. %in% AUselection) ) })
  AUData <- reactive({filter(conventionals, Huc6_Vahu6 %in% c("PL30", 'JM01','JM02', 'JM03', 'JM04', 'JM05', 'JM06', "JU11")) %>%
      left_join(dplyr::select(stationTable, STATION_ID:VAHU6,
                              WQS_ID:CLASS_DESCRIPTION),
                #WQS_ID:`Max Temperature (C)`), 
                by = c('FDT_STA_ID' = 'STATION_ID')) %>%
      filter(!is.na(ID305B_1)) %>%
      pHSpecialStandardsCorrection() %>%
      filter(!is.na(CHLORIDE_mg_L))})
  
  output$test <-renderPrint({stationData1()})   # this is the problem
  
  #### Metals Sub Tab ####---------------------------------------------------------------------------------------------------
  callModule(metalsTableSingleStation,'metals', AUData, reactive(stationData1), WCmetals, WCmetalsForAnalysis, Smetals, 
             fishMetals, fishMetalsScreeningValues, stationSelected, staticLimit)
  
  
}

shinyApp(ui,server)



