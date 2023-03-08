toxicsSingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel('Public Water Supply Water Column Metals',
               wellPanel(
                 h4(strong('Single Station Data Visualization')),
                 uiOutput(ns('WCPWS_oneStationSelectionUI')),
                 uiOutput(ns('intakeProximityFlag')),
                 h5('All water column metals data',span(strong('with Public Water Supply (PWS) criteria')), 'that are available for the ',
                    span(strong('selected site')),' are available below. If no data is presented, then the station does not have any water 
                 column metals data available.'),
                 helpText(span("PWS assessments should noted in a station's COMMENT field of the Stations Table. The table below organizes
                                                  PWS information to expedite the comment process.",
                               strong('Note: Chloride, Sulfate, Total Dissolved Solids, Iron, and Foaming Agents are secondary criteria and are
                                                         only applicable to data collected at the drinking water intake.'))),
                 helpText('See the Metals- Water Column Metals subtab to investigate dissolved metals vs PWS criteria. Total recoverable
                          metals should be compared to PWS criteria (except in the case of Iron where dissolved and total are used). In the
                          absence of total metals data, dissolved metals can indicate potential issues worth further data collection.'), 
                 DT::dataTableOutput(ns('WCPWSRangeTableSingleSite')),
                 br(),
                 h4("Individual PWS Criteria"),
                 fluidRow(column(3,
                                 selectInput(ns('parameterChoice'), 'Choose a PWS parameter to investate further.',
                                             choices = c('Total Nitrate', 'Total Chloride', 'Total Sulfate', 'Total Antimony', 'Total Arsenic', 'Total Barium',
                                                         'Total Cadmium', 'Total Chromium', 'Total Copper', 'Dissolved Iron', 'Total Iron', 'Total Lead',
                                                         'Total Nickel', 'Total Selenium', 'Total Thallium', 'Total Uranium'))),
                          column(9, DT::dataTableOutput(ns('WCPWSparameter')))),
                 br(), br(), br())),
      
      tabPanel('Water Column PCBs',
               wellPanel(
                 h4(strong('Single Station Data Visualization')),
                 uiOutput(ns('WCPBC_oneStationSelectionUI')),
                 h5('All water column PBC data available for the ',span(strong('selected site')),' are available below. 
         If no data is presented, then the station does not have any water column PBC data available.'),
         DT::dataTableOutput(ns('WCPBCRangeTableSingleSite')),br(), br(), br())),#,
      #h5('PBC assessments for the ',span(strong('selected site')),' are highlighted below.'),
      #DT::dataTableOutput(ns("WCstationPBCExceedanceRate")))),
      tabPanel('Sediment PCBs',
               wellPanel(
                 h4(strong('Single Station Data Visualization')),
                 uiOutput(ns('SPBC_oneStationSelectionUI')),
                 h5('All sediment PBC data available for the ',span(strong('selected site')),' are available below. 
                    If no data is presented, then the station does not have any sediment PBC data available.'),
                 DT::dataTableOutput(ns('SPBCRangeTableSingleSite')),br(), br(), br())),#,
      #h5('PBC assessments for the ',span(strong('selected site')),' are highlighted below.'),
      #DT::dataTableOutput(ns("SstationPBCExceedanceRate")))),
      tabPanel('Fish Tissue PCBs',
               wellPanel(
                 h4(strong('Single Station Data Visualization')),
                 fluidRow(column(6, uiOutput(ns('FPBC_oneStationSelectionUI'))),
                          column(6, DT::dataTableOutput(ns('FPBCscreeningValues')))),
                 
                 h5('All fish tissue PBC exceedances for the ',span(strong('selected site')),' are highlighted according to the 
                    screening values listed above. If no data is presented, then the station does not have any fish tissue PBC data.'),
                 DT::dataTableOutput(ns('FPBCRangeTableSingleSite')),br(), br(), br()))#,
      #h5('All fish tissue PBC data available for the ',span(strong('selected site')),' are available below. 
      #    If no data is presented, then the station does not have any fish tissue PBC data available.'),
      # helpText('All concentrations expressed as ppm (mg/kg), wet weight, in edible fish tissue fillet'),
      # DT::dataTableOutput(ns('FPBCRangeTableSingleSite'))) )
      
    ))
}

toxicsSingleStation <- function(input,output,session, AUdata, stationData, WaterToxics, WCmetalsStationPWS, IntakeSites, markPCB, fishPCB, stationSelectedAbove){
  ns <- session$ns
  
  ## Water Column Metals PWS
  
  
  ## Water Intake proximity flag for station
  output$intakeProximityFlag <- renderUI({req(stationData())
    if(unique(stationData()$FDT_STA_ID) %in% IntakeSites()$FDT_STA_ID){
      wellPanel(h5(strong('This station is within 100 meters of a drinking water intake. Please review whether the station
                should be assessed for secondary human health criteria.', style = "color:red")) ) }    })
  
  
  
  ## PWS table output marked up
  output$WCPWSRangeTableSingleSite <- DT::renderDataTable({req(stationData(), WaterToxics())
    if(is.na(unique(stationData()$PWS))){
      PWSconcat <- tibble(STATION_ID = unique(stationData()$FDT_STA_ID),
                          PWS= 'PWS Standards Do Not Apply To Station')
      DT::datatable(PWSconcat, escape=F, rownames = F, options= list(scrollX = TRUE, pageLength = nrow(PWSconcat), dom='t'),
                    selection = 'none')
      
    } else {
      PWSconcat <- WaterToxics() %>%
        dplyr::select(PWSinfo) %>%
        unnest(cols = c(PWSinfo)) %>%
        dplyr::select(-ends_with('exceedanceRate'))
      
      DT::datatable(PWSconcat, escape=F, rownames = F, options= list(scrollX = TRUE, pageLength = nrow(PWSconcat), dom='t'),
                    selection = 'none')  %>%
        formatStyle(c("PWS_NitrateTotal_EXC", "PWS_NitrateTotal_SAMP", "PWS_NitrateTotal_STAT", "PWS_NitrateTotal_MedianExceedance" ), "PWS_NitrateTotal_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>%
        formatStyle(c("PWS_ChlorideTotal_EXC", "PWS_ChlorideTotal_SAMP", "PWS_ChlorideTotal_STAT", "PWS_ChlorideTotal_MedianExceedance"),  "PWS_ChlorideTotal_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>%
        formatStyle(c("PWS_Total_Sulfate_EXC","PWS_Total_Sulfate_SAMP","PWS_Total_Sulfate_STAT", "PWS_Total_Sulfate_MedianExceedance"), "PWS_Total_Sulfate_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>% 
        formatStyle(c('PWS_AntimonyTotal_EXC', 'PWS_AntimonyTotal_SAMP', 'PWS_AntimonyTotal_STAT', 'PWS_AntimonyTotal_MedianExceedance'),  'PWS_AntimonyTotal_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>% 
        formatStyle(c("PWS_ArsenicTotal_EXC","PWS_ArsenicTotal_SAMP" , "PWS_ArsenicTotal_STAT", "PWS_ArsenicTotal_MedianExceedance"), "PWS_ArsenicTotal_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>% 
        formatStyle(c("PWS_BariumTotal_EXC", "PWS_BariumTotal_SAMP", "PWS_BariumTotal_STAT", "PWS_BariumTotal_MedianExceedance"), "PWS_BariumTotal_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>% 
        formatStyle(c("PWS_CadmiumTotal_EXC", "PWS_CadmiumTotal_SAMP", "PWS_CadmiumTotal_STAT", "PWS_CadmiumTotal_MedianExceedance"  ), "PWS_CadmiumTotal_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>% 
        formatStyle(c("PWS_ChromiumTotal_EXC",  "PWS_ChromiumTotal_SAMP", "PWS_ChromiumTotal_STAT","PWS_ChromiumTotal_MedianExceedance"),"PWS_ChromiumTotal_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>% 
        formatStyle(c("PWS_CopperTotal_EXC", "PWS_CopperTotal_SAMP", "PWS_CopperTotal_STAT", "PWS_CopperTotal_MedianExceedance"   ), "PWS_CopperTotal_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>% 
        formatStyle(c("PWS_IronDissolved_EXC",  "PWS_IronDissolved_SAMP" , "PWS_IronDissolved_STAT","PWS_IronDissolved_MedianExceedance"), "PWS_IronDissolved_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>% 
        formatStyle(c("PWS_IronTotal_EXC", "PWS_IronTotal_SAMP", "PWS_IronTotal_STAT", "PWS_IronTotal_MedianExceedance"),"PWS_IronTotal_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>% 
        formatStyle(c("PWS_LeadTotal_EXC","PWS_LeadTotal_SAMP","PWS_LeadTotal_STAT","PWS_LeadTotal_MedianExceedance" ),"PWS_LeadTotal_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>% 
        formatStyle(c("PWS_NickelTotal_EXC", "PWS_NickelTotal_SAMP", "PWS_NickelTotal_STAT", "PWS_NickelTotal_MedianExceedance" ),"PWS_NickelTotal_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>% 
        formatStyle(c("PWS_SeleniumTotal_EXC","PWS_SeleniumTotal_SAMP","PWS_SeleniumTotal_STAT","PWS_SeleniumTotal_MedianExceedance" ),"PWS_SeleniumTotal_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>% 
        formatStyle(c("PWS_ThalliumTotal_EXC", "PWS_ThalliumTotal_SAMP", "PWS_ThalliumTotal_STAT","PWS_ThalliumTotal_MedianExceedance"),"PWS_ThalliumTotal_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>% 
        formatStyle(c("PWS_UraniumTotal_EXC", "PWS_UraniumTotal_SAMP", "PWS_UraniumTotal_STAT", "PWS_UraniumTotal_MedianExceedance"),"PWS_UraniumTotal_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) } })
  
  
  parameterResults <- reactive({req(!is.na(unique(stationData()$PWS)))
    tibble(Parameter = 'Total Nitrate', Result = list(assessPWS(stationData(), NITROGEN_NITRATE_TOTAL_00620_mg_L, LEVEL_00620, 10))) %>% 
      bind_rows(tibble(Parameter = 'Total Chloride',  Result = list(assessPWS(stationData(), CHLORIDE_TOTAL_00940_mg_L, LEVEL_00940, 250)))) %>% 
      bind_rows(tibble(Parameter = 'Total Sulfate',  Result = list(assessPWS(stationData(), SULFATE_TOTAL_mg_L, LEVEL_SULFATE_TOTAL, 250)))) %>% 
      bind_rows(tibble(Parameter = 'Total Antimony',  Result = list(assessPWS(WCmetalsStationPWS(), AntimonyTotal, RMK_AntimonyTotal, 5))))  %>% 
      bind_rows(tibble(Parameter = 'Total Arsenic',  Result = list(assessPWS(WCmetalsStationPWS(), ArsenicTotal, RMK_ArsenicTotal, 10)))) %>% 
      bind_rows(tibble(Parameter = 'Total Barium',  Result = list(assessPWS(WCmetalsStationPWS(), BariumTotal, RMK_BariumTotal, 2000)))) %>% 
      bind_rows(tibble(Parameter = 'Total Cadmium',  Result = list(assessPWS(WCmetalsStationPWS(), CadmiumTotal, RMK_CadmiumTotal, 5)))) %>% 
      bind_rows(tibble(Parameter = 'Total Chromium',  Result = list(assessPWS(WCmetalsStationPWS(), ChromiumTotal, RMK_ChromiumTotal, 100)))) %>% 
      bind_rows(tibble(Parameter = 'Total Copper',  Result = list(assessPWS(WCmetalsStationPWS(), CopperTotal, RMK_CopperTotal, 1300)))) %>% 
      bind_rows(tibble(Parameter = 'Dissolved Iron',  Result = list(assessPWS(WCmetalsStationPWS(), IronDissolved, RMK_IronDissolved, 300)))) %>% 
      bind_rows(tibble(Parameter = 'Total Iron',  Result = list(assessPWS(WCmetalsStationPWS(), IronTotal, RMK_IronTotal, 300)))) %>% 
      bind_rows(tibble(Parameter = 'Total Lead',  Result = list(assessPWS(WCmetalsStationPWS(), LeadTotal, RMK_LeadTotal, 15)))) %>% 
      bind_rows(tibble(Parameter = 'Total Nickel',  Result = list(assessPWS(WCmetalsStationPWS(), NickelTotal, RMK_NickelTotal, 610)))) %>% 
      bind_rows(tibble(Parameter = 'Total Selenium',  Result = list(assessPWS(WCmetalsStationPWS(), SeleniumTotal, RMK_SeleniumTotal, 170)))) %>% 
      bind_rows(tibble(Parameter = 'Total Thallium',  Result = list(assessPWS(WCmetalsStationPWS(), ThalliumTotal, RMK_ThalliumTotal, 0.24)))) %>% 
      bind_rows(tibble(Parameter = 'Total Uranium',  Result = list(assessPWS(WCmetalsStationPWS(), UraniumTotal, RMK_UraniumTotal, 30))))  })
  
  
  
  
  
  
  output$WCPWSparameter <- DT::renderDataTable({req(input$parameterChoice)
    x <- filter(parameterResults(),  Parameter == input$parameterChoice) %>% 
      map(1)
    x <- x$Result
    DT::datatable(x, escape=F, rownames = F, options= list(scrollX = TRUE, pageLength = nrow(x), dom='t'),
                  selection = 'none') })
  
  
  
  
  
  ## Water Column PCBs
  
  # Select One station for individual review
  output$WCPBC_oneStationSelectionUI <- renderUI({req(stationSelectedAbove)
    selectInput(ns('WCPBC_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  WCPBC_oneStation <- reactive({req(ns(input$WCPBC_oneStationSelection))
    filter(markPCB, SampleMedia %in% c( "Water (whole)", "Water (dissolved)")) %>%
      filter(StationID %in% input$WCPBC_oneStationSelection) })
  
  output$WCPBCRangeTableSingleSite <- DT::renderDataTable({req(WCPBC_oneStation())
    DT::datatable(WCPBC_oneStation(), rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(WCPBC_oneStation()), scrollY = "250px", dom='t'),
                  selection = 'none')     })
  
  # output$WCstationPBCExceedanceRate <- DT::renderDataTable({
  #   req(input$WCPBC_oneStationSelection, WCPBC_oneStation())
  #   z <- dplyr::select(WCPBC_oneStation(), FDT_STA_ID, `FDT_DATE_TIME`,`ANTIMONY HUMAN HEALTH PWS`:`ZINC ALL OTHER SURFACE WATERS`)
  #   z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
  #   DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'),
  #                 selection = 'none') %>%
  #     formatStyle(names(z), backgroundColor = styleEqual(c('NSP'), c('red'))) # highlight cells red if not supporting
  # }) 
  
  
  ## Sediment PCBs
  
  # Select One station for individual review
  output$SPBC_oneStationSelectionUI <- renderUI({req(stationSelectedAbove)
    selectInput(ns('SPBC_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  SPBC_oneStation <- reactive({req(ns(input$SPBC_oneStationSelection))
    filter(markPCB, SampleMedia == 'Sediment') %>%
      filter(StationID %in% input$SPBC_oneStationSelection) })
  
  
  
  output$SPBCRangeTableSingleSite <- DT::renderDataTable({req(SPBC_oneStation())
    DT::datatable(SPBC_oneStation(), rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(SPBC_oneStation()), scrollY = "250px", dom='t'),
                  selection = 'none')     })
  
  # output$SstationPBCExceedanceRate <- DT::renderDataTable({
  #   req(input$SPBC_oneStationSelection, SPBC_oneStation())
  #   z <- dplyr::select(SPBC_oneStation(), FDT_STA_ID, `FDT_DATE_TIME`,ARSENIC:COMMENT)
  #   z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
  #   DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'),
  #                 selection = 'none') %>%
  #     formatStyle(names(z), backgroundColor = styleEqual(c('OE'), c('red'))) # highlight cells red if not supporting
  # }) 
  
  ## Fish Tissue PCBs
  
  output$FPBC_oneStationSelectionUI <- renderUI({req(stationSelectedAbove)
    selectInput(ns('FPBC_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  
  FPBC_oneStation <- reactive({req(ns(input$FPBC_oneStationSelection))
    filter(fishPCB, `DEQ rivermile` %in% input$FPBC_oneStationSelection) })
  
  output$FPBCscreeningValues <- DT::renderDataTable({
    tibble(Description = c('DEQ screening value of 18 ppb', 'DEQ screening value of 20 ppb', 'VDH lower level of concern of 100 ppb', 'VDH upper level of concern of 500 ppb'),
           `Screening Value` = c(18.001, 20.001, 100.001, 500.001)) %>% # extra digits to force colors to come in correctly
      datatable(rownames = FALSE, options= list(scrollX = TRUE, pageLength = 4, dom='t')) %>%
      formatRound('Screening Value',digits = 0) %>%
      formatStyle(c('Description', 'Screening Value'), backgroundColor = styleInterval(c( 18, 20, 100, 500), c(NA, '#f2a972', '#c34eed', '#4a57e8', '#ed4242' )))})
  
  
  output$FPBCRangeTableSingleSite <- DT::renderDataTable({ req(input$FPBC_oneStationSelection, FPBC_oneStation())
    DT::datatable(FPBC_oneStation(), rownames = FALSE, 
                  options= list(scrollX = TRUE, pageLength = nrow(FPBC_oneStation()), scrollY = "250px", dom='Bti', buttons=list('copy')),
                  selection = 'none')  %>%
      formatStyle('Parameter Rounded to WQS Format', backgroundColor = styleInterval(c( 18, 20, 100, 500), c(NA, '#f2a972', '#c34eed', '#4a57e8', '#ed4242' ))) }) 
  #formatStyle('Total PCBs', backgroundColor = styleInterval(c( 18, 20, 100, 500), c(NA, '#f2a972', '#c34eed', '#4a57e8', '#ed4242' )))   }) 
  
  
}
