toxicsSingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel('Public Water Supply Water Column Metals',
               wellPanel(
                 h4(strong('Single Station Data Visualization')),
                 verbatimTextOutput(ns('testtesttest')),
                 
                 uiOutput(ns('WCPWS_oneStationSelectionUI')),
                 uiOutput(ns('intakeProximityFlag')),
                 h5('All water column metals data',span(strong('with Public Water Supply (PWS) criteria')), 'that are available for the ',
                 span(strong('selected site')),' are available below. If no data is presented, then the station does not have any water 
                 column metals data available.'),
                 DT::dataTableOutput(ns('WCPWSRangeTableSingleSite')),br(), br(), br())),
      
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

toxicsSingleStation <- function(input,output,session, AUdata, stationData, waterToxics, markPCB, fishPCB, stationSelectedAbove){
  ns <- session$ns
  
  ## Water Column Metals PWS
  
  # Select One station for individual review
  output$WCPWS_oneStationSelectionUI <- renderUI({req(stationSelectedAbove)
    selectInput(ns('WCPWS_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  output$testtesttest <- renderPrint({WCPWS_oneStation()})
  
  ## Water Intake proximity flag for station
  output$intakeProximityFlag <- renderUI({req(stationData())
    if(unique(stationData()$FDT_STA_ID) %in% intakeSites$FDT_STA_ID){
      wellPanel(h5(strong('This station is within 100 meters of a drinking water intake. Please review whether the station
                should be assessed for secondary human health criteria.', style = "color:red")) ) }    })
  
  
  WCPWS_oneStation <- reactive({req(ns(input$WCPWS_oneStationSelection))
    filter(AUdata(), FDT_STA_ID %in% input$WCPWS_oneStationSelection) })

  ## PWS table output marked up
  output$WCPWSRangeTableSingleSite <- DT::renderDataTable({req(input$WCPWS_oneStationSelection)
    if(is.na(unique(stationData()$PWS))){
      PWSconcat <- tibble(STATION_ID = unique(stationData()$FDT_STA_ID),
                          PWS= 'PWS Standards Do Not Apply To Station')
      DT::datatable(PWSconcat, escape=F, rownames = F, options= list(scrollX = TRUE, pageLength = nrow(PWSconcat), dom='t'),
                    selection = 'none')
      
    } else {
      PWSconcat <- waterToxics() %>%
        dplyr::select(PWSinfo) %>%
        unnest(cols = c(PWSinfo)) %>%
        dplyr::select(-ends_with('exceedanceRate'))
      
      DT::datatable(PWSconcat, escape=F, rownames = F, options= list(scrollX = TRUE, pageLength = nrow(PWSconcat), dom='t'),
                    selection = 'none') %>%
        formatStyle(c("PWS_Nitrate_EXC","PWS_Nitrate_SAMP","PWS_Nitrate_STAT"), "PWS_Nitrate_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>%
        formatStyle(c("PWS_Chloride_EXC","PWS_Chloride_SAMP","PWS_Chloride_STAT"), "PWS_Chloride_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>%
        formatStyle(c("PWS_Total_Sulfate_EXC","PWS_Total_Sulfate_SAMP","PWS_Total_Sulfate_STAT"), "PWS_Total_Sulfate_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) } })
  
  
  

  
  
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




ui <- fluidPage(
  helpText('Review each site using the single site visualization section. There are no WQS for Specific Conductivity.'),
  toxicsSingleStationUI('PBC')
)

server <- function(input,output,session){
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  AUData <- reactive({filter(conventionals_HUC, FDT_STA_ID %in% filter(stationTable, VAHU6 %in% c('JU11','JU21'))$STATION_ID) %>% 
      left_join(WQSvalues, by = 'CLASS')})
  
  
  # for PWS WCmetals
  WCmetalsStationPWS <- reactive({req(stationData())
    left_join(dplyr::select(stationData(), FDT_STA_ID, PWS) %>% distinct(FDT_STA_ID, .keep_all = T),
              filter(WCmetalsForAnalysis, Station_Id %in%  stationData()$FDT_STA_ID),
              by = c('FDT_STA_ID' = 'Station_Id')) })
  
  
  
  waterToxics <- reactive({ req(stationData())
    # PWS stuff
    if(nrow(stationData()) > 0){
      if(is.na(unique(stationData()$PWS))  ){
        PWSconcat <- tibble(#STATION_ID = unique(stationData()$FDT_STA_ID),
          PWS= NA)
      } else {
        PWSconcat <- cbind(#tibble(STATION_ID = unique(stationData()$FDT_STA_ID)),
          assessPWSsummary(assessPWS(stationData(), NITRATE_mg_L, LEVEL_NITRATE, 10), 'PWS_Nitrate'),
          assessPWSsummary(assessPWS(stationData(), CHLORIDE_mg_L, LEVEL_CHLORIDE, 250), 'PWS_Chloride'),
          assessPWSsummary(assessPWS(stationData(), SULFATE_TOTAL_mg_L, LEVEL_SULFATE_TOTAL, 250), 'PWS_Total_Sulfate'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), Antimony, RMK_Antimony, 5), 'PWS_Antimony'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), Arsenic, RMK_Arsenic, 10), 'PWS_Arsenic'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), Barium, RMK_Barium, 2000), 'PWS_Barium'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), Cadmium, RMK_Cadmium, 5), 'PWS_Cadmium'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), Chromium, RMK_Chromium, 100), 'PWS_ChromiumIII'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), Copper, RMK_Copper, 1300), 'PWS_Copper'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), IronDissolved, RMK_IronDissolved, 300), 'PWS_IronDissolved'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), IronTotal, RMK_IronTotal, 300), 'PWS_IronTotal'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), Lead, RMK_Lead, 15), 'PWS_Lead'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), Nickel, RMK_Nickel, 610), 'PWS_Nickel'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), Selenium, RMK_Selenium, 170), 'PWS_Selenium'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), Thallium, RMK_Thallium, 0.24), 'PWS_Thallium'),
          assessPWSsummary(assessPWS(WCmetalsStationPWS(), Uranium, RMK_Uranium, 30), 'PWS_Uranium')) %>%
          dplyr::select(-ends_with('exceedanceRate')) }
      
      
      # chloride assessment if data exists
      if(nrow(filter(stationData(), !is.na(CHLORIDE_mg_L)))){
        chlorideFreshwater <- rollingWindowSummary(
          annualRollingExceedanceSummary(
            annualRollingExceedanceAnalysis(chlorideFreshwaterAnalysis(stationData()), yearsToRoll = 3, aquaticLifeUse = TRUE) ), "CHL")
      } else {
        chlorideFreshwater <- tibble(CHL_EXC = NA, CHL_STAT= NA)}
      
      # Water toxics combination with PWS, Chloride Freshwater, and water column PCB data
      if(nrow(bind_cols(PWSconcat,
                        chlorideFreshwater,
                        PCBmetalsDataExists(filter(markPCB, str_detect(SampleMedia, 'Water')) %>%
                                            filter(StationID %in% stationData()$FDT_STA_ID), 'WAT_TOX')) %>%
              dplyr::select(contains(c('_EXC','_STAT'))) %>%
              mutate(across( everything(),  as.character)) %>%
              pivot_longer(cols = contains(c('_EXC','_STAT')), names_to = 'parameter', values_to = 'values', values_drop_na = TRUE) ) >= 1) {
        WCtoxics <- tibble(WAT_TOX_EXC = NA, WAT_TOX_STAT = 'Review',
                           PWSinfo = list(PWSconcat))# add in PWS information so you don't need to run this analysis again
      } else { WCtoxics <- tibble(WAT_TOX_EXC = NA, WAT_TOX_STAT = NA,
                                  PWSinfo = list(PWSconcat))}# add in PWS information so you don't need to run this analysis again
    } else { WCtoxics <- tibble(WAT_TOX_EXC = NA, WAT_TOX_STAT = NA,
                                PWSinfo = list(PWSconcat))}# add in PWS information so you don't need to run this analysis again
    return(WCtoxics) })
  
  
  callModule(toxicsSingleStation,'PBC', AUData,  stationData, waterToxics, markPCB, fishPCB, stationSelected)
  
}

shinyApp(ui,server)

