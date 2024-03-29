source('global.R')

previousIRassessmentDecisions <- pin_get('ejones/PreviousIRbioassessmentDecisions', board = 'rsconnect')



shinyServer(function(input, output, session) {
  
  # empty reactive objects list
  reactive_objects = reactiveValues() 
  
  inputFile <- reactive({inFile <- input$userData
  if(is.null(inFile))
    return(NULL)
  read_excel(inFile$datapath) }) 
  
  # Validate Input data
  userUploadCheck <- reactive({req(inputFile())
    stationValidation(inputFile()) })
  
  validInputData <- reactive({req(inputFile(), userUploadCheck())
    userUploadCheck()$validStations }) #stationValidation(inputFile()) })
  
  output$userIRwindows_ <- renderUI({ req(pinnedDecisions())
    selectInput('userIRwindows', "Choose an IR window to generate report",
                choices = c(NA, sort(unique(pinnedDecisions()$IRYear), decreasing = TRUE)))  })
  
  
  output$userStations_ <- renderUI({ req(pinnedDecisions(),  input$userIRwindows)
    stations <- filter(pinnedDecisions(), IRYear %in% input$userIRwindows) %>% 
      distinct(StationID) %>% 
      arrange(StationID) %>% 
      pull(StationID)
      
    selectInput('userStations', "Choose a station to generate report",
                choices = stations )})
  
  observe({req(validInputData(),  input$userIRwindows)
    updateSelectInput(session, "userStations", "Choose a station to generate report",
                      choices = bind_rows(filter(pinnedDecisions(), StationID %in% validInputData()$StationID),
                                          filter(pinnedDecisions(), ! StationID %in% validInputData()$StationID)) %>%
                        filter(IRYear %in% input$userIRwindows) %>% 
                        dplyr::select(StationID) %>% arrange(StationID) %>% pull() )  })
  
  # Display user input data
  output$inputTable <- DT::renderDataTable({req(inputFile())
    # Identify any stations with issues
    invalidInputData <- userUploadCheck()$invalidStations #filter(inputFile(), ! StationID %in% validInputData()$StationID)
    
    if(nrow(invalidInputData) > 0){
      DT::datatable(mutate(inputFile(), Validated = case_when(StationID %in% invalidInputData$StationID ~ FALSE,
                                                              TRUE ~ TRUE), # for pretty viz
                           ValidatedColor = case_when(StationID %in% invalidInputData$StationID ~ 0,
                                                      TRUE ~ 1)) %>% # for color coding with datatables, styleEqual needs numeric not boolean
                      dplyr::select(Validated, everything()) %>% 
                      arrange(Validated),
                    #mutate(inputFile(), invalidData = case_when(StationID %in% invalidInputData$StationID ~ 1, TRUE ~ 0)),
                    escape=F, rownames = F, options=list(scrollX = TRUE, scrollY = "800px",pageLength=nrow(inputFile()),
                                                         columnDefs = list(list(visible=FALSE, targets=14)))) %>%
        formatStyle('StationID', 'Validated', backgroundColor = styleEqual(c(0, 1), c('yellow', NA))  )
      #formatStyle('StationID', 'invalidData', backgroundColor = styleEqual(c(0, 1), c(NA, 'yellow'))  )
    } else {
      DT::datatable(inputFile(),escape=F, rownames = F,
                    options=list(scrollX = TRUE, scrollY = "800px",pageLength=nrow(inputFile())))  }  })
 
  
  pinnedDecisions <- reactive({
    if(is.null(inputFile())){
      return(bind_rows(previousIRassessmentDecisions,
                       pin_get('ejones/CurrentIRbioassessmentDecisions', board = 'rsconnect')))
    } else {
      pinCheck('ejones/CurrentIRbioassessmentDecisions', validInputData()) # can change to real deal pin in time
      # pull new pin
      pinnedDecisions <- pin_get('ejones/CurrentIRbioassessmentDecisions', board = 'rsconnect')
      return(bind_rows(previousIRassessmentDecisions, 
                       pinnedDecisions))
    } })

  assessmentDecision_UserSelection <- reactive({req(pinnedDecisions(), input$userStations,  input$userIRwindows)
    filter(pinnedDecisions(), StationID %in% input$userStations &
             IRYear %in% input$userIRwindows) })
  
  observe({req(nrow(assessmentDecision_UserSelection()) > 0, input$userStations, input$userIRwindows)
    reactive_objects$SCI_UserSelection <- filter(VSCIresultsAll, StationID %in% filter(assessmentDecision_UserSelection(), AssessmentMethod == 'VSCI')$StationID) %>%
      bind_rows(filter(VCPMI63resultsAll, StationID %in% filter(assessmentDecision_UserSelection(), AssessmentMethod == 'VCPMI63 + Chowan')$StationID)  ) %>%
      bind_rows(
        filter(VCPMI65resultsAll, StationID %in% filter(assessmentDecision_UserSelection(), AssessmentMethod == 'VCPMI65 - Chowan')$StationID)  ) %>%
      filter(between(`Collection Date`, filter(assessmentPeriodLookup, IRYear %in% input$userIRwindows)$PeriodStart,
                     filter(assessmentPeriodLookup, IRYear %in% input$userIRwindows)$PeriodEnd)) %>%# limit data to assessment window of interest
      filter(RepNum %in% c('1')) %>% #, '2')) %>% # drop QA and wonky rep numbers
      filter(`Target Count` == 110) %>% # only assess rarified data
      filter(Gradient != "Boatable") %>%  # don't assess where no SCI not validated
      # add back in description information
      left_join(filter(benSampsAll, StationID %in% input$userStations) %>%
                  dplyr::select(StationID, Sta_Desc, BenSampID,US_L3CODE, US_L3NAME, HUC_12, VAHU6, Basin, Basin_Code),
                by = c('StationID', 'BenSampID')) %>%
      dplyr::select(StationID, Sta_Desc, BenSampID, `Collection Date`, RepNum, everything())

    reactive_objects$benSampsFilter <- filter(benSampsAll, BenSampID %in% reactive_objects$SCI_UserSelection$BenSampID)

    habSampsIR <- filter(habSampsAll, between(`Collection Date`,
                                              filter(assessmentPeriodLookup, IRYear %in% input$userIRwindows)$PeriodStart,
                                              filter(assessmentPeriodLookup, IRYear %in% input$userIRwindows)$PeriodEnd))# limit data to assessment window
    habValuesIR <- filter(habValuesAll, HabSampID %in% habSampsIR$HabSampID)
    reactive_objects$habitatUserSelection <- habitatConsolidation( input$userStations, habSampsIR, habValuesIR)    })


  # have to make separate reactive object in order to send appropriate station name to the download title
  fileNameForReport <- reactive({req( input$userStations, input$userIRwindows)
    paste("IR", input$userIRwindows," ", as.character(unique(input$userStations))," Benthic Assessment Fact Sheet.html", sep = "")})


  output$downloadReport_ <- renderUI({req(reactive_objects$habitatUserSelection)
    list(downloadButton('downloadReport', 'Generate Report'),
         helpText('This button must be clicked for each station in order to generate a unique report.'))})

  output$downloadReport <- downloadHandler(
    filename = fileNameForReport,
    content= function(file){
      tempReport <- normalizePath('bioassessmentFactSheet.Rmd')
      imageToSend1 <- normalizePath('images/riskCategories.PNG') #NEW
      imageToSend2 <- normalizePath('images/HabitatColor.jpg') #NEW

      owd <- setwd(tempdir())
      on.exit(setwd(owd))

      file.copy(tempReport, 'bioassessmentFactSheet.Rmd')
      file.copy(imageToSend1, 'images/riskCategories.PNG') #NEW
      file.copy(imageToSend2, 'images/HabitatColor.jpg') #NEW

      params <- list(assessmentDecision =  assessmentDecision_UserSelection(),
                     SCI = reactive_objects$SCI_UserSelection,
                     benSampsFilter = reactive_objects$benSampsFilter,
                     habitat = reactive_objects$habitatUserSelection,
                     assessmentCycle = assessmentCycle)

      rmarkdown::render(tempReport,output_file = file,
                        params=params,envir=new.env(parent = globalenv()))})


  
  
  







  ## General Purpose Report

  ## filter benSamps by user station
  GPbenSamps <- reactive({req(input$GPuserStation)
    filter(benSampsAll, StationID %in% input$GPuserStation)})

  ## Benthics Data Date Range
  output$GPuserWindow_ <- renderUI({req(GPbenSamps())
    dateRangeInput('GPuserWindow', label = 'Filter Bethic Information By Date Range (YYYY-MM-DD)',
                   start = as.Date(min(GPbenSamps()$`Collection Date`)), end = as.Date(max(GPbenSamps()$`Collection Date`))) })

  ## Update benSamps based on date range filter
  GPbenSampsFilter <- reactive({req(input$GPuserWindow)
    filter(GPbenSamps(), between(as.Date(`Collection Date`), input$GPuserWindow[1], input$GPuserWindow[2]) ) })

  # pull SCI information based on user SCI choice
  GPSCI <- reactive({req(GPbenSampsFilter())
    if(input$GPuserSCIMethod == 'VSCI'){
      GPSCI <- filter(VSCIresultsAll, BenSampID %in% GPbenSampsFilter()$BenSampID) }
    if(input$GPuserSCIMethod == 'VCPMI63 + Chowan'){
      GPSCI <- filter(VCPMI63resultsAll, BenSampID %in% GPbenSampsFilter()$BenSampID) }
    if(input$GPuserSCIMethod == 'VCPMI65 - Chowan'){
      GPSCI <- filter(VCPMI65resultsAll, BenSampID %in% GPbenSampsFilter()$BenSampID) }
    # add back in description information
    return(bind_rows(SCItemplate,
                     filter(GPSCI, `Target Count` == 110) %>%
                       left_join(filter(benSamps, BenSampID %in% GPbenSampsFilter()$BenSampID) %>%
                                   dplyr::select(StationID, Sta_Desc, BenSampID, US_L3CODE, US_L3NAME, HUC_12, VAHU6, Basin, Basin_Code),
                                 by = c('StationID', 'BenSampID')) %>%
                       dplyr::select(StationID, Sta_Desc, BenSampID, `Collection Date`, RepNum, everything())) %>%
             drop_na(StationID) ) })

  #output$testtest <- renderPrint({glimpse(GPSCI())})

  habitatUserSelection <- reactive({req(GPbenSampsFilter())
    habitatConsolidation( input$GPuserStation, habSampsAll, habValuesAll) %>%
      filter(between(as.Date(`Collection Date`), input$GPuserWindow[1], input$GPuserWindow[2])) })

  output$GPSCImetrics <- renderDataTable({req(GPSCI())
    SCImetricstable <- SCImetricsTable(GPSCI())
    DT::datatable(SCImetricstable, rownames = F,  extensions = 'Buttons',
                  options= list(dom = 'Bit', scrollX = TRUE, pageLength = nrow(SCImetricstable), #scrollY = "150px",
                                buttons=list('copy','colvis')), selection = 'none')  })
  output$GPhabitatMetrics <- renderDataTable({req(habitatUserSelection())
    if(nrow(habitatUserSelection()) > 0){
      habitatTable <- habitatUserSelection() %>%
        mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
        dplyr::select(-HabSampID) %>%
        #clean up empty columns with a quick pivot longer (with drop na) and then back to wide
        pivot_longer(cols = `Bank Stability`:`Velocity / Depth Regime`, names_to = 'metric', values_to = 'metricVal', values_drop_na = TRUE) %>%
        pivot_wider(names_from = metric, values_from = metricVal) %>% ungroup() %>%
        arrange(`Collection Date`)

      habBreaks<-seq(0,20, 1)
      habClrs<-c('firebrick', 'firebrick','firebrick','firebrick','firebrick','firebrick', "#F0E442","#F0E442","#F0E442","#F0E442","#F0E442",
                 "#009E73","#009E73","#009E73","#009E73","#009E73", "#0072B2","#0072B2","#0072B2","#0072B2","#0072B2")

      DT::datatable(habitatTable, escape=F, rownames = F,  extensions = 'Buttons',
                    options=list(pageLength=nrow(habitatTable),dom= 'Bit', scrollX=TRUE, buttons=list('copy','colvis'))) %>%
        formatStyle('Total Habitat Score', backgroundColor = "lightgray") %>%
        formatStyle(names(habitatTable)[5:length(habitatTable)],  backgroundColor = styleEqual(habBreaks, habClrs), alpha=0.1,
                    textAlign = 'center')  }                          })


  # have to make separate reactive object in order to send appropriate station name to the download title
  GPfileNameForReport <- reactive({paste(as.character(unique(GPSCI()$StationID))," Benthic Fact Sheet.docx", sep = "")})

  output$GPdownloadReport <- downloadHandler(
    filename = GPfileNameForReport,
    content= function(file){
      tempReport <- normalizePath('GPbioassessmentFactSheet.Rmd')
      imageToSend1 <- normalizePath('images/riskCategories.PNG') #NEW
      imageToSend2 <- normalizePath('images/HabitatColor.jpg') #NEW

      owd <- setwd(tempdir())
      on.exit(setwd(owd))

      file.copy(tempReport, 'GPbioassessmentFactSheet.Rmd')
      file.copy(imageToSend1, 'images/riskCategories.PNG') #NEW
      file.copy(imageToSend2, 'images/HabitatColor.jpg') #NEW

      params <- list(SCI = GPSCI(),
                     habitat = habitatUserSelection())

      rmarkdown::render(tempReport,output_file = file,
                        params=params,envir=new.env(parent = globalenv()))})



})
