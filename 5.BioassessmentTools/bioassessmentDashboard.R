source('global.R')

assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp')
# Get pinned decisions, don't want this in global so bioassessment fact sheet tool doesn't accidentally source this
currentIRassessmentDecisions <- pin_get('ejones/CurrentIRbioassessmentDecisions', board = 'rsconnect')
previousIRassessmentDecisions <- pin_get('ejones/PreviousIRbioassessmentDecisions', board = 'rsconnect')

ui <- dashboardPage(
  
  dashboardHeader(title = paste0('IR ', assessmentCycle, " Bioassessment Dashboard"), titleWidth = '400px'),
  dashboardSidebar(
    sidebarMenu(
      helpText('Pro Tip: You can use each of the drop', br(),
               ' down boxes independently; however, ', br(),
               'if you start with the Basin Filter,', br(),
               ' the cross validation features narrow',br(), 
               ' down options in subsequent selection ', br(),
               ' fields.'),
      uiOutput('filters'),
      checkboxGroupInput("repFilter", "Rep Filter (if none are selected then all are included)", choices = c('1', '2'), selected = NULL) )) ,
  dashboardBody(
    tabsetPanel(
      tabPanel(title = span(tagList(icon("globe", lib = "glyphicon"), " Map")),
               leafletOutput('map'),
               h4('Selected Station Information'),
               dataTableOutput('stationInfoTable'),br(),br()),
      tabPanel(title = span(tagList(icon("stats", lib = "glyphicon"), " SCI Scores")),
               plotlyOutput('SCIplot'), br(),
               dataTableOutput('SCITable'),br(),br()),
      tabPanel(title = span(tagList(icon("stats", lib = "glyphicon"), " Habitat Scores")),
               plotlyOutput('SCIplot1'), br(),
               #br(),
               #plotly::plotlyOutput('habitatPlot', width='100%', height='100%')), 
               dataTableOutput('habitatTable'),br(),br()),
      tabPanel(title = span(tagList(icon("calculator"), " Station Summary")),
               h4('SCI Summary'), 
               dataTableOutput('SCIavgTable'),br(),
               h4('Total Habitat Summary'),
               dataTableOutput('totHabAvgTable'),
               br(), hr(),
               h4('Previous Integrated Report Cycle Bioassessment Information'),
               helpText('If you participated in previous IR automated bioassessment tools projects, you may have 
                        information available below detailing the selected station(s) assessment decisions for the last cycle.'),
               dataTableOutput('previousIRdecisionTable'),br(),br()),
      tabPanel(title = span(tagList(icon("balance-scale"), " Assessment Decision")),
               h4('Assessment Decision Summary'), 
               helpText('If you have uploaded information about the chosen station(s) to the VDEQ Benthic Assessment Fact Sheet Tool, 
                        you will see information that is saved on the server in the table below. If you upload new data about the 
                        selected station(s) to the VDEQ Benthic Assessment Fact Sheet Tool, that information will be reflect here after
                        you refresh the dashboard.'),
               dataTableOutput('pinnedDataTable'),br(),br())
      
      
    )
  )
)
#verbatimTextOutput('table1'), verbatimTextOutput('table2')


server <- function(input, output, session) {
  
  # original filters
  output$filters <- renderUI({
    list(
      selectInput("basinFilter", "Basin Filter", choices = sort(unique(benSamps$Basin_Code)), selected = NULL, multiple = TRUE),
      selectInput("collectorFilter", "Collector Filter", choices = sort(unique(benSamps$`Collected By`)), multiple = TRUE),
      selectInput("stationFilter", "StationID Filter", choices = sort(unique(benSamps$StationID)), selected = NULL, multiple = TRUE)#,
      #selectInput("repFilter", "Rep Filter", choices = sort(unique(benSampsFilter$RepNum)), selected = NULL, multiple = TRUE)    )
      ) })
  
  # update filters if user uses collector first
  observe({ updateSelectInput(session, "collectorFilter", "Collector Filter",
                              choices = if(!is.null(input$basinFilter)){
                                filter(benSamps,  Basin_Code %in% input$basinFilter) %>%
                                  distinct(`Collected By`) %>% arrange(`Collected By`) %>% pull()
                              } else {distinct(benSamps, `Collected By`) %>% arrange(`Collected By`) %>% pull()} 
                              ) })
  observe({ updateSelectInput(session, "stationFilter", "StationID Filter",
                              choices = if(!is.null(input$basinFilter)){
                                filter(benSamps,  Basin_Code %in% input$basinFilter) %>%
                                  {if(!is.null(input$collectorFilter))
                                    filter(., `Collected By` %in% input$collectorFilter)
                                      else . } %>%
                                  distinct(StationID) %>% arrange(StationID) %>% pull()
                              } else {
                                if(!is.null(input$collectorFilter)){
                                  filter(benSamps, `Collected By` %in% input$collectorFilter) %>%
                                    distinct(StationID) %>% arrange(StationID) %>% pull()
                                } else { distinct(benSamps, StationID) %>% arrange(StationID) %>% pull()} } ) })
  
  # Filter by user input
  benSampsFilter <- reactive({
    benSamps %>%
      {if(!is.null(input$collectorFilter))
        filter(., `Collected By` %in% input$collectorFilter)
        else . } %>%
      {if(!is.null(input$basinFilter))
        filter(., Basin_Code %in% input$basinFilter)
        else . } %>%
      {if(!is.null(input$stationFilter))
        filter(., StationID %in% input$stationFilter)
        else . } %>%
      {if(!is.null(input$repFilter))
        filter(., RepNum %in% input$repFilter)
        else .} })
  benSampsFilterStations <- reactive({req(benSampsFilter())
    filter(benSampsStations, StationID %in% benSampsFilter()$StationID) })
  
  
  ### Map Tab
  
  output$map <- renderLeaflet({req(benSampsStations)
    # color palette for assessment polygons
    pal <- colorFactor(palette = topo.colors(7), domain = assessmentRegions$ASSESS_REG)
    pal2 <- colorFactor(palette = rainbow(7), domain = ecoregion$US_L3NAME)

    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE) %>%
      setView(-78, 37.5, zoom=6) %>%
      addPolygons(data= ecoregion,  color = 'gray', weight = 1,
                  fillColor= ~pal2(ecoregion$US_L3NAME), fillOpacity = 0.5,stroke=0.1,
                  group="Level III Ecoregions",label = ~US_L3NAME) %>% hideGroup('Level III Ecoregions') %>%
      addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
                  fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
                  group="Assessment Regions", label = ~ASSESS_REG) %>% hideGroup('Assessment Regions') %>%
      inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
      # inlmisc::AddSearchButton(group = "Biomonitoring Stations", zoom = 15,propertyName = "label",
      #                          textPlaceholder = "Search stations") %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c(#"Biomonitoring Stations",
                         'Level III Ecoregions', 'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')    })

  map_proxy <- leafletProxy("map")

  observe({
    map_proxy %>%
      clearGroup('Biomonitoring Stations') %>%
      {if(nrow(benSampsFilterStations()) > 0)
        addCircleMarkers(., data=benSampsFilterStations(),color='yellow', fillColor='blue', radius = 5,
                         fillOpacity = 0.5,opacity=1,weight = 2,stroke=T,group="Biomonitoring Stations", label = ~StationID,
                         popup=~paste(sep='<br>',
                                      paste(strong('StationID : '), StationID),
                                      paste(strong('Total Station Visits (Not Sample Reps) :'), `Total Station Visits (Not Sample Reps)`),
                                      paste(strong('Ecoregion : '), US_L3CODE),
                                      paste(strong('Ecoregion Name : '), US_L3NAME),
                                      paste(strong('Basin : '), Basin_Code),
                                      paste(strong('HUC12 Name : '), HU_12_NAME),
                                      paste(strong('HUC12 : '), HUC_12),
                                      paste(strong('DEQ Region : '), ASSESS_REG),
                                      paste(strong('VAHU6 : '), VAHU6))) %>%
          addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                           overlayGroups = c('Biomonitoring Stations', #"All Biomonitoring Stations",
                                             'Level III Ecoregions', 'Assessment Regions'),
                           options=layersControlOptions(collapsed=T), position='topleft')
        else . }       })

  output$stationInfoTable <- renderDataTable({req(benSampsFilterStations())
    z <- benSampsFilterStations() %>% arrange(StationID)
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                             pageLength = nrow(z), buttons=list('copy','colvis'))) })      
  
  
  
  ## SCI Scores Tab
  
  SCI_filter <- reactive({ req(benSampsFilter())
    # choose appropriate SCI based on Ecoregion
    SCI_filter <- filter(VSCIresults, BenSampID %in% filter(benSampsFilter(), ! US_L3CODE %in% c(63,65))$BenSampID) %>%
      bind_rows(
        filter(VCPMI63results, BenSampID %in% filter(benSampsFilter(),  US_L3CODE %in% c(63) | str_detect(Basin, "Chowan"))$BenSampID)  ) %>%
      bind_rows(
        filter(VCPMI65results, BenSampID %in% filter(benSampsFilter(),  US_L3CODE %in% c(65) & !str_detect(Basin, "Chowan"))$BenSampID)  ) %>%
      mutate(SeasonGradient = as.factor(paste0(Season, " (",Gradient,")")),
             SeasonGradientColor = case_when(SeasonGradient == "Spring (Riffle)" ~  "#66C2A5",
                                             SeasonGradient == "Spring (Boatable)" ~  "#66C2A5",
                                             SeasonGradient == "Spring (MACS)" ~  "#66C2A5",
                                             SeasonGradient == "Outside Sample Window (Riffle)" ~ "#FC8D62",
                                             SeasonGradient == "Outside Sample Window (Boatable)" ~ "#FC8D62",
                                             SeasonGradient == "Outside Sample Window (MACS)" ~ "#FC8D62",
                                             SeasonGradient == "Fall (Riffle)" ~ "#8DA0CB",
                                             SeasonGradient == "Fall (Boatable)" ~ "#8DA0CB",
                                             SeasonGradient == "Fall (MACS)" ~ "#8DA0CB",
                                             TRUE ~ as.character(NA)) ) %>%
      left_join(dplyr::select(benSampsFilter(), StationID, Sta_Desc, BenSampID), by = c('StationID', 'BenSampID')) %>%
      dplyr::select(StationID, Sta_Desc, BenSampID, `Collection Date`, RepNum, SCI, `SCI Score`, `SCI Threshold`,
                    `Sample Comments`:Season, everything())
      SCI_filter$SeasonGradient <- factor(SCI_filter$SeasonGradient,levels=c("Spring (Riffle)", "Spring (Boatable)", "Spring (MACS)",
                                                                           "Outside Sample Window (Riffle)",
                                                                           "Outside Sample Window (Boatable)",
                                                                           "Outside Sample Window (MACS)",
                                                                           "Fall (Riffle)", "Fall (Boatable)", "Fall (MACS)")) %>% 
      droplevels()
  return(SCI_filter)  })
  
  output$SCIplot <- renderPlotly({ req(SCI_filter())
    # this doesnt seem to help lines show up with single measure
    # # Fix look of single measure
    # if(nrow(SCI_filter) == 1){
    #   SCI_filter <- bind_rows(SCI_filter,
    #                           tibble(`Collection Date` = c(SCI_filter$`Collection Date`- hours(1), SCI_filter$`Collection Date` + hours(1))))
    # } # fill average down so line will plot
    # 
    
    criteriaLine <- dplyr::select(SCI_filter(), `Collection Date`,SeasonGradient) %>% 
      mutate(`VSCI Criteria` = 60, 
             `VCPMI Criteria` = 40,
             SeasonGradient = NA_character_)
    
    
    
    plot_ly(data = SCI_filter()) %>% 
      add_trace(data = SCI_filter(), x = ~`Collection Date`, y = ~`SCI Score`, type = 'bar', 
                color = ~SeasonGradient, #marker = list(color = ~SeasonGradientColor,width = 0.5), # throws off color for some reason
                stroke = list(color = 'rgb(0, 0, 0)', width = 3),
                hoverinfo="text", text=~paste(sep="<br>",
                                              paste("StationID: ", StationID),
                                              paste("Collection Date: ", as.Date(`Collection Date`)),
                                              paste('Replicate: ', RepNum),
                                              paste("Collector ID: ",`Collected By`),
                                              paste("BenSampID: ", BenSampID),
                                              paste("SCI Score: ", format(`SCI Score`, digits=2)),
                                              paste("Gradient: ", Gradient)),
                name = ~paste('Rep ',RepNum, SeasonGradient)) %>%
      add_lines(data=criteriaLine, x= ~`Collection Date`, y=~`VSCI Criteria`, mode='line', 
                line = list(color = 'red', dash= 'dash'), name= 'VSCI Criteria = 60',
                hoverinfo = "text", text= 'VSCI Criteria = 60') %>%
      add_lines(data=criteriaLine, x= ~`Collection Date`, y=~`VCPMI Criteria`, mode='line', 
                line = list(color = 'red', dash= 'dash'), name= 'VCPMI Criteria = 40',
                hoverinfo = "text", text= 'VCPMI Criteria = 40') %>%
      layout(showlegend=TRUE,
             yaxis=list(title="SCI"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10),
                        type = 'date',
                        tickformat = #"%B %Y"))   })
                          "%Y"))   })
  
  output$SCITable <- renderDataTable({req(SCI_filter())
    z <- mutate(SCI_filter(), `Collection Date` = as.Date(`Collection Date`)) %>%
      arrange(StationID, `Collection Date`, RepNum)
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                             pageLength = nrow(z), buttons=list('copy','colvis'))) })
  
  
  ## Habitat Scores Tab
  
  habSamps_Filter <- reactive({req(benSampsFilterStations())
    filter(habSamps, StationID %in% benSampsFilterStations()$StationID) })
  habValues_Filter <- reactive({req(habSamps_Filter())
    filter(habValues, HabSampID %in% habSamps_Filter()$HabSampID) })
  totalHab <- reactive({req(habSamps_Filter(), habValues_Filter())
    habSamps_Filter() %>%
    group_by(HabSampID) %>%
    # get total habitat values
    left_join(totalHabScore(habValues_Filter()), by = 'HabSampID') %>%
    mutate(Season = factor(Season,levels=c("Spring","Outside Sample Window","Fall"))) %>%
    dplyr::select(StationID, HabSampID, everything()) %>%
    arrange(`Collection Date`) %>% ungroup() })
  
  habitatCrosstab <- reactive({req(totalHab())
    bind_rows(habitatTemplate,
              left_join(habValues_Filter(), 
                        dplyr::select(habSamps_Filter(), HabSampID, StationID, `Collection Date`),
                        by = 'HabSampID') %>%
                group_by(StationID, HabSampID, `Collection Date`) %>%
                arrange(HabParameterDescription) %>% ungroup() %>%
                pivot_wider(id_cols = c('StationID','HabSampID','Collection Date'), names_from = HabParameterDescription, values_from = HabValue) %>%
                left_join(dplyr::select(totalHab(), HabSampID, `HabSample Comment`, `Total Habitat Score`), by = 'HabSampID') %>%
                dplyr::select(StationID, HabSampID, `Collection Date`, `HabSample Comment`, `Total Habitat Score`, everything()) ) %>%
      drop_na(StationID) %>%
      arrange(StationID, `Collection Date`)   })
  
  output$SCIplot1 <- renderPlotly({ req(SCI_filter())
    # this doesnt seem to help lines show up with single measure
    # #totalHab <- totalHab[1,]
    # # Fix look of single measure
    # if(nrow(totalHab) == 1){
    #   totalHab <- bind_rows(totalHab,
    #                    tibble(`Collection Date` = c(totalHab$`Collection Date`- hours(1), totalHab$`Collection Date` + hours(1))))
    #                           } # fill average down so line will plot
    
    
    stressLine <- dplyr::select(totalHab(), `Collection Date`, Season) %>% 
      mutate(`No Stress` = 200,
             `Low Stress` = 150,
             `Medium Stress` = 130,
             `High Stress` = 100,
             Season = NA_character_)
    
    plot_ly(data = totalHab()) %>% 
      add_trace(data = totalHab(), x = ~`Collection Date`, y = ~`Total Habitat Score`, type = 'bar', 
                color = ~Season,  
                stroke = list(color = 'rgb(0, 0, 0)', width = 3),
                hoverinfo="text", text=~paste(sep="<br>",
                                              paste("StationID: ", StationID),
                                              paste("Collection Date: ", as.Date(`Collection Date`)),
                                              paste("Gradient: ", Gradient)),
                name = ~paste0(Season, " (", Gradient, " Gradient Method)")) %>%
      add_lines(data=stressLine, x= ~`Collection Date`, y=~`No Stress`, mode='line', 
                line = list(color = '#0072B2', dash= 'dash'), 
                text = 'No Probability of Stress to Aquatic Life', 
                name = 'No Probability of Stress to Aquatic Life') %>%
      add_lines(data=stressLine, x= ~`Collection Date`, y=~`Low Stress`, mode='line', 
                line = list(color = '#009E73', dash= 'dash'), 
                text = 'Low Probability of Stress to Aquatic Life', 
                name = 'Low Probability of Stress to Aquatic Life') %>%
      add_lines(data=stressLine, x= ~`Collection Date`, y=~`Medium Stress`, mode='line', 
                line = list(color = '#F0E442', dash= 'dash'), 
                text = 'Medium Probability of Stress to Aquatic Life', 
                name = 'Medium Probability of Stress to Aquatic Life') %>%
      add_lines(data=stressLine, x= ~`Collection Date`, y=~`High Stress`, mode='line', 
                line = list(color = 'red', dash= 'dash'), 
                text = 'High Probability of Stress to Aquatic Life', 
                name = 'High Probability of Stress to Aquatic Life') %>%
      layout(showlegend=TRUE,
             yaxis=list(title="Total Habitat Score"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10),
                        type = 'date',tickformat = #"%B %Y"))   })
                          "%Y"))  })
  
  
  
  output$habitatTable <- renderDataTable({req(habitatCrosstab())
    # Set how many colors you will use and call them out by hex name
    brks <- 1:19
    clrs <- c("#8B0000", "#9D0000", "#AF0000", "#C10000", "#D40000", "#E60000", "#F80000", "#FF1415", "#FF3235", "#FF5055", "#FF6F75",
              "#FF8D95", "#FFABB5", "#FFC3CD", "#FFCDD5", "#FFD7DE", "#FFE1E6", "#FFEBEE", "#FFF5F6", "#FFFFFF")
    datatable(habitatCrosstab(), escape = F, rownames = F, extensions = 'Buttons',
              options = list(dom='Bift', scrollX= TRUE, scrollY = '300px',
                             pageLength = nrow(habitatCrosstab()), buttons=list('copy','colvis'))) %>%
      formatStyle(c("Bank Stability", "Channel Alteration", "Channel Flow Status", "Channel Sinuosity", "Embeddedness",
                    "Epifaunal Substrate / Available Cover", "Frequency of riffles (or bends)", "Pool Substrate Characterization",
                    "Pool Variability", "Riparian Vegetative Zone Width", "Sediment Deposition", "Vegetative Protection",
                    "Velocity / Depth Regime"),
                  backgroundColor = styleInterval(brks, clrs),
                  textAlign = 'center', `font-family` = 'Arial') %>%
      formatStyle(c("Bank Stability", "Channel Alteration", "Channel Flow Status", "Channel Sinuosity", "Embeddedness",
                    "Epifaunal Substrate / Available Cover", "Frequency of riffles (or bends)", "Pool Substrate Characterization",
                    "Pool Variability", "Riparian Vegetative Zone Width", "Sediment Deposition", "Vegetative Protection",
                    "Velocity / Depth Regime"),
                  fontWeight = styleInterval(10, c('bold','normal')),
                  textAlign = 'center', `font-family` = 'Arial') %>%
      formatStyle('Total Habitat Score', backgroundColor = "lightgray")  })



  
  ## Station Summary Tab
  output$SCIavgTable <- renderDataTable({req(SCI_filter())
    z <- averageSCI_windows(benSampsFilter(), SCI_filter(), assessmentCycle)
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                             pageLength = nrow(z), buttons=list('copy','colvis'))) })
  
  output$totHabAvgTable <- renderDataTable({req(totalHab())
    avgTotalHab <- averageTotHab_windows(totalHab())
    
    datatable(avgTotalHab, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                             pageLength = nrow(avgTotalHab), buttons=list('copy','colvis'))) })
  
  output$previousIRdecisionTable <- renderDataTable({req(benSampsFilter(), previousIRassessmentDecisions)
    z <- filter(previousIRassessmentDecisions, StationID %in% benSampsFilter()$StationID) %>% 
      arrange(desc(IRYear), StationID)
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(z), buttons=list('copy','colvis')))})
  
  output$pinnedDataTable <- renderDataTable({req(benSampsFilter(), currentIRassessmentDecisions)
    z <- filter(currentIRassessmentDecisions, StationID %in% benSampsFilter()$StationID) 
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                             pageLength = nrow(z), buttons=list('copy','colvis')))})
  
  #output$table1 <- renderPrint({ sort(unique(benSampsFilter()$RepNum)) })
  #output$table2 <- renderPrint({ benSampsFilter() })
}

shinyApp(ui, server)

