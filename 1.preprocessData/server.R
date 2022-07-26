source('global.R')

### All conventionals sites
####conventionals_D <- st_read('GIS/conventionals_D.shp') %>%
conventionals_DWQS <- readRDS('data/distinctSites_sf.RDS') %>% #conventionals_D.RDS') %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) %>%
  mutate(StationID= FDT_STA_ID)

assessmentRegions <- st_read( '../GIS/AssessmentRegions_simple.shp')
assessmentLayer <- st_read('../GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) 
subbasins <- st_read('../GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
  rename('SUBBASIN' = 'SUBBASIN_1')



shinyServer(function(input, output, session) {

  # color palette for assessment polygons
  pal <- colorFactor(
    palette = topo.colors(7),
    domain = assessmentRegions$ASSESS_REG)
  
  
  
  ##################################################################################################################################################
  
  ## WQS Side of Application
  
  palBufferDistance <- colorFactor(
    palette = terrain.colors(5),#colorRamps::blue2red(5),
    levels = c("20 m", "40 m", "60 m", "80 m", "No connections within 80 m"))
  
  
  # empty reactive objects list
  WQSreactive_objects = reactiveValues() # for WQS
  
  ## Watershed Selection Tab WQS
  
  ################################# PRE-SPLIT WQS LAYER METHOD, FASTER FOR APP RENDERING ######################################
  
  # Update map Subbasin based on user selection
  output$WQSDEQregionSelection_ <- renderUI({
    req(input$WQSwaterbodyType)
    op <- filter(subbasinOptionsByWQStype, waterbodyType %in% input$WQSwaterbodyType) %>%
      distinct(AssessmentRegion) %>% 
      pull()
    selectInput("WQSDEQregionSelection", "Select DEQ Assessment Region", multiple = FALSE,
                choices= sort(op))  })
  
  output$WQSsubbasinSelection_ <- renderUI({
    req(input$WQSwaterbodyType, input$WQSDEQregionSelection)
    op <- filter(subbasinOptionsByWQStype, waterbodyType %in% input$WQSwaterbodyType) %>%
      filter(AssessmentRegion %in% input$WQSDEQregionSelection) %>%
      distinct(Basin_Code) %>% 
      pull() 
    selectInput("WQSsubbasinSelection", "Select Subbasin", multiple = FALSE,
                choices= sort(op))  })
  
  output$WQSbegin_ <- renderUI({
    req(input$WQSwaterbodyType, input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    actionButton('WQSbegin', HTML("Begin Review With Subbasin Selection <br/>(Retrieves Last Saved Result)"),
                 class='btn-block')  })
  
  basinCodes <- reactive({
    req(input$WQSwaterbodyType, input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    filter(subbasinOptionsByWQStype, waterbodyType %in% input$WQSwaterbodyType) %>%
      filter(AssessmentRegion %in% input$WQSDEQregionSelection) %>%
      filter(Basin_Code %in% input$WQSsubbasinSelection) %>%
      distinct(SubbasinOptions) %>% 
      pull() })
  
  WQSs <- eventReactive(input$WQSbegin, {
    req(input$WQSwaterbodyType, input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    
    typeName <- filter(WQSlayerConversion, waterbodyType %in% input$WQSwaterbodyType) %>%
      distinct(WQS_ID) %>% 
      pull() 
    
    if(length(basinCodes()) > 1){
      WQSs <- withProgress(message = 'Reading in Large Spatial File',
                           st_zm(st_read(paste0('../GIS/processedWQS/',typeName[1],'_', basinCodes()[1], '.shp') , 
                                         fid_column_name = "OBJECTID")) %>%
                             rbind(st_zm(st_read(paste0('../GIS/processedWQS/',typeName[1],'_', basinCodes()[2], '.shp') , 
                                                 fid_column_name = "OBJECTID"))) %>%
                             rbind(st_zm(st_read(paste0('../GIS/processedWQS/',typeName[1],'_', basinCodes()[3], '.shp') , 
                                                 fid_column_name = "OBJECTID"))) )
    } else { WQSs <- withProgress(message = 'Reading in Large Spatial File',
                                  st_zm(st_read(paste0('../GIS/processedWQS/',typeName[1],'_', basinCodes(), '.shp') ,
                                                fid_column_name = "OBJECTID")) )   }
    WQSs <- WQSs %>%
      # withProgress(message = 'Reading in Large Spatial File',
      #              st_zm(
      #                st_read(paste0('data/GIS/processedWQS/',typeName[1],'_', basinCodes(), '.shp') , fid_column_name = "OBJECTID")) ) %>%
      st_transform(4326) %>%
      rename(#"GNIS_Name" = "GNIS_Nm",
        #"WATER_NAME" = "WATER_N" ,
        "WQS_COMMENT" = "WQS_COMMEN" ,
        #"Basin_Code" = "Basn_Cd",
        #"Edit_Date"  = "Edit_Dt",
        #"Tier_III" = "Tir_III" ,
        "SECTION_DESCRIPTION" = 'SECTION_DE',
        "created_user" = "created_us",      
        "created_date" ="created_da",
        "last_edited_user" = "last_edite",
        "last_edited_date" = "last_edi_1", 
        "Shape_Length" = "Shape_Leng", 
        "BASIN_CODE" = "BASIN_CO_1") %>% 
      #"ASSESS_REG"="ASSESS_" ,
      #"Subbasin" = "Subbasn") %>%
      dplyr::select(WQS_ID, everything()) %>%
      {if(input$WQSwaterbodyType %in% c('Lacustrine', 'Estuarine'))
        rename(., "Shape_Area" = "Shape_Area")
        else .}  })
  
  WQSsEL <- reactive({
    req(WQSs(), input$WQSwaterbodyType == "Estuarine")
    typeName <- filter(WQSlayerConversion, waterbodyType %in% input$WQSwaterbodyType) %>%
      distinct(WQS_ID) %>% 
      pull() 
    
    if(length(basinCodes()) > 1){
      WQSsEL <- withProgress(message = 'Reading in Additional Estuarine Spatial File',
                             st_zm(st_read(paste0('../GIS/processedWQS/',typeName[1],'_', basinCodes()[1], '.shp') , 
                                           fid_column_name = "OBJECTID")) %>%
                               rbind(st_zm(st_read(paste0('../GIS/processedWQS/',typeName[1],'_', basinCodes()[2], '.shp') , 
                                                   fid_column_name = "OBJECTID"))) %>%
                               rbind(st_zm(st_read(paste0('../GIS/processedWQS/',typeName[1],'_', basinCodes()[3], '.shp') , 
                                                   fid_column_name = "OBJECTID"))) )
    } else { WQSsEL <- withProgress(message = 'Reading in Additional Estuarine Spatial File',
                                    st_zm(st_read(paste0('../GIS/processedWQS/',typeName[1],'_', basinCodes(), '.shp') ,
                                                  fid_column_name = "OBJECTID")) )   }
    WQSsEL <- WQSsEL %>%
      #withProgress(message = 'Reading in Additional Estuarine Spatial File',
      #             st_zm(
      #               st_read(paste0('data/GIS/processedWQS/',typeName[2],'_', basinCodes(), '.shp') , fid_column_name = "OBJECTID")) ) %>%
      st_transform(4326) %>%
      # match polygon structure
      rename(#"GNIS_Name" = "GNIS_Nm",
        #"WATER_NAME" = "WATER_N" ,
        "WQS_COMMENT" = "WQS_COMMEN" ,
        #"Basin_Code" = "Basn_Cd",
        #"Edit_Date"  = "Edit_Dt",
        #"Tier_III" = "Tir_III" ,
        "SECTION_DESCRIPTION" = 'SECTION_DE',
        "created_user" = "created_us",      
        "created_date" ="created_da",
        "last_edited_user" = "last_edite",
        "last_edited_date" = "last_edi_1", 
        "Shape_Length" = "Shape_Leng", 
        "BASIN_CODE" = "BASIN_CO_1") %>% 
      #"ASSESS_REG"="ASSESS_" ,
      #"Subbasin" = "Subbasn") %>%  # match polygon structure
      mutate(Shape_Area = NA) %>%
      dplyr::select(WQS_ID, names(WQSs()))  })
  
  ## Map output of selected subbasin
  output$WQSVAmap <- renderLeaflet({
    req(input$WQSbegin, WQSs(), input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    subbasins <- filter(subbasins, BASIN_CODE %in% as.character(basinCodes())) %>%
      filter(ASSESS_REG %in% input$WQSDEQregionSelection)
    
    m <- mapview( subbasins, label= subbasins$SUBBASIN, layer.name = 'Selected Subbasin',
                  popup= leafpop::popupTable(subbasins, zcol=c('BASIN_NAME', 'BASIN_CODE', 'SUBBASIN', 'ASSESS_REG', 'VAHU6_NOTE')))
    m@map %>% setView(sum(st_bbox(subbasins)$xmax, st_bbox(subbasins)$xmin)/2,
                      sum(st_bbox(subbasins)$ymax, st_bbox(subbasins)$ymin)/2,
                      zoom = 7)  })
  
  
  ## Make an object (once per Subbasin filter) that encompasses all WQS_ID options for said subbasin for manual WQS_ID adjustment modal, speeds rendering
  #WQS_ID_subbasinOptions <- reactive({req(WQSs())
  #  if(input$WQSwaterbodyType != "Estuarine"){
  #    as.character(WQSs()$WQS_ID)
  #  } else {  c(as.character(WQSs()$WQS_ID), as.character(WQSsEL()$WQS_ID))   }      })
  
  
  
  ### WQS reactive 
  observeEvent(input$WQSbegin, {
    
    # Weblink component based on input$WQSwaterbodyType
    if(input$WQSwaterbodyType == 'Riverine'){WQSreactive_objects$otherLayers <- "Streams/Rivers%20WQS;Public%20water%20supply;Trout;All%20other%20streams/rivers"}
    if(input$WQSwaterbodyType == 'Lacustrine'){WQSreactive_objects$otherLayers <- "Lakes/Reservoirs%20WQS;Public%20Water%20Supply;Trout;All%20other%20lakes/reservoirs"}
    if(input$WQSwaterbodyType == 'Estuarine'){WQSreactive_objects$otherLayers <- "Estuaries%20WQS;Estuarine%20waters;Tidal%20flow%20paths"}
    
    
    # Bring in existing WQS information
    WQSreactive_objects$WQSlookup <- loadData("WQSlookupTable")
    # limit conventionals_DWQS to just chosen subbasin
    WQSreactive_objects$conventionals_DWQS_Region <- st_intersection(conventionals_DWQS, 
                                                                     filter(subbasins, BASIN_CODE %in% basinCodes())) %>%
      mutate(`DEQ GIS Web App Link` =  paste0(webLinkpart1, StationID, webLinkpart2, WQSreactive_objects$otherLayers, webLinkpart3)) %>%
      dplyr::select(`DEQ GIS Web App Link`, everything())
    
    # All sites limited to waterbody type and subbasin
    WQSreactive_objects$snap_input <- readRDS('data/WQStable07262022.RDS') %>% # July 2022 effort with expected DEQ sites
      #  readRDS('data/WQStable.RDS') %>% # original effort
      filter(str_extract(WQS_ID, "^.{2}") %in% filter(WQSlayerConversion, waterbodyType %in% input$WQSwaterbodyType)$WQS_ID) %>%
      filter(gsub("_","",str_extract(WQS_ID, ".{3}_")) %in% 
               str_pad(unique(filter(subbasinOptionsByWQStype, SubbasinOptions %in% basinCodes())$SubbasinOptions), 
                       width = 2, side = 'left', pad = '0')) %>%
      # filter out any sites that happen to have existing WQS_ID
      filter(! StationID %in% WQSreactive_objects$WQSlookup$StationID) %>%
      group_by(StationID) %>%
      mutate(n = n()) %>% ungroup()
    # Sites limited to just region of interest
    WQSreactive_objects$snap_input_Region <- WQSreactive_objects$snap_input %>%
      left_join(WQSreactive_objects$conventionals_DWQS_Region, by = 'StationID') %>%
      filter(!is.na(Latitude) | !is.na(Longitude)) %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = T, # don't remove these lat/lon cols from df
               crs = 4326) %>%
      st_intersection(filter(assessmentRegions, ASSESS_REG %in% input$WQSDEQregionSelection)) %>%
      st_drop_geometry() %>% # back to tibble
      rename('Buffer Distance' = 'Buffer.Distance') %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, n)# %>%
      # # Add back any missing sites that are dropped because they don't fall into assessment region boundaries
      # bind_rows(
      #   filter(WQSreactive_objects$snap_input, StationID %in% 
      #            filter(readRDS('data/missingSites.RDS'), 
      #                   ASSESS_REG %in% input$WQSDEQregionSelection)$FDT_STA_ID) %>%
      #     left_join(conventionals_DWQS, by = 'StationID') %>%
      #     dplyr::select(StationID, WQS_ID, `Buffer Distance`, n) )
    # Make dataset of all sites for highlighting purposes, preliminary list
    WQSreactive_objects$sitesUnique <- WQSreactive_objects$snap_input %>%
      full_join(WQSreactive_objects$conventionals_DWQS_Region, by = 'StationID') %>%
      # catch for sites outside a region
      {if(nrow(filter(.,is.na(FDT_STA_ID))) > 0)
        dplyr::select(., names(WQSreactive_objects$snap_input)) %>%
          left_join( conventionals_DWQS, by = 'StationID') %>%
          mutate(`DEQ GIS Web App Link` =  paste0(webLinkpart1, StationID, webLinkpart2, WQSreactive_objects$otherLayers, webLinkpart3)) %>%
          dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, `DEQ GIS Web App Link`, everything())
        else .} %>%
      filter(!is.na(Latitude) | !is.na(Longitude)) %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326)
    # Make dataset of multiple segments snapped to single site IN REGION
    WQSreactive_objects$tooMany <- filter(WQSreactive_objects$snap_input_Region, n > 1) %>%
      group_by(StationID) %>% mutate(colorFac = row_number()) %>% ungroup() 
    # Make a dataset of actual segments for plotting
    WQSreactive_objects$tooMany_sf <- filter(WQSs(), WQS_ID %in% WQSreactive_objects$tooMany$WQS_ID) %>%
      left_join(WQSreactive_objects$tooMany, by = 'WQS_ID') %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())
    if(input$WQSwaterbodyType == 'Estuarine'){
      WQSreactive_objects$tooMany_sf_EL <- filter(WQSsEL(), WQS_ID %in% WQSreactive_objects$tooMany$WQS_ID) %>%              # bonus polyline feature for Estuarine
        left_join(WQSreactive_objects$tooMany, by = 'WQS_ID') %>%
        dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())    
    } else {WQSreactive_objects$tooMany_sf_EL <- WQSs()[0,] %>%
      left_join(WQSreactive_objects$tooMany, by = 'WQS_ID') %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())} # create dummy variable
    # Make dataset of sites associated with too many segments IN REGION
    WQSreactive_objects$tooMany_sites <- filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$tooMany$StationID) %>%
      left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID') %>%
      {if(input$WQSwaterbodyType == 'Estuarine')
        rbind(left_join(filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$tooMany$StationID), 
                        WQSsEL() %>% st_drop_geometry(), by = 'WQS_ID'))
        else . } %>%
      distinct(StationID, .keep_all = T) %>%
      dplyr::select(-c(WQS_ID, `Buffer Distance`, n))
    # Make dataset of sites that snapped to a single WQS and join WQS info  IN REGION
    WQSreactive_objects$snapSingle <- filter(WQSreactive_objects$sitesUnique, n == 1 ) %>%
      {if(input$WQSwaterbodyType == 'Riverine')
        filter(., `Buffer Distance` != 'No connections within 80 m')
        else . } %>%
      filter(StationID %in% WQSreactive_objects$snap_input_Region$StationID) %>% # limit assignment to just what falls in a region
      left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID') %>%
      {if(input$WQSwaterbodyType == 'Estuarine')
        filter(., str_extract(WQS_ID, "^.{2}") == 'EP') %>% # keep just polygon result from above
          rbind(filter(WQSreactive_objects$sitesUnique, n == 1) %>%
                  filter(StationID %in% WQSreactive_objects$snap_input_Region$StationID & str_extract(WQS_ID, "^.{2}") == 'EL') %>%
                  left_join(WQSsEL() %>% st_drop_geometry(), by = 'WQS_ID') )
        else . } %>%
      mutate(`Buffer Distance` = as.factor(`Buffer Distance`))
    # Make a dataset of actual segments that snapped to a single site for plotting
    WQSreactive_objects$snapSingle_sf <- filter(WQSs(), WQS_ID %in% WQSreactive_objects$snapSingle$WQS_ID) %>%
      left_join(dplyr::select( WQSreactive_objects$snapSingle, StationID, `Buffer Distance`, n, WQS_ID) %>% st_drop_geometry(), by = 'WQS_ID') %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())
    if(input$WQSwaterbodyType == 'Estuarine'){
      WQSreactive_objects$snapSingle_sf_EL <- filter(WQSsEL(), WQS_ID %in% WQSreactive_objects$snapSingle$WQS_ID) %>% # bonus polyline feature for Estuarine
        left_join(dplyr::select( WQSreactive_objects$snapSingle, StationID, `Buffer Distance`, n, WQS_ID) %>% st_drop_geometry(), by = 'WQS_ID') %>%
        dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())    
    } else {WQSreactive_objects$snapSingle_sf_EL <- WQSs()[0,]  %>% # bonus polyline feature for Estuarine
      left_join(dplyr::select(WQSreactive_objects$snapSingle, StationID, `Buffer Distance`, n, WQS_ID) %>% st_drop_geometry(), by = 'WQS_ID') %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything()) } # create dummy variable
    # Make dataset of sites associated with no segments IN REGION
    WQSreactive_objects$snapNone <- filter(WQSreactive_objects$sitesUnique,  `Buffer Distance` == 'No connections within 80 m') %>% #is.na(WQS_ID)) %>%
      filter(StationID %in% WQSreactive_objects$snap_input_Region$StationID) %>% # limit assignment to just what falls in a region
      left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID')
    # Make empty dataset of sites that assessors touched
    WQSreactive_objects$sitesAdjusted <-  WQSreactive_objects$sitesUnique[0,]  %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`) %>%
      mutate(Comments = as.character())
    # Make dataset for user to download
    WQSreactive_objects$finalWQS <- WQSreactive_objects$WQSlookup
    # Make dataset of all selectable sites on map
    #WQSreactive_objects$sitesUniqueFin <- WQSreactive_objects$conventionals_DWQS_Region 
  })
  
  #  # Make dataset of all selectable sites on map
  #  observe({
  #    req(WQSreactive_objects$sitesUnique, WQSreactive_objects$conventionals_DWQS_Region)
  #    WQSreactive_objects$sitesUniqueFin <- WQSreactive_objects$conventionals_DWQS_Region })
  #    # for now, all sites are in conventionals pull, but this may not always be true
  #      #rbind(WQSreactive_objects$sitesUnique, mutate(WQSreactive_objects$conventionals_DWQS_Region, WQS_ID = NA, `Buffer Distance` = NA, n = NA) %>% dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, FDT_STA_ID, everything()))  })
  
  
  # UI summaries of data pulled in to app, first and second tab
  output$singleSnapSummary1WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$snapSingle), ' stations that snapped to 1 WQS segment in preprocessing.'))})
  output$singleSnapSummary2WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$snapSingle), ' stations that snapped to 1 WQS segment in preprocessing.'))})
  output$snapTooManySummary1WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$tooMany_sites), ' stations that snapped to > 1 WQS segment in preprocessing.'))})
  output$snapTooManySummary2WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$tooMany_sites), ' stations that snapped to > 1 WQS segment in preprocessing.'))})
  output$noSnapSummary1WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$snapNone), ' stations that snapped to 0 WQS segments in preprocessing.'))})
  output$noSnapSummary2WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$snapNone), ' stations that snapped to 0 WQS segments in preprocessing.'))})
  
  
  
  ### WQS REVIEW TAB ##################################################################################
  
  output$test1 <- renderPrint({req(WQSs())
    #paste(
    #paste('WQSreactive_objects$tooMany_sites', WQSreactive_objects$tooMany_sites),
    #paste('nrow', nrow(WQSreactive_objects$tooMany_sf) ),
    #paste("sfc_MULTILINESTRING" %in% class(st_geometry(WQSreactive_objects$tooMany_sf))), sep ='<br>')
    #  if(#nrow(WQSreactive_objects$tooMany_sf) > 0 & 
    #    "sfc_MULTILINESTRING" %in% class(st_geometry(WQSreactive_objects$tooMany_sf)) 
    #     ){
    #    print('yes')}
    #WQSreactive_objects$conventionals_DWQS_Region
    
    filter( WQSreactive_objects$sitesUnique, `Buffer Distance` == 'No connections within 80 m')
  })
  
  
  # WQS Map
  output$WQSmap <- renderLeaflet({
    req(WQSreactive_objects$snap_input)
    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE, 
                 options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20,
                                         preferCanvas = TRUE)) %>%
      setView(-78, 37.5, zoom=7)  %>% 
      addCircleMarkers(data = WQSreactive_objects$conventionals_DWQS_Region, color='blue', fillColor='yellow', radius = 4,
                       fillOpacity = 0.5,opacity=0.8,weight = 1,stroke=T, group="Conventionals Stations in Basin",
                       label = ~FDT_STA_ID, layerId = ~FDT_STA_ID) %>% 
      #      {if("sfc_MULTIPOLYGON" %in% class(st_geometry(WQSs()))) 
      #        addPolygons(., data = WQSs(),
      #                    layerId = ~WQS_ID,
      #                    label=~WQS_ID, group="All WQS in selected Region/Basin", 
      #                    color = 'blue', #color = ~palTooMany(reactive_objects$tooMany$colorFac),
      #                    weight = 3,stroke=T,
      #                    popup=leafpop::popupTable(WQSs()),
      #                    popupOptions = popupOptions( maxHeight = 100 )) %>% 
      #          hideGroup("All WQS in selected Region/Basin") 
      #        else addPolylines(., data = WQSs(),
      #                          layerId = ~WQS_ID,
    #                          label=~WQS_ID, group="All WQS in selected Region/Basin", 
    #                          color = 'blue', #color = ~palTooMany(reactive_objects$tooMany$colorFac),
    #                          weight = 3,stroke=T,
    #                          popup=leafpop::popupTable(WQSs()),
    #                          popupOptions = popupOptions( maxHeight = 100 )) %>% 
    #          hideGroup("All WQS in selected Region/Basin")  } %>%
    addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
                fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
                group="Assessment Regions",
                popup=leafpop::popupTable(assessmentRegions, zcol=c('ASSESS_REG'))) %>% hideGroup('Assessment Regions') %>% #,'VAHU6','FedName'))) %>% hideGroup('Assessment Regions') %>%
      #      {if(input$WQSwaterbodyType == 'Estuarine')
      #        addPolylines(., data =WQSsEL(), # WQSs(),
      #                     layerId = ~WQS_ID,
      #                     label=~WQS_ID, group="All WQS in selected Region/Basin", 
      #                     color = 'orange',
      #                     weight = 3,stroke=T,
      #                     popup=leafpop::popupTable(WQSsEL()),#WQSs()),
      #                     popupOptions = popupOptions( maxHeight = 100 )) %>% 
      #          hideGroup("All WQS in selected Region/Basin") 
      #        else . } %>%
      inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
      inlmisc::AddSearchButton(group = "Conventionals Stations in Basin", zoom = 15,propertyName = "label",
                               textPlaceholder = "Search Conventionals Stations in Basin") %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c('Conventionals Stations in Basin',
                                         #"All WQS in selected Region/Basin",
                                         'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')  %>%
      hideGroup("Conventionals Stations in Basin")    
  })
  
  WQSmap_proxy <- leafletProxy("WQSmap")
  
  # Add layers to map as requested- Single snapped sites
  observeEvent(input$plotSingleSnapSummaryWQS, {
    if (nrow(WQSreactive_objects$snapSingle) > 0 ){
      WQSmap_proxy %>%
        addCircleMarkers(data=WQSreactive_objects$snapSingle,
                         layerId = ~paste0(StationID,'_snapSingle'), # need unique layerID 
                         label=~StationID, group="Stations Snapped to 1 WQS Segment", 
                         radius = 5, fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T, color = 'black',
                         fillColor= ~palBufferDistance(WQSreactive_objects$snapSingle$`Buffer Distance`)) %>%
        {if(nrow(WQSreactive_objects$snapSingle_sf) > 0 & "sfc_POLYGON" %in% class(st_geometry(WQSreactive_objects$snapSingle_sf))) 
          addPolygons(., data=WQSreactive_objects$snapSingle_sf,
                      layerId = ~paste0(WQS_ID,'_snapSingle'),  # need unique layerID 
                      label=~WQS_ID, group="WQS Segments of Stations Snapped to 1 Segment", 
                      color = 'blue', weight = 3,stroke=T,
                      popup=leafpop::popupTable(WQSreactive_objects$snapSingle_sf),
                      popupOptions = popupOptions( maxHeight = 100 )) %>% 
            hideGroup("WQS Segments of Stations Snapped to 1 Segment")
          else . } %>%
        {if(nrow(WQSreactive_objects$snapSingle_sf) > 0 & "sfc_LINESTRING" %in% class(st_geometry(WQSreactive_objects$snapSingle_sf)))
          addPolylines(., data=WQSreactive_objects$snapSingle_sf,
                       layerId = ~paste0(WQS_ID,'_snapSingle'),  # need unique layerID 
                       label=~WQS_ID, group="WQS Segments of Stations Snapped to 1 Segment", 
                       color = 'blue', weight = 3,stroke=T,
                       popup=leafpop::popupTable(WQSreactive_objects$snapSingle_sf),
                       popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("WQS Segments of Stations Snapped to 1 Segment")
          else . } %>%
        {if(nrow(WQSreactive_objects$snapSingle_sf_EL) > 0)
          addPolylines(., data=WQSreactive_objects$snapSingle_sf_EL,
                       layerId = ~paste0(WQS_ID,'_snapSingle'),  # need unique layerID 
                       label=~WQS_ID, group="WQS Segments of Stations Snapped to 1 Segment", 
                       color = 'blue', weight = 3,stroke=T,
                       popup=leafpop::popupTable(WQSreactive_objects$snapSingle_sf_EL),
                       popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("WQS Segments of Stations Snapped to 1 Segment")  
          else . } %>%
        addLegend(position = 'topright', pal = palBufferDistance, values = WQSreactive_objects$snapSingle$`Buffer Distance`, 
                  group = 'Stations Snapped to 1 WQS Segment') %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to 1 Segment",
                                           "Stations Snapped to > 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 WQS Segments",
                                           'Conventionals Stations in Basin',
                                           #"All WQS in selected Region/Basin",
                                           'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft')
    } else {
      showNotification("There are no sites that snapped to only 1 WQS segment in preprocessing steps. Nothing to plot.")
    }
  })
  
  # Add layers to map as requested- Too many snapped sites
  observeEvent(input$plotSnapTooManySummaryWQS, {
    
    if(nrow(WQSreactive_objects$tooMany_sites) > 0){
      palTooMany <- colorNumeric(c('green','yellow', 'blue','red', 'pink','purple'), domain = WQSreactive_objects$tooMany$colorFac)
      
      WQSmap_proxy %>%
        addCircleMarkers(data=WQSreactive_objects$tooMany_sites,
                         layerId = ~paste0(StationID,'_tooMany'),  # need unique layerID 
                         label=~StationID, group="Stations Snapped to > 1 WQS Segment", 
                         color='black', fillColor='red', radius = 5,
                         fillOpacity = 0.8,opacity=0.5,weight = 2,stroke=T) %>%
        {if(nrow(WQSreactive_objects$tooMany_sf) > 0 & "sfc_POLYGON" %in% class(st_geometry(WQSreactive_objects$tooMany_sf))) 
          addPolygons(., data=WQSreactive_objects$tooMany_sf,
                      layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                      label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                      color = ~palTooMany(WQSreactive_objects$tooMany_sf$colorFac),weight = 3,stroke=T,
                      popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf),
                      popupOptions = popupOptions( maxHeight = 100 )) %>% 
            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")
          else . } %>%
        {if(nrow(WQSreactive_objects$tooMany_sf) > 0 & "sfc_LINESTRING" %in% class(st_geometry(WQSreactive_objects$tooMany_sf)))
          addPolylines(., data=WQSreactive_objects$tooMany_sf,
                       layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                       label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                       color = ~palTooMany(WQSreactive_objects$tooMany_sf$colorFac),weight = 3,stroke=T,
                       popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf),
                       popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")
          else . } %>%
        {if(nrow(WQSreactive_objects$tooMany_sf_EL) > 0)
          addPolylines(., data=WQSreactive_objects$tooMany_sf_EL,
                       layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                       label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                       color = ~palTooMany(WQSreactive_objects$tooMany_sf_EL$colorFac),weight = 3,stroke=T,
                       popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf_EL),
                       popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")  
          else . } %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to 1 Segment",
                                           "Stations Snapped to > 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 WQS Segments",
                                           'Conventionals Stations in Basin',
                                           #"All WQS in selected Region/Basin",
                                           'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft') 
    } else {
      showNotification("There are no sites that snapped to > 1 WQS segment in preprocessing steps. Nothing to plot.")
    }   })
  
  # Add layers to map as requested- 0 snapped sites
  observeEvent(input$plotNoSnapSummaryWQS, {
    if (nrow(WQSreactive_objects$snapNone) > 0 ){
      WQSmap_proxy %>%
        addCircleMarkers(data=WQSreactive_objects$snapNone,
                         layerId = ~paste0(StationID,'_snapNone'), # need unique layerID 
                         label=~StationID, 
                         group="Stations Snapped to 0 WQS Segments", 
                         color='black', fillColor='orange', radius = 5,
                         fillOpacity = 1,opacity=0.5,weight = 2,stroke=T) %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to 1 Segment",
                                           "Stations Snapped to > 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 WQS Segments",
                                           'Conventionals Stations in Basin',
                                           #"All WQS in selected Region/Basin",
                                           'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft')
    } else {
      showNotification("There are no sites that snapped to 0 WQS segments in preprocessing steps. Nothing to plot.")
    }
  })
  
  # Map marker click (to identify selected sites
  observeEvent(input$WQSmap_marker_click, {
    site_click <- input$WQSmap_marker_click # this is all the info based on your click
    siteid <- strsplit(site_click$id, "_")[[1]][1] # this is just the layerID associated with your click
    # have to remove the unique layerID after _ to make sense of StationID
    
    if(!is.null(siteid)){ # if you clicked a point with info, find all Stations that match (with a round)
      # first find site matches from user input dataset, by lat and long
      siteMatches <- filter(WQSreactive_objects$sitesUnique, 
                            StationID %in% siteid) %>%
        st_drop_geometry() %>%
        pull(StationID)
      
      # and save all this info for later
      siteid_current <-  c(siteMatches)#, as.character(existingSiteMatches))
      
      # add the current site(s) to the selected list for highlighting and displaying in table
      if(is.null(WQSreactive_objects$namesToSmash)){
        WQSreactive_objects$namesToSmash <- siteid_current
      } else {
        WQSreactive_objects$namesToSmash <- append(siteid_current, WQSreactive_objects$namesToSmash)    }
    }
  })
  
  # Update map marker highlights
  observeEvent(WQSreactive_objects$namesToSmash, ignoreNULL=F, {
    if(!is.null(WQSreactive_objects$namesToSmash)){
      WQSmap_proxy %>%
        clearGroup(group='highlight') %>%
        addCircleMarkers(data=filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$namesToSmash),
                         layerId = ~paste0(StationID,'_sitesHighlighted'),  # need unique layerID 
                         group='highlight', 
                         radius = 20, 
                         color='chartreuse', opacity = 0.75, fillOpacity = 0.4)  
    } else {
      WQSmap_proxy %>%
        clearGroup(group='highlight') }  })
  
  ## Clear all selected sites
  observeEvent(input$clear_allWQS, {
    WQSreactive_objects$namesToSmash=NULL
    WQSmap_proxy %>%
      clearGroup(group='highlight')  })
  
  ## Accept Snapped WQS Modal
  observeEvent(input$acceptWQS, {
    showModal(modalDialog(title = 'Accept Snapped WQS', size = 'l', easyClose = T,
                          DT::renderDataTable({
                            filter(WQSreactive_objects$sitesUnique, FDT_STA_ID %in% WQSreactive_objects$namesToSmash) %>%
                              dplyr::select(-`DEQ GIS Web App Link`) %>%
                              st_drop_geometry() %>%
                              datatable(rownames = F,  editable = 'cell',
                                        options = list(dom = 't', scrollX= TRUE, scrollY = '125px'),selection = 'none')  }),
                          br(), br(),
                          textInput('acceptCommentWQS', 'Additional Comments and Documentation'),
                          actionButton('accept_okWQS', 'Accept', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('check-circle')),
                          actionButton('accept_cancelWQS', 'Cancel', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('window-close'))    ))
    jqui_draggable(selector = '.modal-content')  })
  
  # Do something with WQS Accept Modal
  observeEvent(input$accept_cancelWQS, {removeModal()})
  observeEvent(input$accept_okWQS, {
    # Get name and WQS_ID information from tooMany
    sitesUpdated <- filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$namesToSmash) %>%
      #st_drop_geometry() %>%
      #distinct(FDT_STA_ID, .keep_all = T) %>%
      mutate(`Buffer Distance` = paste0('Manual Review | ', `Buffer Distance`),
             Comments = paste0('Manual Accept | ',input$acceptCommentWQS)) %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, Comments)
    
    
    # add the current site(s) to the adjusted list 
    #if(nrow(reactive_objects$sitesAdjusted) == 0){
    #  reactive_objects$sitesAdjusted
    #} else {
    WQSreactive_objects$sitesAdjusted <- rbind(WQSreactive_objects$sitesAdjusted, sitesUpdated) # rbind works better for sf objects
    #}
    
    dropMe <- unique(sitesUpdated$StationID)
    
    ## Remove Site from "to do' list
    # remove from snap to > 1 WQS sites and segments
    WQSreactive_objects$tooMany_sites <- filter(WQSreactive_objects$tooMany_sites, !(StationID %in% dropMe)) # drop sites
    WQSreactive_objects$tooMany_sf <- filter(WQSreactive_objects$tooMany_sf, !(StationID %in% dropMe)) # drop segments
    WQSreactive_objects$tooMany_sf_EL <- filter(WQSreactive_objects$tooMany_sf_EL, !(StationID %in% dropMe)) # drop segments
    
    
    # and if part of snap to 1 WQS, fix that data
    WQSreactive_objects$snapSingle <- filter(WQSreactive_objects$snapSingle, !(StationID%in% dropMe)) # drop sites
    WQSreactive_objects$snapSingle_sf <- filter(WQSreactive_objects$snapSingle_sf, !(StationID %in% dropMe)) # drop segments
    WQSreactive_objects$snapSingle_sf_EL <- filter(WQSreactive_objects$snapSingle_sf_EL, !(StationID %in% dropMe)) # drop segments
    
    # and if part of snap to 0 WQS, fix that data
    WQSreactive_objects$snapNone <- filter(WQSreactive_objects$snapNone, !(StationID %in% dropMe)) # drop sites
    
    # update output dataset
    WQSreactive_objects$finalWQS <- filter(WQSreactive_objects$finalWQS, !(StationID %in% dropMe)) %>%
      bind_rows(sitesUpdated %>% st_drop_geometry())
    
    
    # Empty map selection
    WQSreactive_objects$namesToSmash <- NULL
    
    ### Clear modal
    removeModal()
  })
  
  
  
  ## Manual WQS Adjustment Modal
  observeEvent(input$changeWQS, {
    showModal(modalDialog(title = 'Manually Adjust WQS', size = 'l', easyClose = T, 
                          DT::renderDataTable({
                            filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$namesToSmash) %>%
                              st_drop_geometry() %>%
                              dplyr::select(-`DEQ GIS Web App Link`) %>%
                              datatable(rownames = F,  editable = 'cell',
                                        options = list(dom = 't', scrollX= TRUE, scrollY = '125px'),selection = 'none')  }),
                          br(), br(),
                          textInput('mergeWQSID', "Manually input the WQS_ID you want connected to the selected station."),
                          helpText("Hint: Copy/Paste is your friend."),
                          # too computationally expensive and doesn't allow for other waterbody types to be manually input
                          #selectInput('mergeWQSID','Choose WQS to connect to station', 
                          #            choices = unique(c(as.character(filter(WQSreactive_objects$tooMany, 
                          #                                                   StationID %in% WQSreactive_objects$namesToSmash)$WQS_ID), # likely WQS
                          #                               WQS_ID_subbasinOptions()))), # less likely WQS but an option
                          #                               #as.character(allWQS_ID$WQS_ID)))), # less likely WQS but an option
                          textInput('adjustCommentWQS', 'Additional Comments and Documentation'),
                          actionButton('adjust_okWQS', 'Accept', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('check-circle')),
                          actionButton('adjust_cancelWQS', 'Cancel', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('window-close'))    ))  
    jqui_draggable(selector = '.modal-content')  })
  
  # Do something with WQS Adjustment Modal
  observeEvent(input$adjust_cancelWQS, {removeModal()})
  observeEvent(input$adjust_okWQS, {
    # Get name and location information from tooMany
    sitesUpdated <- filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$namesToSmash) %>%
      #st_drop_geometry() %>%
      distinct(StationID, .keep_all = T) %>% # this time you do want to run a distinct to avoid duplicated rows
      mutate(WQS_ID = input$mergeWQSID,
             `Buffer Distance` = paste0('Manual Review | ', `Buffer Distance`),
             Comments = paste0('Manual Accept | ',input$adjustCommentWQS)) %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, Comments)
    
    # add the current site(s) to the adjusted list 
    WQSreactive_objects$sitesAdjusted <- rbind(WQSreactive_objects$sitesAdjusted, sitesUpdated) # rbind works better for sf objects
    
    dropMe <- unique(sitesUpdated$StationID)
    
    ## Remove Site from "to do' list
    # remove from snap to > 1 WQS sites and segments
    WQSreactive_objects$tooMany_sites <- filter(WQSreactive_objects$tooMany_sites, !(StationID %in% dropMe)) # drop sites
    WQSreactive_objects$tooMany_sf <- filter(WQSreactive_objects$tooMany_sf, !(StationID %in% dropMe)) # drop segments
    
    # and if part of snap to 1 WQS, fix that data
    WQSreactive_objects$snapSingle <- filter(WQSreactive_objects$snapSingle, !(StationID%in% dropMe)) # drop sites
    WQSreactive_objects$snapSingle_sf <- filter(WQSreactive_objects$snapSingle_sf, !(StationID %in% dropMe)) # drop segments
    WQSreactive_objects$snapSingle_sf_EL <- filter(WQSreactive_objects$snapSingle_sf_EL, !(StationID %in% dropMe)) # drop segments
    
    # and if part of snap to 0 WQS, fix that data
    WQSreactive_objects$snapNone <- filter(WQSreactive_objects$snapNone, !(StationID %in% dropMe)) # drop sites
    
    # update output dataset
    WQSreactive_objects$finalWQS <- filter(WQSreactive_objects$finalWQS, !(StationID %in% dropMe)) %>%
      bind_rows(sitesUpdated %>% st_drop_geometry())
    
    # Empty map selection
    WQSreactive_objects$namesToSmash <- NULL
    
    ### Clear modal
    removeModal()
  })
  
  
  
  # update WQSmap after WQS adjustment
  observe({
    req(WQSreactive_objects$sitesAdjusted)
    if(nrow(WQSreactive_objects$tooMany_sites)> 0){
      palTooMany <- colorNumeric(c('green','yellow', 'blue','red', 'pink','purple'), domain = WQSreactive_objects$tooMany$colorFac)
    } else {
      palTooMany <- colorNumeric(c('green','yellow', 'blue','red', 'pink','purple'), domain = 6)
    }
    
    ## Update proxy map
    if(nrow(WQSreactive_objects$sitesAdjusted) > 0){
      WQSmap_proxy %>% 
        # have to manually clear old sites to 'wipe' leaflet memory of joined sites
        clearGroup("Stations Snapped to 0 WQS Segments") %>%
        clearGroup("Stations Snapped to 1 WQS Segment") %>%
        clearGroup("WQS Segments of Stations Snapped to 1 Segment") %>%
        clearGroup("Stations Snapped to > 1 WQS Segment") %>%
        clearGroup("WQS Segments of Stations Snapped to > 1 Segment") %>%
        
        
        {if(nrow(WQSreactive_objects$tooMany_sites) > 0)
          addCircleMarkers(., data=WQSreactive_objects$tooMany_sites,
                           layerId = ~paste0(StationID,'_tooMany'),  # need unique layerID 
                           label=~StationID, group="Stations Snapped to > 1 WQS Segment", 
                           color='black', fillColor='red', radius = 5,
                           fillOpacity = 0.8,opacity=0.5,weight = 2,stroke=T) 
          else . } %>%
        {if(nrow(WQSreactive_objects$tooMany_sf) > 0) 
        {if("sfc_POLYGON" %in% class(st_geometry(WQSreactive_objects$tooMany_sf))) 
          addPolygons(., data=WQSreactive_objects$tooMany_sf,
                      layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                      label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                      color = ~palTooMany(WQSreactive_objects$tooMany_sf$colorFac),weight = 3,stroke=T,
                      popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf),
                      popupOptions = popupOptions( maxHeight = 100 ))# %>% 
          #            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")
          else
            addPolylines(., data=WQSreactive_objects$tooMany_sf,
                         layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                         label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                         color = ~palTooMany(WQSreactive_objects$tooMany_sf$colorFac),weight = 3,stroke=T,
                         popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf),
                         popupOptions = popupOptions( maxHeight = 100 )) #%>%
          #            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")
        }
          else . } %>%
        {if(nrow(WQSreactive_objects$tooMany_sf_EL) > 0)
          addPolylines(., data=WQSreactive_objects$tooMany_sf_EL,
                       layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                       label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                       color = ~palTooMany(WQSreactive_objects$tooMany_sf_EL$colorFac),weight = 3,stroke=T,
                       popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf_EL),
                       popupOptions = popupOptions( maxHeight = 100 )) #%>%
          #            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")  
          else . } %>%
        {if(nrow(WQSreactive_objects$snapSingle) > 0)
          addCircleMarkers(., data=WQSreactive_objects$snapSingle,
                           layerId = ~paste0(StationID,'_snapSingle'), # need unique layerID 
                           label=~StationID, group="Stations Snapped to 1 WQS Segment", 
                           radius = 5, fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T, color = 'black',
                           fillColor= ~palBufferDistance(WQSreactive_objects$snapSingle$`Buffer Distance`)) #%>%
          else .}  %>%
        {if(nrow(WQSreactive_objects$snapSingle_sf) > 0) 
        {if("sfc_MULTIPOLYGON" %in% class(st_geometry(WQSreactive_objects$snapSingle_sf))) 
          addPolygons(., data=WQSreactive_objects$snapSingle_sf,
                      layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                      label=~WQS_ID, group="WQS Segments of Stations Snapped to 1 Segment", 
                      color = "blue",weight = 3,stroke=T,
                      popup=leafpop::popupTable(WQSreactive_objects$snapSingle_sf),
                      popupOptions = popupOptions( maxHeight = 100 )) #%>% 
          #            hideGroup("WQS Segments of Stations Snapped to 1 Segment")
          else
            addPolylines(., data=WQSreactive_objects$snapSingle_sf,
                         layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                         label=~WQS_ID, group="WQS Segments of Stations Snapped to 1 Segment", 
                         color = "blue",weight = 3,stroke=T,
                         popup=leafpop::popupTable(WQSreactive_objects$snapSingle_sf),
                         popupOptions = popupOptions( maxHeight = 100 )) #%>%
          #            hideGroup("WQS Segments of Stations Snapped to 1 Segment")
        }
          else . } %>%
        {if(nrow(WQSreactive_objects$snapSingle_sf_EL) > 0)
          addPolylines(., data=WQSreactive_objects$snapSingle_sf_EL,
                       layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                       label=~WQS_ID, group="WQS Segments of Stations Snapped to 1 Segment", 
                       color = "blue",weight = 3,stroke=T,
                       popup=leafpop::popupTable(WQSreactive_objects$snapSingle_sf_EL),
                       popupOptions = popupOptions( maxHeight = 100 ))# %>%
          #            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")  
          else . } %>%
        {if(nrow(WQSreactive_objects$snapNone) > 0)
          addCircleMarkers(., data=WQSreactive_objects$snapNone,
                           layerId = ~paste0(StationID,'_snapNone'), # need unique layerID 
                           label=~StationID, 
                           group="Stations Snapped to 0 WQS Segments", 
                           color='black', fillColor='orange', radius = 5,
                           fillOpacity = 1,opacity=0.5,weight = 2,stroke=T)
          else . } %>%
        addCircleMarkers(data=WQSreactive_objects$sitesAdjusted,
                         layerId = ~paste0(StationID,'_sitesAdjusted'),  # need unique layerID 
                         label=~StationID, group="Adjusted Sites", 
                         color='black', fillColor='purple', radius = 5,
                         fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>% 
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to 1 Segment",
                                           "Stations Snapped to > 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 WQS Segments",
                                           'Conventionals Stations in Basin',
                                           #"All WQS in selected Region/Basin",
                                           'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft') 
    }    })
  
  
  
  
  
  ### Stations Data and Spatially Joined WQS Tab
  output$selectedSiteTableWQS <- DT::renderDataTable({
    req(WQSreactive_objects$namesToSmash)
    filter(WQSreactive_objects$sitesUnique, FDT_STA_ID %in% WQSreactive_objects$namesToSmash) %>%
      st_drop_geometry() %>%
      datatable(rownames = F, escape= F, options = list(dom = 't', scrollX= TRUE, scrollY = '100px'),selection = 'none')  })
  
  output$associatedWQSTableWQS <- DT::renderDataTable({
    req(WQSreactive_objects$namesToSmash)
    filter(WQSs(), WQS_ID %in% filter(WQSreactive_objects$snap_input, StationID %in% WQSreactive_objects$namesToSmash)$WQS_ID) %>%
      {if(input$WQSwaterbodyType == 'Estuarine')
        rbind(.,
              filter(WQSsEL(), WQS_ID %in%
                       filter(WQSreactive_objects$snap_input, StationID %in% WQSreactive_objects$namesToSmash)$WQS_ID) ) 
        else . } %>%
      st_drop_geometry() %>%
      dplyr::select(WQS_ID, everything()) %>%
      distinct(WQS_ID, .keep_all = T) %>% # for some reason this is duplicated in the app but cannot recreate on local testing
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '200px'),selection = 'none')  })
  
  ### Updated Stations Data and Manually QAed WQS Tab
  output$adjustedStationsTableWQS <- DT::renderDataTable({
    req(WQSreactive_objects$namesToSmash, WQSreactive_objects$sitesAdjusted)
    filter(WQSreactive_objects$sitesAdjusted, StationID %in% WQSreactive_objects$namesToSmash) %>%
      st_drop_geometry() %>%
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '100px'),selection = 'none')  })
  
  ## User adjusted WQS table, WQS details
  output$associatedWQSTableWQSQA <- DT::renderDataTable({
    req(WQSreactive_objects$namesToSmash, WQSreactive_objects$sitesAdjusted)
    filter(WQSs() %>% st_drop_geometry(), WQS_ID %in% filter(WQSreactive_objects$sitesAdjusted, StationID %in% WQSreactive_objects$namesToSmash)$WQS_ID) %>%
      {if(input$WQSwaterbodyType == 'Estuarine')
        rbind(.,
              filter(WQSsEL() %>% st_drop_geometry(), WQS_ID %in% 
                       filter(WQSreactive_objects$sitesAdjusted, StationID %in% WQSreactive_objects$namesToSmash)$WQS_ID) )
        else . } %>%
      #   st_drop_geometry() %>%
      dplyr::select(WQS_ID, everything()) %>%
      distinct(WQS_ID, .keep_all = T) %>% # for some reason this is duplicated in the app but cannot recreate on local testing
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '200px'),selection = 'none')  })
  
  #  ## Download WQS Information
  #  export_file=reactive(paste0('WQSlookupTable.csv'))#, region(), '_', basin(),'_',input$assessmentType, '_', Sys.Date(),'.csv'))
  #  output$downloadWQS <- downloadHandler(
  #    filename=function(){export_file()},
  #    content = function(file) {
  #      write.csv(WQSreactive_objects$finalWQS %>%
  #                  # get rid of geometry if needed
  #                  {if('geometry' %in% names(WQSreactive_objects$finalWQS))
  #                    dplyr::select(., -geometry) 
  #                    else . } %>%
  #                  as.data.frame(), file, row.names = F) }) 
  
  observeEvent(input$saveWQS, {
    saveData(WQSreactive_objects$finalWQS %>%
               # get rid of geometry if needed
               {if('geometry' %in% names(WQSreactive_objects$finalWQS))
                 dplyr::select(., -geometry) 
                 else . } %>%
               as.data.frame(), "WQSlookupTable")
    showNotification("WQS information saved on Connect server.")
    
  })  
  
})