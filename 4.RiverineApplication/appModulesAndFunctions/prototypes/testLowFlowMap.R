oneStation <- filter(AUData,FDT_STA_ID %in% myStation) %>% #'2-DCK003.94') %>%
  filter(!is.na(FDT_TEMP_CELCIUS))
lowFlowData <- filter(oneStation, !is.na(`7Q10 Flag`))
lowFlowData$SampleDate <- as.POSIXct(lowFlowData$FDT_DATE_TIME, format="%m/%d/%y")



point <- dplyr::select(lowFlowData,  FDT_STA_ID, Longitude, Latitude, SampleDate, `7Q10 Flag Gage` ) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), 
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) # add projection
uniqueGageID <- unlist(strsplit(point$`7Q10 Flag Gage`, ", "))
gagePoint <- filter(assessmentWindowLowFlows,  `Gage ID` %in% uniqueGageID) %>% # pull all unique gages
  filter(Date %in% as.Date(point$SampleDate)) %>% #just the sample dates of interest
  distinct(`Gage ID`, .keep_all = T) %>% # dont need more than one point to plot on map
  st_as_sf(coords = c("Longitude", "Latitude"), 
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) # add projection


map1 <- mapview(point, color = 'yellow', lwd = 5, label= point$FDT_STA_ID, layer.name = c('Selected Station'),
                popup=NULL, legend= FALSE) +
  mapview(gagePoint, color = 'blue', lwd = 5, label= gagePoint$`Gage ID`, layer.name = c('Gages below 7Q10 in major river basin'),
          popup= leafpop::popupTable(gagePoint, zcol=c("Agency", "Gage ID", "Station Name", "Site Type",
                                                       "n7Q10", "7Q10 Flag", 
                                                       "BASIN_CODE", "BASIN_NAME")), legend= FALSE) 
  map1@map 

  
z <- filter(assessmentWindowLowFlows,  `Gage ID` %in% uniqueGageID)  %>% # pull all unique gages
  dplyr::select(Agency:`Site Type`, Date:BASIN_NAME, Latitude, Longitude) %>%  #reformat for easier reading
  mutate(`See gage info` = paste0('https://waterdata.usgs.gov/va/nwis/uv?site_no=',
                                  uniqueGageID,
                                  # '&format=gif_mult_sites&PARAmeter_cd=',
                                  # paramCode,
                                  "&period=&begin_date=", Date - 7,
                                  "&end_date=",Date + 7))
z <- filter(assessmentWindowLowFlows,  `Gage ID` %in% uniqueGageID)  %>% # pull all unique gages
  dplyr::select(Agency:`Site Type`, Date:BASIN_NAME, Latitude, Longitude) %>%  #reformat for easier reading
  # first build the weblink
  mutate(webLink1 = paste0('https://waterdata.usgs.gov/monitoring-location/',
                                  uniqueGageID,
                                  "#parameterCode=00065",
                                  "&startDT=", Date - 7,
                                  "&endDT=", Date + 7)) %>% 
  # then convert to a html format DT can handle in pretty way
  mutate(`See gage info` = paste0("<a href='",webLink1, "' target='_blank'>View Flow Data On USGS Website</a>")) %>%
  dplyr::select(-c(webLink1)) %>% 
  dplyr::select(Agency:n7Q10, `See gage info`, everything() )#reformat for easier reading


paste0('https://waterdata.usgs.gov/va/nwis/uv?site_no=',
       uniqueGageID[1],
       # '&format=gif_mult_sites&PARAmeter_cd=',
       # paramCode,
       "&period=&begin_date=", Date - 7,
       "&end_date=",Date + 7), 