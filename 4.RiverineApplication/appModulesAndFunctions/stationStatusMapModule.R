# Station Status Map Function
indStatusMap <- function(parameter, status){
  pal <- colorFactor(
    palette = c('red', 'yellow','green', 'gray'),
    domain = c(1, 2, 3, 4))
  
  if(parameter == 'Overall Status'){
    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE) %>%
      addCircleMarkers(data = status, color='black', fillColor=~pal(status$stationOverallScore), radius = 6,
                       fillOpacity = 0.5,opacity=1,weight = 2,stroke=T,group="Overall Station Status Summary",
                       label = ~STATION_ID, layerId = ~STATION_ID,
                       popup = leafpop::popupTable(status, zcol=c( "STATION_ID", "Overall Station Result", "n Parameters of lowest status"))  ) %>% 
      addLegend('topright', colors = c('red', 'yellow','green', 'gray'),
                labels = c('Station contains at least <br>one parameter status of <br>IM or 10.5% Exceedance',
                           'Station contains at least <br>one parameter status of <br>IN or Review',
                           'Station contains at least <br>one parameter status of S <br>and no IM, IN, <br>10.5% Exceedance, or Review', 
                           'Station contains all <br>NA statuses'), title = 'Legend') %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c('Overall Station Status Summary'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft') 
  } else {
    indParameter <- filter(status, Parameter %in% parameter)
    
    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE) %>%
      addCircleMarkers(data = indParameter , color='black', fillColor=~pal(indParameter$individualScore), radius = 6,
                       fillOpacity = 0.5,opacity=1,weight = 2,stroke=T,group=paste(parameter, "Station Summary"),
                       label = ~STATION_ID, layerId = ~STATION_ID,
                       popup = leafpop::popupTable(indParameter , zcol=c( "STATION_ID", "Parameter", "Status"))  ) %>% 
      addLegend('topright', colors = c('red', 'yellow','green', 'gray'),
                labels = c('Station status of IM or 10.5% Exceedance',
                           'Station status of IN or Review',
                           'Station status of S', 
                           'Station status of NA'), title = 'Legend') %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c(paste(parameter, "Station Summary")),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')  }
}