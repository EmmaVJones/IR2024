httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))

library(tidyverse)
library(shiny)
library(shinyjs)
library(sf)
library(pins)
library(EnvStats)
library(lubridate)
library(config)
library(leaflet)
library(mapview)
library(inlmisc)
library(DT)
library(plotly)
library(readxl)
library(round)


# Bring in assessment Functions and app modules
source('appModulesAndFunctions/updatedBacteriaCriteria.R')
source('appModulesAndFunctions/multipleDependentSelectizeArguments.R')
source('appModulesAndFunctions/automatedAssessmentFunctions.R')


# Connect to R server to bring in pinned data
conn <- config::get("connectionSettings") # get configuration settings
board_register_rsconnect(key = conn$CONNECT_API_KEY,  
                         server = conn$CONNECT_SERVER)


modulesToReadIn <- c('thermocline','temperature','DO','pH', 'SpCond', 'Ecoli', 'Ecoli_AU',
                     'chlA','TP','TSI', 'Ammonia',  #'Nitrate', 'Chloride', 
                     'NitrateDissolvedAndTotal', 'ChlorideDissolvedAndTotal',
                     'Sulfate', 'metals',
                     'toxicsDissolvedAndTotal')
                     #'toxics')
for (i in 1:length(modulesToReadIn)){
  source(paste('appModulesAndFunctions/',modulesToReadIn[i],'Module.R',sep=''))
}




# Water Column Metals with static criteria (helpful for metals module)
staticLimit <- c("Antimony PWS", "Antimony All Other Surface Waters", "Arsenic Acute Freshwater", "Arsenic Chronic Freshwater", "Arsenic PWS",
                 "Arsenic Acute Saltwater", "Arsenic Chronic Saltwater", "Barium PWS","Cadmium PWS","ChromiumIII PWS",
                 "ChromiumVI Acute Freshwater", "ChromiumVI Chronic Freshwater", "ChromiumVI Acute Saltwater", "ChromiumVI Chronic Saltwater", 
                 "Lead PWS", "Mercury Acute Freshwater", "Mercury Chronic Freshwater", "Mercury Acute Saltwater", "Mercury Chronic Saltwater",
                 "Nickel PWS",  "Nickel All Other Surface Waters", "Uranium PWS","Selenium Acute Freshwater", "Selenium Chronic Freshwater", 
                 "Selenium PWS", "Selenium All Other Surface Waters","Thallium PWS", "Thallium All Other Surface Waters","Zinc PWS", 
                 "Zinc All Other Surface Waters")



# Loading screen
load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}


withinAssessmentPeriod <- function(x){
  if((range(unique(x$FDT_DATE_TIME))[1] < assessmentPeriod[1]) | 
     (range(unique(x$FDT_DATE_TIME))[2] > assessmentPeriod[2])){
    return('Data included that falls outside of assessment period. Review input data.')
  }else{return('All input data falls within the assessment period.')}
}



# change WQS for a given module
changeWQSfunction <- function(stationData,  # single station dataset
                              inputCLASS_DESCRIPTION){ # from user module)
  WQSvalues <- bind_rows(WQSvalues, 
                         tribble(
                           ~`pH Min`, ~`pH Max`, ~CLASS_DESCRIPTION, 
                           6.5, 9.5, 'SPSTDS = 6.5-9.5'))
  if(inputCLASS_DESCRIPTION != unique(stationData$CLASS_DESCRIPTION)){
    changedWQS <- filter(WQSvalues, CLASS_DESCRIPTION %in% inputCLASS_DESCRIPTION)
    return(dplyr::select(stationData, -c(`Description Of Waters`:CLASS_DESCRIPTION)) %>%
             mutate(CLASS = changedWQS$CLASS, 
                    `Description Of Waters` = changedWQS$`Description Of Waters` ) %>%
             left_join(changedWQS, by = c('CLASS', 'Description Of Waters'))) 
  } else {return(stationData)} 
}




## Old bacteria methods just hanging on for bacteria modules to calculate the old results for comparison, will phase out over time
bacteria_ExceedancesSTV_OLD <- function(x, STVlimit){                                    
  x %>% rename(parameter = !!names(.[2])) %>% # rename columns to make functions easier to apply
    mutate(limit = STVlimit, exceeds = ifelse(parameter > limit, T, F)) # Single Sample Maximum 
}

bacteria_ExceedancesGeomeanOLD <- function(x, bacteriaType, geomeanLimit){
  if(nrow(x) > 0){
    suppressWarnings(mutate(x, SampleDate = format(FDT_DATE_TIME,"%m/%d/%y"), # Separate sampling events by day
                            previousSample=lag(SampleDate,1),
                            previousSampleBacteria=lag(!! bacteriaType,1)) %>% # Line up previous sample with current sample line
                       rowwise() %>% 
                       mutate(sameSampleMonth= as.numeric(strsplit(SampleDate,'/')[[1]][1])  -  as.numeric(strsplit(previousSample,'/')[[1]][1])) %>% # See if sample months are the same, e.g. more than one sample per calendar month
                       filter(sameSampleMonth == 0 | is.na(sameSampleMonth)) %>% # keep only rows with multiple samples per calendar month  or no previous sample (NA) to then test for geomean
                       # USING CALENDAR MONTH BC THAT'S HOW WRITTEN IN GUIDANCE, rolling 4 wk windows would have been more appropriate
                       mutate(sampleMonthYear = paste(month(as.Date(SampleDate,"%m/%d/%y")),year(as.Date(SampleDate,"%m/%d/%y")),sep='/')) %>% # grab sample month and year to group_by() for next analysis
                       group_by(sampleMonthYear) %>%
                       mutate(geoMeanCalendarMonth =  EnvStats::geoMean(as.numeric(get(bacteriaType)), na.rm = TRUE), # Calculate geomean
                              limit = geomeanLimit, samplesPerMonth = n()))
  }
}
# How bacteria is assessed
bacteria_Assessment_OLD <- function(x, bacteriaType, geomeanLimit, STVlimit){
  if(nrow(x)>1){
    bacteria <- dplyr::select(x,FDT_DATE_TIME, !! bacteriaType)%>% # Just get relevant columns, 
      filter(!is.na(!!bacteriaType)) #get rid of NA's
    # Geomean Analysis (if enough n)
    if(nrow(bacteria)>0){
      bacteriaGeomean <- bacteria_ExceedancesGeomeanOLD(bacteria, bacteriaType, geomeanLimit) %>%     
        distinct(sampleMonthYear, .keep_all = T) %>%
        filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) %>% # minimum sampling rule for geomean to apply
        mutate(exceeds = TRUE) %>%
        dplyr::select(sampleMonthYear, geoMeanCalendarMonth, limit, exceeds, samplesPerMonth)
      geomeanResults <- quickStats(bacteriaGeomean, bacteriaType) %>%
        mutate(`Assessment Method` = 'Old Monthly Geomean')
      geomeanResults[,4] <- ifelse(is.na(geomeanResults[,4]),NA, dplyr::recode(geomeanResults[,4], 'Review' = paste('Review if ', bacteriaType,'_VIO > 1',sep='')))
      
      # Single Sample Maximum Analysis
      bacteriaSSM <- bacteria_ExceedancesSTV_OLD(bacteria, STVlimit) 
      SSMresults <- quickStats(bacteriaSSM, bacteriaType) %>% mutate(`Assessment Method` = 'Old Single Sample Maximum')
      return( rbind(geomeanResults, SSMresults) )
    }
  }
  
}






# TSI plot decision
TSIplotly <- function(dat, parameter){
  box1 <- data.frame(FDT_DATE_TIME = c(min(dat$FDT_DATE_TIME), min(dat$FDT_DATE_TIME), max(dat$FDT_DATE_TIME),max(dat$FDT_DATE_TIME)), y = c(80, 100, 100, 80))
  box2 <- data.frame(x = c(min(dat$FDT_DATE_TIME), min(dat$FDT_DATE_TIME), max(dat$FDT_DATE_TIME),max(dat$FDT_DATE_TIME)), y = c(60, 80, 80, 60))
  box3 <- data.frame(x = c(min(dat$FDT_DATE_TIME), min(dat$FDT_DATE_TIME), max(dat$FDT_DATE_TIME),max(dat$FDT_DATE_TIME)), y = c(40, 60, 60, 40))
  box4 <- data.frame(x = c(min(dat$FDT_DATE_TIME), min(dat$FDT_DATE_TIME), max(dat$FDT_DATE_TIME),max(dat$FDT_DATE_TIME)), y = c(0, 40, 40, 0))
  
  
  
  if(parameter == 'Secchi Depth'){
    return(
      plot_ly(data=box1)%>%
        add_polygons(x = ~FDT_DATE_TIME, y = ~y, data = box1, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Trophic State: Hypereutrophic')) %>%
        add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Trophic State: Eutrophic')) %>%
        add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Trophic State: Mesotrophic')) %>%
        add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Trophic State: Oligotrophic')) %>%
        add_lines(data=dat, x=~FDT_DATE_TIME,y=~`Overall TSI SD`, mode='line', line = list(color = 'black'),
                  hoverinfo = "text", text="Overall TSI Secchi Depth", name="Overall TSI Secchi Depth") %>%
        add_markers(data=dat, x= ~FDT_DATE_TIME, y= ~TSI_SD,mode = 'scatter', name="TSI_SD", marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",FDT_DATE_TIME),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("TSI Secchi Depth: ",TSI_SD)))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="TSI Secchi Depth (unitless)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10))) ) }
  if(parameter == 'Chlorophyll a'){
    return(
      plot_ly(data=box1)%>%
        add_polygons(x = ~FDT_DATE_TIME, y = ~y, data = box1, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Trophic State: Hypereutrophic')) %>%
        add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Trophic State: Eutrophic')) %>%
        add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Trophic State: Mesotrophic')) %>%
        add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Trophic State: Oligotrophic')) %>%
        add_lines(data=dat, x=~FDT_DATE_TIME,y=~`Overall TSI Chl a`, mode='line', line = list(color = 'black'),
                  hoverinfo = "text", text="Overall TSI Chlorophyll a", name="Overall TSI Chlorophyll a") %>%
        add_markers(data=dat, x= ~FDT_DATE_TIME, y= ~TSI_chla,mode = 'scatter', name="TSI_chla", marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",FDT_DATE_TIME),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("TSI Chlorophyll a: ",TSI_chla)))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="TSI Chlorophyll a (unitless)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10))) ) }
  if(parameter == 'Total Phosphorus'){
    return(
      plot_ly(data=box1)%>%
        add_polygons(x = ~FDT_DATE_TIME, y = ~y, data = box1, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Trophic State: Hypereutrophic')) %>%
        add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Trophic State: Eutrophic')) %>%
        add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Trophic State: Mesotrophic')) %>%
        add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Trophic State: Oligotrophic')) %>%
        add_lines(data=dat, x=~FDT_DATE_TIME,y=~`Overall TSI Chl a`, mode='line', line = list(color = 'black'),
                  hoverinfo = "text", text="Overall TSI Total Phosphorus", name="Overall TSI Total Phosphorus") %>%
        add_markers(data=dat, x= ~FDT_DATE_TIME, y= ~TSI_TP,mode = 'scatter', name="TSI_TP", marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",FDT_DATE_TIME),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("TSI Total Phosphorus: ",TSI_TP)))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="TSI Total Phosphorus (unitless)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10))) ) }
}
#TSIplotly(dat, 'Total Phosphorus')#'Chlorophyll a')#'Secchi Depth')

