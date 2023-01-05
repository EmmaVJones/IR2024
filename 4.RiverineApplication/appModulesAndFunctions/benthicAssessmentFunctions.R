# These scripts are helper functions for the Benthics Modules and automated fact sheet

## Benthics metrics
benthicResultsMetrics <- function(x, SCIresults, wadeableOnly, rep1Only){
  z <- filter(SCIresults, StationID %in% unique(x$FDT_STA_ID)) %>%
    filter(`Target Count` == 110) %>% # removing non rarified samples takes care of removing QA samples
    {if(wadeableOnly)
      filter(., Gradient != 'Boatable')
      else .} %>%
    {if(rep1Only)
      filter(., RepNum == 1)
      else .}
  out <- list()
  if(nrow(z) > 0){
    z1 <- mutate(z, Year = lubridate::year(`Collection Date`))
    spring <- filter(z1, Season %in% 'Spring' )
    fall <- filter(z1, Season %in% 'Fall' )
    # output list with all metrics
    out$data <- z1
    out$roundup <- z1 %>%
      group_by(StationID) %>%
      summarise(`n Samples` = n(),
                `Average SCI` = round(mean(`SCI Score`), digits = 0),
                `Minimum SCI` = round(min(`SCI Score`), digits = 0),
                `Maximum SCI` = round(max(`SCI Score`), digits = 0)) %>%
      bind_cols(spring %>%
                  summarise(`n Spring Samples` = n(),
                            `Spring Average SCI` = round(mean(`SCI Score`), digits = 0))) %>%
      bind_cols(fall %>%
                  summarise(`n Fall Samples` = n(),
                            `Fall Average SCI` = round(mean(`SCI Score`), digits = 0)))
    
    out$yearlyAverage <- z1 %>%
      group_by(Year) %>%
      summarise(`Yearly Average` = round(mean(`SCI Score`), digits = 0)) 
  } else {
    out$data <- NA
    out$roundup <- tibble(StationID = NA,  
                          `n Samples`=NA, `Average SCI` =NA, `Minimum SCI` = NA, `Maximum SCI`= NA,
                          `n Spring Samples`= NA, `Spring Average SCI`=NA, 
                          `n Fall Samples` = NA, `Fall Average SCI`= NA)
    out$yearlyAverage <- tibble(Year= NA, `Yearly Average`=NA)  }
  return(out)
}
#benthicResultsMetrics(x, VCPMI63results, TRUE, TRUE)

SCIchooser <- function(x){
  if(unique(x$EPA_ECO_US_L3CODE %in% c(NA, 45, 64, 66, 67, 69))){return('VSCI')
  } else {
    if(unique(x$EPA_ECO_US_L3CODE) %in% 63 | 
       str_detect(unique(x$Basin_Code), 'Chowan')){return('VCPMI 63 + Chowan')}
    if(unique(x$EPA_ECO_US_L3CODE)  %in% 65  #& !str_detect(unique(x$Basin_Code), 'Chowan')
    ){return('VCPMI 65 - Chowan')}
  }
}
#SCIchooser(x)



## functions for Bioassessment use
# Template to standardize variables for DT habitat heatmap across high and low gradients
habitatTemplate <- tibble(StationID = NA, HabSampID = NA, `Collection Date` = NA, `HabSample Comment` = NA, `Total Habitat Score` = NA, `Bank Stability` = NA, 
                          `Channel Alteration` = NA, `Channel Flow Status` = NA, `Channel Sinuosity` = NA, Embeddedness = NA, 
                          `Epifaunal Substrate / Available Cover` = NA, `Pool Substrate Characterization` = NA, `Pool Variability` = NA, 
                          `Frequency of riffles (or bends)` = NA, `Riparian Vegetative Zone Width` = NA, `Sediment Deposition` = NA, 
                          `Vegetative Protection` = NA, `Velocity / Depth Regime` = NA)
SCItemplate <- tibble(StationID = NA, Sta_Desc = NA, BenSampID = NA, `Collection Date` = NA, RepNum = NA, `Family Total Taxa` = NA, `Family EPT Taxa` = NA,      
                      `%Ephem` = NA, `%PT - Hydropsychidae` = NA, `%FamilyScraper` = NA, `%Chiro` = NA, `Family %2 Dominant` = NA, `Family HBI` = NA, `%Ephem Score` = NA,         
                      `%PT-H Score` = NA, `Fam Richness Score` = NA, `%Chironomidae Score` = NA, `Fam EPT Score` = NA, `Fam %Scraper Score` = NA, `Fam %2Dom Score` = NA, `Fam %MFBI Score` = NA,      
                      `SCI Score` = NA, SCI = NA, `SCI Threshold` = NA, `Sample Comments` = NA, `Collected By` = NA, `Field Team` = NA, `Entered By` = NA,           
                      Taxonomist = NA, `Entered Date` = NA, Gradient = NA, `Target Count` = NA, Season = NA, `Family %5 Dominant` = NA, `%ClngP-HS` = NA,            
                      `Richness Score` = NA, `Richness Final` = NA, `HBI Score` = NA, `HBI Final` = NA, `EPT Score` = NA, `EPT Final` = NA, EPHEM = NA,                
                      `PT-H` = NA, `Pct5DOM` = NA, `PctClng-HS` = NA, `%Scrap` = NA, `%Intoler` = NA, PctScrap = NA, PctIntol = NA,             
                      US_L3CODE = NA, US_L3NAME = NA, HUC_12 = NA, VAHU6 = NA, Basin = NA, Basin_Code = NA)



## Reorganize habitat data efficiently
habitatConsolidation <- function( userStationChoice, habSamps, habValues){
  habSampsUserSelection <- filter(habSamps, StationID %in% userStationChoice) 
  habValuesUserSelection <- filter(habValues, HabSampID %in% habSampsUserSelection$HabSampID)
  totalHabitat <- habSampsUserSelection %>%
    group_by(HabSampID) %>%
    # get total habitat values
    left_join(totalHabScore(habValuesUserSelection), by = 'HabSampID') %>%
    mutate(Season = factor(Season,levels=c("Spring","Outside Sample Window","Fall"))) %>%
    dplyr::select(StationID, HabSampID, everything()) %>%
    arrange(`Collection Date`) %>%
    ungroup() 
  
  habitatCrosstab <- bind_rows(habitatTemplate,
                               left_join(habValuesUserSelection, 
                                         dplyr::select(habSampsUserSelection, HabSampID, StationID, `Collection Date`),
                                         by = 'HabSampID') %>%
                                 group_by(StationID, HabSampID, `Collection Date`) %>%
                                 arrange(HabParameterDescription) %>% ungroup() %>%
                                 pivot_wider(id_cols = c('StationID','HabSampID','Collection Date'), names_from = HabParameterDescription, values_from = HabValue) %>%
                                 left_join(dplyr::select(totalHabitat, HabSampID, `HabSample Comment`, `Total Habitat Score`), by = 'HabSampID') %>%
                                 dplyr::select(StationID, HabSampID, `Collection Date`, `HabSample Comment`, `Total Habitat Score`, everything()) ) %>%
    drop_na(StationID) %>%
    arrange(StationID, `Collection Date`) 
  return(habitatCrosstab)
}

totalHabScore <- function(habValues){
  habValues %>%
    group_by(HabSampID) %>%
    summarise(`Total Habitat Score` = sum(HabValue, na.rm = T))
}


# Raw Bug data results for Report
rawBugData <- function(SCI){
  bioResultsTableTemplate <- tibble(StationID = NA, `Collection Date` = NA, `Replicate Number` = NA, Gradient = NA, SCI = NA, 
                                    `Spring SCI Score` = NA, `Fall SCI Score` = NA)
  bind_rows(bioResultsTableTemplate, 
            SCI %>%
              group_by(StationID, `Collection Date`, SCI, RepNum, Gradient) %>%
              dplyr::select(StationID, `Collection Date`, `Replicate Number` = RepNum, Gradient, SCI, Season, `SCI Score`) %>%
              mutate(`Collection Date` = as.Date(`Collection Date`),#, format = '%M-%D-%Y'),
                     Season = paste0(Season, ' SCI Score')) %>%
              pivot_wider(names_from = Season, values_from = `SCI Score`) ) %>%
    drop_na(StationID) %>%
    arrange(`Collection Date`, `Replicate Number`)
}

# SCI Statistics for report
SCIstatistics <- function(SCI1){
  suppressMessages(suppressWarnings(
    SCI1 %>%
      # IR window Average
      group_by(StationID, SCI) %>%
      summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits = 3),
                `n Samples` = n()) %>% 
      mutate(Window = paste0('IR ', assessmentCycle, ' (6 year) Average'))  %>%
      dplyr::select(SCI, Window, `SCI Average`, `n Samples`) %>%
      # Two Year Average
      bind_rows(SCI1 %>%
                  filter(year(`Collection Date`) %in% c(2019, 2020)) %>%
                  group_by(StationID, SCI) %>%
                  summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits=3),
                            `n Samples` = n()) %>% ungroup() %>%
                  mutate(Window = as.character('2019-2020 Average')) %>% 
                  dplyr::select(StationID, SCI, Window, everything())) %>%
      # Two Year Spring Average
      bind_rows(SCI1 %>%
                  filter(year(`Collection Date`) %in% c(2019, 2020) & Season == 'Spring') %>%
                  group_by(StationID, SCI) %>%
                  summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits=3),
                            `n Samples` = n()) %>% ungroup() %>%
                  mutate(Window = as.character('2019-2020 Spring Average')) %>% 
                  dplyr::select(StationID, SCI, Window, everything())) %>%
      # Two Year Fall Average
      bind_rows(SCI1 %>%
                  filter(year(`Collection Date`) %in% c(2019, 2020) & Season == 'Fall') %>%
                  group_by(StationID, SCI) %>%
                  summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits=3),
                            `n Samples` = n()) %>% ungroup() %>%
                  mutate(Window = as.character('2019-2020 Fall Average')) %>% 
                  dplyr::select(StationID, SCI, Window, everything())) %>%
      # Add seasonal averages
      bind_rows(SCI1 %>%
                  group_by(StationID, SCI, Season) %>%
                  mutate(Season = paste0('IR ', assessmentCycle, ' (6 year) ', Season,' Average')) %>%
                  summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits = 3),
                            `n Samples` = n()) %>%
                  rename('Window' = 'Season') %>% ungroup()) %>%
      mutate(Window = factor(Window, levels = c('2019-2020 Average', '2019-2020 Spring Average', '2019-2020 Fall Average', 'IR 2022 (6 year) Average', 
                                                'IR 2022 (6 year) Spring Average', 'IR 2022 (6 year) Fall Average'))) %>%
      arrange(StationID, SCI, Window)  %>% ungroup() ) )
}


# SCI plot for report
SCIresultsPlot <- function(SCI, assessmentMethod){
  if(unique(assessmentMethod) == 'VSCI'){
    if("Outside Sample Window" %in% SCI$Season){
      mutate(SCI, `Collection Date` = as.Date(`Collection Date`)) %>% 
        ggplot(aes(x = `Collection Date`, y = `SCI Score`, fill=Season)) +
        geom_col()+
        scale_fill_manual("Season", values = c("Fall" = "black", "Spring" = "dark grey", "Outside Sample Window" = "light grey"))+
        labs(x="Collection Year", y="VSCI Score") +
        scale_y_continuous(#name="VSCI", 
          breaks=seq(0, 100, 10),limits=c(0,100)) +
        scale_x_date(date_labels = '%Y') +
        geom_hline(yintercept=60, color="red", size=1)+
        theme(axis.text.x=element_text(angle=45,hjust=1))
    } else {
      mutate(SCI, `Collection Date` = as.Date(`Collection Date`)) %>% 
        ggplot(aes(x = `Collection Date`, y = `SCI Score`, fill=Season)) +
        geom_col()+
        scale_fill_manual("Season", values = c("Fall" = "black", "Spring" = "dark grey"))+
        labs(x="Collection Year", y="VSCI Score") +
        scale_y_continuous(#name="VSCI", 
          breaks=seq(0, 100, 10),limits=c(0,100)) +
        scale_x_date(date_labels = '%Y') +
        geom_hline(yintercept=60, color="red", size=1)+
        theme(axis.text.x=element_text(angle=45,hjust=1)) }
    
  } else {
    if("Outside Sample Window" %in% SCI$Season){
      mutate(SCI, `Collection Date` = as.Date(`Collection Date`)) %>% 
        ggplot(aes(x = `Collection Date`, y = `SCI Score`, fill=Season)) +
        geom_col()+
        scale_fill_manual("Season", values = c("Fall" = "black", "Spring" = "dark grey", "Outside Sample Window" = "light grey"))+
        labs(x="Collection Year", y="VCPMI Score") +
        scale_y_continuous(#name="VSCI", 
          breaks=seq(0, 100, 10),limits=c(0,100)) +
        scale_x_date(date_labels = '%Y') +
        geom_hline(yintercept=40, color="red", size=1)+
        theme(axis.text.x=element_text(angle=45,hjust=1))
    } else {
      mutate(SCI, `Collection Date` = as.Date(`Collection Date`)) %>% 
        ggplot(aes(x = `Collection Date`, y = `SCI Score`, fill=Season)) +
        geom_col()+
        scale_fill_manual("Season", values = c("Fall" = "black", "Spring" = "dark grey"))+
        labs(x="Collection Year", y="VCPMI Score") +
        scale_y_continuous(#name="VSCI", 
          breaks=seq(0, 100, 10),limits=c(0,100)) +
        scale_x_date(date_labels = '%Y') +
        geom_hline(yintercept=40, color="red", size=1)+
        theme(axis.text.x=element_text(angle=45,hjust=1)) }
  }
}

# SCI metrics table for report
SCImetricsTable <- function(SCI){
  SCI %>%
    mutate(`Collection Date` = as.Date(`Collection Date`)) %>% 
    group_by(StationID, `Collection Date`, SCI, RepNum) %>%
    dplyr::select(StationID, `Collection Date`, `Replicate Number` = RepNum, Season, SCI, `SCI Score`,`Family Total Taxa`:`Fam %MFBI Score`, 
                  `Family %5 Dominant`:PctIntol) %>%
    #clean up empty columns with a quick pivot longer (with drop na) and then back to wide
    pivot_longer(cols = `Family Total Taxa`:PctIntol, names_to = 'metric', values_to = 'metricVal', values_drop_na = TRUE) %>%
    pivot_wider(names_from = metric, values_from = metricVal) %>% ungroup() %>% 
    arrange(`Collection Date`, `Replicate Number`)
}


## Habitat plot for report
habitatPlot <- function(habitat){
  if(nrow(habitat) > 0){
    minDate <- as.Date(as.character("2015-01-01") , origin ="%Y-%m-%d")
    maxDate <- as.Date(as.character("2020-12-31"), origin ="%Y-%m-%d")# add min and max dates to make rectagle plotting easier, starting at 6 month buffer by can play with
    
    habitat %>%
      mutate(`Collection Date` = as.Date(`Collection Date`)) %>% 
      ggplot(aes(x = `Collection Date`, y = `Total Habitat Score`))+
      #geom_bar(stat="identity")
      annotate("rect", xmin=minDate, xmax=maxDate, ymin=150 ,  ymax=Inf, alpha=1, fill="#0072B2")+ 
      annotate("rect",xmin=minDate, xmax=maxDate, ymin=130, ymax=150, alpha=1, fill="#009E73" ) +
      annotate("rect",xmin=minDate, xmax=maxDate, ymin=100, ymax=130, alpha=1, fill="#F0E442") +
      annotate("rect",xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=100, alpha=1, fill="firebrick" ) +
      geom_bar(stat="identity", width = 75)+
      theme(axis.text=element_text(size=14, face="bold"),
            axis.title=element_text(size=14, face="bold"),
            legend.position = "none") +
      scale_y_continuous(name="Total Habitat Score", breaks=seq(0, 200, 25),limits=c(0,200)) +
      scale_x_date(date_breaks='1 year', date_labels =  "%Y")+
      theme(axis.text.x=element_text(angle=45,hjust=1))  }
}

# Habitat Table for final Report
habitatDTcoloredTable <- function(habitat){
  if(nrow(habitat) > 0){
    habitatTable <- habitat %>%
      mutate(`Collection Date` = as.Date(`Collection Date`)) %>% 
      dplyr::select(-HabSampID) %>%
      #clean up empty columns with a quick pivot longer (with drop na) and then back to wide
      pivot_longer(cols = `Bank Stability`:`Velocity / Depth Regime`, names_to = 'metric', values_to = 'metricVal', values_drop_na = TRUE) %>%
      pivot_wider(names_from = metric, values_from = metricVal) %>% ungroup() %>% 
      arrange(`Collection Date`)
    
    habBreaks<-seq(0,20, 1)
    habClrs<-c('firebrick', 'firebrick','firebrick','firebrick','firebrick','firebrick', "#F0E442","#F0E442","#F0E442","#F0E442","#F0E442", 
               "#009E73","#009E73","#009E73","#009E73","#009E73", "#0072B2","#0072B2","#0072B2","#0072B2","#0072B2")
    
    DT::datatable(habitatTable, escape=F, rownames = F, options=list(pageLength=nrow(habitatTable),dom= 'Bt', scrollX=TRUE)) %>% 
      formatStyle('Total Habitat Score', backgroundColor = "lightgray") %>%
      formatStyle(names(habitatTable)[5:length(habitatTable)],  backgroundColor = styleEqual(habBreaks, habClrs), alpha=0.1,
                  textAlign = 'center')  }
}
