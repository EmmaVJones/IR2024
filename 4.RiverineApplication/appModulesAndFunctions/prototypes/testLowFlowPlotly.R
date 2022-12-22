oneStation <- filter(AUData,FDT_STA_ID %in% '2-DCK003.94') %>%
    filter(!is.na(FDT_TEMP_CELCIUS))
lowFlowData <- filter(oneStation, !is.na(`7Q10 Flag`))
lowFlowData$SampleDate <- as.POSIXct(lowFlowData$FDT_DATE_TIME, format="%m/%d/%y")

dat <- mutate(oneStation, top = `Max Temperature (C)`)
dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y")
plot_ly(data=dat)%>%
  add_lines(x=~SampleDate,y=~top, mode='line',line = list(color = 'black'),
            hoverinfo = "text", text="Temperature Standard", name="Temperature Standard") %>%
  
  add_markers(., data = lowFlowData,
              x= ~SampleDate, y= ~FDT_TEMP_CELCIUS, mode = 'scatter', name="Temperature (Celsius)",
              marker = list(color= 'red'), size = 120,
              hoverinfo="text",text=~paste(sep="<br>",
                                           paste("Date: ",SampleDate),
                                           paste("Depth: ",FDT_DEPTH, "m"),
                                           paste("Temperature: ",FDT_TEMP_CELCIUS,"C"),
                                           paste("7Q10 Flag Gage: ",`7Q10 Flag Gage`))) %>%
  
  add_markers(., data = dat, x= ~SampleDate, y= ~FDT_TEMP_CELCIUS,mode = 'scatter', name="Temperature (Celsius)",marker = list(color= '#535559'),
              hoverinfo="text",text=~paste(sep="<br>",
                                           paste("Date: ",SampleDate),
                                           paste("Depth: ",FDT_DEPTH, "m"),
                                           paste("Temperature: ",FDT_TEMP_CELCIUS,"C"))) %>%
  
  # add_markers(x= ~SampleDate, y= ~FDT_TEMP_CELCIUS,mode = 'scatter', name="Temperature (Celsius)", 
  #             marker = list(list(color= ~`7Q10 Flag`), colors = c('#535559', 'red')),
  #             hoverinfo="text",text=~paste(sep="<br>",
  #                                          paste("Date: ",SampleDate),
  #                                          paste("Depth: ",FDT_DEPTH, "m"),
  #                                          paste("Temperature: ",FDT_TEMP_CELCIUS,"C"))) %>%
  # 
  
  #{if(nrow(lowFlowData) > 0)
    
  #else . } %>%
  
  layout(showlegend=FALSE,
         yaxis=list(title="Temperature (Celsius)"),
         xaxis=list(title="Sample Date",tickfont = list(size = 10)))
