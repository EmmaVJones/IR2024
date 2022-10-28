###Required Functions for DFLOW#################################################

#getmode is a function that will find the mode of a vector. It takes in a vector
#of values and outputs the mode of that vector. In the case of ties, it returns
#both numbers
getmode <- function(v) {
  #Find the unique values of the vector
  uniqv <- unique(v)
  #Count the instances of each unique value
  tab <- tabulate(match(v, uniqv))
  #Return the value that was tabulated the most frequently
  tab <- uniqv[grep(max(tab),tab)]
  return(tab)
}

#check.fs will check to ensure the input exceedance probability is valid
# e.g. 0 <= p <=1
check.fs<-function (fs){
  if (any(fs < 0) || any(fs > 1)) {
    warning("invalid nonexceedance probability")
    return(FALSE)
  }
  return(TRUE)
}

#quape3 will return the quantile of the Peason III distribution from the list
#para. This is taken from the lmomco library, which is not installed on the R
#server. It is Valid for standard deviation greater than (NOT equal) zero
quape3<-function (f, para, paracheck = TRUE){
  if (!check.fs(f)) 
    return()
  SMALL <- sqrt(.Machine$double.eps)
  U <- para$para[1]
  A <- para$para[2]
  G <- para$para[3]
  x <- vector(mode = "numeric", length = length(f))
  if (abs(G) <= SMALL) {
    x <- U + A * qnorm(f)
  }
  else {
    ALPHA <- 4/G^2
    BETA <- abs(0.5 * A * G)
    if (G > 0) {
      x <- U - ALPHA * BETA + qgamma(f, ALPHA, scale = BETA)
    }
    else {
      x <- U + ALPHA * BETA - qgamma(1 - f, ALPHA, scale = BETA)
    }
  }
  x[f == 0 & G > 0] <- U - 2 * A/G
  x[f == 1 & G < 0] <- U - 2 * A/G
  names(x) <- NULL
  return(x)
}
# Return the quantile of the Pearson III distribution given skewness of g, and
# annual exceedance probability, aep. This is taken from the lmomco package.
Ky_Qp3 <- function(g, aep){ 
  #require(lmomco)
  #param <- vec2par(c(0,1,g), type='pe3')
  param <- list(type = "pe3", para = c(0,1,g), source = "vec2par")
  names(param$para) <- c("mu", "sigma", "gamma")
  quape3(aep, param)  
}

#Determine if a function is a leap year
leapYearAssessment<-function(inYear){
  condition1<-(inYear%%4)==0
  condition2<-(inYear%%100)==0
  condition3<-(inYear%%400)==0
  LY<-FALSE
  if(condition1){
    if(condition2){
      if(condition3){
        LY<-TRUE
      }
    }else{
      LY<-TRUE
    }
  }
  return(LY)
}

#lapply wrapper for leapYearAssessment. Used to determine which years in a
#vector of years are leap years
isLeapYear<-function(inYears){
  out<-unlist(lapply(inYears,leapYearAssessment))
  return(out)
}


###DFLOW Calculations###########################################################

#xQyComp is used within the xQy function to calculate the 7Q10 and other
#relevant low flows.It takes in a list of annual minimum flows (xQy_ann) and the
#averaging number (i.e. the x in xQy flows). It also takes the number of days in
#the analysis season (timecheck, which should b 365 for a full year). It
#determines if the leapYearinSeason and uses the return period y to determine
#the xQy flow.
xQyComp<-function(xQy_ann,ndays,timecheck,leapYearinSeason,febLDsameCY,dat,y,dat2){
  #First, determine which analysis years are leap years and correct timecheck as
  #needed
  timecheck_leapYear<-timecheck+1
  timecheck<-rep(timecheck,length(ndays))
  
  #If leapYear is in the anlysis season, ensure timecheck is corrected to adjust
  #for potential leap days
  if(leapYearinSeason){
    if(!is.na(febLDsameCY)){
      if(febLDsameCY){
        timecheck[isLeapYear(unique(dat$WY))]<-timecheck_leapYear
      }else{
        timecheck[(isLeapYear(unique(dat$WY)-1))]<-timecheck_leapYear
      }
    }
  }
  
  #Find the number of years in xQy_ann that represent complete flow years.
  #identify those years in WY_comp
  N<-length(ndays[ndays>=timecheck&!is.na(xQy_ann)])
  WY_comp<-unique(dat$WY)[ndays>=timecheck&!is.na(xQy_ann)]
  
  #Get rid of incomplete analysis years
  xQy_ann<-xQy_ann[which(ndays>=timecheck)]
  xQy_ann<-xQy_ann[!is.na(xQy_ann)]
  #Remove zeroes from analysis for logarithmic analysis
  xQy_ann<-xQy_ann[xQy_ann>0]
  
  #Initialize the vectors to store the output 7Q10 and other low flows
  xQy_out<-NA
  xQy_pctg<-NA
  
  #If there is sufficient data in xQy_ann, proceed with low flow identification
  if(length(xQy_ann)>9){
    #Take the log of annual low flows
    logFlows<-log(xQy_ann)
    n<-length(logFlows)
    #Step 1: Find statistics of log distribution of dates and flows
    u<-mean(logFlows)
    s<-sqrt(var(logFlows))
    
    #Find skewness using the appropriate method. In DFLOW example, matches type 1 but SW toolbox uses type 2
    g<-skewness(logFlows,type = 2)#Based on SW Toolbox output
    #Step 2: Adjust probability based on old manual and as suggested in USGS pub:
    if(n==0){
      p<-0
    }else{
      p<-((N/y)/n)-((N-n)/n)
    }
    #Step 3: Calculate K skewness factor either based on tabulated values (K_t below) or estimated as K below:
    #With best fit equation
    if(p>0){
      #z=4.91*((p)^0.14-(1-p)^0.14)
      #K<-(2/g)*((1+(g*z/6)-g^2/36)^3-1) This equation is based on a small skew range (-1 to 1) and isn't appropriate for bigger skew
      
      if(s>0){
        K<-Ky_Qp3(g,p)#This function from the lmomco package matches the Leon Harper tables fairly well (within 0.005 at most)
        xQy_out<-exp(u+K*s)
      }else{
        xQy_out<-u
      }
      
      #What daily percentile does the 7Q10 flow represent?
      xQy_pctg<-length(dat2$Flow[dat2$Flow<=xQy_out&dat2$WY%in%WY_comp])/length(dat2$Flow[dat2$WY%in%WY_comp])
    }else{
      xQy_out<-0
      xQy_pctg<-0
    }
  }
  #write.table(xQy_ann,paste0(xQy_ann[1],"-",xQy_ann[length(xQy_ann)],".txt"))
  return(list(xQy=xQy_out,pctg=xQy_pctg))
}

#Function to calculate low flows from USGS gage data. Takes in the gage data
#dat, the dates to limit the data set (if any) DS and DE (which should be in
#standard R format of yyyy-mm-dd), the start and end of the analysis season WYS
#and WYE, the x and y for a custom xQy low flow (defaults to 7Q10, which is
#already included)
xQy_EVJ<-function(gageID,#USGS Gage ID
              DS="",#Date to limit the lower end of usgs gage data download in yyyy-mm-dd
              DE="",#Date to limit the upper end of USGS gage data download in yyyy-mm-dd
              WYS="04-01",#The start of the analysis season in mm-dd. Defaults to April 1.
              WYE="03-31",#The end of the analysis season in mm-dd. Defaults to March 31.
              x=7,#If you want to include a different xQY then the defaults, enter x here
              y=10,#If you want to include a different xQY then the defaults, enter y here
              onlyUseAcceptedData = T # EVJ edit: option to allow provisional data into analysis, important for assessment timelines
              ){
  #Use Zoo package for rollapply for easy calculation of rolling means
  require(zoo)
  require(dataRetrieval)
  require(e1071)
  
  dat<-readNWISdv(gageID,"00060",DS,DE)
  if(nrow(dat)==0){
    return(paste0("No gage data found for ",gageID))
  }
  
  #Check for provisional flow data
  indx<-grep("P",dat$X_00060_00003_cd)
  # Filter out Provisional flow data if argument to only use accepted data is selected by user
  if(length(indx) > 0 & onlyUseAcceptedData == T){
    #Filter for provisional data
    dat<-dat[-indx,]
  }
  
  #Give more standard names to gage data
  names(dat)<-c("Agency","Gage ID","Date","Flow","Status")
            
  
  #Treat negative flows as missing flows, per SW toolbox
  dat$Flow[dat$Flow<0]<-NA
  
  dat$Date<-as.Date(dat$Date)
  
  #Initialize vectors to define the analysis year (here incorrectly symbolized
  #as WY since this tool originally used Water Year)
  dat<-dat[order(dat$Date),]
  dat$WY<-NA
  
  #Use regular expression to identify months in next Date that need to be
  #classified as the previous Dates water Date.
  #Grab the months by eliminating all characters after the last hyphen since
  #this function expects dates in standard R format mm-dd
  WYM<-as.numeric(gsub("-.*","",WYS))
  #Find all months leading up to the beginning of the anlysis season WYS
  if(WYM>1){
    WYP<-seq(1,(WYM)-1)
  }else{
    WYP<-0
  }
  
  #Add leading zeros to month number January - September
  WYP[nchar(WYP)==1]<-paste0("0",WYP[nchar(WYP)==1])
  WYP<-paste0(WYP,"$",collapse="|")
  WY<-gsub("-*?.*$","",dat$Date)
  
  #Initialize water year by making it the calendar year+1
  dat$WY<-as.numeric(gsub("-.*","",WY))+1
  
  #Search for all months from January to the start of the water year. These
  #months need to be assigned previous water year number (current calendar year)
  dat$WY[grep(WYP,WY)]<-dat$WY[grep(WYP,WY)]-1
  
  #Exception case occurs when water year is calendar year, in which case water
  #year ends in calendar year such that calendar year = water year. Exception
  #also occurs when looking at a partial season, all of which is contained in
  #the same year e.g. March - September
  dateDiff<-as.numeric(as.Date(paste0("1994-",WYE))-as.Date(paste0("1994-",WYS)))
  if(WYS=="01-01"||dateDiff>0){
    dat$WY<-dat$WY-1
  }
  
  #Water Date correction for day in WYS - matters when water years begin on a
  #day other than the first of the month
  DWYS<-as.numeric(gsub(".*-","",WYS))
  DWYE<-as.numeric(gsub(".*-","",WYE))
  if(DWYS>1){
    #Find all days in the water year start month up until the start of the next
    #water year
    DYP<-seq(1,DWYS)
    DYP<-DYP[-length(DYP)]
    DYP[nchar(DYP)==1]<-paste0("0",DYP[nchar(DYP)==1])
    DYP<-paste0(".*-0?",WYM,"-",DYP,".*",collapse="|")
    #Adjust the days leading up to the water year to have water year = current
    #calendar year
    dat$WY[grep(DYP,dat$Date)]<-dat$WY[grep(DYP,dat$Date)]-1
  }
  
  #If user did not enter limit dates, set to one above and below limits on gage
  #dates
  if(DS == ""){
    DS <- as.numeric(gsub("-.*","",dat$Date[1])) - 1
  }else{
    DS <- as.numeric(gsub("-.*","",DS))
  }
  if(DE == ""){
    DE <- as.numeric(gsub("-.*","",dat$Date[length(dat$Date)])) + 1
  }else{
    DE <- as.numeric(gsub("-.*","",DE))
  }
  
  
  #Only use years in user-input range
  #Years<-as.numeric(gsub("-.*","",dat$Date))
  dat<-dat[dat$WY%in%seq(DS,(DE)),]
  
  #Filter the data to only include that which is in between DS and DE
  if(dat$WY[1] < DS || gsub("^.*[-]+?","",dat$Date[1]) != WYS){
    WYS_Vector<-gsub("^.*[-]+?","",dat$Date)
    WYS_Vector<-grep(WYS,WYS_Vector)
    #If the analysis start date WYE is not in the dataset, WYE_Vector will be
    #empty implying less than one year of data is available after filtering for
    #DS and DE above. 
    if(length(WYS_Vector) == 0){
      return(paste0("Insufficient gage data found for ",gageID," within selected time period."))
    }
    dat<-dat[WYS_Vector[1]:length(dat$Date),]
  }
  
  if(dat$WY[length(dat$WY)] > DE || 
     gsub("^.*[-]+?","",dat$Date[length(dat$WY)]) != WYE){
    WYE_Vector<-gsub("^.*[-]+?","",dat$Date)
    WYE_Vector<-grep(WYE,WYE_Vector)
    #If the analysis end date WYE is not in the dataset, WYE_Vector will be
    #empty implying less than one year of data is available after filtering for
    #DS and DE above
    if(length(WYE_Vector) == 0){
      return(paste0("Insufficient gage data found for ",gageID," within selected time period."))
    }
    dat<-dat[1:WYE_Vector[length(WYE_Vector)],]
    
  }
  
  #Extract the seasonal data (if any)
  #Find the difference in start and end date assuming same Date. If difference
  #is negative, then season extends into next year.
  dateDiff <- as.numeric(as.Date(paste0("1994-",WYE))-as.Date(paste0("1994-",WYS)))
  MS <- as.numeric(gsub("-.*","",WYS))
  ME <- as.numeric(gsub("-.*","",WYE))
  #Difference in end and start months, a negative number implies season extends
  #into next calendar year
  monDiff <- ME - MS
  
  #Extract all relevant months and check if leap year is included in season of
  #analysis:
  leapYearinSeason<-FALSE
  #If an analysis year contains leap day, is it the same as the MS calendar year
  #or is it ahead one year?
  febLDsameCY<-NA 
  if(dateDiff<0){
    if(monDiff==0){
      #If there is no difference in start and end month, but the start date is
      #greater than the end date, then use whole calendar year as water season
      #ends earlier in start month and thus extends through all 12 months
      mons<-seq(1:12)
      timecheck<-as.numeric(as.Date(paste0("1995-",WYE))-as.Date(paste0("1994-",WYS)))+1
      #Because 02-29 is an invalid start date and there is a negative date
      #difference, we know the analysis year must extend into the next calendar
      #year. monDiff is equal to zero, so begin and start days are in same
      #month. So, All possible analyses in this category must include 02-29 in
      #leap years because any start and end date in February include the leap
      #year if the analysis year runs into the next calendar year:
      leapYearinSeason<-TRUE
      #If the month is January or February, the analysis year the leap day
      #occurs will never be the calendar year. The reverse is true for all other
      #months. e.g. if we look at the USGS water year for 2000 (1999-10-01 to
      #2000-09-30), the leap year occurs in the calendar 2000. So, the analysis
      #year = leap year
      if(ME<=2){
        febLDsameCY<-FALSE
      }else{
        febLDsameCY<-TRUE
      }
      
    }else if(dateDiff<0){
      timecheck<-as.numeric(as.Date(paste0("1995-",WYE))-as.Date(paste0("1994-",WYS)))+1
      mons<-c(seq(1,ME),seq(MS,12))
      #Because 02-29 is an invalid start date and there is a negative date
      #difference, we know the analysis year must extend into the next calendar
      #year. monDiff is not equal to zero, so begin and start months are
      #different. Leap years will only be included if February 28th is in the
      #analysis season. e.g. a season from 07-15 to 02-15 does not include the
      #leap year, but a season from 07-15 to 03-15 does include the leap year:
      if(2%in%mons){
        if(ME!=2){
          #If February is included in the analysis season, but isn't the final
          #month, then the leap year is involved in the analysis:
          leapYearinSeason<-TRUE
          # }else if(DWYE>=28&&MS==3&&DWYS==1){
          # leapYearinSeason<-TRUE
        }else{
          #If the analysis season ends on February, leap years should NOT be
          #included. This program will automatically clip leap days out if WYS =
          #03-01 and WYE = 02-28 There is no scenario in which the analysis year
          #ends in February and the leap year is included (assuming monDiff is
          #not equal zero) Leap year not included in analysis:
          leapYearinSeason<-FALSE
        }
        
        #If the month is January or February, the analysis year the leap day
        #occurs will never be the calendar year. The reverse is true for all
        #other months. e.g. if we look at the USGS water year for 2000
        #(1999-10-01 to 2000-09-30), the leap year occurs in the calendar 2000.
        #So, the analysis year = leap year
        if(ME<=2){
          febLDsameCY<-FALSE
        }else{
          febLDsameCY<-TRUE
        }
      }else{
        #February is not included in the analysis season, so analysis leap years
        #are of same length as all other analysis years
        leapYearinSeason<-FALSE
      }
    }
  }else{
    #Use start to end months as the water season is in current calendar year only
    timecheck<-as.numeric(as.Date(paste0("1994-",WYE))-as.Date(paste0("1994-",WYS)))+1
    mons<-seq(MS,ME)
    
    #Because 02-29 is an invalid start date and there is a positive date
    #difference, we know the analysis year starts and ends in the same calendar
    #year. monDiff may or may not be equal to zero. Leap years will only be
    #included if February 28th is in the analysis season,so we must check for
    #analysis season month and day. e.g. a season from 01-15 to 02-15 does not
    #include the leap year, but a season from 02-25 to 10-15 does include the
    #leap year:
    if(2%in%mons){
      if(ME!=2){
        #If February is included in the analysis season, but isn't the final
        #month, then the leap year is involved in the analysis:
        leapYearinSeason<-TRUE
      }else{
        #Leap year not included in analysis as end date occurs before leap year.
        #February 29th is an invalid input date, so the leap year will NOT be
        #included in any analysis if end month is February.
        leapYearinSeason<-FALSE
      }
      
      #If the month is January or February, the analysis year the leap day
      #occurs will never be the calendar year. The reverse is true for all other
      #months. e.g. if we look at the USGS water year for 2000 (1999-10-01 to
      #2000-09-30), the leap year occurs in the calendar 2000. So, the analysis
      #year = leap year In this case, the analysis year is always equal to the
      #calendar year
      febLDsameCY<-TRUE
    }else{
      #February is not included in the analysis season, so analysis leap years
      #are of same length as all other analysis years
      leapYearinSeason<-FALSE
    }
  }
  mons[nchar(mons)==1]<-paste0("0",mons[nchar(mons)==1])
  mons<-paste0(mons,collapse="|")
  
  datDates<-gsub("^.*[-]+?","",dat$Date)
  datMons<-gsub("-.*","",datDates)
  #Extract relevant months
  subst<-dat[grep(mons,datMons),]
  #Adjust the subset data by removing all data points in the starting month
  #prior to the start data and the dates after the end date in the end month
  if(abs(dateDiff)>1||(WYE=="02-28"&&WYS=="03-01")){
    if(monDiff==0){
      #if the start and end month are the same and the date difference is
      #greater than a day, keep only dates in between. Otherwise, remove all
      #dates in between since user is interested in everything else
      if(dateDiff>0){
        dayOut<-seq(DWYS,DWYE)
        dayOut[nchar(dayOut)==1]<-paste0("0",dayOut[nchar(dayOut)==1])
        dayOut<-paste0(".*-0?",WYM,"-",dayOut,".*",collapse="|")
        subst<-subst[(grep(dayOut,subst$Date)),]
      }else{
        #Need to adjust sequence so that DWYE and DWYS are included in final
        #data set
        dayOut<-seq((DWYE+1),(DWYS-1))
        dayOut[nchar(dayOut)==1]<-paste0("0",dayOut[nchar(dayOut)==1])
        dayOut<-paste0(".*-0?",WYM,"-",dayOut,".*",collapse="|")
        subst<-subst[-(grep(dayOut,subst$Date)),]
      }
    }else{
      #If the start and end months are different, remove all dates in the end
      #month after the end date and all dates in the start month before the
      #start date
      if(DWYS>1){
        subst<-subst[-(grep(DYP,subst$Date)),]
      }
      # if(DWYE>1){
      DYP<-seq(1,31)[!(seq(1,31)%in%seq(1,DWYE))]
      DYP[nchar(DYP)==1]<-paste0("0",DYP[nchar(DYP)==1])
      if(length(DYP)>0){
        DYP<-paste0(".*-0?",ME,"-",DYP,".*",collapse="|")
        dayOutE<-grep(DYP,subst$Date)
        if(length(grep(DYP,subst$Date))>0){
          subst<-subst[-(grep(DYP,subst$Date)),]
        }
      }
      # }
    }
  }
  
  #Create a copy of the full gage data and reset dat as the subset above
  dat2<-dat
  dat<-subst
  
  #Compute rolling means of data and find annual (water Date) minimums and the
  #date in which they occurred (for tracking mode months of low flow)
  ndays<-numeric()
  nxQy_ann<-numeric()
  n1Q10_ann<-numeric()
  n7Q10_ann<-numeric()
  n30Q10_ann<-numeric()
  #Because a minimum flow may occur multiple times in one year, may want to save
  #minimum flows days in a growing data frame with water year and minimum flow
  #included in data frame for ease of outputs access
  nxQy_annDate<-data.frame(Date=character(),WY=numeric(),minFlow=numeric())
  n1Q10_annDate<-data.frame(Date=character(),WY=numeric(),minFlow=numeric())
  n7Q10_annDate<-data.frame(Date=character(),WY=numeric(),minFlow=numeric())
  n30Q10_annDate<-data.frame(Date=character(),WY=numeric(),minFlow=numeric())
  
  
  #Calculate the rolling means using zoo package rollapply
  rxAvg<-rollapply(dat$Flow,x,mean)
  rxAvg<-round(rxAvg,10)
  
  r7Avg<-rollapply(dat$Flow,7,mean)
  r7Avg<-round(r7Avg,10)
  
  r30Avg<-rollapply(dat$Flow,30,mean)
  r30Avg<-round(r30Avg,10)
  
  #Ensure minimums are independent from one another, removing last x-1 days from
  #the analysis as they compute rolling means that extend into the next water
  #year
  dat$nxAvg<-c(rxAvg,rep(NA,x-1))
  dat$n7Avg<-c(r7Avg,rep(NA,7-1))
  dat$n30Avg<-c(r30Avg,rep(NA,30-1))
  
  #For each water year, find the minimum x day flow for that year
  for(i in 1:length(unique(dat$WY))){
    #Find the minimum of the rolling x averages by first subsetting to the
    #current year i
    subst<-dat[dat$WY==unique(dat$WY)[i],]
    
    #Record number of days in water year for use in xQy filtering
    ndays[i]<-length(subst$Date[subst$Flow>=0&!is.na(subst$Flow)])
    
    #If missing flows are present, no minimum can be found for that year
    if(any(is.na(subst$Flow))){
      nxQy_ann[i]<-NA
      n1Q10_ann[i]<-NA
      n7Q10_ann[i]<-NA
      n30Q10_ann[i]<-NA
      
      nxQy_annDatei<-data.frame(Date=NA,WY=NA,minFlow=NA)
      n1Q10_annDatei<-data.frame(Date=NA,WY=NA,minFlow=NA)
      n7Q10_annDatei<-data.frame(Date=NA,WY=NA,minFlow=NA)
      n30Q10_annDatei<-data.frame(Date=NA,WY=NA,minFlow=NA)
    }else{
      #There must be enough data in the season to compute a minimum flow for the
      #custom x-day flow
      if((ndays[i]-(x-2))>1){
        #If sufficient data exists, find the minimum flow for the year and when
        #it occurs after removing x days from the end of the year to ensure
        #statistical independence from the following year
        substx <- subst[-((ndays[i]-(x-2)):ndays[i]),]
        nxQy_ann[i] <- min(substx$nxAvg,na.rm = T)
        nxQy_annDatei <- data.frame(Date=substx$Date[substx$nxAvg==nxQy_ann[i]],WY=unique(dat$WY)[i],minFlow=nxQy_ann[i])
      }else{
        #If insufficient data exists, set to NA
        nxQy_ann[i]<-NA
        nxQy_annDatei<-data.frame(Date=NA,WY=NA,minFlow=NA)
      }
      #Find the annual minimum 1 day flow
      n1Q10_ann[i]<-min(subst$Flow,na.rm = T)
      n1Q10_annDatei<-data.frame(Date=subst$Date[subst$Flow==n1Q10_ann[i]],WY=unique(dat$WY)[i],minFlow=n1Q10_ann[i])
      
      #Repeat the procedure above to find the annual minimum 7 day flows
      if((ndays[i]-(7-2))>1){
        subst7<-subst[-((ndays[i]-(7-2)):ndays[i]),]
        n7Q10_ann[i]<-min(subst7$n7Avg,na.rm = T)
        n7Q10_annDatei<-data.frame(Date=subst7$Date[subst7$n7Avg==n7Q10_ann[i]],WY=unique(dat$WY)[i],minFlow=n7Q10_ann[i])
      }else{
        n7Q10_ann[i]<-NA
        n7Q10_annDatei<-data.frame(Date=NA,WY=NA,minFlow=NA)
      }
      
      #Repeate the procedure above to find the annual minimum 30 day flows
      if((ndays[i]-(30-2))>1){
        subst30<-subst[-((ndays[i]-(30-2)):ndays[i]),]
        n30Q10_ann[i]<-min(subst30$n30Avg,na.rm = T)
        n30Q10_annDatei<-data.frame(Date=subst30$Date[subst30$n30Avg==n30Q10_ann[i]],WY=unique(dat$WY)[i],minFlow=n30Q10_ann[i])
      }else{
        n30Q10_ann[i]<-NA
        n30Q10_annDatei<-data.frame(Date=NA,WY=NA,minFlow=NA)
      }
    }
    #Output the minimum xQy, 1Q10, 7Q10, and 30Q10 flow for this iteration of
    #the loop
    nxQy_annDate<-rbind(nxQy_annDate,nxQy_annDatei)
    n1Q10_annDate<-rbind(n1Q10_annDate,n1Q10_annDatei)
    n7Q10_annDate<-rbind(n7Q10_annDate,n7Q10_annDatei)
    n30Q10_annDate<-rbind(n30Q10_annDate,n30Q10_annDatei)
  }
  
  #Compute the low flows using the data from the loop. Dat is the filtered and
  #formatted gage data and dat2 is the unmodified data used to evaluate the flow
  #percentile
  out_xQy<-xQyComp(nxQy_ann,ndays,timecheck,leapYearinSeason,febLDsameCY,dat,y,dat2)
  out_1Q10<-xQyComp(n1Q10_ann,ndays,timecheck,leapYearinSeason,febLDsameCY,dat,10,dat2)
  out_7Q10<-xQyComp(n7Q10_ann,ndays,timecheck,leapYearinSeason,febLDsameCY,dat,10,dat2)
  out_30Q10<-xQyComp(n30Q10_ann,ndays,timecheck,leapYearinSeason,febLDsameCY,dat,10,dat2)
  out_30Q5<-xQyComp(n30Q10_ann,ndays,timecheck,leapYearinSeason,febLDsameCY,dat,5,dat2)
  
  #By using dat 2, are we including data at the beginning and end of record
  #period that is not necessarily in the climate year? Should we clip to start
  #of first analysis year?
  
  #Human Health Standards: Harmonic Mean
  WY_comp<-unique(dat2$WY)[ndays>=timecheck]
  subst<-dat2[!is.na(dat2$Flow)&dat2$Flow>0&(dat2$WY%in%WY_comp),]
  HM<-sum(1/subst$Flow)
  NDays<-length(dat2$Flow[!is.na(dat2$Flow)&(dat2$WY%in%WY_comp)])
  NZeros<-length(dat2$Flow[!is.na(dat2$Flow)&dat2$Flow==0&(dat2$WY%in%WY_comp)])
  HM<-((NDays-NZeros)/HM)*((NDays-NZeros)/NDays)
  HM_pctg<-length(dat2$Flow[dat2$Flow<=HM&dat2$WY%in%WY_comp])/length(dat2$Flow[dat2$WY%in%WY_comp])
  
  #Are missing values included here? why is adjusted sw toolbox harmonic mean so small?
  # subst<-dat2[!is.na(dat2$Flow)&dat2$Flow>0,]
  # HM<-sum(1/subst$Flow)
  # NDays<-length(dat2$Flow[!is.na(dat2$Flow)])
  # NZeros<-length(dat2$Flow[!is.na(dat2$Flow)&dat2$Flow==0])
  # HM<-((NDays-NZeros)/HM)*((NDays-NZeros)/NDays)
  # #Harmonic mean percentile
  # HM_pctg<-length(dat2$Flow[dat2$Flow<=HM&dat2$WY%in%WY_comp])/length(dat2$Flow[dat2$WY%in%WY_comp])
  # browser()
  
  nxQy_annDate$Date<-as.Date(nxQy_annDate$Date)
  n1Q10_annDate$Date<-as.Date(n1Q10_annDate$Date)
  n7Q10_annDate$Date<-as.Date(n7Q10_annDate$Date)
  n30Q10_annDate$Date<-as.Date(n30Q10_annDate$Date)
  
  #Solve for mode minimum month for each input flow type:
  nxQy_ModeMin<-getmode(as.numeric(gsub(".*-(.*)-.*","\\1",nxQy_annDate$Date)))
  n1Q10_ModeMin<-getmode(as.numeric(gsub(".*-(.*)-.*","\\1",n1Q10_annDate$Date)))
  n7Q10_ModeMin<-getmode(as.numeric(gsub(".*-(.*)-.*","\\1",n7Q10_annDate$Date)))
  n30Q10_ModeMin<-getmode(as.numeric(gsub(".*-(.*)-.*","\\1",n30Q10_annDate$Date)))
  n30Q5_ModeMin<-getmode(as.numeric(gsub(".*-(.*)-.*","\\1",n30Q10_annDate$Date)))
  #Function outputs: All xQy flows (see list "Flows" in returned list), all xQy
  #percentiles (see list "Pctg" in returned list), the harmonic mean value and
  #percentile (included in "Flows" and "Pctg" lists), the formatted USGS data
  #frame (for reupload, see "outdat") the complete/unfiltered list of annual
  #minimums for ALL xQy statistics (see "nxQy_ann" - "n30Q5_ann"). the days in
  #each water year for the 30Q10 and the timecheck varaible for the 30Q10 are
  #included for use in the documented markdown file a data frame for each flow
  #that includes dates when minimum flows occurred, the minimum flow, and the
  #associated water year
  return(list(Flows=list(gageNo = gageID, xQy=out_xQy$xQy,n1Q10=out_1Q10$xQy,n7Q10=out_7Q10$xQy,n30Q10=out_30Q10$xQy,n30Q5=out_30Q5$xQy,HM=HM),
              Pctg=list(xQy=out_xQy$pctg,n1Q10=out_1Q10$pctg,n7Q10=out_7Q10$pctg,n30Q10=out_30Q10$pctg,n30Q5=out_30Q5$pctg,HM=HM_pctg),
              outdat=dat,nxQy_ann=nxQy_ann,n1Q10_ann=n1Q10_ann,n7Q10_ann=n7Q10_ann,n30Q10_ann=n30Q10_ann,n30Q5_ann=n30Q10_ann,
              nxQy_annDate=nxQy_annDate,n1Q10_annDate=n1Q10_annDate,n7Q10_annDate=n7Q10_annDate,n30Q10_annDate=n30Q10_annDate,n30Q5_annDate=n30Q10_annDate,
              nxQy_ModeMin=nxQy_ModeMin,n1Q10_ModeMin=n1Q10_ModeMin,n7Q10_ModeMin=n7Q10_ModeMin,n30Q10_ModeMin=n30Q10_ModeMin,n30Q5_ModeMin=n30Q5_ModeMin,
              ndays=ndays,timecheck=timecheck))
}
