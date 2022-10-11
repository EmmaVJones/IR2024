quickStats <- function(parameterDataset, parameter){
  if(nrow(parameterDataset) > 0 & any(!is.na(parameterDataset$limit)) & all(is.na(parameterDataset$flag))){
    # Case when we have all the ingredients we need: data to assess, WQS to assess against, and no flags in the data
    results <- data.frame(EXC = nrow(filter(parameterDataset, exceeds == TRUE)),
                          SAMP = nrow(parameterDataset)) %>%
      # Implement Round to Even on Exceedance Frequency
      mutate(exceedanceRate = as.numeric(round::roundAll((EXC/SAMP)*100,digits=0, "r0.C"))) # round to nearest whole number per Memo to Standardize Rounding for Assessment Guidance
    
    if(results$EXC >= 1){outcome <- 'Review'} # for Mary
    if(results$EXC >= 1 & results$exceedanceRate < 10.5){outcome <- 'Review'}
    if(results$exceedanceRate > 10.5 & results$EXC >= 2 & results$SAMP > 10){outcome <- '10.5% Exceedance'}
    if(results$EXC < 1 &results$exceedanceRate < 10.5 & results$SAMP > 10){outcome <- 'S'}
    if(results$EXC >= 1 & results$SAMP <= 10){outcome <- 'Review'}
    if(results$EXC < 1 & results$SAMP <= 10 & results$SAMP > 1){outcome <- 'S'} # & results$SAMP >1 new 12/21/2020 can't say supporting on 1 sample
    if(results$EXC < 1 & results$SAMP <= 10 & results$SAMP == 1){outcome <- 'Review'} # & results$SAMP >1 new 12/21/2020 can't say supporting on 1 sample
    
    
    results <- mutate(results, STAT = outcome)
    names(results) <- c(paste(parameter,names(results)[1], sep = '_'),
                        paste(parameter,names(results)[2], sep = '_'),
                        paste(parameter,names(results)[3], sep = '_'),
                        paste(parameter,names(results)[4], sep = '_'))
    #rename based on parameter entered
    return(results)
  } else {
    # Case when no data present to analyze
    if(nrow(parameterDataset) == 0){
      z <- data.frame(EXC = NA, SAMP= nrow(parameterDataset), exceedanceRate= NA, STAT= NA)
      names(z) <- paste(parameter,names(z), sep='_')
      return(z)
    } else {
      if(any( !is.na(parameterDataset$flag))){
        # Case when there are data flags present in the data
        parameterDatasetClean <- filter(parameterDataset, is.na(flag))
        results <- data.frame(EXC = nrow(filter(parameterDatasetClean, exceeds == TRUE)),
                              SAMP = nrow(parameterDatasetClean)) %>%
          # Implement Round to Even on Exceedance Frequency
          mutate(exceedanceRate = as.numeric(round::roundAll((EXC/SAMP)*100,digits=0, "r0.C")), # round to nearest whole number per Memo to Standardize Rounding for Assessment Guidance
                 STAT = 'Review- Flagged Data Removed From Results')
        return(results)
      } else {
        # Case when no data flags and WQS data present to compare data against
        z <- data.frame(EXC = NA, SAMP= nrow(parameterDataset), exceedanceRate= NA, STAT= paste(parameter, 'WQS info missing from analysis'))
        names(z) <- paste(parameter,names(z), sep='_')
        return(z)
      }
    }
  }
}



#Max Temperature Exceedance Function
tempExceedances <- function(x){
  dplyr::select(x, FDT_DATE_TIME, FDT_DEPTH, contains('TEMP_CELCIUS'), `Max Temperature (C)`) %>% # Just get relevant columns 
    filter(! (LEVEL_FDT_TEMP_CELCIUS %in% c('Level II', 'Level I'))) %>% # get lower levels out
    filter(! is.na(FDT_TEMP_CELCIUS))%>% # get rid of NA's
    # rename columns to make exceedance analyses easier to apply
    rename(parameter = !! names(.[3]),
           limit = !! names(.[6]),
           flag = !! names(.[4])) %>% 
    # Apply Round to Even Rule before testing for exceedances
    mutate(parameterRound = signif(parameter, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
           exceeds = ifelse(parameterRound > limit, T, F)) # Identify where above max Temperature
}
# tempExceedances(x) %>%
#  quickStats('TEMP')


