# packages
library(tidyverse)
library(sf)
library(readxl)
library(pins)
library(config)
library(EnvStats)
library(lubridate)
library(round) # for correct round to even logic

# Bring in Assessment functions from app
source('automatedAssessmentFunctions.R')

# get configuration settings
conn <- config::get("connectionSettings")

# use API key to register board
board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))


WCmetals <- pin_get("WCmetalsIR2024",  board = "rsconnect") # in case you want to see what comes in from Roger
WCmetalsForAnalysis <- pin_get("ejones/WCmetalsForAnalysisIR2024",  board = "rsconnect") # cleaned up metals data to fit existing functions




stationIssue <- '2-JKS023.61'


stationData <- readRDS('stationData.RDS')




# Water Column Metals analysis using Aquatic Life Toxics rolling window method
WCmetalsStationAnalysis <- filter(WCmetalsForAnalysis, Station_Id %in% stationIssue) %>% 
  metalsAnalysis( stationData, WER = 1) %>% # fixed this version of metals analysis
  rename(FDT_STA_ID = Station_Id) %>% 
  mutate(`Criteria Type` = Criteria) 
WCmetalsExceedanceAnalysis <- annualRollingExceedanceAnalysis(WCmetalsStationAnalysis, yearsToRoll = 3, aquaticLifeUse = TRUE)
WCmetalsExceedanceSummary <- annualRollingExceedanceSummary(WCmetalsExceedanceAnalysis)  
