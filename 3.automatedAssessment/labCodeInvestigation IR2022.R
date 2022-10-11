
library(tidyverse)
library(readxl)

# official conventionals for assessment
conventionalsRaw <-  read_excel('C:/HardDriveBackup/R/GitHub/IR2022/2.organizeMetadata/data/final2022data/CEDSWQM/CONVENTIONALS_20210316.xlsx') # takes forever to read in so just do it once and manipulate from there


conventionalsCodes <- conventionalsRaw %>% 
  mutate(across(contains('RMK'), as.character)) %>% 
  dplyr::select(contains('RMK')) %>% 
  pivot_longer(everything(), names_to = 'parameterRemark', values_to = 'remarkCode') %>% 
  filter(! is.na(remarkCode)) %>% 
  group_by(parameterRemark) %>% 
  summarise(unique(remarkCode))

labCodes <- read.csv('C:/HardDriveBackup/R/GitHub/IR2024/3.automatedAssessment/labCodes.csv')


codeJoin <- conventionalsCodes %>% 
  left_join(labCodes %>% dplyr::select(Com_Code, Com_Description), 
            by = c('unique(remarkCode)' = 'Com_Code')) %>% 
  arrange(`unique(remarkCode)`) 
