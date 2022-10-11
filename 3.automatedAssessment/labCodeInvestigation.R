conventionalsCodes <- conventionals %>% 
  dplyr::select(contains('RMK')) %>% 
  pivot_longer(everything(), names_to = 'parameterRemark', values_to = 'remarkCode') %>% 
  filter(! is.na(remarkCode)) %>% 
  group_by(parameterRemark) %>% 
  summarise(unique(remarkCode)) #%>% 
  #write.csv('conventionalsRemarks.csv', row.names = F)

View(conventionalsCodes)

library(pool)
library(dbplyr)
### Production Environment
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "ODBC Driver 11 for SQL Server", #"SQL Server Native Client 11.0", 
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)

labCodes <- pool %>% tbl(in_schema('wqm', "Wqm_Comment_Cds_Codes_Wqm_View")) %>%
  as_tibble()
#write.csv(labCodes, 'labCodes.csv', row.names = F)


filter(labCodes, Com_Code %in% conventionalsCodes$`unique(remarkCode)`) %>% 
  View()

codeJoin <- conventionalsCodes %>% 
  left_join(labCodes %>% dplyr::select(Com_Code, Com_Description), 
            by = c('unique(remarkCode)' = 'Com_Code')) %>% 
  arrange(`unique(remarkCode)`) 

#write.csv(codeJoin, 'conventionalsCodeByParameter.csv', row.names = F)

IFfields <- filter(codeJoin, `unique(remarkCode)` == 'IF') %>% 
  distinct(parameterRemark) %>% 
  pull()
gsub( "RMK_", "",IFfields)

z <- conventionals %>% 
  group_by(FDT_STA_ID, FDT_DATE_TIME) %>% 
  dplyr::select( IFfields, contains(gsub( "RMK_", "",IFfields))) %>% 
  dplyr::select(-contains('LEVEL')) %>% 
  dplyr::select(any_of(names(conventionals))) %>% 
  filter_at(.vars = vars(contains("RMK")),
            .vars_predicate = any_vars(str_detect(., 'IF')))  
  
# Questions for the group:
# - Do we want to filter out any of the above comment codes to not display to conventionals use cases?
# - Are there other comment codes we want removed?
#
#- conventionals script- do we want to change the process around  
#   mutate(Ana_Uncensored_Value = case_when(Ana_Com_Code %in% c('IF', 'J', 'O', 'PE', 'Q1', 'QF', 'QFQ', 'V') ~ as.numeric(NA), 
#                                           TRUE ~ Ana_Uncensored_Value)) %>% # drop results from invalid lab codes
# to just filter these out, right now the lagging Comment code is sticking around and riding along to future manipulation steps (e.g. Phosphorus calculated field)

