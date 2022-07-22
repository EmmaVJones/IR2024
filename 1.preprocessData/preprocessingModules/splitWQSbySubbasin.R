# This script breaks apart the WQS layers into smaller pieces for easier use in apps


#assessmentRegions <- st_read( '../GIS/AssessmentRegions_simple.shp')



# Make object to save available Subbasins for app 
subbasinOptionsByWQStype <- tibble(waterbodyType = as.character(),
                                   SubbasinOptions = as.character(),
                                   AssessmentRegion = as.character(),
                                   WQS_ID_Prefix = as.character())

basinCodesConversion <- read_csv('data/basinCodeConversion.csv') %>%
  filter(BASIN != 7) %>%
  bind_rows(data.frame(BASIN = '7D', Basin_Code = 'Small Coastal'))

# Bring in subbasin options
subbasins <- st_read('../GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
  rename('SUBBASIN' = 'SUBBASIN_1')


assessmentLayer <- st_read('../GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) %>%
  group_by(VAHUSB, ASSESS_REG) %>%
  summarise() %>% ungroup()


# Riverine split up
riverineL <- st_read('../GIS/draft_WQS_shapefiles_2022/riverine_draft2022.shp' , fid_column_name = "OBJECTID") %>%
  st_transform(4326)   # transform to WQS84 for spatial intersection 

# intersect subbasins withriverine WQS to get appropriate subabsin argument for data organization
riverineLB <- st_join(st_zm(riverineL), dplyr::select(subbasins, BASIN_CODE, ASSESS_REG), join = st_intersects) %>%
  # and if segment doesnt make the cut for subbasins layer (the edges)
  mutate(BASIN_CODE = case_when(is.na(BASIN_CODE) ~ as.character(BASIN), 
                                TRUE ~ as.character(BASIN_CODE))) %>%
  left_join(basinCodesConversion, by = c('BASIN_CODE' ='BASIN')) %>%
  rename('Subbasin' = 'Basin_Code.y',
         'Basin_Code' = 'Basin_Code.x')


for(i in 1:length(unique(riverineLB$BASIN_CODE))){
  z <- filter(riverineLB, BASIN_CODE == as.character(unique(riverineLB$BASIN_CODE)[i])) %>%
    select(-ASSESS_REG)
  # identify which assessment regions have data for app, only way to verify that a given waterbody belongs to a region
  #z1 <- st_intersection(filter(subbasins, BASIN_CODE == unique(riverineLB$BASIN_CODE)[i] ),
  #                      assessmentRegions)
  z1 <- st_join(z, assessmentLayer, join = st_intersects)
  
  for(k in 1:length(unique(z1$VAHUSB))){
    z2 <- filter(z1, VAHUSB == as.character(unique(z1$VAHUSB)[k])) %>%
      # in case segment split between two regions, just keep one, we already have waht we need from regional data
      distinct(WQS_ID, .keep_all = T)
    if(nrow(z2) > 0){
      st_write(z2, paste0('../GIS/processedWQS/RL_', 
                          as.character(unique(z2$BASIN_CODE)),'_',
                          as.character(unique(z2$VAHUSB)), '.shp'), driver = "ESRI Shapefile")
    }
  }
  
  subbasinAssessmentOptions <- tibble(waterbodyType = as.character('Riverine'),
                                      SubbasinOptions = as.character(unique(z1$BASIN_CODE)),
                                      AssessmentRegion = as.character(unique(z1$ASSESS_REG)),
                                      WQS_ID_Prefix = as.character('RL'))
  subbasinOptionsByWQStype <- bind_rows(subbasinOptionsByWQStype, subbasinAssessmentOptions)
  ## in case segment split between two regions, just keep one, we already have waht we need from regional data
  #z <- z %>% distinct(WQS_ID, .keep_all = T)
  #st_write(z, paste0('GIS/processedWQS/RL_', 
  #                   unique(z$BASIN_CODE), '.shp'))
}

# and save all WQS_ID options to be called in the app easily
allWQS_ID <- tibble(WQS_ID = unique(riverineLB$WQS_ID))


rm(riverineLB);rm(riverineL)




# Lacustrine split up
lakesL <- st_read('../GIS/draft_WQS_shapefiles_2022/lakes_reservoirs_draft2022.shp', fid_column_name = "OBJECTID") %>%
  st_transform(4326)   # transform to WQS84 for spatial intersection 

# intersect lake WQS with assessmentLayer to get appropriate Basin argument for data organization
lakesLB <- st_join(st_zm(lakesL), dplyr::select(subbasins, BASIN_CODE, ASSESS_REG), join = st_intersects) %>%
  # and if segment doesnt make the cut for subbasins layer (the edges)
  mutate(BASIN_CODE = case_when(is.na(BASIN_CODE) ~ as.character(BASIN), 
                                TRUE ~ as.character(BASIN_CODE))) %>%
  left_join(basinCodesConversion, by = c('BASIN_CODE' ='BASIN')) %>%
  rename('Subbasin' = 'Basin_Code.y',
         'Basin_Code' = 'Basin_Code.x')

#View(filter(lakesLB, is.na(Subbasin)))
#View(filter(lakesLB, is.na(BASIN_CODE)))
#View(filter(lakesLB, is.na(ASSESS_REG)))

# sometimes lakes can join to more than one assessment region, fix it
tooMany <- lakesLB %>%
 group_by(WQS_ID) %>%
 mutate(n = n()) %>% ungroup() %>%
 filter(n>1) %>% 
  arrange(WQS_ID)

lakesLBfix <- tooMany %>% 
  mutate(keep = case_when(WQS_ID == 'LP_08_028560' & ASSESS_REG == 'NRO' ~ 'Keep',
                          WQS_ID == 'LP_2A_005412' & ASSESS_REG == 'BRRO' ~ 'Keep',
                          WQS_ID == 'LP_2C_039943' & ASSESS_REG == 'TRO' ~ 'Keep',
                          WQS_ID == 'LP_2D_020772' & ASSESS_REG == 'PRO' ~ 'Keep',
                          WQS_ID == 'LP_2D_037943' & ASSESS_REG == 'PRO' ~ 'Keep',
                          WQS_ID == 'LP_4A_009151' & ASSESS_REG == 'BRRO' ~ 'Keep',
                          WQS_ID == 'LP_4A_009441' & ASSESS_REG == 'BRRO' ~ 'Keep',
                          WQS_ID == 'LP_4A_009475' & ASSESS_REG == 'BRRO' ~ 'Keep',
                          WQS_ID == 'LP_4A_013566' & ASSESS_REG == 'BRRO' ~ 'Keep',
                          WQS_ID == 'LP_4A_013612' & ASSESS_REG == 'BRRO' ~ 'Keep',
                          TRUE ~ as.character(NA))) %>% 
  filter(keep == 'Keep') %>% 
  distinct(WQS_ID, .keep_all = T) %>% 
  dplyr::select(names(lakesLB))

lakesLB <- filter(lakesLB, ! WQS_ID %in% lakesLBfix$WQS_ID) %>% 
  rbind(lakesLBfix)

# double check the nrow is as expected
nrow(lakesL) == nrow(lakesLB)

# clean up workspace
rm(lakesLBfix); rm(z); rm(z1); rm(z2); rm(tooMany)


# Organize lake segments
for(i in 1:length(unique(lakesLB$BASIN_CODE))){
  z <- filter(lakesLB, BASIN_CODE == as.character(unique(lakesLB$BASIN_CODE)[i]))
  # identify which assessment regions have data for app, the only way to verify that a given waterbody belongs to a region
  #z1 <- st_intersection(filter(basin7, BASIN_CODE == unique(riverineLB$BASIN_CODE)[i] ),
  #                      assessmentRegions)
  subbasinAssessmentOptions <- tibble(waterbodyType = as.character('Lacustrine'),
                                      SubbasinOptions = as.character(unique(z$BASIN_CODE)),
                                      AssessmentRegion = as.character(unique(z$ASSESS_REG)),
                                      WQS_ID_Prefix = as.character('LP'))
  subbasinOptionsByWQStype <- bind_rows(subbasinOptionsByWQStype, subbasinAssessmentOptions)
  # in case segment split between two regions, just keep one, we already have waht we need from regional data
  z <- z %>% distinct(WQS_ID, .keep_all = T)
  st_write(z, paste0('../GIS/processedWQS/LP_', 
                     unique(z$BASIN_CODE), '.shp'))
}


# and save all WQS_ID options to be called in the app easily
allWQS_ID <- bind_rows(allWQS_ID, tibble(WQS_ID = unique(lakesLB$WQS_ID)))


# clean up workspace
rm(lakesLB);rm(lakesL); rm(z)





# Estuary Lines split up

estuaryL <- st_read('../GIS/draft_WQS_shapefiles_2022/estuarinelines_draft2022.shp' , fid_column_name = "OBJECTID") %>%
  st_transform(4326)   # transform to WQS84 for spatial intersection 

# intersect estuarine lines WQS with assessmentLayer to get appropriate Basin argument for data organization
estuaryLB <- st_join(st_zm(estuaryL), dplyr::select(subbasins, BASIN_CODE, ASSESS_REG), join = st_intersects) %>%
  # and if segment doesnt make the cut for subbasins layer (the edges)
  mutate(BASIN_CODE = case_when(is.na(BASIN_CODE) ~ as.character(BASIN), 
                                TRUE ~ as.character(BASIN_CODE))) %>%
  left_join(basinCodesConversion, by = c('BASIN_CODE' ='BASIN')) %>%
  rename('Subbasin' = 'Basin_Code.y',
         'Basin_Code' = 'Basin_Code.x') %>% 
  dplyr::select(-OBJECTID_1)


#View(filter(estuaryLB, is.na(Subbasin)))
#View(filter(estuaryLB, is.na(BASIN_CODE)))
#View(filter(estuaryLB, is.na(ASSESS_REG)))


for(i in 1:length(unique(estuaryLB$BASIN_CODE))){
  z <- filter(estuaryLB, BASIN_CODE == as.character(unique(estuaryLB$BASIN_CODE)[i]))
  # identify which assessment regions have data for app, takes forever but only way to verify that a given waterbody belongs to a region
  # note difference for estuarine!
  #z1 <- st_intersection(z,assessmentRegions)
  subbasinAssessmentOptions <- tibble(waterbodyType = as.character('Estuarine'),
                                      SubbasinOptions = as.character(unique(z$BASIN_CODE)),
                                      AssessmentRegion = as.character(unique(z$ASSESS_REG)),
                                      WQS_ID_Prefix = as.character('EL'))
  subbasinOptionsByWQStype <- bind_rows(subbasinOptionsByWQStype, subbasinAssessmentOptions)
  # in case segment split between two regions, just keep one, we already have waht we need from regional data
  z <- z %>% distinct(WQS_ID, .keep_all = T)
  st_write(z, paste0('../GIS/processedWQS/EL_', 
                     unique(z$BASIN_CODE), '.shp'))
}



# and save all WQS_ID options to be called in the app easily
allWQS_ID <- bind_rows(allWQS_ID, tibble(WQS_ID = unique(estuaryLB$WQS_ID)))



rm(estuaryLB);rm(estuaryL); rm(z)




# Estuary Polygonss split up


estuaryP <- st_read('../GIS/draft_WQS_shapefiles_2022/estuarinepolygons_draft2022.shp' , fid_column_name = "OBJECTID") %>%
  st_transform(4326)   # transform to WQS84 for spatial intersection 

# intersect estuarine polygons WQS with assessmentLayer to get appropriate Basin argument for data organization
estuaryPB <- st_join(st_zm(estuaryP), dplyr::select(subbasins, BASIN_CODE, ASSESS_REG), join = st_intersects) %>%
  # and if segment doesnt make the cut for subbasins layer (the edges)
  mutate(BASIN_CODE = case_when(is.na(BASIN_CODE) ~ as.character(BASIN), 
                                TRUE ~ as.character(BASIN_CODE))) %>%
  left_join(basinCodesConversion, by = c('BASIN_CODE' ='BASIN')) %>%
  rename('Subbasin' = 'Basin_Code.y',
         'Basin_Code' = 'Basin_Code.x')


#View(filter(estuaryPB, is.na(Subbasin)))
#View(filter(estuaryPB, is.na(BASIN_CODE)))
#View(filter(estuaryPB, is.na(ASSESS_REG)))


for(i in 1:length(unique(estuaryPB$BASIN_CODE))){
  z <- filter(estuaryPB, BASIN_CODE == as.character(unique(estuaryPB$BASIN_CODE)[i]))
  # identify which assessment regions have data for app, takes forever but only way to verify that a given waterbody belongs to a region
  # note difference for estuarine!
  #z1 <- st_intersection(z,assessmentRegions)
  subbasinAssessmentOptions <- tibble(waterbodyType = as.character('Estuarine'),
                                      SubbasinOptions = as.character(unique(z$BASIN_CODE)),
                                      AssessmentRegion = as.character(unique(z$ASSESS_REG)),
                                      WQS_ID_Prefix = as.character('EP'))
  subbasinOptionsByWQStype <- bind_rows(subbasinOptionsByWQStype, subbasinAssessmentOptions)
  # in case segment split between two regions, just keep one, we already have waht we need from regional data
  z <- z %>% distinct(WQS_ID, .keep_all = T)
  st_write(z, paste0('../GIS/processedWQS/EP_', 
                     unique(z$BASIN_CODE), '.shp'))
}


# and save all WQS_ID options to be called in the app easily
allWQS_ID <- bind_rows(allWQS_ID, tibble(WQS_ID = unique(estuaryPB$WQS_ID)))


# Save a list of all available WQS_ID's for app
saveRDS(allWQS_ID, 'data/allWQS_ID.RDS')

# clean up workspace
rm(estuaryPB);rm(estuaryP); rm(z); rm(subbasinAssessmentOptions); rm(i); rm(k)



## Now something

#z <- filter(subbasinOptionsByWQStype, is.na(AssessmentRegion)) #manually check everyone is there already
# all looks good, can drop na's
subbasinOptionsByWQStype <- filter(subbasinOptionsByWQStype, ! is.na(AssessmentRegion))

write.csv(subbasinOptionsByWQStype, 'data/subbasinOptionsByWQStype&Region.csv', row.names = F)

rm(z)

# Now cross reference with Cleo's QAed information and drop subbasin/Assessment region options that don't pass her test
basinAssessmentRegion <- read_csv('data_old/basinAssessmentReg_clb_EVJ.csv')  # Cleo QAed verison

subbasinOptionsByWQStype1 <- left_join(subbasinOptionsByWQStype, basinAssessmentRegion,
                                       by = c('SubbasinOptions' = 'BASIN_CODE',
                                              'AssessmentRegion' = 'ASSESS_REG')) %>%
  filter(! VAHU6_NOTE %in% c('NOT IN THIS REGION', NA))

write.csv(subbasinOptionsByWQStype, 'data/subbasinOptionsByWQStype&Region.csv', row.names = F)
