# Join assessment region and subbasin information to distinctSites

# update with IR2024, since we are using pinned data for spatial information that has already been spatially
# joined to the assessment layer (using this method for capturing stations outside assessment region polygons)
# we no longer have to repeat that method here
library(tidyverse)
library(sf)
library(readxl)
library(lubridate)
library(lwgeom)

distinctSites_sf <- readRDS('data/distinctSites_sf04132023.RDS')
#distinctSites_sf <- readRDS('./data/distinctSites_sf03072023.RDS') #distinctSites_sf_withCitmon02232023.RDS')
# use other data if possible first
#distinctSites_sfold <-  readRDS('./data/distinctSites_sf03072023.RDS') #readRDS('./data/distinctSites_sf_withCitmon02232023.RDS')

# glean what we can from a previous run
distinctSites_sfhelp <- filter(distinctSites_sf, is.na(ASSESS_REG) | is.na(US_L3NAME) | is.na(BASIN_CODE)) 



assessmentLayer <- st_read('../GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) # transform to WQS84 for spatial intersection

subbasinLayer <- st_read('../GIS/DEQ_VAHUSB_subbasins_EVJ.shp')  %>%
  rename('SUBBASIN' = 'SUBBASIN_1')

ecoregion4Large <- st_read('C:/HardDriveBackup/R/GitHub/pinData/data/GIS/VA_level4ecoregion_WGS84.shp')



distinctSites_sf1 <- distinctSites_sfhelp %>% 
  # st_as_sf(distinctSites,
  #                           coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
  #                           remove = F, # don't remove these lat/lon cols from df
  #                           crs = 4326) %>% # add coordinate reference system, needs to be geographic for now bc entering lat/lng, 
  dplyr::select(-BASIN_CODE) %>% 
  #st_intersection(assessmentLayer ) %>% # will need this for citmon
  st_join(dplyr::select(subbasinLayer, BASIN_NAME, BASIN_CODE, SUBBASIN), join = st_intersects) 

# if any joining issues occur, that means that there are stations that fall outside the joined polygon area
# we need to go back in and fix them manually
if(any(nrow(distinctSites_sf1) < nrow(distinctSites_sf) |
       nrow(filter(distinctSites_sf1, is.na(BASIN_CODE))) > 0 )   ){
  missingSites <- bind_rows(filter(distinctSites_sfhelp, ! FDT_STA_ID %in% distinctSites_sf1$FDT_STA_ID),
                            filter(distinctSites_sf1, is.na(BASIN_CODE))) %>% 
    bind_rows(filter(distinctSites_sf1, is.na(ASSESS_REG))) %>% 
    dplyr::select(-c(VAHU6, ASSESS_REG, OFFICE_NM, VaName, Tidal, VAHUSB, FedName, HUC10 ,Basin, BASIN_CODE, BASIN_NAME, SUBBASIN))
    
  # ASSESSMENT REGION FIX
  closest <- mutate(assessmentLayer[0,], FDT_STA_ID =NA) %>%
    dplyr::select(FDT_STA_ID, everything())
  hasVAHU6 <- filter(distinctSites_sf1, !is.na(ASSESS_REG))
  missingSitesVAHU6 <- filter(missingSites, !FDT_STA_ID %in% hasVAHU6$FDT_STA_ID)
  for(i in seq_len(nrow(missingSitesVAHU6))){
    print(i)
    closest[i,] <- assessmentLayer[which.min(st_distance(assessmentLayer, missingSitesVAHU6[i,])),] %>%
      mutate(FDT_STA_ID = missingSitesVAHU6[i,]$FDT_STA_ID) %>%
      dplyr::select(FDT_STA_ID, everything())
  }
  closest <- closest %>% distinct(FDT_STA_ID, .keep_all = T) # sometimes there can be duplicates
  View(closest %>% st_drop_geometry())
  
  # SUBBASIN FIX
  closestSUBB <- mutate(subbasinLayer[0,], FDT_STA_ID =NA) %>%
    dplyr::select(FDT_STA_ID, everything())
  hasSUBB <- filter(distinctSites_sf1, !is.na(BASIN_NAME))
  missingSitesSUBB <- filter(missingSites, !FDT_STA_ID %in% hasSUBB$FDT_STA_ID)
  for(i in seq_len(nrow(missingSitesSUBB))){
    print(i)
    closestSUBB[i,] <- subbasinLayer[which.min(st_distance(subbasinLayer, missingSitesSUBB[i,])),] %>%
      mutate(FDT_STA_ID = missingSitesSUBB[i,]$FDT_STA_ID) %>%
      dplyr::select(FDT_STA_ID, everything())
  }
  closestSUBB <- closestSUBB %>% distinct(FDT_STA_ID, .keep_all = T) # sometimes there can be duplicates
  
  
  
  # ECOREGION FIX
  hasECO <- filter(distinctSites_sf1, !is.na(US_L3CODE))
  missingSitesECO <- filter(missingSites, !FDT_STA_ID %in% hasECO$FDT_STA_ID)
  closestECO <- mutate(ecoregion4Large[0,], FDT_STA_ID =NA) %>%
    dplyr::select(FDT_STA_ID, everything())
  for(i in seq_len(nrow(missingSitesECO))){
    print(i)
    closestECO [i,] <- ecoregion4Large[which.min(st_distance(ecoregion4Large, missingSitesECO[i,])),] %>%
      mutate(FDT_STA_ID = missingSitesECO[i,]$FDT_STA_ID) %>%
      dplyr::select(FDT_STA_ID, everything())
  }
  closestECO <- closestECO %>% distinct(FDT_STA_ID, .keep_all = T) # sometimes there can be duplicates
  #View(closestECO %>% st_drop_geometry())
  
  
  missingSites <- left_join(missingSites, closest %>% st_drop_geometry(),
                            by = 'FDT_STA_ID') %>%
    left_join(dplyr::select(closestSUBB, FDT_STA_ID, BASIN_NAME, BASIN_CODE, SUBBASIN) %>% st_drop_geometry(),
              by = 'FDT_STA_ID') %>% 
    left_join(dplyr::select(hasSUBB, FDT_STA_ID, BASIN_NAME, BASIN_CODE, SUBBASIN) %>% st_drop_geometry(),
              by = 'FDT_STA_ID') %>% 
    left_join(dplyr::select(closestECO, FDT_STA_ID, US_L3CODE, US_L3NAME, US_L4CODE, US_L4NAME) %>% st_drop_geometry(),
              by = 'FDT_STA_ID') %>% 
    mutate(BASIN_NAME = coalesce(BASIN_NAME.x, BASIN_NAME.y),
           BASIN_CODE = coalesce(BASIN_CODE.x, BASIN_CODE.y),
           SUBBASIN = coalesce(SUBBASIN.x, SUBBASIN.y),
           US_L3CODE = coalesce(US_L3CODE.x, US_L3CODE.y),
           US_L3NAME = coalesce(US_L3NAME.x, US_L3NAME.y),
           US_L4CODE = coalesce(US_L4CODE.x, US_L4CODE.y),
           US_L4NAME = coalesce(US_L4NAME.x, US_L4NAME.y)) %>% 
    #st_join(dplyr::select(subbasinLayer, BASIN_NAME, BASIN_CODE, SUBBASIN), join = st_intersects) %>%
    dplyr::select(-c(geometry), geometry) %>%
    dplyr::select(names(distinctSites_sf1))
  #View(missingSites %>% st_drop_geometry())
  
  # 3/26/23
  distinctSites_sf2 <- bind_rows(filter(distinctSites_sf,! FDT_STA_ID %in% missingSites$FDT_STA_ID),
                            missingSites) %>% 
    dplyr::select(FDT_STA_ID, GROUP_STA_ID, STA_DESC, everything()) %>% 
    #group_by(FDT_STA_ID) %>% mutate(n = n()) %>% dplyr::select(n, everything()) %>% arrange(desc(n)) %>% filter(n ==2) %>% 
    distinct(FDT_STA_ID, .keep_all = T)
  
  
  stillMissingBASIN <- filter(distinctSites_sf2, is.na(BASIN_NAME)) %>%
    st_join(dplyr::select(subbasinLayer, BASIN_NAME, BASIN_CODE, SUBBASIN), join = st_intersects) %>% 
    mutate(BASIN_NAME = coalesce(BASIN_NAME.x, BASIN_NAME.y),
           BASIN_CODE = coalesce(BASIN_CODE.x, BASIN_CODE.y),
           SUBBASIN = coalesce(SUBBASIN.x, SUBBASIN.y)) %>%
    dplyr::select(names(distinctSites_sf1)) %>% 
    st_drop_geometry() %>% 
    mutate(BASIN_NAME = case_when(is.na(BASIN_NAME) & BASIN_CODE =='4A' ~ 'Roanoke River',
                                  is.na(BASIN_NAME) & BASIN_CODE =='4B' ~ 'Roanoke River',
                                  is.na(BASIN_NAME) & BASIN_CODE =='5A' ~ 'Chowan River/Dismal Swamp',
                                  is.na(BASIN_NAME) & BASIN_CODE =='6A' ~ 'Tennessee and Big Sandy Rivers',
                                  is.na(BASIN_NAME) & BASIN_CODE =='6B' ~ 'Tennessee and Big Sandy Rivers',
                                  is.na(BASIN_NAME) & BASIN_CODE =='9' ~ 'New River',
                                  TRUE ~ as.character(BASIN_NAME)))
  
  distinctSites_sf3 <- bind_rows(filter(distinctSites_sf2,! FDT_STA_ID %in% stillMissingBASIN$FDT_STA_ID),
                                 stillMissingBASIN)
  
  
  # distinctSites_sf <- rbind(filter(distinctSites_sf1,! FDT_STA_ID %in% missingSites$FDT_STA_ID),
  #                            missingSites)
  
  
} else{
  distinctSites_sf <- distinctSites_sf1
}

# distinctSites_sffixed <- rbind(distinctSites_sfold,
#                                distinctSites_sf2) %>% 
#   distinct(FDT_STA_ID, .keep_all = T)
  # group_by(FDT_STA_ID) %>% 
  # mutate(n = n()) %>% 
  # arrange(desc(n)) %>% 
  # dplyr::select(n, everything())

distinctSites_sf <- distinctSites_sf3

rm(closest); rm(i); rm(subbasinLayer); rm(distinctSites_sf1); rm(closestSUBB); rm(closestECO); rm(hasECO); rm(hasSUBB); rm(hasVAHU6)
rm(missingSites); rm(missingSitesECO); rm(missingSitesSUBB); rm(missingSitesVAHU6); rm(stillMissingBASIN); rm(ecoregion4Large)
rm(distinctSites_sf2); rm(distinctSites_sf3); rm(distinctSites_sfhelp);rm(assessmentLayer)
#saveRDS(distinctSites_sf, './data/distinctSites_sf_postassessmentLayerSubbasinJoin04132023.RDS')#'./data/distinctSites_sf_withCitmon02232023.RDS')
