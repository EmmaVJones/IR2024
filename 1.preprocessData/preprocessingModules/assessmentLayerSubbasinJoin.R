# Join assessment region and subbasin information to distinctSites

# update with IR2024, since we are using pinned data for spatial information that has already been spatially
# joined to the assessment layer (using this method for capturing stations outside assessment region polygons)
# we no longer have to repeat that method here

distinctSites_sf <- readRDS('./data/distinctSites_sf_withCitmon02232023.RDS')

assessmentLayer <- st_read('../GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) # transform to WQS84 for spatial intersection

subbasinLayer <- st_read('../GIS/DEQ_VAHUSB_subbasins_EVJ.shp')  %>%
  rename('SUBBASIN' = 'SUBBASIN_1')




distinctSites_sf1 <- distinctSites_sf %>% 
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
  missingSites <- bind_rows(filter(distinctSites_sf, ! FDT_STA_ID %in% distinctSites_sf1$FDT_STA_ID),
                            filter(distinctSites_sf1, is.na(BASIN_CODE))) %>% 
    bind_rows(filter(distinctSites_sf1, is.na(ASSESS_REG))) %>% 
    dplyr::select(-c(VAHU6, ASSESS_REG, OFFICE_NM, VaName, Tidal, VAHUSB, FedName, HUC10 ,Basin, BASIN_CODE, BASIN_NAME, SUBBASIN))
    
  closest <- mutate(assessmentLayer[0,], FDT_STA_ID =NA) %>%
    dplyr::select(FDT_STA_ID, everything())
  for(i in seq_len(nrow(missingSites))){
    closest[i,] <- assessmentLayer[which.min(st_distance(assessmentLayer, missingSites[i,])),] %>%
      mutate(FDT_STA_ID = missingSites[i,]$FDT_STA_ID) %>%
      dplyr::select(FDT_STA_ID, everything())
  }
  closest <- closest %>% distinct(FDT_STA_ID, .keep_all = T) # sometimes there can be duplicates
  
  closestSUBB <- mutate(subbasinLayer[0,], FDT_STA_ID =NA) %>%
    dplyr::select(FDT_STA_ID, everything())
  for(i in seq_len(nrow(missingSites))){
    print(i)
    closestSUBB[i,] <- subbasinLayer[which.min(st_distance(subbasinLayer, missingSites[i,])),] %>%
      mutate(FDT_STA_ID = missingSites[i,]$FDT_STA_ID) %>%
      dplyr::select(FDT_STA_ID, everything())
  }
  closestSUBB <- closestSUBB %>% distinct(FDT_STA_ID, .keep_all = T) # sometimes there can be duplicates
  
  missingSites <- left_join(missingSites, closest %>% st_drop_geometry(),
                            by = 'FDT_STA_ID') %>%
    left_join(dplyr::select(closestSUBB, FDT_STA_ID, BASIN_NAME, BASIN_CODE, SUBBASIN) %>% st_drop_geometry(),
              by = 'FDT_STA_ID') %>% 
    #st_join(dplyr::select(subbasinLayer, BASIN_NAME, BASIN_CODE, SUBBASIN), join = st_intersects) %>%
    dplyr::select(-c(geometry), geometry) %>%
    dplyr::select(names(distinctSites_sf1))
  
  
  distinctSites_sf <- rbind(filter(distinctSites_sf1,! FDT_STA_ID %in% missingSites$FDT_STA_ID),
                             missingSites)
  
  
  # original method but so many sites are so close, seems silly to not give it a go
#  missingSites <- filter(distinctSites, ! FDT_STA_ID %in% distinctSites_sf$FDT_STA_ID) %>%
#    st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
#             remove = F, # don't remove these lat/lon cols from df
#             crs = 4326) %>%
#    mutate(HUC12 = NA, VAHU6 = NA,Portion = NA, MAP = NA, ASSESS_REG = NA, OFFICE_NM = NA, 
#           States = NA, HUType = NA, HUMod = NA, ToHUC = NA, META_ID = NA, Location = NA, 
#           VaName = NA, PC_Water = NA, Tidal = NA, VAHUSB = NA, FedName = NA, HUC10 = NA, 
#           VAHU5 = NA, Basin = NA) %>%
#    st_join(dplyr::select(subbasinLayer, BASIN_NAME, BASIN_CODE, SUBBASIN), join = st_intersects) %>%
#    dplyr::select(-c(geometry), geometry)
  
} else{
  distinctSites_sf <- distinctSites_sf1
}




rm(closest); rm(i); rm(subbasinLayer); rm(distinctSites_sf1); rm(closestSUBB)
#saveRDS(distinctSites_sf, './data/distinctSites_sf_withCitmon02232023.RDS')
