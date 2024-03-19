data_intersection_union <- function(fishing_effort_clean, mpa_eu_wdpa) {
  
  #Converting cleaned data to sf object
  cleaned_data_sf <- fishing_effort_clean %>%
    st_as_sf(coords= c("lon","lat"), crs = 4326) %>%
    st_transform(crs = st_crs(mpa_eu_wdpa)) %>%
    filter(Year == 2023) %>%
    st_make_valid()
  
  #Union MPA polygons for country level analysis
  WDPA_database_temp <- mpa_eu_wdpa %>%
    st_make_valid() 
  
  #Clean pelagos MPA
  pelagos <- mpa_eu_wdpa %>%
    filter(iso3 == "FRA;ITA;MCO") %>%
    st_intersection(eez) %>%
    mutate(name = paste0(name, SOVEREIGN1),
           iso3 = ifelse(SOVEREIGN1 == "France","FRA",
                         ifelse(SOVEREIGN1 == "Italy","ITA",iso3)),
           gis_m_area = as.numeric(set_units(st_area(geometry),km^2))) %>%
    dplyr::filter(iso3 != "FRA;ITA;MCO") %>%
    dplyr::select(id:source, geometry)
  
  WDPA_database_temp <- WDPA_database_temp %>%
    dplyr::filter(iso3 != "FRA;ITA;MCO") %>%
    bind_rows(pelagos)
  
  #Union of all MPAs
  WDPA_database_union <- WDPA_database_temp %>%
    group_by(iso3) %>%
    reframe(geometry = st_union(geometry)) %>%
    ungroup() %>%
    st_as_sf() 
  
  save(WDPA_database_union, file = "output/WDPA_database_union.Rdata")
  
  #Loop through each country to get intersections
  mpa_intersection_loop <- mclapply(1:nrow(WDPA_database_union), function(i) {
    
    temp = WDPA_database_union[i,]

    iso3<- temp$iso3
    
    #First using st_intersects to get the points that actually match
    y_subset <-
      st_intersects(temp,cleaned_data_sf) %>%
      unlist() %>%
      unique() %>%
      sort() %>%
      {cleaned_data_sf[.,]}
    
    if (nrow(y_subset) > 0) {
    
      intersection <- cbind(y_subset,iso3)
      
      save(intersection, file = paste0("output/intersections_union/intersection_union_",iso3,".Rdata"))
      print(paste(iso3,": Intersection"))
      
    } else {print(paste(iso3,": No intersection"))}
    
  }, mc.cores = 4)
  
}
