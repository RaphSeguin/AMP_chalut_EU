data_intersection <- function(fishing_effort_clean, mpa_eu_wdpa) {
  
  #Converting cleaned data to sf object
  cleaned_data_sf <- fishing_effort_clean %>%
    st_as_sf(coords= c("lon","lat"), crs = 4326) %>%
    st_transform(crs = st_crs(mpa_eu_wdpa)) %>%
    filter(Year == 2023) %>%
    st_make_valid() 
  
  mpa_intersection_loop <- pbmclapply(1:nrow(mpa_eu_wdpa), function(i) {
  
  temp = mpa_eu_wdpa[i,]
  
  temp_id <- temp$id
  
  #intersection
  intersection <- st_intersection(cleaned_data_sf, temp)

  if (nrow(intersection) > 0) {
    
    save(intersection, file = paste0("output/intersections/intersection",temp_id,".Rdata"))
    print(paste(temp_id,": Intersection"))
    
  } else {print(paste(temp_id,": No intersection"))}
  
  }, mc.cores = 8)
  
}