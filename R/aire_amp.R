aire_amp <- function(WDPA_database_union){
  
  #Calculating total MPA area for each country
  aire_amp <- WDPA_database_union %>%
    mutate(area = st_area(WDPA_database_union),
           area = set_units(area, km^2)) %>%
    st_drop_geometry()
  
  save(aire_amp, file = "output/aire_amp.Rdata")
  
  
}