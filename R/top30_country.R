top30_country <- function(){
  
  eez_fishing_loop <-  eez_fishing %>%
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name.fr")) %>%
    arrange(country)
  
  loop_country <- mclapply(1:nrow(eez_fishing_loop),function(i){
    
    iso3_temp <- eez_fishing_loop$iso3[i]
    
    #Top 20 MPAs
    top30 <- fishing_effort_mpa %>%
      st_drop_geometry() %>%
      filter(parent_iso == iso3_temp) %>%
      group_by(id) %>%
      mutate(fishing_in_mpa = sum(apparent_fishing_hours)) %>%
      ungroup() %>%
      arrange(-fishing_in_mpa) %>%
      distinct(id, .keep_all = T) %>%
      distinct(tolower(name.1), .keep_all = T) %>%
      head(30) %>%
      mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name.fr")) %>%
      dplyr::select(name.1,gis_m_area,status_yr,country,fishing_in_mpa) %>%
      mutate(gis_m_area = format(round(gis_m_area),big.mark = " "),
             fishing_in_mpa = format(round(fishing_in_mpa),big.mark = " "),
             name.1 = str_to_sentence(tolower(name.1))) %>%
      dplyr::rename(`Nom de l'AMP`="name.1",
                    `Taille de l'AMP (km^2)`= "gis_m_area",
                    `Année de création`= "status_yr",
                    `Pays` = "country",
                    `Heure de pêche dans l'AMP`="fishing_in_mpa") %>%
      rownames_to_column("Position")
    
    write.csv(top30, file = paste0("output/top30",iso3_temp,".csv"))
    
  })
 
}