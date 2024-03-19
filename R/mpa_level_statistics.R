mpa_level_statistics <- function(fishing_effort_mpa) {
  
  #Number of MPAs fished 
  print("Analyse du nombre d'aires marines protégées pêchées")
  fished_mpas <- fishing_effort_mpa %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    mutate(fishing_in_mpa = sum(apparent_fishing_hours),
           fishing_intensity = fishing_in_mpa/gis_m_area) %>%
    ungroup() %>%
    #ONLY IF FISHING IS HIGHER THAN  5
    filter(fishing_in_mpa > 5) %>%
    distinct(id, .keep_all = T)

  
  fished_mpas_area <- sum(fished_mpas$gis_m_area)
  
  total_mpa_area <- sum(mpa_eu_wdpa$gis_m_area)
  
  print(paste("Parmi les",nrow(mpa_eu_wdpa),"étudiées,",nrow(fished_mpas),"sont pêchées en 2023, soit",(nrow(fished_mpas)/nrow(mpa_eu_wdpa))*100,"%"))
  
  print(paste("La surface des AMPs pêchées est de ",sum(fished_mpas$gis_m_area),"km^2."))
  
  print(paste(round((fished_mpas_area/total_mpa_area)*100,2),"% de la surfaces des AMPs est pêchée"))
  
  #Amp pas pêchées
  non_fished_mpas <- mpa_eu_wdpa %>% filter(!id %in% fished_mpas$id)
  
  sd(non_fished_mpas$gis_m_area)
  sd(fished_mpas$gis_m_area)
  
  
  #Relative to a country's fishing effort
  print("Analyse de l'effort de pêche dans l'AMP vis à vis de l'extérieur")
  
  fished_mpas_stats <- fished_mpas %>%
    mutate(fishing_intensity_in_mpa = fishing_in_mpa/gis_m_area) %>%
    left_join(eez_fishing, by = "iso3") %>%
    mutate(relative_fishing_country = as.numeric(relative_fishing_country)) %>%
    group_by(iso3) %>%
    dplyr::filter(fishing_intensity_in_mpa > relative_fishing_country) %>%
    ungroup()
  
  print(paste("Parmi les",nrow(fished_mpas),"aires marines protégées étudiées",nrow(fished_mpas_stats),"ont une intensité de pêche supérieure à l'extérieur de l'AMP pour le pays concerné, soit",(nrow(fished_mpas_stats)/nrow(fished_mpas))*100,"%"))
  
  print(paste("La surface des AMPs les plus pêchées est de",sum(fished_mpas_stats$gis_m_area),"soit",(sum(fished_mpas_stats$gis_m_area)/sum(fished_mpas$gis_m_area))*100,"%"))
  
  #Le classement pêche
  fished_mpas <- fishing_effort_mpa %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    mutate(fishing_in_mpa = sum(apparent_fishing_hours),
           fishing_intensity = fishing_in_mpa/gis_m_area) %>%
    ungroup() %>%
    filter(fishing_in_mpa > 5) %>%
    distinct(id, .keep_all = T)
  
  #En nombre d'heure de pêche
  top10_peche <- fished_mpas %>%
    arrange(-fishing_in_mpa) %>%
    #Removing duplicate names 
    distinct(name.1,.keep_all = T) %>%
    distinct(fishing_in_mpa,.keep_all=T) %>%
    left_join(shapefile_mpa, by = "id") %>%
    st_as_sf() %>%
    head(5)
  
  top10_peche_plot <- ggplot(top10_peche,aes(reorder(name.1,fishing_in_mpa),fishing_in_mpa))+
    geom_bar(stat='identity',aes(fill = name.1)) +
    scale_fill_hp_d(option = "Ravenclaw") +
    coord_flip() +
    theme_minimal(base_size = 20, element_text(family = "National")) +
    theme(legend.position = "none")  +
    labs( x = " ",
          y = "Effort de pêche dans l'AMP en 2023",
          fill = "MPA name") 
  
  # ggsave(top10_peche_plot, file = "figures/top10_peche_plot.pdf", width = 297, height = 210, units = "mm", dpi = 600)
  ggsave(top10_peche_plot, file = "figures/English/Annexe_3.jpg", width = 350, height = 210, units = "mm", dpi = 600)
  
  #En intensité pêche
  top10_peche_intensity <- fished_mpas %>%
    arrange(-fishing_intensity) %>%
    #Removing duplicate names 
    distinct(name.1,.keep_all = T) %>%
    distinct(fishing_intensity,.keep_all=T) %>%
    left_join(shapefile_mpa, by = "id") %>%
    head(10)
  
  top10_peche_intensity_plot <- ggplot(top10_peche_intensity,aes(reorder(name.1,fishing_intensity),fishing_intensity))+
    geom_bar(stat='identity',aes(fill = name.1)) +
    scale_fill_hp_d(option = "Ravenclaw") +
    coord_flip() +
    theme_minimal(base_size = 16, element_text(family = "National"))+
    theme(legend.position = "none")  +
    labs( x = " ",
          y = "Fishing intensity inside MPAs in 2023")
  
  # ggsave(top10_peche_intensity_plot, file = "figures/top10_peche_intensity_plot.pdf", width = 297, height = 210, units = "mm", dpi = 600)
  ggsave(top10_peche_intensity_plot, file = "figures/English/Annexe_6.jpg", width = 297, height = 210, units = "mm", dpi = 600)
  
  
  
}