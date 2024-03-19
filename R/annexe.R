annexe <- function(){
  
  #Distribution de la taille des navires selon GFW
  (taille_bateau <- fishing_effort_clean %>%
     mutate(vessel_class_registry = sub("\\|.*", "", vessel_class_registry)) %>%
     filter(vessel_class_registry == "trawlers" | gear %in% c("Single boat bottom otter trawls","Bottom trawls nephrops trawls","Single boat midwater otter trawls",
                                                              "Beam trawls","Bottom pair trawls","Twin bottom otter trawls","Midwater pair trawls","Bottom trawls (nei)",
                                                              "Otter trawls (nei)")) %>%
    distinct(mmsi, clean_length) %>%
    ggplot(aes(clean_length)) +
    geom_histogram(binwidth=3, fill="#01C9BA", color="#e9ecef", alpha=0.9) +
    scale_x_continuous(breaks = c(0,10,20,30,40,50,75,100,125,150)) +
    theme_minimal(base_size = 16, element_text(family = "National")) +
    labs(x = "Vessel size (m)",
         y = "Number of vessels detected by Global Fishing Watch"))
  
  ggsave(taille_bateau, file = "figures/English/annexe_1.jpg", width = 297, height = 210, units = "mm", dpi = 600)
  
  #Distribution des AMPs pêchées/pas pêchées
  
  #Total hours of fishing per MPA
  (fished_mpas <- fishing_effort_mpa %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    mutate(fishing_in_mpa = sum(apparent_fishing_hours)) %>%
    ungroup() %>%
    #ONLY IF FISHING IS HIGHER THAN  5
    filter(fishing_in_mpa > 5) %>%
    distinct(id, fishing_in_mpa) %>%
    full_join(mpa_eu_wdpa, by = "id") %>%
    st_drop_geometry() %>%
    mutate(fishing = ifelse(is.na(fishing_in_mpa),"No fishing","Fishing")) %>%
    ggplot(aes(fishing, log(gis_m_area+1))) +
    geom_violin() +
    theme_minimal(base_size = 16, element_text(family = "National")) +
      labs(x = " ",
           y = "MPA size (km^2)"))
  
  ggsave(fished_mpas, file = "figures/English/annexe_2.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #Classement pays nombre d'heure de pêche
  (pays_peche_classement <- fishing_effort_mpa_country %>%
      st_drop_geometry() %>%
      left_join(aire_amp, by = "iso3") %>%
      mutate(country = countrycode(iso3,origin="iso3c",destination="country.name"))  %>%
      group_by(country) %>%
      mutate(sum_fishing = sum(apparent_fishing_hours),
             intensity_fishing = sum_fishing/area,
             number_of_boats = length(unique(mmsi)),
             mean_length = mean(length_m_registry)) %>%
      ungroup() %>%
      distinct(country, .keep_all = T) %>%
      ggplot(aes(reorder(country,sum_fishing), sum_fishing)) +
      geom_bar(stat="identity",fill="#01C9BA") +
      coord_flip() +
      theme_minimal(base_size = 14, element_text(family = "National")) +
      labs(y = "Fishing hours",
           x = " "))
  
  ggsave(pays_peche_classement, file = "figures/English/Annexe_7.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  
  #Classement intensité de pêche
  (pays_intensite_classement <- fishing_effort_mpa_country %>%
      st_drop_geometry() %>%
      left_join(aire_amp, by = "iso3") %>%
      mutate(country = countrycode(iso3,origin="iso3c",destination="country.name"))  %>%
      group_by(country) %>%
      mutate(sum_fishing = sum(apparent_fishing_hours),
             intensity_fishing = sum_fishing/area,
             number_of_boats = length(unique(mmsi)),
             mean_length = mean(length_m_registry)) %>%
      ungroup() %>%
      distinct(country, .keep_all = T) %>%
      ggplot(aes(reorder(country,intensity_fishing), intensity_fishing)) +
      geom_bar(stat="identity",fill="#01C9BA") +
      coord_flip() +
      theme_minimal(base_size = 16,element_text(family = "National")) + 
      labs(y = "Fishing intensity",
           x = " "))
  
  ggsave(pays_intensite_classement, file = "figures/English/annexe_11.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #Disribution intensité par amp
  (amp_intensite_classement <- fishing_effort_mpa %>%
      st_drop_geometry() %>%
      mutate(country = countrycode(iso3,origin="iso3c",destination="country.name"))  %>%
      group_by(id) %>%
      mutate(sum_fishing = sum(apparent_fishing_hours),
             intensity_fishing = sum_fishing/gis_m_area) %>%
      ungroup() %>%
      distinct(id, .keep_all = T) %>%
    filter(sum_fishing > 5) %>%
    left_join(eez_fishing, by = "iso3") %>%
    ggplot(aes(reorder(country,-relative_fishing_country), log10(intensity_fishing+1), fill = iso3)) +
    scale_fill_manual(values = country_color_scale) +
    geom_jitter(alpha = 0.5, size = 0.1) + 
    geom_boxplot(alpha = 0.5,aes(middle = mean(intensity_fishing))) +
      geom_point(aes(country, log10(as.numeric(relative_fishing_country)+1)), fill = "darkred",pch =21, color = "black") + 
    theme_minimal(base_size = 16, element_text(family = "National")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      labs(x = " ",
           y = "Fishing intensity",
           fill = "Country") +
      theme(legend.position = "bottom"))
  
  ggsave(amp_intensite_classement, file = "figures/English/Annexe_12.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #Aire de l'AMP couverte par chaque pays
  (S1 <- WDPA_database_union %>%
    mutate(area = set_units(st_area(WDPA_database_union),km^2)) %>%
     mutate(country = countrycode(iso3,origin="iso3c",destination="country.name"))  %>%
    st_drop_geometry() %>%
    ggplot(aes(reorder(country,area), area)) +
    geom_bar(stat="identity",fill="#01C9BA") +
    coord_flip()+
     labs(x = " ",
          y = "Area covered by MPAs") +
     theme_minimal(base_size = 16, element_text(family = "National")))
  
  ggsave(S1, file = "figures/English/annexe_8.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #Flotte par pays
  (S2 <- fishing_effort_mpa_country %>%
    group_by(flag_registry) %>%
    reframe(sum = length(unique(mmsi))) %>%
    ungroup() %>%
    mutate(country = countrycode(flag_registry,origin="iso3c",destination="country.name"))  %>%
      na.omit() %>%
    ggplot(aes(reorder(country,sum),sum)) +
    geom_bar(stat="identity",fill="#01C9BA") +
    coord_flip()+
    theme_minimal(base_size = 16, element_text(family = "National"))+
      labs(x= " ",
           y = "Number of vessels identified according to MMSI"))
  
  ggsave(S2, file = "figures/English/annexe_9.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #Taille moyenne des navires 
  (S3 <- fishing_effort_mpa_country %>%
      group_by(iso3) %>%
      reframe(mean_length = mean(length_m_registry, na.rm=T)) %>%
      ungroup() %>%
      mutate(country = countrycode(iso3,origin="iso3c",destination="country.name"))  %>%
      ggplot(aes(reorder(country,mean_length),mean_length)) +
      geom_bar(stat="identity",fill="#01C9BA") +
      coord_flip()+
      theme_minimal(base_size = 16, element_text(family = "National"))+
      labs(x= " ",
           y = "Average vessel size inside MPAs (m)"))
  
  ggsave(S3, file = "figures/English/Annexe_10.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #Bateau qui pêche le plus dans les AMPs 
  S4 <- fishing_effort_mpa_country %>%
    st_drop_geometry() %>%
    group_by(mmsi) %>%
    mutate(sum_hours = sum(apparent_fishing_hours)) %>%
    ungroup() %>%
    distinct(mmsi, clean_length, sum_hours) %>%
    na.omit() %>%
    left_join(fishing_fleet_complete, by = "mmsi") %>%
    arrange(-clean_length, -sum_hours ) %>%
    filter(clean_length >= 80) %>%
    dplyr::select(mmsi,sum_hours, country, vessel_name, length) %>%
    na.omit() %>%
    dplyr::filter(!mmsi %in% c(228080600, 275503000,219029579)) %>%
    mutate(type = "inside_mpa")
  
  write.csv(S4, "figures/navire_usine.csv")
  
  #AMP golfe d gascognze
  gascogne_mega_trawlers <- fishing_effort_mpa %>%
    filter(id == "555643633") %>%
    filter(mmsi %in% S4$mmsi)
  
  flandre_mega_trawlers <- fishing_effort_mpa %>%
    filter(id == "555556925") %>%
    filter(mmsi %in% S4$mmsi)

  #Detail for each boat
  time_spent_full <- fishing_effort_no_mpa %>%
    st_drop_geometry() %>%
    filter(mmsi %in% S4$mmsi) %>%
    group_by(mmsi,vessel_name,clean_length,flag_registry) %>%
    reframe(sum_hours= sum(apparent_fishing_hours)) %>%
    ungroup() %>%
    mutate(type = "outside_mpa") %>%
    dplyr::select(vessel_name, sum_hours, type)
  
  bilan <- S4 %>% 
    left_join(time_spent_full, by = "vessel_name") %>%
    mutate(percentage = round((sum_hours.x/(sum_hours.y+sum_hours.x))*100,2)) %>%
    mutate(country = countrycode(country,origin="iso3c",destination="country.name.fr")) %>%
    dplyr::select(mmsi, country, vessel_name, length, sum_hours.y, sum_hours.x, percentage) %>%
    arrange(-percentage)
  
  write.csv(bilan, "figures/navire_usine.csv")
  
  (full_plot <- time_spent_full %>%
      dplyr::select(vessel_name, sum_hours, type) %>%
      bind_rows(S4 %>% dplyr::select(vessel_name, sum_hours, type)) %>%
    ggplot(aes(reorder(vessel_name,sum_hours),sum_hours,fill=type))+
      scale_fill_manual(values = c(outside_mpa = "#4477AA",inside_mpa= "#DDCC77"),labels = c(outside_mpa = "Outside MPAs",inside_mpa = "Inside MPAs")) + 
    geom_bar(stat="identity") +
    coord_flip()+
      theme_minimal(base_size = 16, element_text(family = "National"))+
      theme(legend.position = "bottom") +
      labs(x = " ",
           y= "Fishing hours in 2023",
           fill = "Fishing:"))
  
  ggsave(full_plot, file = "figures/English/Figure_6.jpg", width = 297, height = 210, units = "mm", dpi = 600)
  
  #Graphe
  mega_trawlers_map_data <- fishing_effort_mpa_country %>%
    filter(mmsi %in% S4$mmsi)
  
  mega_trawlers_map <- ggplot() +
    geom_sf(data = europe_clean, fill = "lightgrey") + 
    geom_sf(data = mpa_eu_wdpa, fill = "lightblue", alpha = 0.7) +
    geom_sf(data = mega_trawlers_map_data) +
    # scale_color_hp_d(option = "Ravenclaw") + 
    xlim(-16,6) +
    ylim(44,55) +
    theme_map(base_size = 14) +
    # # guides(fill=guide_legend(nrow=4,byrow=TRUE)) +
    # labs(color = "Nom du navire") +
    # theme(legend.position = "bottom") + 
    ggspatial::annotation_scale(
      location = "tr",
      bar_cols = c("grey60", "white")
    ) +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
      style = ggspatial::north_arrow_nautical(
        fill = c("grey40", "white"),
        line_col = "grey20"
      )
    )
  
  ggsave(mega_trawlers_map, file = "figures/mega_trawlers_map.jpg", width = 297, height = 210, units = "mm", dpi = 600)
  
  #Good countires
  good_countries <- fishing_effort_mpa_country %>%
    st_drop_geometry() %>%
    left_join(aire_amp, by = "iso3") %>%
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name.fr"))  %>%
    group_by(country) %>%
    mutate(sum_fishing = sum(apparent_fishing_hours),
           intensity_fishing = sum_fishing/area,
           number_of_boats = length(unique(mmsi)),
           mean_length = mean(length_m_registry)) %>%
    ungroup() %>%
    distinct(country, .keep_all = T) %>%
    arrange(-area,sum_fishing)
  
  #Top 10 des plus grandes AMPs
  (S5 <- mpa_eu_wdpa %>% 
    st_drop_geometry() %>%
    arrange(-gis_m_area) %>%
    distinct(id, .keep_all = T) %>%
    distinct(name, .keep_all = T) %>%
    head(10) %>%
    ggplot() +
    geom_bar(aes(reorder(name,gis_m_area), gis_m_area), stat="identity",fill="#01C9BA") +
    coord_flip() +
    theme_minimal(base_size = 14, element_text(family = "National")) + 
    labs(x = " ",
         y = "MPA size (km²)"))
  
  ggsave(S5, file = "figures/English/Annexe_5.jpg", width = 297, height = 210, units = "mm", dpi = 300) 

  
  }
