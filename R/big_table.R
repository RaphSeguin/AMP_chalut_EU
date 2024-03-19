big_table <- function() {
  
  #Liste des pays
  pays <- fishing_effort_mpa_country %>% 
    st_drop_geometry() %>%
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name.fr")) %>%
    distinct(country) %>%
    na.omit()
  
  #Nom de l'AMP
  nombre_amp <- mpa_eu_wdpa %>%
    st_drop_geometry() %>%
    group_by(parent_iso) %>%
    reframe(sum_mpas = n()) %>%
    ungroup() %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name.fr")) %>%
    dplyr::select(-parent_iso) %>%
    na.omit()
  
  #Surface couverte par les AMP
  surface_couverte <- aire_amp %>%
    mutate(area = round(as.numeric(area))) %>%
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name.fr")) %>%
    dplyr::select(-iso3) %>%
    na.omit()
  
  #Pourcentage protégé
  eez_EU_non_spatial <- eez_EU %>% st_drop_geometry() %>% dplyr::select(iso3, AREA_KM2)
  
  pourcentage_protege <- aire_amp %>%
    mutate(area = as.numeric(area)) %>%
    na.omit() %>%
    left_join(eez_EU_non_spatial, by = "iso3") %>%
    na.omit() %>%
    mutate(pourcentage_protege = round((area/ AREA_KM2)*100,2)) %>%
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name.fr")) %>%
    dplyr::select(country, pourcentage_protege)
  
  #Nombre de bateaux détectés
  nb_bateaux <- fishing_effort_clean %>%
    mutate(vessel_class_registry = sub("\\|.*", "", vessel_class_registry)) %>%
    dplyr::filter(vessel_class_registry == "trawlers" | gear %in% c("Single boat bottom otter trawls","Bottom trawls nephrops trawls","Single boat midwater otter trawls",
                                                                    "Beam trawls","Bottom pair trawls","Twin bottom otter trawls","Midwater pair trawls","Bottom trawls (nei)",
                                                                    "Otter trawls (nei)")) %>%
    group_by(iso3) %>%
    reframe(nb_bateaux = n_distinct(mmsi)) %>%
    ungroup() %>%
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name.fr")) %>%
    dplyr::select(-iso3) %>%
    na.omit()
  
  #Nombre de bateaux détectés dans les AMPs
  nb_bateaux_AMP <- fishing_effort_mpa_country %>%
    st_drop_geometry() %>%
    group_by(iso3) %>%
    reframe(nb_bateaux_amp = n_distinct(mmsi)) %>%
    ungroup() %>%
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name.fr")) %>%
    dplyr::select(-iso3) %>%
    na.omit()
  
  #Effort de pêche total au chalut
  heure_chalut <- fishing_effort_clean %>%
    mutate(vessel_class_registry = sub("\\|.*", "", vessel_class_registry)) %>%
    dplyr::filter(vessel_class_registry == "trawlers" | gear %in% c("Single boat bottom otter trawls","Bottom trawls nephrops trawls","Single boat midwater otter trawls",
                                                                    "Beam trawls","Bottom pair trawls","Twin bottom otter trawls","Midwater pair trawls","Bottom trawls (nei)",
                                                                    "Otter trawls (nei)")) %>%
    group_by(iso3) %>%
    reframe(heure_chalut = format(round(sum(apparent_fishing_hours)),big.mark=" ")) %>%
    ungroup() %>%
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name.fr")) %>%
    dplyr::select(-iso3) %>%
    na.omit()
  
  #Effort de pêche total au chalut
  heure_chalut_amp <- fishing_effort_mpa_country %>%
    group_by(iso3) %>%
    reframe(heure_chalut_amp = round(sum(apparent_fishing_hours))) %>%
    ungroup() %>%
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name.fr")) %>%
    dplyr::select(-iso3) %>%
    na.omit()
  
  #Pourcentage du chalut
  pourcentage_chalut <- fishing_effort_mpa_country %>%
    group_by(iso3) %>%
    reframe(heure_chalut_amp = sum(apparent_fishing_hours)) %>%
    ungroup() %>%
    left_join(eez_fishing, by = "iso3") %>%
    mutate(pourcentage_chalut = round((heure_chalut_amp/(sum_fishing_country+heure_chalut_amp))*100)) %>%
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name.fr")) %>% 
    dplyr::select(country, pourcentage_chalut)
  
  #Intensité moyenne du chalutage hors amp
  intensite_hors_amp <- eez_fishing %>%
    dplyr::select(iso3, relative_fishing_country) %>%
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name.fr")) %>% 
    mutate(relative_fishing_country = round(as.numeric(relative_fishing_country),2)) %>%
    dplyr::select(country, relative_fishing_country)
  
  #Intensite peche dans les AMPs
  intensite_amp <- fishing_effort_mpa_country %>%
    st_drop_geometry() %>%
    left_join(aire_amp, by = "iso3") %>%
    group_by(iso3) %>%
    reframe(sum_fishing = sum(apparent_fishing_hours),
            intensity_fishing = round(as.numeric(sum_fishing/area),2)) %>%
    ungroup() %>%
    distinct(iso3, intensity_fishing) %>%
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name.fr")) %>%  
    dplyr::select(country, intensity_fishing)
  
  #Taille moyenne des navires
  taille_moyenne <- fishing_effort_clean %>%
    mutate(vessel_class_registry = sub("\\|.*", "", vessel_class_registry)) %>%
    dplyr::filter(vessel_class_registry == "trawlers" | gear %in% c("Single boat bottom otter trawls","Bottom trawls nephrops trawls","Single boat midwater otter trawls",
                                                                    "Beam trawls","Bottom pair trawls","Twin bottom otter trawls","Midwater pair trawls","Bottom trawls (nei)",
                                                                    "Otter trawls (nei)")) %>%
    distinct(iso3, mmsi, clean_length) %>%
    group_by(iso3) %>%
    reframe(mean_length = round(mean(clean_length, na.rm=T))) %>%
    ungroup() %>%
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name.fr")) %>%  
    dplyr::select(country, mean_length)
  
  #Taille moyenne des navires
  taille_moyenne_amp <- fishing_effort_mpa_country %>%
    st_drop_geometry() %>%
    mutate(vessel_class_registry = sub("\\|.*", "", vessel_class_registry)) %>%
    dplyr::filter(vessel_class_registry == "trawlers" | gear %in% c("Single boat bottom otter trawls","Bottom trawls nephrops trawls","Single boat midwater otter trawls",
                                                                    "Beam trawls","Bottom pair trawls","Twin bottom otter trawls","Midwater pair trawls","Bottom trawls (nei)",
                                                                    "Otter trawls (nei)")) %>%
    distinct(iso3, mmsi, clean_length) %>%
    group_by(iso3) %>%
    reframe(mean_length_amp = round(mean(clean_length, na.rm=T))) %>%
    ungroup() %>%
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name.fr")) %>%  
    dplyr::select(country, mean_length_amp)
  
  #Final dataframe
  full_dataframe <- pays %>%
    left_join(nombre_amp, by = "country") %>%
    left_join(surface_couverte, by = "country") %>%
    left_join(pourcentage_protege, by = "country") %>%
    left_join(nb_bateaux, by = "country") %>%
    left_join(nb_bateaux_AMP, by = "country") %>%
    left_join(heure_chalut, by = "country") %>%
    left_join(heure_chalut_amp, by = "country") %>%
    left_join(pourcentage_chalut, by = "country") %>%
    left_join(intensite_hors_amp, by = "country") %>%
    left_join(intensite_amp, by = "country") %>%
    left_join(taille_moyenne, by = "country") %>%
    left_join(taille_moyenne_amp, by = "country") %>%
    arrange(-heure_chalut_amp) %>%
    mutate(heure_chalut_amp = format(heure_chalut_amp, big.mark = " "))
  
  write.csv(full_dataframe, "Figures/full_dataframe.csv")

}