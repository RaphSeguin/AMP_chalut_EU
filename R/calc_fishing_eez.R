calc_fishing_eez <- function(fishing_effort_clean){
  
  #Union of all MPAs
  all_mpas_eu = WDPA_database_union %>% st_union()
  
  #EEZ without MPAs
  eez_clean <- st_difference(eez_EU, all_mpas_eu)
  
  unprotected_area_eez <- eez_clean %>%
    mutate(area= set_units(st_area(eez_clean), km^2))

  sum(unprotected_area_eez$area)
  
  #Fishing data outside of MPAs
  eez_clean_sf <- st_as_sfc(eez_clean)
  fishing_effort_no_mpa <- fishing_effort_clean %>%
    mutate(vessel_class_registry = sub("\\|.*", "", vessel_class_registry)) %>%
    dplyr::filter(vessel_class_registry == "trawlers" | gear %in% c("Single boat bottom otter trawls","Bottom trawls nephrops trawls","Single boat midwater otter trawls",
                                                                    "Beam trawls","Bottom pair trawls","Twin bottom otter trawls","Midwater pair trawls","Bottom trawls (nei)",
                                                                    "Otter trawls (nei)")) %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
    st_transform(crs = st_crs(eez_clean)) 
  
  fishing_effort_no_mpa <- st_intersection(fishing_effort_no_mpa, eez_clean_sf)
  
  save(fishing_effort_no_mpa,  file = "output/fishing_effort_no_mpa.Rdata")
  
  #Area
  eez_clean <- eez_clean %>%
    mutate(area_no_mpa = st_area(eez_clean),
           area_no_mpa = set_units(area_no_mpa, km^2)) %>%
    st_drop_geometry() %>%
    dplyr::select(iso3, area_no_mpa)

  #Calculating mean fishing effort outside of MPAs in EEZs without MPAs
  eez_fishing <- fishing_effort_no_mpa %>%
  dplyr::select(-c(iso3.x,iso3.y)) %>%
  #Associating iso3 name to each country
  mutate(iso3 = ifelse(name == "Belgian_2023","BEL",
                       ifelse(name == "Portugese_azores_2023","PRT",
                              ifelse(name == "Portugese_madeira_2023", "PRT",
                                     ifelse(name == "Spain_canary_2023", "ESP",
                                            ifelse(name == "Slovenia_2023","SVN",
                        ifelse(name == "Bulgarian_2023","BGR",
                              ifelse(name == "Croatian_2023","HRV",
                                     ifelse(name == "Cypriote_2023","CYP",
                                            ifelse(name == "Danish_2023","DNK",
                                                   ifelse(name == "Dutch_2023","NLD",
                                                          ifelse(name == "Estonian_2023","EST",
                                                                 ifelse(name == "Finland_2023","FIN",
                                                                        ifelse(name == "France_2023","FRA",
                                                                               ifelse(name == "Gerrman_2023","DEU",
                                                                                      ifelse(name == "Greek_2023","GRC",
                                                                                             ifelse(name == "Irish_2023","IRL",
                                                                                                    ifelse(name == "Italia_2023","ITA",
                                                                                                           ifelse(name == "Latvia_2023","LVA",
                                                                                                                  ifelse(name == "Lithuania_2023","LTU",
                                                                                                                         ifelse(name == "Maltese_2023","MLT",
                                                                                                                                ifelse(name == "Polish_2023","POL",
                                                                                                                                       ifelse(name == "Portugese_2023","PRT",
                                                                                                                                              ifelse(name == "Romania_2023","ROU",
                                                                                                                                                     ifelse(name == "Spain_2023","ESP",
                                                                                                                                                            ifelse(name == "Sweden_2023","SWE","NA")))))))))))))))))))))))))) %>%
  left_join(eez_clean, by = "iso3") %>%
  #Calculating fishing in each EEZ
  group_by(iso3) %>%
  mutate(sum_fishing_country = sum(apparent_fishing_hours),
         relative_fishing_country = sum_fishing_country/area_no_mpa) %>%
  ungroup() %>%
  distinct(iso3,.keep_all = T) %>%
  dplyr::select(iso3, sum_fishing_country, relative_fishing_country)
  
  save(eez_fishing, file = "output/eez_fishing.Rdata")
  
  return(eez_fishing)

}
