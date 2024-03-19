united_kingdom <- function(){

  #Fishing effort
  fishing_effort_uk <- read.csv("data/uk_fishing.csv") %>%
    clean_names() %>%
    mutate(mmsi = as.factor(mmsi),
           name = "Britain_2023")
  
  fishing_effort_uk <- data_clean(fishing_effort_uk) %>%
    mutate(vessel_class_registry = sub("\\|.*", "", vessel_class_registry)) %>%
    dplyr::filter(vessel_class_registry == "trawlers" | gear %in% c("Single boat bottom otter trawls","Bottom trawls nephrops trawls","Single boat midwater otter trawls",
                                                                    "Beam trawls","Bottom pair trawls","Twin bottom otter trawls","Midwater pair trawls","Bottom trawls (nei)",
                                                                    "Otter trawls (nei)")) %>%
    st_as_sf(coords = c("lon","lat"),crs=4326)
  
  #EEZ
  eez_uk <- eez %>% filter(MRGID == 5696) %>% dplyr::select(geometry)
  
  uk_mpas <- mpa_wdpa %>%
    filter(iso3 == "GBR") %>%
    clean_names() %>%
    dplyr::filter(marine %in% c(1,2),
                  !status_yr == 0) %>%
    dplyr::select(id, name, desig_eng, iucn_cat, status_yr, gov_type, own_type, mang_auth, mang_plan, iso3, parent_iso, gis_m_area,marine,status) %>%
    st_intersection(eez_uk) %>%
    st_make_valid()
  
  #Intersect MPAs with fishing effort
  uk_mpas_fishing <- st_intersection(fishing_effort_uk,uk_mpas)
  save(uk_mpas_fishing, file = "output/uk_mpas_fishing.Rdata")
  
  #Intersect with unionized MPAs 
  uk_mpas_union <- uk_mpas %>%
    reframe(geometry = st_union(geometry)) %>%
    st_as_sf()
  
  uk_mpas_fishing_country <- st_intersects(uk_mpas_union,fishing_effort_uk) %>%
    unlist() %>%
    unique() %>%
    sort() %>%
    {fishing_effort_uk[.,]}
  
  save(uk_mpas_fishing_country, file = "output/uk_mpas_fishing_country.Rdata")
  
  #Fishing in EEZ
  eez_uk_clean <- st_difference(eez_uk, uk_mpas_union) %>% st_make_valid()
  
  fishing_effort_uk_eez <- fishing_effort_uk %>%
    st_transform(crs = st_crs(eez_uk)) %>%
    st_intersection(eez_uk_clean) %>%
    st_drop_geometry()
  
  save(fishing_effort_uk_eez, file = "output/fishing_effort_uk_eez.Rdata")
  
  #Vizualizing fishing effort
  uk <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf', country = "united kingdom")
  
  #
  sum(uk_mpas_fishing_country$apparent_fishing_hours)
  
  uk_mpas_fished <- uk_mpas_fishing %>%
    dplyr::select(!all_of(colnames(eez))) %>%
    group_by(id) %>%
    mutate(fishing_in_mpa = sum(apparent_fishing_hours)) %>%
    ungroup() %>%
    distinct(id, .keep_all = T)
  
  #Dogger and stuff
  dogger_mpa <- uk_mpas_fishing %>% dplyr::filter(id %in% c(555556968)) %>% filter(vessel_name != "SCOMBRUS")
  
  sum(dogger_mpa$apparent_fishing_hours)
  
  dogger_mpa_sf <- uk_mpas %>% dplyr::filter(id %in% c(555557035, 555556968))
  
  dogger_map <- ggplot() +
    geom_sf(data = dogger_mpa_sf, fill = "lightblue" ) +
    geom_sf(data = dogger_mpa,aes(color = vessel_name))
  
  ggsave(dogger_map, file = "figures/dogger_map.jpg")
  
  st_area(uk_mpas_union)/st_area(eez_uk)
  
  
  
}