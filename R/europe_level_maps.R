europe_level_maps <- function(fishing_effort_mpa){
  
  #Shapefile of mpas
  shapefile_mpa <- mpa_eu_wdpa %>% dplyr::select(id)
  
  europe_clean <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf', continent="Europe") %>%
    mutate(europe = ifelse(adm0_a3 %in% eez_fishing$iso3, "Europe","Not Europe"))
  
  #Europe map clean
  # europe_clean <- europe %>% filter(LEVL_CODE == 0)
  
  #Total hours of fishing per MPA
  fished_mpas <- fishing_effort_mpa %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    mutate(fishing_in_mpa = sum(apparent_fishing_hours),
           fishing_intensity = fishing_in_mpa/gis_m_area) %>%
    ungroup() %>%
    #ONLY IF FISHING IS HIGHER THAN  5
    filter(fishing_in_mpa > 5) %>%
    distinct(id, .keep_all = T) %>%
    full_join(shapefile_mpa, by = "id") %>%
    st_as_sf()
  
  #Presence absence
  presence_absence <- fished_mpas %>%
    mutate(fishing_presence = ifelse(is.na(fishing_in_mpa), "Absence","Présence")) %>%
    mutate(fishing_presence = as.factor(fishing_presence)) %>%
    ggplot() + 
    geom_sf(data = europe_clean, aes(fill = europe)) +
    guides(fill = "none") +
    scale_fill_manual(values = c("Europe"="darkgrey","Not Europe","lightgrey")) +
    new_scale_fill() + 
    geom_sf(aes(fill = fishing_presence)) +
    scale_fill_manual(values = c("Présence" = "red","Absence" = "lightblue")) +
    xlim(-19,35) +
    ylim(32,70) +
    labs(fill = "Présence de la pêche industrielle") +
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
    ) +
   theme_map(base_size = 14, element_text(family = "National")) +
    theme(legend.position = "bottom") 
    
  ggsave(presence_absence, file = "figures/Figure_1.pdf",width = 297, height = 210, units = "mm", dpi = 600)
  ggsave(presence_absence, file = "figures/Figure_1.jpg",width = 297, height = 210, units = "mm", dpi = 600)
  
  #Map of Europe fishing effort
  europe_fishing_effort <- 
    ggplot()+
    geom_sf(data = europe_clean, aes(fill = europe)) +
    guides(fill = "none") +
    scale_fill_manual(values = c("Europe"="darkgrey","Not Europe","lightgrey")) +
    new_scale_fill() + 
    geom_sf(data = fished_mpas %>% filter(fishing_in_mpa > 0), aes(fill = log10(fishing_in_mpa+1))) +
    scale_fill_viridis_c(option = "rocket", direction = -1, begin = 0.3, end = 1) + 
    theme_map() + 
    xlim(-19,35) +
    ylim(32,70) +
    labs(fill = "Effort de pêche (échelle logarithmique)") +
    
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
    ) +
    theme_map(base_size = 14, element_text(family = "National")) +
    theme(legend.position = "bottom") 
  
  ggsave(europe_fishing_effort, file = "figures/Figure_2.pdf",width = 297, height = 210, units = "mm", dpi = 600)
  ggsave(europe_fishing_effort, file = "figures/Figure_2.jpg",width = 297, height = 210, units = "mm", dpi = 600)
  
  #Map of Europe fishing intensity
  europe_fishing_intensity <- 
    ggplot()+
    geom_sf(data = europe_clean, aes(fill = europe)) +
    guides(fill = "none") +
    scale_fill_manual(values = c("Europe"="darkgrey","Not Europe","lightgrey")) +
    new_scale_fill() + 
    geom_sf(data = fished_mpas %>% filter(fishing_intensity > 0), aes(fill = log10(fishing_intensity+1))) +
    scale_fill_viridis_c(option = "rocket", direction = -1, begin = 0.3, end = 1) + 
    theme_map() + 
    xlim(-19,35) +
    ylim(32,70) +
    labs(fill = "Intensité de pêche (échelle logarithmique)") +
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
    )  +
    theme_map(base_size = 14, element_text(family = "National")) +
    theme(legend.position = "bottom") 
  
  ggsave(europe_fishing_intensity, file = "figures/Figure_3.pdf",width = 300, height = 210, units = "mm", dpi = 600)
  ggsave(europe_fishing_intensity, file = "figures/Figure_3.jpg",width = 297, height = 210, units = "mm", dpi = 600)
  
  #Country color maps
  
  #Calculating total MPA area for each country
  aire_amp <- WDPA_database_union %>%
    mutate(area = st_area(WDPA_database_union),
           area = set_units(area, km^2)) %>%
    st_drop_geometry()
  
  #Calculating fishing effort and intensity in MPAs for each country
  pays_peche <- fishing_effort_mpa_country %>%
    st_drop_geometry() %>%
    full_join(aire_amp, by = "iso3") %>%
    group_by(iso3) %>%
    mutate(sum_fishing = sum(apparent_fishing_hours),
           intensity_fishing = sum_fishing/area) %>%
    ungroup() %>%
    distinct(iso3, .keep_all = T) %>%
    dplyr::select(iso3, sum_fishing,intensity_fishing) %>%
    mutate(NUTS_ID = ifelse(iso3 == "BEL","BE",
                            ifelse(iso3 == "CYP","CY",
                            ifelse(iso3 == "BGR","BG",
                                   ifelse(iso3 == "DEU","DE",
                                          ifelse(iso3 == "DNK","DK",
                                                 ifelse(iso3 == "ESP","ES",
                                                        ifelse(iso3 == "EST","EE",
                                                               ifelse(iso3 == "FIN","FI",
                                                                      ifelse(iso3 == "FRA","FR",
                                                                             ifelse(iso3 == "GRC","EL",
                                                                                    ifelse(iso3 == "HRV","HR",
                                                                                           ifelse(iso3 == "IRL","IE",
                                                                                                  ifelse(iso3 == "ITA","IT",
                                                                                                         ifelse(iso3 == "LTU","LT",
                                                                                                                ifelse(iso3 == "LVA","LV",
                                                                                                                       ifelse(iso3 == "MLT","MT",
                                                                                                                             ifelse(iso3 == "NLD","NL",
                                                                                                                                    ifelse(iso3 == "POL","PL",
                                                                                                                                           ifelse(iso3 == "PRT","PT",
                                                                                                                                                  ifelse(iso3 == "ROU","RO",
                                                                                                                                                         ifelse(iso3 == "SWE","SE",
                                                                                                                                                                ifelse(iso3 == "SVN","SI",NA))))))))))))))))))))))) %>%
    full_join(europe_clean, by = "NUTS_ID") %>%
    st_as_sf()
  
  #Map of fishing effort per country
  country_fishing_effort <- 
    ggplot()+
    geom_sf(data = pays_peche, aes(fill = log10(sum_fishing+1))) +
    scale_fill_viridis_c(option = "rocket", direction = -1, begin = 0.4, end = 1) + 
    theme_map() +
    xlim(-24,36) +
    ylim(35,70) +
    labs(fill = "Effort de pêche dans les AMPs par pays (échelle logarithmique)") +
    theme(legend.position = "bottom")+
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
  
  ggsave(country_fishing_effort, file = "figures/country_fishing_effort.pdf",width = 297, height = 210, units = "mm", dpi = 600)
  ggsave(country_fishing_effort, file = "figures/country_fishing_effort.jpg",width = 297, height = 210, units = "mm", dpi = 600)

  #Map of fishing intensity per country
  country_fishing_intensity <- 
    ggplot()+
    geom_sf(data = pays_peche %>% mutate(intensity_fishing = as.numeric(intensity_fishing)), aes(fill = log10(intensity_fishing+1))) +
    scale_fill_viridis_c(option = "rocket", direction = -1, begin = 0.3, end = 1) + 
    theme_map() +
    xlim(-25,35) +
    ylim(35,70) +
    labs(fill = "Intensité de pêche dans les AMPs par pays (échelle logarithmique)")+
    theme(legend.position = "bottom")+
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
  
  
  ggsave(country_fishing_intensity, file = "figures/country_fishing_intensity.pdf",width = 297, height = 210, units = "mm", dpi = 600)
  ggsave(country_fishing_intensity, file = "figures/country_fishing_intensity.jpg",width = 297, height = 210, units = "mm", dpi = 600)
  
  #Map of gaascogne
  
  gascogne_map <- 
    ggplot()+
    geom_sf(data = europe_clean, fill = "lightgrey") +
    geom_sf(data = gascogne, fill = "red") + 
    # geom_sf(data = fished_mpas %>% filter(fishing_in_mpa > 0), aes(fill = log10(fishing_in_mpa+1))) +
    # scale_fill_viridis_c(option = "rocket", direction = -1, begin = 0.3, end = 1) + 
    theme_map() + 
    xlim(-19,35) +
    ylim(32,70) +
    labs(fill = "Effort de pêche (échelle logarithmique)") +
    theme(legend.position = "bottom") +
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
  
  ggsave(gascogne_map , file ="figures/gascogne_map.jpg", width = 297, height = 210, units ="mm", dpi = 600)
  
  
  #Fishing effort of trawlers in france
  
  uk_french_trawlers <- fishing_effort_uk %>%
    filter(flag_registry == "FRA" | country == "FRA") 
  
  french_trawlers <-  fishing_effort_clean %>%
    mutate(vessel_class_registry = sub("\\|.*", "", vessel_class_registry)) %>%
    dplyr::filter(vessel_class_registry == "trawlers" | gear %in% c("Single boat bottom otter trawls","Bottom trawls nephrops trawls","Single boat midwater otter trawls",
                                                                    "Beam trawls","Bottom pair trawls","Twin bottom otter trawls","Midwater pair trawls","Bottom trawls (nei)",
                                                                    "Otter trawls (nei)")) %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
    dplyr::select(all_of(colnames(fishing_effort_uk))) %>%
    rbind(fishing_effort_uk)
  
  plot_europe_france_trawlers <-  
    ggplot()+
    geom_sf(data = europe_clean, aes(fill = europe)) +
    guides(fill = "none") +
    scale_fill_manual(values = c("Europe"="darkgrey","Not Europe","lightgrey")) +
    new_scale_fill() + 
    geom_sf(data = mpa_eu_wdpa, fill = "lightblue") +
    geom_sf(data = french_trawlers, shape = ".", alpha = 0.2) +
    # geom_sf(data = uk_french_trawlers, shape = ".", alpha = 0.5) +
    # geom_sf(data = fished_mpas %>% filter(fishing_in_mpa > 0), aes(fill = log10(fishing_in_mpa+1))) +
    scale_color_viridis_c(option = "rocket", direction = -1, begin = 0.3, end = 1) +
    # xlim(-19,10) +
    # ylim(42,61) +
    xlim(-25,35) +
    ylim(35,70) +
    labs(fill = "Effort de pêche au chalut") +
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
    ) +
    theme_map(base_size = 14, element_text(family = "National")) +
    theme(legend.position = "bottom") 
  
  ggsave(plot_europe_france_trawlers, file = "figures/europe_mpas_fishing.jpg", 
         width = 297, height = 210, units = "mm", dpi= 600)
  
  
}
  
