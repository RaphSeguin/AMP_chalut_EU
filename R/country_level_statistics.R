country_level_statistics <- function(fishing_effort_mpa_country){

  #Calculating fishing effort and intensity in MPAs for each country
  pays_peche <- fishing_effort_mpa_country %>%
    st_drop_geometry() %>%
    left_join(aire_amp, by = "iso3") %>%
    group_by(iso3) %>%
    mutate(sum_fishing = sum(apparent_fishing_hours),
           intensity_fishing = sum_fishing/area,
           number_of_boats = length(unique(mmsi)),
           mean_length = mean(length_m_registry)) %>%
    ungroup() %>%
    distinct(iso3, .keep_all = T) %>%
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name.fr"))
  
  #Plot of percentage of each type of fishing for each country
  pays_peche_vs_eez <- pays_peche %>%
    left_join(eez_fishing, by = "iso3") %>%
    drop_na(iso3) %>%
    dplyr::select(iso3, sum_fishing, sum_fishing_country,number_of_boats,mean_length) %>%
    group_by(iso3) %>%
    mutate(percentage = round((sum_fishing/(sum_fishing_country+sum_fishing))*100,0),) %>%
    ungroup() %>%
    pivot_longer(c(sum_fishing,sum_fishing_country)) 
  
  max_values <- pays_peche_vs_eez%>% group_by(iso3) %>% summarise(value = max(value))
  
  peche_vs_eez_plot <- ggplot(pays_peche_vs_eez, aes(reorder(iso3,-value), value, fill = name))+
    geom_bar(stat = "identity") +
    scale_fill_manual(labels = c("sum_fishing"="Pêche dans les AMPs",
                                                    "sum_fishing_country"="Pêche hors AMP"),
                      values = c("sum_fishing"="#F6C85F",
                                 "sum_fishing_country"="#2684A4")) + 
    theme_minimal(base_size = 18, element_text(family = "National")) + 
    theme(legend.position = "bottom") + 
    scale_x_discrete(labels = c("ITA"="Italie",
                                "FRA"="France",
                                "ESP"="Espagne",
                                "IRL"="Irlande",
                                "PRT"="Portugal",
                                "DNK"="Danemark",
                                "NLD"="Pays-Bas",
                                "HRV"="Croatie",
                                "DEU"="Allemagne",
                                "GRC"="Grèce",
                                "SWE"="Suède",
                                "MLT"="Malte",
                                "LVA"="Lettonie",
                                "FIN"="Finlande",
                                "BGR"="Bulgarie",
                                "BEL"="Belgique",
                                "EST"="Estonie",
                                "CYP"="Chypre",
                                "POL"="Pologne",
                                "ROU"="Roumanie",
                                "LTU"="Lituanie",
                                "SVN"="Slovénie")) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x = " ",
         y = "Nombre d'heure de pêche total dans le pays",
         fill = " ") 
  
  ggsave(peche_vs_eez_plot, file = "figures/Figure_5.jpg", width = 297, height = 210, units = "mm", dpi = 600)
  ggsave(peche_vs_eez_plot, file = "figures/Figure_5.pdf", width = 297, height = 210, units = "mm", dpi = 600)
  
  #Top 10 pays aux AMPs les plus pêchées
  pays_peche_effort <- pays_peche %>%
    arrange(-sum_fishing) %>%
    head(10)
  
  print(paste0("Pays avec les AMPs les plus pêchées: ",pays_peche_effort[1,]$name))
  paste(paste("Avec",pays_peche_effort[1,]$sum_fishing,"heures de pêche"))
  
  print(paste0("2ème avec les AMPs les plus pêchées: ",pays_peche_effort[2,]$name))
  paste(paste("Avec",pays_peche_effort[2,]$sum_fishing,"heures de pêche"))
  
  print(paste0("3ème avec les AMPs les plus pêchées: ",pays_peche_effort[3,]$name))
  paste(paste("Avec",pays_peche_effort[3,]$sum_fishing,"heures de pêche"))
  
  pays_peche_effort_plot <- ggplot(pays_peche_effort,aes(reorder(country,sum_fishing),sum_fishing))+
    geom_bar(stat='identity',aes(fill = iso3)) +
    scale_fill_manual(values=country_color_scale)+
    coord_flip() +
    theme_minimal() +
    theme(text = element_text(size = 18,family = "National")) +
    theme(legend.position = "none")  +
    labs( x = " ",
          y = "Effort de pêche dans les AMPs en 2023",
          fill = "Pays")
  
  ggsave(pays_peche_effort_plot, file = "figures/Figure_4.pdf", width = 297, height = 210, units = "mm", dpi = 600)
  ggsave(pays_peche_effort_plot, file = "figures/Figure_4.jpg", width = 297, height = 210, units = "mm", dpi = 600)
  
  #Top 10 pays aux AMPs les plus pêchées en intensité
  pays_peche_intensity <- pays_peche %>%
    arrange(-intensity_fishing) %>%
    head(10)
  
  print(paste0("Pays avec les AMPs les plus pêchées en intensité: ",pays_peche_intensity[1,]$name))
  paste(paste("Avec",pays_peche_intensity[1,]$intensity_fishing,"heures de pêche/km^2"))
  
  print(paste0("2ème avec les AMPs les plus pêchées en inténsité: ",pays_peche_intensity[2,]$name))
  paste(paste("Avec",pays_peche_intensity[2,]$intensity_fishing,"heures de pêche/km^2"))
  
  print(paste0("3ème avec les AMPs les plus pêchées en inténsité: ",pays_peche_intensity[3,]$name))
  paste(paste("Avec",pays_peche_intensity[3,]$intensity_fishing,"heures de pêche/km^2"))
  
  pays_peche_intensity_plot <- ggplot(pays_peche_intensity,aes(reorder(country,intensity_fishing),intensity_fishing))+
    geom_bar(stat='identity',aes(fill = iso3)) +
    scale_fill_manual(values=country_color_scale)+
    coord_flip() +
    theme_minimal() +
    theme(text = element_text(size = 18,family = "National")) +
    theme(legend.position = "none")  +
    labs( x = " ",
          y = "Intensité de pêche dans les AMPs en 2023",
          fill = "Pays")
  
  ggsave(pays_peche_intensity_plot, file = "figures/pays_peche_intensity_plot.pdf", width = 297, height = 210, units = "mm", dpi = 600)
  ggsave(pays_peche_intensity_plot, file = "figures/pays_peche_intensity_plot.jpg", width = 297, height = 210, units = "mm", dpi = 600)
  
  full_plot_pays <- ggarrange(pays_peche_effort_plot,pays_peche_intensity_plot,ncol = 2)
  
  # ggsave(full_plot_pays, file = "figures/full_plot_pays.pd
  ggsave(full_plot_pays, file = "figures/full_plot_pays.pdf", width = 320, height = 210, units = "mm", dpi = 600)
  ggsave(full_plot_pays, file = "figures/full_plot_pays.jpg", width = 320, height = 210, units = "mm", dpi = 600)
    
  #Now looking at details
  detailed_gear <- fishing_effort_mpa_country %>%
    #Joining with fleet
    # filter(vessel_class_registry == "trawlers") %>%
    dplyr::select(mmsi, apparent_fishing_hours) %>%
    left_join(fishing_fleet_full, by = "mmsi") %>%
    #Removing NAs
    group_by(gear) %>%
    reframe(detailed_gear_fishing = sum(apparent_fishing_hours)) %>%
    ungroup() %>%
    na.omit()
  
  print(paste(
    (detailed_gear %>% filter(gear=="Single boat bottom otter trawls"))$detailed_gear_fishing/sum(detailed_gear$detailed_gear_fishing) * 100))

  fishing_gear_detailed_plot <- ggplot(detailed_gear,aes(reorder(gear,detailed_gear_fishing), detailed_gear_fishing,fill=gear)) +
    geom_bar(stat='identity') +
    theme_minimal() + 
    theme(legend.position = "none") +
    scale_fill_viridis_d() + 
    labs(x = " ",
         y = "Nombre d'heure de pêche",
         fill = "Méthode") + 
    coord_flip() 
  
  
  ggsave(fishing_gear_detailed_plot, file = "figures/fishing_gear_detailed_plot.jpg", width = 297, height = 210, units = "mm", dpi = 600)
  


}
