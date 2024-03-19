fiche_par_pays <- function(iso_number){
  
  #Europe map
  europe_clean <- europe %>%
    filter(LEVL_CODE == 0) %>%
    mutate(iso3 =  ifelse(NUTS_ID == "BE","BEL",
                          ifelse(NUTS_ID == "CY","CYP",
                                 ifelse(NUTS_ID == "BG","BGR",
                                        ifelse(NUTS_ID == "DE","DEU",
                                               ifelse(NUTS_ID == "DK","DNK",
                                                      ifelse(NUTS_ID == "ES","ESP",
                                                             ifelse(NUTS_ID == "EE","EST",
                                                                    ifelse(NUTS_ID == "FI","FIN",
                                                                           ifelse(NUTS_ID == "FR","FRA",
                                                                                  ifelse(NUTS_ID == "EL","GRC",
                                                                                         ifelse(NUTS_ID == "HR","HRV",
                                                                                                ifelse(NUTS_ID == "IE","IRL",
                                                                                                       ifelse(NUTS_ID == "IT","ITA",
                                                                                                              ifelse(NUTS_ID == "LT","LTU",
                                                                                                                     ifelse(NUTS_ID == "LV","LVA",
                                                                                                                            ifelse(NUTS_ID == "MT","MLT",
                                                                                                                                   ifelse(NUTS_ID == "NL","NLD",
                                                                                                                                          ifelse(NUTS_ID == "PL","POL",
                                                                                                                                                 ifelse(NUTS_ID == "PT","PRT",
                                                                                                                                                        ifelse(NUTS_ID == "RO","ROU",
                                                                                                                                                               ifelse(NUTS_ID == "SE","SWE",
                                                                                                                                                                      ifelse(NUTS_ID == "SI","SVN",NA)))))))))))))))))))))))
  
  europe_clean <- europe_clean %>% st_crop(xmin = -19, xmax = 35, ymin = 32, ymax = 70)
  
  #Isolating country 
  temp_country <- europe_clean %>% filter(iso3 == iso_number)
  
  #Fishing fleet of country
  fishing_fleet_country <- fishing_fleet_complete %>% 
    filter(country == iso_number) %>%
    filter(gear %in% c("Single boat bottom otter trawls","Bottom trawls nephrops trawls","Single boat midwater otter trawls",
                                 "Beam trawls","Bottom pair trawls","Twin bottom otter trawls","Midwater pair trawls","Bottom trawls (nei)",
                                 "Otter trawls (nei)"))
  
  #EEZ 
  eez_country <- eez_EU %>% filter(iso3 == iso_number)
  
  #MPA union
  mpa_union_country <- WDPA_database_union %>% filter(iso3 == iso_number)
  
  # #Carte de situation
  # situation_map <- ggplot() + 
  #   geom_sf(data = europe_clean, fill = "lightgrey") + 
  #   geom_sf(data = temp_country, fill = "red") +
  #   xlim(-19,35) +
  #   ylim(32,70) +
  #   theme_map() +
  #   theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5)) +
  #   coord_sf(expand =)
  # 
  #fishing in country
  fished_contry <- fishing_effort_clean %>%
    mutate(vessel_class_registry = sub("\\|.*", "", vessel_class_registry)) %>%
    dplyr::filter(vessel_class_registry == "trawlers" | gear %in% c("Single boat bottom otter trawls","Bottom trawls nephrops trawls","Single boat midwater otter trawls",
                                                                    "Beam trawls","Bottom pair trawls","Twin bottom otter trawls","Midwater pair trawls","Bottom trawls (nei)",
                                                                    "Otter trawls (nei)")) %>%
    filter(iso3 == iso_number) %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326)
  
  #mpa_country
  mpa_country <- mpa_eu_wdpa %>% filter(iso3 == iso_number)
  # 
  # map_country <- ggplot() + 
  #   geom_sf(data = mpa_country, fill = "lightblue", alpha = 0.5) +
  #   geom_sf(data = fished_contry, shape = ".", alpha = 0.2) +
  #   geom_sf(data = europe_clean %>% filter(iso3 == iso_number), fill = "lightgrey") +
  #   theme_void() +
  #   theme(
  #     text = element_text(family = "Futura-Medium"),
  #     legend.title = element_text(family = "Futura-Bold", size = 10),
  #     legend.text = element_text(family = "Futura-Medium", size = 10)
  #   ) + 
  #   coord_sf(expand = F)
  # 
  # # ggsave(map_country, file = "figures/map_country.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  
  #Full map of situational map + country
  # full_map <- map_country + 
  #   theme(panel.border = element_rect(colour = "black", fill=NA, linewidth= 0.5)) +
  #   inset_element(situation_map, left = 0,
  #                 bottom = 0,
  #                 right =0.3,
  #                 top = 0.3, align_to = "plot") 
  # 
  # ggsave(full_map, file = paste0("figures/fiche_pays_figures/full_map",iso_number,".jpg"), width = 297, height = 210, units = "mm", dpi = 300)
  
  # Text containing information on the country
  mpa_number <- paste("Nombre d'AMP :",format(nrow(mpa_country),big.mark = " "))
  mpa_surface <- paste("Surface des AMP :",format((round(sum(mpa_country$gis_m_area,na.rm=T),0)), big.mark = " "),"km²")
  print(paste("Nombre total de bateaux",length(unique(fished_contry$mmsi))))
  print(paste("Nombre total de bateaux",length(unique((fishing_effort_mpa_country %>% filter(iso3 == iso_number))$mmsi))))
  print(paste("Heure de peche total",sum(fished_contry$apparent_fishing_hours)))
  print(paste("Heure de pêche en AMP",sum((fishing_effort_mpa_country %>% filter(iso3 == iso_number))$apparent_fishing_hours)))
  # print(paste("Pourcentage en AMP", ((sum((fishing_effort_mpa_country %>% filter(iso3 == iso_number))$apparent_fishing_hours))/(sum(fished_contry$apparent_fishing_hours)))*100))
  paste0("       ",round((sum((fishing_effort_mpa_country %>% filter(iso3 == iso_number))$apparent_fishing_hours))/(((eez_fishing %>% filter(iso3 == iso_number))$sum_fishing_country)+(sum((fishing_effort_mpa_country %>% filter(iso3 == iso_number))$apparent_fishing_hours)))*100,2),"%")

  # 

  nombre_bateau <- paste("Dans ce pays,",format(length(unique(fished_contry$mmsi)),big.mark = " "),"chalutiers ont été détectés, dont",round((length(unique((fished_contry %>% filter(flag_registry == iso_number))$mmsi)))/(length(unique(fished_contry$mmsi))) * 100,2),
                         "% sous le pavillon du pays, soit",round((nrow(fished_contry %>% st_drop_geometry() %>% distinct(mmsi) %>% filter(mmsi %in% fishing_fleet_country$mmsi))/(nrow(fishing_fleet_country)))*100,2),"% de la flotte toute taille confondue.")
  pourcentage_taille <- paste("Dans ce pays,",round((nrow(fishing_fleet_country %>% filter(length > 15)))/(nrow(fishing_fleet_country))*100,2),"% des chalutiers font plus de 15 mètres.")
  # 
  # df <- data.frame(x = 0, y = 1, label = paste(mpa_number,mpa_surface,nombre_bateau,pourcentage_taille,sep = "<br>"))
  # 
  # # text.p <- ggparagraph(text = text_plot, size = 12, color = "black",family = "Futura-Medium")
  # 
  # test_plot <- ggplot(df) +
  #   ggtext::geom_textbox(aes(x = x, y = y, label = label), box.colour = NA, width = unit(27, "cm"),family = "National",size=9) +
  #   theme_void() 
  #   # theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=2))
  # 
  # ggsave(test_plot, file = paste0("figures/fiche_pays_figures/text_plot_",iso_number,".jpg"), width = 297, height = 100, dpi = 300, units = "mm")
  # 
  #Plotting both things together
  
  #Top 3 AMP en chalutage
  fished_country_mpa <- fishing_effort_mpa %>%
    st_drop_geometry() %>%
    filter(iso3 == iso_number) %>%
    group_by(id) %>%
    mutate(fishing_in_mpa = sum(apparent_fishing_hours)) %>%
    ungroup() %>%
    distinct(id, name.1,fishing_in_mpa, .keep_all = T) %>%
    distinct(name.1,.keep_all = T) %>%
    arrange(-fishing_in_mpa) %>%
    head(3)
  
  top_3_peche <- ggplot(fished_country_mpa) +
    geom_bar(aes(reorder(name.1,fishing_in_mpa),fishing_in_mpa,fill=name.1), stat="identity") +
    scale_fill_manual(values = c("#0173C2","#EFC000","#868686"),guide = guide_legend(reverse = TRUE)) +
    coord_flip() +
    theme_minimal(base_size = 25) +
    labs(x = " ",
         y = "Number of trawling hours",
         fill = " ") +
    theme(legend.position = "bottom",
          axis.title.y=element_blank(),
          axis.text.y =element_blank(),
          axis.ticks.y=element_blank()) +
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth= 0.5)) +
    guides(fill=guide_legend(nrow=3,byrow=TRUE))
  
  ggsave(top_3_peche, file = paste0("figures/English/fiche_pays/top_3_peche",iso_number,".jpg"), width = 297, height = 210, units = "mm",dpi=600)
  
  #Top 3 AMP en chalutage intensité
  fished_country_mpa_intensity <- fishing_effort_mpa %>%
    st_drop_geometry() %>%
    filter(iso3 == iso_number) %>%
    group_by(id) %>%
    mutate(fishing_in_mpa = sum(apparent_fishing_hours)/gis_m_area) %>%
    ungroup() %>%
    distinct(id, name.1,fishing_in_mpa, .keep_all = T) %>%
    distinct(name.1,.keep_all = T) %>%
    arrange(-fishing_in_mpa) %>%
    head(3)
  
  top_3_peche_intensity <- ggplot(fished_country_mpa_intensity) +
    geom_bar(aes(reorder(name.1,fishing_in_mpa),fishing_in_mpa,fill=name.1), stat="identity") +
    scale_fill_manual(values = c("#0173C2","#EFC000","#868686")) +
    coord_flip() +
    coord_flip() +
    theme_minimal(base_size = 25) +
    labs(x = " ",
         y = "Trawling intensity (hours/km²)",
         fill = " ") +
    theme(legend.position = "bottom",
          axis.title.y=element_blank(),
          axis.text.y =element_blank(),
          axis.ticks.y=element_blank()) +
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth= 0.5)) +
    guides(fill=guide_legend(nrow=3,byrow=TRUE))
  
  
  ggsave(top_3_peche_intensity, file = paste0("figures/English/fiche_pays/top_3_peche_intensity",iso_number,".jpg"), width = 297, height = 210, units = "mm",dpi=600)
  
  # #Create text
  nombre_hr_chalut <- format((round(sum((fishing_effort_mpa_country %>% filter(iso3 == iso_number))$apparent_fishing_hours),0)),big.mark = " ")

  df <- data.frame(
    x = 1, y = 1,
    label = paste(
      "<span style='color: #092C3B; font-size: 100pt'>",nombre_hr_chalut,"</span>",
      "<span style='color: #53A5B3; font-size: 20pt'>              heures de chalutage dans les AMPs</span>", sep = "<br>"))

  number_1 <- ggplot(df) +
    ggtext::geom_textbox(aes(x = x, y = y, label = label), box.colour = NA, width = unit(12, "cm"),family = "National") +
    theme_nothing()

  ggsave(number_1, file = paste0("figures/fiche_pays_figures/number_1",iso_number,".svg"), width = 5, height = 3)

  #Percentage EEZ
  percentage_eez <- paste0(format(round((as.numeric(set_units(st_area(mpa_union_country),km^2))/eez_country$AREA_KM2)*100,1),big.mark = " "), "%")

  df <- data.frame(
    x = 1, y = 1,
    label = paste(
      "<span style='color: #092C3B; font-size: 100pt'>",percentage_eez,"</span>",
      "<span style='color: #53A5B3; font-size: 20pt'>de la ZEE protégée</span>", sep = "<br>"))

  number_4 <- ggplot(df) +
    ggtext::geom_textbox(aes(x = x, y = y, label = label), box.colour = NA, width = unit(12, "cm"),family = "National") +
    theme_nothing()

  ggsave(number_4, file = paste0("figures/fiche_pays_figures/number_4",iso_number,".svg"), width = 5, height = 3)

  pourcentage_chalut <- paste0("       ",
                               round((sum((fishing_effort_mpa_country %>% filter(iso3 == iso_number))$apparent_fishing_hours))/
                                       (((eez_fishing %>% filter(iso3 == iso_number))$sum_fishing_country)+
                                          (sum((fishing_effort_mpa_country %>% filter(iso3 == iso_number))$apparent_fishing_hours)))*100,2),"%")

  df_2 <- data.frame(
    x = 1, y = 1,
    label = paste(
      "<span style='color: #092C3B; font-size: 100pt'>",pourcentage_chalut,"</span>",
      "<span style='color: #53A5B3; font-size: 20pt'>à l'intérieur des AMPs</span>", sep = "<br>"))

  number_2 <- ggplot(df_2) +
    ggtext::geom_textbox(aes(x = x, y = y, label = label), box.colour = NA, width = unit(12, "cm"),family = "National") +
    theme_nothing()

  ggsave(number_2, file = paste0("figures/fiche_pays_figures/number_2",iso_number,".svg"), width = 5, height = 3)

  #Calculate mean intensity

  mean_intensity <- unique((fishing_effort_mpa_country %>%
    st_drop_geometry() %>%
    filter(iso3 == iso_number) %>%
    left_join(aire_amp, by = "iso3") %>%
    mutate(sum_fishing = sum(apparent_fishing_hours),
           intensity_fishing = sum_fishing/area))$intensity_fishing)

  mean_intensity <- paste0(round(as.numeric(mean_intensity),2),"h/km²")

  df_3 <- data.frame(
    x = 1, y = 1,
    label = paste(
      "<span style='color: #092C3B; font-size: 100pt'>",mean_intensity,"</span>",
      "<span style='color: #53A5B3; font-size: 20pt'>intensité moyenne de chalutage</span>", sep = "<br>"))

  number_3 <- ggplot(df_3) +
    ggtext::geom_textbox(aes(x = x, y = y, label = label), box.colour = NA, width = unit(12, "cm"),family = "National") +
    theme_nothing()

  ggsave(number_3, file = paste0("figures/fiche_pays_figures/number_3",iso_number,".svg"), width = 5, height = 3)


}
