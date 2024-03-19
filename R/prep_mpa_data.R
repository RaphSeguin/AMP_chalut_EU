prep_mpa_data <- function(){
  
  ### Load MPAs from World Database on Protected Areas (WDPA)
  #Download data source here 
  #https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA
  mpa_wdpa <- bind_rows(st_read(dsn = "data/shapefiles/WDPA_Oct2023/WDPA_Oct2023_Public_shp_0",
                                layer = "WDPA_Oct2023_Public_shp-polygons",
                                quiet = TRUE),
                        st_read(dsn =
                                  "data/shapefiles/WDPA_Oct2023/WDPA_Oct2023_Public_shp_1",
                                layer = "WDPA_Oct2023_Public_shp-polygons",
                                quiet = TRUE),
                        st_read(dsn =
                                  "data/shapefiles/WDPA_Oct2023/WDPA_Oct2023_Public_shp_2",
                                layer = "WDPA_Oct2023_Public_shp-polygons",
                                quiet = TRUE)) %>%
    clean_names() %>%
    dplyr::filter(marine %in% c(1,2),
                  !status_yr == 0) %>%
    dplyr::select(id= wdpaid, name, desig_eng, iucn_cat, status_yr, gov_type, own_type, mang_auth, mang_plan, iso3, parent_iso, gis_m_area,marine,status) %>%
    dplyr::mutate(source = "wdpa",
                  marine = as.factor(marine))
  
  save(mpa_wdpa, file = "output/mpa_total_wdpa.Rdata")
  
  # Save EU mpas
  mpa_eu_wdpa_eu_countries<- mpa_wdpa %>%
    filter(iso3 %in% c("DEU","AUT","BEL","BGR","CYP","HRV","DNK","ESP","EST","FIN","FRA","FRA;ITA;MCO","GRC","HUN","IRL","ITA",
                       "LVA","LTU","LUX","MLT","NLD","POL","PRT","CZE","ROU","SVK","SVN","SWE"))
  
  # #For EU MPAs that have a terrestrial component, keep only marine part
  mpa_eu_wdpa_marine <- mpa_eu_wdpa_eu_countries %>%
    filter(marine == 1) %>%
    st_make_valid() %>%
    st_intersection(eez_EU) %>%
    dplyr::select(all_of(colnames(mpa_eu_wdpa_eu_countries)))
  
  # #Final WDPA EU database
  mpa_eu_wdpa <- mpa_eu_wdpa_eu_countries %>%
    filter(marine != 1) %>%
    rbind(mpa_eu_wdpa_marine)
  #
  save(mpa_eu_wdpa, file = "output/mpa_eu_clean.Rdata")
  

}