load_fishing_data <- function() {
  
  #Fishing fleet complete
  #Download from european fleet registry
  fishing_fleet_complete<- read_rds("data/clean_fleet_register_20230626.rds") %>%
    distinct(mmsi, .keep_all = T) 
  
  save(fishing_fleet_complete, file = "output/fishing_fleet_complete.Rdata")
  
  # #Loading fishing fleet
  fishing_fleet_full <- read_rds("data/clean_fleet_register_20230626.rds") %>%
    distinct(mmsi, gear) %>%
    distinct(mmsi, .keep_all = T) %>%
    na.omit()
  
  save(fishing_fleet_full, file = "output/fishing_fleet_full.Rdata")
  
  #Global fishing watch fishing fleet
  #Download from Global fishing watch
  fishing_fleet_gfw <- read.csv("data/fishing_vessels_v2.csv") %>%
    dplyr::select(mmsi, flag_registry, vessel_class_registry, length_m_registry, engine_power_kw_registry, tonnage_gt_registry) %>%
    dplyr::mutate(mmsi = as.factor(mmsi),
                  flag_registry = as.factor(flag_registry),
                  vessel_class_registry = as.factor(vessel_class_registry))

  save(fishing_fleet_gfw, file = "output/fishing_fleet_gfw.Rdata")

  # Loading fishing effort
  #Download from Global fishing watch
  fishing_effort <- list.files(path = "data/fishing_effort/2023",
                               pattern="*.csv",
                               full.names = T) %>%
    map_df(~read_csv(., id = "name", col_types = cols(.default = "c"))) %>%
    mutate(name = sub('.*/', '', name)) %>%
    mutate(name = sub("\\..*", "", name)) %>%
    clean_names() %>%
    mutate(mmsi = as.factor(mmsi)) %>%
    mutate(unique_id = paste0(name,time_range,mmsi,entry_timestamp, exit_timestamp,apparent_fishing_hours,lat,lon)) 
    
  
  save(fishing_effort, file = "output/fishing_effort.Rdata")
  

}