load_intersections_union <- function(){
  
  #Load intersections union
  path = (here::here("output/intersections_union"))
  setwd(path)
  fishing_effort_mpa_country <- list.files(pattern = ".Rdata") %>%
    map_df(~ get(load(file = .x)))%>%
    mutate(vessel_class_registry = sub("\\|.*", "", vessel_class_registry)) %>%
    filter(vessel_class_registry == "trawlers" | gear %in% c("Single boat bottom otter trawls","Bottom trawls nephrops trawls","Single boat midwater otter trawls",
                                                             "Beam trawls","Bottom pair trawls","Twin bottom otter trawls","Midwater pair trawls","Bottom trawls (nei)",
                                                             "Otter trawls (nei)"))
  setwd(here())
  
  save(fishing_effort_mpa_country, file = "output/fishing_effort_mpa_country.Rdata")
  
  
  
}