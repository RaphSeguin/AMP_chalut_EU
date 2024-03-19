load_intersections <- function(){
  
  path = (here::here("output/intersections"))
  setwd(path)
  fishing_effort_mpa <- list.files() %>%
    map_df(~ get(load(file = .x))) %>%
    mutate(vessel_class_registry = sub("\\|.*", "", vessel_class_registry)) %>%
    dplyr::filter(vessel_class_registry == "trawlers" | gear %in% c("Single boat bottom otter trawls","Bottom trawls nephrops trawls","Single boat midwater otter trawls",
                                                                    "Beam trawls","Bottom pair trawls","Twin bottom otter trawls","Midwater pair trawls","Bottom trawls (nei)",
                                                                    "Otter trawls (nei)")) 
  setwd(here())
  
  save(fishing_effort_mpa, file = "output/fishing_effort_mpa.Rdata")
  
}