load_shapefiles <- function(){
  
  #EEZ shapefile
  #Downloadble here: 
  #https://www.marineregions.org/downloads.php 
  setwd(here())
  eez <- st_read("data/shapefiles/World_EEZ_v11/eez_v11.shp")
  save(eez, file = "output/eez.Rdata")
  
  #Map of Europe
  #Download from here:
  #https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/countries
  europe <- st_read("data/shapefiles/NUTS_europe.shp")
  save(europe, file = "output/europe.Rdata")

  #Selecting countries in EU
  eez_EU <- eez %>%
    filter(ISO_TER1 %in% c("DEU","AUT","BEL","BGR","CYP","HRV","DNK","ESP","EST","FIN","FRA","FRA;ITA;MCO","GRC","HUN","IRL","ITA",
                           "LVA","LTU","LUX","MLT","NLD","POL","PRT","CZE","ROU","SVK","SVN","SWE")) %>%
    #delete joint regimes to avoid double counting
    filter(POL_TYPE != "Joint regime") %>%
    dplyr::rename(iso3 = "ISO_TER1")
    
  save(eez_EU, file = "output/eez_EU.Rdata")
  
}