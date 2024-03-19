#AMP iroise
iroise <- st_read("data/carto_amp.france_amp_fordownload_1709822839_6803/carto_amp.france_amp_fordownload.shp") %>%
  dplyr::filter(gid == "178") %>%
  st_transform(crs = st_crs(iroise_spatial))

#load csv
iroise_fishing <- read.csv("data/iroise_fishing.csv")

#fishing method
registry <- read.csv("data/fishing_vessels_v2.csv")

iroise_clean <- iroise_fishing %>% 
  left_join(registry, by = "mmsi")

library(sf)

#Total fishing effort
iroise_total <- iroise_fishing %>%
  group_by(mmsi) %>%
  reframe(total_fishing = sum(Apparent.Fishing.Hours)) %>%
  ungroup()

iroise_spatial <- iroise_fishing %>%
  st_as_sf(coords = c("Lon","Lat"), crs = 4326) %>%
  st_intersection(iroise)

length(unique(iroise_spatial$mmsi))
  

ggplot() + 
  geom_sf(data = iroise, fill = "lightblue") + 
  geom_sf(data = iroise_spatial)
