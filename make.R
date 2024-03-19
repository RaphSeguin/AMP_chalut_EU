#' Run the Entire Project
#'

#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","cowplot","sf","RColorBrewer","ggridges","plotly","heatmaply","parsedate","birk","ggthemes","MASS","automap","pbmcapply","janitor","gfwr","arrow","beepr","sfarrow","countrycode",
          "harrypotter","wesanderson","ranger","cowplot","patchwork","ggnewscale","extrafont","ggpubr","units","data.table","xml2","XML","rnaturalearth","ggExtra","raster","exactextractr","gstat","magrittr","scales","grid","gridExtra","XML","sjmisc","wesanderson")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

key <- gfw_auth()
sf_use_s2(FALSE)

#-----------------Loading all data---------------------

#Load Rdata
files <- list.files(here::here("data"),pattern="Rdata",recursive = T)
data_list = lapply(files, load, .GlobalEnv)

#-----------------Loading all functions---------------------

path = (here::here("R"))
setwd(path)
files.source = list.files(here::here("R"))
sapply(files.source, source)

#----------------Preparing data-----------------------------

setwd(here())

#Loading necessary shapefiles
load_shapefiles()

#Fishing data
load_fishing_data()

#Loading all outputs
path = (here::here("output"))
setwd(path)
files <- list.files(pattern=".Rdata|RData")
data_list = lapply(files, load, .GlobalEnv)

#Prepping mpa data
prep_mpa_data()

#Color scales
prep_color_scales() 

#Loading all outputs
path = (here::here("output"))
setwd(path)
files <- list.files(pattern=".Rdata|RData")
data_list = lapply(files, load, .GlobalEnv)
setwd(here())

#Now that we have the gfw data, we can filter out info we want
fishing_effort_clean <- data_clean(fishing_effort)
save(fishing_effort_clean, file = "output/fishing_effort_clean.Rdata")

#-----------------ANALYSES-----------------

setwd(here())

#Intersecting with WDPAID polygons
data_intersection(fishing_effort_clean,mpa_eu_wdpa)

#Loading all results into dataframe
load_intersections()

#Intersecting with WDPAID polygons merged by country
data_intersection_union(fishing_effort_clean,mpa_eu_wdpa)

#Loading all results into dataframe
load_intersections_union()

#MPA Area union polygons
aire_amp < aire_amp(WDPA_database_union)

#Calculate mean fishing effort in EEZ
eez_fishing <- calc_fishing_eez(fishing_effort_clean)

#-----------Calculating statistics--------

#Country level statistics
country_level_statistics(fishing_effort_mpa_country)

#Map level statistics
mpa_level_statistics(fishing_effort_mpa)

#United kingdom level analysis
united_kingdom()

#------Plotting maps, figures and tables -------

#Europe level maps
europe_level_maps(fishing_effort_mpa)

#Big table of all countries
big_table()

##----------ANNEXE-------

#Top 30 per country
top30_country()

#Country level figures
iso3_number_vector <- unique(mpa_eu_wdpa$iso3) 

lapply(1:length(iso3_number_vector),function(i){
  
  temp = iso3_number_vector[i]
  
  fiche_par_pays(temp)
  
})



