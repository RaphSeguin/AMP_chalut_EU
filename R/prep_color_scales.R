prep_color_scales <- function(){
  
  
  #Countries
  country_color_scale <- c("FRA" = "#FEEFA5",
                           "ESP" = "#FFB01C",
                           "NLD" = "#806429",
                           "ITA" = "#E48519",
                           "DEU" = "#EAA057",
                           "FRA;ITA;MCO" = "#DDB889",
                           "DNK" = "#F4CB91",
                           "PRT" = "#CC9D65",
                           "HRV" = "#9B6635",
                           "MLT" = "#4B2E20",
                           "BEL"="#F09801",
                           "SWE"="#B37400",
                           "BGR"="#BA621E",
                           "LVA"="#C4834F")
  
  return(country_color_scale)
  
  #Gear color scale
  gear_color_scale <- c("dredge_fishing"="#5D2676",
                        "drifting_longlines"="#702E74",
                        "driftnets"="#8A3A75",
                        "fishing"="#A34774",
                        "other_purse_seines"="#BB5374",
                        "other_seines"="#D25F74",
                        "pole_and_line"="#EB6A74",
                        "pots_and_traps"="#EC7C74",
                        "purse_seines"="#F08D73",
                        "seiners"="#F69D71",
                        "set_gillnets"="#F9AF71",
                        "set_longlines"="#FEBD71",
                        "trawlers"="#FFCC8D",
                        "trollers"="#FFE0BA",
                        "tuna_purse_seines"="#262254")
  
  #Fishing gear names
  fishing_names <- c("dredge_fishing"="Pêche à la drague",
                     "drifting_longlines"="palangres_dérivantes",
                     "driftnets"="filets dérivants ",
                     "fishing"="Pêche",
                     "other_purse_seines"="Autres sennes coulissantes",
                     "other_seines"="Autre sennes",
                     "pole_and_line"="Cannes et lignes",
                     "pots_and_traps"="Casiers et pièges",
                     "purse_seines"="Sennes coulissantes",
                     "seiners"="Senneurs",
                     "set_gillnets"="Filets maillants fixes",
                     "set_longlines"="Palangres fixes",
                     "trawlers"="Chalutiers",
                     "trollers"="Ligneurs",
                     "tuna_purse_seines"="Thoniers senneurs")
  
  
}