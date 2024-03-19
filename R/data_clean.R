#Cleaning gfw data

data_clean <- function(fishing_effort){
  
  cleaned_data <- fishing_effort %>%
    #Merging with MMMSI data from GFW
    left_join(fishing_fleet_gfw, by = "mmsi") %>%
    #Keeping only vessels longer than 15 meters
    #Merging with data from Europe fishing vessels registry
    left_join(fishing_fleet_complete, by = "mmsi") %>%
    mutate(clean_length = ifelse(is.na(length),length_m_registry,length)) %>%
    filter(clean_length > 15) %>%
    #When trawling is unknown, define as otter trawl, the most common type of trawling
    # mutate(Geartype = ifelse(gear_type == "trawlers" & is.na(Geartype), "OTB", Geartype)) %>%
    #Adding month and year
    dplyr::rename(date = "time_range") %>%
    mutate(Year = ifelse(str_detect(date, "2012"),"2012",
                         ifelse(str_detect(date,"2013"),"2013",
                                ifelse(str_detect(date,"2014"),"2014",
                                       ifelse(str_detect(date,"2015"),"2015",
                                              ifelse(str_detect(date,"2016"),"2016",
                                                     ifelse(str_detect(date,"2017"),"2017",
                                                            ifelse(str_detect(date,"2018"),"2018",
                                                                   ifelse(str_detect(date,"2019"),"2019",
                                                                          ifelse(str_detect(date,"2020"),"2020",
                                                                                 ifelse(str_detect(date,"2021"),"2021",
                                                                                        ifelse(str_detect(date,"2022"),"2022","2023")))))))))))) %>%
    mutate(MonthNum = as.factor(ifelse(str_detect(date,"-03-"),"03",
                                       ifelse(str_detect(date,"-04-"),"04",
                                              ifelse(str_detect(date,"-05-"),"05",
                                                     ifelse(str_detect(date,"-06-"),"06",
                                                            ifelse(str_detect(date,"-07-"),"07",
                                                                   ifelse(str_detect(date,"-08-"),"08",
                                                                          ifelse(str_detect(date,"-09-"),"09",
                                                                                 ifelse(str_detect(date,"-10-"),"10",
                                                                                        ifelse(str_detect(date,"-11-"),"11",
                                                                                               ifelse(str_detect(date,"-01-"),"01",
                                                                                                      ifelse(str_detect(date,"-12-"),"12","02"))))))))))))) %>%
    #Associating iso3 name to each country
    mutate(iso3 = ifelse(name == "Belgian_2023","BEL",
                         ifelse(name == "Portugese_azores_2023","PRT",
                                ifelse(name == "Portugese_madeira_2023", "PRT",
                                       ifelse(name == "Spain_canary_2023", "ESP",
                                              ifelse(name == "Bulgarian_2023","BGR",
                                                     ifelse(name == "Croatian_2023","HRV",
                                                            ifelse(name == "Cypriote_2023","CYP",
                                                                   ifelse(name == "Danish_2023","DNK",
                                                                          ifelse(name == "Dutch_2023","NLD",
                                                                                 ifelse(name == "Estonian_2023","EST",
                                                                                        ifelse(name == "Finland_2023","FIN",
                                                                                               ifelse(name == "France_2023","FRA",
                                                                                                      ifelse(name == "Gerrman_2023","DEU",
                                                                                                             ifelse(name == "Greek_2023","GRC",
                                                                                                                    ifelse(name == "Irish_2023","IRL",
                                                                                                                           ifelse(name == "Italia_2023","ITA",
                                                                                                                                  ifelse(name == "Latvia_2023","LVA",
                                                                                                                                         ifelse(name == "Lithuania_2023","LTU",
                                                                                                                                                ifelse(name == "Maltese_2023","MLT",
                                                                                                                                                       ifelse(name == "Polish_2023","POL",
                                                                                                                                                              ifelse(name == "Portugese_2023","PRT",
                                                                                                                                                                     ifelse(name == "Romania_2023","ROU",
                                                                                                                                                                            ifelse(name == "Slovenia_2023","SVN",
                                                                                                                                                                            ifelse(name == "Spain_2023","ESP",
                                                                                                                                                                                   ifelse(name == "Sweden_2023","SWE",
                                                                                                                                                                                          ifelse(name == "Britain_2023","GBR","NA"))))))))))))))))))))))))))) %>%
    mutate(date = as.Date(date),
           apparent_fishing_hours = as.numeric(apparent_fishing_hours),
           flag_registry = as.factor(flag_registry),
           gear = as.factor(gear),
           Year = as.factor(Year))
  
  return(cleaned_data)

}