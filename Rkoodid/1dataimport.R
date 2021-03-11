#Data import from Statistics Estonia database and Eurostat
library(dplyr)
library(httr)

#Imports data

#1. Household budget survey data (Leibkonna Eelarve Uuring) 2010-2019------------
#Statistics Estonia

#Query from Statistics Estonia database
myqueryhbs = '{
  "query": [
    {
      "code": "Kulutuste liik",
      "selection": {
        "filter": "item",
        "values": [
        "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
        "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 
        "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", 
        "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", 
        "41", "42", "43", "44", "45", "46", "47", "48", "49"]
      }
    }
  ],
  "response": {
    "format": "csv"
  }
}'

#Download the data
#in Estonian
hbsexp_ee <- content(POST("https://andmed.stat.ee/api/v1/et/stat/LE209", 
                       body = myqueryhbs, encode = "json"), 
                  type = "text/csv", encoding = "UTF-8") %>%
  as.data.frame() 
save(hbsexp_ee, file = "Andmed/hbsexp_ee.RData")


#in English
hbsexp_en <- content(POST("https://andmed.stat.ee/api/v1/en/stat/LE209", 
                          body = myqueryhbs, encode = "json"), 
                     type = "text/csv", encoding = "UTF-8") %>%
  as.data.frame() 
save(hbsexp_en, file = "Andmed/hbsexp_en.RData")


#2. Household budget survey data (Leibkonna Eelarve Uuring) 1996-2007 ------------

#Statistics Estonia
#LE113: LEIBKONNALIIKME KULUTUSED KUUS LEIBKONNA KULUDETSIILI JÄRGI (1996-2007)
#https://andmed.stat.ee/et/stat/Lepetatud_tabelid__Sotsiaalelu.%20Arhiiv__Leibkonnad.%20%20Arhiiv__kuu-kulutused/LE113
#https://andmed.stat.ee/api/v1/et/stat/LE113

myqueryoldhbs = '{
  "query": [
    {
      "code": "Kulutuse liik",
      "selection": {
        "filter": "item",
        "values": [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
        "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21"]
      }
    }
  ],
  "response": {
    "format": "csv"
  }
}'


#in Estonian
hbsexpold_ee <- content(POST("https://andmed.stat.ee/api/v1/et/stat/LE113", 
                          body = myqueryoldhbs, encode = "json"), 
                     type = "text/csv", encoding = "UTF-8") %>%
  as.data.frame() 
save(hbsexpold_ee, file = "Andmed/hbsexpold_ee.RData")

hbsexpold_en <- content(POST("https://andmed.stat.ee/api/v1/en/stat/LE113", 
                             body = myqueryoldhbs, encode = "json"), 
                        type = "text/csv", encoding = "UTF-8") %>%
  as.data.frame() 
save(hbsexpold_en, file = "Andmed/hbsexpold_en.RData")



#3. Consumer price indexes -----------------

library(eurostat)

#Annual averages
temp_hicp <- eurostat::get_eurostat("prc_hicp_aind", time_format = "num", cache = FALSE) 
hicp_en <- temp_hicp %>% 
  filter(geo =="EE", unit == "INX_A_AVG")
save(hicp_en, file = "Andmed/hicp_en.RData")

