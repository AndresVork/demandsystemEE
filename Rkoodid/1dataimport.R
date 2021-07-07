#Data import from Statistics Estonia database and Eurostat
library(dplyr)
library(httr)

#Imports data

#1. Household budget survey data (Leibkonna Eelarve Uuring) 2010-2020------------
# https://andmed.stat.ee/et/stat/sotsiaalelu__leibkonnad__leibkonna-eelarve__aasta-kulutused/LE209
#LE209: LEIBKONNALIIKME KULUTUSED AASTAS KULUKVINTIILI JÄRGI
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
write.csv(hbsexp_ee, file = "Andmed/hbsexp_ee.csv", row.names = FALSE)


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
write.csv(hbsexpold_ee, file = "Andmed/hbsexpold_ee.csv", row.names = FALSE)


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

write.csv(hicp_en, file = "Andmed/hicp_en.csv", row.names = FALSE)


temp_hicp <- eurostat::get_eurostat("prc_hicp_aind", time_format = "num", cache = FALSE) 

hicp_en_balt <- temp_hicp %>% 
  filter(geo  %in% c("EE", "LV", "LT"), unit == "INX_A_AVG")

hicp_en_balt <- hicp_en_balt %>% 
  filter(coicop %in% c("CP00", "CP01", "CP02", "CP03", "CP04", "CP05", "CP06", "CP07", "CP08", "CP09", "CP10", "CP11", "CP12")) %>% 
  mutate (commodity_ee = case_when(
    coicop == "CP00" ~ "Tarbimiskulud",
    coicop == "CP01" ~ "Toit ja alkoholita joogid",
    coicop == "CP02" ~ "Alkoholjoogid ja tubakas",
    coicop == "CP03" ~ "Rõivad ja jalatsid",
    coicop == "CP04" ~ "Eluase",
    coicop == "CP05" ~ "Majapidamiskulud",
    coicop == "CP06" ~ "Tervishoid",
    coicop == "CP07" ~ "Transport",
    coicop == "CP08" ~ "Side",
    coicop == "CP09" ~ "Vaba aeg",
    coicop == "CP10" ~ "Haridus",
    coicop == "CP11" ~ "Restoranid ja hotellid",
    coicop == "CP12" ~ "Mitmesugused kaubad ja teenused"
  ))

save(hicp_en_balt, file = "Andmed/hicp_en_balt.RData")
write.csv(hicp_en_balt, file = "Andmed/hicp_en_balt.csv", row.names = FALSE)


#Tarbimiskulutuste struktuur kvintiilide lõikes lätis ja leedus

#Läti
#https://data.stat.gov.lv/pxweb/en/OSP_PUB/START__POP__MB__MBI/?tablelist=true
#https://data.stat.gov.lv/pxweb/en/OSP_PUB/START__POP__MB__MBI/MBI140/table/tableViewLayout1/
#MBI140. Composition and structure of consumption expenditure by quintile average per household member per month (euro; per cent) by Quintile groups, Goods and services, Time period and Unit

#Leedu andmeid ei ole. Üksnes asustuste lõikes (linn, maa), aga seal võiks olla hindade varieeruvus väga tugev, mistõttu ei kasutaks.
#https://osp.stat.gov.lt/statistiniu-rodikliu-analize?indicator=S7R157#/

#Läti andmed 2002-2019!
hbslv_en <- read.csv(file = "Andmed/latvia_MBI140_11juuli2021_av", sep = ";")





