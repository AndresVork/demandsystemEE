#Data import from Statistics Estonia database and Eurostat
library(tidyverse)
library(httr)

#library(readxl)
#library(zoo)
#library(lubridate)
#library(data.table)

#1. Household budget survey data (Leibkonna Eelarve Uuring)

#Query from Statistics Estonia database
myquery = '{
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
                       body = myquery, encode = "json"), 
                  type = "text/csv", encoding = "UTF-8") %>%
  as.data.frame() 

#Cleaning
# hbsexp_ee <- hbsexp_ee %>% gather(key = "Kvintiil", value = "Kulutus", -Näitaja, -Aasta, -`Kulutuste liik`) %>% 
#   mutate(Kulutusnr = as.numeric(Kulutus))
# colnames(hbsexp_ee) = 

save(hbsexp_ee, file = "Andmed/hbsexp_ee.RData")


#in English
hbsexp_en <- content(POST("https://andmed.stat.ee/api/v1/en/stat/LE209", 
                          body = myquery, encode = "json"), 
                     type = "text/csv", encoding = "UTF-8") %>%
  as.data.frame() 
colnames(hbsexp_en) <- c("year", "indicator", "commodity", "quint1", "quint2", "quint3", "quint4", "quint5")






#Cleaning and imputation
hbsexp_en <- hbsexp_en %>% gather(key = "group", value = "value", -year, -indicator, -commodity) %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(indicator %in% c("Expenditure, euros", "Percentage (expenditure total = 100)")) %>% 
  mutate(indicator= case_when(
    indicator=="Expenditure, euros" ~ "eur",
    indicator=="Percentage (expenditure total = 100)" ~ "share",
    TRUE ~ indicator
  ))


#Impute missing values
#86 missing values
table(is.na(hbsexp_en$value))
#use proportions from neighbouring time points and quintiles?

hbsexp_en %>% filter(is.na(value)) %>% arrange(year, commodity, group) %>% filter(indicator =="share") %>%  View()

shares <- hbsexp_en %>% filter(indicator =="share")


temp<- hbsexp_en %>% filter(indicator =="")
lm(expenditure)



save(hbsexp_en, file = "Andmed/hbsexp_en.RData")

colnames(hbsexp_en)


#Old data 1996-2007

#LE113: LEIBKONNALIIKME KULUTUSED KUUS LEIBKONNA KULUDETSIILI JÄRGI (1996-2007)
#https://andmed.stat.ee/et/stat/Lepetatud_tabelid__Sotsiaalelu.%20Arhiiv__Leibkonnad.%20%20Arhiiv__kuu-kulutused/LE113
#https://andmed.stat.ee/api/v1/et/stat/LE113

myquery = '{
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
                          body = myquery, encode = "json"), 
                     type = "text/csv", encoding = "UTF-8") %>%
  as.data.frame() 
str(hbsexpold_ee)
