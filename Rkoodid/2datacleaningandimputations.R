library(tidyverse)

#Data cleaning

#- changes in names and labels
#- wide -> long
#- filtering
#- imputation of missing data

# HBS --------------------
#1. Read in data
#2. Keep consumption expenditure
#3. Assign missing shares average values (TODO:! use some regression based approach where total (real) expenditure matters)
#4. Reweight shares so that the sum would be 1
#5. Add COICOP codes
#6. Merge total consumption expenditure

load(file = "Andmed/hbsexp_en.RData")
#pick numbers for quintiles that would be similar to deciles
colnames(hbsexp_en) <- c("year", "indicator", "commodity", "12", "34", "56", "78", "90")

#Cleaning and imputation
hbsexp <- hbsexp_en %>% gather(key = "quantile", value = "value", -year, -indicator, -commodity) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(quantile = as.numeric(quantile)) %>% 
  filter(indicator %in% c("Expenditure, euros", "Percentage (expenditure total = 100)")) %>% 
  mutate(indicator= case_when(
    indicator=="Expenditure, euros" ~ "eur",
    indicator=="Percentage (expenditure total = 100)" ~ "share",
    TRUE ~ indicator
  )) %>% 
  #keep only consumption expenditure
  filter(!(commodity %in% c("Expenditure total", "Other expenditure")))

#Imputation
#Let's use currently just some average numbers
#TODO! Check by levels and use when possible substractions from total
meanshares <- hbsexp  %>% 
  filter(indicator =="share") %>% 
  group_by(commodity, quantile) %>% 
  summarise(averageshares = mean(value, na.rm = TRUE)) %>% ungroup()

#Let's keep only shares
hbsexp <- hbsexp %>% filter(indicator =="share") %>%  left_join(meanshares, by = c("commodity", "quantile")) %>% 
  mutate(share = ifelse(is.na(value), ifelse(is.na(averageshares), 0, averageshares), value))

#keep First level
hbsexplvl1 <- hbsexp %>%  filter(!str_detect(commodity, "\\."))  %>% 
  mutate (coicop = case_when(
  commodity == "Consumption expenditure" ~ "CP00",
  commodity == "Food and non-alcoholic beverages" ~ "CP01",
  commodity == "Alcoholic beverages and tobacco" ~ "CP02",
  commodity == "Clothing and footwear" ~ "CP03",
  commodity == "Housing" ~ "CP04",
  commodity == "Household equipment and operation" ~ "CP05",
  commodity == "Health" ~ "CP06",
  commodity == "Transport" ~ "CP07",
  commodity == "Communication" ~ "CP08",
  commodity == "Recreation and culture" ~ "CP09",
  commodity == "Education" ~ "CP10",
  commodity == "Restaurants and hotels" ~ "CP11",
  commodity == "Miscellaneous goods and services" ~ "CP12",
  TRUE ~ "XXX"
)) %>% 

  #Viskame välja kogu tarbimiskulud, sest neid ei lähe tegelikult vaja osakaalus
  filter(coicop !="CP00")  %>% 
  #scale it to sum 1
    rename(shareold = share) %>% 
    group_by(year, quantile) %>% 
    mutate(share = shareold/sum(shareold)) %>% ungroup() %>% 
  select(year, quantile, coicop, commodity, share)

ggplot(hbsexplvl1, aes(x=year, y= share, fill = coicop))+
  geom_area()+
  facet_wrap(~quantile) +
  scale_x_continuous(breaks = c(2010, 2015, 2019))

# ggplot(hbsexplvl1, aes(x=year, y= share, fill = commodity))+
#   geom_area()+
#   facet_wrap(~quantile) +
#   scale_x_continuous(breaks = c(2010, 2015, 2019))


#Total consumption expenditures, needed for AIDS model
hbsexptotal <- hbsexp_en %>% gather(key = "quantile", value = "value", -year, -indicator, -commodity) %>% 
  filter(indicator == "Expenditure, euros",
         commodity == "Consumption expenditure") %>% 
  mutate(quantile =as.numeric(quantile),
           consexp = as.numeric(value)) %>%
  select(year, quantile, consexp)

#save(hbsexptotal, file = "Andmed/hbsexptotal.RData")
#head(hbsexplvl1)

rm(hbsexp, hbsexp_en, meanshares)


#Old HBS data -----------------

#Steps:
#1. Read in data
#2. Keep consumption expenditure
#3. Calculate shares (item expenditure/total consumption expenditure) by year-decile
#4. Assign missing shares average values (TODO:! use some regression based approach where total (real) expenditure matters)
#5. Reweight shares so that the sum would be 1
#6. Aggregate to suitable level. 
#7. Add COICOP codes
#8. Merge total consumption expenditure

#1.Read in data
load(file = "Andmed/hbsexpold_en.RData")
#unique(hbsexpold_en$`Kind of expenditure`)
colnames(hbsexpold_en) <- c("year", "commodity", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")

hbsexpold <- hbsexpold_en %>% gather(key = "quantile", value = "value", -year, -commodity) %>% 
  mutate(value = as.numeric(value)*12) %>%  #multiply it by 12 to get annual data
  mutate(quantile = as.numeric(quantile)) %>% 
  #keep only consumption expenditure
  filter(!(commodity %in% c("Expenditure total", "Other expenditure")))

#Keep totals and merge it back to data as a variable
hbsexptotalold <- hbsexpold %>% filter(commodity =="Consumption expenditure") %>% 
  select(year, quantile, value) %>% 
  rename(consexp = value)

#check
# ggplot(hbsexptotalold, aes(x=as.factor(year), y = consexp, color = as.factor(quantile), group = quantile)) +
#   geom_line()+
#   geom_point()

#Merge it back to data
hbsexpold <- hbsexpold %>% left_join(hbsexptotalold, by = c("year", "quantile")) %>% 
  #and calculate fractions 
  mutate(share = value/consexp)

#Imputation
  #Let's use currently just some average numbers
  #TODO! Check by levels and use when possible substractions from total
meanshares <- hbsexpold  %>% 
  group_by(commodity, quantile) %>% 
  summarise(averageshares = mean(share, na.rm = TRUE)) %>% ungroup()

#check
 # ggplot(hbsexpold %>% filter(commodity == "Food and non-alcoholic beverages"), aes(x=as.factor(year), y = share, color = group, group = group)) +
 #   geom_line()+
 #   geom_point()

#Let's keep only shares for now
#Add back to data and replace missing values with averages
hbsexpold <- hbsexpold %>% left_join(meanshares, by = c("commodity", "quantile")) %>% 
  mutate(share = ifelse(is.na(share), ifelse(is.na(averageshares), 0, averageshares), share))

#Add up alcohol and tobacco, as these were together in new data and non-monetary consumption
hbsexpold <-hbsexpold %>% 
  #It created problems
  filter(commodity != "Non-monetary consumption") %>% 
  rename(commodityold = commodity) %>% 
  mutate(commodity = case_when(
    commodityold %in% c("Alcoholic beverages", "Tobacco") ~  "Alcoholic beverages and tobacco",
    #It created problems
    #commodityold %in% c("Non-monetary consumption") ~  "Miscellaneous goods and services",
    TRUE ~ commodityold)) %>% 
  group_by(year, commodity, quantile) %>% 
  summarise(share = sum(share)) %>% ungroup()

#Level 1 data only
hbsexpoldlvl1 <- hbsexpold  %>%  filter(!str_detect(commodity, "\\."))  %>% 
  mutate (coicop = case_when(
    commodity == "Consumption expenditure" ~ "CP00",
    commodity == "Food and non-alcoholic beverages" ~ "CP01",
    commodity == "Alcoholic beverages and tobacco" ~ "CP02",
    commodity == "Clothing and footwear" ~ "CP03",
    commodity == "Housing" ~ "CP04",
    commodity == "Household equipment and operation" ~ "CP05",
    commodity == "Health" ~ "CP06",
    commodity == "Transport" ~ "CP07",
    commodity == "Communication services" ~ "CP08",
    commodity == "Recreation, leisure and entertainment" ~ "CP09",
    commodity == "Education" ~ "CP10",
    commodity == "Hotels, cafés, restaurants" ~ "CP11",
    commodity == "Miscellaneous goods and services" ~ "CP12",
    TRUE ~ "XXX"
  ))  %>%  
#Viskame välja kogu tarbimiskulud, sest neid ei lähe tegelikult vaja
filter(coicop !="CP00")  %>% 
  #scale it to sum 1
  rename(shareold = share) %>% 
  group_by(year, quantile) %>% 
    mutate(share = shareold/sum(shareold)) %>% ungroup() %>% 
  select(year, quantile, coicop, commodity, share)

# ggplot(hbsexpoldlvl1, aes(x=year, y= share, fill = coicop))+
#    geom_area()+
#    facet_wrap(~quantile) +
#   scale_x_continuous(breaks = c(1996, 2001, 2007))

#Valmis vana fail HBS fail

#Append new and old data. Note that quantiles do not match

hbslvl1 <- bind_rows(hbsexpoldlvl1, hbsexplvl1)

# ggplot(hbslvl1, aes(x=year, y= share, fill = coicop))+
#   geom_area()+
#   facet_wrap(~quantile)

totals <- bind_rows(hbsexptotalold, hbsexptotal)

# ggplot(totals, aes(x=year, y= consexp, color = as.factor(quantile), group=quantile))+
#   geom_line() +
#   geom_point()

#Add totals to shares
#head(hbslvl1)
#head(totals)

hbslvl1 <- hbslvl1 %>% left_join(totals, by = c("year", "quantile"))

# Prices  --------------------
load(file = "Andmed/hicp_en.RData")
hbsexppriceslevel1 <- hbslvl1  %>% left_join(hicp_en %>% rename(year = time, price = values) %>% 
                                                        select(coicop, year, price), by = c("coicop", "year")) %>% 
  arrange(year, quantile, coicop, commodity)

#Eesti keelsed nimetused
hbsexppriceslevel1 <- hbsexppriceslevel1 %>% 
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

save(hbsexppriceslevel1, file = "Andmed/hbsexppriceslevel1.RData")

#Annex

#
# 2            Consumption expenditure ; CP00 - All-items HICP
# 3   Food and non-alcoholic beverages 100000;  CP01 - Food and non-alcoholic beverages
# 4    Alcoholic beverages and tobacco 200000 CP02 - Alcoholic beverages, tobacco and narcotics
# 5              Clothing and footwear 300000 CP03 - Clothing and footwear
# 6                            Housing 400000 CP04 - Housing, water, electricity, gas and other fuels
# 7  Household equipment and operation 500000 CP05 - Furnishings, household equipment and routine household maintenance
# 8                             Health 600000 CP06 - Health
# 9                          Transport 700000 CP07 - Transport
# 10                     Communication 800000 CP08 - Communications
# 11            Recreation and culture 900000 CP09 - Recreation and culture
# 12                         Education 1000000 CP10 - Education
# 13            Restaurants and hotels 1100000 CP11 - Restaurants and hotels
# 14  Miscellaneous goods and services 1200000 CP12 - Miscellaneous goods and services


# LEU 2010-2019	Hinnaindeks
# Kulutused kokku	
# Tarbimiskulud	CP00 Kokku
# Toit ja alkoholita joogid	CP01 Toit ja mittealkohoolsed joogid
# Alkoholjoogid ja tubakas	CP02 Alkohoolsed joogid ja tubakatooted
# Rõivad ja jalatsid	CP03 Riietus ja jalatsid
# Eluase	CP04 Eluase
# Majapidamiskulud	CP05 Majapidamine
# Tervishoid	CP06 Tervishoid
# Transport	CP07 Transport
# Side	CP08 Side
# Vaba aeg	CP09 Vaba aeg
# Haridus	CP10 Haridus ja lasteasutused
# Restoranid ja hotellid	CP11 Söömine väljaspool kodu, majutus
# Mitmesugused kaubad ja teenused	CP12 Mitmesugused kaubad ja teenused

# 
#   
#   
#   mutate (commodity_ee = case_when(
#     commodity == "Consumption expenditure" ~ "Tarbimiskulud",
#     commodity == "Food and non-alcoholic beverages" ~ "Toit ja alkoholita joogid",
#     commodity == "Alcoholic beverages and tobacco" ~ "Alkoholjoogid ja tubakas",
#     commodity == "Clothing and footwear" ~ "Rõivad ja jalatsid",
#     commodity == "Housing" ~ "Eluase",
#     commodity == "Household equipment and operation" ~ "Majapidamiskulud",
#     commodity == "Health" ~ "Tervishoid",
#     commodity == "Transport" ~ "Transport",
#     commodity == "Communication services" ~ "Side",
#     commodity == "Recreation, leisure and entertainment" ~ "Vaba aeg",
#     commodity == "Education" ~ "Haridus",
#     commodity == "Hotels, cafés, restaurants" ~ "Restoranid ja hotellid",
#     commodity == "Miscellaneous goods and services" ~ "Mitmesugused kaubad ja teenused",
#     TRUE ~ "XXX"
#   ))

