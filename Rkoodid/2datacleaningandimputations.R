#Data cleaning

#- changes in names and labels
#- wide -> long
#- filtering
#- imputation of missing data


# HBS --------------------
load(file = "Andmed/hbsexp_en.RData")
colnames(hbsexp_en) <- c("year", "indicator", "commodity", "id1", "id2", "id3", "id4", "id5")

#Cleaning and imputation
hbsexp <- hbsexp_en %>% gather(key = "group", value = "value", -year, -indicator, -commodity) %>% 
  mutate(value = as.numeric(value)) %>% 
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
hbsexp_means <- hbsexp  %>% 
  filter(indicator =="share") %>% 
  group_by(commodity, group) %>% 
  summarise(averageshares = mean(value, na.rm = TRUE)) %>% ungroup()

#Let's keep only shares for now
hbsexpshares <- hbsexp %>% filter(indicator =="share") %>%  left_join(hbsexp_means, by = c("commodity", "group")) %>% 
  mutate(share = ifelse(is.na(value), ifelse(is.na(averageshares), 0, averageshares), value))

#head(hbsexp1)
#hbsexp1 %>% filter(is.na(value)) %>% View()

#First level
hbsexpshareslvl1 <- hbsexpshares %>%  filter(!str_detect(commodity, "\\."))  %>% 
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
))


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



# Prices  --------------------
load(file = "Andmed/hicp_en.RData")

#Merged data
hbsexppriceslevel1 <- hbsexpshareslvl1  %>% left_join(hicp_en %>% rename(year = time, price = values) %>% 
                                                   select(coicop, year, price), by = c("coicop", "year"))
hbsexppriceslevel1 <- hbsexppriceslevel1 %>% select(year, group, coicop, share, price)

save(hbsexppriceslevel1, file = "Andmed/hbsexppriceslevel1.RData")


#Total consumption expenditures, needed for AIDS model
hbsexptotal <- hbsexp %>% filter(indicator =="eur", commodity =="Consumption expenditure") %>% 
  select(year, group, value) %>% 
  rename(consexp = value)

save(hbsexptotal, file = "Andmed/hbsexptotal.RData")

