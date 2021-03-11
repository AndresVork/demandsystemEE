#Impute missing shares and from there total expenditure
#Estimate a separate model for each commodity group
# share_ijg = beta0 + a

temp_shares <- hbsexp %>% filter(indicator =="share")
table(is.na(temp_shares$value))

hbsexp


#use proportions predicted from regression model (?)
missingitems <- temp_shares %>% filter(is.na(temp_shares$value)) %>% select(commodity) %>% distinct(commodity)
missingitems <- missingitems$commodity

temp_lm <- lm(value ~ factor(year) + factor(commodity) + factor(group), data = temp_shares %>% filter(commodity %in% missingitems[1]))

temp_lm <- lm(value ~ factor(year) + factor(group), data = temp_shares %>% filter(commodity %in% missingitems[1]))

temp_shares %>% filter(commodity %in% missingitems[1])
table(temp_shares$commodity)

temp<- hbsexp_en %>% filter(indicator =="")
lm(expenditure)


temp1 <- temp_shares %>% dplyr::filter(commodity %in% missingitems[1])
View(temp1)
temp_lm <- lm(value ~ factor(year) + factor(group), data = temp_shares %>% filter(commodity %in% missingitems[1]))
summary(temp_lm)


save(hbsexp_en, file = "Andmed/hbsexp_en.RData")

colnames(hbsexp_en)


#Cleaning
# hbsexp_ee <- hbsexp_ee %>% gather(key = "Kvintiil", value = "Kulutus", -Näitaja, -Aasta, -`Kulutuste liik`) %>% 
#   mutate(Kulutusnr = as.numeric(Kulutus))
# colnames(hbsexp_ee) = 


temp_shares %>% filter(is.na(value)) %>%   View()


missingtq <- temp_shares %>% filter(is.na(temp_shares$value)) %>% distinct(year, group) %>% mutate(onpuuduv =1)

temp_shares1 <- temp_shares %>% left_join(missingtq, by = c("year", "group"))


temp_shares1 %>% filter(onpuuduv==1, !(str_detect(commodity, "\\."))) %>% View()



 %>% distinct()


hbsexplevel1 <- hbsexp %>% select(commodity) %>% filter(!str_detect(commodity, "\\.")) %>% distinct()