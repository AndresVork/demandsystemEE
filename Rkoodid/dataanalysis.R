#Consumption shares
load(file = "Andmed/hbsexp_ee.RData")


load(file = "Andmed/hbsexptotal.RData")
load(file = "Andmed/hbsexppriceslevel1.RData")

#Need to shape it to wide
head(hbsexppriceslevel1)
#long
df <- hbsexppriceslevel1 %>% gather(key = "xvar", value  = "value", -year, -group, -coicop)
df <- df %>% mutate(x = paste0(coicop, xvar))
dfw <- df %>% select(year, group, x, value) %>% spread(key = "x", value = "value")

dfw <- dfw %>% left_join(hbsexptotal, by = c("year", "group"))

priceNames <- setdiff(colnames(dfw)[grepl("price", colnames(dfw))], "CP00price")
shareNames <- setdiff(colnames(dfw)[grepl("share", colnames(dfw))], "CP00share")

laaidsResult <- aidsEst( priceNames, shareNames, "consexp", data = dfw,  priceIndex = "S" )

temp <- dfw %>% mutate(osakaaludesumma = CP01share+CP02share+CP03share+CP04share+CP05share+CP06share+
                         CP07share+CP08share + CP09share+CP10share+CP11share+CP12share
) %>% 
  mutate_at(.vars = vars(ends_with("share")), . = ./osakaaludesumma)


#https://cran.r-project.org/web/packages/micEconAids/vignettes/micEconAids_vignette.pdf
library("micEconAids")



aidsEst( priceNames, shareNames, totExpName, data )


data( "Blanciforti86" )

colnames(Blanciforti86)
priceNames <- c( "pFood1", "pFood2", "pFood3", "pFood4" )
shareNames <- c( "wFood1", "wFood2", "wFood3", "wFood4" )

data %>% dplyr::select(starts_with("pFood"), starts_with("wFood"), xFood) %>% View()

data %>% dplyr::select(starts_with("pFood")) %>% View()

laaidsResult <- aidsEst( priceNames, shareNames, "xFood", data = Blanciforti86,  priceIndex = "S" )

print( laaidsResult )




