#Consumption shares
#load(file = "Andmed/hbsexp_ee.RData")
#load(file = "Andmed/hbsexptotal.RData")


load(file = "Andmed/hbsexppriceslevel1.RData")

#Shares
ggplot(hbsexppriceslevel1, aes(x=year, y = share, color = as.factor(quantile))) +
  geom_line() +
  geom_point() +
  facet_wrap(~coicop, scales = "free_y")

#Shares and relative prices
temp1 <- hbsexppriceslevel1 %>% left_join(hicp_en %>% filter(coicop =="CP00") %>% rename(year = time, CP00price = values) %>%  
                           select(year, CP00price), by = c("year"))

ggplot(temp1 , aes(x=price/CP00price, y = share, color = as.factor(quantile))) +
  geom_point() +
  facet_wrap(~quantile)

#Need to shape it to wide
head(hbsexppriceslevel1)

#First put price and share into single variable
df <- hbsexppriceslevel1 %>% gather(key = "xvar", value  = "value", -year, -quantile, -coicop, -consexp)
#Merge values to get new variable names
df <- df %>% mutate(x = paste0(coicop, xvar))
head(df)
#Shape back to wide
dfw <- df %>% select(year, quantile, consexp, x, value) %>% spread(key = "x", value = "value") %>% 
  as.data.frame()


head(dfw)

#Single regression ------------------
#Add general price level to run single regression model?
head(hicp_en)
temp1 <- dfw %>% left_join(hicp_en %>% filter(coicop =="CP00") %>% rename(year = time, CP00price = values) %>%  
            select(year, CP00price), by = c("year"))
head(temp1)

#Alcohol and tobacco
model1 <- lm(data = temp1, CP02share ~ log(consexp/CP00price) + log(CP02price/CP00price) + year)
summary(model1)

model1 <- lm(data = temp1, CP02share ~ log(consexp/CP00price) + log(CP02price/CP00price))
summary(model1)

ggplot(temp1, aes(x=year, y = CP02share, color = as.factor(quantile))) +
  geom_line() +
  geom_point()

priceNames <- setdiff(colnames(dfw)[grepl("price", colnames(dfw))], "CP00price")
shareNames <- setdiff(colnames(dfw)[grepl("share", colnames(dfw))], "CP00share")


#https://cran.r-project.org/web/packages/micEconAids/vignettes/micEconAids_vignette.pdf
library("micEconAids")

laaidsResult <- aidsEst( priceNames, shareNames, "consexp", data = dfw,  priceIndex = "S" )
laaidsResult <- aidsEst( priceNames, shareNames, "consexp", data = dfw,  priceIndex = "P" )
laaidsResult <- aidsEst( priceNames, shareNames, "consexp", data = dfw,  priceIndex = "L" )
laaidsResult <- aidsEst( priceNames, shareNames, "consexp", data = dfw,  priceIndex = "Ls" )

laaidsResult <- aidsEst( priceNames, shareNames, "consexp", data = dfw,  priceIndex = "Ls", method = "IL" )

summary(laaidsResult)

?micEconAids()
?aidsEst

print(laaidsResult)
elas(laaidsResult)

aidsElas()

all.equal( sum( coef( laaidsResult )$alpha ), 1 )
all.equal( sum( coef( laaidsResult )$beta ), 0 )
all.equal( colSums( coef( laaidsResult )$gamma ), rep( 0, 12 ), check.attributes = FALSE )
all.equal( rowSums( coef( laaidsResult )$gamma ), rep( 0, 12 ), check.attributes = FALSE )
isSymmetric( coef( laaidsResult )$gamma, tol = 1e-10, check.attributes = FALSE )


shifterNames = "trend"

pMeans <- colMeans( dfw[ , priceNames ] )
wMeans <- colMeans( dfw[ , shareNames ] )

aidsResultElas <- aidsElas( coef( laaidsResult ), prices = pMeans, shares = wMeans )
print( aidsResultElas )

xtMean <- mean( dfw[ , "consexp" ] )

aidsResultElasCov <- aidsElas( coef( laaidsResult ), prices = pMeans, totExp = xtMean, coefCov = vcov( laaidsResult ),
                               df = df.residual( laaidsResult ) )
summary( aidsResultElasCov )



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




