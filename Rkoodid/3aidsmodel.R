#TÖÖPÕLD. Puhastamata
require(tidyverse)

load(file = "Andmed/hbsexppriceslevel1.RData")
load(file = "Andmed/hicp_en.RData")

#Shares
ggplot(hbsexppriceslevel1, aes(x=year, y = share, color = as.factor(quantile))) +
  geom_line() +
  geom_point() +
  facet_wrap(~coicop, scales = "free_y") + 
  labs(color = "Quantile", x ="", y = "Share")

#Shares and relative prices
temp1 <- hbsexppriceslevel1 %>% left_join(hicp_en %>% filter(coicop =="CP00") %>% rename(year = time, CP00price = values) %>%  
                           select(year, CP00price), by = c("year"))

ggplot(temp1 , aes(x=price/CP00price, y = share, color = as.factor(quantile))) +
  geom_point() +
  facet_wrap(~quantile)
#Need to shape it to wide

#First put price and share into single variable
df <- hbsexppriceslevel1 %>% gather(key = "xvar", value  = "value", -year, -quantile, -coicop, -consexp)
#Merge values to get new variable names
df <- df %>% mutate(x = paste0(coicop, xvar))
#Shape back to wide
dfw <- df %>% select(year, quantile, consexp, x, value) %>% spread(key = "x", value = "value") %>% 
  as.data.frame()

#Single regression ------------------
#Add general price level to run single regression model?
temp1 <- dfw %>% left_join(hicp_en %>% filter(coicop =="CP00") %>% rename(year = time, CP00price = values) %>%  
            select(year, CP00price), by = c("year"))
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


#Nõudluse süsteem -----
#https://cran.r-project.org/web/packages/micEconAids/vignettes/micEconAids_vignette.pdf
library("micEconAids")
laaidsResult <- aidsEst( priceNames, shareNames, "consexp", data = dfw,  priceIndex = "S" )
laaidsResult <- aidsEst( priceNames, shareNames, "consexp", data = dfw,  priceIndex = "P" )
laaidsResult <- aidsEst( priceNames, shareNames, "consexp", data = dfw,  priceIndex = "L" )
laaidsResult <- aidsEst( priceNames, shareNames, "consexp", data = dfw,  priceIndex = "Ls" )

laaidsResult <- aidsEst( priceNames, shareNames, "consexp", data = dfw,  priceIndex = "Ls", method = "IL" )
summary(laaidsResult)
print(laaidsResult)
elas(laaidsResult)

all.equal( sum( coef( laaidsResult )$alpha ), 1 )
all.equal( sum( coef( laaidsResult )$beta ), 0 )
all.equal( colSums( coef( laaidsResult )$gamma ), rep( 0, 12 ), check.attributes = FALSE )
all.equal( rowSums( coef( laaidsResult )$gamma ), rep( 0, 12 ), check.attributes = FALSE )
isSymmetric( coef( laaidsResult )$gamma, tol = 1e-10, check.attributes = FALSE )

pMeans <- colMeans( dfw[ , priceNames ] )
wMeans <- colMeans( dfw[ , shareNames ] )

aidsResultElas <- aidsElas( coef( laaidsResult ), prices = pMeans, shares = wMeans )
print( aidsResultElas )
xtMean <- mean( dfw[ , "consexp" ] )

aidsResultElasCov <- aidsElas( coef( laaidsResult ), prices = pMeans, totExp = xtMean, coefCov = vcov( laaidsResult ),
                               df = df.residual( laaidsResult ) )
summary( aidsResultElasCov )
