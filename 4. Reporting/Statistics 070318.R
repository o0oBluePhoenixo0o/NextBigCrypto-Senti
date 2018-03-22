library(httr)
library(RCurl)
library(crypto)
library(formatR)
library(yaml)
library(googleVis)


op <- options(gvis.plot.tag="chart")

coins <- crypto::listCoins()

data1 <- crypto::getCoins(coin = c("Bitcoin", "Ethereum", "Ripple", "Bitcoin Cash", 
                                   "Litecoin"))
p = gvisAnnotationChart(data1, idvar = "name", "date", "market", 
                        options = list(title = "Market Cap Trend", 
                        legend = "top"))
plot(p)
install.packages("treemap")
library(treemap)
library(coinmarketcapr)
market_today <- get_marketcap_ticker_all()
head(market_today[,1:8])
df1 <- na.omit(market_today[,c('id','market_cap_usd')])
df1$market_cap_usd <- as.numeric(df1$market_cap_usd)
df1$formatted_market_cap <-  paste0(df1$id,'\n','$',format(df1$market_cap_usd,big.mark = ',',scientific = F, trim = T))
png('Cryptocurency Market Cap 8th March 2018.png', width = 800, height = 800)
treemap(df1, index = 'formatted_market_cap', vSize = 'market_cap_usd', title = 'Cryptocurrency Market Cap', fontsize.labels=c(12, 8), palette='RdYlGn')
dev.off()
