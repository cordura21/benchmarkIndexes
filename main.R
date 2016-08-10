
# load libraries ----------------------------------------------------------
library(readxl)
library(dplyr)
library(reshape2)
library(data.table) # for rbindlist
library(PerformanceAnalytics)
library(TTR)

# load prices -------------------------------------------------------------
prices.file <- 'benchmarks_prices.xlsx'
prices.sheets <- readxl::excel_sheets(prices.file)
prices <- data.frame()
for(fileLoop in 1:length(prices.sheets)){
  
  tmp.price <- readxl::read_excel(prices.file,sheet = prices.sheets[fileLoop])[,1:2]
  tmp.price$ticker <- prices.sheets[fileLoop]
  names(tmp.price) <- c('date','value','var')
  prices <- rbind(prices,tmp.price)
}

# Merval: merge official and blue chip dollar ------------------------------
dollar <- prices %>% filter(var %in% c('ccl','ars'))
dollar <- dollar %>% dcast(date ~ var, fun.aggregate = sum) %>%
  filter(!(is.na(date))) %>%
  mutate(dollar = ifelse(ccl == 0,ars,ccl)) %>%
  mutate(value = dollar,var = 'dollar_con_brecha') %>%
  select(date,value,var)



# Merval: add dividends ---------------------------------------------------
merval.div.yield <- prices %>% filter(var == 'merval_div_yield')
merval.mean.dy <-mean(merval.div.yield$value,na.rm = TRUE) / 100
merval.mean.dy<- log(merval.mean.dy  + 1)

merval.cost <- 0.70 / 100
merval.cost<- log(merval.cost  + 1)

merval.price <-  prices %>% filter(var == 'merval_price') %>%
  mutate(return = TTR::ROC(value,type = 'continuous')) %>%
  mutate(net.return = return + (merval.mean.dy - merval.cost) / 12) %>%
  mutate(net.return = ifelse(is.na(net.return),0,net.return)) %>%
  mutate(cumReturn = cumsum(net.return)) %>%
  mutate(base100 = 100 * (1+ exp(cumReturn)-1)) %>%
  mutate( var = 'merval_price_TR_net') %>%
  select(date,base100,var)

names(merval.price) <- c('date','value','var')

# Merval: convert to dollars ----------------------------------------------

merval.total.return.index <- merge(merval.price, dollar,by = 'date') %>%
  mutate(value = value.x / value.y, var = 'merval_TR_net_USD') %>%
  select(date,value,var)

prices <- rbind(prices,merval.total.return.index) # Add to prices object


# Define Portfolios -------------------------------------------------------
benchmarks <- list()
benchmarks$alfa <- list()
benchmarks$alfa$rebalancing <- 'months'
benchmarks$alfa$securities <- c('cta', 'vol')
benchmarks$alfa$weights <- c(.80,.20)

benchmarks$latam <- list()
benchmarks$latam$rebalancing <- NA # No sé si es NA o 'none'
benchmarks$latam$securities <- c('ilf','merval_TR_net_USD')
benchmarks$latam$weights <- c(.50,.50)

benchmarks$five.assets <- list()
benchmarks$five.assets$rebalancing <- 'years'
benchmarks$five.assets$securities <- c('eem',
                                       'gsg',
                                       'ief',
                                       'iyr',
                                       'rf',
                                       'spy')
benchmarks$five.assets$weights <- c(0.4 / 5,
                                    0.4 / 5,
                                    0.4 / 5,
                                    0.4 / 5,
                                    .60,
                                    0.4 / 5)

enchmarks$five.assets.risk.on <- list()
benchmarks$five.assets.risk.on$rebalancing <- 'years'
benchmarks$five.assets.risk.on$securities <- c('eem',
                                       'gsg',
                                       'ief',
                                       'iyr',
                                       'spy')

benchmarks$five.assets.risk.on$weights <- c(.20,
                                    .20,
                                    .20,
                                    .20,
                                    .20)



# Calculate Alfa Benchmark ------------------------------------------------


myBenchmarks <- list()
myBenchmarks$alfa <- prices %>% filter(var %in% benchmarks$alfa$securities) %>%
mutate(date = as.yearmon(date))

myBenchmarks$alfa <- dcast(myBenchmarks$alfa,date ~ var, fun.aggregate = sum) 
myBenchmarks$alfa <- xts(myBenchmarks$alfa[,-1],myBenchmarks$alfa[,1])
index(myBenchmarks$alfa) <- as.Date(index(myBenchmarks$alfa),1)
myBenchmarks$alfa <- myBenchmarks$alfa[!is.na(index(myBenchmarks$alfa)),]
myBenchmarks$alfa <- TTR::ROC (myBenchmarks$alfa,type = 'discrete')

myBenchmarks$alfa <- Return.portfolio(myBenchmarks$alfa,
                                      weights = benchmarks$alfa$weights,
                                      rebalance_on = benchmarks$alfa$rebalancing)



# Calculate Latam Benchmark -----------------------------------------------

myBenchmarks$latam <- prices %>% filter(var %in% benchmarks$latam$securities) %>%
  mutate(date = as.yearmon(date))

myBenchmarks$latam <- dcast(myBenchmarks$latam,date ~ var, fun.aggregate = sum) 
myBenchmarks$latam <- xts(myBenchmarks$latam[,-1],myBenchmarks$latam[,1])
index(myBenchmarks$latam) <- as.Date(index(myBenchmarks$latam),1)
myBenchmarks$latam <- myBenchmarks$latam[!is.na(index(myBenchmarks$latam)),]
myBenchmarks$latam <- TTR::ROC (myBenchmarks$latam,type = 'discrete')

myBenchmarks$latam <- Return.portfolio(myBenchmarks$latam,
                                      weights = benchmarks$latam$weights,
                                      rebalance_on = benchmarks$latam$rebalancing)

# Calculate 5 Assets Benchmark -----------------------------------------------

myBenchmarks$five.assets <- prices %>% filter(var %in% benchmarks$five.assets$securities) %>%
  mutate(date = as.yearmon(date))

myBenchmarks$five.assets <- dcast(myBenchmarks$five.assets,date ~ var, fun.aggregate = sum) 
myBenchmarks$five.assets <- xts(myBenchmarks$five.assets[,-1],myBenchmarks$five.assets[,1])
index(myBenchmarks$five.assets) <- as.Date(index(myBenchmarks$five.assets),1)
myBenchmarks$five.assets <- myBenchmarks$five.assets[!is.na(index(myBenchmarks$five.assets)),]
myBenchmarks$five.assets <- TTR::ROC (myBenchmarks$five.assets,type = 'discrete')

myBenchmarks$five.assets <- Return.portfolio(myBenchmarks$five.assets,
                                       weights = benchmarks$five.assets$weights,
                                       rebalance_on = benchmarks$five.assets$rebalancing)


# Calculate 5 Assets Benchmark, Risk On -----------------------------------------------

myBenchmarks$five.assets.risk.on <- prices %>% filter(var %in% benchmarks$five.assets.risk.on$securities) %>%
  mutate(date = as.yearmon(date))

myBenchmarks$five.assets.risk.on <- dcast(myBenchmarks$five.assets.risk.on,date ~ var, fun.aggregate = sum) 
myBenchmarks$five.assets.risk.on <- xts(myBenchmarks$five.assets.risk.on[,-1],myBenchmarks$five.assets.risk.on[,1])
index(myBenchmarks$five.assets.risk.on) <- as.Date(index(myBenchmarks$five.assets.risk.on),1)
myBenchmarks$five.assets.risk.on <- myBenchmarks$five.assets.risk.on[!is.na(index(myBenchmarks$five.assets.risk.on)),]
myBenchmarks$five.assets.risk.on <- TTR::ROC (myBenchmarks$five.assets.risk.on,type = 'discrete')

myBenchmarks$five.assets.risk.on <- Return.portfolio(myBenchmarks$five.assets.risk.on,
                                             weights = benchmarks$five.assets.risk.on$weights,
                                             rebalance_on = benchmarks$five.assets.risk.on$rebalancing)
