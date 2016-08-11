portfolio.global <- data.frame()
# Add Alfa Benchmark
portfolio.global <- myBenchmarks$alfa.bmk
# Add Beta Benchmark
portfolio.global <- merge(portfolio.global,myBenchmarks$five.assets.bmk)
# Add Liquid real estate
portfolio.global <- merge(portfolio.global,myBenchmarks$five.assets[,'iyr'])
# Add Merval
portfolio.global <- merge(portfolio.global,
                          myBenchmarks$latam[,"merval_TR_net_USD"])
# Add Latam
portfolio.global <- merge(portfolio.global,
                          myBenchmarks$latam[,"ilf"])
# Add risk free rate
portfolio.global <- merge(portfolio.global,
                          myBenchmarks$five.assets[,"rf"])
#Rename variables
names(portfolio.global) <- c('Alfa', 'Beta Ajustado',
                             'Real Estate (IYR)', 'Merval',
                             'Latam 40 ETF', 'Risk Free (1 month T-Bill)')
portfolio.global <- portfolio.global[,order(names(portfolio.global))]

portfolio.global.wgts <- c(.25,.25,.12,.08,.20,.10)

portfolio.global.bmk <- Return.portfolio(R = portfolio.global,
                                         weights = portfolio.global.wgts,
                                         rebalance_on = 'years')

date.interval <- '2009-01::2016-06'
Return.annualized(portfolio.global.bmk[date.interval,]) %>% as.data.frame() %>%formattable()
maxDrawdown(portfolio.global.bmk[date.interval,]) %>% 
  as.data.frame() %>% t() %>% formattable()

Return.annualized(portfolio.global[date.interval,]) %>% 
 t()%>% as.data.frame()%>% formattable()
maxDrawdown(portfolio.global[date.interval,]) %>%
  t()%>% as.data.frame()%>% formattable()


