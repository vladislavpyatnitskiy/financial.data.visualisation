# Beta of a Stock Data Visualisation

lapply(c("quantmod",
         "dplyr"),
       require,
       character.only = TRUE)

tickers <- c("SBUX")

portfolioPrices <- NULL
for (Ticker in tickers) 
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols(Ticker,
                                      src = "yahoo",
                                      auto.assign=FALSE
                                      )[,4]
                           )
portfolioPrices <- portfolioPrices[apply(portfolioPrices,
                                         1,
                                         function(x) all(!is.na(x))
                                         ),
                                   ]
colnames(portfolioPrices) <- tickers

portfolioReturns <- ROC(portfolioPrices,
                        type = "discrete")
portfolioReturns <-as.timeSeries(portfolioPrices)

stock <- as.data.frame(getSymbols("SBUX",
                                  auto.assign=FALSE))
market <- as.data.frame(getSymbols("^GSPC",
                                   auto.assign=FALSE))

stock <- cbind(Date = rownames(stock),
               stock)
rownames(stock) <- 1:nrow(stock)

market <- cbind(Date = rownames(market),
                market)
rownames(market) <- 1:nrow(market)

data_frame <- inner_join(stock,
                         market,
                         by=c("Date"))
market_return <- diff(log(data_frame$GSPC.Adjusted))
stock_return <- diff(log(data_frame$SBUX.Adjusted))

fit <- lm(stock_return ~ market_return)
plot(market_return,
     stock_return,
     ylab="Stock Return (%)",
     xlab="Market Return (%)",
     main="Beta for Stock",
     sub = "Source: Yahoo Finance",
     las = 1)
abline(fit)
fit$coefficients[2]
