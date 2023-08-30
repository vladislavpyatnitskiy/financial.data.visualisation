lapply(c("quantmod",
         "ggplot2",
         "tidyverse"),
       require,
       character.only = TRUE)

tickers <- c("F")

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

portfolioReturns <- ROC(portfolioPrices, type = "discrete")
portfolioReturns <-as.timeSeries(portfolioPrices)

portfolioReturns

lrtn=diff(log(portfolioReturns))
lrtn <- lrtn[-1,]

qqnorm(lrtn)
qqline(lrtn, col = "red")
