# Install and activate these 4 packages in RStudio
lapply(c("quantmod",
         "PortfolioAnalytics",
         "timeSeries",
         "fBasics"
         ),
       require,
       character.only = TRUE
       )

# Then, select tickers of stocks you want to get data of
tickers <- c("1801.HK",
             "6618.HK",
             "1093.HK",
             "1177.HK",
             "2269.HK"
             )
             
# Set up any start date you need  
start_date <- "2017-10-17"

# Here you can set up time period that you would like to analyse
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

# Get first values of stock prices
head(portfolioReturns)

# Of course you want to get returns
lrtn=diff(log(portfolioReturns)
          )
lrtn <- lrtn[-1,]

# Boxplot
boxPlot(lrtn,
        main = "Hong Kong Stocks",
        title = FALSE,
        xlab = "Source: Yahoo Finance",
        ylab = "Returns"
        )
