lapply(c("quantmod",
         "ggplot2",
         "tidyverse",
         "timeSeries"),
       require,
       character.only = TRUE)

tickers <- c("SBUX", "C")
start_date <- "2022-04-10"

portfolioPrices <- NULL
for (Ticker in tickers) 
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols(Ticker,
                                      from = start_date,
                                      src = "yahoo",
                                      auto.assign=FALSE)[,4])


portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
                                         function(x) all(!is.na(x))),]

colnames(portfolioPrices) <- tickers

portfolioReturns <- ROC(portfolioPrices, type = "discrete")
portfolioReturns <-as.timeSeries(portfolioPrices)

# Chart with Moving Averages
chart_with_ma <- function(x){ for (n in 1:ncol(x)){ security <- x[,n]
    
    # Create chart itself
    new_chart_series_with_MA <- chartSeries(round(security, 2),
                name = sprintf("%s Stock Performance", colnames(security)), 
                theme = "white", # colour of plot
                TA="addEMA(50, col='purple');addEMA(200, col='red')") }
}
# Test
chart_with_ma(portfolioReturns)
