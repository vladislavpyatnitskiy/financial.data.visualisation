lapply(c("quantmod",
         "ggplot2",
         "tidyverse",
         "timeSeries"),
       require,
       character.only = TRUE)

# Chart with Moving Averages
chart_with_ma <- function(tickers, start_date = NULL,
                          end_date = NULL, data = T){ 
  
  # When data is needed create empty variable to contain values
  if (isFALSE(data)){ portfolioPrices <- NULL
  
  # Data download
  for (Ticker in tickers) 
    portfolioPrices <- cbind(portfolioPrices,
                             getSymbols(Ticker,
                                        from = start_date,
                                        to = end_date,
                                        src = "yahoo",
                                        auto.assign=FALSE)[,4])
  
  # Remove NAs
  portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
                                           function(x) all(!is.na(x))),]
  # Give tickers to data
  colnames(portfolioPrices) <- tickers
  
  # Make data discrete
  portfolioReturns <- ROC(portfolioPrices, type = "discrete")
  
  # Make data time series
  portfolioReturns <-as.timeSeries(portfolioPrices) }
  
  # For each column in data set
  for (n in 1:ncol(portfolioReturns)){ security <- portfolioReturns[,n] 

    # Create chart itself
    new_chart_series_with_MA <- chartSeries(round(security, 2),
                         name = sprintf("%s Stock Performance",
                        colnames(security)), theme = "white", # colour of plot
                      TA="addEMA(50, col='purple');addEMA(200, col='red')") }
}
# Test
chart_with_ma(tickers = "OMF", start_date = "2022-01-01",
              end_date = "2023-01-01", data = F)
