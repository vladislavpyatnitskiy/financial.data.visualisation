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

chart_with_ma <- function(x){
  # For each column in dataset
  for (n in 1:ncol(x)){
    # Take column name
    name_for_MA <- colnames((x)[,n])
    
    # Make it string to put into plot title later
    main_for_MA <- sprintf("%s Stock Performance", name_for_MA)
    
    # Create chart itself
    new_chart_series_with_MA <- chartSeries(x[,n],
                name = main_for_MA, # title of plot
                theme = "black", # colour of plot
                TA="addEMA(50, col='purple');addEMA(200, col='red')")
  }
}
chart_with_ma(portfolioReturns)
