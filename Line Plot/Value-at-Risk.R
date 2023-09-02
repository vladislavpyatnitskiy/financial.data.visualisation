# Performance Analytics

lapply(c("quantmod", "fGarch", "PerformanceAnalytics", "timeSeries"),
       require, character.only = TRUE)

tickers <- c("AMZN", "GOOGL")

portfolioPrices <- NULL
for (Ticker in tickers) 
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols(Ticker, from = "2020-02-20",
                                      src = "yahoo", auto.assign=FALSE)[,4])


portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
                                         function(x) all(!is.na(x))),]

colnames(portfolioPrices) <- tickers

portfolioReturns <- ROC(portfolioPrices, type = "discrete")
portfolioReturns <-as.timeSeries(portfolioPrices)

val_risk <- function(x){
  # Calculate Returns
  x=diff(log(x))
  # Delete row with NA
  x <- x[-1,]
  # For each column in dataset
  for (n in 1:ncol(x)){
    # Set index
    t <- index(x[,n])
    
    # Calculate mean
    ybar <- mean(x[,n])
    
    # Set up GARCH model
    garchmodel1 <- garchFit( ~ garch(1,1), data=coredata(x[,n]), trace=FALSE)
    
    # Calculate VaR at 95%
    var5.garch <- ybar - 1.645 * garchmodel1@sigma.t
    
    # Calculate VaR at 99%
    var1.garch <- ybar - 2.326 * garchmodel1@sigma.t
    
    # Calculate VaR at 99.9%
    var0.1.garch <- ybar - 3.09 * garchmodel1@sigma.t
    
    # Take column name
    name_for_column_VaR <- colnames(x[,n])
      
    # Put column name into string be displayed on plot later
    main_name_for_VaR <- sprintf("%s VaR GARCH(1,1)", name_for_column_VaR)
    
    # Plot graph
    plot(t, x[,n], type="h",
         xlab = "Trading Days",
         ylab = "Returns",
         main =main_name_for_VaR,
         col = "black",
         sub = "Source: Yahoo! Finance")
    
    # Create lines of each VaR value
    line_for_VaR_95 <- lines(t, var5.garch, col ="green")
    line_for_VaR_99 <- lines(t, var1.garch, col ="blue")
    line_for_VaR_999 <- lines(t, var0.1.garch, col ="red")
    
    # Reflect them on plot
    line_for_VaR_95
    line_for_VaR_99
    line_for_VaR_999
  }
}
val_risk(portfolioReturns)
