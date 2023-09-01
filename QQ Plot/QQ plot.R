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

qqgraph <- function(x){
  # Calculate Returns
  x=diff(log(x))
  
  # Get rid of NA
  x <- x[-1,]
  
  # Loop enables to generate multiple plots if there are more than 1   
  for (n in 1:ncol(x)){
    # Take ticker from Time Series to reflect on plot
    qqstocknames <- colnames(x[,n])
    
    # Create variable to reflect in main section of plot settings
    qqmain <- sprintf("%s Q-Q Plot", qqstocknames)
    
    # Plot QQ plot
    qqnorm(x[,n],
           main = qqmain,
           sub = "Source: Yahoo Finance"
    )
    
    # Add line
    qqline(x[,n],
           col = "red",
           lwd = 3)
  }
}

qqgraph(portfolioReturns)
