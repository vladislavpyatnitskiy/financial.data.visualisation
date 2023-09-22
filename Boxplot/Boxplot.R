# Libraries to install
lapply(c("quantmod",
         "PortfolioAnalytics",
         "timeSeries",
         "fBasics"
),
require,
character.only = TRUE
)

# Function to create boxplot
plot_of_box <- function(x, y = NULL, z = NULL, main = NULL){
  
  # Create empty variable to store values
  portfolioPrices <- NULL
  
  # For each ticker get data
  for (Ticker in x) 
    portfolioPrices <- cbind(portfolioPrices,
                             getSymbols(Ticker,
                                        from = y,
                                        to = z,
                                        src = "yahoo",
                                        auto.assign=FALSE)[,4])
  # Get rid of NAs
  portfolioPrices <- portfolioPrices[apply(portfolioPrices,
                                           1,
                                           function(x) all(!is.na(x))),]
  # Give column names 
  colnames(portfolioPrices) <- x
  
  # Make data discrete
  portfolioReturns <- ROC(portfolioPrices,
                          type = "discrete")
  
  # Make it Time Series
  portfolioReturns <-as.timeSeries(portfolioPrices)

  # Make log returns and get rid of NA
  portfolioReturns=diff(log(portfolioReturns))[-1,]
  
  # Boxplot
  boxPlot(portfolioReturns,
          main = main,
          title = FALSE,
          xlab = "Source: Yahoo Finance",
          ylab = "Returns"
  )
}
# Test
plot_of_box(c("AAPL", "MSFT", "META", "GOOGL", "AMZN"),
            y = "2020-01-01",
            main = "Boxplot of IT companies")
