# Libraries you need
lapply(c("quantmod",
         "PortfolioAnalytics",
         "timeSeries",
         "fBasics",
         "dplyr"
),
require,
character.only = TRUE
)

# Type stocks you want to get Beta for
tickers_for_test <- c("AIG", "MET", "UNM", "HIG", "OMF")

# Function to visualise Beta Scatter Plot
betaplot <- function(y, bnchm = "^GSPC", start_date = NULL, end_date = NULL){

  # Add benchmark to list
  y <- c(y, bnchm)
  
  # Create an empty variable
  portfolioPrices <- NULL
  
  # Loop for data extraction
  for (Ticker in y){
    # Set up statements for start and end dates
    if (is.null(start_date) && is.null(end_date)) {
      # When neither start date nor end date are defined
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols(Ticker,
                                          from = as.Date(Sys.Date()) - 365,
                                          to = Sys.Date(),
                                          src = "yahoo",
                                          auto.assign=FALSE)[,4])
    } else { 
      # When both start date and end date are defined
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols(Ticker,
                                          from = start_date,
                                          to = end_date,
                                          src = "yahoo", 
                                          auto.assign=FALSE)[,4]) }
  }
  # Get rid of NAs
  portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
                                           function(x) all(!is.na(x))),]
  # Put the tickers in data set
  colnames(portfolioPrices) <- y
  
  # Make data discrete
  portfolioReturns <- ROC(portfolioPrices, type = "discrete")
  
  # Make it time series
  portfolioReturns <-as.timeSeries(portfolioPrices)
  
  # Calculate Returns and get rid of NA
  x=diff(log(portfolioReturns))[-1,]
  
  # Copy column index column and make separate column
  spx <- x[,bnchm]
  
  # Subset index from data set
  stock_returns <- x[, -which(names(x) == bnchm)]
  
  # Loop generates multiple plots if there are more than 1 for each column   
  for (n in 1:ncol(stock_returns)) { security <- stock_returns[,n]
    
    # Create Scatter Plot
    plot_for_beta <- plot(spx,
                          security,
                          ylab=sprintf("%s Return (%%)", colnames(security)),
                          xlab="Market Return (%)",
                          main=sprintf("%s Beta", colnames(security)),
                          sub = "Source: Yahoo Finance",
                          las = 1)
    # Draw a trend line
    abline_for_beta <- abline(lm(security ~ spx), col = "red", lwd = 3) }
}
# Test
betaplot(tickers_for_test, bnchm = "^GSPC")
