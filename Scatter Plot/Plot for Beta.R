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
betaplot <- function(y, z = NULL, i = NULL){
  
  # Define Yahoo ticker for S&P 500
  spx <- "^GSPC"
  
  # Add 10 year Treasuries to list
  y <- c(y, spx)
  
  # Create an empty variable
  portfolioPrices <- NULL
  
  # Loop for data extraction
  for (Ticker in y){
           
    # Set up statements for start and end dates
    if (is.null(z) && is.null(i)) {
      # When neither start date nor end date are defined
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols(Ticker,
                                          from = as.Date(Sys.Date()) - 365,
                                          to = Sys.Date(),
                                          src = "yahoo",
                                          auto.assign=FALSE)[,4])
    } else if (is.null(i)) {
      # When only start date is defined
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols(Ticker, from = z, src = "yahoo",
                                          auto.assign=FALSE)[,4])
    } else if (is.null(z)) {
      # When only end date is defined
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols(Ticker, to = i, src = "yahoo",
                                          auto.assign=FALSE)[,4])
    } else { 
      # When both start date and end date are defined
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols(Ticker, from = z, to = i,
                                          src = "yahoo", 
                                          auto.assign=FALSE)[,4])
    }
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
  
  # Copy column S&P 500 column and make separate column
  spx <- x[,"^GSPC"]
  
  # Subset S&P 500 from data set
  stock_returns <- x[, -which(names(x) == "^GSPC")]
  
  # Loop enables to generate multiple plots if there are more than 1   
  for (n in 1:ncol(stock_returns)) {
    
    # Run a regression (CAPM) 
    fit <- lm(stock_returns[,n] ~ spx)
    
    # Take ticker from Time Series to reflect on plot
    name_beta_stock <- colnames(stock_returns[,n])
    
    # Create variable to reflect in ylab section of plot settings
    beta_y_lab <- sprintf("%s Return (%%)", name_beta_stock)
    
    # Create variable to reflect in main section of plot settings
    beta_main <- sprintf("%s Beta", name_beta_stock)
    
    # Create Scatter Plot
    plot_for_beta <- plot(spx,
                          stock_returns[,n],
                          ylab=beta_y_lab,
                          xlab="Market Return (%)",
                          main=beta_main,
                          sub = "Source: Yahoo Finance",
                          las = 1
    )
    
    # Draw a trend line
    abline_for_beta <- abline(fit, col = "red", lwd = 3)
  }
}
# Test
betaplot(tickers_for_test)
