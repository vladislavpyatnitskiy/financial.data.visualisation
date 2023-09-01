options(max.print=1000000)

lapply(c("quantmod",
         "PortfolioAnalytics",
         "timeSeries",
         "fBasics",
         "dplyr"
),
require,
character.only = TRUE
)

tickers <- c("AIG",
             "MET",
             "UNM",
             "HIG",
             "OMF",
             "^GSPC"
)
start_date <- "2023-01-10"

portfolioPrices <- NULL
for (Ticker in tickers) 
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols(Ticker,
                                      from = start_date,
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

portfolioReturns

betaplot <- function(x){
  # Calculate Returns
  x=diff(log(portfolioReturns))
  
  # Get rid of NA
  x <- x[-1,]
  
  # Create a Separate Time Series for Index
  market_return <- x$`^GSPC`
  
  # Create Separate Time Series for Stock Returns
  stock_returns <- x[,1:(ncol(x) - 1)]
  
  # Loop enables to generate multiple plots if there are more than 1   
  for (n in 1:ncol(stock_returns)) {
    # Run a regression (CAPM) 
    fit <- lm(stock_returns[,n] ~ market_return)
    
    # Take ticker from Time Series to reflect on plot
    name_beta_stock <- colnames(stock_returns[,n])
    
    # Create variable to reflect in ylab section of plot settings
    beta_y_lab <- sprintf("%s Return (%%)", name_beta_stock)
    
    # Create variable to reflect in main section of plot settings
    beta_main <- sprintf("%s Beta", name_beta_stock)
    
    # Create Scatter Plot
    plot_for_beta <- plot(market_return,
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
betaplot(portfolioReturns)
