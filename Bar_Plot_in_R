# Barplot

options(max.print=1000000)

lapply(c("quantmod",
         "PortfolioAnalytics",
         "timeSeries",
         "fBasics"
         ),
       require,
       character.only = TRUE
       )


tickers <- c("AIG",
             "MET",
             "UNM",
             "HIG",
             "OMF"
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

brplt <- function(x){
  # Define the time period
  lgrn <- merge(x[1,],
                x[nrow(x),])
  
  # Calculate the change
  lgrn=diff(log(lgrn))
  lgrn<-lgrn[-1,]
  
  #
  colnames(lgrn)
  lgrn1 <- as.data.frame(lgrn)
  lgrn1 <- sort(lgrn1, decreasing = T)
  tickers_for_barplot <- colnames(lgrn1)
  lgrn <- as.numeric(lgrn)
  lgrn <- sort(lgrn, decreasing = T)
  
  # Create barplot
  barplot(lgrn,
          names.arg = tickers_for_barplot,
          horiz = T,
          las=1,
          col = "blue",
          main = "Insurance Stocks Performance for the Year",
          sub = "Source: Yahoo! Finance",
          xlab = "",
          ylab = "Return"
  )
  
}

brplt(portfolioReturns)
