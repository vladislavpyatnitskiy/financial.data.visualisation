lapply(c("quantmod", "timeSeries"), require, character.only = T) # Library

# Chart with Moving Averages
ma.plt <- function(tickers, s = NULL, e = NULL, data = T){ # Variable for data
  
  if (isFALSE(data)){ p <- NULL # Data download loop for 4 scenarios:
  
  for (Ticker in tickers){ if (is.null(s) && is.null(e)) { # none is typed
    
    p <- cbind(p, getSymbols(Ticker, src = "yahoo", auto.assign=F)[,4])
    
  } else if (is.null(e)) { # When only start date is defined
    
    p <- cbind(p, getSymbols(Ticker, from = s,src="yahoo",auto.assign=F)[,4])
    
  } else if (is.null(s)) { # When only end date is defined
    
    p <- cbind(p,getSymbols(Ticker,to=e,src="yahoo",auto.assign=F)[,4])
    
  } else { # When both start date and end date are defined
    
    p<-cbind(p,getSymbols(Ticker,from=s,to=e,src="yahoo",auto.assign=F)[,4])} }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Remove NA
  
  colnames(p) <- tickers # Give tickers to data
  
  r <- as.timeSeries(p) # Make data time series
  
  for (n in 1:ncol(r)){ # Create plot for each column
  
    chartSeries(round(r[,n], 2), theme = "white",
                name=sprintf("%s Stock Performance",colnames(r[,n])),
                TA="addEMA(50, col='purple');addEMA(200, col='red')")} } else {
                  
  for (n in 1:ncol(tickers)){ # Create plot for each column when data out Yahoo
                    
    chartSeries(round(tickers[,n], 2), theme = "white",
                name=sprintf("%s Stock Performance",colnames(tickers[,n])),
                TA="addEMA(50, col='purple');addEMA(200, col='red')") } }
}
ma.plt(tickers = "OMF", s = "2022-01-01", e = "2023-01-01", data = F) # Test
