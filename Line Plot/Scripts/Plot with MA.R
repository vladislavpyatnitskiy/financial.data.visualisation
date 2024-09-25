lapply(c("quantmod", "timeSeries"), require, character.only = T) # Library

ma.plt <- function(x, s = NULL, e = NULL, data = T){ # Plot of Moving Averages
  
  if (isTRUE(data)){ p <- NULL # Data download loop for 4 scenarios:
  
    for (A in x){ if (is.null(s) && is.null(e)) { 
      
        q <- getSymbols(A, src = "yahoo", auto.assign = F)
      
      } else if (is.null(e)){ q<-getSymbols(A,from=s,src="yahoo",auto.assign=F)
    
      } else if (is.null(s)){ q<-getSymbols(A,to=e,src="yahoo",auto.assign=F)
    
      } else { q <- getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F) }
      
      p <- cbind(p, q[,4]) } # Join all columns into one data frame
    
      p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Remove NA
    
      colnames(p) <- x
      
      x <- p } # Give tickers to data
    
  r <- as.timeSeries(x) # Make data time series
  
  for (n in 1:ncol(r)){
  
    chartSeries(round(r[,n], 2), theme = "white",
                name = sprintf("%s Stock Performance", colnames(r[,n])),
                TA = "addEMA(50, col='purple');addEMA(200, col='red')") }
}
ma.plt(x = "AIG", s = "2022-01-01", data = T) # Test
