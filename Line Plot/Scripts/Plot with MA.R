lapply(c("quantmod", "timeSeries"), require, character.only = T) # Library

ma.plt <- function(x, s = NULL, e = NULL, data = T){ # Plot of Moving Averages
  
  if (isTRUE(data)){ p <- NULL # Data download loop for 4 scenarios:
  
    src <- "yahoo"
    
    getData <- function(A, s, e) {
      if (is.null(s) && is.null(e)) return(getSymbols(A,src=src,auto.assign=F)) 
      if (is.null(e)) return(getSymbols(A, from = s, src=src, auto.assign=F)) 
      if (is.null(s)) return(getSymbols(A, to = e, src=src, auto.assign=F)) 
      return(getSymbols(A, from = s, to = e, src=src, auto.assign=F)) 
    }
    for (A in x){ p <- cbind(p, getData(A, s, e)[,4]) } # Join data
    
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
