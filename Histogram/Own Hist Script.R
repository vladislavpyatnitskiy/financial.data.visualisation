lapply(c("quantmod","timeSeries"),require,character.only=T) # Libraries

hist.plt <- function(x, s = NULL, e = NULL, lg = T, data = F){ # Histogram Plot
  
  L <- NULL
  
  if (isTRUE(data)){ p <- NULL # data off
  
    src <- "yahoo"
    
    getData <- function(A, s, e) {
      if (is.null(s) && is.null(e)) return(getSymbols(A,src=src,auto.assign=F)) 
      if (is.null(e)) return(getSymbols(A, from = s, src=src, auto.assign=F)) 
      if (is.null(s)) return(getSymbols(A, to = e, src=src, auto.assign=F)) 
      return(getSymbols(A, from = s, to = e, src=src, auto.assign=F)) 
    }
    for (A in x){ p <- cbind(p, getData(A, s, e)[,4]) } # Join data
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Assign tickers
    
    x <- p } 
  
  if (isTRUE(lg) || isTRUE(data)){ x <- diff(log(x))[-1,] }
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each column
  
  h <- hist(s, main=sprintf("%s Histogram & Normal Distribution",colnames(s)),
            ylab = "Likelihood", xlab = "Returns", xlim = c(min(s), max(s)),
            col = "darkgreen", border = "white", breaks = 100, las=1, freq=F)
  
  grid(nx = NULL, ny = NULL, col = "grey", lty = "dotted", lwd = 1)
  abline(v = 0, col = "lightblue", lwd = 2) # Add vertical line at 0
  abline(h = 0)
  
  curve(dnorm(x, mean = mean(s), sd = sd(s)), col = "red", lwd = 3, add = T)
  
  box() } # Define borders
}
hist.plt(c("OMF", "HIG", "AAPL", "XOM"), s = "2020-01-01", data = T) # Test
