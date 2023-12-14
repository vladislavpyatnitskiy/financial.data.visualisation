# Function to generate Q-Q plot
qq.plt <- function(x, lg = F, data = F, s = NULL, e = NULL){ 
  
  if (isTRUE(data) && (isTRUE(lg) || isFALSE(lg))){ p <- NULL # data off
  
  for (A in x){ if (is.null(s) && is.null(e)) { # When dates are not defined
    
      p <- cbind(p, getSymbols(A, src = "yahoo", auto.assign = F)[,4])
    
    } else if (is.null(e)) { # When only start date is defined
    
      p <- cbind(p, getSymbols(A, from = s, src="yahoo", auto.assign=F)[,4])
    
    } else if (is.null(s)) { # When only end date is defined
    
      p <- cbind(p, getSymbols(A, to = e, src="yahoo", auto.assign = F)[,4])
    
    } else { # When both start date and end date are defined
    
      p <- cbind(p,getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F)[,4]) } }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- x # Put tickers as column names
  
  x = diff(log(as.timeSeries(p)))[-1,] # Make it time series, log and NA off 
  
  } else if (isFALSE(data) && isTRUE(lg)) { x = diff(log(x))[-1, ] } # data on
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each asset make qq plot
  
    qqnorm(s, main = sprintf("%s Q-Q Plot", colnames(s)), las = 1) # QQ plot
    
    axis(side = 2, at = seq(-1, 1, .01), las = 1) # Y-axis with 0.01 intervals
    
    qqline(s, col = "red", lwd = 3)  # Add line
  
    # Add grey dotted vertical & horizontal lines
    for (n in seq(-3, 3, 1)){ abline(v = n, col = "grey", lty = 3) }
    for (n in seq(-1, 1, .01)){ abline(h = n, col = "grey", lty = 3) } }
}
qq.plt(x = c("AIG","MET","HIG","UNM","OMF"),lg=T,data=T,s="2023-10-01") # Test
