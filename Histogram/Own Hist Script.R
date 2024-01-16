lapply(c("quantmod","timeSeries"),require,character.only=T) # Libraries

hist.plt <- function(x, s = NULL, e = NULL, lg = T, data = F){ # Histogram Plot
  
  if (isTRUE(data)){ p <- NULL # data off
  
  for (A in x){ if (is.null(s) && is.null(e)){ # When dates are not defined
    
      p <- cbind(p, getSymbols(A, src = "yahoo", auto.assign = F)[,4])
      
    } else if (is.null(e)){ # When only start date is defined
      
      p <- cbind(p, getSymbols(A, from = s, src="yahoo", auto.assign = F)[,4])
      
    } else if (is.null(s)){ # When only end date is defined
      
      p <- cbind(p, getSymbols(A, to = e, src = "yahoo", auto.assign = F)[,4])
      
    } else { # When both start date and end date are defined
      
      p <- cbind(p,getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F)[,4]) } }
    
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- x # Assign tickers
  
  x <- p } 
  
  if (isTRUE(lg) || isTRUE(data)) { x <- diff(log(x))[-1,] }
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each column
  
    s.min <- min(s) # Minimum value
    s.max <- max(s) # Maximum value
    
    # Parameters
    hist(s, main = sprintf("%s",colnames(s)), freq = F, ylab = "Likelihood",
         xlab = "Histogram & Normal Distribution", xlim = c(s.min, s.max),
         col = "darkgreen", border = "white",breaks = 100, las = 1)
    
    for (n in seq(round(s.min, 1), round(s.max, 1), by=.05)){ # Add grey lines
      
      abline(v = n, col = "grey", lty = 3) } # Add Vertical lines
    
    abline(v = 0, col = "lightblue", lwd = 2) # Add vertical line at x = 0
    
    lines(seq(round(s.min,2),round(s.max,2),by=.0001), # Normal Distribution
          dnorm(seq(round(s.min,2),round(s.max,2),by=.0001),
                mean(s),sd(s)),col="black",lwd=2)
    
    for (n in seq(0,100,2)){ abline(h=n,col="grey",lty=3) } # Horizontal lines
    
    box() } # Define borders
}
hist.plt(c("OMF", "HIG"), s = "2020-01-01", data = T) # Test
