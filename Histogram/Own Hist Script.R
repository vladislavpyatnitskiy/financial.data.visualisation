lapply(c("quantmod","timeSeries"),require,character.only=T) # Libraries

hist.plt <- function(x, s = NULL, e = NULL, lg = T, data = F){ # Histogram Plot
  
  L <- NULL
  
  if (isTRUE(data)){ p <- NULL # data off
  
    for (A in x){ if (is.null(s) && is.null(e)) { 
      
        q <- getSymbols(A, src = "yahoo", auto.assign = F)
        
      } else if (is.null(e)){ q<-getSymbols(A,from=s,src="yahoo",
                                            auto.assign=F)
      
      } else if (is.null(s)){ q<-getSymbols(A,to=e,src="yahoo",auto.assign=F)
      
      } else { q <- getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F) }
        
      p <- cbind(p, q[,4]) } # Join all columns into one data frame
      
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Assign tickers
    
    x <- p } 
    
  if (isTRUE(lg) || isTRUE(data)){ x <- diff(log(x))[-1,] }
    
  for (n in 1:ncol(x)){ s <- x[,n] # For each column
    
    h <- hist(s, main=sprintf("%s Histogram & Normal Distribution",colnames(s)),
              ylab = "Likelihood", xlab = "Returns", xlim = c(min(s), max(s)),
              col = "darkgreen", border = "white", breaks = 100, las=1, freq=F)
    
    
    abline(v = 0, col = "lightblue", lwd = 2) # Add vertical line at 0
    abline(h = 0)
    
    grid(nx = NULL, ny = NULL, col = "grey", lty = "dotted", lwd = 1)
    
    curve(dnorm(x, mean = mean(s), sd = sd(s)), col = "red", lwd = 3, add = T)
    
    box() } # Define borders
}
hist.plt(c("OMF", "HIG", "AAPL", "XOM"), s = "2020-01-01", data = T) # Test
