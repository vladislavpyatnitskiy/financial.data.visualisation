# Plot Histogram  (Calculate log returns & ramove NA if necessary)
hist.plt <- function(x, lg = T){ if (isTRUE(lg)) { x <- diff(log(x))[-1,] }
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each column
  
    s.min <- min(s) # Minimum value
    s.max <- max(s) # Maximum value
    
    # Parameters
    hist(s,main=sprintf("%s",colnames(s)),freq=F,ylab="Likelihood",las=1,
         xlab = "Histogram & Normal Distribution", xlim=c(s.min, s.max),
         col = "darkgreen",border = "white",breaks = 100)
    
    for (n in seq(round(s.min,1),round(s.max,1),by=.05)){ # Add grey lines
      
      abline(v = n, col = "grey", lty = 3) } # Add Vertical lines
    
    abline(v = 0, col = "lightblue", lwd = 2) # Add vertical line at x = 0
    
    lines(seq(round(s.min,2),round(s.max,2),by=.0001), # Normal Distribution
          dnorm(seq(round(s.min,2),round(s.max,2),by=.0001),
                mean(s),sd(s)),col="black",lwd=2)
    
    for (n in seq(0,100,2)){ abline(h=n,col="grey",lty=3) } # Horizontal lines
  
  box() } # Define borders
}
# Test
hist.plt(stock_data)
