# Plot Histogram
hist.plt <- function(x, lg = T){
  
  # Calculate log returns and remove NA if necessary
  if (isTRUE(lg)) { x <- diff(log(x))[-1,] }
  
  # For each column
  for (n in 1:ncol(x)){ security <- x[,n]
    
    # Minimum value
    min_sec <- min(security)
  
    # Maximum value
    max_sec <- max(security)
      
    # Parameters
    hist(security,
         main=sprintf("%s",colnames(security)),
         freq = F,
         xlab = "Histogram & Normal Distribution",
         ylab = "Likelihood",
         las = 1,
         xlim = c(min_sec, max_sec),
         col = "darkgreen",
         border = "white",
         breaks = 100, # Number of bins
         )
    
    # Add grey lines for fast visual percentage calculation
    for (n in seq(round(min_sec, 1), round(max_sec, 1), by = 0.05)){ 
      
      # Add Vertical lines
      abline(v = n, col = "grey", lty = 3) }
    
    # Add vertical line at x = 0
    abline(v = 0, col = "lightblue", lwd = 2)
    
    # Add Normal Distribution
    lines(seq(round(min_sec, 2), round(max_sec, 2), by = 0.0001),
          dnorm(seq(round(min_sec, 2), round(max_sec, 2), by = 0.0001),
                mean(security),
                sd(security)),
          col="black", lwd = 2)
    
    # Horizontal lines
    for (n in seq(0, 100, 2)){ abline(h = n, col = "grey", lty = 3) }
    
    # Define borders
    box() }
}
# Test
hist.plt(stock_data)
