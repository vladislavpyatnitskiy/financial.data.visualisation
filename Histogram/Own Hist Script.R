# Plot Histogram
hist.plt <- function(x, lg = T){
  
  # Calculate log returns and remove NA if necessary
  if (isTRUE(lg)) { x <- diff(log(x))[-1,] }
  
  # For each column
  for (n in 1:ncol(x)){ security <- x[,n]
    
    # Parameters
    hist(security, main = sprintf("%s Histogram", colnames(security)),
         freq = F, xlab = "Returns Distribution", ylab = "Likelihood", las = 1,
         xlim = c(round(min(security),2), round(max(security),2)),
         col = "darkred", breaks = 100, density = T)

    # Add grey lines for fast visual percentage calculation
    for (n in seq(round(min(security), 1), round(max(security), 1),
                  by = 0.05)){ abline(v = n, col = "grey", lty = 3) }
    
    # Line x = 0
    abline(v = 0, col = "lightblue")
                       
    # Horizontal lines
    for (n in seq(0, 100, 2)){ abline(h = n, col = "grey", lty = 3) }
      
    # Define borders
    box() }
}
# Test
hist.plt(stock_data)
