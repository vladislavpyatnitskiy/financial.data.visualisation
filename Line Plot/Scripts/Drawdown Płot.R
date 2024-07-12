drawdown.plt <- function(x, SD = T){ # Function to plot asset drawdown
  
  x <- diff(log(x)) # Log returns
  
  x[1,] <- 0 # Assign first value as 0
  
  if (isFALSE(SD)){ r <- rownames(x) # Save dates for Cumulative Returns
  
    x <- apply(x, 2, function(col) exp(cumsum(col)) - 1) # Cumulative Returns
    
    rownames(x) <- r } # Return dates as row names as they were vanished 
  
  x <- x * 100 # Multiply returns by 100
  
  x[x > 0] <- 0 # Replace positive values as 0
  
  for (n in 1:ncol(x)){ s <- x[,n] # Plot each column in data frame
  
    plot(s, type = "l", las = 2, ylim = c(min(s), 0), lwd = 1, col = "red",
         xlab = "Trading Days", ylab = "Negative Returns (%)",
         main = sprintf("%s Drawdown", colnames(s)))
    
    abline(h = 0) # Add horizontal line at break even
    
    l <- as.numeric(s)[!is.na(as.numeric(s))] # Get values for intervals
    
    m <- round(min(l) * -1 + max(l),0)/10^(nchar(round(min(l) * -1 + max(l),0)))
    
    if (m > 0 && m < 1){ mn <- 1 * 10 ^ (nchar(m) - 3) }
    
    else if (m > 1 && m < 2){ mn <- 2 * 10 ^ (nchar(m) - 3) }
    
    else if (m > 2 && m < 5){ mn <- 5 * 10 ^ (nchar(m) - 3) }
    
    if (isTRUE(SD)){ mn <- mn / 2 } # Optimise for Volatility
    
    abline(h = seq(-100, 0, by = mn)[-match(0, seq(-100, 0, by = mn))],
           col = "grey", lty = 3) }
    
    axis(side = 4, at = seq(-100, 0, by = mn), las = 2) # Right y-axis
}
drawdown.plt(stock_data, SD = T) # Test
