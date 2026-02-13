drawdown.plt <- function(x, SD = T){ # Function to plot asset draw down
  
  x <- diff(log(x))[-1,] # Log returns
  
  if (!SD){ r <- rownames(x) # Save dates for Cumulative Returns
    
    x <- apply(x, 2, function(col) exp(cumsum(col)) - 1) # Cumulative Returns
    
    rownames(x) <- r } # Return dates as row names as they were vanished 
    
  x <- x * 100 # Multiply returns by 100
  
  x[x > 0] <- 0 # Replace positive values as 0
  
  par(mar = c(5, rep(4, 3))) # Define borders of the plot to fit right y-axis
  
  for (n in 1:ncol(x)){ s <- x[,n] # Plot each column in data frame
  
    plot(
      s, 
      type = "l", 
      las = 2, 
      ylim = c(min(s), 0), 
      lwd = 1, 
      col = "red",
      xlab = "Trading Days", 
      ylab = "Negative Returns (%)",
      main = sprintf("%s Drawdown", colnames(s))
    )
    
    grid(nx = 1, ny = NULL, col = "grey", lty = "dotted", lwd = 1)
    
    abline(h = 0)
    
    axis(side = 4, las = 2) # Right y-axis
  } 
}
drawdown.plt(stock_data, SD = T) # Test
