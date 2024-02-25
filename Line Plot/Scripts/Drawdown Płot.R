drawdown.plt <- function(x, SD = T){ # Function to plot asset drawdown
  
  x <- diff(log(x)) # Log returns
  
  x[1,] <- 0 # Assign first value as 0
  
  if (isFALSE(SD)){ # Calculate Cumulative Returns
    
    x <- as.timeSeries(apply(x, 2, function(col) exp(cumsum(col)) - 1)) }
  
  x <- x * 100 # Multiply returns by 100
  
  x[x > 0] <- 0 # Reduce postive values
  
  for (n in 1:ncol(x)){ security <- x[,n] # Plot each column in data frame
  
    plot(security, type = "l", ylim = c(signif(min(security), 1), 0), las = 2,
         xlab = "Trading Days", ylab = "Negative Returns (%)", col = "red",
         main = sprintf("%s Drawdown", colnames(security)), lwd = 1)
    
    abline(h = 0) # Add horizontal line at break even
    
    if (abs(min(security)) < 15){ # Add grey horizontal dotted lines
      
      abline(h = seq(-20, -2, 2), lty = 3, col = "grey") } # 2
    
    else if (abs(min(security)) > 15 && abs(min(security)) < 50){ # 5
    
      abline(h = seq(-50, -5, 5), lty = 3, col = "grey") }
    
    else if (abs(min(security)) > 50 && abs(min(security)) < 100){ # 10
      
      abline(h = seq(-100, -10, 10), lty = 3, col = "grey") } }
}
drawdown.plt(stock_data, SD = T) # Test
