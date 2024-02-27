volatility.plt <- function(x, abs = F){ # Plot Security volatility
  
  x <- diff(log(x)) # Calculate logs
  x[1,] <- 0 # Assign first value as 0
  
  x <- x * 100 # Multiply by 100
  
  if (isFALSE(abs)){ # Choose between real and absolute values of returns
  
    for (n in 1:ncol(x)){ # Plot volatility with positive and negative returns
        
      plot(x[,n], col = "red", ylab = "Returns (%)", xlab = "Trading Days",
           main = sprintf("%s Volatility", colnames(x[,n])), las = 2,
           ylim = c(min(x[,n]), max(x[,n])))
      
      axis(side = 2, at = seq(-100, 100, 5), las = 2) # Set up axis
        
      abline(h = 0)
      abline(h = seq(-100, -5, 5), col = "grey", lty = 3)
      abline(h = seq(5, 100, 5), col = "grey", lty = 3) } } else {
        
        x <- abs(x)
          
    for (n in 1:ncol(x)){ # Plot returns fluctuations
        
      plot(x[,n], col = "red", ylab = "Fluctuations (%)", las = 2,
           xlab = "Trading Days", ylim = c(min(x[,n]), max(x[,n])),
           main = sprintf("%s Volatility", colnames(x[,n])))
      
      if (max(x[,n]) < 15){
        
        axis(side = 2, at = seq(-100, 100, 2), las = 2) # Set up axis
        
        abline(h = 0)
        abline(h = seq(-100, -2, 2), col = "grey", lty = 3)
        abline(h = seq(2, 100, 2), col = "grey", lty = 3) } else {
      
      axis(side = 2, at=seq(100, from = 5, by = 5), las = 2) # Set up axis
          
      abline(h = 0)
      abline(h = seq(5, 100, 5), col = "grey", lty = 3) } } }
}
volatility.plt(stock_data, abs = F)
