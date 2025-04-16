volatility.plt <- function(x, abs = F){ # Plot Security volatility
  
  x <- diff(log(x))[-1,] * 100 # Calculate logs
  main = ifelse(abs == T, "Volatility", "Fluctuations")
  if (isTRUE(abs)){ x <- abs(x) }
  
  for (n in 1:ncol(x)){ 
    
    plot(x[,n], col = "red", ylab = "Returns (%)", xlab = "Trading Days",
         main = sprintf("%s %s", colnames(x[,n]), main), las = 2,
         ylim = c(min(x[,n]), max(x[,n]))) 
  
    axis(side = 4, las = 2) # Right y axis and lines
    
    grid(nx = 1, ny = NULL, col = "grey", lty = "dotted", lwd = 1)
    abline(h = 0) }
}
volatility.plt(stock_data, abs = T)
