# Performance Plot of several assets
lines.plt <- function(x, name = NULL, ylab = NULL){ x <- diff(log(x))
  
  x[1,] <- 0 # Transform data & Plot
  x <- apply(x, 2, function(col) exp(cumsum(col)) - 1)
  
  plot(x[,1],ylim=c(min(x),max(x)),main=sprintf("%s",name),ylab=ylab,las=1) 
  
  for (n in 2:ncol(x)){ lines(x[,n], col = n) } # Add other assets
}
lines.plt(stock_data, "Stock Performance", ylab = "Return") # Test
