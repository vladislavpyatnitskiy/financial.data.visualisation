# Plot with Performance of several assets
lines.plt <- function(x, name = NULL, ylab = NULL){
  
  # Transforming data
  x <- diff(log(x))
  x[1,] <- 0
  x <- apply(x, 2, function(col) exp(cumsum(col)) - 1)
  
  # Plot
  plot(x[,1], ylim=c(min(x),max(x)), main=sprintf("%s",name), ylab=ylab, las=1)
  
  # Add other assets
  for (n in 2:ncol(x)){ lines(x[,n], col = n) }
}
# Test
lines.plt(stock_data, "Stock Performance", ylab = "Return")
