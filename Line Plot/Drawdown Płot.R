# Function to plot asset drawdown
drawdown.plt <- function(x){
  
  # Log returns
  x <- diff(log(x))
  
  # Assign first value as 0
  x[1,] <- 0
  
  # Calculate exponential sum
  x <-apply(x, 2, function(col) (exp(cumsum(col))-1))
  
  # Transform into time series
  x <- as.timeSeries(x)
  
  # Reduce postive values
  x[x > 0] <- 0
  
  # For each column in data frame
  for (n in 1:ncol(x)){ security <- x[,n]
  
    # Plot
    plot(security,
         type = "l",
         main = sprintf("%s Drawdown", colnames(security)),
         xlab = "Trading Days",
         ylab = "Negative Return (%)",
         las = 2) }
}
# Test
drawdown.plt(stock_data)
