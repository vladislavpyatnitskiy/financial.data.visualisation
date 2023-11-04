# Function to plot from each data column & # Make logs if needed
line.plt <- function(x, lg = F){ if (isTRUE(lg)) { x = diff(log(x))[-1,] }
  
  for (n in seq(colnames(x))){ security <- x[,n] # Plot for each column
  
  # Plot
  plot(security, main=sprintf("%s Performance", colnames(security)), las = 1,
       sub="Data Source: Yahoo! Finance", xlab="Trading Days",
       ylab=sprintf("%s Prices", colnames(security)))
  
  abline(h = 0) } # Add horizontal line
}
line.plt(returns_df) # Test
