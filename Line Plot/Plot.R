# Function to create plots from each data set column 
plot_data <- function(x, lg = F){
  
  # Make logs if needed
  if (isTRUE(lg)) { x = diff(log(x))[-1,] }
  
  # For each column in data set define column
  for (n in seq(colnames(x))){ security <- x[,n]
    
    # Plot
    plot(security,
         main=sprintf("%s Performance", colnames(security)), # Security Title
         sub="Source: Yahoo! Finance",
         xlab="Trading Days",
         ylab=sprintf("%s Prices", colnames(security)))
    
    # Add horizontal line
    abline(h = 0) }
}
# Test
plot_data(returns_df)
