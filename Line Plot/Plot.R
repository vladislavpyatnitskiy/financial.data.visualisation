# Function to create plots from each data set column 
plot_data <- function(x){
  
  # For each column in data set
  for (n in seq(colnames(x))){

    # Plot
    plot(x[,n],
         main=sprintf("%s Performance", colnames(x[,n])), # Title of Security
         sub="Source: Yahoo! Finance",
         xlab="Trading Days",
         ylab=sprintf("%s Prices", colnames(x[,n])) # Prices of Security
    )
  }
}
# Test
plot_data(stock_data)
