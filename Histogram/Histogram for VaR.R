# Calculate VaR via Historical Method
histVaR.plt <- function(x, VaR, lg = T){
  
  # Check whether there are less than 100 observations
  if (nrow(x) < 100) { 
    print("Error. Insufficient number of observations for analysis.") } else {
      
      # Calculate log returns and remove NA if necessary
      if (isTRUE(lg)) { x <- diff(log(x))[-1,] }
      
      # Calculate historical VaR value and transform into matrix format
      VaR_value <- as.matrix(apply(x, 2,
                           function(col) quantile(col, 1 - VaR * 0.01)))
      # For each column
      for (n in 1:ncol(x)){
        
        # Make histogram
        histPlot(x[,n], las = 1)
        
        # Add line to show VaR Value
        abline(v = VaR_value[n], lwd = 3, col = "red")
        
        # Add text
        text(x = VaR_value[n] + 0.75 * VaR_value[n],
             y = 7.5, sprintf("VaR %s is %s", VaR, round(VaR_value[n],2)))
        }
    }
}
# Test
histVaR.plt(stock_data, 95)
