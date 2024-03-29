library(fBasics)

histVaR.plt <- function(x, VaR, lg = T){ # VaR by Historical Method
  
  if (nrow(x)<100) { # Check whether there are less than 100 observations
    print("Error. Insufficient number of observations.") } else {
      
      if (isTRUE(lg)) { x <- diff(log(x))[-1,] } # Log returns & NA off
      
      # Calculate historical VaR value and transform into matrix format
      VaR_value <- as.matrix(apply(x,2,function(col) quantile(col, 1-VaR*.01)))
      
      # For each column make histogram
      for (n in 1:ncol(x)){ histPlot(x[,n], las = 1) 
        
        abline(v = VaR_value[n], lwd = 3, col = "red") # line for VaR Value
        
        text(x=VaR_value[n]+.75*VaR_value[n],y=7.5, # Add text
             sprintf("VaR %s is %s", VaR, round(VaR_value[n],2))) } }
}
histVaR.plt(stock_data, 95) # Test
