library(fBasics)

histVaR.plt <- function(x, VaR, lg = T){ # VaR by Historical Method
  
  if (nrow(x) < 100) return("Error. Insufficient number of observations.")
      
  if (lg) x <- diff(log(x))[-1,] # Log returns & NA off
      
  # Calculate historical VaR value and transform into matrix format
  v <- apply(x, 2, function(col) quantile(col, 1 - VaR * .01))
      
  # For each column make histogram
  for (n in 1:ncol(x)){ histPlot(x[,n], las = 1) 
        
    abline(v = v[n], lwd = 3, col = "red") # line for VaR Value
        
    text(
      x = 2 * v[n],
      y = 7.5,
      sprintf("VaR %s is %s", VaR, round(v[n], 2))
      ) } 
}
histVaR.plt(stock_data, 95) # Test
