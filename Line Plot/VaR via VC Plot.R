# Libraries
lapply(c("fGarch", "timeSeries"), require, character.only = TRUE)

# Function to generate plot with VaR values (V-C method) 
VaR.plt <- function(x, VaR = c(95, 99, 99.9), lg = F){
  
  # Calculate Returns and remove NA if needed
  if (isTRUE(lg)) { x=diff(log(x))[-1,] }
  
  # For each column in data set
  for (n in 1:ncol(x)){ s <- x[,n]
    
    # Set index
    t <- seq(nrow(s))
    
    # Set up GARCH model
    gm <- garchFit( ~ garch(1,1), data=coredata(s), trace=FALSE)
    
    # Plot graph
    plot(t, s, type="l",
         xlab = "Trading Days",
         ylab = "Returns",
         main = sprintf("%s VaR GARCH(1,1)", colnames(s)),
         col = "black",
         sub = "Data Source: Yahoo! Finance",
         las = 1)
    
    # Add horizontal lines
    abline(h = seq(-1, 1, 0.05), lty = 3, col = "grey")
    
    # Plot VaR values 
    for (v in seq(VaR)){ v.g <- mean(s) + qnorm(1 - VaR[v] * 0.01) * gm@sigma.t
      
      lines(t, v.g, col = v + 1) } } # VaR lines
}
# Test
VaR.plt(stock_data, VaR = c(95, 97.5, 99), lg = T)
