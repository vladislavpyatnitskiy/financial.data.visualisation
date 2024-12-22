lapply(c("fGarch", "timeSeries", "quantmod"), require, character.only=T) # Libs

VaR.plt <- function(x, VaR = c(95, 99, 99.9), lg = F){ # Plot with VaR values
  
  if (isTRUE(lg)) { x = diff(log(x))[-1,] } # log returns 
  
  for (n in 1:ncol(x)){ s <- x[,n]  # For each column in data set
    
    t <- seq(nrow(s)) # Set index
    
    gm <- garchFit( ~ garch(1, 1), data = coredata(s), trace = F) # GARCH model
    
    plot(t, s, type="l", xlab = "Trading Days", ylab = "Returns (%)", las = 1,
         col = "black", main = sprintf("%s VaR GARCH (1,1)", colnames(s)),
         sub = "Data Source: Yahoo! Finance") # Plot graph
    
    abline(h = 0)
    
    for (v in seq(VaR)){ lines(t,mean(s)+qnorm(1-VaR[v]*.01)*gm@sigma.t,
                               col=v+1) }  
    
    grid(nx = NULL, ny = NULL, col = "grey", lty = "dotted", lwd = 1)
    
    axis(side = 4, las = 2) # Right y-axis
    
    par(mar = c(5, 4, 4, 4)) } # Define borders of the plot to fit right y-axis
}
VaR.plt(stock_data, VaR = c(95, 97.5, 99), lg = T) # Test
