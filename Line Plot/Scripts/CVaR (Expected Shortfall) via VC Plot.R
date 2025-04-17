lapply(c("fGarch", "timeSeries", "quantmod"), require, character.only=T) # Libs

CVaR.plt <- function(x, CVaR = c(95, 99, 99.9), lg = F){
  
  if (isTRUE(lg)) { x = diff(log(x))[-1,] } # log returns 
  
  for (n in 1:ncol(x)){ s <- x[,n] * 100 # For each column in data set
    
    t <- seq(nrow(s)) # Set index
    
    gm <- garchFit( ~ garch(1, 1), data = coredata(s), trace = F) # GARCH model
    
    plot(t, s, type="l", xlab = "Trading Days", ylab = "Returns (%)", las = 1,
         col = "black", main = sprintf("%s VaR GARCH (1,1)", colnames(s)),
         sub = "Data Source: Yahoo! Finance") # Plot graph
    
    axis(side = 4, las = 2) # Right y - axis
    
    grid(nx = NULL, ny = NULL, col = "grey", lty = "dotted", lwd = 1)
    
    abline(h = 0) # Add horizontal lines
    
    par(mar = rep(5, 4))
    
    for (v in seq(CVaR)){ # Calculate values for Conditional Value-at-Risk
      
      v.g <- - mean(s) + qnorm(1 - CVaR[v]*.01)/(CVaR*.01)*gm@sigma.t # CVaR 
      
      lines(t, v.g, col = v + 1) } } # Add CVaR values as lines
}
CVaR.plt(stock_data, CVaR = c(95, 97.5, 99), lg = T) # Test
