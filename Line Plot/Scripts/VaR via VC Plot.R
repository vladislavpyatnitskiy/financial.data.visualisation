lapply(c("fGarch", "timeSeries", "quantmod"), require, character.only=T) # Libs

# Function to generate plot with VaR values (V-C method) 
VaR.plt <- function(x, VaR = c(95, 99, 99.9), lg = F){
  
  if (isTRUE(lg)) { x = diff(log(x))[-1,] } # log returns 
  
  for (n in 1:ncol(x)){ s <- x[,n] * 100 # For each column in data set
  
    t <- seq(nrow(s)) # Set index
    
    gm <- garchFit( ~ garch(1, 1), data = coredata(s), trace = F) # GARCH model
    
    # Plot graph
    plot(t, s, type="l", xlab = "Trading Days", ylab = "Returns (%)", las = 1,
         col = "black", main = sprintf("%s VaR GARCH (1,1)", colnames(s)),
         sub = "Data Source: Yahoo! Finance")
    
    abline(h = 0) # Add horizontal lines
    
    if (max(s) < 10){ abline(h = seq(-100, -1, 1), lty = 3, col = "grey")
      
      abline(h = seq(1, 100, 1), lty = 3, col = "grey")  
      
    } else { abline(h = seq(-100, -5, 5), lty = 3, col = "grey")
      
    abline(h = seq(5, 100, 5), lty = 3, col = "grey") }
    
    for (v in seq(VaR)){ v.g<-mean(s)+qnorm(1-VaR[v]*.01)*gm@sigma.t # VaR 
    
      lines(t, v.g, col = v + 1) } # 
    
    if (max(s) < 10){ axis(side = 2, at = seq(-100, 100, 1), las = 2) } else {
    
    axis(side = 2, at = seq(-100, 100, 5), las = 2) } } # VaR lines
}
VaR.plt(stock_data, VaR = c(95, 97.5, 99), lg = T) # Test
