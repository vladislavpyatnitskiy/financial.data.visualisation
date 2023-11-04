lapply(c("fGarch", "timeSeries"), require, character.only = TRUE) # Libraries

# Function to generate plot with VaR values (V-C method) 
VaR.plt <- function(x, VaR = c(95, 99, 99.9), lg = F){
  
  if (isTRUE(lg)) { x=diff(log(x))[-1,] } # log returns and remove NA if needed
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each column in data set
  
    t <- seq(nrow(s)) # Set index
    
    gm <- garchFit( ~ garch(1,1),data=coredata(s),trace=F) # Set up GARCH model
    
    # Plot graph
    plot(t, s, type="l", xlab = "Trading Days", ylab = "Returns", col="black",
         main = sprintf("%s VaR GARCH (1,1)", colnames(s)), las = 1,
         sub = "Data Source: Yahoo! Finance")
    
    abline(h = seq(-1, 1, .05), lty = 3, col = "grey") # Add horizontal lines
    
    for (v in seq(VaR)){ v.g<-mean(s)+qnorm(1-VaR[v]*.01)*gm@sigma.t # VaR 
    
    lines(t, v.g, col = v + 1) } } # VaR lines
}
VaR.plt(stock_data, VaR = c(95, 97.5, 99), lg = T) # Test
