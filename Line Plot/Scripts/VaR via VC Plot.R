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
    
    m <- round(min(s)*-1 + max(s),1)/10^(nchar(round(min(s)*-1 + max(s), 1)))
    
    i <- c(0, 1, 2, 5) # Calculate intervals for lines and axes
    
    for (n in 1:length(i) - 1){ if (m > i[n] && m < i[n + 1]){
      
        mn <- i[n + 1] * 10 ^ (nchar(m) - 6) } else { next } }
    
    abline(h = seq(-1,1,by=mn)[-match(0, seq(-1,1,by=mn))], col="grey", lty=3)
    
    par(mar = c(5, 4, 4, 4)) } # Define borders of the plot to fit right y-axis
}
VaR.plt(stock_data, VaR = c(95, 97.5, 99), lg = T) # Test
