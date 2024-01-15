# Function to plot each data column to see Stock Performance
line.plt <- function(x, lg = F, rtrn = F){ 
  
  if (isTRUE(lg) && isTRUE(rtrn)) { x = diff(log(x)) # calculate logs
    
    x[1,] <- 0 # Assign first value
    x <- apply(x,2,function(col) (exp(cumsum(col))-1) * 100) 
  
    for (n in seq(colnames(x))){ security <- x[,n] # Plot for each column
  
      # Plot
      plot(security, main = sprintf("%s Performance", colnames(security)),
           sub = "Data Source: Yahoo! Finance", xlab = "Trading Days", las = 1,
           ylab = sprintf("%s Return (%%)", colnames(security)))
      
      abline(h = 0, col = "red", lwd = 3) } } else if (isTRUE(lg)) {
    
      x = diff(log(x)) # calculate logs
      x[1,] <- 0 # Assign first value
    
    for (n in seq(colnames(x))){ security <- x[,n] # Plot for each column
    
      # Plot
      plot(security, main = sprintf("%s Performance", colnames(security)),
           sub = "Data Source: Yahoo! Finance", xlab = "Trading Days", las = 1,
           ylab = sprintf("%s Fluctuations", colnames(security)))
    
      abline(h=0,col="red",lwd=3) } } else if (isFALSE(lg) && isFALSE(rtrn)){
       
    for (n in seq(colnames(x))){ security <- x[,n] # Plot for each column
        
    # Plot
    plot(security, main = sprintf("%s Performance", colnames(security)),
         sub = "Data Source: Yahoo! Finance", xlab = "Trading Days", las = 1,
         ylab = sprintf("%s Prices", colnames(security)))
        
    } }  else { sprintf("Incorrent settings of parameters") }
}
line.plt(stock_data, lg = T, rtrn = T) # Test
