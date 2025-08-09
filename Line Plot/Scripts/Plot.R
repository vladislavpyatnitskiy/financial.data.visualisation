library(timeSeries) # Library

line.plt <- function(x, lg = F, rtrn = F){ # Line Plot and its variations
  
  if (rtrn & !lg){ # Warning message
    
    return(message("Incorrent settings: Return ON only when log ON")) }
  
  if (lg) x = diff(log(x))[-1,] # Logs
    
  M = ifelse(lg == T, ifelse(rtrn == T, "Return", "Fluctuations"), "Prices")
  
  if (rtrn) x <- apply(x, 2, function(col) (exp(cumsum(col)) - 1))
    
  for (n in seq(colnames(x))){ s <- x[,n] # Plot for each column
    
    plot(
      s,
      main = sprintf("%s Performance", colnames(s)),
      lwd = 1,
      las = 1,
      col = "red",
      sub = "Data Source: Yahoo! Finance",
      xlab = "Trading Days",
      ylab = sprintf("%s %s", colnames(s), M)
      )
    
    grid(nx = 1, ny = NULL, col = "grey", lty = "dotted", lwd = 1)
    
    axis(side = 4, las = 2) # Right y-axis
    
    abline(h = 0) # Break Even Point
    
    par(mar = rep(5, 4)) } # Margins
}
line.plt(stock_data, lg = T, rtrn = T) # Test
