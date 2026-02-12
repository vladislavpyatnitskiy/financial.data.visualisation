library(timeSeries) # Library

line.plt <- function(x, lg=F, rtrn=F, sub=NULL, main=NULL, ylab=NULL,col=NULL){ 
  
  L <- "Incorrent settings: Return ON only when log ON" # Warning message
  
  if (rtrn & !lg) return(message(L))
  
  if (lg) x = diff(log(x))[-1,] # Logs
  
  M = ifelse(lg == T, ifelse(rtrn == T, "Return", "Fluctuations"), "Prices")
  
  if (rtrn) x <- apply(x, 2, function(col) (exp(cumsum(col)) - 1))

  par(mar = rep(5, 4)) 
                       
  for (n in seq(colnames(x))){ s <- x[,n] # Plot for each column
  
    plot(
      s,
      main = main,
      lwd = 3,
      las = 1,
      col = ifelse(is.null(col) == T, "red", col),
      sub = sub,
      xlab = "Trading Days",
      ylab = ylab
    )
    
    grid(nx = 1, ny = NULL, col = "grey", lty = "dotted", lwd = 1)
    
    axis(side = 4, las = 2) # Right y-axis
    
    abline(h = 0) # Break Even Point
    
    } # Margins
}
line.plt(cbr_ir_data, lg = F, rtrn = F, main = "Interest Rate Dynamics",
         sub = "Data Source: cbr.ru") # Test
