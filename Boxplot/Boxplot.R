lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

# Function to create boxplot
box.plt <- function(x, s = NULL, e = NULL, main = NULL, data = T, lg = F){
  
  if (isTRUE(data)){ p <- NULL # data off
  
  for (A in x){ if (is.null(s) && is.null(e)) { # When dates are not defined
    
    p <- cbind(p, getSymbols(A, src = "yahoo", auto.assign = F)[,4])
    
  } else if (is.null(e)) { # When only start date is defined
    
    p <- cbind(p, getSymbols(A, from = s, src="yahoo", auto.assign = F)[,4])
    
  } else if (is.null(s)) { # When only end date is defined
    
    p <- cbind(p, getSymbols(A, to = e, src = "yahoo", auto.assign = F)[,4])
    
  } else { # When both start date and end date are defined
    
    p <- cbind(p,getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F)[,4]) } }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- x
  
  x <- p } # Give column names 
  
  if (isTRUE(lg) || isTRUE(data)) { x <- diff(log(as.timeSeries(x)))[-1,] }
  
  boxplot.matrix(x, main = main, title = F, las = 1, col = "steelblue",
                 xlab = "Data Source: Yahoo Finance", ylab = "Returns")
  
  abline(h = 0, col = "grey", lty = 3) # data is ready
}
# Test
box.plt(c("AAPL", "MSFT", "META", "GOOGL", "AMZN"),s = "2023-01-01",
        main = "Boxplot of IT companies")
