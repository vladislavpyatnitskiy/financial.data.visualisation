lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

box.plt <- function(x, s = NULL, e = NULL, data = T, lg = F){ # Box Plot
  
  if (isTRUE(data)){ p <- NULL # 4 scenarios: no dates, start, end & both dates
  
  for (A in x){ if (is.null(s) && is.null(e)) { 
    
      q <- getSymbols(A, src = "yahoo", auto.assign = F)
      
    } else if (is.null(e)){ q <- getSymbols(A,from=s,src="yahoo",auto.assign=F)
    
    } else if (is.null(s)){ q <- getSymbols(A, to=e, src="yahoo",auto.assign=F)
    
    } else { q <- getSymbols(A, from = s, to = e, src="yahoo", auto.assign=F) }
      
    p <- cbind(p, q[,4]) } # Join all columns into one data frame
  
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Put the tickers in column names
    
    x <- p } # Give column names 
    
  if (isTRUE(lg) || isTRUE(data)) { x <- diff(log(as.timeSeries(x)))[-1,] }
  
  boxplot.matrix(x, main = "Fluctuations of Securities", title = F, las = 1,
                 col = "steelblue", xlab = "Data Source: Yahoo Finance")
  
  axis(side = 4, las = 2)
  
  par(mar = c(5, 4, 4, 4)) # Define borders of the plot to fit right y-axis
  
  grid(nx = NULL, ny = NULL, col = "grey", lty = "dotted", lwd = 1)
  
  abline(h = 0, col = "black", lty = 3) 
}
box.plt(c("AAPL", "MSFT", "META", "GOOGL", "AMZN"), s = "2023-01-01") # Test
