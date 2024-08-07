lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

box.plt <- function(x, s = NULL, e = NULL, data = T, lg = F){ # Box Plot
  
  if (isTRUE(data)){ p <- NULL # data off
  
    for (A in x){ if (is.null(s) && is.null(e)) { # When dates are not defined
      
        p <- cbind(p, getSymbols(A, src = "yahoo", auto.assign = F)[,4])
      
      } else if (is.null(e)) { # When only start date is defined
      
        p <- cbind(p, getSymbols(A, from = s, src="yahoo", auto.assign=F)[,4])
        
      } else if (is.null(s)) { # When only end date is defined
        
        p <- cbind(p, getSymbols(A, to = e, src = "yahoo", auto.assign=F)[,4])
        
      } else { # When both start date and end date are defined
        
        p <- cbind(p,getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F)[,4])} }
      
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x
    
    x <- p } # Give column names 
    
  if (isTRUE(lg) || isTRUE(data)) { x <- diff(log(as.timeSeries(x)))[-1,] }
  
  boxplot.matrix(x, main = "Fluctuations of Securities", title = F, las = 1,
                 col = "steelblue", xlab = "Data Source: Yahoo Finance")
  
  l <- as.numeric(x)[!is.na(as.numeric(x))] # Get values & define intervals
  
  m <- round(min(l) * -1 + max(l),1)/10^(nchar(round(min(l) * -1 + max(l),1)))
  
  i <- c(0, 1, 2, 5) # Calculate intervals for lines and axes
  
  for (n in 1:length(i) - 1){ if (m > i[n] && m < i[n + 1]){
    
      mn <- i[n + 1] * 10 ^ (nchar(m) - 6) } else { next } }
  
  axis(side = 4, at = seq(-1, 1, by = mn / 2), las = 2)
  
  par(mar = c(5, 4, 4, 4)) # Define borders of the plot to fit right y-axis
  
  # Add Vertical and Horizontal lines
  abline(h=seq(-1,1,by=mn/2)[-match(0, seq(-1,1,by=mn/2))], col="grey", lty=3)
  abline(v = seq(ncol(x)), col = "grey", lty = 3) 
  abline(h = 0, col = "black", lty = 3) 
}
box.plt(c("AAPL", "MSFT", "META", "GOOGL", "AMZN"), s = "2023-01-01") # Test
