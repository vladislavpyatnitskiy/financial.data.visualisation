lapply(c("quantmod", "timeSeries"), require, character.only = T) # libraries

bar.plt <- function(x, s = NULL, e = NULL, data=F){ # Bar Plot
  
  if (isTRUE(data)){ p <- NULL # Set time period for stock price data
    
    for (A in x){ if (is.null(s) && is.null(e)) { # Period Intervals
      
        p <- cbind(p, getSymbols(A, src = "yahoo", auto.assign = F)[,4])
      
        } else if (is.null(e)) { p <- cbind(p, getSymbols(A,from=s,src="yahoo",
                                                          auto.assign=F)[,4])
      
        } else if (is.null(s)) { p <- cbind(p, getSymbols(A,to=e,src="yahoo",
                                                          auto.assign=F)[,4])
      
        } else { p <- cbind(p, getSymbols(A,from=s,to=e,src="yahoo",
                                          auto.assign=F)[,4]) }
    }
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Put the tickers in data set
    
    x <- as.timeSeries(p) } # Make it time series & perform Data Cleaning
    
  x <- sort(apply(x, 2, function(col) exp(sum(diff(log(col))[-1]))-1),
            decreasing=T) * 100
  
  C = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
        "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
        "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
        "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
        "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
        "#895c8b","#bd5975") # Set of colours
  
  B <- barplot(x, names.arg = names(x), horiz = T, las = 1, xlab="Returns (%)",
               main = sprintf("Securities Performance from %s", s), col = C,
               xlim = c(round(min(x),0)-1, round(max(x),0)+1),xpd=F) # Bar plot
  
  m <- round(min(x)*-1 + max(x),0)/10^(nchar(round(min(x)*-1 + max(x),0))-1)
  
  i <- c(0, 1, 2, 5) # Calculate intervals for lines and axes
  
  for (n in 1:length(i) - 1){ if (m > i[n] && m < i[n + 1]){
    
      mn <- i[n + 1] * 10 ^ (nchar(m) - 3) } else { next } }
  
  axis(side = 1, las = 1, at = seq(-100, 100, mn)) # Axes
  
  abline(v = 0) # Break Even
  
  # Add grey dotted lines
  for (n in seq(-100, -mn, mn)){ abline(v = n, col = "grey", lty = 3) }
  for (n in seq(mn, 100, mn)){ abline(v = n, col = "grey", lty = 3) }
  abline(h = B, col = "grey", lty = 3) # through bars
  
  abline(v = mean(x), col = "red", lwd = 3) # Mean percentage line
  abline(v = median(x), col = "green", lwd = 3) # Median percentage line
  
  par(mar = c(6, 6, 3, 3)) # Define borders of the plot
  
  legend(x="bottom",inset=c(0,-.17),cex=.9,bty="n",horiz=T,col=c("red","green"),
         legend=c((sprintf("Mean: %s",round(mean(x),2))),
                  sprintf("Median: %s",round(median(x),2))),xpd=T,pch=15)
  
  box() # Make borders for plot
}
bar.plt(x = c("AIG", "MET", "HIG", "UNM", "OMF"), s = "2023-10-01", data = T)
