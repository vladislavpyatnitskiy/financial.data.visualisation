lapply(c("quantmod", "timeSeries"), require, character.only = T) # libraries

bar.plt <- function(x, s = NULL, e = NULL, data = F){ # Bar Plot
  
  if (data){ p <- NULL # data off
    
    src <- "yahoo"
    
    getData <- function(A, s, e) {
      if (is.null(s) && is.null(e)) return(getSymbols(A,src=src,auto.assign=F)) 
      if (is.null(e)) return(getSymbols(A, from = s, src=src, auto.assign=F)) 
      if (is.null(s)) return(getSymbols(A, to = e, src=src, auto.assign=F)) 
      return(getSymbols(A, from = s, to = e, src=src, auto.assign=F)) 
    }
    for (A in x){ p <- cbind(p, getData(A, s, e)[,4]) 
      
      message(
        sprintf(
          "%s is downloaded (%s / %s)", 
          A, which(x == A), length(x)
        )
      )
    } # Join data
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Put the tickers in data set
    
    x <- as.timeSeries(p) } # Make it time series & perform Data Cleaning
    
  x <- sort(
    apply(x, 2, function(col) exp(sum(diff(log(col))[-1])) - 1), decreasing = T
    ) * 100
  
  C = c(
    "#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
    "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
    "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
    "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
    "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
    "#895c8b","#bd5975"
    ) # Set of colours

  par(mar = c(6, 6, 3, 3)) # Define borders of the plot
    
  B <- barplot(
    x, 
    names.arg = names(x), 
    horiz = T, 
    las = 1, 
    xlab = "Returns (%)",
    main = sprintf("Securities Performance from %s", s), 
    col = C,
    xpd = F,
    xlim = c(
      round(min(x), 0) - 1, 
      round(max(x), 0) + 1
      )
    ) # Bar plot
  
  # Add grey dotted lines
  grid(nx = NULL, ny = 1, col = "grey", lty = "dotted", lwd = 1)
  abline(h = B, col = "grey", lty = 3) # through bars
  
  abline(v = mean(x), col = "red", lwd = 3) # Mean percentage line
  abline(v = median(x), col = "green", lwd = 3) # Median percentage line
  
  legend(
    x = "bottom",
    inset = c(0, -.15),
    cex = .9,
    bty = "n",
    horiz = T,
    xpd = T, 
    pch = 15,
    col = c("red","green"),
    legend = c(
      (
        sprintf("Mean: %s", round(mean(x), 2))),
        sprintf("Median: %s", round(median(x), 2))
      )
    ) # legend
  
  box() # Make borders for plot
}
bar.plt(x = c("AIG", "MET", "HIG", "UNM", "OMF"), s = "2023-10-01", data = T)
