# Barplot
bar.plt <- function(x, col = "blue", main = NULL){ # Data Cleaning
  
  x<-sort(apply(x,2,function(col) exp(sum(diff(log(col))[-1]))-1),decreasing=T)
  
  # Barplot
  bar.plt.script <- barplot(x,
                            names.arg = names(x),
                            horiz = T,
                            las=1,
                            col = col,
                            main = main,
                            sub = "Data Source: Yahoo! Finance",
                            xlab = "Returns",
                            ylab = "",
                            xlim = c(signif(min(x), 1), signif(max(x), 1)))
  # Add grey dotted lines
  abline(h = bar.plt.script, col = "grey",lty = 3) # through bars
  
  p.seq <- seq(signif(min(x), 1), signif(max(x), 1), by = .1)
  
  for (n in p.seq){ abline(v = n, col = "grey", lty = 3) } # vertical bars
  
  axis(side = 1, at = p.seq, las = 1) # configure x axis
  
  box() # Make borders for plot
}
bar.plt(stock_data, main = "Asset Performance") # Test
