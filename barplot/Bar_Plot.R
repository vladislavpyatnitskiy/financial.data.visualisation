lapply(c("quantmod", "timeSeries"), require, character.only = T) # libraries

# Barplot
bar.plt <- function(x, s = NULL, e = NULL, col = "blue", main = NULL, data=F){ 
  
  if (isTRUE(data)){ p <- NULL # Create an empty variable
    
    # Loop for data extraction & # Set up statements for start and end dates
    for (Ticker in x){ if (is.null(s) && is.null(e)) {
      
      # When neither start date nor end date are defined
      p <- cbind(p, getSymbols(Ticker, src = "yahoo", auto.assign=F)[,4])
      
    } else if (is.null(e)) { # When only start date is defined
      
      p <- cbind(p, getSymbols(Ticker, from = s,src="yahoo",auto.assign=F)[,4])
      
    } else if (is.null(s)) { # When only end date is defined
      
      p <- cbind(p,getSymbols(Ticker,to=e,src="yahoo",auto.assign=F)[,4])
      
    } else { # When both start date and end date are defined
      
      p<-cbind(p,getSymbols(Ticker,from=s,to=e,src="yahoo",auto.assign=F)[,4])}
    }
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Put the tickers in data set
    
    x <- as.timeSeries(p) } # Make it time series & perform Data Cleaning
  
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
bar.plt(x = c("AIG", "MET", "HIG", "UNM", "OMF"), s = "2023-10-01",
        main = "Asset Performance", data = T) # Test
