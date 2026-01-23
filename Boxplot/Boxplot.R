lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

box.plt <- function(x, s = NULL, e = NULL, data = T, lg = F){ # Box Plot
  
  if (data){ p <- NULL # 4 scenarios: no dates, start, end & both dates
    
    src <- "yahoo"
    
    getData <- function(A, s, e) {
      if (is.null(s) && is.null(e)) return(getSymbols(A,src=src,auto.assign=F)) 
      if (is.null(e)) return(getSymbols(A, from = s, src=src, auto.assign=F)) 
      if (is.null(s)) return(getSymbols(A, to = e, src=src, auto.assign=F)) 
      return(getSymbols(A, from = s, to = e, src=src, auto.assign=F)) 
    }
    for (A in x){ p <- cbind(p, getData(A, s, e)[,4]) # Join data
      
      message(
        sprintf(
          "%s is downloaded (%s / %s)", 
          A, which(x == A), length(x)
        )
      )
      }
      p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
      
      colnames(p) <- x # Put the tickers in column names
      
      x <- p } # Give column names 
      
  if (lg | data) x <- diff(log(as.timeSeries(x)))[-1,] 
  
  boxplot.matrix(
    x, 
    main = "Fluctuations of Securities", 
    title = F, 
    las = 1,
    col = "steelblue", 
    xlab = "Data Source: Yahoo Finance"
    )
  
  axis(side = 4, las = 2)
  
  par(mar = c(5, rep(4, 3))) # Define borders of the plot to fit right y-axis
  
  grid(nx = NULL, ny = NULL, col = "grey", lty = "dotted", lwd = 1)
  
  abline(h = 0, col = "black", lty = 3) 
}
box.plt(c("AAPL", "MSFT", "META", "GOOGL", "AMZN"), s = "2023-01-01") # Test
