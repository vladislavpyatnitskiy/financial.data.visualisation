lapply(c("quantmod", "timeSeries"), require,character.only = T) # Libraries

beta.plt <- function(y, i = "^GSPC", s=NULL, e=NULL){ # Scatter Plot of Beta
  
  y <- c(y, i) # Add benchmark to list
  
  p <- NULL # Create an empty variable
  
  src <- "yahoo"
  
  getData <- function(A, s, e) {
    if (is.null(s) && is.null(e)) return(getSymbols(A,src=src,auto.assign=F)) 
    if (is.null(e)) return(getSymbols(A, from = s, src=src, auto.assign=F)) 
    if (is.null(s)) return(getSymbols(A, to = e, src=src, auto.assign=F)) 
    return(getSymbols(A, from = s, to = e, src=src, auto.assign=F)) 
  }
  for (A in y){ p <- cbind(p, getData(A, s, e)[,4]) } # Join data
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  x = diff(log(as.timeSeries(p)))[-1,] # Returns and NA off
  
  R <- x[,-which(names(x) == i)] # Subset index from data set
  
  for (n in 1:ncol(R)) { S <- R[,n] # Scatter plot for each stock
  
    plot(
      x[,i],
      S,
      sub = "Data Source: Yahoo Finance",
      xlab = "Market Return",
      las = 1,
      ylab = "Stock Return",
      main = sprintf("%s Beta", colnames(S))
      )
    
    grid(nx = NULL, ny = NULL, col = "grey", lty = "dotted", lwd = 1)
    
    abline(lm(S ~ x[,i]), col = "red", lwd = 3) } # Regression line
}
beta.plt(c("AIG", "MET", "UNM", "HIG", "OMF"), i = "^GSPC") # Test
