lapply(c("timeSeries", "quantmod"), require, character.only = T) # Libs

qq.plt <- function(x, lg = T, data = T, s = NULL, e = NULL){ # Q-Q plot
  
  if (isTRUE(data)){ p <- NULL 
  
    src <- "yahoo" # Source
    
    getData <- function(A, s, e) {
      if (is.null(s) && is.null(e)) return(getSymbols(A,src=src,auto.assign=F)) 
      if (is.null(e)) return(getSymbols(A, from = s, src=src, auto.assign=F)) 
      if (is.null(s)) return(getSymbols(A, to = e, src=src, auto.assign=F)) 
      return(getSymbols(A, from = s, to = e, src=src, auto.assign=F)) 
    }
    for (A in x){ p <- cbind(p, getData(A, s, e)[,4]) } # Join data
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Put tickers as column names
    
    x <- p } # Make it time series, log and NA off 
    
  if (isTRUE(lg)) { x = diff(log(as.timeSeries(x)))[-1,] } # data on
    
  for (n in 1:ncol(x)){ s <- x[,n] # For each asset make qq plot
    
    qqnorm(s, main = sprintf("%s Q-Q Plot", colnames(s)), las = 1) # QQ plot
      
    axis(side = 4, las = 2) # Right y-axis
    
    qqline(s, col = "red", lwd = 3)  # Add line
    
    grid(nx = NULL, ny = NULL, col = "grey", lty = "dotted", lwd = 1) 
    
    par(mar = rep(5, 4)) } # Margins
}
qq.plt(x = c("AIG", "OMF", "UNM"), lg = T, data = T ,s = "2023-10-01") # Test
