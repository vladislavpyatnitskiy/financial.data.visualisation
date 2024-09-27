lapply(c("quantmod","timeSeries"),require,character.only=T) # Libraries

hist.plt <- function(x, s = NULL, e = NULL, lg = T, data = F){ # Histogram Plot
  
  L <- NULL
  
  if (isTRUE(data)){ p <- NULL # data off
  
    for (A in x){ if (is.null(s) && is.null(e)) { 
      
        q <- getSymbols(A, src = "yahoo", auto.assign = F)
      
        } else if (is.null(e)){ q<-getSymbols(A,from=s,src="yahoo",
                                              auto.assign=F)
    
        } else if (is.null(s)){ q<-getSymbols(A,to=e,src="yahoo",auto.assign=F)
    
        } else { q <- getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F) }
      
      p <- cbind(p, q[,4]) } # Join all columns into one data frame
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Assign tickers
    
    x <- p } 
  
  if (isTRUE(lg) || isTRUE(data)){ x <- diff(log(x))[-1,] }
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each column
    
    h <- hist(s, main=sprintf("%s Histogram & Normal Distribution",colnames(s)),
         ylab = "Likelihood", xlab = "Returns", xlim = c(min(s), max(s)),
         col = "darkgreen", border = "white", breaks = 100, las = 1, freq = F)
    
    m <- round(min(s)*-1 + max(s),1)/10^(nchar(round(min(s)*-1 + max(s),1))-2)
    
    d <- c(0,.0001,.0002,.0005,.001,.002,.005,.01,.02,.05,.1,.2,.5,1) 
    
    for (n in 1:length(d) - 1){ if (m > d[n] && m < d[n + 1]){
      
        mn <- d[n + 1] } else { next } }
    
    M <- round(round(max(h$density)) / 10 ^ (nchar(round(max(h$density))) - 1))
    
    i <- c(0, 1, 2, 5) # Calculate intervals for lines and axes
    
    for (n in 1:length(i) - 1){ if (M >= i[n] && M < i[n + 1]){
      
        mx <- i[n + 1] * 10 ^ (nchar(M) - 1) } else { next } }
    
    abline(v = 0, col = "lightblue", lwd = 2) # Add vertical line at 0
    for (n in seq(-1, 1, by = mn)[-match(0, seq(-1, 1, by = mn))]){ 
      
      abline(v = n, col = "grey", lty = 3) } # Add Vertical lines
    
    abline(h = 0) # Horizontal line at 0 and other above ones
    for (n in seq(mx, 100, by = mx)){ abline(h = n, col = "grey", lty = 3) }
    
    curve(dnorm(x, mean = mean(s), sd = sd(s)), col = "red", lwd = 2, add = T)
    
    box() } # Define borders
}
hist.plt(c("OMF", "HIG", "AAPL", "XOM"), s = "2020-01-01", data = T) # Test
