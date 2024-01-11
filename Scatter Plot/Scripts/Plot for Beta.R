lapply(c("quantmod", "timeSeries"), require,character.only = T) # Libraries

# Function to visualise Beta Scatter Plot
beta.plt <- function(y, benchmark = "^GSPC", s = NULL, e = NULL){
  
  y <- c(y, benchmark) # Add benchmark to list
  
  p <- NULL # Create an empty variable
  
  for (A in y){ if (is.null(s) && is.null(e)) { # Download data when dates off

      p<-cbind(p,getSymbols(A,from=as.Date(Sys.Date())-365,to=Sys.Date(),
                                          src = "yahoo", auto.assign=F)[,4])
    } else { # When dates are set
      
      p<-cbind(p,getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F)[,4]) } }
  
  p <- p[apply(p,1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  x=diff(log(as.timeSeries(p)))[-1,] # Returns and NA off
  
  stock_returns<-x[,-which(names(x)==benchmark)] # Subset index from data set
  
  # Loop generates Scatter plots with trend line  
  for (n in 1:ncol(stock_returns)) { security <- stock_returns[,n]
  
    plot_for_beta<-plot(x[,benchmark], security,xlab="Market Return (%)",las=1,
                          ylab = sprintf("%s Return (%%)", colnames(security)),
                          main = sprintf("%s Beta", colnames(security)),
                          sub = "Data Source: Yahoo Finance")
    
    abline_for_beta <- abline(lm(security ~ x[,benchmark]),col="red",lwd=3) }
}
beta.plt(c("AIG", "MET", "UNM", "HIG", "OMF"), benchmark = "^GSPC") # Test
