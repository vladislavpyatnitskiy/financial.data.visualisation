lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

# Function to generate Scatter Plot for selected securities
risk.return.plt <- function(x,s = NULL,e = as.Date(Sys.Date()),data = T,lg=F){
  
  if (isTRUE(data)){ p <- NULL # Empty variable for values when data is needed
    
    for (A in x){ # For each ticker get data
      p<-cbind(p,getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F)[,4])}
    
    p <- p[apply(p,1,function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Give column names 
    
    p <- diff(log(as.timeSeries(p)))[-1,] # Calculate logs
    
    r <- apply(p,2,function(col) ((exp(cumsum(col))-1)*100)[nrow(p)]) # Return
    
    r.sd <- apply(p, 2, function(col) sd(col) * 1000) # Standard Deviation
    
    plot(r.sd, r, las = 1, xlab="Risk (Standard Deviation)",ylab="Return (%)",
         main = "Risk & Return Plot", ylim = c(min(r)-3, max(r)+3),
         xlim = c(min(r.sd)-3, max(r.sd)+3)) # Plot
    
    abline(lm(r ~ r.sd), col="red", lwd=3) # Add regression line
    
    abline(h = 0) # Add horizontal line 
    
    text(r.sd, r+1, labels=names(r.sd), pos = 4) # Put labels
    
  } else { if (isTRUE(lg)){ p <-diff(log(as.timeSeries(x)))[-1,] }
      
    r <- apply(p,2,function(col) ((exp(cumsum(col))-1)*100)[nrow(p)]) # Return
    
    r.sd <- apply(p, 2, function(col) sd(col) * 1000) # Standard Deviation
    
    plot(r.sd, r, las = 1, xlab="Risk (Standard Deviation)",ylab="Return (%)",
         main = "Risk & Return Plot", ylim = c(min(r)-3, max(r)+3),
         xlim = c(min(r.sd)-3, max(r.sd)+3)) # Plot
    
    abline(lm(r ~ r.sd), col="red", lwd=3) # Add regression line
    
    abline(h = 0) # Add horizontal line 
    
    text(r.sd, r+1, labels=names(r.sd), pos = 4)} # Put labels
}
risk.return.plt(x = c("AMZN", "GOOGL", "MSFT", "AAPL", "NFLX", "META", "NVDA",
                      "TSLA", "ORCL", "INTC", "TSM", "ASML", "AMD", "QCOM",
                      "MU"), "2023-10-10")
