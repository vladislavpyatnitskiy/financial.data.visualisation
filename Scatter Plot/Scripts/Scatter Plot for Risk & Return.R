lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

# Function to generate Scatter Plot for selected securities
risk.return.plt <- function(x,s = NULL,e = as.Date(Sys.Date()),data = T,lg=F){
  
  if (isTRUE(data)){ p <- NULL # Empty variable for values when data is needed
  
    for (A in x){ p <- cbind(p, getSymbols(A, from = s, to = e, src = "yahoo",
                                           auto.assign=F)[,4]) }
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
      
    colnames(p) <- x } else { p <- x } # Give column names 
      
  r <- apply(diff(log(as.timeSeries(p)))[-1,], 2,
             function(col) c((exp(sum(col)) - 1) * 100, sd(col) * 1000)) 
  
  plot(r[2,], r[1,], xlab = "Risk (Standard Deviation)", ylab = "Return (%)",
       main = "Risk & Return Plot", ylim = c(min(r[1,]) - 3, max(r[1,]) + 3),
       xlim = c(min(r[2,]) - 3, max(r[2,]) + 3), las = 1) # Plot
  
  abline(lm(r[1,] ~ r[2,]), col = "red", lwd = 3)
  abline(h = 0) # Add regression & horizontal line
  
  text(r[2,], r[1,] + 1, labels = names(r[2,]), pos = 4) # Put labels
}
risk.return.plt(x = c("AMZN", "GOOGL", "MSFT", "AAPL", "NFLX", "META", "NVDA",
                      "TSLA", "ORCL", "INTC", "TSM", "ASML", "AMD", "QCOM",
                      "MU"), "2023-10-10")
