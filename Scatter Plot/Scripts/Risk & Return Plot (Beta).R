# Function to generate Scatter Plot for selected securities with Beta
beta.return.plt <- function(x, s = NULL,e = as.Date(Sys.Date()), i = "^GSPC"){
  
  x <- c(x, i) # Add S&P 500 ticker to rest
  
  p <- NULL # Empty variable and get data from Yahoo! Finance
  
  for (A in x){ p <- cbind(p, getSymbols(A, from = s, to = e, src = "yahoo",
                                         auto.assign = F)[,4]) }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- x # Give column names 
  
  p <- diff(log(as.timeSeries(p)))[-1,] # Calculate logs
  
  o <- apply(p[,-which(names(p) == i)], 2,
             function(col) c(((exp(sum(col)) - 1) * 100),
                             (lm((col) ~ p[,i]))$coefficients[2]))
  
  plot(o[2,], o[1,], las = 1, xlab = "Risk (Beta)", ylab = "Return (%)",
       main = "Risk & Return Plot", ylim = c(min(o[1,]) - 3, max(o[1,]) + 3),
       xlim = c(min(o[2,]), max(o[2,]))) # Plot
  
  abline(lm(o[1,] ~ o[2,]), col = "red", lwd = 3) # Add regression line

  grid(nx = NULL, ny = NULL, col = "grey", lty = "dotted", lwd = 1) # Grid
             
  abline(h = 0) # Add horizontal line 
  
  text(o[2,], o[1,] + 1, labels = names(o[2,]), pos = 4) # Put labels
}
beta.return.plt(x = c("AMZN", "GOOGL", "MSFT", "AAPL", "NFLX", "META", "NVDA",
                      "TSLA", "ORCL", "INTC", "TSM", "ASML", "AMD", "QCOM",
                      "MU"), "2023-10-10")
