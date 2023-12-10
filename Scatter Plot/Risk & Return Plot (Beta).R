# Function to generate Scatter Plot for selected securities with Beta
beta.return.plt <- function(x,s = NULL,e = as.Date(Sys.Date()), i = "^GSPC"){
  
  x <- c(x, i)
  
  p <- NULL # Empty variable for values when data is needed
  
  for (A in x){ # For each ticker get data
    p<-cbind(p,getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F)[,4]) }
  
  p <- p[apply(p,1,function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- x # Give column names 
  
  p <- diff(log(as.timeSeries(p)))[-1,] # Calculate logs
  
  r <- p[,-which(names(p)==i)] # Subset index from data set
  
  #a <- apply(r,2,function(col) (exp(cumsum(col)) - 1) * 100)[nrow(r),] # Return
  
  #r.beta <- apply(r, 2, function(col) (lm((col) ~ p[,i]))$coefficients[2])
  
  o <- apply(r,2,function(col) c(((exp(sum(col)) - 1) * 100),
                                 (lm((col) ~ p[,i]))$coefficients[2]))
  
  
  plot(o[2,], o[1,], las = 1, xlab = "Risk (Beta)", ylab = "Return (%)",
       main = "Risk & Return Plot", ylim = c(min(o[1,]) - 3, max(o[1,]) + 3),
       xlim = c(min(o[2,]), max(o[2,]))) # Plot
  
  abline(lm(o[1,] ~ o[2,]),col="red",lwd=3) # Add regression line
  
  abline(h = 0) # Add horizontal line 
  
  text(o[2,], o[1,]+1, labels=names(o[2,]), pos = 4) # Put labels
}
beta.return.plt(x = c("AMZN", "GOOGL", "MSFT", "AAPL", "NFLX", "META", "NVDA",
                      "TSLA", "ORCL", "INTC", "TSM", "ASML", "AMD", "QCOM",
                      "MU"), "2023-10-10")
