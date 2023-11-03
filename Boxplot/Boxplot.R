lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

# Function to create boxplot
box.plt <- function(x, s = NULL, e = NULL, main = NULL, data = T){
  
  if (isTRUE(data)){ # When data is needed
  
    p <- NULL # Create empty variable to store values
    
    for (A in x) # For each ticker get data
      p <- cbind(p,getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F)[,4])
    
    p <- p[apply(p,1,function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Give column names 
    
    p <- diff(log(as.timeSeries(p)))[-1,]
    
    # Boxplot
    boxplot.matrix(p,main=main,title=F, las=1, col="steelblue",ylab="Returns",
                   xlab="Data Source: Yahoo Finance",ylab="Returns")
    abline(h = 0, col = "grey", lty = 3) } else { # data is ready
      
    # Boxplot
    boxplot.matrix(x,main=main,title=F,las=1,col="steelblue",ylab="Returns",
                   xlab="Data Source: Yahoo Finance")
    
    abline(h = 0, col = "grey", lty = 3) }
}
# Test
box.plt(c("AAPL", "MSFT", "META", "GOOGL", "AMZN"),s = "2020-01-01",
        main = "Boxplot of IT companies")
