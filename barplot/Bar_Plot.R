# Barplot
brplt <- function(x){
    # Define the time period
    x <- merge(x[1,],
                  x[nrow(x),])
    
    # Calculate the change
    x=diff(log(x))
    x<-x[-1,]
    
    #
    colnames(x)
    x1 <- as.data.frame(x)
    x1 <- sort(x1, decreasing = T)
    tickers_for_barplot <- colnames(x1)
    x <- as.numeric(x)
    x <- sort(x, decreasing = T)
    
    # Create barplot
    barplot(x,
            names.arg = tickers_for_barplot,
            horiz = T,
            las=1,
            col = "blue",
            main = "Insurance Stocks Performance for the Year",
            sub = "Source: Yahoo! Finance",
            xlab = "Returns",
            ylab = "",
            xlim = c(-0.15, 0.2)
    )
    
  }
