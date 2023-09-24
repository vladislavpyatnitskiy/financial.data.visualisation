# Barplot
brplt <- function(x, y, col = "blue", main = NULL, xlim = NULL){
    # Define the time period
    x <- merge(x[y,],
                  x[nrow(x),])
    
    # Calculate the change and remove NA
    x=diff(log(x))[-1,]
    
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
            col = col,
            main = main,
            sub = "Source: Yahoo! Finance",
            xlab = "Returns",
            ylab = "",
            xlim = xlim
    )
    
  }
