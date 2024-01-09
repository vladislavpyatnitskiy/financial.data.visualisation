# Libraries
lapply(c("quantmod","timeSeries","ggplot2","ggrepel"),require,character.only=T)

scatter.plt <- function(x, main = NULL, xlab = NULL, ylab = NULL,
                        s = NULL, e = NULL, data = F, lg = F){
  
  if (isTRUE(data)){ p <- NULL # Create an empty variable
  
    # Loop for data extraction & # Set up statements for start and end dates
    for (A in x){ if (is.null(s) && is.null(e)) {
      
      # When neither start date nor end date are defined
      p <- cbind(p, getSymbols(A, src = "yahoo", auto.assign = F)[,4])
      
    } else if (is.null(e)) { # When only start date is defined
      
      p <- cbind(p, getSymbols(A, from = s, src="yahoo", auto.assign = F)[,4])
      
    } else if (is.null(s)) { # When only end date is defined
      
      p <- cbind(p, getSymbols(A, to = e, src = "yahoo", auto.assign = F)[,4])
      
    } else { # When both start date and end date are defined
      
      p <- cbind(p, getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F)[,4]) }
    } 
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Put the tickers in data set
    
    x <- p } # Redefine variable, log it and then calculate return and sd
  
  if (isTRUE(lg) || isTRUE(data)){ x <- diff(log(as.timeSeries(x)))[-1,] }
  
  v <- t(apply(x,2,function(col) c(sd(col) * 1000, (exp(sum(col)) - 1) * 100)))
  
  # Plot
  ggplot(as.data.frame(v), mapping = aes(x=v[,1], y=v[,2])) + geom_point() + 
    theme_minimal() + geom_smooth(method = 'lm', se = F, col = "red") +
    geom_text_repel(aes(label=rownames(v))) + labs(title=main,x=xlab,y=ylab)
}
# Test
scatter.plt(x = c("AMZN", "GOOGL", "MSFT", "AAPL", "NFLX", "META", "NVDA",
                  "TSLA", "ORCL", "INTC", "TSM", "ASML", "AMD", "QCOM", "MU"),
            s = "2023-01-01", main = "Securities Performance", data = T,
            xlab = "Risk (Standard Deviation)", ylab = "Return (%)")
