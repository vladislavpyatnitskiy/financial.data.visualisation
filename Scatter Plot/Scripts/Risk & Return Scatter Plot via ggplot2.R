# Libraries
lapply(c("quantmod","timeSeries","ggplot2","ggrepel"),require,character.only=T)

scatter.plt <- function(x, main = NULL, xlab = NULL, ylab = NULL,
                        s = NULL, e = NULL, data = T, lg = F){
  
  if (data){ p <- NULL # Create an empty variable
    
    src <- "yahoo"
    
    getData <- function(A, s, e) {
      if (is.null(s) && is.null(e)) return(getSymbols(A,src=src,auto.assign=F)) 
      if (is.null(e)) return(getSymbols(A, from = s, src=src, auto.assign=F)) 
      if (is.null(s)) return(getSymbols(A, to = e, src=src, auto.assign=F)) 
      return(getSymbols(A, from = s, to = e, src=src, auto.assign=F)) 
    }
    for (A in x){ p <- cbind(p, getData(A, s, e)[,4]) } # Join data
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Put the tickers in data set
    
    x <- p } # Redefine variable, log it and then calculate return and sd
    
  if (lg | data) x <- diff(log(as.timeSeries(x)))[-1,]
  
  v <- t(apply(x,2,function(col) c(sd(col) * 1000, (exp(sum(col)) - 1) * 100)))
  
  if (is.null(xlab)) xlab = "Risk (Standard Deviation)"
  if (is.null(ylab)) ylab = "Return (%)"
  if (is.null(main)) main = "Securities Performance"
  
  # Plot
  ggplot(
    as.data.frame(v), mapping = aes(x = v[,1], y = v[,2])
    ) +
    geom_point() + 
    theme_minimal() +
    geom_smooth(method = 'lm', se = F, col = "red") +
    geom_text_repel(aes(label = rownames(v))) +
    labs(title = main, x = xlab, y = ylab)
}
# Test
scatter.plt(x = c("AMZN", "GOOGL", "MSFT", "AAPL", "NFLX", "META", "NVDA",
                  "TSLA", "ORCL", "INTC", "TSM", "ASML", "AMD", "QCOM", "MU"),
            s = "2023-01-01")
