# Libraries
lapply(c("quantmod","timeSeries","ggplot2","ggrepel"),require,character.only=T)

# Scatter Plot for selected securities with Beta via ggplot2
scatter.plt.beta <- function(x, s = NULL,e = as.Date(Sys.Date()), i = "^GSPC",
                            xlab = NULL, ylab = NULL, main = NULL){
  
  x <- c(x, i) # Add S&P 500 ticker to rest
  
  p <- NULL # Empty variable and get data from Yahoo! Finance
  
  for (A in x){ p <- cbind(p, getSymbols(A, from = s, to = e, src = "yahoo",
                                         auto.assign = F)[,4]) }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- x # Give column names 
  
  p <- diff(log(as.timeSeries(p)))[-1,] # Calculate logs
  
  o <- t(apply(p[,-which(names(p) == i)], 2,
               function(col) c(((exp(sum(col)) - 1) * 100),
                               (lm((col) ~ p[,i]))$coefficients[2])))
  # Plot
  ggplot(as.data.frame(o), mapping = aes(x=o[,2], y=o[,1])) + geom_point() + 
    theme_minimal() + geom_smooth(method = 'lm', se = F, col = "red") +
    labs(title=main,x=xlab,y=ylab) + geom_text_repel(aes(label=rownames(o))) 
}
scatter.plt.beta(x = c("AMZN", "GOOGL", "MSFT", "AAPL", "NFLX", "META", "NVDA",
                       "TSLA", "ORCL", "INTC", "TSM", "ASML", "AMD", "QCOM",
                       "MU"), "2023-10-10", xlab = "Risk (Beta)",
                 ylab = "Return (%)", main = "Risk & Return")
