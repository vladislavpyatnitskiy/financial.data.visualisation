lapply(c("quantmod", "ggplot2", "tidyverse", "timeSeries"),
       require, character.only = TRUE)

# Chart with Moving Averages
chart_with_ma <- function(tickers, s = NULL, e = NULL, data = T){ 
  
  # When data is needed create empty variable to contain values
  if (isFALSE(data)){ p <- NULL
  
  # Data download
  for (Ticker in tickers){ 
    p <- cbind(p,getSymbols(Ticker,from=s,to=e,src="yahoo",auto.assign=F)[,4])}
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Remove NA
  
  colnames(p) <- tickers # Give tickers to data
  
  r <-as.timeSeries(p) } # Make data time series
  
  # For each column in data set # Create chart itself
  for (n in 1:ncol(r)){
  
    chartSeries(round(r[,n], 2),
                name=sprintf("%s Stock Performance",colnames(r[,n])),
                theme = "white",
                TA="addEMA(50, col='purple');addEMA(200, col='red')") }
}
# Test
chart_with_ma(tickers = "OMF", s = "2022-01-01", e = "2023-01-01", data = F)
