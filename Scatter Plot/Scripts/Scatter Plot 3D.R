lapply(c("quantmod","timeSeries","plotly"),require,
       character.only=T) # libraries

scatter.plt.3d <- function(y, s = NULL, e = NULL, zaxis = NULL, yaxis = NULL,
                           xaxis = NULL){
  
  p <- NULL # Create an empty variable
  
  # Loop for data extraction & # Set up statements for start and end dates
  for (Ticker in y){ if (is.null(s) && is.null(e)) {
    
      # When neither start date nor end date are defined
      p <- cbind(p, getSymbols(Ticker, src = "yahoo", auto.assign=F)[,4])
      
    } else if (is.null(e)) { # When only start date is defined
      
      p <- cbind(p, getSymbols(Ticker, from = s,src="yahoo",auto.assign=F)[,4])
      
    } else if (is.null(s)) { # When only end date is defined
      
      p <- cbind(p,getSymbols(Ticker,to=e,src="yahoo",auto.assign=F)[,4])
      
    } else { # When both start date and end date are defined
      
      p<-cbind(p,getSymbols(Ticker,from=s,to=e,src="yahoo",auto.assign=F)[,4])}
    }
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  r <- as.data.frame(p) # Make it time series
  
  fig <- plot_ly(r, x = ~r[,3], y = ~r[,2], z = ~r[,1])
  fig <- fig %>% add_markers()
  fig <- fig %>% layout(scene = list(xaxis = list(title = xaxis),
                                     yaxis = list(title = yaxis),
                                     zaxis = list(title = zaxis)))
  fig # Display
}
scatter.plt.3d(y = c("UNM", "CL=F", "GC=F"), s = "2022-01-01", zaxis = "UNM",
               yaxis = "Oil", xaxis = "Gold")
