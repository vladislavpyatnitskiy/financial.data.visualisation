lapply(c("quantmod", "timeSeries", "plotly"), require, character.only=T) # libs

scatter.plt.3d <- function(y,s=NULL,e=NULL,zaxis=NULL,yaxis=NULL,xaxis=NULL){
  
  p <- NULL # 4 scenarios for data extraction 
  
  for (A in y){ if (is.null(s) && is.null(e)) {
    
      q <- getSymbols(A, src = "yahoo", auto.assign = F)
      
      } else if (is.null(e)){ q<-getSymbols(A,from=s,src="yahoo",auto.assign=F)
      
      } else if (is.null(s)){ q<-getSymbols(A,to=e,src="yahoo",auto.assign=F)
      
      } else { q <- getSymbols(A, from=s, to=e, src="yahoo", auto.assign=F) }
        
    p <- cbind(p, q[,4]) } # Join all columns into one data frame
    
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  r <- as.data.frame(p) # Make it time series
  
  plot_ly(r, x = ~r[,3], y = ~r[,2], z = ~r[,1]) %>% add_markers() %>%
    layout(scene = list(xaxis = list(title=xaxis), yaxis = list(title=yaxis),
                        zaxis = list(title=zaxis)))
}
scatter.plt.3d(y = c("UNM", "CL=F", "GC=F"), s = "2022-01-01", zaxis = "XOM",
               yaxis = "Oil", xaxis = "Gold")
