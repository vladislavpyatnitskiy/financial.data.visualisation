lapply(c("quantmod", "timeSeries"), require,character.only = T) # Libraries

# Function to visualise Beta Scatter Plot
beta.plt <- function(y, i = "^GSPC", s = NULL, e = NULL){
  
  y <- c(y, i) # Add benchmark to list
  
  p <- NULL # Create an empty variable
  
  for (A in y){ if (is.null(s) && is.null(e)) { # Download data when dates off
    
      p <- cbind(p, getSymbols(A, src = "yahoo", auto.assign = F)[,4])
    
    } else if (is.null(e)) { # When only start date is defined
    
      p <- cbind(p,getSymbols(A, from = s, src = "yahoo", auto.assign = F)[,4])
    
    } else if (is.null(s)) { # When only end date is defined
    
      p <- cbind(p, getSymbols(A, to = e, src = "yahoo", auto.assign = F)[,4])
    
    } else { # When both start date and end date are defined
    
      p <- cbind(p,getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F)[,4]) } }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  x = diff(log(as.timeSeries(p)))[-1,] # Returns and NA off
  
  R <- x[,-which(names(x) == i)] # Subset index from data set
  
  # Loop generates Scatter plots with trend line  
  for (n in 1:ncol(R)) { S <- R[,n]
  
    plot(x[,i], S, las = 1, ylab = sprintf("%s Return (%%)", colnames(S)),
         main = sprintf("%s Beta", colnames(S)), xlab = "Market Return (%)",
         sub = "Data Source: Yahoo Finance")
    
    abline(lm(S ~ x[,i]), col = "red", lwd = 3) }
}
beta.plt(c("AIG", "MET", "UNM", "HIG", "OMF"), i = "^GSPC") # Test
