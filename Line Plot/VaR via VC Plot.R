# Libraries
lapply(c("fGarch", "timeSeries"),
       require, character.only = TRUE)

# Function to generate plot with VaR values (V-C method) 
val_risk <- function(x, lg = F){
  
  # Calculate Returns and remove NA if needed
  if (isTRUE(lg)) { x=diff(log(x))[-1,] }
  
  # For each column in data set
  for (n in 1:ncol(x)){

    # Assign variable for each column proceeding loop
    security <- x[,n]
    
    # Set index
    t <- seq(nrow(security))
    
    # Set up GARCH model
    garchmodel1 <- garchFit( ~ garch(1,1), data=coredata(security),
                             trace=FALSE)
    
    # Calculate VaR at 95%
    var5.garch <- mean(security) + qnorm(0.05) * garchmodel1@sigma.t
    
    # Calculate VaR at 99%
    var1.garch <- mean(security) + qnorm(0.01) * garchmodel1@sigma.t
    
    # Calculate VaR at 99.9%
    var0.1.garch <- mean(security) + qnorm(0.001) * garchmodel1@sigma.t
    
    # Plot graph
    plot(t, security, type="l",
         xlab = "Trading Days",
         ylab = "Returns",
         main =sprintf("%s VaR GARCH(1,1)", colnames(security)),
         col = "black",
         sub = "Source: Yahoo! Finance",
         las = 1)
    
    # Create lines of each VaR value
    lines(t, var5.garch, col ="green")
    lines(t, var1.garch, col ="blue")
    lines(t, var0.1.garch, col ="red")
  }
}
# Test
val_risk(stock_data, lg = T)
