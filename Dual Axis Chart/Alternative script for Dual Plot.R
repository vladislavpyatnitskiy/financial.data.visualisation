library(latticeExtra) # library to use

# Dual Plot
dual.plt <- function(x){ d <- 1:nrow(x) # Set up array for x-axis
  
  f <- as.matrix(x[,1]) # Set up array for first y-axis
  
  s <- as.matrix(x[,2]) # Set up array for second y-axis
  
  data <- data.frame(d, f, s) # Add all of them into one df
  
  # First data to plot
  obj1 <- xyplot(f ~ d,data,ylab=colnames(f),xlab="Trading Days", type="l",
                 main="Stock Performance Comparison", lwd=2, col="steelblue",
                 sub = "Data Source: Yahoo! Finance")
  
  # Second data to plot
  obj2<-xyplot(s ~ d,data,ylab=colnames(s),type="l",lwd=2,col="#69b3a2")
  
  doubleYScale(obj1, obj2, add.ylab2 = T) # Make the plot with second y axis:
}
dual.plt(portfolioReturns) # Test 
