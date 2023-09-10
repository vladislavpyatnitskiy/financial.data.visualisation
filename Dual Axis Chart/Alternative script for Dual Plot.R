# library to use
library(latticeExtra)

# Function 
dual_axis_plot <- function(x){
  
  # Set up array for x-axis
  dates_for_dual_plot <- 1:nrow(x) 
  
  # Set up array for first y-axis
  first_dual_part <- as.matrix(x[,1])
  
  # Set up array for second y-axis
  second_dual_part <- as.matrix(x[,2])
  
  # Add all of them into one df
  data_for_dual_plot <- data.frame(dates_for_dual_plot,
                                   first_dual_part,
                                   second_dual_part)
  
  # First data to plot
  obj1 <- xyplot(first_dual_part ~ dates_for_dual_plot,
                 data_for_dual_plot,
                 ylab = colnames(first_dual_part),
                 xlab = "Trading Days",
                 main = "Stock Performance Comparison",
                 type = "l" , lwd=2, col="steelblue",
                 sub = "Source: Yahoo! Finance")
  
  # Second data to plot
  obj2 <- xyplot(second_dual_part ~ dates_for_dual_plot,
                 data_for_dual_plot,
                 ylab = colnames(second_dual_part),
                 type = "l", lwd=2, col="#69b3a2")
  
  # Make the plot with second y axis:
  doubleYScale(obj1, obj2, add.ylab2 = TRUE)
}
# Test 
dual_axis_plot(portfolioReturns)
