# Function to generate Q-Q plot
qqgraph <- function(x){
         
  # Calculate Returns and get rid of NA
  x=diff(log(x))[-1,]
  
  # Loop enables to generate multiple plots if there are more than 1   
  for (n in 1:ncol(x)){
           
    # Take ticker from Time Series to reflect on plot
    qqstocknames <- colnames(x[,n])
    
    # Create variable to reflect in main section of plot settings
    qqmain <- sprintf("%s Q-Q Plot", qqstocknames)
    
    # Plot QQ plot
    qqnorm(x[,n],
           main = qqmain,
           sub = "Source: Yahoo Finance"
    )
    
    # Add line
    qqline(x[,n],
           col = "red",
           lwd = 3)
  }
}
# Test
qqgraph(portfolioReturns)
