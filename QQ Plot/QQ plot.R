# Function to generate Q-Q plot
qqgraph <- function(x, lg = F){
  
  # Calculate Returns and remove NA if applicable
  if (isTRUE(lg)) { x = diff(log(x))[-1,] }
  
  # Loop enables to generate multiple plots if there are more than 1   
  for (n in 1:ncol(x)){
    
    # Current Column
    security <- x[,n]
    
    # Plot QQ plot
    qqnorm(security,
           main = sprintf("%s Q-Q Plot", colnames(security)),
           sub = "Source: Yahoo Finance")
    
    # Add line
    qqline(security,
           col = "red",
           lwd = 3)
  }
}
# Test
qqgraph(returns_df)
