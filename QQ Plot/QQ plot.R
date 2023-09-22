# Function to generate Q-Q plot
qqgraph <- function(x){
  
  # Calculate Returns and get rid of NA
  x=diff(log(x))[-1,]
  
  # Loop enables to generate multiple plots if there are more than 1   
  for (n in seq(colnames(x))){
    
    # Plot QQ plot
    qqnorm(x[,n],
           main = sprintf("%s Q-Q Plot", colnames(x[,n])), # Security title
           sub = "Source: Yahoo Finance"
    )
    
    # Add line
    qqline(x[,n],
           col = "red",
           lwd = 3)
  }
}
# Test
qqgraph(stock_data)
