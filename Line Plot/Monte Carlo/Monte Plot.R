# Libraries 
lapply(c("ggplot2", "data.table", "timeSeries"), require, character.only = T)

# Function to plot Monte Carlo Simulation
monte.carlo.plt <- function(c, ndays, n){

  all_monte_graphs <- NULL # Create an empty list to contain plots
  
  # For each column in data set
  for (b in 1:ncol(c)){ lrtn_monte <- as.numeric(c[,b] / lag(c[,b]))
    
    lrtn_monte[1] <- 1 # Set up a value for first observation
    
    set.seed(0) # Calculate various scenarios of Stock Performance
    paths <- replicate(n,expr = round(sample(lrtn_monte,ndays,replace = T), 2))
    paths <- data.table(apply(paths, 2, cumprod))
    paths$days <- 1:nrow(paths)
    paths <- melt(paths, id.vars = "days")
    
    # Plot with all scenarios
    monte_graph <- ggplot(paths, aes(x=days,y=(value-1)*100, col = variable)) +
      geom_line() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(sprintf("%s Performance by Monte Carlo", colnames(c[,b]))) +
      xlab("Days Invested") + 
      ylab("Return (%)")
    
    all_monte_graphs <- list(all_monte_graphs, monte_graph) } # Add to list
  
  return(all_monte_graphs) # Display plots
}
# Test
monte.carlo.plt(portfolioReturns, 1000, 100)
