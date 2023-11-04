# Libraries 
lapply(c("ggplot2", "data.table", "timeSeries"), require, character.only = T)

# Function to plot Monte Carlo Simulation
monte.carlo.plt <- function(c, ndays, n){ l.m.plt <- NULL 
  
  for (b in 1:ncol(c)){ m <- as.numeric(c[,b] / lag(c[,b])) # Plot each column
  
    m[1] <- 1 # Set up a value for first observation
    
    set.seed(0) # Calculate various scenarios of Stock Performance
    paths <- replicate(n, expr = round(sample(m, ndays, replace = T), 2))
    paths <- data.table(apply(paths, 2, cumprod))
    paths$days <- 1:nrow(paths)
    paths <- melt(paths, id.vars = "days")
    
    # Plot with all scenarios
    m.plt <- ggplot(paths, aes(x=days, y=(value-1)*100, col = variable)) +
      geom_line() + theme_bw() + theme(legend.position = "none") +
      ggtitle(sprintf("%s Performance by Monte Carlo", colnames(c[,b]))) +
      xlab("Days Invested") + ylab("Return (%)")
    
  l.m.plt <- list(l.m.plt, m.plt) } # Add to list
  
  return(l.m.plt) # Display plots
}
monte.carlo.plt(portfolioReturns, 1000, 100) # Test
