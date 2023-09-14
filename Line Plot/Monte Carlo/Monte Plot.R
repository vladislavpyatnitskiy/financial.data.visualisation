# Libraries 
lapply(c("ggplot2",
         "data.table",
         "timeSeries"),
       require,
       character.only = TRUE)

# Monte Function
monte_carlo <- function(c, ndays, n){
  
  # Create an empty list to contain plots
  all_monte_graphs <- NULL
  
   # For each column in data set
  for (b in 1:(ncol(c))){
    
    # Take column names for the graph
    names_for_montec <- colnames(c[,b])
    
    # Title for Graph
    main_for_monte <- sprintf("%s Performance by Monte Carlo",
                              names_for_montec)
    
    # Calculate returns
    lrtn_monte <- (c[,b]) / lag(c[,b])
    
    # Change class type to numeric
    lrtn_monte <- as.numeric(lrtn_monte)
    
    # Set up a standard index column
    lrtn_monte[1] <- 1
    
    # Calculate various scenarios of Stock Performance
    set.seed(0)
    paths <- replicate(n, 
                       expr = round(sample(lrtn_monte,
                                           ndays,
                                           replace = TRUE),
                                    2))
    
    # Calculate cumulative values for each scenarios
    paths <- apply(paths,
                   2,
                   cumprod)
    
    # Transform it into Time Series
    paths <- data.table(paths)
    paths$days <- 1:nrow(paths)
    paths <- melt(paths,
                  id.vars = "days")
    
    # Make Line Charts with all scenarious
    monte_graph <- ggplot(paths,
                          aes(x = days,
                              y = (value - 1) * 100,
                              col = variable)) +
      geom_line() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(main_for_monte) +
      xlab("Days Invested") + 
      ylab("Portfolio Return (%)")
    
    # Save generated plot to list
    all_monte_graphs <- list(all_monte_graphs, monte_graph)
  }
  
  # Display plots 
  return(all_monte_graphs)  
}
# Test
monte_carlo(portfolioReturns, 1000, 100)
