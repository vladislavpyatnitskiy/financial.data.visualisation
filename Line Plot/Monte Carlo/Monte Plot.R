# Libraries 
lapply(c("ggplot2", "data.table", "timeSeries","quantmod"),require,
       character.only=T)

monte.carlo.plt <- function(x, ndays, n){ # Plot Monte Carlo Simulation
  
  L <- NULL 
  
  for (A in x){ p <- getSymbols(A, src="yahoo", auto.assign=F)[,4] 
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- A # Put the tickers in data set
    
    p <- as.timeSeries(p)  # Make it time series & perform Data Cleaning

    m <- as.numeric(p / lag(p)) # Plot each column
    
    m[1] <- 1 # Set up a value for first observation
    
    set.seed(0) # Calculate various scenarios of Stock Performance
    paths <- replicate(n, expr = round(sample(m, ndays, replace = T), 2))
    paths <- data.table(apply(paths, 2, cumprod))
    paths$days <- 1:nrow(paths)
    paths <- melt(paths, id.vars = "days")
    
    # Plot with all scenarios
    plt <- ggplot(
      paths,
      aes(x=days, y=(value - 1) * 100, col = variable)) +
      geom_line() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(sprintf("%s Performance by Monte Carlo", A)) +
      xlab("Days Invested") +
      ylab("Return (%)")
    
    if (is.null(L)){ L <- plt } else { L <- list(L, plt) } }
    
  return(L) # Display plots
}
monte.carlo.plt(c("T", "C"), 1000, 100) # Test
