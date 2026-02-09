library("ggplot2") # Library

bar.plt.r <- function(x){ # Bar Plot of Portfolio Returns
  
  L <- NULL
  
  for (m in 1:ncol(x)){ y <- x[,m]
  
    y <- diff(log(y))[-1,]
    
    ticker <- colnames(y)
    
    y <- data.frame(as.Date(rownames(y)), y) # Data Frame with dates and values
    
    rownames(y) <- seq(nrow(y)) # Create sequence for index column
    
    D <- NULL # Define variable to contain values
    
    for (n in 2:ncol(y)){ s <- y[,n] # Loop to make monthly data
    
      # Convert daily data to monthly
      v <- round(tapply(s, format(as.Date(y[,1]),"%Y-%m"), sum), 4) * 100
      
      df.rownames <- rownames(v) # Take dates from index column
      
      v <- data.frame(df.rownames, v) # Join with new data set
      
      rownames(v) <- seq(nrow(v)) # Generate sequence for index column
      
      colnames(v)[1] <- 'Date' # Name column as Date
      
      # If defined empty variable is still empty # Put new dataset there
      if (is.null(D)){ D = v } else { D = merge(x = D, y = v, by = "Date") } }
      
    D <- as.data.frame(D) # Convert to data frame format
    
    D[,1] <- substr(D[,1], 3, 7)
    
    colnames(D)[2] <- "Returns" # Rename column to Returns
    
    D$fill <- ifelse(D$Returns < 0, "red3", "green4") # Colour column
    
    P <- ggplot(
      D, aes(x = Date, y = Returns)) + 
      theme_minimal() +
      geom_bar(position = "stack", stat = "identity", fill = D$fill) + 
      labs(
        title = sprintf("%s Monthly Returns", ticker), 
        x = "Months",
        y = "Returns (%)"
        )
    
    if (is.null(L)){ L <- list(P) } else { L[[m]] <- P } } # Add plot to list
    
  L # Display
}
bar.plt.r(stock_data) # Test
