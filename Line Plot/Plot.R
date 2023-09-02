plot_data <- function(x){
  # For each column in data set
  for (n in 1:ncol(x)){
    # Take names from columns
    plot_data_names <- colnames(x[,n])
    
    # Put them in string to be reflected in the main of the plot
    main_plot_data <- sprintf("%s Stock Performance", plot_data_names)
    
    # And on the ylab as well
    ylab_plot_data <- sprintf("%s Prices", plot_data_names)
    
    # Plot finally
    plot(x[,n],
         main = main_plot_data,
         sub = "Source: Yahoo! Finance",
         xlab = "Trading Days",
         ylab = ylab_plot_data
         )
  }
}
plot_data(stock_data)
