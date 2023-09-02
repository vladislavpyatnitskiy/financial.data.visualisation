plot_data <- function(x){
  for (n in 1:ncol(x)){
    plot_data_names <- colnames(x[,n])
    main_plot_data <- sprintf("%s Stock Performance", plot_data_names)
    ylab_plot_data <- sprintf("%s Prices", plot_data_names)
    plot(x[,n],
         main = main_plot_data,
         sub = "Source: Yahoo! Finance",
         xlab = "Trading Days",
         ylab = ylab_plot_data
         )
  }
}
plot_data(stock_data)
