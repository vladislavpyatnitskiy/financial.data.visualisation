plot_for_multiple_lines <- function(x, name_for_title = NULL){
  x <- diff(log(x))
  x[1,] <- 0
  title_name <- sprintf("%s", name_for_title)
  x <-apply(x, 2, function(col) (exp(cumsum(col))-1))
  plot(x[,1], ylim = c(min(x), max(x)),
       main = title_name,
       ylab = "Return")
  for (n in 1:(ncol(x))){
    lines(x[,n], col = n)
  }
}
plot_for_multiple_lines(stock_data, "Stock Performance")
