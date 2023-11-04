# Function to facilitate Pie Plot generation
pie.plt <- function(slices, tickers, main = NULL, sub = NULL){
  
  pie(slices, col = rainbow(length(lbls)), main = main, sub = sub,
      labels = sprintf("%s %s%%", tickers, round(slices / sum(slices) * 100))) 
}
# Test
pie.plt(slices = c(0.278716291651169, 0.411679570881108, 0.0727978571595576,
                   0.176817615423501, 0.0599886648846643),
        tickers = c("BELU", "BSPB", "IRAO", "LKOH", "RTKM"),
        main="Dividend Yield Distribution", sub="Data Source: Investing.com")
