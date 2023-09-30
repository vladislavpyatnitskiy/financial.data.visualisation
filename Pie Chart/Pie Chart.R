# Function to facilitate Pie Plot generation
piePlot <- function(slices, tickers, main = NULL, sub = NULL){
  
  # Percentage calculation
  pct <- round(slices / sum(slices) * 100)
  
  # Join tickers with percentages
  lbls <- sprintf("%s %s%%", tickers, pct)
  
  # Pie Chart configuration
  pie(slices,
      labels = lbls,
      col = rainbow(length(lbls)),
      main = main,
      sub = sub) 
}

# Test
piePlot(slices = c(0.278716291651169, 0.411679570881108, 0.0727978571595576,
                   0.176817615423501, 0.0599886648846643),
        tickers = c("BELU", "BSPB", "IRAO", "LKOH", "RTKM"),
        main = "Dividend Yield Distribution",
        sub = "Source: Investing.com")
