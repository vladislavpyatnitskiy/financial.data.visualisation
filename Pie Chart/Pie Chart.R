# Stock Portion in Pie Chart
slices <- c(0.278716291651169, 0.411679570881108, 0.0727978571595576, 0.176817615423501, 0.0599886648846643)

# Stock Tickers
lbls <- c("BELU", "BSPB", "IRAO", "LKOH", "RTKM")

# Percentage calculation
pct <- round(slices/sum(slices)*100)

# Add percents to labels 
lbls <- paste(lbls, pct) 

# Add % to labels 
lbls <- paste(lbls,"%",sep="") 

# Pie Chart configuration
pie(slices,labels = lbls,
    col=rainbow(length(lbls)),
    main="Dividend Yield Distribution",
    mtext = "hdds")
pie(slices,
    labels = lbls,
    main="Dividend Yield Distribution",
    sub = "Source: Investing.com")
