# Function to show Duration dependency on Year to Maturity 
duration.plt.ytm <- function(P, C, r, f = 1, ytm = 100){ D = NULL
  
  for (y in seq(ytm,from = 1)){ Pr <- P * (1 + C/f)/(1 + r/f)^(y*f) # Principle
  
    PV <- NULL # Variable to store PV
    pays <- NULL # Variable to store payments
    
    for (n in 1:(y * f - 1)){ PV <- cbind(PV, C * P / f / (1 + r / f) ^ n * f) 
    
      pays <- cbind(pays, n * PV[n]) } # Coupon PV
    
    # Duration
    D <- rbind(D, (sum(pays[seq(y*f-1)])+Pr*y*f) / (Pr+sum(PV[seq(y*f-1)]))) }
  
  # Generate plot
  plot(x = seq(ytm, from = 1),
       y = D,
       type = "l",
       xlab = "Bond Year to Maturity",
       ylab = "Bond Duration",
       main = "Bond Duration Dependency On Year to Maturity",
       col = "red",
       lwd = 3,
       las = 1)
  
  axis(side = 4, las = 2)
  
  grid(nx = NULL, ny = NULL, lty = 3, col = "grey") # Horizontal lines
  
  par(mar = c(5, 5, 5, 4)) # Define borders of the plot
}
duration.plt.ytm(1000, 0.01, 0.1) # Test
