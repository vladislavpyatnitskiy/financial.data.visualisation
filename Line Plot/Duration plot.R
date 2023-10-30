# Function to show Duration dependency on Year to Maturity 
duration_plot <- function(P, C, r, f = 1, ytm = 100, D = NULL){
  
  for (y in seq(ytm,from = 1)){ P. <- P * (1 + C/f)/(1 + r/f)^(y*f) # Principle
    
    PV <- NULL # Variable to store PV
    payments <- NULL # Variable to store payments
    
    for (n in 1:(y * f - 1)){ PV <- cbind(PV, C * P / f / (1 + r / f) ^ n * f) 
    
      payments <- cbind(payments, n * PV[n]) } # Coupon PV
    
    # Duration
    D <- rbind(D,(sum(payments[seq(y*f-1)])+P.*y*f)/(P.+sum(PV[seq(y*f-1)])))}
  
  # Generate plot
  plot(x = seq(ytm, from = 1),
       y = D,
       type = "l",
       xlab = "Bond Year to Maturity",
       ylab = "Bond Duration",
       main = "Bond Duration Dependency On Year to Maturity",
       sub = "Source: None",
       col = "red",
       lwd = 3,
       las = 1)
  
  axis(side = 1, at=seq(10, 100, 10)) # Axes
  axis(side = 2, at=seq(0, 100, 1), las = 1)
  
  abline(v = seq(0, 100, 10), lty = 3, col = "grey") # lines 
  abline(h = seq(0, 100, 1), lty = 3, col = "grey")
}
# Test
duration_plot(1000, 0.01, 0.1)
