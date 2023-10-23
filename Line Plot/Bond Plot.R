# Function to plot a relationship between rate and price
bond.plt <- function(P, C = 0, ytm, f = 1, l.bound.r = 0, h.bound.r = 100){
  
  # Empty lists for yields and prices
  l.r <- 0
  l.B.prices <- 0
  
  # For each yield value
  for (n in l.bound.r:h.bound.r){ r <- 0.01 * (1 * n)
    
    # Denominator
    d <- (1 + r / f) ^ ytm * f
    
    # Add rate value to list
    l.r <- c(l.r, r)
    
    # Coupon part
    C.part <- (C * P) / f
    
    # Rate part
    r.part <- (f / r - f / (r * d))
    
    # Principle part
    P.part <- P / d
    
    # Add bond price value to list
    l.B.prices <- c(l.B.prices, C.part * r.part + P.part) }
  
  # Generate plot
  plot(x = l.r,
       y = l.B.prices,
       type = "l",
       xlab = "Bond Yield",
       ylab = "Bond Price",
       main = "Bond Pricing Dependency On Yield",
       sub = "Source: None",
       col = "red",
       lwd = 3)
  #ylim = c(min(l.B.prices), max(l.B.prices))
}
# Test
bond.plt(P = 1000, C = 0.08, ytm = 3, f = 1, l.bound.r = 0, h.bound.r = 20)
