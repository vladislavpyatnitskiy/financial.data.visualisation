# Relationship between rate and price
bond.y.plt <- function(P, C = 0, y, f = 1, l = 0, h = 100){
  
  p <- NULL # Empty lists for prices
  
  # For each yield value add bond price value to list
  for (n in l:h){ d <- (1 + .01 * n / f) ^ -y * f
  
    p <- c(p, P * (C / .01 / n * (1 - d) + d)) }
  
  par(mar = rep(5, 4)) # Margins
  
  plot(
    x = as.vector(seq(l, h) * .01) * 100,
    y = p,
    type = "l",
    xlab = "Bond Yield (%)",
    ylab = "Bond Price",
    main = "Bond Pricing Dependency On Yield (%)",
    col = "red",
    lwd = 3,
    las = 1
    ) # Plot
  
  axis(side = 4, las = 2) # Right y-axis
  
  grid(nx = NULL, ny = NULL, col = "grey", lty = 3, lwd = 1) # Dotted lines
}
bond.y.plt(P = 1000, C = .08, y = 3, f = 1, l = 0, h = 100) # Test
