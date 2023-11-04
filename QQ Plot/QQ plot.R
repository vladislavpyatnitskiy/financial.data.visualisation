# Function to generate Q-Q plot
qq.plt <- function(x, lg = F){ if (isTRUE(lg)) { x = diff(log(x))[-1,] }
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each asset make qq plot
  
  qqnorm(s, main = sprintf("%s Q-Q Plot", colnames(s)), las = 1) # QQ plot
  
  axis(side = 2, at=seq(-1, 1, .01), las = 1) # Y-axis with 0.01 intervals
  
  qqline(s, col = "red", lwd = 3) } # Add line
  
  # Add grey dotted vertical & horizontal lines
  for (n in seq(-3, 3, 1)){ abline(v = n, col = "grey", lty = 3) }
  for (n in seq(-1, 1, .01)){ abline(h = n, col = "grey", lty = 3) }
}
qq.plt(returns_df) # Test
