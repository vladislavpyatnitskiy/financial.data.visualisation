# Function to create heatmap
heatmap.plt <- function(data){
  
  m.correlation = as.matrix(data) # Convert data into matrix
  
  c.correlation = ncol(m.correlation) # Get number of columns
  
  # Cut column and row names if there are longer than 4 characters
  names_for_correlation = abbreviate(colnames(m.correlation), 4)
  
  new_cor <- cor(m.correlation) # Calculate correlation coefficients
  
  # Create appropriate colour for each pair of correlation for heatmap
  ncolors_for_cor <- 10 * length(unique(as.vector(new_cor)))
  k_for_corr <- round(ncolors_for_cor/2)
  r_for_corr <- c(rep(0, k_for_corr), seq(0, 1, length = k_for_corr))
  g_for_corr <- c(rev(seq(0, 1, length = k_for_corr)), rep(0, k_for_corr))
  b_for_corr <- rep(0, 2 * k_for_corr)
  corrColorMatrix <- (rgb(r_for_corr, g_for_corr, b_for_corr))
  
  # Display heatmap
  image(x = 1:c.correlation,
        y = 1:c.correlation,
        z = new_cor[, c.correlation:1],
        col = corrColorMatrix,
        axes = FALSE, main = "",
        xlab = "",
        ylab = "")
  
  # Add labels for both axis
  axis(2, at = c.correlation:1, labels = colnames(m.correlation), las = 2)
  axis(1, at = 1:c.correlation, labels = colnames(m.correlation), las = 2)
  
  title(main = "Heatmap for Correlation") # Add title for heatmap
  
  box() # Box heatmap
  
  # Add correlation values as text strings to each heatmap cell
  x = y = 1:c.correlation
  n_x = n_y = length(y)
  xoy = cbind(rep(x, n_y), as.vector(matrix(y, n_x, n_y, byrow = TRUE)))
  coord_for_corr = matrix(xoy, n_x * n_y, 2, byrow = FALSE)
  X_for_corr = t(new_cor)
  for (i in 1:(c.correlation ^ 2)) {
    text(coord_for_corr[i, 1],
         coord_for_corr[c.correlation * c.correlation + 1 - i, 2],
         round(X_for_corr[coord_for_corr[i,1], coord_for_corr[i, 2]],digits=2),
         col = "white",
         cex = 1.5) }
}
# Test
heatmap.plt(data = stock_log_rets)
