# Enables to demonstrate VaR 95%, 99% and 99.9% on graph with returns
library(fGarch)

val_risk <- function(x){
  # Calculate returns
  x=diff(log(x))
  x <- x[-1,]

  # Set index
  t <- index(x)
  
  # Calculate Mean
  ybar <- mean(x)

  # GARCH
  garchmodel1 <- garchFit( ~ garch(1,1), data=coredata(x), trace=FALSE)

  # calculate VaRs
  var5.garch <- ybar - 1.645 * garchmodel1@sigma.t
  var1.garch <- ybar - 2.326 * garchmodel1@sigma.t
  var0.1.garch <- ybar - 3.09 * garchmodel1@sigma.t

  # Plot graph
  plot(t, x, type="h", xlab = "Trading Days",
       ylab = "Returns", main ="VaR GARCH(1,1)")

  # visualise all 3 VaRs
  lines(t, var5.garch, col ="green")
  lines(t, var1.garch, col ="blue")
  lines(t, var0.1.garch, col ="red")
}
val_risk(portfolioReturns)
