# Enables to demonstrate VaR 95%, 99% and 99.9% on graph with returns

t <- index(Oil)
ybar <- mean(Oil)
archmodel1 <- garchFit( ~ garch(1,1), data=coredata(Oil), trace=FALSE)
var5.garch <- ybar - 1.645 * garchmodel1@sigma.t
var1.garch <- ybar - 2.326 * garchmodel1@sigma.t
var0.1.garch <- ybar - 3.09 * garchmodel1@sigma.t
plot(t, Oil, type="l", main ="VaR GARCH(1,1)")
lines(t, var5.garch, col ="green")
lines(t, var1.garch, col ="blue")
lines(t, var0.1.garch, col ="red")
