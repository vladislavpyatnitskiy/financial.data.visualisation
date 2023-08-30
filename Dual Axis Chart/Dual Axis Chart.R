suppressPackageStartupMessages({
  library(quantmod)
  library(dplyr)
  library(ggplot2)
})

tickers <- c("WFG", "^GSPC")
start_date <- "2022-04-10"

portfolioPrices <- NULL
for (Ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols(Ticker,
                                      from = start_date,
                                      src = "yahoo",
                                      auto.assign=FALSE)[,4])
}
head(portfolioPrices)

portfolioPrices <- portfolioPrices[apply(portfolioPrices,
                                         1,
                                         function(x) all(!is.na(x))),]
colnames(portfolioPrices) <- c("WFG",
                               "GSPC")
portfolioReturns <- ROC(portfolioPrices,
                        type = "discrete")

autoplot(portfolioReturns,
         facets = NULL) +
  scale_color_manual(values = c(WFG = "black",
                                GSPC = "red")) +
  ggtitle("Stock Returns") +
  theme_bw()

Prices <- fortify(portfolioPrices)
(fac <- with(Prices,
             range(WFG)/range(GSPC)))
#> [1] 0.01676810 0.02124536
fac <- max(fac)

ggplot(data = Prices, aes(Index, WFG)) +
  geom_line(aes(color = "WFG")) +
  geom_line(aes(y = GSPC * fac,
                color = "GSPC")) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%b %Y") +
  scale_color_manual(values = c(WFG = "red",
                                GSPC = "black")) +
  scale_y_continuous(sec.axis = sec_axis(~ . / fac,
                                         name = "S&P 500 Closes")) +
  labs(x = "Date", y = "WFG Close Price",
       color = "") +
  ggtitle("Stock Price") +
  theme_bw()
