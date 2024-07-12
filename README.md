# Data Visualisation for Finance

![](https://github.com/vladislavpyatnitskiy/financial.data.visualisation/blob/main/Scatter%20Plot/FTSE%20100%20Bubble%20Plot.png?raw=true)

Welcome to the Finance Data Visualisation Toolkit – a powerful set of tools and resources designed to facilitate comprehensive data visualization in the field of finance. This repository aims to provide a collection of scripts, notebooks, and examples for creating insightful visualizations to aid financial analysis, decision-making, and reporting.

## 1. Scatter Plot 

### 1.1 Stock's Beta Visualisation
![](https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Plot%20for%20Beta/Betaplot.png?raw=true)

Can be used to visualise Stock's Beta. Beta measures the impact of index values on a single stock. The closer the dots to the line, the more significant the influence of the stock market on the share price.

I have modified the script so now it can be used to plot this kind of scatter plot for more than one column at the time. Therefore, when typing time series dataset, you will get plots for all securities in it.

* My R Script: https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Plot%20for%20Beta/Beta_for_stock.R

### 1.2 Scatter Plot of Risk & Return 

### 1.2.1 Scatter Plot using default tools

![](https://github.com/vladislavpyatnitskiy/financial.data.visualisation/blob/main/Scatter%20Plot/Risk%20%26%20Return%20Plot.png)

![](https://github.com/vladislavpyatnitskiy/financial.data.visualisation/blob/main/Scatter%20Plot/Risk%20&%20Return%20(Beta).png?raw=true)

* My R Script: https://github.com/vladislavpyatnitskiy/financial.data.visualisation/blob/main/Scatter%20Plot/Scatter%20Plot%20for%20Risk%20%26%20Return.R

* My R Script: https://github.com/vladislavpyatnitskiy/financial.data.visualisation/blob/main/Scatter%20Plot/Risk%20%26%20Return%20Plot%20(Beta).R

### 1.2.2 Scatter Plot via ggplot2 package

![](https://github.com/vladislavpyatnitskiy/financial.data.visualisation/blob/main/Scatter%20Plot/Scatter%20via%20ggplot2.png?raw=true)

![](https://github.com/vladislavpyatnitskiy/financial.data.visualisation/blob/main/Scatter%20Plot/Risk%20&%20Return%20(Beta)%20via%20ggplot2.png?raw=true)

### 1.3 Bubble Plot

#### Dow Jones Industrial Average
![](https://github.com/vladislavpyatnitskiy/financial.data.visualisation/blob/main/Scatter%20Plot/DJIA%2030%20Bubble%20Plot.png?raw=true)

#### FTSE 100
![](https://github.com/vladislavpyatnitskiy/financial.data.visualisation/blob/main/Scatter%20Plot/FTSE%20100%20Bubble%20Plot.png?raw=true)

#### DAX
![](https://github.com/vladislavpyatnitskiy/financial.data.visualisation/blob/main/Scatter%20Plot/DAX%20Bubble%20Plot.png?raw=true)

It is an extended version of standard risk and return graph where market capitalisation (cap) and industry infos are incorporated into graph. Market Cap is represented by the size of the dot or bubble (the higher the cap, the bigger the bubble), whereas the industry is showed by the colour of the bubble. The application was inspired by TED lectures and "Factfulness" of Swedish statistician Hans Rosling.

* My R Script: https://github.com/vladislavpyatnitskiy/financial.data.visualisation/blob/main/Scatter%20Plot/Risk%20and%20Return%20Bubble%20Plot.R
* Library: ggplot2, ggrepel, quantmod, timeSeries, rvest 

### 1.4 3D Scatter Plot
![](https://github.com/vladislavpyatnitskiy/financial.data.visualisation/blob/main/Scatter%20Plot/Scatter%20Plot%203D.png?raw=true)

3D version of a Scatter Plot where it is possible to visually evaluate the impact of 2 factors on the security.

* R script: https://github.com/vladislavpyatnitskiy/financial.data.visualisation/blob/main/Scatter%20Plot/Scatter%20Plot%203D.R

* R Libraries: Plotly, quantmod

### 1.5 3D Bubble Plot
![](https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Scatter%20Plot/3D%20Scatter%20Plot.png?raw=true)

Standard Scatter Plot is frequently used to visualise risk and return of securities, where risk is represented either by standard deviation or beta. Therefore, I think it is interesting solution to combine both of these risk metrics to see a whole picture. As standard deviation is a measure of total risk and beta of systematic risk, investor might find new insights for portfolio.

* R Libraries: Plotly, Tidyverse, quantmod

* Python Libraries: plotly.express as px

* R script: https://github.com/vladislavpyatnitskiy/financial.data.visualisation/blob/main/Scatter%20Plot/3D%20Scatter%20Plot.R

## 2. Line Plot

* Default R code: `plot()`

### 2.1 Single Stock Plot
![](https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Line%20Plot/Plot.png?raw=true)

• My R script: https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Line%20Plot/Plot.R

### 2.2 Plot with Moving Average
![](https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Line%20Plot/Line%20Plot.png?raw=true)

Script is an extension of function of quantmod library, which enables to visualise 50 and 200 days moving average together with main line plot. 

* My R code: https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Line%20Plot/Plot%20with%20MA.R

### 2.3 Drawdown Plot
![](https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Line%20Plot/Drawdown%20Plot.png?raw=true)

Used to concentrate on asset price/return decrease. It shows either prices lower than specified or negative return values so troughs are clearly seen.

* R script: https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Line%20Plot/Drawdown%20Płot.R

### 2.4 Plot of Returns and Value-at-Risk (VaR)
![](https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Line%20Plot/Value%20at%20Risk%20plot.png?raw=true)

It is also possible to visualise returns via line plot. It shows volatility of the security and might be applied to compare two securities. In addition other parameters can be shown, for example, VaR for long positions.

* R Code: https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Line%20Plot/Value-at-Risk.R

### 2.5 Dual Axis Chart

This chart is suitable for the comparison of two tickers. For example, comparison of stock and its index or commodity it relies on. It would not be suitable for comparison of several securities where Multiple Line Plot suits much better.

![](https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Dual%20Axis%20Chart/Dual%20Axis%20Chart.png?raw=true)

* R Code: https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Dual%20Axis%20Chart/Dual%20Axis%20Chart.R

![](https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Dual%20Axis%20Chart/Dual%20Axis%20Plot.png?raw=true)

* R Code: https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Dual%20Axis%20Chart/Alternative%20script%20for%20Dual%20Plot.R

### 2.6 Multiple Line Plot

![](https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Line%20Plot/Plot%20with%20several%20instruments.png?raw=true)

This plot is the best when comparing performance of more than two securities, as it shows real value changes for time. It enables to evaluate industry peer performance and indicate winners and outsiders of current market conditions.

* My R script: https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Line%20Plot/Line%20Plot%20with%20several%20securities.R

![](https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Line%20Plot/Monte%20Carlo/Monte%20Plot.jpeg?raw=true)

Another possible option for application is visualisation of Monte Carlo Simulation which is based on both stochastic processes and previous security dynamics. 

* My R script: https://github.com/vladislavpyatnitskiy/Risk-Management-Analytics/blob/main/Monte%20Carlo%20Method.R

## 3. Pie Chart

![](https://github.com/vladislavpyatnitskiy/financial.data.visualisation/blob/main/Pie%20Chart/Top%20Institutional%20Holders%20Pie%20Plot.png?raw=true)

This kind of plot can be utilised to show Major Institutional Holders of companies

My R Script: https://github.com/vladislavpyatnitskiy/financial.data.visualisation/blob/main/Pie%20Chart/Top%20Institutional%20Holders%20Pie%20Chart.R

![](https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Pie%20Chart/Pie%20Chart.png?raw=true)

It can be used for portfolio assessment.

* R Code: https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Pie%20Chart/Pie%20Chart.R

## 4. Statistics related plots

### 4.1 Histogram

fBasics Histogram

![](https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/AIG%20Histogram.png?raw=true)

My Script Histogram
![](https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Histogram/Histogram.png)

It shows distribution of stock return what can tell about past performance. For example, negatively skewed distribution demonstrates recent strong negative returns as well positively skewed distribution with positive returns. If you are a contrarian investor who prefers stocks that recently dropped, you should look for ones with negatively skewed distributions. Momentum investors would prefer positively skewed distributions as they select stocks that have been performing good recently hoping that growth will continue. Red line represents normal distribution enabling to evaluate approximate properties of generated histogram.

* Default R script : `hist()`
* fBasics R script: `histPlot()`
* My R script: https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Histogram/Own%20Hist%20Script.R

### 4.2 Barplot
![](https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/barplot/Barplot.png?raw=true)

It demonstrates the change in stocks' returns for the certain periods of time and enables to compare their performance. The default script does not sort parameters both ascending and descending correctly so some improvements have been completed to make script flawless.

* Default R Code: `barplot()`
* My R Code: https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/barplot/Bar_Plot.R

### 4.3 Boxplot
![](https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Boxplot/Stocks%20Boxplot.png?raw=true)

This one is similar to barplot but focused on volatility of several securities and enable to identify outliers in data. In default and fBasics scripts finance data should be extracted and cleaned prior plotting, while I modified it in a way it is only required to type tickers, time period and title inside a function. Therefore, plot process is more convenient especially if it is a frequent activity. 

* Default R Code: `boxplot()`
* fBasics R Code: `boxPlot()`
* My R Script: https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Boxplot/Boxplot.R

### 4.4 QQ-plot 
![](https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/QQ%20Plot/Q-Q%20Plot.png?raw=true)

Plot is a visualised evidence whether security's distribution is normally distributed or not (sort of visual version of Shapiro or Jarque Bera tests). In first case it should perfectly lie on a red line. When ends of the distribution line moving away from a general line proves data is not normally distributed.

* Default R Code: `qqnorm(lrtn)
qqline(lrtn, col = "red")`
* My R code: https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/QQ%20Plot/QQ%20plot.R

### 4.5 Heatmap
![](https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Heatmap/Heatmap.png?raw=true)

Visualised version of correlation table. Feature of the heatmap as it makes more clear for audience to see weak and strong correlations between certain things. For this particular example, weak correlation is positive sign as it is an indicator to add weakly correlated pairs to portfolio because security may decrease unsystematic risk.

* My R script: https://github.com/vladislavpyatnitskiy/Data-Visualisation-for-finance-in-R/blob/main/Heatmap/Heatmap%20script.R

## Data Source:
Yahoo! Finance API

## Libraries:

### 1. Data Visulisation:
* ggplot2
* broom
* tidyr
* ggrepel
* dplyr
* Plotly

### 2. Other:
* quantmod
* PortfolioAnalytics
* timeSeries
* fBasics
* rvest 
