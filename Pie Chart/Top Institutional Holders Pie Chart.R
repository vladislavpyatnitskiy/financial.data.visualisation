lapply(c("plotly", "rvest"), require, character.only = T) # Libraries

holders.pie.plt <- function(x){ # Plot with info of Institutional holders
  
  a <- sprintf("https://finance.yahoo.com/quote/%s?p=%s&.tsrc=fin-srch", x, x)
  s <- sprintf("https://finance.yahoo.com/quote/%s/holders?p=%s", x, x)
  
  s <- read_html(s) # Read html info
  a <- read_html(a) # Read html info
  
  s.yahoo <- a %>% html_nodes('body') %>% .[[1]] -> tab1 # Assign Body
  a.yahoo <- s %>% html_nodes('table') %>% .[[2]] -> tab2 # Assign Table 
  
  y <- tab1 %>% html_nodes('div') %>% html_nodes('h1') %>% html_text()
  w <- tab2 %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  v <- NULL # Create lists to contain variables & Subset excess row
  
  for (n in 0:(length(w) / 5)){ # Scrape data for holders and portions
    
    v <- rbind(v, cbind(w[(1+n*5)], read.fwf(textConnection(w[(4+n*5)]),
                                             widths=c(nchar(w[(4+n*5)])-1,1),
                                             colClasses = "character"))) } 
  v <- v[-nrow(v),][,-ncol(v)] 
  
  v[,2] <- as.numeric(v[,2]) # Change format to numeric
  
  v[nrow(v) + 1,] = c("Others", 100 - sum(v[,2])) # Numbers for others
  
  plot_ly(v, labels = ~v[,1], values = ~as.numeric(v[,2]) / 100 , type = 'pie',
          textposition = 'outside',textinfo = 'percent') %>%
    layout(title = sprintf("Top Institutional Holders of %s", y),
           xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
}
holders.pie.plt("M") # Test
