library("rvest") # Library

smb.plt <- function(x){ # Plot with Market Cap & Price to Book Value Per Share
  
  m <- NULL
  
  for (n in 1:length(x)){ v <- x[n] # Subset ticker
  
    p <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",v,v)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab
    
    i <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    s <- i[grep("Market Cap", i) + 1] # Market Cap
    
    s<-read.fwf(textConnection(s),widths=c(nchar(s)-1,1),
                colClasses = "character")
    
    if (s[1,2] == "M"){ s <- as.numeric(s[1,1]) / 1000 }
    
    else if (s[1,2] == "T"){ s <- as.numeric(s[1,1]) * 1000 }
    
    else s <- as.numeric(s[1,1]) # Format to billion format
    
    m <- rbind(m, cbind(s, as.numeric(i[grep("Price/Book", i) + 1]))) } 
    
  rownames(m) <- x # Tickers
  colnames(m) <- c("Marker Cap ($billions)", "P/B") # Column Name
  
  plot(m[,1], m[,2], xlab = "Marker Cap ($billions)", las = 1, ylab = "P/B",
       main = "Value Small Cap & Growth Big Cap Stocks",
       ylim = c(min(m[,2]), max(m[,2])), xlim = c(min(m[,1]), max(m[,1])))
  
  abline(h = 0) # Add regression & horizontal line
  
  text(m[,1], m[,2], labels = rownames(m), pos = 4) # labels
}
smb.plt(c("AIG", "UNM", "MET", "HIG", "OMF")) # Test
