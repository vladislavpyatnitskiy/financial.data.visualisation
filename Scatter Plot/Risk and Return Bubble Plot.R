lapply(c("quantmod", "timeSeries", "ggplot2", "ggrepel", "rvest"),
       require, character.only = T) # Libraries

bubble.plt <- function(x, s = NULL, e = as.Date(Sys.Date())){
  
  p <- NULL # Create empty variable to store values
  
  for (A in x) # For each ticker get data
    p<-cbind(p,getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F)[,4])
  
  p <- p[apply(p,1,function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- x # Give column names 
  
  p <- diff(log(as.timeSeries(p))) # Calculate logs
  
  p[1,] <- 0 # Assign first value as 0; calculate mean & standard deviation 
  
  r<-as.data.frame(t(apply(p,2,function(x) c(sd(x)*1000,(exp(sum(x))-1)*100))))
  
  new.info1 <- NULL # Create list for market cap and industry info 
  
  for (n in 1:nrow(r)){ c <- rownames(r)[n] # names of securitities
    
    p <- sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s",c,c)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab11
    
    m <- tab11 %>% html_nodes('p') %>% html_nodes('span') %>% html_text()
    
    j <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",c,c)
    
    s.page <- read_html(j) # Read HTML of page
    
    s.yahoo <- s.page %>% html_nodes('table') %>% .[[1]] -> tab1 # Assign Table 
    
    s <- tab1 %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    s <- s[2] # Info about market capitalisation
    
    s<-read.fwf(textConnection(s),widths=c(nchar(s)-1,1), # Reduce sign
                colClasses="character") 
    
    if (s[1,2] == "M"){ s <- as.numeric(s[1,1])/1000 } # million->billion
    
    else if (s[1,2] == "T"){ s <- as.numeric(s[1,1])*1000 } # trillion->billion
    
    else s <- as.numeric(s[1,1]) # Format to billion format
    
    new.info <- data.frame(s, m[2]) # Join sector and market cap infos
    
    rownames(new.info) <- c # assign security names
    
    new.info1 <- rbind.data.frame(new.info1, new.info) } # Join info for assets
  
  d <- data.frame(r, new.info1) # Final data frame
  
  # Plot
  ggplot(data = d, mapping = aes(x = d[,1], y = d[,2], size = d[,3],
                            color = d[,4], label = d[,4])) + geom_point() +
    labs(title="Securities", x="Risk (Standard Deviation)",y = "Return (%)",
         size = "Market Capitalisation", color = "Sector") + theme_minimal() +
    geom_text_repel(aes(label = rownames(d), fill = d[,4],
                        size = NULL, color = NULL), nudge_y = .0125) + 
    guides(fill=guide_legend(title = "Sector", override.aes=aes(label = ""))) +
    theme(plot.title = element_text(hjust = .5))
}
bubble.plt(x = c("X", "M", "C", "VIR", "XOM"), "2023-10-01") # Test
