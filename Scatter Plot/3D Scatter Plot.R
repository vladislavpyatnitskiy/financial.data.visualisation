# Main script
fig <- plot_ly(Basic_Materials, x = ~Basic_Materials$`Standard Deviation`,
               y = ~Basic_Materials$Beta, z = ~Basic_Materials$Return,
               size=~Basic_Materials$MarketCap,color=~Basic_Materials$Industry,
               marker = list(symbol = 'circle', sizemode = 'diameter'),
               sizes = c(5, 150),text=~paste('Ticker:',Basic_Materials$Ticker))
# Rename axes
fig <- fig %>% layout(title='Basic Materials',
                      scene=list(xaxis=list(title='Standard Deviation'),
                                 yaxis=list(title='Beta'),
                                 zaxis=list(title = 'Return')))
fig # Display
