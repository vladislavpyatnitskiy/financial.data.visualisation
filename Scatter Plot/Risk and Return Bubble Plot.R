lapply(c("ggplot2","broom","tidyr","ggrepel"),require,character.only=T) # Libs

ggplot(data=Health_Care,mapping=aes(x=Risk,y=Return,size=MarketCap,
                                    color = Industry,label = Industry)) +
  geom_point() +
  labs(title="Risk and Return",x="Risk (Standard Deviation)",
       y="Expected Return",size="Market Capitalisation",color="Industry") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  geom_text_repel(aes(label=Ticker,fill=Industry,size=NULL,color=NULL),
                  nudge_y=.0125) + 
  guides(fill=guide_legend(title="Industry",override.aes=aes(label = ""))) +
  theme(axis.title.y=element_text(angle=0, vjust = .5)) +
  theme(plot.title = element_text(hjust = .5))
