rm(list = ls())
library(readxl)
library(ggplot2)
library(dplyr)

load("D:/Downloads/diff_dataframe(1).RData")


cbbPalette <- c( "#E69F00", "#56B4E9", "#009E73", "#CC79A7")

ML_dataframe%>%
  ggplot(aes(x=reorder(code,-TT_power),y=TT_power,fill=Groep))+
  geom_bar(position = 'dodge', stat='identity',show.legend = T,alpha=0.6,color='black')+
  ylab('Change in TT performance (%)')+
  labs(fill = "Group")+
  scale_fill_manual(values  =cbbPalette,
                    labels=c('CON','POL',bquote("CT"["CON"]),bquote("CT"["ECC"])))+
  theme_minimal()+
  theme(axis.title.y = element_text( size = 14 ),
        axis.title.x = element_blank(),
        axis.text.y = element_text( size = 14 ),
        axis.text.x = element_blank(), 
        strip.text = element_text(size = 14))+
  scale_y_continuous(limit = c(-10,20))
