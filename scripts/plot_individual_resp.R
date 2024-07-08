plot_individual_resp <- function(data) {

  # rm(list = ls())
  # library(readxl)
  # library(ggplot2)
  # library(dplyr)
  # load("diff_TT_performance.RData")
  
cbbPalette <- c( "#ffc034", "#56B4E9", "#009E73", "#e9566b")

data %>%
  ggplot(aes(x=reorder(code,TT_power),y=TT_power,fill=Groep)) +
  #geom_rect(aes(xmin = -0.5, xmax = 27.5, ymin = -.14/4.1*100, ymax = .14/4.1*100, fill = "#2C77BF")) +
  geom_bar(position = 'dodge', stat='identity',show.legend = T,alpha=0.6,color='black')+
  geom_hline(aes(yintercept = .14/4.1*100)) +
  geom_hline(aes(yintercept = -.14/4.1*100)) +
  geom_hline(aes(yintercept = 2), linetype = 'dashed') +
  ylab('Change in TT performance (%)')+
  labs(fill = "Group")+
  scale_fill_manual(values  =cbbPalette,
                    labels=c('LIE','POL',bquote("CT"["CON"]),bquote("CT"["ECC"])))+
  theme_minimal()+
  theme(axis.title.y = element_text( size = 14 ),
        axis.title.x = element_blank(),
        axis.text.y = element_text( size = 14 ),
        axis.text.x = element_blank(), 
        strip.text = element_text(size = 14))+
  scale_y_continuous(limit = c(-10,20))

}
