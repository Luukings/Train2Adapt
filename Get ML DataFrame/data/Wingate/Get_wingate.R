rm(list=ls())
#author Luuk Vos 

# load packages -----------------------------------------------------------

library(readxl)
library(dplyr)

setwd("~/Bewegingswetenschappen/Master/Thesis/Code/Wingate")

files <- list.files(path='data/',pattern='*.xlsx*')
data_all<-c()
for (file in files){
  tmp <- suppressMessages(read_excel(paste0('data/',file)))
  
  data<- data.frame(code = tmp[1,1],
                    peak_power = max(tmp$`PP [W]`),
                    Average_power = tmp$`AP [W]`[1],
                    Min_power = tmp$`MP [W]`[1],
                    power_drop = tmp$`PD [W]`[1],
                    vmax = tmp$`Vmax [rpm]`,
                    power_vmax = tmp$`P Vmax [W]`,)
}
  
  




