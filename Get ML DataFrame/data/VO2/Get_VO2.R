rm(list=ls())


# load packages -----------------------------------------------------------

library(readxl)
library(dplyr)

setwd("~/Bewegingswetenschappen/Master/Thesis/Code/VO2")

data <- read_excel("excel_data/Thresholds_POST_complete.xlsx", sheet = 'PRE')
names(data) <- c('code', 'VT1_VO2','VT1_Power','VT1_HR','VT2_VO2','VT2_Power','VT2_HR','Peak_power','VO2_max','HR_max','weight')

data<- data%>%mutate(VT1_VO2_norm = VT1_VO2/weight,
                     VT1_Power_norm = VT1_Power/weight,
                     VT2_VO2_norm = VT2_VO2/weight,
                     VT2_Power_norm = VT2_Power/weight,
                     VO2_max_norm = VO2_max/weight
                     )


