rm(list=ls())
#author Luuk Vos 

# load packages -----------------------------------------------------------

library(readxl)
library(dplyr)
library(janitor)

setwd("~/Bewegingswetenschappen/Master/Thesis/Code/Antropometrics") #set this to where the antropometrics folder is located

files <- list.files(path='data/',pattern='*.xlsx*')
data_all<-c()
for (file in files){
  tmp <- suppressMessages(read_excel(paste0('data/',file),sheet = "1A. Antropometrie"))
  

  
  data<- data.frame(code= as.character(tmp[3,2]),
                    gender = as.factor(ifelse(tmp[4,2]==1,'male','female')),
                    age = as.numeric(tmp[35,7]),
                    weight = as.numeric(tmp[14,8]),
                    height = as.numeric(tmp[15,8]),
                    bicep_sf = as.numeric(tmp[17,8]),
                    tripeps_sf = as.numeric(tmp[18,8]),
                    subscapular_sf = as.numeric(tmp[19,8]),
                    illiaccrest_sf = as.numeric(tmp[20,8]),
                    sum_sf = as.numeric(tmp[32,8]),
                    fat_per = NA,
                    thigh_length = as.numeric(tmp[22,8]),
                    thigh_circ = as.numeric(tmp[23,8]),
                    shank_length = as.numeric(tmp[25,8]),
                    shank_circ = as.numeric(tmp[26,8]))
  
  tmp_siri_eq <- data.frame(tmp[35:41,2:3])
  tmp_siri_eq <- tmp_siri_eq%>%row_to_names(row_number = 1)
  tmp_siri_eq$low_age <- c(0, 16, 20, 30, 40, 50)
  tmp_siri_eq$high_age <- c(17, 19, 29, 39, 49, 100)
  
  idx_siri<- data$age>=tmp_siri_eq$low_age & data$age<=tmp_siri_eq$high_age
  
  data$fat_per <- ifelse(data$gender == 'male',as.numeric(tmp_siri_eq[idx_siri,'Man']),as.numeric(tmp_siri_eq[idx_siri,'Vrouw']))
  
  if (length(data_all)==0){
    data_all <- data
  } else{
    data_all <- rbind(data_all,data)
  }
}



