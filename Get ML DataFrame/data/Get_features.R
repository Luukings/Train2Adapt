rm(list = ls())

library(readxl)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(lubridate)
library(tidyr)
library(effsize)
library(rvg)
library(officer)
library(here)
library(caret)
library(janitor)
library(purrr)
library(visdat)
library(xgboost)
library(SHAPforxgboost)
library(FactoMineR)
library(factoextra)
library(ranger)
library(glmnet)
library(MLmetrics)
library(DiagrammeR)

# To DO ----------------------------------------------------------

# add NIRS data

# Antropometrics ----------------------------------------------------------

setwd("L:/basic/Personal Archive/L/lvos2/VU/ML paper/Final folder/Get ML dataframe/data/Antropometrics")

files <- list.files(path='data/PRE/',pattern='*.xlsx*')
data_all<-c()
for (file in files){
  tmp <- suppressMessages(read_excel(paste0('data/PRE/',file),sheet = "1A. Antropometrie"))
  
  
  
  data<- data.frame(code= as.character(tmp[3,2]),
                    gender = as.factor(ifelse(tmp[4,2]==1,1,0)),
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
  
  data$fat_per <- ifelse(data$gender == 1,as.numeric(tmp_siri_eq[idx_siri,'Man']),as.numeric(tmp_siri_eq[idx_siri,'Vrouw']))
  data <- data%>%mutate(lbm = weight-((fat_per/100)*weight))
  
  if (length(data_all)==0){
    data_all <- data
  } else{
    data_all <- rbind(data_all,data)
  }
}

Antropometrics_pre <- data_all
# Antropometrics <- data_all%>%select(-contains('sf'))

rm(data,data_all,tmp,tmp_siri_eq,idx_siri,files,file)


files <- list.files(path='data/POST/',pattern='*.xlsx*')
data_all<-c()
for (file in files){
  tmp <- suppressMessages(read_excel(paste0('data/POST/',file),sheet = "1A. Antropometrie"))
  
  
  
  data<- data.frame(code= as.character(tmp[3,2]),
                    gender = as.factor(ifelse(tmp[4,2]==1,1,0)),
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
  
  data$fat_per <- ifelse(data$gender == 1,as.numeric(tmp_siri_eq[idx_siri,'Man']),as.numeric(tmp_siri_eq[idx_siri,'Vrouw']))
  data <- data%>%mutate(lbm = weight-((fat_per/100)*weight))
  
  if (length(data_all)==0){
    data_all <- data
  } else{
    data_all <- rbind(data_all,data)
  }
}

Antropometrics_post <- data_all
# Antropometrics <- data_all%>%select(-contains('sf'))

rm(data,data_all,tmp,tmp_siri_eq,idx_siri,files,file)



# Wingate -----------------------------------------------------------------

setwd("L:/basic/Personal Archive/L/lvos2/VU/ML paper/Final folder/Get ML dataframe/data/Wingate")

files <- list.files(path='data/PRE/',pattern='*.xlsx*')
data_all<-c()
for (file in files){
  tmp <- suppressMessages(read_excel(paste0('data/PRE/',file)))
  tmp$Person <- gsub(" ", "",tmp$Person)
  weight <- Antropometrics_pre$weight[Antropometrics_pre$code == tmp$Person[1]]
  data<- data.frame(code = as.character(tmp[1,1]),
                    Wingate_peak_power = max(tmp$`PP [W]`),
                    Wingate_Average_power = tmp$`AP [W]`[1],
                    Wingate_Min_power = tmp$`MP [W]`[1],
                    Wingate_power_drop = tmp$`PD [W]`[1],
                    Wingate_vmax = tmp$`Vmax [rpm]`[1],
                    Wingate_power_vmax = tmp$`P Vmax [W]`[1]
  )
  
  
  # data<- data%>%mutate(Wingate_peak_power_norm = peak_power/weight,
  #                      Wingate_Average_power_norm = Average_power/weight,
  #                      Wingate_Min_power_norm = Min_power/weight,
  #                      Wingate_power_drop_norm = power_drop/weight,
  #                      Wingate_power_vmax_norm = power_vmax/weight)
  if (length(data_all)==0){
    data_all <- data
  } else{
    data_all <- rbind(data_all,data)
  }
}

# Wingate <- data_all # select all features
Wingate_pre <- data_all%>%select(-contains('norm'))

rm(data, data_all,tmp,weight,files,file)

files <- list.files(path='data/POST/',pattern='*.xlsx*')
data_all<-c()
for (file in files){
  tmp <- suppressMessages(read_excel(paste0('data/POST/',file)))
  tmp$Person <- gsub(" ", "",tmp$Person)
  weight <- Antropometrics_post$weight[Antropometrics_post$code == tmp$Person[1]]
  data<- data.frame(code = as.character(tmp[1,1]),
                    Wingate_peak_power = max(tmp$`PP [W]`),
                    Wingate_Average_power = tmp$`AP [W]`[1],
                    Wingate_Min_power = tmp$`MP [W]`[1],
                    Wingate_power_drop = tmp$`PD [W]`[1],
                    Wingate_vmax = tmp$`Vmax [rpm]`[1],
                    Wingate_power_vmax = tmp$`P Vmax [W]`[1]
                    )

  
  # data<- data%>%mutate(Wingate_peak_power_norm = peak_power/weight,
  #                      Wingate_Average_power_norm = Average_power/weight,
  #                      Wingate_Min_power_norm = Min_power/weight,
  #                      Wingate_power_drop_norm = power_drop/weight,
  #                      Wingate_power_vmax_norm = power_vmax/weight)
  if (length(data_all)==0){
    data_all <- data
  } else{
    data_all <- rbind(data_all,data)
  }
}

# Wingate <- data_all # select all features
Wingate_post <- data_all%>%select(-contains('norm'))

rm(data, data_all,tmp,weight,files,file)


# VO2 ---------------------------------------------------------------------

setwd("L:/basic/Personal Archive/L/lvos2/VU/ML paper/Final folder/Get ML dataframe/data/VO2")

data <- read_excel("excel_data/Thresholds_POST_complete.xlsx", sheet = 'PRE')
names(data) <- c('code', 'Maxtest_VT1_VO2','Maxtest_VT1_Power','Maxtest_VT1_HR','Maxtest_VT2_VO2','Maxtest_VT2_Power','Maxtest_VT2_HR','Maxtest_PO_VO2max','Maxtest_VO2_max','Maxtest_HR_max','weight')
# data<- data%>%mutate(VT1_VO2_norm = VT1_VO2/weight,
#                      VT1_Power_norm = VT1_Power/weight,
#                      VT2_VO2_norm = VT2_VO2/weight,
#                      VT2_Power_norm = VT2_Power/weight,
#                      VO2_max_norm = VO2_max/weight
# )
# VO2_max = data%>%select(-weight) # select all features
VO2_max_pre = data%>%select(-weight,-contains('norm')) # remove normalized features to reduce over fitting
# VO2_max <- data[,-12]%>% select(-weight)
rm(data)

data <- read_excel("excel_data/Thresholds_POST_complete.xlsx", sheet = 'POST')
names(data) <- c('code', 'Maxtest_VT1_VO2','Maxtest_VT1_Power','Maxtest_VT1_HR','Maxtest_VT2_VO2','Maxtest_VT2_Power','Maxtest_VT2_HR','Maxtest_PO_VO2max','Maxtest_VO2_max','Maxtest_HR_max','weight')
# data<- data%>%mutate(VT1_VO2_norm = VT1_VO2/weight,
#                      VT1_Power_norm = VT1_Power/weight,
#                      VT2_VO2_norm = VT2_VO2/weight,
#                      VT2_Power_norm = VT2_Power/weight,
#                      VO2_max_norm = VO2_max/weight
# )
# VO2_max = data%>%select(-weight) # select all features
# VO2_max = data%>%select(-weight,-contains('norm')) # remove normalized features to reduce over fitting
VO2_max_post <- data[,-12]%>% select(-weight)
rm(data)


# Jump test ---------------------------------------------------------------

setwd("L:/basic/Personal Archive/L/lvos2/VU/ML paper/Final folder/Get ML dataframe/data/Jump")

files <- list.files('data/PRE', pattern='.xls', recursive=F)
data_all <- c()


for (file in files){
  tmp<- suppressMessages(read_excel(paste0('data/PRE/',file),col_names = F))
  data <- data.frame(code = substr(file,1,6),
                     Jump1 = as.numeric(tmp[2,1]),
                     Jump2 = as.numeric(tmp[3,1]),
                     Jump3 = as.numeric(tmp[4,1]),
                     Jump4 = as.numeric(tmp[5,1]),
                     Jump5 = as.numeric(tmp[6,1]))

 leg_length <- data.frame(thigh_length=Antropometrics_pre$thigh_length[Antropometrics_pre$code == data$code],
                          shank_length=Antropometrics_pre$shank_length[Antropometrics_pre$code == data$code])

  data<-data%>%rowwise()%>% summarise(code=code[1],
                                      Jump_max_height = max(c(Jump1,Jump2,Jump3,Jump4,Jump5),na.rm=T),
                                      Jump_mean_height = mean(c(Jump1,Jump2,Jump3,Jump4,Jump5),na.rm=T),
                                      Jump_sd_jump_heigt = sd(c(Jump1,Jump2,Jump3,Jump4,Jump5),na.rm=T),
                                      Jump_max_height_norm = Jump_max_height/(leg_length$thigh_length+leg_length$shank_length))
  if (length(nrow(data_all))==0) {
    data_all    <- data
  } else {
    data_all    <- suppressMessages(merge(data_all, data,all=T))
  }

}

Jump_test_pre <- data_all%>%select(code,Jump_max_height,Jump_max_height_norm) #only select max jump height

rm(data_all,data,tmp,leg_length,file,files)

files <- list.files('data/POST', pattern='.xls', recursive=F)
data_all <- c()

tmp<- suppressMessages(read_excel(paste0('data/POST/',files),col_names = T))

leg_length <- data.frame(code=Antropometrics_post$code,
                         thigh_length=Antropometrics_post$thigh_length,
                         shank_length=Antropometrics_post$shank_length)
data<-merge(tmp,leg_length,by='code')


data<-data%>%rowwise()%>%mutate(code=code,
                                max_height = max(c(Jump1,Jump2,Jump3,Jump4,Jump5,Jump6),na.rm=T),
                                mean_height = mean(c(Jump1,Jump2,Jump3,Jump4,Jump5,Jump6),na.rm=T),
                                sd_jump_heigt = sd(c(Jump1,Jump2,Jump3,Jump4,Jump5,Jump6),na.rm=T),
                                max_height_norm = max_height/(thigh_length+shank_length))%>%select(-Jump1:-Jump6)



Jump_test_post <- data%>%select(code,max_height,max_height_norm) #only select max jump height

rm(data_all,data,tmp,leg_length,files)
# VO2 kinetics ------------------------------------------------------------

setwd("L:/basic/Personal Archive/L/lvos2/VU/ML paper/Final folder/Get ML dataframe/data/VO2_kinetics")

data <- read_excel("test_data_R.xlsx")
data<- data%>%filter(time == 'Pre')%>%select(Code,base:MRT)
data<-data%>%rename(code=Code)


VO2_kinetics_pre <- data

rm(data)

data <- read_excel("test_data_R.xlsx")
data<- data%>%filter(time == 'Post')%>%select(Code,base:MRT)
data<-data%>%rename(code=Code)


VO2_kinetics_post <- data

rm(data)


# 3DUS --------------------------------------------------------------------

setwd("L:/basic/Personal Archive/L/lvos2/VU/ML paper/Final folder/Get ML dataframe/data/3DUS")

load("Data_Overall.xlsx")
data<- Data_Overall
data<- data%>%rename(code=PP_Code)%>%select(code:Mean_LM1)%>%filter(Time_Point == 'PRE')%>%
  arrange(code)%>%select(-Time_Point,-Group,-Scan_Number)%>%
  group_by(code)%>%summarise(mean_Fascicle_length = mean(Fascicle_Length),
                             mean_Pennation_Angle = mean(Pennation_Angle),
                             max_ACSA = Max_ACSA[1],
                             Volume = Volume[1],
                             Mean_Lm1 = Mean_LM1[1])

UltraSound_pre <- data
rm(data,Data_Overall)


load("Data_Overall.xlsx")
data<- Data_Overall
data<- data%>%rename(code=PP_Code)%>%select(code:Mean_LM1)%>%filter(Time_Point == 'POST')%>%
  arrange(code)%>%select(-Time_Point,-Group,-Scan_Number)%>%
  group_by(code)%>%summarise(mean_Fascicle_length = mean(Fascicle_Length),
                             mean_Pennation_Angle = mean(Pennation_Angle),
                             max_ACSA = Max_ACSA[1],
                             Volume = Volume[1],
                             Mean_Lm1 = Mean_LM1[1])

UltraSound_post <- data
rm(data,Data_Overall)

# Time trial --------------------------------------------------------------


setwd("L:/basic/Personal Archive/L/lvos2/VU/ML paper/Final folder/Get ML dataframe/data/Time Trial")
Data <- read_excel("test_data_R.xlsx")
Data <- Data%>%
  drop_na(Groep)%>%
  mutate(TT = as.POSIXct(TT)) %>%
  mutate(TT = minute(TT)*60 + second(TT)) %>%
  mutate(TT = (4000/TT)*3.6)
Data$Groep <- as.factor(Data$Groep)

Data_TT_pre <- Data%>%summarise(code = Data$Code[1:27],
                         Groep = Data$Groep[1:27],
                         TT_power = Data$Mean_TT_power[1:27])

Data_TT_post <- Data%>%summarise(code = Data$Code[28:54],
                            Groep = Data$Groep[28:54],
                            TT_power = Data$Mean_TT_power[28:54]) #for post data



# training HR -------------------------------------------------------------
setwd("L:/basic/Personal Archive/L/lvos2/VU/ML paper/Final folder/Get ML dataframe")

load('HR_features.Rdata') 

# questionnaire -------------------------------------------------------------

setwd("L:/basic/Personal Archive/L/lvos2/VU/ML paper/Final folder/Get ML dataframe")
load('Questionaire_features.Rdata')
# Questionare_features<- Questionare_features%>%select(-contains('missing'))

# compliance --------------------------------------------------------------
setwd("L:/basic/Personal Archive/L/lvos2/VU/ML paper/Final folder/Get ML dataframe")

# get compliance as numeric value
compliance<-read_excel("compliance_complete.xlsx", sheet = 'Tot_per')
compliance<-compliance[,1:6]%>%select(-GROUP)


# merge all dataframes ----------------------------------------------------

# df_list <- list(Data_TT,Antropometrics,Wingate,VO2_max,Jump_test,UltraSound,VO2_kinetics,Features_HR,Questionaire_features,compliance)
df_list_pre <- list(Data_TT_pre,Antropometrics_pre,Wingate_pre,VO2_max_pre,Jump_test_pre,UltraSound_pre,VO2_kinetics_pre) # only pre data
df_list_post <- list(Data_TT_post,Antropometrics_post,Wingate_post,VO2_max_post,Jump_test_post,UltraSound_post,VO2_kinetics_post) # only post data

ML_dataframe_pre <- df_list_pre %>% reduce(full_join, by='code')
ML_dataframe_post <- df_list_post %>% reduce(full_join, by='code')

ML_dataframe_pre <- ML_dataframe_pre%>% filter(!is.na(TT_power))
ML_dataframe_post <- ML_dataframe_post%>% filter(!is.na(TT_power))
# save(ML_dataframe,file="Post_dataframe.RData")

ML_diff <- ((ML_dataframe_post[,3:45]%>%select(-age,-gender)-ML_dataframe_pre[,3:45]%>%select(-age,-gender))/ML_dataframe_pre[,3:45]%>%select(-age,-gender))*100
colnames(ML_diff) <- paste(colnames(ML_diff),'diff',sep='_')
ML_diff <- data.frame(ML_dataframe_pre%>%select(code),ML_diff)

df_list_diff <- list(ML_dataframe_pre%>%select(-TT_power),ML_diff,Features_HR,Questionaire_features,compliance)
ML_dataframe_diff <- df_list_diff %>% reduce(full_join, by='code')
ML_dataframe_diff <- ML_dataframe_diff %>% rename_at('TT_power_diff', ~'TT_power')
ML_dataframe <- ML_dataframe_diff
# save(ML_dataframe,file = 'diff_dataframe.RData')

