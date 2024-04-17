rm(list = ls())


# load packages -----------------------------------------------------------

library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(tidyr)
library(janitor)
library(tidyr)
library(DescTools)
library(data.table)
library(tidyverse)
library(openxlsx)
library(zoo)
library(officer)


# load data and functions -------------------------------------------------
setwd("L:/basic/Personal Archive/L/lvos2/VU/ML paper/Final folder/Get ML dataframe")# set wd for the dir where all functions and data is stored
load("data/HR_data_new_filtered.Rdata")

source('interpolate_offset.R')
source('Get_weekly_training_sessions.R')
source('Get_HR_zones.R')
source('get_features.R')
source('impute_NA_hr.R')



# Change SDV name to study name ---------------------------------------------

SDV_study_names  <- read_excel("data/SDV_T2A names.xlsx")
HR_data          <- left_join(HR_data,SDV_study_names, by='owner_id')


# filter data -------------------------------------------------------------

filtered_ID_HR <- read_excel("data/filtered_ID_HR_new.xlsx") 
HR_data        <- HR_data%>%filter(file_ID %in% filtered_ID_HR$ID)%>%group_by(file_ID)%>%
                  mutate(Training_time_min = as.numeric(end_date-start_date,unit='mins'))
HR_data        <- HR_data%>%filter(start_date<= as.Date('2022-12-12'))


# Interpolating data ------------------------------------------------------

HR_data  <- interpolate_offset(HR_data) # interpolate data takes about 2 min 
HR_data <- HR_data%>%mutate(hr = ifelse(hr==0, NA, hr))
HR_data  <- HR_data%>% # select relevant columns 
            select(file_ID,start_date, end_date, code,group,filename,offset,hr,speed,elevation,latlong,distance,power,Training_time_min)

load("data/T2A_42/total_HR_data_interp.Rdata") # load T2A_42 data separate
data_all$group <- 'POL'
HR_data        <-rbind(HR_data,data_all)

# HR per zone -------------------------------------------------------------



# First get the HR zone data from the pre tests
HR_zones <- read_excel("data/Thresholds_POST_complete.xlsx", sheet='PRE')
HR_zones <- HR_zones%>% filter(PPcode %in% SDV_study_names$code)%>%select(PPcode,`VT1 HR (slagen/min)`,`VT2 HR (slagen/min)`,`HRmax (slagen/min)`) # only select Code and HR zones

HR_data   <- Get_HR_zones(HR_data,HR_zones) # set the correct zone for each heart rate
ids_no_hr <- read_excel("data/Id_No_HR.xlsx")
HR_data   <- impute_NA_hr(HR_data,ids_no_hr$ID) # impute values for missing HR with training logs
HR_data   <- Get_HR_zones(HR_data,HR_zones) # do this again for imputed HR data
HR_data   <- HR_data%>%group_by(file_ID)%>%mutate(TRIMP = (sum(zone==1,na.rm=T)+sum(zone==2,na.rm=T)*2+sum(zone==3,na.rm=T)*3)/60)



zone_data <- HR_data%>% group_by(code)%>% summarize(code = head(code,1),
                                               GROUP = head(group,1),
                                               Z1 = sum(zone==1,na.rm=T),
                                               Z2 = sum(zone==2,na.rm=T),
                                               Z3 = sum(zone==3,na.rm=T),
                                               tot_time = length(zone),
                                               compliance = ifelse(head(group,1) == 'CON', tot_time/(60*5250),
                                                                   ifelse(head(group,1) == 'POL',tot_time/(60*3750),
                                                                          ifelse(head(group,1) == 'CT1', tot_time/(60*2250),
                                                                                 ifelse(head(group,1) == 'CT2' , tot_time/(60*2250),NA)))),
                                               Z1_per = (sum(zone==1,na.rm=T)/length(zone)) * 100,
                                               Z2_per = (sum(zone==2,na.rm=T)/length(zone)) * 100,
                                               Z3_per = (sum(zone==3,na.rm=T)/length(zone)) * 100,
                                               tot_per = Z1_per+Z2_per+Z3_per
                                               )




# count training sessions per week ----------------------------------------

weekly_training <- Get_weekly_training_sessions(HR_data)



# get compliance ----------------------------------------------------------
compliance <- weekly_training[seq(2,nrow(weekly_training),2),]
compliance <- compliance%>%rename(code=id)
compliance <- left_join(SDV_study_names,compliance,by = 'code')
compliance <- compliance%>%group_by(code)%>%mutate(compl =ifelse(head(group,1) == 'CON', sum/(5250),
                                                 ifelse(head(group,1) == 'POL',sum/(3750),
                                                  ifelse(head(group,1) == 'CT1', sum/(2250),
                                                   ifelse(head(group,1) == 'CT2' , sum/(2250),NA)))) )%>%arrange(code)


# Set daily variables and get features ------------------------------------
HR_data  <- HR_data%>%mutate(days = abs(as.numeric(as.Date(start_date)-as.Date('2022-12-12'))))
days     <- seq(0,84,1)
codes    <- unique(HR_data$code)

daily_df <- c()
for (code in codes){
  tmp <- data.frame(code=rep(code,length(days)),days,hr=NA)
  if (length(df)==0){
    daily_df <- tmp
  } else{
    daily_df<-rbind(daily_df,tmp)
  }
}

functions <- c('mean','sd','IQR','quantile')
windows <- c('1','2','3','all')
# get mesocycle number based on days
HR_data <- HR_data %>% mutate (cycle = case_when(days>56 & days<=84 ~ 3,
                                                                     days>28 & days<=56 ~ 2,
                                                                     days>0 & days<=28 ~ 1))

test  <- left_join(daily_df,HR_data,by=c('days','code'))
test  <- rename(test,hr=hr.y)%>%select(-hr.x)

hr_features    <- get_features(test,windows,functions,'hr')
TRIMP_features <- get_features(test,windows,functions,'TRIMP')

hr_features2    <- hr_features%>%select(-contains('NA'))
TRIMP_features2 <- TRIMP_features%>%select(-contains('NA'))

Features_HR <- full_join(hr_features2,TRIMP_features2,by = 'code')

# Features_HR <- Features_HR %>% select(-c(hr_quantile_cycle_1_percentage_missing_days,hr_quantile_cycle_2_percentage_missing_days,hr_quantile_cycle_3_percentage_missing_days,hr_quantile_cycle_all_percentage_missing_days))

save(Features_HR,file= 'HR_features.Rdata')
