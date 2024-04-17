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


# load and organise data and load functions -------------------------------------------------
setwd("L:/basic/Personal Archive/L/lvos2/VU/ML paper/Final folder/Get ML dataframe")

source('get_questionare_features.R')
source('get_sick_days.R')

restructured_data <- read_csv("data/Full_questionare_data.csv")
restructured_data <- restructured_data[,-c(1,5,7)]
restructured_data <- restructured_data%>%mutate(daily_resting_hr = ifelse(daily_resting_hr==0,NA,daily_resting_hr))

# get sleep score wellness score and day number relative to test date
restructured_data <- restructured_data%>%mutate(wellness = (daily_sleep_quality+daily_fatigue+daily_stress+daily_soreness+daily_mood)/25,
                                               days = abs(as.numeric(as.Date(file_start_date)-as.Date('2022-12-12'))),
                                               Sleepscore = daily_sleep_quality*daily_sleep_duration)%>%
  filter(file_start_date<=as.Date('2022-12-12')& file_start_date>=as.Date('2022-9-19'))

# get mesocycle number based on days
restructured_data <- restructured_data %>% mutate (cycle = case_when(days>56 & days<=84 ~ 3,
                                                                     days>28 & days<=56 ~ 2,
                                                                     days>0 & days<=28 ~ 1))

Questionare_data  <- restructured_data%>%filter(type=='weekly_questionnaire' | type == 'daily_questionnaire')
Training_log_data <-restructured_data%>%filter(type=='train_questionnaire')


# Get Compliance ----------------------------------------------------------

compliance_questionare_daily <- Questionare_data%>%filter(type == 'daily_questionnaire')%>%group_by(code)%>%summarize(start_date = '19-9-2022',
                                                                                                 end_date = '12-12-2022',
                                                                                                 days = n(),
                                                                                                 count = 85,
                                                                                                 compliance = n()/85,
                                                                                                 compliant = ifelse(compliance>=.8,1,0))
compliance_questionare_weekly <- Questionare_data%>%filter(type == 'weekly_questionnaire')%>%group_by(code)%>%summarize(start_date = '19-9-2022',
                                                                                                                   end_date = '12-12-2022',
                                                                                                                   days = n(),
                                                                                                                   count = 12,
                                                                                                                   compliance = n()/12,
                                                                                                                   compliant = ifelse(compliance>=.8,1,0))

# based on a cut-off of 80% there are 20 participants that met these requirements


compliance_training_log <- restructured_data%>%filter(type=='train_questionnaire')%>%group_by(code)%>%summarize(start_date = '19-9-2022',
                                                                                                              end_date = '12-12-2022',
                                                                                                              days = n(),
                                                                                                              count = 60,
                                                                                                              compliance = n()/60,
                                                                                                              compliant = ifelse(compliance>=.8,1,0))

# create daily dataframe and get features ---------------------------------

days = seq(0,84,1)
codes = unique(Questionare_data$code)
factors = 
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
windows   <- c('1','2','3','all')

daily_sleep_features                    <- get_questionare_features(Questionare_data,windows,functions,'daily_sleep_duration')
daily_wellness_features                 <- get_questionare_features(Questionare_data,windows,functions,'wellness')
daily_resting_hr_features               <- get_questionare_features(Questionare_data,windows,functions,'daily_resting_hr')
daily_sick_days                         <- get_sick_days(Questionare_data,windows,functions,'daily_sick')
daily_sick_days[is.na(daily_sick_days)] <-0

df_list <- list(daily_sleep_features,daily_wellness_features,daily_resting_hr_features,daily_sick_days)      

#merge all data frames together
Questionaire_features <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)  
Questionaire_features <- Questionaire_features%>%select(-daily_sick_cycle_1,-daily_sick_cycle_2,-daily_sick_cycle_3)

save(Questionaire_features,file='Questionaire_features.Rdata')

