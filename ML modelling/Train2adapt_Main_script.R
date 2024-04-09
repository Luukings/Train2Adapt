rm(list = ls())

library(readxl)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(officer)
library(caret)
library(treeshap)
library(visdat)
library(xgboost)
library(SHAPforxgboost)
library(FactoMineR)
library(factoextra)
library(ranger)
library(glmnet)
library(MLmetrics)
library(DiagrammeR)
library(ggpattern)
library(kernlab)
library(purrr)
library(fastDummies)
library(missRanger)
library(pbapply)
library(MLeval)
library(missMethods)
library(tidyverse)
library(gridExtra)
library(RANN)

# ------------------------------------------------------
# Load helper scripts
# ------------------------------------------------------ 

source('L:/basic/Personal Archive/L/lvos2/VU/ML paper/Final folder/ML modelling/perform_ml_modelling.R')
source('L:/basic/Personal Archive/L/lvos2/VU/ML paper/Final folder/ML modelling/eval_ml_modelling.R')

# ------------------------------------------------------
# Data loading
# ------------------------------------------------------  

setwd("L:/basic/Personal Archive/L/lvos2/VU/ML paper/Final folder/Get ML dataframe") # set wd where data is stored

load('Pre_dataframe.RData') # load pre intervention dataframe
# load('Post_dataframe.RData') # load post intervention dataframe
# load('diff_dataframe.RData')

data          <- ML_dataframe
# data$TT_power <- data$TT_power/data$weight # normalize power to bodyweight

target  <- 'TT_power' # select target for modelling
formula <- as.formula(paste0(target,'~.')) # create formula for modelling

# ------------------------------------------------------
# Data pre-processing 
# ------------------------------------------------------   

data  <- dummy_cols(data,select_columns='Groep',remove_first_dummy = T,remove_selected_columns = T) # create dummy cols based on 'Groep' column
# data  <- data %>% drop_na() # drop rows with missing values
# ------------------------------------------------------
# Data partitioning
# ------------------------------------------------------   

# Data partitioning: split data into train and holdout test set
set.seed(123)
# set.seed(321)
trainIndex      <- createDataPartition(data$TT_power, times=1, p = .65, list=F)
data_train      <- data[trainIndex,]
data_test       <- data[-trainIndex,]

# add split variable for plotting

data$split              <- ''

data$split[trainIndex]  <- 'Train'
data$split[-trainIndex] <- 'Test'

# set plotting settings
order      <-c('Train','Test')
cbbPalette <- c( "#E69F00", "#56B4E9")

#plot TT_power for train and test set
data%>%ggplot(aes(split,TT_power,fill=split))+
  geom_boxplot(width = 0.1)+
  geom_jitter(width=0.1)+
  scale_fill_manual(values  =cbbPalette)+
  scale_x_discrete(limits = order)+
  theme_minimal()+
  xlab("")

# remove split and code features from data
data <- data%>%select(-split,-code)
data_train <- data_train%>%select(-code,-daily_sick_cycle_2)

# data<- data%>%select(-mean_Fascicle_length,-mean_Pennation_Angle,-Mean_Lm1,-Volume,-max_ACSA)

# ------------------------------------------------------
# Modelling  
# ------------------------------------------------------ 

# perform modelling.
model_glm_no_hpt_no_filt_no_cv   <- perform_ml_modelling(data_train,target,'glm',filtering ='no',HPT='no',CV = 'no') 
model_glm_no_hpt_filt_no_cv      <- perform_ml_modelling(data_train,target,'glm',filtering ='yes',HPT='no',CV = 'no')
model_glm_no_hpt_filt_cv         <- perform_ml_modelling(data_train,target,'glm',filtering ='yes',HPT='no',CV = 'yes')
model_glm_hpt_no_filt_cv         <- perform_ml_modelling(data_train,target,'glm',filtering ='no',HPT='yes',CV = 'yes')
model_glm_hpt_filt_cv            <- perform_ml_modelling(data_train,target,'glm',filtering ='yes',HPT='yes',CV = 'yes')

model_rf_no_hpt_no_filt_no_cv    <- perform_ml_modelling(data_train,target,'rf',filtering ='no',HPT='no',CV = 'no')
model_rf_no_hpt_filt_no_cv       <- perform_ml_modelling(data_train,target,'rf',filtering ='yes',HPT='no',CV = 'no')
model_rf_no_hpt_filt_cv          <- perform_ml_modelling(data_train,target,'rf',filtering ='yes',HPT='no',CV = 'yes')
model_rf_hpt_no_filt_cv          <- perform_ml_modelling(data_train,target,'rf',filtering ='no',HPT='yes',CV = 'yes')
model_rf_hpt_filt_cv             <- perform_ml_modelling(data_train,target,'rf',filtering ='yes',HPT='yes',CV = 'yes')

model_svm_no_hpt_no_filt_no_cv   <- perform_ml_modelling(data_train,target,'svm',filtering ='no',HPT='no',CV = 'no')
model_svm_no_hpt_filt_no_cv      <- perform_ml_modelling(data_train,target,'svm',filtering ='yes',HPT='no',CV = 'no')
model_svm_no_hpt_filt_cv         <- perform_ml_modelling(data_train,target,'svm',filtering ='yes',HPT='no',CV = 'yes')
model_svm_hpt_no_filt_cv         <- perform_ml_modelling(data_train,target,'svm',filtering ='no',HPT='yes',CV = 'yes')
model_svm_hpt_filt_cv            <- perform_ml_modelling(data_train,target,'svm',filtering ='yes',HPT='yes',CV = 'yes')

# Evaluate machine learning models based on RMSE and show plots of measured vs predicted values

results_glm <- rbind('model_glm_no_hpt_no_filt_no_cv' = eval_ml_modelling(model_glm_no_hpt_no_filt_no_cv,data_train,target,plot = TRUE),
                     'model_glm_no_hpt_filt_no_cv' = eval_ml_modelling(model_glm_no_hpt_filt_no_cv,data_train,target,plot = TRUE),
                     'model_glm_no_hpt_filt_cv' = eval_ml_modelling(model_glm_no_hpt_filt_cv,data_train,target,plot = TRUE),
                     'model_glm_hpt_no_filt_cv' = eval_ml_modelling(model_glm_hpt_no_filt_cv,data_train,target,plot = TRUE),
                     'model_glm_hpt_filt_cv' = eval_ml_modelling(model_glm_hpt_filt_cv,data_train,target,plot = TRUE))

results_rf <- rbind('model_rf_no_hpt_no_filt_no_cv' = eval_ml_modelling(model_rf_no_hpt_no_filt_no_cv,data_train,target,plot = TRUE),
                     'model_rf_no_hpt_filt_no_cv' = eval_ml_modelling(model_rf_no_hpt_filt_no_cv,data_train,target,plot = TRUE),
                     'model_rf_no_hpt_filt_cv' = eval_ml_modelling(model_rf_no_hpt_filt_cv,data_train,target,plot = TRUE),
                     'model_rf_hpt_no_filt_cv' = eval_ml_modelling(model_rf_hpt_no_filt_cv,data_train,target,plot = TRUE),
                     'model_rf_hpt_filt_cv' = eval_ml_modelling(model_rf_hpt_filt_cv,data_train,target,plot = TRUE))

results_svm <- rbind('model_svm_no_hpt_no_filt_no_cv' = eval_ml_modelling(model_svm_no_hpt_no_filt_no_cv,data_train,target,plot = TRUE),
                     'model_svm_no_hpt_filt_no_cv' = eval_ml_modelling(model_svm_no_hpt_filt_no_cv,data_train,target,plot = TRUE),
                     'model_svm_no_hpt_filt_cv' = eval_ml_modelling(model_svm_no_hpt_filt_cv,data_train,target,plot = TRUE),
                     'model_svm_hpt_no_filt_cv' = eval_ml_modelling(model_svm_hpt_no_filt_cv,data_train,target,plot = TRUE),
                     'model_svm_hpt_filt_cv' = eval_ml_modelling(model_svm_hpt_filt_cv,data_train,target,plot = TRUE))


# organise training results to plot as table
results_train <- rbind(results_glm,results_rf,results_svm)
results_train <- as.data.frame(results_train) %>% rownames_to_column()

table_train<-tableGrob(results_train)
grid.arrange(table_train)

best_model_train <- results_train%>%arrange(RMSE)%>%first()

# organise test results to plot as table
results_test <- rbind( 'results' = eval_ml_modelling(eval(as.name(best_model_train$rowname)),data_test,target,plot = TRUE))
results_test <- as.data.frame(results_test) %>% rownames_to_column()
results_test$rowname[1] <-  paste0('results_test_',best_model_train$rowname)
table_test<-tableGrob(results_test)
grid.arrange(table_test)

imp <- varImp(eval(as.name(best_model_train$rowname)),scale=F, nonpara = T)
# imp <- varImp(model_glm_no_hpt_filt,scale=FALSE)
plot(imp,top=10)

# results_pre<- rbind(results_train,results_test)
# save(results_pre,file = 'results_pre.Rdata')

# results_post<- rbind(results_train,results_test)
# save(results_post,file = 'results_post.Rdata')
# 
results_diff<- rbind(results_train,results_test)
save(results_diff,file = 'results_diff.Rdata')


results_test <- rbind( 'results' = eval_ml_modelling(model_rf_no_hpt_no_filt_no_cv,data_test,target,plot = TRUE))
