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

source('ML modelling/perform_ml_modelling.R')
source('ML modelling/eval_ml_modelling.R')

# ------------------------------------------------------
# Data loading
# ------------------------------------------------------  

setwd("Get ML dataframe") # set wd where data is stored

# Select model target
predict_target <- 'post'

# Load relevant dataframe correspoding to target
if (predict_target == 'pre') {
  
  load('Pre_dataframe.RData') # load pre intervention dataframe
  ML_dataframe <- ML_dataframe %>% select(-Groep)
  
} else if (predict_target == 'post') {
  
  # load('diff_dataframe.RData')
  # ML_add <- ML_dataframe %>% select(code,contains('_diff'))
  load('Post_dataframe.RData') # load post intervention dataframe
  # ML_dataframe <- ML_dataframe %>% left_join(ML_add, by='code')
  
} else if (predict_target == 'delta') {
  
  load('diff_dataframe.RData')

}

# ------------------------------------------------------
# Data pre-processing 
# ------------------------------------------------------   

# Add some preprocessing
data          <- ML_dataframe  %>% 
                 select(-contains('cycle_1'),-contains('cycle_2'),-contains('cycle_3'),-contains('missing_days')) %>%
                 rename_at(.vars = vars(ends_with("_cycle_all")),
                           .funs = ~gsub("_cycle_all$", "", .)) %>% 
                 rename_at(.vars = vars(ends_with("_percentile")),
                           .funs = ~gsub("_percentile$", "p", .)) %>% 
                 rename_at(.vars = vars(any_of(c("Training_data","Daily questionare","Weekly questionare","Training log"))),
                           .funs = ~paste0('compl_',.)) %>%
                 rename(sex = gender) %>%
                 mutate(sex = ifelse(sex==0,'female','male'))
data  <- dummy_cols(data,select_columns='sex',remove_first_dummy = T,remove_selected_columns = T) # create dummy cols based on 'Groep' column

#data <- left_join(data, ML_dataframe %>% select(code, contains('_diff')), by=c('code'))

# Perform normalization to body weight for pre and post predictions
if (predict_target %in% c('pre','post')) {
  
data$TT_power <- data$TT_power/data$weight # normalize power to bodyweight

}

# Remove participants without time trial performance
data <- data %>% filter(!is.na(TT_power))
# data <- data %>% mutate_at(.vars = vars(contains('VO2'),contains('power')),
#                            .funs = list(~./weight))

# Set prediction formula
target  <- 'TT_power' # select target for modelling
formula <- as.formula(paste0(target,'~.')) # create formula for modelling

# Perform dummification for training intervention groups
if ('Groep' %in% colnames(data)){
  data <- data %>%  rename(training_group = Groep) 
  data  <- dummy_cols(data,select_columns='training_group',remove_first_dummy = T,remove_selected_columns = T) # create dummy cols based on 'Groep' column
}

# data  <- data %>% drop_na() # drop rows with missing values
# ------------------------------------------------------
# Data partitioning
# ------------------------------------------------------   

# Data partitioning: split data into train set and holdout test set
if (predict_target %in% c('pre','post')) {
  set.seed(123)
} else {
  set.seed(321)   # to obtain similar train-test distribution
}
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
data_train <- data_train%>%select(-code) #daily_sick_cycle_2

# data<- data%>%select(-mean_Fascicle_length,-mean_Pennation_Angle,-Mean_Lm1,-Volume,-max_ACSA)

# ------------------------------------------------------
# Modelling  
# ------------------------------------------------------ 

# perform modelling (for three types of algorithm either with/without hyperparameter tuning and with/without prior filtering based on multicollinearity).
model_glm_no_hpt_no_filt       <- perform_ml_modelling(data_train,target,'glm',filtering ='no', HPT='no',CV = 'no') 
model_glm_hpt_no_filt          <- perform_ml_modelling(data_train,target,'glm',filtering ='no', HPT='yes',CV = 'yes')
model_glm_hpt_filt             <- perform_ml_modelling(data_train,target,'glm',filtering ='yes',HPT='yes',CV = 'yes')

model_rf_no_hpt_no_filt        <- perform_ml_modelling(data_train,target,'rf',filtering ='no', HPT='no',CV = 'no')
model_rf_hpt_no_filt           <- perform_ml_modelling(data_train,target,'rf',filtering ='no', HPT='yes',CV = 'yes')
model_rf_hpt_filt              <- perform_ml_modelling(data_train,target,'rf',filtering ='yes',HPT='yes',CV = 'yes')

model_pcr_no_hpt_no_filt       <- perform_ml_modelling(data_train,target,'pcr',filtering ='no',HPT='no',CV = 'no')
model_pcr_hpt_no_filt          <- perform_ml_modelling(data_train,target,'pcr',filtering ='no',HPT='yes',CV = 'yes')
model_pcr_hpt_filt             <- perform_ml_modelling(data_train,target,'pcr',filtering ='yes',HPT='yes',CV = 'yes')

# Evaluate machine learning models based on MAE and show plots of measured vs predicted values
# Model performance on train set
results_glm <- rbind('model_glm_no_hpt_no_filt' = eval_ml_modelling(model_glm_no_hpt_no_filt,data_train,target,plot = FALSE),
                     'model_glm_hpt_no_filt'    = eval_ml_modelling(model_glm_hpt_no_filt,data_train,target,plot = FALSE),
                     'model_glm_hpt_filt'       = eval_ml_modelling(model_glm_hpt_filt,data_train,target,plot = FALSE))

results_rf <- rbind('model_rf_no_hpt_no_filt'   = eval_ml_modelling(model_rf_no_hpt_no_filt,data_train,target,plot = FALSE),
                     'model_rf_hpt_no_filt'     = eval_ml_modelling(model_rf_hpt_no_filt,data_train,target,plot = FALSE),
                     'model_rf_hpt_filt'        = eval_ml_modelling(model_rf_hpt_filt,data_train,target,plot = FALSE))

results_pcr <- rbind('model_pcr_no_hpt_no_filt' = eval_ml_modelling(model_pcr_no_hpt_no_filt,data_train,target,plot = FALSE),
                     'model_pcr_hpt_no_filt'    = eval_ml_modelling(model_pcr_hpt_no_filt,data_train,target,plot = FALSE),
                     'model_pcr_hpt_filt'       = eval_ml_modelling(model_pcr_hpt_filt,data_train,target,plot = FALSE))

# Model performance on test set
testresults_glm <- rbind('model_glm_no_hpt_no_filt'= eval_ml_modelling(model_glm_no_hpt_no_filt,data_test,target,plot = FALSE),
                     'model_glm_hpt_no_filt'       = eval_ml_modelling(model_glm_hpt_no_filt,data_test,target,plot = FALSE),
                     'model_glm_hpt_filt'          = eval_ml_modelling(model_glm_hpt_filt,data_test,target,plot = FALSE))

testresults_rf <- rbind('model_rf_no_hpt_no_filt'  = eval_ml_modelling(model_rf_no_hpt_no_filt,data_test,target,plot = FALSE),
                    'model_rf_hpt_no_filt'         = eval_ml_modelling(model_rf_hpt_no_filt,data_test,target,plot = FALSE),
                    'model_rf_hpt_filt'            = eval_ml_modelling(model_rf_hpt_filt,data_test,target,plot = FALSE))

testresults_pcr <- rbind('model_pcr_no_hpt_no_filt'= eval_ml_modelling(model_pcr_no_hpt_no_filt,data_test,target,plot = FALSE),
                     'model_pcr_hpt_no_filt'       = eval_ml_modelling(model_pcr_hpt_no_filt,data_test,target,plot = FALSE),
                     'model_pcr_hpt_filt'          = eval_ml_modelling(model_pcr_hpt_filt,data_test,target,plot = FALSE))


# Organise model performance results to plot as table
results_modelling <- cbind(rbind(results_glm,results_rf,results_pcr),
                           rbind(testresults_glm,testresults_rf,testresults_pcr))
results_modelling <- as.data.frame(results_modelling) #
colnames(results_modelling) <- c(paste0('train_',c("RMSE","R2","MAE")),paste0('test_',c("RMSE","R2","MAE")))

results_modelling <- results_modelling %>% mutate_all(~round(.,3)) %>% rownames_to_column() %>%
                     mutate(algorithm = gsub('_','',substr(rowname,7,10)),
                            tuning = ifelse(str_detect(rowname,'no_hpt'),'','x'),
                            filter = ifelse(str_detect(rowname,'no_filt'),'','x')) %>% 
                     mutate(algorithm = ifelse(str_detect(algorithm,'rf'),'rf',algorithm)) 

table_models <-tableGrob(results_modelling %>%
                         select(algorithm,tuning,filter,contains('train_'),contains('test_')) %>%
                         select(-contains('RMSE')))
grid.arrange(table_models)

best_model_train <- results_modelling %>% arrange(train_MAE) %>% first()

# --- Summarise model results -----
if (predict_target == 'pre') {
  
  results_pre<- rbind(results_modelling)
  results_pre  <- results_pre  %>% mutate(target = 'pre')
  # save(results_pre,file = 'results_pre.Rdata')
  
} else if (predict_target == 'post') {
  
  results_post<- rbind(results_train)
  results_post <- results_post %>% mutate(target = 'post')
  # save(results_post,file = 'results_post.Rdata')
  
} else if (predict_target == 'delta') {
  
  results_diff<- rbind(results_train)
  results_diff <- results_diff %>% mutate(target = 'diff')
  #save(results_diff,file = 'results_diff.Rdata')
  
}

# Analyse all results
results_all  <- rbind(results_pre, results_post, results_diff)
results_all  <- results_all %>% mutate_at(.vars = vars(any_of(c('filter','tuning'))),
                                          .funs = ~ifelse(.=='x',1,0)) %>%
                mutate(train_test_diff_R2  = train_R2 - test_R2,
                       train_test_diff_MAE = train_MAE - test_MAE)

# --- Plot feature importance -----

# imp <- varImp(eval(as.name(best_model_train$rowname)),scale=F, nonpara = T)
# plot(imp,top=30)

# Fit final model
myControl  <- trainControl(method = "none",verboseIter = F)
model_glm <- train(formula,
                   data,
                   preProcess = c('center','scale','medianImpute'),
                   method = "glmnet",
                   trControl = myControl,
                   tuneGrid = eval(as.name(best_model_train$rowname))$bestTune,
                   importance = 'permutation',
                   na.action = na.pass,
                   metric = 'RMSE')
imp <- varImp(model_glm,scale=F, nonpara = T)
plot(imp,top=30)





# results_test <- rbind( 'results' = eval_ml_modelling(model_rf_no_hpt_no_filt_no_cv,data_test,target,plot = TRUE))

# results_glm <- rbind('model_glm_no_hpt_no_filt_no_cv' = eval_ml_modelling(model_glm_no_hpt_no_filt_no_cv,data_train,target,plot = FALSE),
#                      'model_glm_no_hpt_filt_no_cv'    = eval_ml_modelling(model_glm_no_hpt_filt_no_cv,data_train,target,plot = FALSE),
#                      'model_glm_no_hpt_filt_cv'       = eval_ml_modelling(model_glm_no_hpt_filt_cv,data_train,target,plot = FALSE),
#                      'model_glm_hpt_no_filt_cv'       = eval_ml_modelling(model_glm_hpt_no_filt_cv,data_train,target,plot = FALSE),
#                      'model_glm_hpt_filt_cv'          = eval_ml_modelling(model_glm_hpt_filt_cv,data_train,target,plot = FALSE))
# 
# results_rf <- rbind('model_rf_no_hpt_no_filt_no_cv'  = eval_ml_modelling(model_rf_no_hpt_no_filt_no_cv,data_train,target,plot = FALSE),
#                     'model_rf_no_hpt_filt_no_cv'    = eval_ml_modelling(model_rf_no_hpt_filt_no_cv,data_train,target,plot = FALSE),
#                     'model_rf_no_hpt_filt_cv'       = eval_ml_modelling(model_rf_no_hpt_filt_cv,data_train,target,plot = FALSE),
#                     'model_rf_hpt_no_filt_cv'       = eval_ml_modelling(model_rf_hpt_no_filt_cv,data_train,target,plot = FALSE),
#                     'model_rf_hpt_filt_cv'          = eval_ml_modelling(model_rf_hpt_filt_cv,data_train,target,plot = FALSE))
# 
# results_pcr <- rbind('model_pcr_no_hpt_no_filt_no_cv' = eval_ml_modelling(model_pcr_no_hpt_no_filt_no_cv,data_train,target,plot = FALSE),
#                      'model_pcr_no_hpt_filt_no_cv'    = eval_ml_modelling(model_pcr_no_hpt_filt_no_cv,data_train,target,plot = FALSE),
#                      'model_pcr_no_hpt_filt_cv'       = eval_ml_modelling(model_pcr_no_hpt_filt_cv,data_train,target,plot = FALSE),
#                      'model_pcr_hpt_no_filt_cv'       = eval_ml_modelling(model_pcr_hpt_no_filt_cv,data_train,target,plot = FALSE),
#                      'model_pcr_hpt_filt_cv'          = eval_ml_modelling(model_pcr_hpt_filt_cv,data_train,target,plot = FALSE))
# 
# testresults_glm <- rbind('model_glm_no_hpt_no_filt_no_cv' = eval_ml_modelling(model_glm_no_hpt_no_filt_no_cv,data_test,target,plot = FALSE),
#                          'model_glm_no_hpt_filt_no_cv'    = eval_ml_modelling(model_glm_no_hpt_filt_no_cv,data_test,target,plot = FALSE),
#                          'model_glm_no_hpt_filt_cv'       = eval_ml_modelling(model_glm_no_hpt_filt_cv,data_test,target,plot = FALSE),
#                          'model_glm_hpt_no_filt_cv'       = eval_ml_modelling(model_glm_hpt_no_filt_cv,data_test,target,plot = FALSE),
#                          'model_glm_hpt_filt_cv'          = eval_ml_modelling(model_glm_hpt_filt_cv,data_test,target,plot = FALSE))
# 
# testresults_rf <- rbind('model_rf_no_hpt_no_filt_no_cv'  = eval_ml_modelling(model_rf_no_hpt_no_filt_no_cv,data_test,target,plot = FALSE),
#                         'model_rf_no_hpt_filt_no_cv'    = eval_ml_modelling(model_rf_no_hpt_filt_no_cv,data_test,target,plot = FALSE),
#                         'model_rf_no_hpt_filt_cv'       = eval_ml_modelling(model_rf_no_hpt_filt_cv,data_test,target,plot = FALSE),
#                         'model_rf_hpt_no_filt_cv'       = eval_ml_modelling(model_rf_hpt_no_filt_cv,data_test,target,plot = FALSE),
#                         'model_rf_hpt_filt_cv'          = eval_ml_modelling(model_rf_hpt_filt_cv,data_test,target,plot = FALSE))
# 
# testresults_pcr <- rbind('model_pcr_no_hpt_no_filt_no_cv' = eval_ml_modelling(model_pcr_no_hpt_no_filt_no_cv,data_test,target,plot = FALSE),
#                          'model_pcr_no_hpt_filt_no_cv'    = eval_ml_modelling(model_pcr_no_hpt_filt_no_cv,data_test,target,plot = FALSE),
#                          'model_pcr_no_hpt_filt_cv'       = eval_ml_modelling(model_pcr_no_hpt_filt_cv,data_test,target,plot = FALSE),
#                          'model_pcr_hpt_no_filt_cv'       = eval_ml_modelling(model_pcr_hpt_no_filt_cv,data_test,target,plot = FALSE),
#                          'model_pcr_hpt_filt_cv'          = eval_ml_modelling(model_pcr_hpt_filt_cv,data_test,target,plot = FALSE))
# 
# 
