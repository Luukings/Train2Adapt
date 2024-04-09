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
library(DMwR)
library(glmnet)
library(MLmetrics)
library(DiagrammeR)
library(ggpattern)
library(kernlab)
library(purrr)
library(fastDummies)
library(missRanger)
library(pbapply)


# load data ---------------------------------------------------------------
setwd("~/VU/ML paper/Final folder/Get ML dataframe") # set wd where data is stored

load('Pre_dataframe.RData') # load pre intervention dataframe

ML_dataframe <- ML_dataframe %>% select(code:MRT) # exclude training data

selected <- ML_dataframe
selected$TT_power <- selected$TT_power/selected$weight

target <- 'TT_power'
formula <- as.formula(paste0(target,'~.'))

set.seed(1234)
k=10
# fold_idx<-createFolds(selected$Power,k=k,list=F) # create 10 fold for nested cv
fold_idx<-createFolds(selected$TT_power,k=k,list=F) # create 10 fold for nested cv

selected<- selected%>%mutate(fold_id = fold_idx)

selected <- dummy_cols(selected,select_columns='Groep',remove_first_dummy = T,remove_selected_columns = T)

myControl  <- trainControl(method = "LOOCV",verboseIter = F)
myGrid <- expand.grid(alpha= seq(0,1,length=20),
                      lambda = seq(0.001,10,length=100))
impute_x <- function(x, ...) {
  missRanger(as.data.frame(x), num.trees = 50, ...)
}

pp_data <- preProcess(selected,method = c('center','scale','medianImpute'))

model_glmnet_filtered <- nestcv.train(selected$TT_power,
                               # Train2_df, # for all data
                               selected%>%select(-TT_power,-code,-fold_id), # for filtered data
                               tuneGrid = myGrid,
                               trControl = myControl,
                               preProcess = 'medianImpute',
                               # modifyX = impute_x,
                               # preProcess = c('center','scale'),
                               method = "glmnet",
                               importance = 'permutation',
                               na.action = na.pass,
                               metric = 'RMSE',
)

#initialize empty variables
list_baseline <- list()
list_glm <- list()
list_rf <- list()
list_svm <- list()
list_rfe <-list()

tot_res <- c()
importance<-c()
outer_loop = F # for running importance outside of general loop
# set nested cv loop
seq = c(1:k)
for (i in  seq){
  Train_df <- selected%>%filter(fold_id!=i)%>%select(-fold_id)
  Test_df <- selected%>%filter(fold_id==i)%>%select(-fold_id)
  
  # imputing missing variables ----------------------------------------------
  # 
  # preProcValues <- preProcess(as.data.frame(Train_df),method = c("medianImpute"))
  # Train_df <- predict(preProcValues, Train_df)
  # 
  # Test_df<- predict(preProcValues, Test_df)
  # 
  # # data preprocessing ------------------------------------------------------
  # 
  # #create dummy variables train set
  # dummies <- dummyVars(formula, data = Train_df)
  # Train2_df<-data.frame(predict(dummies,newdata=Train_df))
  # Train2_df[target] <- Train_df[target]
  # Train2_df <- Train2_df%>%mutate_at(c('Groep.CON','Groep.POL','Groep.CT1','Groep.CT2'),as.numeric)
  # 
  # #create dummy variables test set
  # dummies <- dummyVars(formula, data = Test_df)
  # Test2_df<-data.frame(predict(dummies,newdata=Test_df))
  # Test2_df[target] <- Test_df[target]
  # Test2_df <- Test2_df%>%mutate_at(c('Groep.CON','Groep.POL','Groep.CT1','Groep.CT2'),as.numeric)
  # 
  # 
  descrCor <-  cor(Train2_df%>%select(where(is.numeric),-target),use = 'complete.obs')
  highlyCorDescr <- findCorrelation(descrCor, cutoff = .7)
  names<-colnames(Train2_df%>%select(where(is.numeric)))[-highlyCorDescr]
  filtered_df <- Train2_df[,names]
  filtered_df[target] <-(Train_df[target])
  
  
  # RFE on filtered data ----------------------------------------------------
  
  myRfeControl <- rfeControl(functions = rfFuncs, # linear model
                             method = "repeatedcv",repeats=5,verbose = F) # Leave one out cross validation
  subsets <- c(2:ncol(filtered_df))
  set.seed(123)
  rfe_results <- rfe(formula,
                     data=filtered_df,
                     sizes = subsets,
                     rfeControl = myRfeControl)
  names<-predictors(rfe_results)
  filtered_df <- Train2_df[,names]
  filtered_df[target] <-(Train_df[target])

  list_rfe[[length(list_rfe)+1]] = names
  # 
  # baseline model ----------------------------------------------------------
  myControl  <- trainControl(method = "none",verboseIter = F)
  set.seed(321)
  baseline_train<- data.frame(target=Train2_df[target],dummy=mean(Train2_df[,target]))
  names(baseline_train)<-c(target,'dummy')
  baseline_test <- data.frame(target=Test2_df[target],dummy=mean(Test2_df[,target]))
  names(baseline_test)<-c(target,'dummy')
  formula_baseline <-  as.formula(paste0(target,'~dummy'))
  model_baseline <- train(formula_baseline,
                          baseline_train,
                          trControl = myControl,
                          method = "lm",
  )
  list_baseline[[length(list_baseline)+1]] <- model_baseline
  
  # GLM net model ---------------------------------------------------------
  
  myControl <- trainControl(method = "none",verboseIter = F)
  set.seed(321)
  model_glmnet_filtered <- train(formula,
                                 Train2_df, #C for all data
                                 # filtered_df, # for filtered data
                                 preProcess = c('center','scale'),
                                 method = "glmnet",
                                 trControl = myControl,
                                 importance = 'permutation',
                                 na.action = na.pass,
                                 metric = 'RMSE'
  )
  
  list_glm[[length(list_glm)+1]] <- model_glmnet_filtered
  
  # random forest -----------------------------------------------------------
  set.seed(321)
  myControl <- trainControl(method = "none",verboseIter = F)
  model_rf_filtered <- train(formula,
                             Train2_df, # for all data
                             # filtered_df, # for filtered data
                             method = "ranger",
                             trControl = myControl,
                             importance = 'permutation',
                             na.action = na.pass,
                             metric = 'RMSE'
  )
  
  list_rf[[length(list_rf)+1]] <- model_rf_filtered
  
  
  # support vector machine --------------------------------------------------
  set.seed(321)
  myControl <- trainControl(method = "none",verboseIter = F)
  model_svm_filtered <- train(formula,
                              Train2_df, # for all data
                              # filtered_df, # for filtered data
                              method = "svmRadial",
                              preProcess = c('center','scale'),
                              trControl = myControl,
                              importance = 'permutation',
                              na.action = na.pass,
                              metric = 'RMSE'
  )
  
  list_svm[[length(list_svm)+1]] <- model_svm_filtered
  

# calculate errors --------------------------------------------------------

  error<-(predict(list_baseline[[i]],baseline_train))-baseline_train$Power
  error<-unlist(lapply(error,as.numeric))
  RMSE_train_baseline<-sqrt(mean(error^2))
  
  error<-(predict(list_baseline[[i]],baseline_test))-baseline_test$Power
  error<-unlist(lapply(error,as.numeric))
  RMSE_test_baseline<-sqrt(mean(error^2))
  
  error<-(predict(list_glm[[i]],Train2_df))-Train2_df$Power
  error<-unlist(lapply(error,as.numeric))
  RMSE_train_glm<-sqrt(mean(error^2))
  error<-(predict(list_glm[[i]],Test2_df))-Test2_df$Power
  error<-unlist(lapply(error,as.numeric))
  RMSE_test_glm<-sqrt(mean(error^2))
  
  error<-(predict(list_rf[[i]],Train2_df))-Train2_df$Power
  error<-unlist(lapply(error,as.numeric))
  RMSE_train_rf<-sqrt(mean(error^2))
  error<-(predict(list_rf[[i]],Test2_df))-Test2_df$Power
  error<-unlist(lapply(error,as.numeric))
  RMSE_test_rf<-sqrt(mean(error^2))
  
  error<-(predict(list_svm[[i]],Train2_df))-Train2_df$Power
  error<-unlist(lapply(error,as.numeric))
  RMSE_train_svm<-sqrt(mean(error^2))
  error<-(predict(list_svm[[i]],Test2_df))-Test2_df$Power
  error<-unlist(lapply(error,as.numeric))
  RMSE_test_svm<-sqrt(mean(error^2))
  
  df<- data.frame(RMSE_train_baseline,RMSE_test_baseline,
                  RMSE_train_glm,RMSE_test_glm,
                  RMSE_train_rf,RMSE_test_rf,
                  RMSE_train_svm,RMSE_test_svm)
  
  tot_res <- rbind(df,tot_res)  
  
  # calculate importance ----------------------------------------------------
  
  imp <- varImp(list_glm[[i]],scale=F)
  imp <- imp$importance
  imp <- as.data.frame(imp)
  imp$varnames <- rownames(imp)
  rownames(imp) <- NULL
  colnames(imp) <- c(paste0(i,' fold'),'varnames')
  df <- imp
  
  
  if(length(importance)==0){
    importance = df}
  else{ importance <- merge(importance,df,by = 'varnames',all.x=T,all.y = T) }  
  

# get hyperparameters (in loop) -----------------------------------------------------

  tmp_rf <-list_rf[[i]]
  tmp_glm <-list_glm[[i]]
  tmp_svm <-list_svm[[i]]
  
  
  if(i==1){
    hyper_rf <- tmp_rf$bestTune
    hyper_glm <- tmp_glm$bestTune
    hyper_svm <- tmp_svm$bestTune
  } else{
    hyper_rf <- rbind(hyper_rf,tmp_rf$bestTune)
    hyper_glm <- rbind(hyper_glm,tmp_glm$bestTune)
    hyper_svm <- rbind(hyper_svm,tmp_svm$bestTune)
  }  
  
}

test<-tot_res %>% summarise(across(everything(), list(mean,sd)))


importance_filt<-importance %>%
  filter(rowSums(is.na(importance)) < 4)%>%rowwise()%>%mutate(mean = mean(c_across(c(contains('fold'))),na.rm=T),
                                                              sd = sd(c_across(c(contains('fold'))),na.rm=T))%>%
  arrange(desc(mean))

ggplot(importance_filt%>%arrange(desc(mean))%>%head(10), aes(x=reorder(varnames, mean), y=mean)) + 
  geom_bar(stat="identity",position=position_dodge(),fill = '#E69F00') +
  geom_errorbar(aes(ymin = mean-sd,ymax = mean+sd),width=.2)+
  coord_flip()+
  ggtitle('Top 10 important features')+
  theme_minimal()



# get hyperparameters and SHAP values -----------------------------------------------------

library(kernelshap)
library(shapviz)
library(plyr)
tot_res_shap<-c()
for (i in seq){

  Train_df <- selected%>%filter(fold_id!=i)%>%select(-fold_id)
  Test_df <- selected%>%filter(fold_id==i)%>%select(-fold_id)

  # get df for baseline models
  baseline_train<- data.frame(target=Train2_df[target],dummy=mean(Train2_df[,target]))
  names(baseline_train)<-c(target,'dummy')
  baseline_test <- data.frame(target=Test2_df[target],dummy=mean(Test2_df[,target]))
  names(baseline_test)<-c(target,'dummy')
  formula_baseline <-  as.formula(paste0(target,'~dummy'))

  # imputing missing variables ----------------------------------------------

  preProcValues <- preProcess(as.data.frame(Train_df),method = c("medianImpute"))
  Train_df <- predict(preProcValues, Train_df)

  Test_df<- predict(preProcValues, Test_df)

  # data preprocessing ------------------------------------------------------

  #create dummy variables train set
  dummies <- dummyVars(formula, data = Train_df)
  Train2_df<-data.frame(predict(dummies,newdata=Train_df))
  Train2_df[target] <- Train_df[target]
  Train2_df <- Train2_df%>%mutate_at(c('Groep.CON','Groep.POL','Groep.CT1','Groep.CT2'),as.numeric)

  #create dummy variables test set
  dummies <- dummyVars(formula, data = Test_df)
  Test2_df<-data.frame(predict(dummies,newdata=Test_df))
  Test2_df[target] <- Test_df[target]
  Test2_df <- Test2_df%>%mutate_at(c('Groep.CON','Groep.POL','Groep.CT1','Groep.CT2'),as.numeric)


  descrCor <-  cor(Train2_df%>%select(where(is.numeric),-target),use = 'complete.obs')
  highlyCorDescr <- findCorrelation(descrCor, cutoff = .7)
  names<-colnames(Train2_df%>%select(where(is.numeric)))[-highlyCorDescr]
  filtered_df <- Train2_df[,names]
  filtered_df[target] <-(Train_df[target])




  fit = list_glm[[i]]
  # 1) Select rows to explain
  # X = filtered_df%>%select(-target)
  X = Train2_df%>%select(-target)

  # 2) Select small representative background data
  set.seed(321)
  # bg_X <- filtered_df[sample(nrow(filtered_df), 3), ]%>%select(-target)
  bg_X <- Train2_df[sample(nrow(Train2_df), 2), ]%>%select(-target)

  # # 3) Calculate SHAP values in fully parallel mode
  # registerDoFuture()
  # plan(multisession, workers = 6)  # Windows
  # # plan(multicore, workers = 6)   # Linux, macOS, Solaris


  shap_values <- kernelshap(
    fit, X, bg_X = bg_X, parallel = F, parallel_args = list(.packages = "splines",exact=T)
  )
  sv <- shapviz(shap_values)
  tmp <- data.frame(sv$S)
  # tmp<-as.data.frame.list(tmp %>% colMeans(na.rm = TRUE))
  tmp$len = nrow(tmp)
  tmp$idx = i
  if(length(tot_res_shap)==0){
    tot_res_shap <-tmp}
  else{
    tot_res_shap<-rbind.fill(tot_res_shap,tmp)
  }
}
detach('package:plyr')
# get the tot res_shap dataframe into format for plotting
numobs<-tot_res_shap%>%group_by(idx)%>%summarise_at(vars('len'),list(mean))%>%summarise(sum(len))%>%as.vector()%>%unlist()

test<-tot_res_shap%>%select(-len,-idx)%>%t()%>%as.data.frame()
test<-test%>%filter(rowSums(is.na(test))<round(0.4*numobs))
test$varnames <- rownames(test)
rownames(test) <- 1:nrow(test)
test<-test%>%rowwise()%>%mutate(mean = mean(abs(c_across(V1:V171)),na.rm=T),
                                sd = sd(abs(c_across(V1:V171)),na.rm=T))%>%
  arrange(desc(mean))
test[1:5,]$varnames<-c('Prior_training_distance','Fat_percentage','VT2_VO2','VO2_max','VT2_power')

ggplot(test%>%arrange(desc(mean))%>%head(5), aes(x=reorder(varnames, mean), y=mean)) + 
  geom_bar(stat="identity",position=position_dodge(),fill = '#E69F00',color='black') +
  geom_errorbar(aes(ymin = mean,ymax = mean+sd),width=.2)+
  coord_flip()+
  ggtitle('Top 5 important features Glmnet')+
  xlab('')+
  ylab('Mean absolute SHAP score')+
  theme_minimal()



