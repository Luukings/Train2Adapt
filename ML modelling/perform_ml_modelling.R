perform_ml_modelling <- function(data, target, type, filtering, HPT, CV) {
  
  # Input:   train data set, target value for models, type of algorithm: rf, glmnet, svm , 
  #          perform filtering y/n, perform HPT y/n 
  # Process: perform ML modelling based on train set and type of algorithm  with preprocessing 
  #          within the folds and hyperparameter tuning. Models are optimized for RMSE metric
  # Output:  final model
  
  formula <- as.formula(paste0(target,'~.'))
  
  # perform pre processing here
  preprocessing <- c('center','scale','medianImpute')
  
  # preproc <- preProcess(as.data.frame(data),method = 'knnImpute')
  # data    <- predict(preproc,data)
  
  if (filtering == 'yes'){
    print(ncol(data)-1)
    data_tmp       <- data[target]
    descrCor       <- cor(data%>%select(where(is.numeric),-target),use = 'complete.obs')
    # descrCor       <- descrCor[rowSums(is.na(descrCor)) == 1,]
    highlyCorDescr <- findCorrelation(descrCor, cutoff = .7)
    names          <- colnames(data%>%select(where(is.numeric)))[-(highlyCorDescr+1)]
    data           <- data[,names]
    print(ncol(data))
    
    data[target]   <- data_tmp
  } 
  if ( CV == 'yes'){
    myControl  <- trainControl(method = "LOOCV",verboseIter = F)
    
  } else{
    myControl <- trainControl(method = "none",verboseIter = F)
  }
  
  
  if (HPT == 'yes'){
    # Set cross-validation parameters for HPT
    # myControl  <- trainControl(method = "LOOCV",verboseIter = F)
    
    # Train model using caret package
    
    if (type == 'glm'){
      
      # 1. Glmnet 
      
      # Define grid for HPT
      Grid_glm <- expand.grid(alpha= seq(0,1,length=20),
                              lambda = seq(0.001,10,length=100))
      
      # Perform HPT
      model_glm <- train(formula,
                         data,
                         preProcess = preprocessing,
                         method = "glmnet",
                         trControl = myControl,
                         # tuneGrid = Grid_glm,
                         tuneLength = 10,
                         importance = 'permutation',
                         na.action = na.pass,
                         metric = 'RMSE')
      
      return(model_glm)
      
    } else if (type == 'rf'){
      
      # 2. Random Forest 
      
      # Define grid for HPT 
      
      # if(ncol(data>60)){
      #   Grid_rf  <- expand.grid(mtry = seq(1,ncol(data)-1,10), # check general approach
      #                           splitrule = c('variance', 'extratrees'),
      #                           min.node.size = c(1,3,5,7,9))
      # } else{
        
        Grid_rf  <- expand.grid(mtry = seq(1,8,1),
                                splitrule = c('variance', 'extratrees'),
                                min.node.size = c(1,3,5,7,9))
      # }
      
      # Perform HPT
      model_rf <- train(formula,
                         data,
                         preProcess = preprocessing,
                         method = "ranger",
                         trControl = myControl,
                         #tuneGrid = Grid_rf,
                         tuneLength = 10,
                         importance = 'permutation',
                         na.action = na.pass,
                         metric = 'RMSE')
      
      return(model_rf)
      
    } else if (type == 'svm'){
      
      # 3. SVM
      
      # Define grid for HPT
      Grid_svm  <- expand.grid(sigma = seq(0.00001,0.02,0.0005),
                               C = seq(0.001,10,.25))
      
      # Perform HPT
      model_svm <- train(formula,
                        data,
                        preProcess = preprocessing,
                        method = 'svmRadialSigma',
                        trControl = myControl,
                        #tuneGrid = Grid_svm,
                        tuneLength = 10,
                        importance = 'permutation',
                        na.action = na.pass,
                        metric = 'RMSE')

      return(model_svm)

    } else if (type == 'pcr'){
    
    # 3. Principle component regression
    
    # Define grid for HPT
    Grid_pcr  <- expand.grid(ncomp = seq(1:10))
    
    # Perform HPT
    model_pcr <- train(formula,
                       data,
                       preProcess = preprocessing,
                       method = 'pcr',
                       trControl = myControl,
                       #tuneGrid = Grid_pcr,
                       tuneLength = 10,
                       importance = 'permutation',
                       na.action = na.pass,
                       metric = 'RMSE')
    
    return(model_pcr)
  }
    
  }
  
  if (type == 'glm' & HPT == 'no'){
    
    # 1. Glmnet 
    
    
    
    # Fit final model
    model_glm <- train(formula,
                       data,
                       preProcess = preprocessing,
                       method = "glmnet",
                       trControl = myControl,
                       importance = 'permutation',
                       na.action = na.pass,
                       metric = 'RMSE')
    return(model_glm)
  }
  
  if (type == 'rf' & HPT == 'no' ){
    
    # 2. Random Forest 
    
    # myControl  <- trainControl(method = "none",verboseIter = F)
    
    # Fit final model
    model_rf <- train(formula,
                      data,
                      preProcess = preprocessing,
                      method = "ranger",
                      trControl = myControl,
                      importance = 'permutation',
                      na.action = na.pass,
                      metric = 'RMSE')
    return(model_rf)
  }
  
  if (type == 'svm'& HPT == 'no'){
    # 3. SVM

    # Fit final model
    model_svm <- train(formula,
                       data,
                       preProcess = preprocessing,
                       method = 'svmRadialSigma',
                       trControl = myControl,
                       importance = 'permutation',
                       na.action = na.pass,
                       metric = 'RMSE')
    
    return(model_svm)
  }
  
  if (type == 'pcr'& HPT == 'no'){
    
  # 3. Principle component regression
  
  # Define grid for HPT
  Grid_pcr  <- expand.grid(ncomp = seq(1:10))
  
  # Perform HPT
  model_pcr <- train(formula,
                     data,
                     preProcess = preprocessing,
                     method = 'pcr',
                     trControl = myControl,
                     importance = 'permutation',
                     na.action = na.pass,
                     metric = 'RMSE')
  
  return(model_pcr)
}


}