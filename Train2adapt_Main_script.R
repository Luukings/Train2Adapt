    rm(list = ls())

    # Processing
    library(readxl)
    library(dplyr)
    library(tidyr)
    library(officer)
    library(purrr)
    library(tidyverse)
    
    # Modelling
    library(fastDummies)
    library(caret)
    library(ranger)
    library(MLmetrics)
    library(MLeval)
    library(glmnet)
    
    # Visualization
    library(ggplot2)
    library(ggpubr)
    library(gridExtra)
    library(ggpattern)
    
    # Other
    library(visdat)
    library(FactoMineR)
    library(DiagrammeR)
    library(kernlab)
    library(missRanger)
    library(pbapply)
    library(missMethods)

# ------------------------------------------------------
# Load helper scripts
# ------------------------------------------------------ 

    source('scripts/perform_ml_modelling.R')
    source('scripts/eval_ml_modelling.R')
    source('scripts/plot_feature_importance.R')
    source('scripts/plot_individual_resp.R')
    source('scripts/rename_columns.R')
    
# ------------------------------------------------------
# Data loading
# ------------------------------------------------------  

    setwd("data") # set wd where data is stored
    
    # Select model target
    predict_target <- 'post'
    
    # Load relevant dataframe correspoding to target
    if (predict_target == 'pre') {
      
      load('Pre_dataframe.RData') # load pre intervention dataframe
      ML_dataframe <- ML_dataframe %>% select(code,TT_power:MRT,contains('O2Hb'),contains('HHb')) 
      
    } else if (predict_target == 'post') {
      
      load('Post_dataframe.RData') # load post intervention dataframe
    
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
    data  <- rename_columns(data)
    data  <- dummy_cols(data,select_columns='sex',remove_first_dummy = T,remove_selected_columns = T) # create dummy cols based on 'Groep' column
    
    # Perform normalization to body weight for pre and post predictions
    if (predict_target %in% c('pre','post')) {
      
    data$TT_power <- data$TT_power/data$weight # normalize power to bodyweight
    
    }
    
    # Remove participants without time trial performance
    data <- data %>% filter(!is.na(TT_power))
    
    # Set prediction formula
    target  <- 'TT_power' # select target for modelling
    formula <- as.formula(paste0(target,'~.')) # create formula for modelling
    
    # Perform dummification for training intervention groups
    if ('Groep' %in% colnames(data)){
      data <- data %>%  rename(training_group = Groep) %>% mutate(training_group = case_when(training_group == 'CT1'~ 'CTcon',
                                                                                             training_group == 'CT2'~ 'CTecc',
                                                                                             TRUE ~ training_group))
      data  <- dummy_cols(data,select_columns='training_group',remove_first_dummy = T,remove_selected_columns = T) # create dummy cols based on 'Groep' column
    }
    
    # Remove variables related to weight as this is used as denominator in the target 
    data <- data %>% select(-contains('weight'))
    

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

    # Check target distributions for training and testing set.
    order      <-c('Train','Test')
    cbbPalette <- c( "#E69F00", "#56B4E9")
    
    #plot TT_power for train and test set
    data%>%ggplot(aes(split,TT_power,fill=split))+
      geom_boxplot(width = 0.1)+
      geom_jitter(width=0.01)+
      scale_fill_manual(values  =cbbPalette)+
      scale_x_discrete(limits = order)+
      theme_minimal()+
      xlab("")

    # remove split and code features from data
    data <- data %>% select(-split,-code)
    data_train <- data_train %>% select(-code)

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
    
    # Plot table with model performance on training and testing set
    table_models <-tableGrob(results_modelling %>%
                             select(algorithm,tuning,filter,contains('train_'),contains('test_')) %>%
                             select(-contains('RMSE')))
    grid.arrange(table_models)


    # Obtain best model for each target.
    if (predict_target == 'pre') {
      
      results_pre <- rbind(results_modelling)
      results_pre <- results_pre  %>% mutate(target = 'pre')
      best_model_pre  <- results_modelling %>% arrange(train_MAE) %>% first()
      # save(results_pre,file = 'results_pre.Rdata')
      
    } else if (predict_target == 'post') {
      
      results_post <- rbind(results_modelling)
      results_post <- results_post %>% mutate(target = 'post')
      best_model_post  <- results_modelling %>% arrange(train_MAE) %>% first()
      # save(results_post,file = 'results_post.Rdata')
      
    } else if (predict_target == 'delta') {
      
      results_diff <- rbind(results_modelling)
      results_diff <- results_diff %>% mutate(target = 'diff')
      best_model_diff <- results_modelling %>% arrange(desc(test_R2)) %>% first()
      #save(results_diff,file = 'results_diff.Rdata')
      
    }

    # Combine all results
    if (exists('results_pre') & exists('results_post') & exists('results_diff')) {
      results_all  <- rbind(results_pre, results_post, results_diff)
      results_all  <- results_all %>% mutate_at(.vars = vars(any_of(c('filter','tuning'))),
                                                .funs = ~ifelse(.=='x',1,0)) %>%
                      mutate(train_test_diff_R2  = train_R2 - test_R2,
                             train_test_diff_MAE = train_MAE - test_MAE)
    }

    # --- Plot feature importance -----
    
    # Obtain best model for each target
    #predict_target = 'pre'
    if (predict_target == 'pre') {
      
      set.seed(123)
      best_model  <- best_model_pre
      
    } else if (predict_target == 'post') {
      
      set.seed(123)
      best_model  <- best_model_post
      
    } else if (predict_target == 'delta') {
    
      set.seed(321)
      best_model  <- best_model_diff
    }
    # Determine feature importance based on entire dataset
    myControl <- trainControl(method = "none",verboseIter = F)
    model     <- train(formula,
                       data,
                       preProcess = c('center','scale','medianImpute'),
                       method = case_when(str_detect(best_model$rowname,'_rf_')  ~ "ranger",
                                          str_detect(best_model$rowname,'_glm_') ~ "glmnet",
                                          str_detect(best_model$rowname,'_pcr_') ~ "pcr"),
                       trControl = myControl,
                       tuneGrid = eval(as.name(best_model$rowname))$bestTune,
                       importance = 'permutation',
                       na.action = na.pass,
                       metric = 'RMSE')
    
    # Process importance scores
    imp  <- varImp(model,scale=F, nonpara = T)
    imp  <- data.frame(imp$importance) %>% arrange(desc(Overall)) %>% rename(feature_imp = Overall) %>% filter(feature_imp>0) %>% top_n(30) %>% rownames_to_column()
    corr <- cor(data%>%select(TT_power,where(is.numeric)),use = 'pairwise.complete.obs') 
    corr <- data.frame(corr) %>% select(TT_power) %>% rename(corr = TT_power) %>% rownames_to_column()
    imp  <- left_join(imp,corr) %>% mutate(corr_sign = factor(case_when(abs(corr)<=.05 ~'±',corr < .05 ~'-',corr > .05 ~'+'),levels=c('-','±','+')))
    
    # Plot feature importance scores
    if (predict_target == 'pre') {
      A <-  plot_feature_importance(imp,predict_target)
    } else if (predict_target == 'post') {
      B <-  plot_feature_importance(imp,predict_target)
    } else if (predict_target == 'delta') {
      C <-  plot_feature_importance(imp,predict_target)
    }
    
    png(paste0("./Figure3_",format(Sys.Date(),"%d%m%y"),".png"),
         bg = "transparent", width = 12, height = 6, unit = "in", pointsize = 11, res = 1200)
    
    graphic <- ggarrange(A, B, C,
                         labels = c("A","B","C"),
                         ncol = 3, nrow = 1,
                         align = "v")
    print({graphic})
    
    dev.off()
    
    # Plot individual training response
    load("diff_TT_performance.RData")
    plot_individual_resp(ML_dataframe)

    
# ------------------------------------------- end of script ----------------------------------------
    
