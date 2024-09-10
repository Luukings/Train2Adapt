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
    library(writexl)

# ------------------------------------------------------
# Load helper scripts
# ------------------------------------------------------ 

    source('scripts/perform_ml_modelling.R')
    source('scripts/eval_ml_modelling.R')
    source('scripts/plot_feature_importance.R')
    source('scripts/plot_individual_resp.R')
    source('scripts/rename_columns.R')
    source('scripts/univariate_analysis.R')
    
# ------------------------------------------------------
# Data loading
# ------------------------------------------------------  

    setwd("data") # set wd where data is stored
    
    # Select model target
    predict_target <- 'delta'
    
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
    data <- data %>% select(-contains('weight'),-code)
    

# ------------------------------------------------------
# Data partitioning
# ------------------------------------------------------   

    # Data partitioning: split data into train set and holdout test set for outer loop of nested cross-validation
    if (predict_target %in% c('pre','post')) {
      set.seed(289, kind = 'Mersenne-Twister', normal.kind = 'Inversion') 
    } else {
      set.seed(19, kind = 'Mersenne-Twister', normal.kind = 'Inversion')   # to obtain similar train-test distribution
    } # 206 | 4
    
    # Perform stratified sampling for all folds (making sure male/female participants are equally distributed)
    repeats     <- 5 # Apply 5-fold CV for outer loop
    trainIndex  <- createDataPartition(data %>% arrange(sex_male,TT_power) %>% pull(TT_power), 
                                       times=repeats, p = .65, list=F)
    out         <- vector("list", length = repeats)
    
    results_repeated_modelling <- c()
    pred_test_df <- c()
    for (k in seq(1,repeats)) {
      
        # For each fold obtain relevant train and test data
        data            <- data %>% arrange(sex_male,TT_power)
        data_train      <- data[trainIndex[,k],]
        data_test       <- data[-trainIndex[,k],]
        
        # add split variable for plotting
        data$split              <- ''
        
        data$split[trainIndex[,k]]  <- 'Train'
        data$split[-trainIndex[,k]] <- 'Test'
    
        # Check target distributions for training and testing set.
        order      <-c('Train','Test')
        cbbPalette <- c( "#E69F00", "#56B4E9")
        
        #plot TT_power for train and test set
        out[[k]]<-eval(bquote(data%>%ggplot(aes(split,TT_power,fill=split))+
          geom_boxplot(width = 0.1)+
          geom_jitter(width=0.01)+
          scale_fill_manual(values  =cbbPalette)+
          scale_x_discrete(limits = order)+
          theme_minimal()+
          xlab("")))
    
        # remove split and code features from data
        data <- data %>% select(-split)
        
    # ------------------------------------------------------
    # Modelling  
    # ------------------------------------------------------ 
    
        # Perform modelling (for three types of algorithm either with/without hyperparameter tuning and with/without prior filtering based on multicollinearity).
        # hyperparameter tuning is performed within the inner loop of the nested cross-validation (using leave-one-out cross validation to obtain the best model
        # tuning paramaters based on the train dataset)
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
            results_glm <- rbind('model_glm_no_hpt_no_filt'    = eval_ml_modelling(model_glm_no_hpt_no_filt,data_train,target,plot = FALSE),
                                 'model_glm_hpt_no_filt'       = eval_ml_modelling(model_glm_hpt_no_filt,data_train,target,plot = FALSE),
                                 'model_glm_hpt_filt'          = eval_ml_modelling(model_glm_hpt_filt,data_train,target,plot = FALSE))
            
            results_rf <- rbind('model_rf_no_hpt_no_filt'      = eval_ml_modelling(model_rf_no_hpt_no_filt,data_train,target,plot = FALSE),
                                'model_rf_hpt_no_filt'         = eval_ml_modelling(model_rf_hpt_no_filt,data_train,target,plot = FALSE),
                                'model_rf_hpt_filt'            = eval_ml_modelling(model_rf_hpt_filt,data_train,target,plot = FALSE))
            
            results_pcr <- rbind('model_pcr_no_hpt_no_filt'    = eval_ml_modelling(model_pcr_no_hpt_no_filt,data_train,target,plot = FALSE),
                                 'model_pcr_hpt_no_filt'       = eval_ml_modelling(model_pcr_hpt_no_filt,data_train,target,plot = FALSE),
                                 'model_pcr_hpt_filt'          = eval_ml_modelling(model_pcr_hpt_filt,data_train,target,plot = FALSE))
    
            # Model performance on independent test set
            testresults_glm <- rbind('model_glm_no_hpt_no_filt'= eval_ml_modelling(model_glm_no_hpt_no_filt,data_test,target,plot = FALSE),
                                 'model_glm_hpt_no_filt'       = eval_ml_modelling(model_glm_hpt_no_filt,data_test,target,plot = FALSE),
                                 'model_glm_hpt_filt'          = eval_ml_modelling(model_glm_hpt_filt,data_test,target,plot = FALSE))
            
            testresults_rf <- rbind('model_rf_no_hpt_no_filt'  = eval_ml_modelling(model_rf_no_hpt_no_filt,data_test,target,plot = FALSE),
                                'model_rf_hpt_no_filt'         = eval_ml_modelling(model_rf_hpt_no_filt,data_test,target,plot = FALSE),
                                'model_rf_hpt_filt'            = eval_ml_modelling(model_rf_hpt_filt,data_test,target,plot = FALSE))
            
            testresults_pcr <- rbind('model_pcr_no_hpt_no_filt'= eval_ml_modelling(model_pcr_no_hpt_no_filt,data_test,target,plot = FALSE),
                                 'model_pcr_hpt_no_filt'       = eval_ml_modelling(model_pcr_hpt_no_filt,data_test,target,plot = FALSE),
                                 'model_pcr_hpt_filt'          = eval_ml_modelling(model_pcr_hpt_filt,data_test,target,plot = FALSE))
    
            # Model predictions on independent test set
            pred_test_df <- rbind(pred_test_df,
                                  eval_ml_modelling(model_glm_no_hpt_no_filt,data_test,target,plot = FALSE, return_df_pred = TRUE) %>% mutate(fold=k,model='model_glm_no_hpt_no_filt'),
                                  eval_ml_modelling(model_glm_hpt_no_filt,data_test,target,plot = FALSE, return_df_pred = TRUE) %>% mutate(fold=k,model='model_glm_hpt_no_filt'),
                                  eval_ml_modelling(model_glm_hpt_filt,data_test,target,plot = FALSE, return_df_pred = TRUE) %>% mutate(fold=k,model='model_glm_hpt_filt'),
                                  eval_ml_modelling(model_rf_no_hpt_no_filt,data_test,target,plot = FALSE, return_df_pred = TRUE) %>% mutate(fold=k,model='model_rf_no_hpt_no_filt'),
                                  eval_ml_modelling(model_rf_hpt_no_filt,data_test,target,plot = FALSE, return_df_pred = TRUE) %>% mutate(fold=k,model='model_rf_hpt_no_filt'),
                                  eval_ml_modelling(model_rf_hpt_filt,data_test,target,plot = FALSE, return_df_pred = TRUE) %>% mutate(fold=k,model='model_rf_hpt_filt'),
                                  eval_ml_modelling(model_pcr_no_hpt_no_filt,data_test,target,plot = FALSE, return_df_pred = TRUE) %>% mutate(fold=k,model='model_pcr_no_hpt_no_filt'),
                                  eval_ml_modelling(model_pcr_hpt_no_filt,data_test,target,plot = FALSE, return_df_pred = TRUE) %>% mutate(fold=k,model='model_pcr_hpt_no_filt'),
                                  eval_ml_modelling(model_pcr_hpt_filt,data_test,target,plot = FALSE, return_df_pred = TRUE) %>% mutate(fold=k,model='model_pcr_hpt_filt'))
            
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
        
        # Join data of model performance over all 5 folds of the outer loop of nested cross-validation
        results_repeated_modelling <- rbind(results_repeated_modelling,results_modelling)
    }
    # Plot table with model performance on training and testing set
    # table_models <-tableGrob(results_modelling %>%
    #                          select(algorithm,tuning,filter,contains('train_'),contains('test_')) %>%
    #                          select(-contains('RMSE')))
    # grid.arrange(table_models)

    # Get final results for model performance based on nested cross-validation
    table_repeated_modelling <- results_repeated_modelling %>% 
            mutate(algorithm = factor(algorithm,levels = c('glm','rf','pcr'))) %>%
            group_by(rowname,algorithm,tuning,filter) %>%
            summarize(mean_train_RMSE = mean(train_RMSE,na.rm=T),
                      sd_train_RMSE2 = sd(train_RMSE,na.rm=T),
                      mean_train_R2 = mean(train_R2,na.rm=T),
                      sd_train_R2 = sd(train_R2,na.rm=T),
                      mean_train_MAE = mean(train_MAE,na.rm=T),
                      sd_train_MAE = sd(train_MAE,na.rm=T),
                      mean_test_RMSE = mean(test_RMSE,na.rm=T),
                      sd_test_RMSE = sd(test_RMSE,na.rm=T),
                      mean_test_R2 = mean(test_R2,na.rm=T),
                      sd_test_R2 = sd(test_R2,na.rm=T),
                      mean_test_MAE = mean(test_MAE,na.rm=T),
                      sd_test_MAE = sd(test_MAE,na.rm=T),
                      train_R2  = paste0(round(mean(train_R2,na.rm=T),3),'+-',round(sd(train_R2,na.rm=T),3)),
                      train_MAE = paste0(round(mean(train_MAE,na.rm=T),3),'+-',round(sd(train_MAE,na.rm=T),3)),
                      test_R2   = paste0(round(mean(test_R2,na.rm=T),3),'+-',round(sd(test_R2,na.rm=T),3)),
                      test_MAE  = paste0(round(mean(test_MAE,na.rm=T),3),'+-',round(sd(test_MAE,na.rm=T),3))) %>%
           ungroup() %>% 
           arrange(desc(mean_train_R2)) %>% mutate(rank_train_R2 = row_number()) %>%
           arrange(mean_train_MAE) %>% mutate(rank_train_MAE = row_number()) %>%
           arrange(desc(mean_test_R2)) %>% mutate(rank_test_R2 = row_number()) %>%
           arrange(mean_test_MAE) %>% mutate(rank_test_MAE = row_number()) %>%
           mutate(rank_train = (rank_train_R2+rank_train_MAE)/2,
                  rank_test  = (rank_test_R2 + rank_test_MAE)/2) %>%
           arrange(algorithm,tuning,filter)

    printed_table_repeated_modelling <- table_repeated_modelling %>% select(algorithm:filter,train_R2:test_MAE)
    write_xlsx(printed_table_repeated_modelling, 'modlres.xlsx')  
    
    # Add information about model specifics to pred-meas df
    pred_test_df <- pred_test_df %>% left_join(table_repeated_modelling %>% select(rowname:filter), by = c('model'='rowname')) %>%
                    mutate(algorithm = factor(algorithm,levels = c('glm','rf','pcr'))) %>%
                    select(model:filter,fold,everything()) %>%
                    arrange(algorithm,tuning,filter)
      
    # Obtain best model for each target.
    if (predict_target == 'pre') {
      
      results_pre <- rbind(results_modelling)
      results_pre <- results_pre  %>% mutate(target = 'pre')
      
      results_df_pre <- pred_test_df  %>% mutate(target = 'pre')

      results_repeated_pre <- rbind(results_repeated_modelling)
      results_repeated_pre <- results_repeated_pre  %>% mutate(target = 'pre')
      
      best_model_pre           <- results_modelling %>% arrange(train_MAE) %>% first()
      best_repeated_model_pre  <- table_repeated_modelling %>% arrange(rank_train) %>% first()
      
      # save(results_pre,file = 'results_pre.Rdata')
      save(results_pre,results_df_pre,results_repeated_pre,best_repeated_model_pre,
           file = 'results_repeated_pre.Rdata')
      
    } else if (predict_target == 'post') {
      
      results_post <- rbind(results_modelling)
      results_post <- results_post %>% mutate(target = 'post')
      
      results_df_post <- pred_test_df  %>% mutate(target = 'post')
      
      results_repeated_post <- rbind(results_repeated_modelling)
      results_repeated_post <- results_repeated_post  %>% mutate(target = 'post')
      
      best_model_post           <- results_modelling %>% arrange(train_MAE) %>% first()
      best_repeated_model_post  <- table_repeated_modelling %>% arrange(rank_train) %>% first()
      
      # save(results_post,file = 'results_post.Rdata')
      save(results_post,results_df_post,results_repeated_post,best_repeated_model_post,
           file = 'results_repeated_post.Rdata')
      
    } else if (predict_target == 'delta') {
      
      results_diff <- rbind(results_modelling)
      results_diff <- results_diff %>% mutate(target = 'diff')
      
      results_df_diff <- pred_test_df  %>% mutate(target = 'diff')
      
      results_repeated_diff <- rbind(results_repeated_modelling)
      results_repeated_diff <- results_repeated_diff  %>% mutate(target = 'diff')
      
      best_model_diff <- results_modelling %>% arrange(desc(test_R2)) %>% first()
      best_repeated_model_diff  <- table_repeated_modelling %>% arrange(rank_train) %>% first()
      #best_repeated_model_diff  <- table_repeated_modelling %>% arrange(rank_test) %>% first()
      
      # save(results_diff,file = 'results_diff.Rdata')
      save(results_diff,results_df_diff,results_repeated_diff,best_repeated_model_diff,
           file = 'results_repeated_diff.Rdata')
    }

    # Combine all results
    if (exists('results_repeated_pre') & exists('results_repeated_post') & exists('results_repeated_diff')) {
      results_all  <- rbind(results_repeated_pre, results_repeated_post, results_repeated_diff)
      results_all  <- results_all %>% mutate_at(.vars = vars(any_of(c('filter','tuning'))),
                                                .funs = ~ifelse(.=='x',1,0)) %>%
                      mutate(train_test_diff_R2  = train_R2 - test_R2,
                             train_test_diff_MAE = train_MAE - test_MAE)
    }

    print({ggplot(pred_test_df, aes(x=predicted,y=measured,color=fold)) + 
          geom_point() + geom_abline(slope=1,intercept=0) + facet_wrap(algorithm~model) + 
          coord_fixed(ratio=1) + ggtitle(paste0(predict_target,' - ',best_repeated_model_diff))})
    print({grid.arrange(tableGrob(printed_table_repeated_modelling))})
    
    # --- Plot feature importance -----
    
    # Obtain best model for each target
    if (predict_target == 'pre') {
      
      set.seed(289, kind = 'Mersenne-Twister', normal.kind = 'Inversion')
      best_model  <- best_repeated_model_pre
      
    } else if (predict_target == 'post') {
      
      set.seed(289, kind = 'Mersenne-Twister', normal.kind = 'Inversion')
      best_model  <- best_repeated_model_post
      
    } else if (predict_target == 'delta') {
    
      set.seed(19, kind = 'Mersenne-Twister', normal.kind = 'Inversion')
      best_model  <- best_repeated_model_diff
    }
    
    # Determine feature importance (on entire dataset)
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
    
    # perform univariate analysis
    univariate_analysis <- univariate_analysis(data,target)
    if (predict_target == 'pre') {
      
      write_xlsx(univariate_analysis,"univariate_analysis_pre.xlsx")
      
    } else if (predict_target == 'post') {
      
      write_xlsx(univariate_analysis,"univariate_analysis_post.xlsx")
      
    } else if (predict_target == 'delta') {
      
      write_xlsx(univariate_analysis,"univariate_analysis_diff.xlsx")
    }
    
    # Plot individual training response
    load("diff_TT_performance.RData")
    plot_individual_resp(ML_dataframe)
    
# ------------------------------------------- end of script ----------------------------------------
    