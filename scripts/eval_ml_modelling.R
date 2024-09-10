eval_ml_modelling <- function(model, data, target, plot=FALSE, header = '', return_df_pred = FALSE) {
  
  # Input:   model, test data set, target column and whether to plot the results
  # Process: Predict model outcome and calculate R^2and RMSE 
  # Output:  model performance 
  
  # preprocess <-  preProcess(as.data.frame(data),method = "knnImpute") # if there are missing values in the data these are imputated
  # data       <-  predict(preprocess,data) # if there are missing values in the data these are imputated

  data <- impute_median(data)
  
  measured  <- as.numeric(data[[target]]) # get the measured data
  predicted <- predict(model,data) # get the predicted data
  
  # try with postResample function
  pred_outcome <- postResample(pred = predicted, obs = measured)

  df <- data.frame(measured,predicted)
  
  error <- measured - predicted # calculate error
  SSE   <- sum(error^2)
  SST   <- sum((measured-mean(measured))^2)
  
  R_sq <- 1-SSE/SST
  RMSE <- sqrt(mean(error^2))
  
  model_results <- data.frame(RMSE = pred_outcome['RMSE'],
                              Rsquared = pred_outcome['Rsquared'],
                              MAE = pred_outcome['MAE'])
  model_results <- model_results %>% mutate(Rsquared = ifelse(is.na(Rsquared),R_sq, Rsquared))
  
  # when result plot is desired a plot will be shown
  if (plot == TRUE){
    plot <- ggplot(df,aes(predicted, measured))+geom_point()+geom_abline(slope=1,intercept=0) + theme_light() + ggtitle(header)
    print(plot)
  }
  
  if (return_df_pred == TRUE) {
    return(df)
  } else {
    return(model_results)
  }
  
  
   
}


