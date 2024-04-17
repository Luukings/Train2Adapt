get_questionare_features<-function(data,windows,functions,column){
  # calculate features based on the T2A questionnaire dataset. data contains all questionnaire data and days of the study. 
  # window contains all the windows used for feature engineering
  # functions contains all the aggregation functions used
  # column is a string vector over which the aggregations are used
  
  features<-c()
  for(window in windows){
    for (fun in functions){
      func = match.fun(fun)
      if (fun == 'quantile'){
        
        tmp <- suppressMessages(data%>%
                                  filter(if (window == 'all') cycle<=3 else cycle == as.numeric(window))%>%
                                  group_by(code)%>%
                                  summarize(var = func(.data[[column]],probs = c(0.05,0.25,0.50,0.75,0.95),na.rm=T)))
        
        tmp <- data.frame(code = tmp$code[seq(1,nrow(tmp),5)],
                          var1 =tmp$var[seq(1,nrow(tmp),5)],
                          var2 =tmp$var[seq(2,nrow(tmp),5)],
                          var3 =tmp$var[seq(3,nrow(tmp),5)],
                          var4 =tmp$var[seq(4,nrow(tmp),5)],
                          var5 =tmp$var[seq(5,nrow(tmp),5)])
        
        tmp<-rename(tmp,
                    !!paste0(column,'_','5_percentile_','cycle_',window) := var1,
                    !!paste0(column,'_','25_percentile_','cycle_',window) := var2,
                    !!paste0(column,'_','50_percentile_','cycle_',window) := var3,
                    !!paste0(column,'_','75_percentile_','cycle_',window) := var4,
                    !!paste0(column,'_','95_percentile_','cycle_',window) := var5)
        
        NA_vec = data%>%
          filter(if (window == 'all') cycle<=3 else cycle == as.numeric(window))%>%
          group_by(code)%>%
          filter(type == 'daily_questionnaire')%>%
          summarize(num_days = n(),
                    window = first(window))%>%
          mutate(NA_var = case_when(window=='1' | window=='2' | window=='3' ~ (28-num_days)/28,
                                    window=='all' ~ (84-num_days)/84))%>%
          select(NA_var,code)
          
        
        # NA_vec <- rename(NA_vec,!!paste0(column,'_',fun,'_cycle_',window,'_percentage_missing_days') := NA_var)
     
      }else{
        tmp<-data%>%
          filter(if (window == 'all') cycle<=3 else cycle == as.numeric(window))%>%
          group_by(code)%>%
          summarize(var = func(.data[[column]],na.rm=T))
        
        tmp   <-rename(tmp,!!paste0(column,'_',fun,'_cycle_',window) := var)
      }
      
      if (length(features) == 0){
        features <- tmp
      } else{
        if (nrow(tmp)> nrow(features)){
          features <- right_join(features,tmp,by='code')
        }else{
          features <- left_join(features,tmp,by='code')
        }
      }
    }
    # features <- left_join(features,NA_vec,by='code')
  }
  return(features)
}