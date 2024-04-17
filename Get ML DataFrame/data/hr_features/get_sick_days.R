get_sick_days<-function(data,windows,functions,column){
  features<-c()
  for(window in windows){
    tmp<-data%>%
      filter(if (window == 'all') cycle<=3 else cycle == as.numeric(window))%>%
      group_by(code)%>%
      summarize(var = sum(daily_sick == 'ja',na.rm=T))
    
    tmp   <-rename(tmp,!!paste0(column,'_cycle_',window) := var)
    if (length(features) == 0){
      features <- tmp
    } else{
      if (nrow(tmp)>nrow(features)){
        features <- right_join(features,tmp,by='code')
      }else{
        features <- left_join(features,tmp,by='code')
      }
    }
  }
  return(features)
}