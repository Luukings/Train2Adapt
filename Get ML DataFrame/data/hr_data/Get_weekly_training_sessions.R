Get_weekly_training_sessions <- function(data) {


  Week_count<-data%>%
    group_by(code,file_ID)%>%
    summarize(wk=isoweek(start_date[n()]),
              Training_time = first(Training_time_min))%>%
    group_by(code,wk)%>%
    summarize(wk=first(wk),
              no_training = n(),
              Training_time = sum(Training_time))
  
  unique_id <- unique(Week_count$code)
  weekly_training<- data.frame()
  for (id in unique_id){
    tmp<-Week_count%>%
      filter(code == id)
    tmp2 <- data.frame(t(tmp))
    tmp2<-tmp2%>%
      row_to_names(row_number = 2)
    tmp2$id<- id
    if (length(weekly_training)==0){
      weekly_training<-tmp2
    }
    else {
      weekly_training<-bind_rows(weekly_training,tmp2)
    }
    
  }
  weekly_training<- weekly_training%>%
    select(id,'38','39','40','41','42','43','44','45','46','47','48','49')
  row.names(weekly_training)<- 1:nrow(weekly_training)
  
  weekly_training<-weekly_training%>%
    mutate_at(.vars = vars(!one_of('id')),
              .funs = list(~as.numeric(.)))%>%
    mutate(sum = rowSums(across(where(is.numeric)),na.rm=T))
  
  return(weekly_training)
}
