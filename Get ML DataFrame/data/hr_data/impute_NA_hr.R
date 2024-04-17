impute_NA_hr<-function(data,ids){
  
  pb <- txtProgressBar(min = 0, max = length(ids), style = 3)
  i=0
  for (id in ids){
    tmp <- data%>%filter(file_ID==id)%>%summarize(day=first(start_date),code = first(code)) #get filter data from file ID
    values = data%>%filter(code==tmp$code & start_date<tmp$day & zone==1)%>%ungroup()%>%summarize(median = median(hr,na.rm=T), sd=sd(hr,na.rm=T)) # get mean and sd from all zone 1 data before 'missing data'
    
    idx <- (data$file_ID==id& is.na(data$hr)) # get the indices of the missing HR data 
    
    data[idx,'hr']<-  values$median
    data[idx,'zone']<-1
    # data[idx,'hr']<-  round(rnorm(sum(idx),mean=values$mean,sd=values$sd)) # impute the missing values from a normal distribution
    # data <- data %>% mutate(hr = ifelse(hr<0,abs(hr),hr))
    
    i=i+1
    setTxtProgressBar(pb, i)
  }
  return(data)
  }
