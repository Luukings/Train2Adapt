Get_HR_zones <- function(data,zones) {
  
  HR_zones <- c()
  pb <- txtProgressBar(min = 0, max = nrow(zones), style = 3)
  for (i in 1:nrow(zones)){
    
    tmp <- data%>%
      filter(code == zones$PPcode[i])
    
    tmp2 <- zones%>%filter(PPcode==zones$PPcode[i])
    
    tmp3 <- tmp%>%mutate(zone = case_when(hr<=tmp2$`VT1 HR (slagen/min)` ~ 1,
                                          hr>tmp2$`VT1 HR (slagen/min)` & hr<tmp2$`VT2 HR (slagen/min)`~ 2,
                                          hr>=tmp2$`VT2 HR (slagen/min)` ~3),
                         hr_per = hr/tmp2$`HRmax (slagen/min)`)
    
    if(length(HR_zones) == 0){
      HR_zones<- tmp3
    }
    else{
      HR_zones<-bind_rows(HR_zones,tmp3)
    }
    setTxtProgressBar(pb, i)
  }

  close(pb)
  return (HR_zones)
}