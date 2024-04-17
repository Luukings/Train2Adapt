interpolate_offset <- function(data) {
  

  # Find end time for each test
  offset_data <- data %>% group_by(file_ID) %>% summarise(offset_end = max(offset)) %>% filter(!is.na(offset_end))
  
  offset_rows <- c()
  pb <- txtProgressBar(min = 0, max = nrow(offset_data), style = 3)
  
  for (i in 1:nrow(offset_data)) {
    
    offset_row = data.frame(offset = seq(1,offset_data$offset_end[i],by=1),
                          file_ID = offset_data$file_ID[i])
    if (i==1) {
      offset_rows <- offset_row
    } else {
      offset_rows <- rbind(offset_rows,offset_row)
    }
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  final_data <- offset_rows %>% 
    left_join(data, 
              by=c('offset','file_ID')) %>%
    group_by(file_ID) %>%
    mutate_at(.vars = vars(!one_of('offset','file_ID')),
              .funs = list(~na.locf(.,fromLast=T, na.rm=F,maxgap=120))) %>%
    ungroup()
  
  return(final_data)
  
}