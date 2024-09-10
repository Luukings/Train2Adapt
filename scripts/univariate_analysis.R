univariate_analysis <- function(data,target) {
  # Test univariate correlation between feature and TT_performance
  
  cor_data <- data[[target]]
  data <- data%>%select(-target)
  # Test for normality 
  p_normality <- c()
  for (c in 1:dim(data)[2]) {
    if (data[1,c] == cor_data[1]) next

    norm_stats <- shapiro.test(data[,c])
    p_normality <- cbind(p_normality,norm_stats$p.value)
  }
  colnames(p_normality) <- colnames(data)[1:dim(data)[2]]
  
  # Test correlation between feature and TT_performance by Pearson (parametric) and Spearman (non-parametric)
  uni_cor <- data.frame()

  for (c in 1:dim(data)[2]) {
    if (data[1,c] == cor_data[1]) next
    
    if (p_normality[,colnames(data)[c]] < 0.05) { 
      cor_test  <- cor.test(data[,c],cor_data,use = 'pairwise.complete.obs',method = 'spearman')
      uni_cor <- rbind(uni_cor,c(colnames(data)[c],cor_test$estimate,cor_test$p.value,"Spearman"))
    } else {
      cor_test  <- cor.test(data[,c],cor_data,use = 'pairwise.complete.obs',method = 'pearson')
      uni_cor <-rbind(uni_cor,c(colnames(data)[c],cor_test$estimate,cor_test$p.value,"Pearson"))
    }
    uni_cor <- data.frame(uni_cor)
    colnames(uni_cor) <- c("variables","correlation","p-value","test")
    uni_cor <- uni_cor%>%arrange(correlation)
  }
  return(uni_cor)
}