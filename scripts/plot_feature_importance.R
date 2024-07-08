plot_feature_importance <- function(imp,predict_target) {

# Plot feature importance scores

cbbPalette <- c( "#e9566b",'grey',"#009E73")

ggplot2::ggplot(imp, aes(x=reorder(rowname,feature_imp), y=feature_imp)) +
  geom_segment( data = imp,
                aes(x=rowname, xend=rowname, y=0, yend=feature_imp),
                color='black', linewidth = .5) +
  geom_point(color = "skyblue", size=3)+
  #scale_color_manual(values =cbbPalette)+
  xlab(case_when(predict_target=='delta' ~ '',
                 predict_target=='post'  ~ '',
                 predict_target=='pre'   ~ 'Predictors'))+
  ylab('Feature Importance')+
  ggtitle(case_when(predict_target=='delta' ~ 'C) Changes in performance',
                    predict_target=='post'  ~ 'B) Performance after training',
                    predict_target=='pre'   ~ 'A) Performance at baseline')) +
  scale_y_continuous(n.breaks = 4) +
  theme_classic() +
  coord_flip() + 
  theme(plot.title.position = "plot") 

}