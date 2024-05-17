plot_feature_importance <- function(imp,predict_target) {

# Plot feature importance scores

cbbPalette <- c( "#e9566b",'grey',"#009E73")

ggplot2::ggplot(imp, aes(x=reorder(rowname,feature_imp), y=feature_imp)) +
  geom_segment( data = imp,
                aes(x=rowname, xend=rowname, y=0, yend=feature_imp),
                color='black', linewidth = .5) +
  geom_point(color = "skyblue", size=3)+
  #scale_color_manual(values =cbbPalette)+
  xlab('Variable')+
  ylab('Feature Importance')+
  ggtitle(case_when(predict_target=='delta' ~ 'C) Critical determinants for changes in TT performance',
                    predict_target=='post'  ~ 'B) Critical determinants for TT performance after training',
                    predict_target=='pre'   ~ 'A) Critical determinants for TT performance at baseline')) +
  theme_light() +
  coord_flip() + 
  theme(plot.title.position = "plot")

}