# Renaming of columns

rename_columns <- function(df) {
  
  df <- df %>% rename(Performance_VO2 = TT_VO2_mean,
                     Gross_efficiency = TT_eff,
                     VO2kinetics_tau = tau,
                     VO2kinetics_base = base,
                     VO2kinetics_delay = time_delay,
                     VO2kinetics_amplitude = amplitude,
                     VL_ACSAmax = max_ACSA,
                     VL_Volume = Volume,
                     VL_Lf = mean_Fascicle_length,
                     VL_pennation = mean_Pennation_Angle,
                     LBM = lbm,
                     body_fat_perc = fat_per,
                     ) %>%
              rename_at(.vars = vars(contains('compl_Training')),.funs = ~gsub('compl_Training log','training_compliance',.)) %>% 
              rename_at(.vars = vars(starts_with('Maxtest_')),.funs = ~substr(.,9,nchar(.))) %>% 
              rename_at(.vars = vars(contains('Wingate_Average')),.funs = ~gsub('Wingate_Average','Wingate_mean',.)) %>% 
              rename_at(.vars = vars(contains('Wingate_Min')),.funs = ~gsub('Wingate_Min','Wingate_min',.)) %>% 
              rename_at(.vars = vars(contains('HHB_delta')),  .funs = ~gsub('HHB_delta','deoxy[heme]',.)) %>% 
              rename_at(.vars = vars(contains('O2HB_delta')), .funs = ~gsub('O2HB_delta','oxy[heme]',.)) %>% 
              rename_at(.vars = vars(contains('Wingate_Min')),.funs = ~gsub('Wingate_Min','Wingate_min',.)) %>% 
              rename_at(.vars = vars(contains('Jump_max')),   .funs = ~gsub('Jump_max','Jump',.)) %>% 
              rename_at(.vars = vars(contains('Jump_max')),   .funs = ~gsub('Jump_max','Jump',.)) %>% 
              rename_at(.vars = vars(contains('bicep')),      .funs = ~gsub('bicep','biceps',.)) %>% 
              rename_at(.vars = vars(contains('tripeps')),    .funs = ~gsub('tripeps','triceps',.)) %>% 
              rename_at(.vars = vars(contains('Mean_Lm1')),   .funs = ~gsub('Mean_Lm1','VL_Lm',.)) %>% 
              rename_at(.vars = vars(contains('MRT')),        .funs = ~gsub('MRT','VO2kinetics_MRT',.)) %>% 
              rename_at(.vars = vars(contains('wellness')),   .funs = ~gsub('wellness','well-being',.)) %>% 
              rename_at(.vars = vars(contains('_max')),       .funs = ~gsub('_max','max',.))
  
  return(df)
}