# Renaming of columns

rename_columns <- function(df) {
  
  df <- df %>% rename_at(.vars = vars(contains('TT_VO2_mean')),.funs = ~gsub('TT_VO2_mean','Performance_VO2',.)) %>% 
               rename_at(.vars = vars(contains('TT_eff')),.funs = ~gsub('TT_eff','Gross_efficiency',.)) %>% 
               rename_at(.vars = vars(contains('tau')),.funs = ~gsub('tau','VO2kinetics_tau',.)) %>% 
               rename_at(.vars = vars(contains('base')),.funs = ~gsub('base','VO2kinetics_base',.)) %>% 
               rename_at(.vars = vars(contains('time_delay')),.funs = ~gsub('time_delay','VO2kinetics_delay',.)) %>% 
               rename_at(.vars = vars(contains('amplitude')),.funs = ~gsub('amplitude','VO2kinetics_amplitude',.)) %>% 
               rename_at(.vars = vars(contains('max_ACSA')),.funs = ~gsub('max_ACSA','VL_ACSAmax',.)) %>% 
               rename_at(.vars = vars(contains('Volume')),.funs = ~gsub('Volume','VL_Volume',.)) %>% 
               rename_at(.vars = vars(contains('mean_Fascicle_length')),.funs = ~gsub('mean_Fascicle_length','VL_Lf',.)) %>% 
               rename_at(.vars = vars(contains('mean_Pennation_Angle')),.funs = ~gsub('mean_Pennation_Angle','VL_pennation',.)) %>% 
               rename_at(.vars = vars(contains('lbm')),    .funs = ~gsub('lbm','LBM',.)) %>% 
               rename_at(.vars = vars(contains('fat_per')),.funs = ~gsub('fat_per','body_fat_perc',.)) %>% 
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
               rename_at(.vars = vars(contains('MRT')),        .funs = ~gsub('MRT','VO2kinetics_MRT',.)) %>% 
               rename_at(.vars = vars(contains('wellness')),   .funs = ~gsub('wellness','well-being',.)) %>% 
               rename_at(.vars = vars(contains('_max')),       .funs = ~gsub('_max','max',.))
  
  return(df)
}