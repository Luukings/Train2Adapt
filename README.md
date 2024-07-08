# Train2Adapt code repository
<hr>
<h2> Publication <h2/>
<hr>

### Abstract 
Background: Performance optimization is a major goal in sports science. However, this remains difficult due to the small samples and large individual variation in physiology and training adaptations. Machine learning (ML) solutions seem promising, but have not been tested for their capability to predict performance in this setting. The aim of this study was to predict 4-km cycling performance following a 12-week training intervention based on ML models with predictors from physiological profiling, individual training load and well-being, and to retrieve the most important predictors. Methods: From 34 recreational male and female cyclists, 27 met criteria for inclusion in the analysis. Comprehensive physiological testing was performed at baseline, including 4-km time-trial performance, maximal oxygen uptake (V̇O2max), pulmonary V̇O2-kinetics, Wingate performance, vertical squat jump, 3D ultrasound imaging and anthropometry. Participants performed a 12-week training intervention, including moderate-intensity endurance training (n=6), polarised endurance training (n=8), concurrent polarised with concentric strength training (n=7) and concurrent polarised with eccentric strength training (n=6). Adaptations were assessed post- intervention. Cycling performance was predicted at baseline, after training and for changes in performance using ML models (generalized linear models (glm), random forest (rf) and principal component regression (pcr) models). Specific techniques were applied to reduce the risk of overfitting and model performance was assessed by R2 and mean absolute error (MAE). Results: Cyclists completed the 4-km time trial with a mean power output of 4.1±0.7 W/kg. Changes in time-trial performance after training were not different between intervention groups (P>0.05), but included substantial inter-individual differences. ML models predicted cycling performance with excellent model performance on unseen data before (R2=0.923, MAE=0.183 W/kg using glm) and after training (R2=0.758, MAE=0.338 W/kg using glm). Absolute changes in performance were more difficult to predict (R2=0.483, MAE=0.191 W/kg using rf). Important predictors included power at V̇O2max, performance V̇O2, ventilatory thresholds and efficiency, but also parameters related to body composition, training impulse, sleep, sickness and well-being. Conclusion: ML models allow accurate predictions of cycling performance based on physiological profiling, individual training load and well-being during a 12-week training intervention, even using small sample sizes, although changes in cycling performance were more difficult to predict.

### Keywords 
Artificial intelligence, ML, AI, concurrent training, cycling, training response, strength training, endurance training, performance optimization 

 <hr>
 
 <h2> Archive </h2>
 
 <hr>
 

<h4> Main script </h4>

The main script is used to perform ML modelling. note that the main script can be run to obtain model results on the pre, post and difference in performance based on the dataset used. This script calls other subfunctions and scripts that are located within the `scripts` subdirectory.

 <hr>
 
 **Description**:                  
 **Authors**:        Luuk Vos [l.vos3@amsterdamumc.nl] Stephan van der Zwaard [s.vander.zwaard@vu.nl]                                                      
 **Date:**           08-07-2024                                                                                                 
 **Version:**        1.0                                                                                                        
 **R.version:**      4.2.2 (2022-11-01)                                                                                        
                                                                                                                          
 **Publication:**         
 doi:                        
 <hr>      
 
<h4> RStudio project </h4>

The RStudio project that is associated with the analysis of this publication.
                                                                                                                          
<h3> data/ </h3>

The `data` subdirectory contains the dataframes required for machine learning modelling. Pre_dataframe is used to predict time trial performance as baseline. Post_dataframe us used to predict time trial performance after training. diff_dataframe is used to predict absolute changes in time trial performance. diff_TT_performance is used for data visualization.

<h3> scripts/ </h3>

The `scripts` subdirectory contains the necessary code for performing the analysis. These are related to the machine learning models, data visualization and results.

<h3> results/ </h3>

The `results` subdirectory contains figures and tables associated with the publication. This folder also contains the supplemental file.

<hr>
