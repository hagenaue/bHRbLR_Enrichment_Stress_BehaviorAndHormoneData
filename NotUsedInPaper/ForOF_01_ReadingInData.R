#Analysis of Angela's Behavioral and Hormonal Data
##Megan Hagenauer, March 10, 2020
##Updated, April 30,2020
##Updated again, May 18, 2020
##Updated again in July & Sept 2020
##Updated again in Feb 2021


#***********************************************************

# 1. Reading in Data: 

setwd("~/Documents/Microarray Gen/Angela_HRLR_EE_Stress/ReDone_OpenFieldData_20201008")

Data<-read.csv("HRLR_EE_Stress_AllBehavData_forR_withNewCORTOxytIL6_SI_OFSDScoresFixed_FixedFormatIDs_TimeOnTop_forFullBehavior2.csv", header=T, stringsAsFactors = F)
#Notes about the dataset:
##1) The IL6 data comes from two separate runs (one run is bHR and one run is bLR) due to problems with the standard curve
##2) The corticosterone was re-done
##3) Several subdatasets (behavior, hormones, USV, social defeat video data) were combined together in excel to make this spreadsheet
##4) Minor issues were fixed in Excel prior to loading in: "L07" and " L07" were made the same format, "nil" was made "NIL", individual variables for the treatments were created.
##5) We also tried to pull out Litter as a variable (maybe - this seems to partially contradict information that Angela gave us recently about the full rat ID #s)
##6) We had already looked at the data extensively as part of the student projects, so many of the issues were already noticed before pulling together this analysis.
##7) The social defeat video data was scored in three generation-based batches. In my first attempt at the analysis, I was missing the data from Generation F56.
##8) There were issues with the open field data and USV data in the July/Sept analyses: 
#####- Angela realized that the fact that the rats were starting out the open field task in the center meant they were sometimes freezing there at the beginning
#####- so she outputted a time-binned (by minute) version of the open field results, but accidentally outputted it from files that hadn't been cleaned of video artifacts
#####- so then she re-outputted it again from the cleaned files. 
#####-For the USVs: we discovered that the two generations were run with completely different recording settings (i.e., basically measuring different USVs) and can't be easily combined
#####-We also discovered that many of the "USV" calls were outside the range of actual calls (lower or higher mean peak frequency) and filtered them out.
#####-So everything related to these variables was re-outputted again for just the proteoglycan animals in 01/2021
##### but then Angela noticed that I was summing across the bins for %time in center instead of averaging, so we fixed it and reoutputted everything one more time for the full dataset and the proteoglycan animals (02/2021)

colnames(Data)

# [1] "Rat_ID"                                                                                                         
# [2] "Litter"                                                                                                         
# [3] "From.Kathryn.or.not...Kathryn.may.not.have.culled.on.P1."                                                       
# [4] "Cage..pair.housed_.if.uneven.last.three.together."                                                              
# [5] "Generation"                                                                                                     
# [6] "Line"                                                                                                           
# [7] "Treatment_group..EC.Enriched.Cage.Control..EE.Environmental.Enrichment..NIL.Standard.Housing..SD.Social.Defeat."
# [8] "Enrichment"                                                                                                     
# [9] "Social_Defeat"                                                                                                  
# [10] "distance_open_field"                                                                                            
# [11] "time_centre_open_field"                                                                                         
# [12] "time_open_arms_EPM"                                                                                             
# [13] "time_social_avoidance_Ethovision"                                                                               
# [14] "time_social_interaction_EthoVision"                                                                             
# [15] "time_other_areas_in_SI_Test_EthoVision"                                                                         
# [16] "time_on_top_of_stimulus_animal_Ethovision"                                                                      
# [17] "time_on_top_of_stimulus_animal_Video"                                                                           
# [18] "time_approaching_stimulus_animal_Video"                                                                         
# [19] "time_other_behav_in_SI_Test_Video"                                                                              
# [20] "cranky_USVs"                                                                                                    
# [21] "happy_USVs"                                                                                                     
# [22] "SD.Dates"                                                                                                       
# [23] "D1.Aggressor.ID"                                                                                                
# [24] "SD.d1.score"                                                                                                    
# [25] "SD.d1.time.caged..min."                                                                                         
# [26] "D2.Aggressor.ID"                                                                                                
# [27] "SD.d2.score"                                                                                                    
# [28] "SD.d2.time.caged..min."                                                                                         
# [29] "D3.Aggressor.ID"                                                                                                
# [30] "SD.d3.score"                                                                                                    
# [31] "SD.d3.time.caged..min."                                                                                         
# [32] "D4.Aggressor.ID"                                                                                                
# [33] "SD.d4.score"                                                                                                    
# [34] "SD.d4.time.caged..min."                                                                                         
# [35] "SUB_DefeatDay1"                                                                                                 
# [36] "AGG_DefeatDay1"                                                                                                 
# [37] "OTHER_DefeatDay1"                                                                                               
# [38] "SUB_DefeatDay2"                                                                                                 
# [39] "AGG_DefeatDay2"                                                                                                 
# [40] "OTHER_DefeatDay2"                                                                                               
# [41] "SUB_DefeatDay3"                                                                                                 
# [42] "AGG_DefeatDay3"                                                                                                 
# [43] "OTHER_DefeatDay3"                                                                                               
# [44] "SUB_DefeatDay4"                                                                                                 
# [45] "AGG_DefeatDay4"                                                                                                 
# [46] "OTHER_DefeatDay4"                                                                                               
# [47] "date_of_sac"                                                                                                    
# [48] "CORT"                                                                                                           
# [49] "testosterone"                                                                                                   
# [50] "Oxytocin..pg.ml."                                                                                               
# [51] "IL.6..pg.ml."                                                                                                   
# [52] "NACC_date_of_dissection"                                                                                        
# [53] "NACC_hemisphere_dissected"                                                                                      
# [54] "NACC_date_of_RNA_extraction"                                                                                    
# [55] "NACC_RNA_conc"                                                                                                  
# [56] "NACC_ratio_260_280"                                                                                             
# [57] "NACC_brain_region"                                                                                              
# [58] "NACC_Sequencing_core_sample_ID"                                                                                 
# [59] "HC_date_of_dissection"                                                                                          
# [60] "HC_hemisphere_dissected"                                                                                        
# [61] "HC_date_of_RNA_extraction"                                                                                      
# [62] "HC_RNA_conc"                                                                                                    
# [63] "HC_ratio_260_280"                                                                                               
# [64] "HC_brain_region"                                                                                                
# [65] "HC_Sequencing_core_sample_ID"                                                                                   
# [66] "OF_Distance_Start.0.01.00"                                                                                      
# [67] "OF_Distance_0.01.00.0.02.00"                                                                                    
# [68] "OF_Distance_0.02.00.0.03.00"                                                                                    
# [69] "OF_Distance_0.03.00.0.04.00"                                                                                    
# [70] "OF_Distance_0.04.00.0.05.00"                                                                                    
# [71] "OF_PercentTimeInCenter_Start.0.01.00"                                                                           
# [72] "OF_PercentTimeInCenter_0.01.00.0.02.00"                                                                         
# [73] "OF_PercentTimeInCenter_0.02.00.0.03.00"                                                                         
# [74] "OF_PercentTimeInCenter_0.03.00.0.04.00"                                                                         
# [75] "OF_PercentTimeInCenter_0.04.00.0.05.00"                                                                         
# [76] "Number.of.calls.noises.detected"                                                                                
# [77] "Number.of.18...32.kHz"                                                                                          
# [78] "Number.of.32...96.kHz"                                                                                          
# [79] "average.max.peak.frequency"                                                                                     
# [80] "average.duration.of.18...32.kHz"                                                                                
# [81] "average.duration.of.32...96.kHz"                                                                                
# [82] "number.of.calls.18...96.kHz"                                                                                    
# [83] "X..18.32.kHz.over.total.recorded.calls.noises"                                                                  
# [84] "X..32.96.kHz.calls.over.total.recorded.calls.noises"                                                            
# [85] "Cranky.USVs_..18.32.kHz.over.18.96.kHz.calls"                                                                   
# [86] "Happy.USVs_..32.96.kHz.calls.over.18.96.kHz.calls"

#Re-naming the variables using concise, consistent names:

##Aside: Angela may want different names as the final labels on figures for the paper - we'll have to deal with that later. 
##This code just makes it so that we can more easily look at our preliminary graphs and results. 

colnames(Data)[7]<-"Treatment_Group"

colnames(Data)[10]<-"OpenField_DistanceTraveled"                                                                                          
colnames(Data)[11]<-"OpenField_Explore_Center"  
colnames(Data)[12]<-"EPM_Explore_Open_Arms"

colnames(Data)[13]<-"SITest_SocialAvoidance_Ethovision"                                                                          
colnames(Data)[14]<-"SITest_SocialInteraction_Ethovision"                                                                        
colnames(Data)[15]<-"SITest_OtherAreas_Ethovision"        
colnames(Data)[16]<-"SITest_TimeOnTop_Ethovision"  
colnames(Data)[17]<-"SITest_TimeOnTop_Video"
colnames(Data)[18]<-"SITest_SocialApproach_Video"
colnames(Data)[19]<-"SITest_OtherBehavior_Video"
colnames(Data)[20]<-"SITest_USVs_Distress_20kHz"
colnames(Data)[21]<-"SITest_USVs_Happy_50kHz"  

colnames(Data)[23]<-"DefeatDay1_AggressorID"
colnames(Data)[24]<-"DefeatDay1_DefeatScore"
colnames(Data)[25]<-"DefeatDay1_TimeCaged"

colnames(Data)[26]<-"DefeatDay2_AggressorID"
colnames(Data)[27]<-"DefeatDay2_DefeatScore"
colnames(Data)[28]<-"DefeatDay2_TimeCaged"

colnames(Data)[29]<-"DefeatDay3_AggressorID"
colnames(Data)[30]<-"DefeatDay3_DefeatScore"
colnames(Data)[31]<-"DefeatDay3_TimeCaged"

colnames(Data)[32]<-"DefeatDay4_AggressorID"
colnames(Data)[33]<-"DefeatDay4_DefeatScore"
colnames(Data)[34]<-"DefeatDay4_TimeCaged"


colnames(Data)[35]<- "DefeatDay1_Submissive"                                                                                              
colnames(Data)[36]<- "DefeatDay1_Aggressive"                                                                                              
colnames(Data)[37]<- "DefeatDay1_OtherBehavior"  

colnames(Data)[38]<- "DefeatDay2_Submissive"                                                                                              
colnames(Data)[39]<- "DefeatDay2_Aggressive"                                                                                              
colnames(Data)[40]<- "DefeatDay2_OtherBehavior"                                                                                            
colnames(Data)[41]<- "DefeatDay3_Submissive"                                                                                              
colnames(Data)[42]<- "DefeatDay3_Aggressive"                                                                                              
colnames(Data)[43]<- "DefeatDay3_OtherBehavior"                                                                                           

colnames(Data)[44]<- "DefeatDay4_Submissive"                                                                                              
colnames(Data)[45]<- "DefeatDay4_Aggressive"                                                                                              
colnames(Data)[46]<- "DefeatDay4_OtherBehavior"                                                                                           

colnames(Data)[48]<-"Corticosterone"
colnames(Data)[49]<-"Testosterone"
colnames(Data)[50]<-"Oxytocin"
colnames(Data)[51]<-"IL6"


str(Data)
#'data.frame':	142 obs. of  86 variables:

#Sets up Generation, Line, Enrichment, and Social_Defeat as factors.
Data$Generation<-as.factor(Data$Generation)
Data$Line<-as.factor(Data$Line)
Data$Enrichment<-as.factor(Data$Enrichment)
Data$Social_Defeat<-as.factor(Data$Social_Defeat)

str(Data)

# $ Generation                                              : Factor w/ 3 levels "F49","F53","F56": 1 1 1 1 1 1 1 1 1 1 ...
# $ Line                                                    : Factor w/ 2 levels "bHR","bLR": 1 1 1 1 1 1 1 1 1 1 ...
# $ Treatment_Group                                         : chr  "HR EE" "HR EE" "HR EE" "HR EE" ...
# $ Enrichment                                              : Factor w/ 3 levels "EC","EE","NIL": 2 2 2 2 2 2 3 3 3 3 ...
# $ Social_Defeat                                           : Factor w/ 2 levels "NIL","SD": 1 1 1 1 1 1 2 2 2 2 ...

#Sets up the appropriate reference levels for the "Generation" and "Enrichment" factors
Data$Generation<-relevel(Data$Generation, ref="F53")
Data$Enrichment<-relevel(Data$Enrichment, ref="NIL")
