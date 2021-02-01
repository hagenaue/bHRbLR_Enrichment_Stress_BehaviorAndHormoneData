#Analysis of Angela's Behavioral and Hormonal Data
##Megan Hagenauer, March 10, 2020
##Updated, April 30,2020
##Updated again, May 18, 2020
## Subsetted and tweaked to make it fit the results for just the Proteoglycan ISH animals Sept 14 2020

# Updated to use the new Proteoglycan dataset Nov 12 2020 (Liam C. Thew Forrester)


#***********************************************************

# 1. Reading in Data: 

setwd("C:/Users/Frosty/Desktop/Research/Research during Summer of 2020/R Data/Angela_HRLR_StressEnrichData")

Data<-read.csv("HRLR_EE_Stress_AllBehavData_forR_withNewCORTOxytIL6_SI_OFSDScoresFixed_FixedFormatIDs_TimeOnTop_forProteoglycan2.csv", header=T, stringsAsFactors = F)
#Notes about the dataset:
##1) The IL6 data comes from two separate runs (one run is bHR and one run is bLR) due to problems with the standard curve
##2) The corticosterone was re-done
##3) Several subdatasets (behavior, hormones, USV, social defeat video data) were combined together in excel to make this spreadsheet
##4) Minor issues were fixed in Excel prior to loading in: "L07" and " L07" were made the same format, "nil" was made "NIL", individual variables for the treatments were created.
##5) We also tried to pull out Litter as a variable (maybe - this seems to partially contradict information that Angela gave us recently about the full rat ID #s)
##6) We had already looked at the data extensively as part of the student projects, so many of the issues were already noticed before pulling together this analysis.
##7) The social defeat video data was scored in three generation-based batches. In my first attempt at the analysis, I was missing the data from Generation F56.


colnames(Data)

# [1] "?..Rat_ID"                                                                                                      
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

#Re-naming the variables using concise, consistent names:

##Aside: Angela may want different names as the final labels on figures for the paper - we'll have to deal with that later. 
##This code just makes it so that we can more easily look at our preliminary graphs and results. 
                                                                                         
colnames(Data)[1]<-"Rat_ID"

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
#'data.frame':	142 obs. of  51 variables:



ProteoglycanISH_ForComparisonWBehav<-read.csv("ProteoglycanISH_ForComparisonWBehav.csv", header=T, stringsAsFactors = F)
colnames(ProteoglycanISH_ForComparisonWBehav)
# [1] "Rat_ID"                                                                                                         
# [2] "Generation"                                                                                                     
# [3] "Line"                                                                                                           
# [4] "Treatment_group..EC.Enriched.Cage.Control..EE.Environmental.Enrichment..NIL.Standard.Housing..SD.Social.Defeat."
# [5] "InISH"                                                                                                          
# [6] "Animal.ID"                                                                                                      
# [7] "Generation.1"                                                                                                   
# [8] "bred.line"                                                                                                      
# [9] "treatment.group"                                                                                                
# [10] "Gpc1_CA1"                                                                                                       
# [11] "Gpc1_CA2"                                                                                                       
# [12] "Gpc1_CA3"                                                                                                       
# [13] "Gpc1_DG"                                                                                                        
# [14] "Sdc4_CA1"                                                                                                       
# [15] "Sdc4_CA2"                                                                                                       
# [16] "Sdc4_CA3"                                                                                                       
# [17] "Sdc4_DG"                                                                                                        
# [18] "OrderInOriginalForDoubleChecking"                                                                               
# [19] "OrderInMasterSpreadsheet" 

#Double-checking that the order is the same:
cbind(Data$Rat_ID, ProteoglycanISH_ForComparisonWBehav$Rat_ID)
sum(Data$Rat_ID==ProteoglycanISH_ForComparisonWBehav$Rat_ID)
#[1] 142
length(Data$Rat_ID)
#[1] 142
#Looks good. :)

table(ProteoglycanISH_ForComparisonWBehav$InISH)
#     Y 
# 94 48 

sum(ProteoglycanISH_ForComparisonWBehav$InISH=="Y")
#[1] 48

#Subsetting:

Data<-Data[ProteoglycanISH_ForComparisonWBehav$InISH=="Y",]
str(Data)
#'data.frame':	48 obs. of  58 variables:
#'
ProteoglycanISH_ForComparisonWBehav<-ProteoglycanISH_ForComparisonWBehav[ProteoglycanISH_ForComparisonWBehav$InISH=="Y",]
str(ProteoglycanISH_ForComparisonWBehav)
#'data.frame':	48 obs. of  19 variables:

#********************************

#Sets up Generation, Line, Enrichment, and Social_Defeat as factors.
Data$Generation<-as.factor(Data$Generation)
Data$Line<-as.factor(Data$Line)
Data$Enrichment<-as.factor(Data$Enrichment)
Data$Social_Defeat<-as.factor(Data$Social_Defeat)

str(Data)

#Sets up the appropriate reference levels for the "Generation" and "Enrichment" factors
Data$Generation<-relevel(Data$Generation, ref="F49")
Data$Enrichment<-relevel(Data$Enrichment, ref="NIL")

str(Data)

# $ Generation                                              : Factor w/ 3 levels "F49","F53","F56": 1 1 1 1 1 1 1 1 1 1 ...
# $ Line                                                    : Factor w/ 2 levels "bHR","bLR": 1 1 1 1 1 1 1 1 1 1 ...
# $ Treatment_Group                                         : chr  "HR EE" "HR EE" "HR EE" "HR EE" ...
# $ Enrichment                                              : Factor w/ 2 levels "EE","NIL": 1 1 1 1 1 1 2 2 2 2 ...
# $ Social_Defeat                                           : Factor w/ 2 levels "NIL","SD": 1 1 1 1 1 1 2 2 2 2 ...

###################################


# 2. Determining which variables may require transformations for running stats (and/or non-parametric statistics):


#I made histograms to determine whether the distributions for the variables were highly skewed. 
### Note for follow-up: We should come back to these and output nicer, publishable versions to include in the supplementary section.

hist(Data$Testosterone, breaks=20)
#Highly skewed, Needs log transform
Data$Testosterone
#No zeros
hist(log2(Data$Testosterone), breaks=20)
Data$Testosterone_Log2<-log2(Data$Testosterone)

hist(Data$Corticosterone, breaks=20)
#Highly skewed, Needs log transform
Data$Corticosterone
#No zeros
hist(log2(Data$Corticosterone), breaks=20)

Data$Corticosterone_Log2<-log2(Data$Corticosterone)

hist(Data$Oxytocin, breaks=20)
#Skewed, but not as dramatically.

hist(Data$IL6, breaks=20)
#Skewed, but not as dramatically... and comes from two batches

hist(Data$OpenField_DistanceTraveled, breaks=20)
#Highly skewed, Log transform?
Data$OpenField_DistanceTraveled
#No zeros
hist(log2(Data$OpenField_DistanceTraveled), breaks=20)
Data$OpenField_DistanceTraveled_Log2<-log2(Data$OpenField_DistanceTraveled)

hist(Data$OpenField_Explore_Center, breaks=20)
#Highly skewed, Log transform?
Data$OpenField_Explore_Center
#Includes zeros - can't be directly log transformed
hist(log2(Data$OpenField_Explore_Center+1), breaks=20)
Data$OpenField_Explore_Center_Log2<-log2(Data$OpenField_Explore_Center+1)

hist(Data$SITest_SocialAvoidance_Ethovision, breaks=20)
#Highly skewed, Log transform?
Data$SITest_SocialAvoidance_Ethovision
#Includes zeros - can't be directly log transformed
hist(log2(Data$SITest_SocialAvoidance_Ethovision+1), breaks=20)
Data$SITest_SocialAvoidance_Ethovision_Log2<-log2(Data$SITest_SocialAvoidance_Ethovision+1)

hist(Data$SITest_OtherAreas_Ethovision, breaks=20)
#skewed
hist(log2(Data$SITest_OtherAreas_Ethovision), breaks=20)
#more skewed, other direction

hist(Data$SITest_SocialInteraction_Ethovision, breaks=20)

hist(Data$EPM_Explore_Open_Arms, breaks=20)
#Highly skewed, Log transform?
Data$EPM_Explore_Open_Arms
#Includes zeros - can't be directly log transformed
hist(log2(Data$EPM_Explore_Open_Arms+1), breaks=20)
Data$EPM_Explore_Open_Arms_Log2<-log2(Data$EPM_Explore_Open_Arms+1)

#These are all skewed too... Let's wait until we figure out how best to deal with the missing data...
hist(Data$DefeatDay1_Submissive, breaks=20)
hist(log2(Data$DefeatDay1_Submissive+1), breaks=20)
hist(Data$DefeatDay2_Submissive, breaks=20)
hist(Data$DefeatDay3_Submissive, breaks=20)
hist(Data$DefeatDay4_Submissive, breaks=20)
#Skewed all days

hist(Data$DefeatDay1_Aggressive, breaks=20)
#Also crazy skewed
hist(Data$DefeatDay2_Aggressive, breaks=20)
hist(Data$DefeatDay3_Aggressive, breaks=20)
hist(Data$DefeatDay4_Aggressive, breaks=20)
#skewed for all days

hist(Data$DefeatDay1_OtherBehavior, breaks=20)
#Skewed, but the other direction
hist(Data$DefeatDay2_OtherBehavior, breaks=20)
#Skewed, but the other direction
hist(Data$DefeatDay3_OtherBehavior, breaks=20)
hist(Data$DefeatDay4_OtherBehavior, breaks=20)
#Not skewed for all days

#I'm just going to go ahead and log transform all of these...

Data$DefeatDay1_Submissive_Log2<-log2(Data$DefeatDay1_Submissive+1)
Data$DefeatDay2_Submissive_Log2<-log2(Data$DefeatDay2_Submissive+1)
Data$DefeatDay3_Submissive_Log2<-log2(Data$DefeatDay3_Submissive+1)
Data$DefeatDay4_Submissive_Log2<-log2(Data$DefeatDay4_Submissive+1)

Data$DefeatDay1_Aggressive_Log2<-log2(Data$DefeatDay1_Aggressive+1)
Data$DefeatDay2_Aggressive_Log2<-log2(Data$DefeatDay2_Aggressive+1)
Data$DefeatDay3_Aggressive_Log2<-log2(Data$DefeatDay3_Aggressive+1)
Data$DefeatDay4_Aggressive_Log2<-log2(Data$DefeatDay4_Aggressive+1)


hist(Data$DefeatDay1_DefeatScore)
#Not really a continuous variable - discrete, range 1-5
table(Data$DefeatDay1_DefeatScore)
# 1  2  4  5 
# 1  4 59  7 
table(Data$DefeatDay2_DefeatScore)
# 1  2  4  5 
# 2 13 51  5 
table(Data$DefeatDay3_DefeatScore)
# 2  4  5 
# 17 43 11 
table(Data$DefeatDay4_DefeatScore)
# 1  2  4  5 
# 2 14 49  6 

hist(Data$DefeatDay1_TimeCaged, breaks=20)
#highly skewed, lots of near 0's
#Nothing is exactly 0 though, so can be log transformed
hist(log2(Data$DefeatDay1_TimeCaged), breaks=20)
#better
Data$DefeatDay1_TimeCaged_Log2<-log2(Data$DefeatDay1_TimeCaged)
hist(Data$DefeatDay2_TimeCaged, breaks=20)
#bimodal - there are a bunch of animals with 15s now
hist(Data$DefeatDay3_TimeCaged, breaks=20)
#bimodal- there are a bunch of animals with 15s now
hist(Data$DefeatDay4_TimeCaged, breaks=20)
#bimodal- there are a bunch of animals with 15s now


hist(Data$SITest_TimeOnTop_Ethovision, breaks=20)
#Extremely skewed
Data$SITest_TimeOnTop_Ethovision
#Includes zeros - can't be directly log transformed
hist(log2(Data$SITest_TimeOnTop_Ethovision+1), breaks=20)
#Still extemely skewed - its a floor effect. Many 0s. Log transformation only helps a little.
Data$SITest_TimeOnTop_Ethovision_Log2<-log2(Data$SITest_TimeOnTop_Ethovision+1)


hist(Data$SITest_TimeOnTop_Video, breaks=20)
#Very skewed
Data$SITest_TimeOnTop_Video
#Includes zeros - can't be directly log transformed
hist(log2(Data$SITest_TimeOnTop_Video+1), breaks=20)
#less skewed except for the huge floor effect.
Data$SITest_TimeOnTop_Video_Log2<-log2(Data$SITest_TimeOnTop_Video+1)

hist(Data$SITest_SocialApproach_Video, breaks=20)
#Ok
hist(Data$SITest_OtherBehavior_Video, breaks=20)
#Ok, little skewed the other direction.


hist(Data$SITest_USVs_Distress_20kHz, breaks=20)
#Highly skewed, Log transform?
Data$SITest_USVs_Distress_20kHz
#No zeros
hist(log2(Data$SITest_USVs_Distress_20kHz), breaks=20)
#Skewed the other direction. Probably not worth it.

hist(Data$SITest_USVs_Happy_50kHz, breaks=20)
#Not particularly skewed

####################################################



#The sample size is much more limited in this dataset than in the full dataset. Let's take a quick look to see which variables we can actually work with:

table(Data$Generation, Data$Treatment_Group)
#       HR EE HR EE + SD HR NIL HR NIL + SD LR EE LR EE + SD LR NIL LR NIL + SD
# F49     6          6      6           4     6          6      2           4
# F53     0          0      0           0     0          0      2           0
# F56     0          0      0           2     0          0      2           2

#Generation is unbalanced. Most subjects are F49, with a small handful of NIL animals from later generations.
#This will make it very difficult to analyze any of the variables with generation-related batch effects (e.g. USVs)

#Also important: **There aren't any EC/SE animals in this dataset**

table(is.na(Data$OpenField_DistanceTraveled), Data$Treatment_Group)

#         HR EE HR EE + SD HR NIL HR NIL + SD LR EE LR EE + SD LR NIL LR NIL + SD
# FALSE     6          6      6           6     6          6      6           6

table(is.na(Data$EPM_Explore_Open_Arms), Data$Treatment_Group)
#       HR EE HR EE + SD HR NIL HR NIL + SD LR EE LR EE + SD LR NIL LR NIL + SD
# FALSE     6          6      6           6     6          6      6           6


table(is.na(Data$SITest_SocialAvoidance_Ethovision), Data$Treatment_Group)
#       HR EE HR EE + SD HR NIL HR NIL + SD LR EE LR EE + SD LR NIL LR NIL + SD
# FALSE     6          6      6           6     6          6      6           6


#All animals have open field, EPM data, and social interaction ethovision data.

table(is.na(Data$SITest_TimeOnTop_Video), Data$Treatment_Group)

#       HR EE HR EE + SD HR NIL HR NIL + SD LR EE LR EE + SD LR NIL LR NIL + SD
# FALSE     0          0      0           2     0          0      4           2
# TRUE      6          6      6           4     6          6      2           4

#Very few animals have video analysis data.

table(is.na(Data$DefeatDay1_TimeCaged), Data$Treatment_Group)

#         HR EE HR EE + SD HR NIL HR NIL + SD LR EE LR EE + SD LR NIL LR NIL + SD
# FALSE     0          6      0           6     0          6      0           6
# TRUE      6          0      6           0     6          0      6           0

#We do have time caged and social defeat score data for the defeated animals (total n=24)

table(is.na(Data$DefeatDay1_Submissive), Data$Treatment_Group)

#         HR EE HR EE + SD HR NIL HR NIL + SD LR EE LR EE + SD LR NIL LR NIL + SD
# FALSE     0          6      0           6     0          6      0           6
# TRUE      6          0      6           0     6          0      6           0

table(is.na(Data$DefeatDay2_Submissive), Data$Treatment_Group)
#         HR EE HR EE + SD HR NIL HR NIL + SD LR EE LR EE + SD LR NIL LR NIL + SD
# FALSE     0          6      0           6     0          6      0           5
# TRUE      6          0      6           0     6          0      6           1

table(is.na(Data$DefeatDay3_Submissive), Data$Treatment_Group)

#         HR EE HR EE + SD HR NIL HR NIL + SD LR EE LR EE + SD LR NIL LR NIL + SD
# FALSE     0          0      0           5     0          0      0           6
# TRUE      6          6      6           1     6          6      6           0

table(is.na(Data$DefeatDay4_Submissive), Data$Treatment_Group)

#         HR EE HR EE + SD HR NIL HR NIL + SD LR EE LR EE + SD LR NIL LR NIL + SD
# FALSE     0          6      0           2     0          6      0           2
# TRUE      6          0      6           4     6          0      6           4

#We have social defeat video analysis for all animals on day 1, almost all animals days 2 &3, and about half of the animals on day 4.

table(is.na(Data$Corticosterone), Data$Treatment_Group)

#         HR EE HR EE + SD HR NIL HR NIL + SD LR EE LR EE + SD LR NIL LR NIL + SD
# FALSE     0          0      0           2     0          0      4           2
# TRUE      6          6      6           4     6          6      2           4

#Not enough hormone data to do a comparison.



#####################################################
# 3. Dealing with missing time series data:

# We have missing time series data within several of these variables (video scored behaviors during defeat), so we should derive a fit:

#*************

#I made a new data frame with the defeat data in long version for time-series analyses:


#Note: This is code that I needed to tweak to make it work for this subsetted data:

length(Data$Rat_ID)
#[1] 48
#dataset has 48 subjects:
48*4
#[1] 192

DefeatDays_LongVersion<-data.frame(Day=numeric(length=192), RatID=character(length=192), Generation=factor(character(length=192)), Line=factor(character(length=192)), Enrichment=factor(character(length=192)), Social_Defeat=factor(character(length=192)), AggressorID=character(length=192), TimeCaged=numeric(length=192), DefeatScore=integer(length=192), Submissive=numeric(length=192), Aggressive=numeric(length=192), OtherBehavior=numeric(length=192), Submissive_Log2=numeric(length=192), Aggressive_Log2=numeric(length=192))

DefeatDays_LongVersion$Day<-c(rep(0, length(Data$DefeatDay1_TimeCaged_Log2)), rep(1, length(Data$DefeatDay1_TimeCaged_Log2)), rep(2, length(Data$DefeatDay1_TimeCaged_Log2)), rep(3, length(Data$DefeatDay1_TimeCaged_Log2)))

DefeatDays_LongVersion$RatID<-rep(Data$Rat_ID, 4)

DefeatDays_LongVersion$Generation<-rep(Data$Generation, 4)

DefeatDays_LongVersion$Line<-rep(Data$Line, 4)

DefeatDays_LongVersion$Enrichment<-rep(Data$Enrichment, 4)

DefeatDays_LongVersion$Social_Defeat<-rep(Data$Social_Defeat, 4)

DefeatDays_LongVersion$AggressorID<-c(Data$DefeatDay1_AggressorID, Data$DefeatDay2_AggressorID, Data$DefeatDay3_AggressorID, Data$DefeatDay4_AggressorID)

DefeatDays_LongVersion$TimeCaged<-c(Data$DefeatDay1_TimeCaged, Data$DefeatDay2_TimeCaged, Data$DefeatDay3_TimeCaged, Data$DefeatDay4_TimeCaged)

DefeatDays_LongVersion$DefeatScore<-c(Data$DefeatDay1_DefeatScore, Data$DefeatDay2_DefeatScore, Data$DefeatDay3_DefeatScore, Data$DefeatDay4_DefeatScore)

DefeatDays_LongVersion$Submissive<-c(Data$DefeatDay1_Submissive, Data$DefeatDay2_Submissive, Data$DefeatDay3_Submissive, Data$DefeatDay4_Submissive)

DefeatDays_LongVersion$Submissive_Log2<-c(Data$DefeatDay1_Submissive_Log2, Data$DefeatDay2_Submissive_Log2, Data$DefeatDay3_Submissive_Log2, Data$DefeatDay4_Submissive_Log2)

DefeatDays_LongVersion$Aggressive<-c(Data$DefeatDay1_Aggressive, Data$DefeatDay2_Aggressive, Data$DefeatDay3_Aggressive, Data$DefeatDay4_Aggressive)

DefeatDays_LongVersion$Aggressive_Log2<-c(Data$DefeatDay1_Aggressive_Log2, Data$DefeatDay2_Aggressive_Log2, Data$DefeatDay3_Aggressive_Log2, Data$DefeatDay4_Aggressive_Log2)

DefeatDays_LongVersion$OtherBehavior<-c(Data$DefeatDay1_OtherBehavior, Data$DefeatDay2_OtherBehavior, Data$DefeatDay3_OtherBehavior, Data$DefeatDay4_OtherBehavior)

# DefeatDays_LongVersion$TimeCaged_Log2<-c(Data$DefeatDay1_TimeCaged_Log2, Data$DefeatDay2_TimeCaged_Log2, Data$DefeatDay3_TimeCaged_Log2, Data$DefeatDay4_TimeCaged_Log2)



str(DefeatDays_LongVersion)
# 'data.frame':	192 obs. of  14 variables:
#   $ Day            : num  0 0 0 0 0 0 0 0 0 0 ...
# $ RatID          : chr  "H01-2 A1" "H01-2 A2" "H01-2 B1" "H01-2 B2" ...
# $ Generation     : Factor w/ 3 levels "F49","F53","F56": 1 1 1 1 1 1 1 1 1 1 ...
# $ Line           : Factor w/ 2 levels "bHR","bLR": 1 1 1 1 1 1 1 1 1 1 ...
# $ Enrichment     : Factor w/ 2 levels "NIL","EE": 2 2 2 2 2 2 1 1 1 1 ...
# $ Social_Defeat  : Factor w/ 2 levels "NIL","SD": 1 1 1 1 1 1 2 2 2 2 ...
# $ AggressorID    : chr  NA NA NA NA ...
# $ TimeCaged      : num  NA NA NA NA NA NA 0.5 15 3.7 1.88 ...
# $ DefeatScore    : int  NA NA NA NA NA NA 4 2 4 4 ...
# $ Submissive     : num  NA NA NA NA NA ...
# $ Aggressive     : num  NA NA NA NA NA ...
# $ OtherBehavior  : num  NA NA NA NA NA ...
# $ Submissive_Log2: num  NA NA NA NA NA ...
# $ Aggressive_Log2: num  NA NA NA NA NA ...


DefeatDays_LongVersion$Day_CenteredOn4<-DefeatDays_LongVersion$Day-3


#Subsetting:
DefeatDays_LongVersion_VideoRecorded<-DefeatDays_LongVersion[is.na(DefeatDays_LongVersion$Submissive)==F,]                                                 

DefeatDays_LongVersion_VideoRecorded$Submissive

DefeatDays_LongVersion_OnlyDefeated<-DefeatDays_LongVersion[is.na(DefeatDays_LongVersion$TimeCaged)==F,]

DefeatDays_LongVersion_OnlyDefeated$TimeCaged


#For analysis purposes, it might be helpful to set the intercept as Day4, since we are missing Day4 information for some individuals and that day is most likely to reflect the cumulative effects of social defeat.
DefeatDay<-c(-3,-2,-1,0)

#********************************

#Time Series Data: Submissive_Log2

DefeatDay<-c(-3,-2,-1,0)
#set DefeatDay as vector


#Actual code fitting an intercept and slope to the time series data for each individual subject (rat):

Submissive_Log2_Matrix<-cbind(Data$DefeatDay1_Submissive_Log2, Data$DefeatDay2_Submissive_Log2, Data$DefeatDay3_Submissive_Log2, Data$DefeatDay4_Submissive_Log2)
#cbind = combine defeat day data for submissive data with log2 transformation
#rename combined data from cbind function
str(Submissive_Log2_Matrix)
#output the combined data


#A quick illustration of what a line (intercept & slope) fit to the time series data for an individual rat looks like:
plot(Submissive_Log2_Matrix[7,]~DefeatDay)
#plot defeat day on x axis and submissive data on y axis with log2 transformation, 2 points
FitLine<-lm(Submissive_Log2_Matrix[7,]~DefeatDay)
#set linear model for submissive data with log2 transformation as 
##dependent and defeat day as independent variable
abline(FitLine)
#add best fit line from linear model through the plot created above
FitLine[[1]][1]
#get intercept
FitLine[[1]][2]
#get slope 

#Creates empty numeric vectors the same length as a column in the data matrix (i.e., length=number of subjects) to store results
Data$SubmissiveLog2_Intercept<-numeric(length(Submissive_Log2_Matrix[,1]))
#makes combined data created above numeric vector
#rename numeric data for intercept
Data$SubmissiveLog2_Slope<-numeric(length(Submissive_Log2_Matrix[,1]))
#rename numeric data for slope
#sets length of vector

  Data$SubmissiveLog2_Slope<-numeric(length(Submissive_Log2_Matrix[,1]))

for(i in c(1:length(Submissive_Log2_Matrix[,1]))){
  if(sum(is.na(Submissive_Log2_Matrix[i,]))<3){
    FitLine<-lm(Submissive_Log2_Matrix[i,]~DefeatDay)
    Data$SubmissiveLog2_Intercept[i]<-FitLine[[1]][[1]]
    Data$SubmissiveLog2_Slope[i]<-FitLine[[1]][[2]]
  }else{
    Data$SubmissiveLog2_Intercept[i]<-NA 
    Data$SubmissiveLog2_Slope[i]<-NA
  }
}
#Loops over time series data for all subjects (rats) in the dataset:
#is.na implies missing values
#There needs to be fewer than 3 NA (i.e. at least 2 data points) so that we can fit intercept and slope
#if this is true:
## then set data to linear model with submissive data as 
###dependent variable and defeat day as independent variable
#Set both the intercept and the slope to the linear model
#if there are too many missing values in the combined data for defeat days, 
##set missing elements to NA (remove from data set)
#for loop runs the if else function for each value within the vector
##of submissive data with log2 transformation

#********************************

#Time Series Data: Aggressive_Log2

Aggressive_Log2_Matrix<-cbind(Data$DefeatDay1_Aggressive_Log2, Data$DefeatDay2_Aggressive_Log2, Data$DefeatDay3_Aggressive_Log2, Data$DefeatDay4_Aggressive_Log2)
#combine defeat days for aggressive data with log2 transformation
str(Aggressive_Log2_Matrix)

#Creates empty numeric vectors the same length as a column in the data matrix (i.e., length=number of subjects) to store results
Data$AggressiveLog2_Intercept<-numeric(length(Aggressive_Log2_Matrix[,1]))
#make aggressive intercept data with log2 transformarion numeric vector and rename
Data$AggressiveLog2_Slope<-numeric(length(Aggressive_Log2_Matrix[,1]))
#make aggreeive slope data with log2 transmormation numeric vector and rename

  Data$AggressiveLog2_Slope<-numeric(length(Aggressive_Log2_Matrix[,1]))


for(i in c(1:length(Aggressive_Log2_Matrix[,1]))){
  if(sum(is.na(Aggressive_Log2_Matrix[i,]))<3){
    FitLine<-lm(Aggressive_Log2_Matrix[i,]~DefeatDay)
    Data$AggressiveLog2_Intercept[i]<-FitLine[[1]][[1]]
    Data$AggressiveLog2_Slope[i]<-FitLine[[1]][[2]]
  }else{
    Data$AggressiveLog2_Intercept[i]<-NA 
    Data$AggressiveLog2_Slope[i]<-NA
  }
}
#Loops over time series data for all subjects (rats) in the dataset:
#is.na implies missing values
#There needs to be fewer than 3 NA (i.e. at least 2 data points) so that we can fit intercept and slope
#if this is true:
## then set data to linear model with aggressive data 
###as dependent variable and defeat day as independent variable
#Set intercept and slope to linear model

#if there are missing values from the data, remove the missing values
#for loop runs the if else function for each value within the vector
##of aggressive data with log2 transformation
=======
  #if there are too many missing values from the data, make output to NA

Data$AggressiveLog2_Intercept
#view aggressive log2 intercept data; missing values = NA
Data$AggressiveLog2_Slope
#view aggressive log2 slope data; missing values = NA

#********************************

#Time Series Data: Submissive

Submissive_Matrix<-cbind(Data$DefeatDay1_Submissive, Data$DefeatDay2_Submissive, Data$DefeatDay3_Submissive, Data$DefeatDay4_Submissive)
#combine defeat day data for submissive data WITHOUT log transformation
str(Submissive_Matrix)

#Creates empty numeric vectors the same length as a column in the data matrix (i.e., length=number of subjects) to store results
Data$Submissive_Intercept<-numeric(length(Submissive_Matrix[,1]))
#set submissive intercept data as numeric vector and rename
Data$Submissive_Slope<-numeric(length(Submissive_Matrix[,1]))
#set submissive slope data as numeric vector and rename

  Data$Submissive_Slope<-numeric(length(Submissive_Matrix[,1]))


for(i in c(1:length(Submissive_Matrix[,1]))){
  if(sum(is.na(Submissive_Matrix[i,]))<3){
    FitLine<-lm(Submissive_Matrix[i,]~DefeatDay)
    Data$Submissive_Intercept[i]<-FitLine[[1]][[1]]
    Data$Submissive_Slope[i]<-FitLine[[1]][[2]]
  }else{
    Data$Submissive_Intercept[i]<-NA 
    Data$Submissive_Slope[i]<-NA
  }
}
#Loops over time series data for all subjects (rats) in the dataset:
#is.na implies missing values
#There needs to be fewer than 3 NA (i.e. at least 2 data points) so that we can fit intercept and slope
#if this is true:
##set data to linear model with submissive data as dependent variable
###and defeat day as independent variable
#Set both the intercept and slope data to the linear model
#if there are missing values from the data, remove them
#for loop runs the if else function for each value within the vector
##of submissive data
  #if there are too many missing values from the data, set output to NA

Data$Submissive_Intercept
#Get intercept data; missing values = NA
Data$Submissive_Slope
#Get slope data; missing values = NA

#********************************

#Time Series Data: Aggressive

Aggressive_Matrix<-cbind(Data$DefeatDay1_Aggressive, Data$DefeatDay2_Aggressive, Data$DefeatDay3_Aggressive, Data$DefeatDay4_Aggressive)
#combine defeat days for aggressive data WITHOUT log transformation
str(Aggressive_Matrix)

#Creates empty numeric vectors the same length as a column in the data matrix (i.e., length=number of subjects) to store results
Data$Aggressive_Intercept<-numeric(length(Aggressive_Matrix[,1]))
#make agressive intercept data numeric vector and rename
Data$Aggressive_Slope<-numeric(length(Aggressive_Matrix[,1]))
#make agressive slope data numeric vector and rename

  Data$Aggressive_Slope<-numeric(length(Aggressive_Matrix[,1]))


for(i in c(1:length(Aggressive_Matrix[,1]))){
  if(sum(is.na(Aggressive_Matrix[i,]))<3){
    FitLine<-lm(Aggressive_Matrix[i,]~DefeatDay)
    Data$Aggressive_Intercept[i]<-FitLine[[1]][[1]]
    Data$Aggressive_Slope[i]<-FitLine[[1]][[2]]
  }else{
    Data$Aggressive_Intercept[i]<-NA 
    Data$Aggressive_Slope[i]<-NA
  }
}
#Loops over time series data for all subjects (rats) in the dataset:
#is.na implies missing values
#There needs to be fewer than 3 NA (i.e. at least 2 data points) so that we can fit intercept and slope
#if this is true:
###linear model with agressive data as dependent variable
###and defeat day as independent variable
#Set both the intercept and slope data to the linear model
#if there are missing values from the data, remove them
#for loop runs the if else function for each value within the vector
##of aggressive data

  #if there are too many missing values from the data, set output to NA

Data$Aggressive_Intercept
#get agressive intercept data; missing values = NA
Data$Aggressive_Slope
#get agressive slope data; missing values = NA

#********************************

#Time Series Data: OtherBehavior

OtherBehavior_Matrix<-cbind(Data$DefeatDay1_OtherBehavior, Data$DefeatDay2_OtherBehavior, Data$DefeatDay3_OtherBehavior, Data$DefeatDay4_OtherBehavior)
#combine defeat days for other behavior data
str(OtherBehavior_Matrix)

#Creates empty numeric vectors the same length as a column in the data matrix (i.e., length=number of subjects) to store results
Data$OtherBehavior_Intercept<-numeric(length(OtherBehavior_Matrix[,1]))

#make other behavior intercept data numeric vector and rename
Data$OtherBehavior_Slope<-numeric(length(OtherBehavior_Matrix[,1]))
#make other behavior slope data numeric vector and rename

  Data$OtherBehavior_Slope<-numeric(length(OtherBehavior_Matrix[,1]))

for(i in c(1:length(OtherBehavior_Matrix[,1]))){
  if(sum(is.na(OtherBehavior_Matrix[i,]))<3){
    FitLine<-lm(OtherBehavior_Matrix[i,]~DefeatDay)
    Data$OtherBehavior_Intercept[i]<-FitLine[[1]][[1]]
    Data$OtherBehavior_Slope[i]<-FitLine[[1]][[2]]
  }else{
    Data$OtherBehavior_Intercept[i]<-NA 
    Data$OtherBehavior_Slope[i]<-NA
  }
}
#Loops over time series data for all subjects (rats) in the dataset:
#is.na implies missing values
#There needs to be fewer than 3 NA (i.e. at least 2 data points) so that we can fit intercept and slope
#if this is true:
##set data to linear model with other behavior data as dependent variable
###and defeat day as independent variable
#Set both the intercept and slope data to the linear model
#if there are missing values from the data, remove them
#for loop runs the if else function for each value within the vector
##of other behvaior data
=======
  #if there are too many missing values from the data, make output NA

Data$OtherBehavior_Intercept
#get intercept data, missing values = NA
Data$OtherBehavior_Slope
#get slope data, missing values = NA

#********************************

#Time Series Data: TimeCaged

TimeCaged_Matrix<-cbind(Data$DefeatDay1_TimeCaged, Data$DefeatDay2_TimeCaged, Data$DefeatDay3_TimeCaged, Data$DefeatDay4_TimeCaged)
#combine defeat days for time caged data and rename
str(TimeCaged_Matrix)

#Creates empty numeric vectors the same length as a column in the data matrix (i.e., length=number of subjects) to store results
Data$TimeCaged_Intercept<-numeric(length(TimeCaged_Matrix[,1]))
Data$TimeCaged_Slope<-numeric(length(TimeCaged_Matrix[,1]))

for(i in c(1:length(TimeCaged_Matrix[,1]))){
  if(sum(is.na(TimeCaged_Matrix[i,]))<3){
    FitLine<-lm(TimeCaged_Matrix[i,]~DefeatDay)
    Data$TimeCaged_Intercept[i]<-FitLine[[1]][[1]]
    Data$TimeCaged_Slope[i]<-FitLine[[1]][[2]]
  }else{
    Data$TimeCaged_Intercept[i]<-NA 
    Data$TimeCaged_Slope[i]<-NA
  }
}
#Loops over time series data for all subjects (rats) in the dataset:
#is.na implies missing values
#There needs to be fewer than 3 NA (i.e. at least 2 data points) so that we can fit intercept and slope
#if this is true:
##set data to linear model with time caged data as dependent variable
###and defeat day as independent variable
#Set both the intercept and slope data to the linear model
#if there are too many missing values from the data, make output NA

Data$TimeCaged_Intercept
#get time caged intercept data; missing values = NA
Data$TimeCaged_Slope
#get time caged slope data; missing values = NA

#********************************

#Time Series Data: DefeatScore

DefeatScore_Matrix<-cbind(Data$DefeatDay1_DefeatScore, Data$DefeatDay2_DefeatScore, Data$DefeatDay3_DefeatScore, Data$DefeatDay4_DefeatScore)
#combine defeat days for defeat score data and rename
str(DefeatScore_Matrix)

#Creates empty numeric vectors the same length as a column in the data matrix (i.e., length=number of subjects) to store results
Data$DefeatScore_Intercept<-numeric(length(DefeatScore_Matrix[,1]))
Data$DefeatScore_Slope<-numeric(length(DefeatScore_Matrix[,1]))

for(i in c(1:length(DefeatScore_Matrix[,1]))){
  if(sum(is.na(DefeatScore_Matrix[i,]))<3){
    FitLine<-lm(DefeatScore_Matrix[i,]~DefeatDay)
    Data$DefeatScore_Intercept[i]<-FitLine[[1]][[1]]
    Data$DefeatScore_Slope[i]<-FitLine[[1]][[2]]
  }else{
    Data$DefeatScore_Intercept[i]<-NA 
    Data$DefeatScore_Slope[i]<-NA
  }
}
#Loops over time series data for all subjects (rats) in the dataset:
#is.na implies missing values
#There needs to be fewer than 3 NA (i.e. at least 2 data points) so that we can fit intercept and slope
#if this is true:
##set data to linear model with defeat score data as dependent variable
###and defeat day as independent variable
#Set both the intercept and slope data to the linear model
#if there are too many missing values from the data, make output NA

Data$DefeatScore_Intercept
#get defeat score intercept data; missing values = NA
Data$DefeatScore_Slope
#get defeat score intercept data; missing values = NA

###########################################

#Tacking the Proteoglycan data on to the rest of the dataset so that we can run parallel plotting/analyses:

colnames(ProteoglycanISH_ForComparisonWBehav)

# [1] "Rat_ID"                                                                                                         
# [2] "Generation"                                                                                                     
# [3] "Line"                                                                                                           
# [4] "Treatment_group..EC.Enriched.Cage.Control..EE.Environmental.Enrichment..NIL.Standard.Housing..SD.Social.Defeat."
# [5] "InISH"                                                                                                          
# [6] "Animal.ID"                                                                                                      
# [7] "Generation.1"                                                                                                   
# [8] "bred.line"                                                                                                      
# [9] "treatment.group"                                                                                                
# [10] "Gpc1_CA1"                                                                                                       
# [11] "Gpc1_CA2"                                                                                                       
# [12] "Gpc1_CA3"                                                                                                       
# [13] "Gpc1_DG"                                                                                                        
# [14] "Sdc4_CA1"                                                                                                       
# [15] "Sdc4_CA2"                                                                                                       
# [16] "Sdc4_CA3"                                                                                                       
# [17] "Sdc4_DG"                                                                                                        
# [18] "OrderInOriginalForDoubleChecking"                                                                               
# [19] "OrderInMasterSpreadsheet"    


colnames(Data)

str(data.frame(Data, ProteoglycanISH_ForComparisonWBehav[,c(10:17)]))
#Factors are still formatted correctly

Data<-data.frame(Data, ProteoglycanISH_ForComparisonWBehav[,c(10:17)])

Dcolnames(Data)

Data$Gpc1_Average<-apply(Data[,c(91:94)], 1, mean)
Data$Sdc4_Average<-apply(Data[,c(95:98)], 1, mean)


# Outputting simple stats:

#I had to tweak this code too to fit the subsetted dataset:




#Basic analysis decisions: 

#Using traditional parametric statistics on this data is risky:
##Most of the variables have distinctfully non-normal distributions of residuals. 
##Some also have outliers
##The design is not perfectly balanced, and sample sizes are small
##Because of the small subgroup sample sizes, I'm hesitant to model the non-normal distributions either implicitly using transformations or explicitly using glm 
##Some of the distributions also don't really fit a simple transformation.

#Rank-based transformations allow us to examine monotonic relationships, but a certain amount of information is lost

#Permutation based methods allow us to keep all information. 
## The Freedman-Lane method involves permutating residuals within a reduced model to calculate an empirical T-statistic sampling distribution 
## In datasets with small subgroups sample sizes, The Freedman-Lane method has been shown to have better Type 1 error control in LM or ANOVA with data with non-normally distributed residuals and outliers
## The Freedman-Lane method improves power by ~3-9% within these situations
## Citations: Anderson and Legendre, 1999, Anderson and Ter Braak 2003, Peres-Neto 2001

library(permuco)
#Side note: I double-checked permuco's parametric output vs. other standard packages (base package, car, ezANOVA)
#the lm parametric output from permuco matches standard lm output with Type III SS and contr.treatment
#the anova lm parametric output from permuco matches standard anova output with Type III SS and contr.sum

library(rcompanion)

#I've run two versions of the analysis, one of which includes generation as a co-variate, because many variables had batches defined by generation
#I'm inclined to use the version with generation as a co-variate for all variables just to be conservative, especially since it is unbalanced across subgroups.

#I'm still not sure whether it would be better to use LM or ANOVA-style output
##ANOVA is traditional in neuroscience (type III SS, with contr.sum), followed by post-hoc comparisons
###But it feels awkward to essentially lump together HRs and LRs to examine an "average" effect of interventions
###And it makes it a little more difficult to compare effects across variables, which do not necessarily all have the same subgroups (e.g. no EC)
###And the sheer number of post-hoc t-tests is overwhelming and missing any ability to control for generation as a co-variate.
##LM has more easily interpretable coefficients, but neuroscientists are less comfortable looking at the output

#Depending on how we plan to present the results, we may want to change the output mode to .csv instead of .txt. 

#Other notes: Check with Angela about IL6, which ran HRs and LRs in separate batches.


colnames(Data)


DataColumnsForStats<-c(10:16, 91:100)
#Tweaked to skip SI Test video variables, USV, hormones

#Then I tweaked the analysis code to not perform analyses with generation as a co-variate:
#And also removed _Factor from the factor variable names - I don't seem to have the code where those were created, and they all seem to already be factors with the correct reference levels.

setwd("~/Documents/Microarray Gen/Angela_HRLR_EE_Stress/Proteoglycan_ISH")

for(i in DataColumnsForStats){
  
  Temp<-Data[is.na(Data[,i])==F,]
  
  Temp$Enrichment<-droplevels(Temp$Enrichment)
  Temp$Generation<-droplevels(Temp$Generation)
  
  
  OutputtingStats<-file(paste("LM_byTreatmentGroup_", colnames(Temp)[i], ".txt", sep=""))
  out<-c(
    
    print(paste("Variable :", colnames(Temp)[i], sep=" ")),
    
    print("******************************"),
    
    print("Traditional & Permutation-Based Linear Regression Model"),
    
    capture.output(summary(lmperm(Temp[,i]~Line*Enrichment*Social_Defeat, data = Temp, np=15000, method="freedman_lane"))),
    
    
    print("******************************"),
    
    print("Traditional & Permutation-Based ANOVA (contrasts=contr.sum)"),
    
    capture.output(aovperm(Temp[,i]~Line*Enrichment*Social_Defeat, data = Temp, contrasts="contr.sum", np=15000, method="freedman_lane")) 
    
    
  )
  cat(out, file=paste("LM_byTreatmentGroup_", colnames(Temp)[i], ".txt", sep=""), sep="\n", append=TRUE)
  close(OutputtingStats)
  rm(out)
  rm(Temp)
  
}


######################

#Also: Outputting sample sizes for each variable in the dataset for reporting statistics:

colnames(Data)

setwd("~/Documents/Microarray Gen/Angela_HRLR_EE_Stress/Proteoglycan_ISH")

SampleSizeForEachVariable<-apply(Data[,c(10:92)], 2, function(y) sum(is.na(y)==F))
write.csv(SampleSizeForEachVariable, "SampleSizeForEachVariable.csv")


#########################

#Megan's edits to Liam's functionalized version of TimeSeriesPlots. Second Edition.
#The goal was to make a version that matched other figure formatting for the paper.

#This code had to be seriously tweaked due to the lack of SE animals.

TempCol<-as.character(Data$Treatment_Group)
TempPch<-as.character(Data$Treatment_Group)


#so that they match the colors/symbols used for the enrichment groups in the boxplots used for other variables:  
TempCol[TempCol=="HR NIL + SD"]<-"green3"
#TempCol[TempCol=="HR EC + SD"]<-"forestgreen"
TempCol[TempCol=="HR EE + SD"]<-"darkgreen"
TempCol[TempCol=="LR NIL + SD"]<-"red1"
#TempCol[TempCol=="LR EC + SD"]<-"red3"
TempCol[TempCol=="LR EE + SD"]<-"red4"

TempCol[TempCol=="HR NIL"]<-"green3"
#TempCol[TempCol=="HR EC"]<-"forestgreen"
TempCol[TempCol=="HR EE"]<-"darkgreen"
TempCol[TempCol=="LR NIL"]<-"red1"
#TempCol[TempCol=="LR EC"]<-"red3"
TempCol[TempCol=="LR EE"]<-"red4"

TempPch[TempPch=="HR NIL + SD"]<-16
#TempPch[TempPch=="HR EC + SD"]<-15
TempPch[TempPch=="HR EE + SD"]<-17
TempPch[TempPch=="LR NIL + SD"]<-16
#TempPch[TempPch=="LR EC + SD"]<-15
TempPch[TempPch=="LR EE + SD"]<-17

TempPch[TempPch=="HR NIL"]<-16
#TempPch[TempPch=="HR EC"]<-15
TempPch[TempPch=="HR EE"]<-17
TempPch[TempPch=="LR NIL"]<-16
#TempPch[TempPch=="LR EC"]<-15
TempPch[TempPch=="LR EE"]<-17

TempPch<-as.numeric(TempPch)

#Making the treatment group names match that used in the rest of the paper:
Data$Treatment_Group2<-Data$Treatment_Group

#Data$Treatment_Group2[Data$Treatment_Group2=="HR EC"]<-"bHR SE"
#Data$Treatment_Group2[Data$Treatment_Group2=="LR EC"]<-"bLR SE"
#Data$Treatment_Group2[Data$Treatment_Group2=="HR EC + SD"]<-"bHR SE + SD"
#Data$Treatment_Group2[Data$Treatment_Group2=="LR EC + SD"]<-"bLR SE + SD"

Data$Treatment_Group2[Data$Treatment_Group2=="HR EE"]<-"bHR EE"
Data$Treatment_Group2[Data$Treatment_Group2=="LR EE"]<-"bLR EE"
Data$Treatment_Group2[Data$Treatment_Group2=="HR EE + SD"]<-"bHR EE + SD"
Data$Treatment_Group2[Data$Treatment_Group2=="LR EE + SD"]<-"bLR EE + SD"

Data$Treatment_Group2[Data$Treatment_Group2=="HR NIL"]<-"bHR NIL"
Data$Treatment_Group2[Data$Treatment_Group2=="LR NIL"]<-"bLR NIL"
Data$Treatment_Group2[Data$Treatment_Group2=="HR NIL + SD"]<-"bHR NIL + SD"
Data$Treatment_Group2[Data$Treatment_Group2=="LR NIL + SD"]<-"bLR NIL + SD"



#Creates time plots and outputs them to a pdf. 
#time_series_matrix corresponds to the specific variable matrix the function will be graphing from. Example: Aggressive_Log2_Matrix
#time_series_intercept corresponds to the intercept stored in Data of the specified time_Series_matrix. Example: Data$AggressiveLog2_Intercept
#time_series_slope corresponds to the slope stored in Data of the specified time_Series_matrix. Example: Data$AggressiveLog2_Slope
#pdf_title corresponds to the title of the pdf that is outputted (DO NOT INCLUDE ".pdf" IN THIS TITLE). Example: "AggressiveBehavior_Log2"
#graph_y_label corresponds to the y label of the graphs. Example: "Aggressive Behavior (Log2)"
create_time_plots <- function(time_series_matrix, time_series_intercept, time_series_slope, pdf_title, graph_y_label) {
  
  #Starts the graphics device driver for producing the graphs in a pdf. 
  pdf(paste(pdf_title, "_vs_DefeatDay.pdf", sep = ""), height=12, width=8.5)
  
  #Sets up the graphical parameters for the graphs. 
  #mfrow causes subsequent figures to be drawn in an nr-by-nc array by rows [the array is specified using c(3,2)].
  #cex is a numerical value that gives the amount by which plotting text and symbols should be magnified relative to the default of 1.
  #cex.lab is the magnification to be used for x and y labels relative to the current setting of cex.
  #cex.axis is the magnification to be used for axis annotation relative to the current setting of cex.
  #cex.main is the magnification to be used for main titles relative to the current setting of cex.
  #mar is a numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot. The default is c(5, 4, 4, 2) + 0.1.
  par(mfrow=c(2,2), cex.lab=2, cex.axis=1.5, cex.main=2, mar=c(5.1, 5.1, 5.1, 3.1))
  
  #Determines the proper starting point when graphing a time series panel. "HR NIL + SD" & "HR EC + SD" are not included because they start at 1.
  #Note to Self (Liam): Rework this to automatically determine the starting point, as the various time series variables likely start at different spots as well
  determine_starting_point <- function(treatment_group) {
    
    starting_point <- 1
    
    if (treatment_group == "bLR NIL + SD") {
      starting_point <- 1
    } else if (treatment_group == "bLR SE + SD") {
      starting_point <- 1
    } else if (treatment_group == "bLR EE + SD") {
      starting_point <- 1
    } else if (treatment_group == "bHR EE + SD") {
      starting_point <- 1
    } 
    
    return(starting_point)
  }
  
  #A function that creates a time_series panel using the selected time series variable
  plot_time_series_panel <- function(treatment_group) {
    #stores the starting point for graphing the panel
    panel_starting_point <- determine_starting_point(treatment_group)
    #Takes the data from the specified treatment group in the selected time series variable and assigns it to Temp. 
    Temp<-time_series_matrix[Data$Treatment_Group2==treatment_group,]
    #Creates vectors with formatting info for the specified treatment groups.
    TempCol_T<-TempCol[which(Data$Treatment_Group2==treatment_group)]
    TempPch_T<-TempPch[which(Data$Treatment_Group2==treatment_group)]
    
    
    #Creates the groundwork of the time plot for the selected time series variable for the specified treatment group using a generic X-Y plot.
    #Note to Self (Liam): Rework the labeling system.
    plot(Temp[panel_starting_point,]~DefeatDay, ylab=graph_y_label, xlab="Defeat Day", main=treatment_group, ylim=c(min(cbind(time_series_intercept,time_series_matrix), na.rm=T), max(cbind(time_series_intercept,time_series_matrix), na.rm=T)), pch=TempPch_T[i], col="grey", xaxt="n")
    #relabeling the x-axis so that it is just social defeat day:
    xtick=c(1,2,3,4)
    xtickloc=c(-3,-2,-1,0)
    #axis(side=1, at=xtickloc, labels = FALSE)
    text(x=xtickloc,  par("usr")[3], labels = xtick, pos = 1, xpd = TRUE, cex=2)
    
    #Cycles through the values in Temp[,1] and, after checking to make sure that the value is NOT NA, adds and fits the necessary lines to the plot created in line 27.
    #lines() adds connected line segments to the plot. I'm not sure what type="o" does, so I just ask Dr. Hagenauer for clarification. The help function did not help illuminate what it does.
    #FitLine is assigned the value of lm(Temp[i,]~DefeatDay), which is a function used to fit linear models (specifically, in this case, to fit a linear model using the data in Temp according to DefeatDay)
    #abline() adds straight lines to the plot. In this case, it uses FitLine to provide the coefficients (Intercept and DefeatDay). It assigns colors to the lines based on the colors listed in TempGen ("red2", etc).
    for(i in c(1:length(Temp[,1]))){
      if(sum(is.na(Temp[i,]))<3){
        points(Temp[i,]~DefeatDay, pch=TempPch_T[i], col="grey")
        FitLine<-lm(Temp[i,]~DefeatDay)
        abline(FitLine, col=TempCol_T[i])}else{}
    }
    #Removes "Temp" from the environment.
    rm(Temp)
    #Adds another straight line to the plot made in line 27. This line is black and represents the mean value over the defeat days. 
    abline(mean(time_series_intercept[Data$Treatment_Group2==treatment_group], na.rm=T), mean(time_series_slope[Data$Treatment_Group2==treatment_group], na.rm=T), col="black", lwd=4) 
  }
  
  #Creates the graphs.
  for (i in c("bLR NIL + SD", "bHR NIL + SD", "bLR EE + SD", "bHR EE + SD")) {
    plot_time_series_panel(i)
  }
  
  #Shuts down the graphics device that was putting the plots into a pdf.
  dev.off()
}

#Creates the appropriate time series plots for each Social Defeat Time Series variable
create_time_plots(Aggressive_Matrix, Data$Aggressive_Intercept, Data$Aggressive_Slope, "Aggressive Behavior", "% time aggressive")
create_time_plots(Aggressive_Log2_Matrix, Data$AggressiveLog2_Intercept, Data$AggressiveLog2_Slope, "Aggressive Behavior (Log2)", "% time aggressive (log2)")
create_time_plots(Submissive_Matrix, Data$Submissive_Intercept, Data$Submissive_Slope, "Submissive Behavior", "% time submissive")
create_time_plots(Submissive_Log2_Matrix, Data$SubmissiveLog2_Intercept, Data$SubmissiveLog2_Slope, "Submissive Behavior (Log2)", "% time submissive (Log2)")
create_time_plots(OtherBehavior_Matrix, Data$OtherBehavior_Intercept, Data$OtherBehavior_Slope, "Other Behavior", "% time other behavior")
create_time_plots(DefeatScore_Matrix, Data$DefeatScore_Intercept, Data$DefeatScore_Slope, "Defeat Score", "defeat score")
create_time_plots(TimeCaged_Matrix, Data$TimeCaged_Intercept, Data$TimeCaged_Slope, "Time Caged", "time caged")

#######################################################


#I tweaked this code too (no EC/SE animals)
#Also removed reference to the _factor variables

#General Decoder for Symbols:

#No Defeat=Open, Defeated=Filled Black

Data$TreatmentPch[Data$Enrichment=="EE" & Data$Social_Defeat=="SD"]<-17
Data$TreatmentPch[Data$Enrichment=="EE" & Data$Social_Defeat=="NIL"]<-2

#Data$TreatmentPch[Data$Enrichment=="EC" & Data$Social_Defeat=="SD"]<-15
#Data$TreatmentPch[Data$Enrichment=="EC" & Data$Social_Defeat=="NIL"]<-0

Data$TreatmentPch[Data$Enrichment=="NIL" & Data$Social_Defeat=="SD"]<-16
Data$TreatmentPch[Data$Enrichment=="NIL" & Data$Social_Defeat=="NIL"]<-1

#General Decoder for Colors:
#LR levels of enrichment: NIL: "green3", EC: "forestgreen", EE: "darkgreen"
#HR levels of enrichment: NIL: "red1", EC: "red3", EE: "red4" 

#To Do:
#I vectorized the y-axis labels to match Angela's preferences, but some of her variables didn't have label preferences - double check.
#The treatment group labels should probably be made into some sort of info graphic. They're too much.
#How the heck are we going to mark off "significance"?  It feels overwhelming.
#Double-check limits of detection for the hormone plots with Angela - esp. IL6
#IL6 needs a dividing line calling out the fact that HR and LR are separate batches. Just do in graphics program.


setwd("~/Documents/Microarray Gen/Angela_HRLR_EE_Stress/Proteoglycan_ISH")

colnames(Data)

#colnames(Data[,c(10:16, 24:25, 33:34, 65, 69,76:79,91:98)])

#DataColumnsForBoxplots_AllGroups<-c(10:16, 24:25, 33:34, 65, 69,76:79, 91:98)
DataColumnsForBoxplots_AllGroups<-c(10:16)
YLabelsForBoxplots_AllGroups<-c("Distance (cm)", "% time center", "% time open arms", "% time avoiding (Ethovision)", "% time interacting (Ethovision)", "% time other behavior (Ethovision)", "% time on top (Ethovision)")

#"social defeat day 1: defeat score", "social defeat day 1: time caged", "social defeat day 4: defeat score", "social defeat day 4: time caged", "social defeat day 1: percent time submissive (log2)", "social defeat day 1: percent time aggressive (log2)", "social defeat: percent time submissive (log2 intercept: day 4)", "social defeat: percent time submissive (log2 slope)",  "social defeat: percent time aggressive (log2 intercept: day 4)", "social defeat: percent time aggressive (log2 slope)", 

DataColumnsForBoxplots_Proteoglycans<-c(91:100)
YLabelsForBoxplots_Proteoglycans<-c("Gpc1 (CA1: IOD)", "Gpc1 (CA2: IOD)", "Gpc1 (CA3: IOD)","Gpc1 (DG: IOD)", "Sdc4 (CA1: IOD)", "Sdc4 (CA2: IOD)", "Sdc4 (CA3: IOD)", "Sdc4 (DG: IOD)", "Gpc1 (Average IOD)","Sdc4 (Average IOD)")


for(i in DataColumnsForBoxplots_AllGroups){
  
  Temp<-Data[is.na(Data[,i])==F,]
  
  Temp$Enrichment<-droplevels(Temp$Enrichment)
  Temp$Generation<-droplevels(Temp$Generation) 
  
  pdf(paste("Boxplot_", colnames(Temp)[i], "vsAllGroups.pdf", sep=""), width=6, height=4)
  par(mar=c(6.5, 4.5, 1.1, 1.1), mgp=c(3, 1, 0))
  boxplot(Temp[,i]~Enrichment*Social_Defeat*Line, data = Temp, col=c("green3", "darkgreen", "green3", "darkgreen", "red1", "red4", "red1", "red4"), las=3, ylab=paste(YLabelsForBoxplots_AllGroups[i-9], sep=""), xlab="", cex.lab=1.1, cex.main=1.75, cex.axis=0.9, outline=F)
  
  stripchart(Temp[,i]~Enrichment*Social_Defeat*Line, data = Temp, vertical = TRUE, 
             method = "jitter", add = TRUE, col = 'black', cex=1, pch=c(c(1,2, 16,17),c(1,2, 16,17)))
  dev.off()
  rm(Temp)
}


for(i in DataColumnsForBoxplots_Proteoglycans){
  
  Temp<-Data[is.na(Data[,i])==F,]
  
  Temp$Enrichment<-droplevels(Temp$Enrichment)
  Temp$Generation<-droplevels(Temp$Generation) 
  
  pdf(paste("Boxplot_", colnames(Temp)[i], "vsAllGroups.pdf", sep=""), width=6, height=4)
  par(mar=c(6.5, 4.5, 1.1, 1.1), mgp=c(3, 1, 0))
  boxplot(Temp[,i]~Enrichment*Social_Defeat*Line, data = Temp, col=c("green3", "darkgreen", "green3", "darkgreen", "red1", "red4", "red1", "red4"), las=3, ylab=paste(YLabelsForBoxplots_Proteoglycans[i-90], sep=""), xlab="", cex.lab=1.1, cex.main=1.75, cex.axis=0.9, outline=F)
  
  stripchart(Temp[,i]~Enrichment*Social_Defeat*Line, data = Temp, vertical = TRUE, 
             method = "jitter", add = TRUE, col = 'black', cex=1, pch=c(c(1,2, 16,17),c(1,2, 16,17)))
  dev.off()
  rm(Temp)
}


########################


#This code had to be tweaked too (to remove analyses including generation as a co-variate):


#Code for running a multilevel regression model (MLM) to examine group differences in behavior during the four social defeat sessions
#MLM was chosen because the time-series data has a clear structure (slope) and it makes more sense to model this change with time than to assume that correlations between adjacent time points don't exist
#Also, the data for some individuals is incomplete (some rats only have 3 days worth of data)

#Liam's generalized code for running a multilevel regression model (MLM) to examine group differences in behavior during the four social defeat sessions
#Loops through all of the time series variables in the data frame DefeatDays_LongVersion_OnlyDefeated

setwd("~/Documents/Microarray Gen/Angela_HRLR_EE_Stress/Proteoglycan_ISH")


time_series_variable_names <- c("TimeCaged", "DefeatScore", "Submissive", "Aggressive", "OtherBehavior", "Submissive_Log2", "Aggressive_Log2")

for(i in 8:14) {
  
  current_time_variable <- unlist(data.frame(DefeatDays_LongVersion_OnlyDefeated[i]))
  
  DefeatDays_LongVersion_OnlyDefeated$Temp <- current_time_variable
  
  OutputtingStats<-file(paste("MLM_byTreatmentGroup_", colnames(DefeatDays_LongVersion_OnlyDefeated)[i], ".txt", sep=""))
  
  stats_output <- c(
    
    print(paste("\n", "Multilevel Regression Model for ", time_series_variable_names[i-7], "\n", sep="")),
    
    print(paste("", "Summary of model with intercept set at Day 4:", "\n", sep="\n")),
    
    capture.output(summary(lme(Temp~Day_CenteredOn4*Line*Enrichment, random=~Day_CenteredOn4|RatID, data=DefeatDays_LongVersion_OnlyDefeated, correlation = corAR1(), na.action = na.omit, control = lmeControl(opt = 'optim')))),
    
    print(paste("******************************************************************************", "", "Summary of model with intercept set at Day 1:", "", sep="\n")),
    
    capture.output(summary(lme(Temp~Day*Line*Enrichment, random=~Day|RatID, data=DefeatDays_LongVersion_OnlyDefeated, correlation = corAR1(), na.action = na.omit, control = lmeControl(opt = 'optim')))),
    

    print(paste("", "******************************************************************************","", "ANOVA Summary of model with intercept set at Day 4:", "", sep="\n")),
    
    capture.output(car::Anova(lme(Temp~Day_CenteredOn4*Line*Enrichment, random=~Day_CenteredOn4|RatID, data=DefeatDays_LongVersion_OnlyDefeated, correlation = corAR1(), na.action=na.omit, method="ML", control = lmeControl(opt = 'optim'), contrasts=list(Line=contr.sum, Enrichment=contr.sum)), type="III")),
    
    print(paste("", "******************************************************************************","", "ANOVA Summary of model with intercept set at Day 1:", "", sep="\n")),
    
    capture.output(car::Anova(lme(Temp~Day*Line*Enrichment, random=~Day|RatID, data=DefeatDays_LongVersion_OnlyDefeated, correlation = corAR1(), na.action=na.omit, method="ML", control = lmeControl(opt = 'optim'), contrasts=list(Line=contr.sum, Enrichment=contr.sum)), type="III"))
    

  )
  
  cat(stats_output, file=paste("MLM_byTreatmentGroup_", colnames(DefeatDays_LongVersion_OnlyDefeated)[i], ".txt", sep=""), sep="\n", append=TRUE)
  
  close(OutputtingStats)
  
  rm(stats_output)
  rm(current_time_variable)
  
  DefeatDays_LongVersion_OnlyDefeated$Temp <- NULL
}

#Hit a convergence error for the Log2 version of the variables. I didn't bother to troubleshoot it because I don't think we are likely to use this analysis anyway - this data is more appropriate for the larger behavioral/hormone paper. 

# Error in lme.formula(Temp ~ Day_CenteredOn4 * Line * Enrichment, random = ~Day_CenteredOn4 |  : 
#                        optim problem, convergence error code = 1
#                      message = 

#********************************************

DataColumnsForHistograms_AllGroups <- c(10:16)
DataColumnsForHistograms_Proteoglycans<-c(91:100)

XLabelsForHistograms_AllGroups<-c("Distance (cm)", "% time center", "% time open arms", "% time avoiding (Ethovision)", "% time interacting (Ethovision)", "% time other behavior (Ethovision)", "% time on top (Ethovision)")
XLabelsForHistograms_Proteoglycans<-c("Gpc1 (CA1: IOD)", "Gpc1 (CA2: IOD)", "Gpc1 (CA3: IOD)","Gpc1 (DG: IOD)", "Sdc4 (CA1: IOD)", "Sdc4 (CA2: IOD)", "Sdc4 (CA3: IOD)", "Sdc4 (DG: IOD)", "Gpc1 (Average IOD)","Sdc4 (Average IOD)")

for(i in DataColumnsForHistograms_AllGroups) {
  
  Temp <- Data[is.na(Data[,i])==F,]
  
  pdf(paste("Histograms", colnames(Temp)[i], 
            ".pdf", sep=""), height=5, width=5)
  hist(Temp[,i], xlab=XLabelsForHistograms_AllGroups[i-9], col="lightblue")
  
  dev.off()
  rm(Temp)
}

for(i in DataColumnsForHistograms_Proteoglycans) {
  
  Temp <- Data[is.na(Data[,i])==F,]
  
  pdf(paste("Histograms", colnames(Temp)[i], 
            ".pdf", sep=""), height=5, width=5)
  hist(Temp[,i], xlab=XLabelsForHistograms_Proteoglycans[i-90], col="lightblue")
  
  dev.off()
  rm(Temp)
}

#********************************************

#Tweaked this code too:


bLRData<-Data[Data$Line=="bLR",]
bHRData<-Data[Data$Line=="bHR",]

TempCol<-as.character(Data$Treatment_Group)
TempPch<-as.character(Data$Treatment_Group)


#Color codes the graphs so that they match the colors used for the enrichment groups in the boxplots used for other variables:  
TempCol[TempCol=="HR NIL + SD"]<-"green3"
#TempCol[TempCol=="HR EC + SD"]<-"forestgreen"
TempCol[TempCol=="HR EE + SD"]<-"darkgreen"
TempCol[TempCol=="LR NIL + SD"]<-"red1"
#TempCol[TempCol=="LR EC + SD"]<-"red3"
TempCol[TempCol=="LR EE + SD"]<-"red4"

TempCol[TempCol=="HR NIL"]<-"green3"
#TempCol[TempCol=="HR EC"]<-"forestgreen"
TempCol[TempCol=="HR EE"]<-"darkgreen"
TempCol[TempCol=="LR NIL"]<-"red1"
#TempCol[TempCol=="LR EC"]<-"red3"
TempCol[TempCol=="LR EE"]<-"red4"

#to reuse older formatting code:
Data$LineColor<-TempCol

TempPch[TempPch=="HR NIL + SD"]<-16
#TempPch[TempPch=="HR EC + SD"]<-15
TempPch[TempPch=="HR EE + SD"]<-17
TempPch[TempPch=="LR NIL + SD"]<-16
#TempPch[TempPch=="LR EC + SD"]<-15
TempPch[TempPch=="LR EE + SD"]<-17

TempPch[TempPch=="HR NIL"]<-1
#TempPch[TempPch=="HR EC"]<-0
TempPch[TempPch=="HR EE"]<-2
TempPch[TempPch=="LR NIL"]<-1
#TempPch[TempPch=="LR EC"]<-0
TempPch[TempPch=="LR EE"]<-2

TempPch<-as.numeric(TempPch)

#to reuse older formatting code:
Data$TreatmentPch<-TempPch


#Outputting scatterplots between all relevant numeric variables, coded by treatment group:
#I later removed the HR and LR specific trend lines, since the subgroup sample sizes are pretty puny.

colnames(Data[,c(10:16)])

#Reusing code from the boxplots:
DataColumnsForBoxplots_AllGroups<-c(10:16)
YLabelsForBoxplots_AllGroups<-c("distance (cm)", "% time center", "% time open arms", "% time avoiding (Ethovision)", "% time interacting (Ethovision)", "% time other behavior (Ethovision)", "% time on top (Ethovision)")

DataColumnsForBoxplots_Proteoglycans<-c(91:100)
YLabelsForBoxplots_Proteoglycans<-c("Gpc1 (CA1: IOD)", "Gpc1 (CA2: IOD)", "Gpc1 (CA3: IOD)","Gpc1 (DG: IOD)", "Sdc4 (CA1: IOD)", "Sdc4 (CA2: IOD)", "Sdc4 (CA3: IOD)", "Sdc4 (DG: IOD)", "Gpc1 (Average IOD)","Sdc4 (Average IOD)")

setwd("~/Documents/Microarray Gen/Angela_HRLR_EE_Stress/Proteoglycan_ISH/Scatterplots")

for(i in DataColumnsForBoxplots_AllGroups){
  for(j in DataColumnsForBoxplots_AllGroups){
    
    pdf(paste("Scatterplot_", colnames(Data)[i], "vs", colnames(Data)[j], ".pdf", sep=""), width=6, height=6)
    
    plot(Data[,i]~Data[,j], col=Data$LineColor, pch=Data$TreatmentPch, cex.lab=1.3,
         ylab=YLabelsForBoxplots_AllGroups[i-9], 
         xlab=YLabelsForBoxplots_AllGroups[j-9])
    
    #LRmodel<-lm(bLRData[,i]~bLRData[,j], data=bLRData)
    #abline(LRmodel, col="red3", lwd=1.5)
    
    #HRmodel<-lm(bHRData[,i]~bHRData[,j], data=bHRData)
    #abline(HRmodel, col="forestgreen", lwd=1.5)
    
    Allmodel<-lm(Data[,i]~Data[,j], data=Data)
    abline(Allmodel, col="black", lwd=3)
    
    dev.off()
  }
  
  for(k in DataColumnsForBoxplots_Proteoglycans){
    
    pdf(paste("Scatterplot_", colnames(Data)[k], "vs", colnames(Data)[i], ".pdf", sep=""), width=6, height=6)
    
    plot(Data[,k]~Data[,i], col=Data$LineColor, pch=Data$TreatmentPch, cex.lab=1.3,
         ylab=YLabelsForBoxplots_Proteoglycans[k-90], 
         xlab=YLabelsForBoxplots_AllGroups[i-9])
    
    #LRmodel<-lm(bLRData[,k]~bLRData[,i], data=bLRData)
    #abline(LRmodel, col="red3", lwd=1.5)
    
    #HRmodel<-lm(bHRData[,k]~bHRData[,i], data=bHRData)
    #abline(HRmodel, col="forestgreen", lwd=1.5)
    
    Allmodel<-lm(Data[,k]~Data[,i], data=Data)
    abline(Allmodel, col="black", lwd=3)
    
    dev.off()
  }
}

#############################

library(reshape2)
library(ggplot2)


PrettyCorrelationMatrixFunction<-function(NumericDataToCorrelate, UseForCor=c("everything", "all.obs", "complete.obs", "na.or.complete","pairwise.complete.obs"), MethodForCor=c("pearson", "kendall", "spearman")){
  
  cormat <- round(cor(NumericDataToCorrelate, use=UseForCor, method=MethodForCor),2)
  head(cormat)  
  
  melted_cormat <- melt(cormat)
  head(melted_cormat)
  
  # Get lower triangle of the correlation matrix
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  
  upper_tri <- get_upper_tri(cormat)
  upper_tri
  
  # Melt the correlation matrix
  
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  # Heatmap
  
  ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name=paste(MethodForCor, "correlation", sep=" ")) +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()
  
  reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
  }
  
  
  # Reorder the correlation matrix
  cormat <- reorder_cormat(cormat)
  upper_tri <- get_upper_tri(cormat)
  # Melt the correlation matrix
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  
  #Originally I had the pdf outputted as part of the function, but for unknown reasons I couldn't get it to work like usual:
  #pdf(file=OutputFileName, width=OutputFileSize, height=OutputFileSize)
  
  # Create a ggheatmap
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name=paste(MethodForCor, "correlation", sep=" ")) +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 8, hjust = 1))+
    coord_fixed()
  # Print the heatmap
  print(ggheatmap)
  
  
  ggheatmap + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 1.5) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal") + guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5))
  
  #dev.off()
  
}

###################


#Applying the function:

setwd("~/Documents/Microarray Gen/Angela_HRLR_EE_Stress/Proteoglycan_ISH")

PrettyCorrelationMatrixFunction(NumericDataToCorrelate=Data[,c(10:16, 91:100)], UseForCor="pairwise.complete", MethodForCor="spearman")
dev.copy(pdf, "/Users/mhh/Documents/Microarray Gen/Angela_HRLR_EE_Stress/Proteoglycan_ISH/CorrMatrix_AllVariables_SpearmanNoLog.pdf")
dev.off()

PrettyCorrelationMatrixFunction(NumericDataToCorrelate=Data[,c(10:16, 99:100)], UseForCor="pairwise.complete", MethodForCor="spearman")
dev.copy(pdf, "/Users/mhh/Documents/Microarray Gen/Angela_HRLR_EE_Stress/Proteoglycan_ISH/CorrMatrix_AllVariables_SpearmanNoLog_v2.pdf")
dev.off()

#Outputting some quick correlation stats:
cor(Data$Gpc1_Average, Data$EPM_Explore_Open_Arms, method="spearman")
#[1] -0.3558825
summary.lm(lm(Data$Gpc1_Average~Data$EPM_Explore_Open_Arms))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 0.199344   0.008123   24.54  < 2e-16 ***
#   Data$EPM_Explore_Open_Arms -0.001256   0.000375   -3.35  0.00162 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.03981 on 46 degrees of freedom
# Multiple R-squared:  0.1961,	Adjusted R-squared:  0.1787 
# F-statistic: 11.22 on 1 and 46 DF,  p-value: 0.00162
summary(lmperm(Data$Gpc1_Average~Data$EPM_Explore_Open_Arms, np=15000, method="freedman_lane"))
# Table of marginal t-test of the betas
# Permutation test using freedman_lane to handle nuisance variables and 15000 permutations.
# Estimate Std. Error t value parametric Pr(>|t|) permutation Pr(<t)
# (Intercept)                 0.199343   0.008123   24.54           4.544e-28                   
# Data$EPM_Explore_Open_Arms -0.001256   0.000375   -3.35           1.620e-03          0.0009333
# permutation Pr(>t) permutation Pr(>|t|)
# (Intercept)                                                       
# Data$EPM_Explore_Open_Arms             0.9991               0.0012


cor(Data$Gpc1_Average, Data$OpenField_DistanceTraveled, method="spearman")
#[1] -0.3978506
summary.lm(lm(Data$Gpc1_Average~Data$OpenField_DistanceTraveled))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      2.037e-01  1.230e-02  16.560   <2e-16 ***
#   Data$OpenField_DistanceTraveled -5.973e-06  2.706e-06  -2.208   0.0323 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.04222 on 46 degrees of freedom
# Multiple R-squared:  0.09579,	Adjusted R-squared:  0.07613 
# F-statistic: 4.873 on 1 and 46 DF,  p-value: 0.0323
summary(lmperm(Data$Gpc1_Average~Data$OpenField_DistanceTraveled, np=15000, method="freedman_lane"))
# Table of marginal t-test of the betas
# Permutation test using freedman_lane to handle nuisance variables and 15000 permutations.
# Estimate Std. Error t value parametric Pr(>|t|) permutation Pr(<t)
# (Intercept)                      2.037e-01  1.230e-02  16.560           5.226e-21                   
# Data$OpenField_DistanceTraveled -5.973e-06  2.706e-06  -2.208           3.230e-02            0.01627
# permutation Pr(>t) permutation Pr(>|t|)
# (Intercept)                                                            
# Data$OpenField_DistanceTraveled             0.9838              0.03253


cor(Data$Gpc1_Average, Data$SITest_TimeOnTop_Ethovision, method="spearman")
#[1] -0.4301999
summary.lm(lm(Data$Gpc1_Average~Data$SITest_TimeOnTop_Ethovision))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       0.189799   0.007960  23.844   <2e-16 ***
#   Data$SITest_TimeOnTop_Ethovision -0.003294   0.001711  -1.925   0.0604 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.04272 on 46 degrees of freedom
# Multiple R-squared:  0.07454,	Adjusted R-squared:  0.05443 
# F-statistic: 3.705 on 1 and 46 DF,  p-value: 0.06044
summary(lmperm(Data$Gpc1_Average~Data$SITest_TimeOnTop_Ethovision, np=15000, method="freedman_lane"))
# Table of marginal t-test of the betas
# Permutation test using freedman_lane to handle nuisance variables and 15000 permutations.
# Estimate Std. Error t value parametric Pr(>|t|) permutation Pr(<t)
# (Intercept)                       0.189799   0.007960  23.844           1.553e-27                   
# Data$SITest_TimeOnTop_Ethovision -0.003294   0.001711  -1.925           6.044e-02              0.036
# permutation Pr(>t) permutation Pr(>|t|)
# (Intercept)                                                             
# Data$SITest_TimeOnTop_Ethovision             0.9641               0.0622


cor(Data$Gpc1_Average, Data$SITest_SocialInteraction_Ethovision, method="spearman")
#[1] -0.2765288
summary.lm(lm(Data$Gpc1_Average~Data$SITest_SocialInteraction_Ethovision))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               0.1981221  0.0122482  16.176   <2e-16 ***
#   Data$SITest_SocialInteraction_Ethovision -0.0003558  0.0002084  -1.707   0.0946 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.04306 on 46 degrees of freedom
# Multiple R-squared:  0.05956,	Adjusted R-squared:  0.03912 
# F-statistic: 2.913 on 1 and 46 DF,  p-value: 0.09459
summary(lmperm(Data$Gpc1_Average~Data$SITest_SocialInteraction_Ethovision, np=15000, method="freedman_lane"))
# Table of marginal t-test of the betas
# Permutation test using freedman_lane to handle nuisance variables and 15000 permutations.
# Estimate Std. Error t value parametric Pr(>|t|)
# (Intercept)                               0.1981221  0.0122482  16.176           1.318e-20
# Data$SITest_SocialInteraction_Ethovision -0.0003558  0.0002084  -1.707           9.459e-02
# permutation Pr(<t) permutation Pr(>t) permutation Pr(>|t|)
# (Intercept)                                                                                        
# Data$SITest_SocialInteraction_Ethovision            0.04567             0.9544              0.09613



cor(Data$Gpc1_Average, Data$SITest_SocialAvoidance_Ethovision, method="spearman")
#[1] 0.1548822
summary.lm(lm(Data$Gpc1_Average~Data$SITest_SocialAvoidance_Ethovision))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            0.1754035  0.0079633  22.026   <2e-16 ***
#   Data$SITest_SocialAvoidance_Ethovision 0.0001961  0.0002007   0.977    0.334    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.04395 on 46 degrees of freedom
# Multiple R-squared:  0.02034,	Adjusted R-squared:  -0.0009581 
# F-statistic: 0.955 on 1 and 46 DF,  p-value: 0.3336
summary(lmperm(Data$Gpc1_Average~Data$SITest_SocialAvoidance_Ethovision, np=15000, method="freedman_lane"))
# Table of marginal t-test of the betas
# Permutation test using freedman_lane to handle nuisance variables and 15000 permutations.
# Estimate Std. Error t value parametric Pr(>|t|) permutation Pr(<t)
# (Intercept)                            0.1754035  0.0079633 22.0264           4.469e-26                   
# Data$SITest_SocialAvoidance_Ethovision 0.0001961  0.0002007  0.9772           3.336e-01             0.8389
# permutation Pr(>t) permutation Pr(>|t|)
# (Intercept)                                                                   
# Data$SITest_SocialAvoidance_Ethovision             0.1612               0.3286


cor(Data$Sdc4_Average, Data$EPM_Explore_Open_Arms, method="spearman")
#[1] 0.3222216
summary.lm(lm(Data$Sdc4_Average~Data$EPM_Explore_Open_Arms))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                0.5060692  0.0211802  23.894   <2e-16 ***
#   Data$EPM_Explore_Open_Arms 0.0023478  0.0009777   2.401   0.0204 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1038 on 46 degrees of freedom
# Multiple R-squared:  0.1114,	Adjusted R-squared:  0.09208 
# F-statistic: 5.767 on 1 and 46 DF,  p-value: 0.02043
summary(lmperm(Data$Sdc4_Average~Data$EPM_Explore_Open_Arms, np=15000, method="freedman_lane"))
# Table of marginal t-test of the betas
# Permutation test using freedman_lane to handle nuisance variables and 15000 permutations.
# Estimate Std. Error t value parametric Pr(>|t|) permutation Pr(<t)
# (Intercept)                0.506069  0.0211802  23.894           1.422e-27                   
# Data$EPM_Explore_Open_Arms 0.002348  0.0009777   2.401           2.043e-02              0.989
# permutation Pr(>t) permutation Pr(>|t|)
# (Intercept)                                                       
# Data$EPM_Explore_Open_Arms            0.01107              0.02127


cor(Data$Sdc4_Average, Data$OpenField_DistanceTraveled, method="spearman")
#[1] 0.252931
summary.lm(lm(Data$Sdc4_Average~Data$OpenField_DistanceTraveled))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                     5.150e-01  3.175e-02   16.22   <2e-16 ***
#   Data$OpenField_DistanceTraveled 6.843e-06  6.984e-06    0.98    0.332    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.109 on 46 degrees of freedom
# Multiple R-squared:  0.02044,	Adjusted R-squared:  -0.0008527 
# F-statistic:  0.96 on 1 and 46 DF,  p-value: 0.3323
summary(lmperm(Data$Sdc4_Average~Data$OpenField_DistanceTraveled, np=15000, method="freedman_lane"))
# Table of marginal t-test of the betas
# Permutation test using freedman_lane to handle nuisance variables and 15000 permutations.
# Estimate Std. Error t value parametric Pr(>|t|) permutation Pr(<t)
# (Intercept)                     5.150e-01  3.175e-02 16.2205           1.182e-20                   
# Data$OpenField_DistanceTraveled 6.843e-06  6.984e-06  0.9798           3.323e-01             0.8293
# permutation Pr(>t) permutation Pr(>|t|)
# (Intercept)                                                            
# Data$OpenField_DistanceTraveled             0.1708               0.3355



cor(Data$Sdc4_Average, Data$SITest_TimeOnTop_Ethovision, method="spearman")
#[1] 0.2763857
summary.lm(lm(Data$Sdc4_Average~Data$SITest_TimeOnTop_Ethovision))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      0.526072   0.020180  26.068   <2e-16 ***
#   Data$SITest_TimeOnTop_Ethovision 0.005421   0.004339   1.249    0.218    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1083 on 46 degrees of freedom
# Multiple R-squared:  0.03282,	Adjusted R-squared:  0.0118 
# F-statistic: 1.561 on 1 and 46 DF,  p-value: 0.2178
summary(lmperm(Data$Sdc4_Average~Data$SITest_TimeOnTop_Ethovision, np=15000, method="freedman_lane"))
# Table of marginal t-test of the betas
# Permutation test using freedman_lane to handle nuisance variables and 15000 permutations.
# Estimate Std. Error t value parametric Pr(>|t|) permutation Pr(<t)
# (Intercept)                      0.526072   0.020180  26.068           3.386e-29                   
# Data$SITest_TimeOnTop_Ethovision 0.005421   0.004339   1.249           2.178e-01             0.8887
# permutation Pr(>t) permutation Pr(>|t|)
# (Intercept)                                                             
# Data$SITest_TimeOnTop_Ethovision             0.1113               0.2159


cor(Data$Sdc4_Average, Data$SITest_SocialInteraction_Ethovision, method="spearman")
#[1] -0.07581189
summary.lm(lm(Data$Sdc4_Average~Data$SITest_SocialInteraction_Ethovision))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               0.5511418  0.0312829  17.618   <2e-16 ***
#   Data$SITest_SocialInteraction_Ethovision -0.0001801  0.0005324  -0.338    0.737    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.11 on 46 degrees of freedom
# Multiple R-squared:  0.002483,	Adjusted R-squared:  -0.0192 
# F-statistic: 0.1145 on 1 and 46 DF,  p-value: 0.7366
summary(lmperm(Data$Sdc4_Average~Data$SITest_SocialInteraction_Ethovision, np=15000, method="freedman_lane"))
# Table of marginal t-test of the betas
# Permutation test using freedman_lane to handle nuisance variables and 15000 permutations.
# Estimate Std. Error t value parametric Pr(>|t|)
# (Intercept)                               0.5511418  0.0312829 17.6180           4.425e-22
# Data$SITest_SocialInteraction_Ethovision -0.0001801  0.0005324 -0.3384           7.366e-01
# permutation Pr(<t) permutation Pr(>t) permutation Pr(>|t|)
# (Intercept)                                                                                        
# Data$SITest_SocialInteraction_Ethovision             0.3771              0.623               0.7417

cor(Data$Sdc4_Average, Data$SITest_SocialAvoidance_Ethovision, method="spearman")
#[1] 0.1327251
summary.lm(lm(Data$Sdc4_Average~Data$SITest_SocialAvoidance_Ethovision))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            0.5326905  0.0198223  26.873   <2e-16 ***
#   Data$SITest_SocialAvoidance_Ethovision 0.0003889  0.0004995   0.779     0.44    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1094 on 46 degrees of freedom
# Multiple R-squared:  0.01301,	Adjusted R-squared:  -0.00845 
# F-statistic: 0.6062 on 1 and 46 DF,  p-value: 0.4402
summary(lmperm(Data$Sdc4_Average~Data$SITest_SocialAvoidance_Ethovision, np=15000, method="freedman_lane"))
# Table of marginal t-test of the betas
# Permutation test using freedman_lane to handle nuisance variables and 15000 permutations.
# Estimate Std. Error t value parametric Pr(>|t|) permutation Pr(<t)
# (Intercept)                            0.5326905  0.0198223 26.8733           9.096e-30                   
# Data$SITest_SocialAvoidance_Ethovision 0.0003889  0.0004995  0.7786           4.402e-01             0.7797
# permutation Pr(>t) permutation Pr(>|t|)
# (Intercept)                                                                   
# Data$SITest_SocialAvoidance_Ethovision             0.2204               0.4461
