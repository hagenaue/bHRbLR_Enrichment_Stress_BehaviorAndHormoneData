
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


