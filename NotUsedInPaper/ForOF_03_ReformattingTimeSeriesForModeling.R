# 3. Dealing with missing time series data:

# We have missing time series data within several of these variables (video scored behaviors during defeat), so we should derive a fit:

#*************

#I made a new data frame with the defeat data in long version for time-series analyses:

#Full dataset has 142 subjects:
142*4
#[1] 568

DefeatDays_LongVersion<-data.frame(Day=numeric(length=568), RatID=character(length=568), Generation=factor(character(length=568)), Line=factor(character(length=568)), Enrichment=factor(character(length=568)), Social_Defeat=factor(character(length=568)), AggressorID=character(length=568), TimeCaged=numeric(length=568), DefeatScore=integer(length=568), Submissive=numeric(length=568), Aggressive=numeric(length=568), OtherBehavior=numeric(length=568), Submissive_Log2=numeric(length=568), Aggressive_Log2=numeric(length=568))

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
#'data.frame':	568 obs. of  14 variables:

#Factors are leveled properly:
# $ Generation     : Factor w/ 3 levels "F53","F49","F56": 2 2 2 2 2 2 2 2 2 2 ...
# $ Line           : Factor w/ 2 levels "bHR","bLR": 1 1 1 1 1 1 1 1 1 1 ...
# $ Enrichment     : Factor w/ 3 levels "NIL","EC","EE": 3 3 3 3 3 3 1 1 1 1 ...
# $ Social_Defeat  : Factor w/ 2 levels "NIL","SD": 1 1 1 1 1 1 2 2 2 2 ...

# # str(DefeatDays_LongVersion)
# # DefeatDays_LongVersion$Line<-as.factor(DefeatDays_LongVersion$Line)
# # DefeatDays_LongVersion$Enrichment<-as.factor(DefeatDays_LongVersion$Enrichment)
# # DefeatDays_LongVersion$Social_Defeat<-as.factor(DefeatDays_LongVersion$Social_Defeat)
# # str(DefeatDays_LongVersion)
# # DefeatDays_LongVersion$Enrichment<-relevel(DefeatDays_LongVersion$Enrichment, ref="NIL")
# 
# str(DefeatDays_LongVersion)
# # $ Line           : Factor w/ 2 levels "bHR","bLR": 1 1 1 1 1 1 1 1 1 1 ...
# # $ Enrichment     : Factor w/ 3 levels "NIL","EC","EE": 3 3 3 3 3 3 1 1 1 1 ...
# # $ Social_Defeat  : Factor w/ 2 levels "NIL","SD": 1 1 1 1 1 1 2 2 2 2 ...



#' DefeatDays_LongVersion_OnlyDefeated<-DefeatDays_LongVersion[which(DefeatDays_LongVersion$Social_Defeat=="SD"),]
#' 
#' str(DefeatDays_LongVersion_OnlyDefeated)
#' #'data.frame':	284 obs. of  14 variables:
#' 


DefeatDays_LongVersion$Day_CenteredOn4<-DefeatDays_LongVersion$Day-3


#Subsetting:
DefeatDays_LongVersion_VideoRecorded<-DefeatDays_LongVersion[is.na(DefeatDays_LongVersion$Submissive)==F,]                                                 
                                                      
DefeatDays_LongVersion_VideoRecorded$Submissive

DefeatDays_LongVersion_OnlyDefeated<-DefeatDays_LongVersion[is.na(DefeatDays_LongVersion$TimeCaged)==F,]

DefeatDays_LongVersion_OnlyDefeated$TimeCaged


#Justifying the decision to cut out the first min of the open field data:


5*142
#[1] 710

colnames(Data)

OFData_LongVersion<-data.frame(Minute=numeric(length=710), RatID=character(length=710), Generation=factor(character(length=710)), Line=factor(character(length=710)), Enrichment=factor(character(length=710)), OF_Distance=numeric(length=710), OF_PercentTimeInCenter=numeric(length=710))

OFData_LongVersion$OF_Distance<-c(Data$OF_Distance_0.01.00.0.02.00, Data$OF_Distance_0.01.00.0.02.00, Data$OF_Distance_0.02.00.0.03.00, Data$OF_Distance_0.03.00.0.04.00, Data$OF_Distance_0.04.00.0.05.00)

OFData_LongVersion$OF_PercentTimeInCenter<-c(Data$OF_PercentTimeInCenter_0.01.00.0.02.00, Data$OF_PercentTimeInCenter_0.01.00.0.02.00, Data$OF_PercentTimeInCenter_0.02.00.0.03.00, Data$OF_PercentTimeInCenter_0.03.00.0.04.00, Data$OF_PercentTimeInCenter_0.04.00.0.05.00)

OFData_LongVersion$Minute<-c(rep(1, 142), rep(2, 142), rep(3, 142), rep(4, 142), rep(5, 142))

OFData_LongVersion$RatID<-rep(Data$RatID, 5)

OFData_LongVersion$Generation<-rep(Data$Generation, 5)

OFData_LongVersion$Line<-rep(Data$Line, 5)

OFData_LongVersion$Enrichment<-rep(Data$Enrichment, 5)

OFData_LongVersion$Social_Defeat<-rep(Data$Social_Defeat, 5)

boxplot(OFData_LongVersion$OF_PercentTimeInCenter~OFData_LongVersion$Minute, Data=OFData_LongVersion[OFData_LongVersion$Line=="bHR",])
boxplot(OFData_LongVersion$OF_PercentTimeInCenter~OFData_LongVersion$Minute, Data=OFData_LongVersion[OFData_LongVersion$Line=="bLR",])

OF_Distance_Matrix<-cbind(Data$OF_Distance_0.01.00.0.02.00, Data$OF_Distance_0.01.00.0.02.00, Data$OF_Distance_0.02.00.0.03.00, Data$OF_Distance_0.03.00.0.04.00, Data$OF_Distance_0.04.00.0.05.00)

OF_PercentTimeInCenter_Matrix<-cbind(Data$OF_PercentTimeInCenter_0.01.00.0.02.00, Data$OF_PercentTimeInCenter_0.01.00.0.02.00, Data$OF_PercentTimeInCenter_0.02.00.0.03.00, Data$OF_PercentTimeInCenter_0.03.00.0.04.00, Data$OF_PercentTimeInCenter_0.04.00.0.05.00)
