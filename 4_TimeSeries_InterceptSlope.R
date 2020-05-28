#For analysis purposes, it might be helpful to set the intercept as Day4, since we are missing Day4 information for some individuals and that day is most likely to reflect the cumulative effects of social defeat.
DefeatDay<-c(-3,-2,-1,0)

#Fitting an intercept and slope to the time series data:

Submissive_Log2_Matrix<-cbind(Data$DefeatDay1_Submissive_Log2, Data$DefeatDay2_Submissive_Log2, Data$DefeatDay3_Submissive_Log2, Data$DefeatDay4_Submissive_Log2)
str(Submissive_Log2_Matrix)

Data$SubmissiveLog2_Intercept<-numeric(length(Submissive_Log2_Matrix[,1]))
Data$SubmissiveLog2_Slope<-numeric(length(Submissive_Log2_Matrix[,1]))

for(i in c(1:length(Submissive_Log2_Matrix[,1]))){
  if(is.na(Submissive_Log2_Matrix[i,1])==F){
    FitLine<-lm(Submissive_Log2_Matrix[i,]~DefeatDay)
    Data$SubmissiveLog2_Intercept[i]<-FitLine[[1]][[1]]
    Data$SubmissiveLog2_Slope[i]<-FitLine[[1]][[2]]
  }else{
    Data$SubmissiveLog2_Intercept[i]<-NA 
    Data$SubmissiveLog2_Slope[i]<-NA
  }
}

Aggressive_Log2_Matrix<-cbind(Data$DefeatDay1_Aggressive_Log2, Data$DefeatDay2_Aggressive_Log2, Data$DefeatDay3_Aggressive_Log2, Data$DefeatDay4_Aggressive_Log2)
str(Aggressive_Log2_Matrix)
DefeatDay<-c(-3,-2,-1,0)

plot(Aggressive_Log2_Matrix[7,]~DefeatDay)
FitLine<-lm(Aggressive_Log2_Matrix[7,]~DefeatDay)
abline(FitLine)


Data$AggressiveLog2_Intercept<-numeric(length(Aggressive_Log2_Matrix[,1]))
Data$AggressiveLog2_Slope<-numeric(length(Aggressive_Log2_Matrix[,1]))


for(i in c(1:length(Aggressive_Log2_Matrix[,1]))){
  if(is.na(Aggressive_Log2_Matrix[i,1])==F){
    FitLine<-lm(Aggressive_Log2_Matrix[i,]~DefeatDay)
    Data$AggressiveLog2_Intercept[i]<-FitLine[[1]][[1]]
    Data$AggressiveLog2_Slope[i]<-FitLine[[1]][[2]]
  }else{
    Data$AggressiveLog2_Intercept[i]<-NA 
    Data$AggressiveLog2_Slope[i]<-NA
  }
}

Data$AggressiveLog2_Intercept
Data$AggressiveLog2_Slope


Submissive_Matrix<-cbind(Data$DefeatDay1_Submissive, Data$DefeatDay2_Submissive, Data$DefeatDay3_Submissive, Data$DefeatDay4_Submissive)
str(Submissive_Matrix)
DefeatDay<-c(-3,-2,-1,0)

plot(Submissive_Matrix[7,]~DefeatDay)
FitLine<-lm(Submissive_Matrix[7,]~DefeatDay)
abline(FitLine)
FitLine[[1]][1]
FitLine[[1]][2]

Data$Submissive_Intercept<-numeric(length(Submissive_Matrix[,1]))
Data$Submissive_Slope<-numeric(length(Submissive_Matrix[,1]))

for(i in c(1:length(Submissive_Matrix[,1]))){
  if(is.na(Submissive_Matrix[i,1])==F){
    FitLine<-lm(Submissive_Matrix[i,]~DefeatDay)
    Data$Submissive_Intercept[i]<-FitLine[[1]][[1]]
    Data$Submissive_Slope[i]<-FitLine[[1]][[2]]
  }else{
    Data$Submissive_Intercept[i]<-NA 
    Data$Submissive_Slope[i]<-NA
  }
}

Data$Submissive_Intercept
Data$Submissive_Slope


Aggressive_Matrix<-cbind(Data$DefeatDay1_Aggressive, Data$DefeatDay2_Aggressive, Data$DefeatDay3_Aggressive, Data$DefeatDay4_Aggressive)
str(Aggressive_Matrix)
DefeatDay<-c(-3,-2,-1,0)

plot(Aggressive_Matrix[7,]~DefeatDay)
FitLine<-lm(Aggressive_Matrix[7,]~DefeatDay)
abline(FitLine)
FitLine[[1]][1]
FitLine[[1]][2]

Data$Aggressive_Intercept<-numeric(length(Aggressive_Matrix[,1]))
Data$Aggressive_Slope<-numeric(length(Aggressive_Matrix[,1]))

for(i in c(1:length(Aggressive_Matrix[,1]))){
  if(is.na(Aggressive_Matrix[i,1])==F){
    FitLine<-lm(Aggressive_Matrix[i,]~DefeatDay)
    Data$Aggressive_Intercept[i]<-FitLine[[1]][[1]]
    Data$Aggressive_Slope[i]<-FitLine[[1]][[2]]
  }else{
    Data$Aggressive_Intercept[i]<-NA 
    Data$Aggressive_Slope[i]<-NA
  }
}

Data$Aggressive_Intercept
Data$Aggressive_Slope

OtherBehavior_Matrix<-cbind(Data$DefeatDay1_OtherBehavior, Data$DefeatDay2_OtherBehavior, Data$DefeatDay3_OtherBehavior, Data$DefeatDay4_OtherBehavior)
str(OtherBehavior_Matrix)
DefeatDay<-c(-3,-2,-1,0)

plot(OtherBehavior_Matrix[7,]~DefeatDay)
FitLine<-lm(OtherBehavior_Matrix[7,]~DefeatDay)
abline(FitLine)
FitLine[[1]][1]
FitLine[[1]][2]

Data$OtherBehavior_Intercept<-numeric(length(OtherBehavior_Matrix[,1]))
Data$OtherBehavior_Slope<-numeric(length(OtherBehavior_Matrix[,1]))

for(i in c(1:length(OtherBehavior_Matrix[,1]))){
  if(is.na(OtherBehavior_Matrix[i,1])==F){
    FitLine<-lm(OtherBehavior_Matrix[i,]~DefeatDay)
    Data$OtherBehavior_Intercept[i]<-FitLine[[1]][[1]]
    Data$OtherBehavior_Slope[i]<-FitLine[[1]][[2]]
  }else{
    Data$OtherBehavior_Intercept[i]<-NA 
    Data$OtherBehavior_Slope[i]<-NA
  }
}

Data$OtherBehavior_Intercept
Data$OtherBehavior_Slope

TimeCaged_Matrix<-cbind(Data$DefeatDay1_TimeCaged, Data$DefeatDay2_TimeCaged, Data$DefeatDay3_TimeCaged, Data$DefeatDay4_TimeCaged)
str(TimeCaged_Matrix)
DefeatDay<-c(-3,-2,-1,0)

plot(TimeCaged_Matrix[7,]~DefeatDay)
FitLine<-lm(TimeCaged_Matrix[7,]~DefeatDay)
abline(FitLine)
FitLine[[1]][1]
FitLine[[1]][2]

Data$TimeCaged_Intercept<-numeric(length(TimeCaged_Matrix[,1]))
Data$TimeCaged_Slope<-numeric(length(TimeCaged_Matrix[,1]))

for(i in c(1:length(TimeCaged_Matrix[,1]))){
  if(is.na(TimeCaged_Matrix[i,1])==F){
    FitLine<-lm(TimeCaged_Matrix[i,]~DefeatDay)
    Data$TimeCaged_Intercept[i]<-FitLine[[1]][[1]]
    Data$TimeCaged_Slope[i]<-FitLine[[1]][[2]]
  }else{
    Data$TimeCaged_Intercept[i]<-NA 
    Data$TimeCaged_Slope[i]<-NA
  }
}

Data$TimeCaged_Intercept
Data$TimeCaged_Slope

DefeatScore_Matrix<-cbind(Data$DefeatDay1_DefeatScore, Data$DefeatDay2_DefeatScore, Data$DefeatDay3_DefeatScore, Data$DefeatDay4_DefeatScore)
str(DefeatScore_Matrix)
DefeatDay<-c(-3,-2,-1,0)

plot(DefeatScore_Matrix[7,]~DefeatDay)
FitLine<-lm(DefeatScore_Matrix[7,]~DefeatDay)
abline(FitLine)
FitLine[[1]][1]
FitLine[[1]][2]

Data$DefeatScore_Intercept<-numeric(length(DefeatScore_Matrix[,1]))
Data$DefeatScore_Slope<-numeric(length(DefeatScore_Matrix[,1]))

for(i in c(1:length(DefeatScore_Matrix[,1]))){
  if(is.na(DefeatScore_Matrix[i,1])==F){
    FitLine<-lm(DefeatScore_Matrix[i,]~DefeatDay)
    Data$DefeatScore_Intercept[i]<-FitLine[[1]][[1]]
    Data$DefeatScore_Slope[i]<-FitLine[[1]][[2]]
  }else{
    Data$DefeatScore_Intercept[i]<-NA 
    Data$DefeatScore_Slope[i]<-NA
  }
}

Data$DefeatScore_Intercept
Data$DefeatScore_Slope
