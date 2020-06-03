#For analysis purposes, it might be helpful to set the intercept as Day4, since we are missing Day4 information for some individuals and that day is most likely to reflect the cumulative effects of social defeat.
DefeatDay<-c(-3,-2,-1,0)

#Fitting an intercept and slope to the time series data:

Submissive_Log2_Matrix<-cbind(Data$DefeatDay1_Submissive_Log2, Data$DefeatDay2_Submissive_Log2, Data$DefeatDay3_Submissive_Log2, Data$DefeatDay4_Submissive_Log2)
#cbind = combine defeat day data for submissive data with log2 transformation
#rename combined data from cbind function
str(Submissive_Log2_Matrix)
#output the combined data

Data$SubmissiveLog2_Intercept<-numeric(length(Submissive_Log2_Matrix[,1]))
#makes combined data created above numeric
#rename numeric data for intercept
Data$SubmissiveLog2_Slope<-numeric(length(Submissive_Log2_Matrix[,1]))
#rename numeric data for slope

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
#is.na implies missing values
#is.na is set equal to false, implying no missing values
#if there are no missing values in the combined submissive data with log2 
  ##transformation,then set data to linear model with submissive data as 
  ###dependent variable and defeat day as independent variable
  #Set both the intercept and the slope to the linear model
#if there are missing values in the combined data for defeat days, 
  ##set missing elements to NA (remove from data set)

Aggressive_Log2_Matrix<-cbind(Data$DefeatDay1_Aggressive_Log2, Data$DefeatDay2_Aggressive_Log2, Data$DefeatDay3_Aggressive_Log2, Data$DefeatDay4_Aggressive_Log2)
#combine defeat days for aggressive data with log2 transformation
str(Aggressive_Log2_Matrix)
DefeatDay<-c(-3,-2,-1,0)
#set DefeatDay as vector

plot(Aggressive_Log2_Matrix[7,]~DefeatDay)
#plot defeat day on x axis and agressive data on y axis with log2 transformation, 2 points
FitLine<-lm(Aggressive_Log2_Matrix[7,]~DefeatDay)
#set linear model for aggressive data with log2 transformation as 
  ##dependent and defeat day as independent variable
abline(FitLine)
#add best fit line from linear model through the plot created above


Data$AggressiveLog2_Intercept<-numeric(length(Aggressive_Log2_Matrix[,1]))
#make aggressive intercept data with log2 transformarion numeric and rename
Data$AggressiveLog2_Slope<-numeric(length(Aggressive_Log2_Matrix[,1]))
#make aggreeive slope data with log2 transmormation numeric and rename


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
#if there are no missing values from the combined aggressive data with 
  ##log2 trasformation, then set data to linear model with aggressive data 
  ###as dependent variable and defeat day as independent variable
  #Set intercept and slope to linear model
#if there are missing values from the data, remove the missing values

Data$AggressiveLog2_Intercept
#view aggressive log2 intercept data; missing values = NA
Data$AggressiveLog2_Slope
#view aggressive log2 slope data; missing values = NA


Submissive_Matrix<-cbind(Data$DefeatDay1_Submissive, Data$DefeatDay2_Submissive, Data$DefeatDay3_Submissive, Data$DefeatDay4_Submissive)
#combine defeat day data for submissive data WITHOUT log transformation
str(Submissive_Matrix)
DefeatDay<-c(-3,-2,-1,0)
#set DefeatDay as vector

plot(Submissive_Matrix[7,]~DefeatDay)
#plot submissive data on y axis and defeat day on x axis, 3 points
FitLine<-lm(Submissive_Matrix[7,]~DefeatDay)
#set susbmissive data to linear model with defeat day as independent 
  ##variable and submissive data as dependent variable
abline(FitLine)
#add best fit line from linear model to plot created above
FitLine[[1]][1]
#get intercept
FitLine[[1]][2]
#get slope 

Data$Submissive_Intercept<-numeric(length(Submissive_Matrix[,1]))
#set submissive intercept data as numeric and rename
Data$Submissive_Slope<-numeric(length(Submissive_Matrix[,1]))
#set submissive slope data as numeric and rename

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
#if there are no missing values from the combined submissive data, then 
  ##set data to linear model with submissive data as dependent variable
  ###and defeat day as independent variable
  #Set both the intercept and slope data to the linear model
#if there are missing values from the data, remove them

Data$Submissive_Intercept
#Get intercept data; missing values = NA
Data$Submissive_Slope
#Get slope data; missing values = NA

Aggressive_Matrix<-cbind(Data$DefeatDay1_Aggressive, Data$DefeatDay2_Aggressive, Data$DefeatDay3_Aggressive, Data$DefeatDay4_Aggressive)
#combine defeat days for aggressive data WITHOUT log transformation
str(Aggressive_Matrix)
DefeatDay<-c(-3,-2,-1,0)
#set DefeatDay as vector

plot(Aggressive_Matrix[7,]~DefeatDay)
#plot agressive data on y axis and defeat day on x axis, 3 points
FitLine<-lm(Aggressive_Matrix[7,]~DefeatDay)
#set data to linear model with aggressive data as dependent variable and
  ##defeat day as independent variable
abline(FitLine)
#add best fit line from linear model to plot created above
FitLine[[1]][1]
#Get intercept
FitLine[[1]][2]
#Get slope

Data$Aggressive_Intercept<-numeric(length(Aggressive_Matrix[,1]))
#make agressive intercept data numeric and rename
Data$Aggressive_Slope<-numeric(length(Aggressive_Matrix[,1]))
#make agressive slope data numeric and rename

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
#if there are no missing values from the combined agressive data, then 
  ##set data to linear model with agressive data as dependent variable
  ###and defeat day as independent variable
  #Set both the intercept and slope data to the linear model
#if there are missing values from the data, remove them

Data$Aggressive_Intercept
#get agressive intercept data; missing values = NA
Data$Aggressive_Slope
#get agressive slope data; missing values = NA

OtherBehavior_Matrix<-cbind(Data$DefeatDay1_OtherBehavior, Data$DefeatDay2_OtherBehavior, Data$DefeatDay3_OtherBehavior, Data$DefeatDay4_OtherBehavior)
#combine defeat days for other behavior data
str(OtherBehavior_Matrix)
DefeatDay<-c(-3,-2,-1,0)
#make DefeatDay a vector

plot(OtherBehavior_Matrix[7,]~DefeatDay)
#plot other behavior data on y axis and defeat days on x axis, 3 points
FitLine<-lm(OtherBehavior_Matrix[7,]~DefeatDay)
#set data to linear model with other behavior data as dependent variable
  ##and defeat days as independent variable
abline(FitLine)
#add line of best fit from linear model to plot created above
FitLine[[1]][1]
#get intercept
FitLine[[1]][2]
#get slope

Data$OtherBehavior_Intercept<-numeric(length(OtherBehavior_Matrix[,1]))
#make other behavior intercept data numeric and rename
Data$OtherBehavior_Slope<-numeric(length(OtherBehavior_Matrix[,1]))
#make other behavior slope data numeric and rename

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
#if there are no missing values from the combined other behavior data, then 
  ##set data to linear model with other behavior data as dependent variable
  ###and defeat day as independent variable
  #Set both the intercept and slope data to the linear model
#if there are missing values from the data, remove them

Data$OtherBehavior_Intercept
#get intercept data, missing values = NA
Data$OtherBehavior_Slope
#get slope data, missing values = NA

TimeCaged_Matrix<-cbind(Data$DefeatDay1_TimeCaged, Data$DefeatDay2_TimeCaged, Data$DefeatDay3_TimeCaged, Data$DefeatDay4_TimeCaged)
#combine defeat days for time caged data and rename
str(TimeCaged_Matrix)
DefeatDay<-c(-3,-2,-1,0)
#set defeat day as vector

plot(TimeCaged_Matrix[7,]~DefeatDay)
#plot time caged data on y axis and defeat days on x axis, 4 points
FitLine<-lm(TimeCaged_Matrix[7,]~DefeatDay)
#set data to linear model with time caged data as dependent variable
  ##and defeat day as independent variable
abline(FitLine)
#add best fit line from linear model to plot created above
FitLine[[1]][1]
#get intercept
FitLine[[1]][2]
#get slope

Data$TimeCaged_Intercept<-numeric(length(TimeCaged_Matrix[,1]))
#make time caged intercept data numeric and rename
Data$TimeCaged_Slope<-numeric(length(TimeCaged_Matrix[,1]))
#make time caged slope data numeric and rename

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
#if there are no missing values from the combined time caged data, then 
  ##set data to linear model with time caged data as dependent variable
  ###and defeat day as independent variable
  #Set both the intercept and slope data to the linear model
#if there are missing values from the data, remove them

Data$TimeCaged_Intercept
#get time caged intercept data; missing values = NA
Data$TimeCaged_Slope
#get time caged slope data; missing values = NA

DefeatScore_Matrix<-cbind(Data$DefeatDay1_DefeatScore, Data$DefeatDay2_DefeatScore, Data$DefeatDay3_DefeatScore, Data$DefeatDay4_DefeatScore)
#combine defeat days for defeat score data and rename
str(DefeatScore_Matrix)
DefeatDay<-c(-3,-2,-1,0)
#make DefeatDay a vector

plot(DefeatScore_Matrix[7,]~DefeatDay)
#plot defeat score data on y axis and defeat day on x axis, 4 points
##first 3 points very linear near 4.0, last point drastically different near 2.0
FitLine<-lm(DefeatScore_Matrix[7,]~DefeatDay)
#create linear model with defeat score as dependent variable and defeat
  ##day as independent variable
abline(FitLine)
#add best fit line from linear model to plot created above
FitLine[[1]][1]
#get intercept
FitLine[[1]][2]
#get slope

Data$DefeatScore_Intercept<-numeric(length(DefeatScore_Matrix[,1]))
#set defeat score intercept data as numeric and rename
Data$DefeatScore_Slope<-numeric(length(DefeatScore_Matrix[,1]))
#set defeat score slope data as numeric and rename

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
#if there are no missing values from the combined defeat score data, then 
  ##set data to linear model with defeat score data as dependent variable
  ###and defeat day as independent variable
  #Set both the intercept and slope data to the linear model
#if there are missing values from the data, remove them

Data$DefeatScore_Intercept
#get defeat score intercept data; missing values = NA
Data$DefeatScore_Slope
#get defeat score intercept data; missing values = NA
