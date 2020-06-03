#For analysis purposes, it might be helpful to set the intercept as Day4, since we are missing Day4 information for some individuals and that day is most likely to reflect the cumulative effects of social defeat.
DefeatDay<-c(-3,-2,-1,0)

#********************************

#Time Series Data: Submissive_Log2

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

#Actual code fitting an intercept and slope to the time series data for each individual subject (rat):

Submissive_Log2_Matrix<-cbind(Data$DefeatDay1_Submissive_Log2, Data$DefeatDay2_Submissive_Log2, Data$DefeatDay3_Submissive_Log2, Data$DefeatDay4_Submissive_Log2)
#cbind = combine defeat day data for submissive data with log2 transformation
#rename combined data from cbind function
str(Submissive_Log2_Matrix)
#output the combined data

#Creates empty numeric vectors the same length as a column in the data matrix (i.e., length=number of subjects) to store results
Data$SubmissiveLog2_Intercept<-numeric(length(Submissive_Log2_Matrix[,1]))
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

#********************************

#Time Series Data: Aggressive_Log2

Aggressive_Log2_Matrix<-cbind(Data$DefeatDay1_Aggressive_Log2, Data$DefeatDay2_Aggressive_Log2, Data$DefeatDay3_Aggressive_Log2, Data$DefeatDay4_Aggressive_Log2)
#combine defeat days for aggressive data with log2 transformation
str(Aggressive_Log2_Matrix)
DefeatDay<-c(-3,-2,-1,0)
#set DefeatDay as vector

#Creates empty numeric vectors the same length as a column in the data matrix (i.e., length=number of subjects) to store results
Data$AggressiveLog2_Intercept<-numeric(length(Aggressive_Log2_Matrix[,1]))
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
