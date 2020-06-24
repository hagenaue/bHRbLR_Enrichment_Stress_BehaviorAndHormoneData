#Code for running a multilevel regression model (MLM) to examine group differences in behavior during the four social defeat sessions
#MLM was chosen because the time-series data has a clear structure (slope) and it makes more sense to model this change with time than to assume that correlations between adjacent time points don't exist
#Also, the data for some individuals is incomplete (some rats only have 3 days worth of data)

#Liam's generalized code for running a multilevel regression model (MLM) to examine group differences in behavior during the four social defeat sessions
#Loops through all of the time series variables in the data frame DefeatDays_LongVersion_OnlyDefeated

library(nlme)

time_series_variable_names <- c("TimeCaged", "DefeatScore", "Submissive", "Aggressive", "OtherBehavior", "Submissive_Log2", "Aggressive_Log2")

for(i in 8:14) {
  
  current_time_variable <- unlist(data.frame(DefeatDays_LongVersion_OnlyDefeated[i]))
  
  DefeatDays_LongVersion_OnlyDefeated$Temp <- current_time_variable
  
  
  stats_output <- c(
    
    print(paste("\n", "Multilevel Regression Model for ", time_series_variable_names[i-7], "\n", sep="")),
    
    print(paste("", "Summary of model with intercept set at Day 4:", "\n", sep="\n")),
    
    capture.output(summary(lme(Temp~Day_CenteredOn4*Line*Enrichment, random=~Day_CenteredOn4|RatID, data=DefeatDays_LongVersion_OnlyDefeated, correlation = corAR1(), na.action = na.omit, control = lmeControl(opt = 'optim')))),
    
    print(paste("******************************************************************************", "", "Summary of model with intercept set at Day 1:", "", sep="\n")),
    
    capture.output(summary(lme(Temp~Day*Line*Enrichment, random=~Day|RatID, data=DefeatDays_LongVersion_OnlyDefeated, correlation = corAR1(), na.action = na.omit, control = lmeControl(opt = 'optim')))),
    
    print(paste("******************************************************************************", "", "Summary of model with intercept set at Day 4 w/ Generation as a co-variate:", "", sep="\n")),
    
    capture.output(summary(lme(Temp~Day_CenteredOn4*Line*Enrichment+Generation, random=~Day_CenteredOn4|RatID, data=DefeatDays_LongVersion_OnlyDefeated, correlation = corAR1(), na.action = na.omit, control = lmeControl(opt = 'optim')))),
    
    print(paste("******************************************************************************", "", "Summary of model with intercept set at Day 1 w/ Generation as a co-variate:", "", sep="\n")),
    
    capture.output(summary(lme(Temp~Day*Line*Enrichment+Generation, random=~Day|RatID, data=DefeatDays_LongVersion_OnlyDefeated, correlation = corAR1(), na.action = na.omit, control = lmeControl(opt = 'optim')))),
    
    print(paste("", "******************************************************************************", sep="\n")),
    
    print("******************************************************************************")
  )
  
  cat(stats_output, file="Multilevel Regression Model Stats for Time Series Data.txt", sep="\n", append=TRUE)
  
  rm(stats_output)
  rm(current_time_variable)
  
  DefeatDays_LongVersion_OnlyDefeated$Temp <- NULL
}


#Side note: to parallel the other stats, it might make sense to use an anova summary instead of Lm output, e.g., car::Anova(Model, type="III") - but I still need to double-check that I'm coding it correctly.
