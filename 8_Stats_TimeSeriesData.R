

#Code for running a multilevel regression model (MLM) to examine group differences in behavior during the four social defeat sessions
#MLM was chosen because the time-series data has a clear structure (slope) and it makes more sense to model this change with time than to assume that correlations between adjacent time points don't exist
#Also, the data for some individuals is incomplete (some rats only have 3 days worth of data)

#I have example code for one variable here (Defeat Score)

#We should change the code so that it outputs to a file instead of the console
#We should also change the code so that it is generalized and loop it over all of the time series variables in the data frame DefeatDays_LongVersion_OnlyDefeated



library(nlme)

#Model with intercept set at Day 4

Model<-lme(DefeatScore~Day_CenteredOn4*Line*Enrichment, random=~Day_CenteredOn4|RatID, data=DefeatDays_LongVersion_OnlyDefeated, correlation = corAR1(), control = lmeControl(opt = 'optim'))

summary(Model)

#Model with intercept set at Day 1

Model<-lme(DefeatScore~Day*Line*Enrichment, random=~Day|RatID, data=DefeatDays_LongVersion_OnlyDefeated, correlation = corAR1(), control = lmeControl(opt = 'optim'))

summary(Model)

#Model with intercept set at Day 4 w/ Generation as a co-variate:

Model<-lme(DefeatScore~Day_CenteredOn4*Line*Enrichment+Generation, random=~Day_CenteredOn4|RatID, data=DefeatDays_LongVersion_OnlyDefeated, correlation = corAR1(), control = lmeControl(opt = 'optim'))

summary(Model)

#Model with intercept set at Day 1 w/ Generation as a co-variate:

Model<-lme(DefeatScore~Day*Line*Enrichment+Generation, random=~Day|RatID, data=DefeatDays_LongVersion_OnlyDefeated, correlation = corAR1(), control = lmeControl(opt = 'optim'))

summary(Model)


#Side note: to parallel the other stats, it might make sense to use an anova summary instead of Lm output, e.g., car::Anova(Model, type="III") - but I still need to double-check that I'm coding it correctly.
