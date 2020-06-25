#Code for running a multilevel regression model (MLM) to examine group differences in behavior during the four social defeat sessions
#MLM was chosen because the time-series data has a clear structure (slope) and it makes more sense to model this change with time than to assume that correlations between adjacent time points don't exist
#Also, the data for some individuals is incomplete (some rats only have 3 days worth of data)

#Liam's generalized code for running a multilevel regression model (MLM) to examine group differences in behavior during the four social defeat sessions
#Loops through all of the time series variables in the data frame DefeatDays_LongVersion_OnlyDefeated


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
    
    print(paste("******************************************************************************", "", "Summary of model with intercept set at Day 4 w/ Generation as a co-variate:", "", sep="\n")),
    
    capture.output(summary(lme(Temp~Day_CenteredOn4*Line*Enrichment+Generation, random=~Day_CenteredOn4|RatID, data=DefeatDays_LongVersion_OnlyDefeated, correlation = corAR1(), na.action = na.omit, control = lmeControl(opt = 'optim')))),
    
    print(paste("******************************************************************************", "", "Summary of model with intercept set at Day 1 w/ Generation as a co-variate:", "", sep="\n")),
    
    capture.output(summary(lme(Temp~Day*Line*Enrichment+Generation, random=~Day|RatID, data=DefeatDays_LongVersion_OnlyDefeated, correlation = corAR1(), na.action = na.omit, control = lmeControl(opt = 'optim')))),
    
    print(paste("", "******************************************************************************","", "ANOVA Summary of model with intercept set at Day 4:", "", sep="\n")),
    
    capture.output(car::Anova(lme(Temp~Day_CenteredOn4*Line*Enrichment, random=~Day_CenteredOn4|RatID, data=DefeatDays_LongVersion_OnlyDefeated, correlation = corAR1(), na.action=na.omit, method="ML", control = lmeControl(opt = 'optim'), contrasts=list(Line=contr.sum, Enrichment=contr.sum)), type="III")),
    
    print(paste("", "******************************************************************************","", "ANOVA Summary of model with intercept set at Day 1:", "", sep="\n")),
    
    capture.output(car::Anova(lme(Temp~Day*Line*Enrichment, random=~Day|RatID, data=DefeatDays_LongVersion_OnlyDefeated, correlation = corAR1(), na.action=na.omit, method="ML", control = lmeControl(opt = 'optim'), contrasts=list(Line=contr.sum, Enrichment=contr.sum)), type="III")),
    
    print(paste("", "******************************************************************************","", "ANOVA Summary of model with intercept set at Day 4 w/ Generation as a co-variate:", "", sep="\n")),
    
    capture.output(car::Anova(lme(Temp~Day_CenteredOn4*Line*Enrichment+Generation, random=~Day_CenteredOn4|RatID, data=DefeatDays_LongVersion_OnlyDefeated, correlation = corAR1(), na.action=na.omit, method="ML", control = lmeControl(opt = 'optim'), contrasts=list(Line=contr.sum, Enrichment=contr.sum, Generation=contr.sum)), type="III")),
    
    print(paste("", "******************************************************************************","", "ANOVA Summary of model with intercept set at Day 1 w/ Generation as a co-variate:", "", sep="\n")),
    
    capture.output(car::Anova(lme(Temp~Day*Line*Enrichment+Generation, random=~Day|RatID, data=DefeatDays_LongVersion_OnlyDefeated, correlation = corAR1(), na.action=na.omit, method="ML", control = lmeControl(opt = 'optim'), contrasts=list(Line=contr.sum, Enrichment=contr.sum, Generation=contr.sum)), type="III")),
    
    print("******************************************************************************")
  )
  
  cat(stats_output, file=paste("MLM_byTreatmentGroup_", colnames(DefeatDays_LongVersion_OnlyDefeated)[i], ".txt", sep=""), sep="\n", append=TRUE)
  
  close(OutputtingStats)
  
  rm(stats_output)
  rm(current_time_variable)
  
  DefeatDays_LongVersion_OnlyDefeated$Temp <- NULL
}

#******************************

#Scrap code used to troubleshoot ANOVA output:


#Playing with the creation of an ANOVA-type summary for the MLM stats:

Model<-lme(Submissive_Log2~Day_CenteredOn4*Line*Enrichment+Generation, random=~Day_CenteredOn4|RatID, data=DefeatDays_LongVersion_VideoRecorded, correlation = corAR1(), na.action=na.omit)

summary(Model)

# Linear mixed-effects model fit by REML
# Data: DefeatDays_LongVersion_VideoRecorded 
# AIC      BIC    logLik
# 942.7892 1008.113 -452.3946
# 
# Random effects:
#   Formula: ~Day_CenteredOn4 | RatID
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev      Corr  
# (Intercept)     0.001003389 (Intr)
# Day_CenteredOn4 0.234732967 0.44  
# Residual        1.484125136       
# 
# Correlation Structure: AR(1)
# Formula: ~1 | RatID 
# Parameter estimate(s):
#   Phi 
# -0.1936363 
# Fixed effects: Submissive_Log2 ~ Day_CenteredOn4 * Line * Enrichment + Generation 
# Value Std.Error  DF   t-value p-value
# (Intercept)                           2.7383654 0.4761199 168  5.751420  0.0000
# Day_CenteredOn4                       0.5967017 0.2154464 168  2.769606  0.0062
# LinebLR                               1.6451649 0.5578699  62  2.949012  0.0045
# EnrichmentEC                         -1.6283646 0.5519682  62 -2.950106  0.0045
# EnrichmentEE                         -1.7501313 0.5450878  62 -3.210733  0.0021
# GenerationF49                         1.7170177 0.2649083  62  6.481555  0.0000
# GenerationF56                         1.6457458 0.2747002  62  5.991061  0.0000
# Day_CenteredOn4:LinebLR               0.1907199 0.3034797 168  0.628444  0.5306
# Day_CenteredOn4:EnrichmentEC         -0.4073471 0.3017587 168 -1.349910  0.1789
# Day_CenteredOn4:EnrichmentEE         -0.6017853 0.2923108 168 -2.058717  0.0411
# LinebLR:EnrichmentEC                  1.7932171 0.7566185  62  2.370041  0.0209
# LinebLR:EnrichmentEE                  1.4048545 0.7393852  62  1.900030  0.0621
# Day_CenteredOn4:LinebLR:EnrichmentEC  0.5133541 0.4280409 168  1.199311  0.2321
# Day_CenteredOn4:LinebLR:EnrichmentEE  0.4662382 0.4156103 168  1.121816  0.2635


car::Anova(Model)

# Analysis of Deviance Table (Type II tests)
# 
# Response: Submissive_Log2
# Chisq Df Pr(>Chisq)    
# Day_CenteredOn4                  34.1997  1  4.974e-09 ***
#   Line                            120.1193  1  < 2.2e-16 ***
#   Enrichment                        5.1864  2   0.074781 .  
# Generation                       46.8996  2  6.545e-11 ***
#   Day_CenteredOn4:Line              9.4060  1   0.002163 ** 
#   Day_CenteredOn4:Enrichment        3.2415  2   0.197749    
# Line:Enrichment                   5.5576  2   0.062113 .  
# Day_CenteredOn4:Line:Enrichment   1.7812  2   0.410401    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

car::Anova(Model, type="II")

# Analysis of Deviance Table (Type II tests)
# 
# Response: Submissive_Log2
# Chisq Df Pr(>Chisq)    
# Day_CenteredOn4                  34.1997  1  4.974e-09 ***
#   Line                            120.1193  1  < 2.2e-16 ***
#   Enrichment                        5.1864  2   0.074781 .  
# Generation                       46.8996  2  6.545e-11 ***
#   Day_CenteredOn4:Line              9.4060  1   0.002163 ** 
#   Day_CenteredOn4:Enrichment        3.2415  2   0.197749    
# Line:Enrichment                   5.5576  2   0.062113 .  
# Day_CenteredOn4:Line:Enrichment   1.7812  2   0.410401    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


car::Anova(Model, type="III")
  

# Analysis of Deviance Table (Type III tests)
# 
# Response: Submissive_Log2
# Chisq Df Pr(>Chisq)    
# (Intercept)                     33.0788  1  8.850e-09 ***
#   Day_CenteredOn4                  7.6707  1   0.005612 ** 
#   Line                             8.6967  1   0.003188 ** 
#   Enrichment                      12.1419  2   0.002309 ** 
#   Generation                      46.8996  2  6.545e-11 ***
#   Day_CenteredOn4:Line             0.3949  1   0.529713    
# Day_CenteredOn4:Enrichment       4.3381  2   0.114288    
# Line:Enrichment                  6.1061  2   0.047216 *  
#   Day_CenteredOn4:Line:Enrichment  1.7812  2   0.410401    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#https://www.researchgate.net/post/Problems_performing_ANOVA_and_Mixed_Linear_model_on_R
#https://www.researchgate.net/post/REML_FALSE_versus_REML_TRUE_lme4_package_in_R-any_thoughts
#Apparently REML (which is the standard for evaluating multilevel models) shouldn't be used in combination with anova, but anova() function may automatically correct it (make it ML)
#Let's check:

Model<-lme(Submissive_Log2~Day_CenteredOn4*Line*Enrichment+Generation, random=~Day_CenteredOn4|RatID, data=DefeatDays_LongVersion_VideoRecorded, correlation = corAR1(), na.action=na.omit, method="ML")

car::Anova(Model, type="III")

# Analysis of Deviance Table (Type III tests)
# 
# Response: Submissive_Log2
# Chisq Df Pr(>Chisq)    
# (Intercept)                     36.0780  1  1.896e-09 ***
#   Day_CenteredOn4                  8.2357  1   0.004107 ** 
#   Line                             9.6025  1   0.001943 ** 
#   Enrichment                      13.0350  2   0.001477 ** 
#   Generation                      52.7122  2  3.578e-12 ***
#   Day_CenteredOn4:Line             0.4464  1   0.504055    
# Day_CenteredOn4:Enrichment       4.7188  2   0.094479 .  
# Line:Enrichment                  6.4014  2   0.040734 *  
#   Day_CenteredOn4:Line:Enrichment  1.8990  2   0.386934    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Interesting - so anova doesn't automatically convert lme() output to "ML" the way it does for lme4 output.

#More things to double-check: how the contrasts are defined:

Model<-lme(Submissive_Log2~Day_CenteredOn4*Line*Enrichment+Generation, random=~Day_CenteredOn4|RatID, data=DefeatDays_LongVersion_VideoRecorded, correlation = corAR1(), na.action=na.omit, method="ML", contrasts=list(Line=contr.sum, Enrichment=contr.sum, Generation=contr.sum))

car::Anova(Model, type="III")


# Analysis of Deviance Table (Type III tests)
# 
# Response: Submissive_Log2
# Chisq Df Pr(>Chisq)    
# (Intercept)                     745.8060  1  < 2.2e-16 ***
#   Day_CenteredOn4                  39.5503  1  3.197e-10 ***
#   Line                             88.9520  1  < 2.2e-16 ***
#   Enrichment                        8.2125  2   0.016470 *  
#   Generation                       52.7122  2  3.578e-12 ***
#   Day_CenteredOn4:Line              9.9652  1   0.001595 ** 
#   Day_CenteredOn4:Enrichment        3.4408  2   0.178996    
# Line:Enrichment                   6.4014  2   0.040734 *  
#   Day_CenteredOn4:Line:Enrichment   1.8990  2   0.386934    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Model<-lme(Submissive_Log2~Day_CenteredOn4*Line*Enrichment+Generation, random=~Day_CenteredOn4|RatID, data=DefeatDays_LongVersion_VideoRecorded, correlation = corAR1(), na.action=na.omit, method="ML", contrasts=list(Line=contr.treatment, Enrichment=contr.treatment, Generation=contr.treatment))

car::Anova(Model, type="III")

# Analysis of Deviance Table (Type III tests)
# 
# Response: Submissive_Log2
# Chisq Df Pr(>Chisq)    
# (Intercept)                     36.0780  1  1.896e-09 ***
#   Day_CenteredOn4                  8.2357  1   0.004107 ** 
#   Line                             9.6025  1   0.001943 ** 
#   Enrichment                      13.0350  2   0.001477 ** 
#   Generation                      52.7122  2  3.578e-12 ***
#   Day_CenteredOn4:Line             0.4464  1   0.504055    
# Day_CenteredOn4:Enrichment       4.7188  2   0.094479 .  
# Line:Enrichment                  6.4014  2   0.040734 *  
#   Day_CenteredOn4:Line:Enrichment  1.8990  2   0.386934    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#Checking on whether the centering for day matters in the anova summary. Note - I had to remove generation because I was getting convergence errors for the version that included day instead of day centered on 4.

Model<-lme(Submissive_Log2~Day_CenteredOn4*Line*Enrichment, random=~Day_CenteredOn4|RatID, data=DefeatDays_LongVersion_VideoRecorded, correlation = corAR1(), na.action=na.omit, method="ML", contrasts=list(Line=contr.sum, Enrichment=contr.sum))

car::Anova(Model, type="III")

# Analysis of Deviance Table (Type III tests)
# 
# Response: Submissive_Log2
# Chisq Df Pr(>Chisq)    
# (Intercept)                     619.4421  1  < 2.2e-16 ***
#   Day_CenteredOn4                  36.9749  1  1.197e-09 ***
#   Line                             62.6864  1  2.424e-15 ***
#   Enrichment                       13.1563  2   0.001390 ** 
#   Day_CenteredOn4:Line             10.3218  1   0.001315 ** 
#   Day_CenteredOn4:Enrichment        3.3352  2   0.188698    
# Line:Enrichment                   4.7656  2   0.092292 .  
# Day_CenteredOn4:Line:Enrichment   1.9097  2   0.384873    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Model<-lme(Submissive_Log2~Day*Line*Enrichment, random=~Day|RatID, data=DefeatDays_LongVersion_VideoRecorded, correlation = corAR1(), na.action=na.omit, method="ML", contrasts=list(Line=contr.sum, Enrichment=contr.sum))

car::Anova(Model, type="III")

# Analysis of Deviance Table (Type III tests)
# 
# Response: Submissive_Log2
# Chisq Df Pr(>Chisq)    
# (Intercept)         277.4060  1  < 2.2e-16 ***
#   Day                  36.9749  1  1.197e-09 ***
#   Line                 11.2895  1  0.0007795 ***
#   Enrichment            3.9765  2  0.1369360    
# Day:Line             10.3218  1  0.0013147 ** 
#   Day:Enrichment        3.3352  2  0.1886981    
# Line:Enrichment       0.0576  2  0.9716094    
# Day:Line:Enrichment   1.9097  2  0.3848731    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#So the default settings for car::Anova are contr.treatment with a type II (sequential) test. The centering for numeric variables or co-variates matters for interepreting the results. And it doesn't not automatically re-do lme() testing as "ML" before performing the anova comparison. 
#All useful to know.
