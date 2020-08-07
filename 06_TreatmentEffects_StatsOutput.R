

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


DataColumnsForStats<-c(10:21, 48:51, 59:63, 73:74)


setwd("~/Documents/Microarray Gen/Angela_HRLR_EE_Stress/Behav_Analysis_20200518/LM_Output")

for(i in DataColumnsForStats){
  
  Temp<-Data[is.na(Data[,i])==F,]
  
  Temp$Enrichment_Factor<-droplevels(Temp$Enrichment_Factor)
  Temp$Generation<-droplevels(Temp$Generation)
  
  
  OutputtingStats<-file(paste("LM_byTreatmentGroup_", colnames(Temp)[i], ".txt", sep=""))
  out<-c(
    
  print(paste("Variable :", colnames(Temp)[i], sep=" ")),
  
  # print("******************************"),
  # 
  # print("Traditional Linear Regression Model"),
  # 
  # capture.output(summary.lm(lm(Temp[,i]~Line_Factor*Enrichment_Factor*Social_Defeat_Factor, data=Temp))),
  # 
  print("******************************"),
  
  print("Traditional & Permutation-Based Linear Regression Model"),
  
  capture.output(summary(lmperm(Temp[,i]~Line_Factor*Enrichment_Factor*Social_Defeat_Factor, data = Temp, np=15000, method="freedman_lane"))),
  
  # print("******************************"),
  # 
  # print("Traditional Linear Regression Model, w/ Generation Covariate"),
  # 
  # capture.output(summary.lm(lm(Temp[,i]~Line_Factor*Enrichment_Factor*Social_Defeat_Factor+Generation, data=Temp))),
  # 
  print("******************************"),
  
  print("Traditional & Permutation-Based Linear Regression Model, w/ Generation Covariate"),
  
  capture.output(summary(lmperm(Temp[,i]~Line_Factor*Enrichment_Factor*Social_Defeat_Factor+Generation, data = Temp, np=15000, method="freedman_lane"))),
  
  
  print("******************************"),
  
  print("Traditional & Permutation-Based ANOVA (contrasts=contr.sum)"),
  
  capture.output(aovperm(Temp[,i]~Line_Factor*Enrichment_Factor*Social_Defeat_Factor, data = Temp, contrasts="contr.sum", np=15000, method="freedman_lane")), 
  
  print("******************************"),
  
  print("Traditional & Permutation-Based ANOVA (contrasts=contr.sum), w/ Generation Covariate"),
  
  capture.output(aovperm(Temp[,i]~Line_Factor*Enrichment_Factor*Social_Defeat_Factor+Generation, data = Temp, contrasts="contr.sum", np=15000, method="freedman_lane")),
  
  
  print("******************************"),
  
  print("Permutation-Based T-Test Post-Hoc Comparisons (FDR-Corrected)"),
  
  capture.output(PT = pairwisePermutationTest(EPM_Explore_Open_Arms~Treatment_Group, data = Temp, method = "fdr"))
  
  )
  cat(out, file=paste("LM_byTreatmentGroup_", colnames(Temp)[i], ".txt", sep=""), sep="\n", append=TRUE)
  close(OutputtingStats)
  rm(out)
  rm(Temp)
  
}


######################

#Also: Outputting sample sizes for each variable in the dataset for reporting statistics:

colnames(Data)

setwd("~/Documents/Microarray Gen/Angela_HRLR_EE_Stress/Behav_Analysis_20200518")

SampleSizeForEachVariable<-apply(Data[,c(10:90)], 2, function(y) sum(is.na(y)==F))
write.csv(SampleSizeForEachVariable, "SampleSizeForEachVariable.csv")
