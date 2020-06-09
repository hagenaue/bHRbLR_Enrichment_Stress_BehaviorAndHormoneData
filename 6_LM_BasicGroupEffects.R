DataColumnsForStats<-c(10:21, 48:51, 59:63, 73:74)




for(i in DataColumnsForStats){
  
  OutputtingStats<-file(paste("LM_byTreatmentGroup_", colnames(Data)[i], ".txt", sep=""))
  out<-c(
    
  print(paste("Variable :", colnames(Data)[i], sep=" ")),
  
  print("******************************"),
  
  print("Traditional Linear Regression Model"),
  
  capture.output(summary.lm(lm(Data[,i]~Data$Line*Data$Enrichment*Data$Social_Defeat))),
  
  print("******************************"),
  
  print("Permutation-Based Linear Regression Model"),
  
  capture.output(summary(lmp(Data[,i]~Data$Line*Data$Enrichment*Data$Social_Defeat, perm="Prob", center=F, x=T, y=T, contrasts="contr.sum"))),
  
  print("******************************"),
  
  print("Traditional Linear Regression Model, w/ Generation Covariate"),
  
  capture.output(summary.lm(lm(Data[,i]~Data$Line*Data$Enrichment*Data$Social_Defeat+Data$Generation))),
  
  print("******************************"),
  
  print("Permutation-Based Linear Regression Model, w/ Generation Covariate"),
  
  capture.output(summary(lmp(Data[,i]~Data$Line*Data$Enrichment*Data$Social_Defeat+Data$Generation, perm="Prob", center=F, x=T, y=T, contrasts="contr.sum")))
  )
  cat(out, file=paste("LM_byTreatmentGroup_", colnames(Data)[i], ".txt", sep=""), sep="\n", append=TRUE)
  close(OutputtingStats)
  rm(out)
  
}
