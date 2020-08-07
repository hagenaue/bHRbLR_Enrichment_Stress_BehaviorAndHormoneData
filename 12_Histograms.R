DataColumnsForHistograms_AllGroups <- c(10:21)

XLabelsForHistograms_AllGroups<-c("Distance (cm)", "% time center", 
"% time open arms", "% time avoiding (Ethovision)", "% time interacting 
(Ethovision)", "% time other behavior (Ethovision)", "% time on top 
(Ethovision)", "% time on top (video)", "% time approaching stimulus 
animal (video)", "% time other behavior (video)", "% 18-32kHz", "% 32-96kHz")

for(i in DataColumnsForHistograms_AllGroups) {
  
  Temp <- Data[is.na(Data[,i])==F,]
  
  pdf(paste("Histograms", colnames(Temp)[i], 
            ".pdf", sep=""), height=5, width=5)
  hist(Temp[,i], xlab=XLabelsForHistograms_AllGroups[i-9], col="lightblue")
  
  dev.off()
  rm(Temp)
}


DataColumnsForHistograms_Hormones <- c(48:51)

XLabelsForHistograms_Hormones<-c("plasma corticosterone (pg/mL)", 
"plasma testosterone (pg/mL)", "plasma oxytocin (pg/mL)", 
"plasma IL-6 (pg/mL)")

LimitOfDetection_Hormones<-c(12.8, 30.6, 15.5, 20)


for(i in DataColumnsForHistograms_Hormones) {
  
  Temp <- Data[is.na(Data[,i])==F,]
  
  pdf(paste("Histograms",colnames(Temp)[i],
            ".pdf", sep=""), height=5, width=5)
  hist(Temp[,i], xlab=XLabelsForHistograms_Hormones[i-9], col="lightblue")
  
  dev.off()
  rm(Temp)
}
