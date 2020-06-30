

bLRData<-Data[Data$Line=="bLR",]
bHRData<-Data[Data$Line=="bHR",]

TempCol<-as.character(Data$Treatment_Group)
TempPch<-as.character(Data$Treatment_Group)


#Color codes the graphs so that they match the colors used for the enrichment groups in the boxplots used for other variables:  
TempCol[TempCol=="HR NIL + SD"]<-"green3"
TempCol[TempCol=="HR EC + SD"]<-"forestgreen"
TempCol[TempCol=="HR EE + SD"]<-"darkgreen"
TempCol[TempCol=="LR NIL + SD"]<-"red1"
TempCol[TempCol=="LR EC + SD"]<-"red3"
TempCol[TempCol=="LR EE + SD"]<-"red4"

TempCol[TempCol=="HR NIL"]<-"green3"
TempCol[TempCol=="HR EC"]<-"forestgreen"
TempCol[TempCol=="HR EE"]<-"darkgreen"
TempCol[TempCol=="LR NIL"]<-"red1"
TempCol[TempCol=="LR EC"]<-"red3"
TempCol[TempCol=="LR EE"]<-"red4"

#to reuse older formatting code:
Data$LineColor<-TempCol

TempPch[TempPch=="HR NIL + SD"]<-16
TempPch[TempPch=="HR EC + SD"]<-15
TempPch[TempPch=="HR EE + SD"]<-17
TempPch[TempPch=="LR NIL + SD"]<-16
TempPch[TempPch=="LR EC + SD"]<-15
TempPch[TempPch=="LR EE + SD"]<-17

TempPch[TempPch=="HR NIL"]<-1
TempPch[TempPch=="HR EC"]<-0
TempPch[TempPch=="HR EE"]<-2
TempPch[TempPch=="LR NIL"]<-1
TempPch[TempPch=="LR EC"]<-0
TempPch[TempPch=="LR EE"]<-2

TempPch<-as.numeric(TempPch)

#to reuse older formatting code:
Data$TreatmentPch<-TempPch


#Outputting scatterplots between all numeric variables, coded by treatment group:

colnames(Data[,c(10:21)])

#Reusing code from the boxplots:
DataColumnsForBoxplots_AllGroups<-c(10:21)
YLabelsForBoxplots_AllGroups<-c("distance (cm)", "% time center", "% time open arms", "% time avoiding (Ethovision)", "% time interacting (Ethovision)", "% time other behavior (Ethovision)", "% time on top (Ethovision)", "% time on top (video)", "% time approaching stimulus animal (video)", "% time other behavior (video)", "% 18-32kHz", "% 32-96kHz")

DataColumnsForBoxplots_Hormones<-c(48:51)
YLabelsForBoxplots_Hormones<-c("plasma corticosterone (pg/mL)", "plasma testosterone (pg/mL)", "plasma oxytocin (pg/mL)", "plasma IL-6 (pg/mL)")
LimitOfDetection_Hormones<-c(12.8, 30.6, 15.5, 20)


for(i in DataColumnsForBoxplots_AllGroups){
  for(j in DataColumnsForBoxplots_AllGroups){
    
    pdf(paste("Scatterplot_", colnames(Data)[i], "vs", colnames(Data)[j], ".pdf", sep=""), width=6, height=6)
    
    plot(Data[,i]~Data[,j], col=Data$LineColor, pch=Data$TreatmentPch, cex.lab=1.3,
         ylab=YLabelsForBoxplots_AllGroups[i-9], 
         xlab=YLabelsForBoxplots_AllGroups[j-9])
    
    LRmodel<-lm(bLRData[,i]~bLRData[,j], data=bLRData)
    abline(LRmodel, col="red3", lwd=1.5)
    
    HRmodel<-lm(bHRData[,i]~bHRData[,j], data=bHRData)
    abline(HRmodel, col="forestgreen", lwd=1.5)
    
    Allmodel<-lm(Data[,i]~Data[,j], data=Data)
    abline(Allmodel, col="black", lwd=3)
    
    dev.off()
  }
  
  for(k in DataColumnsForBoxplots_Hormones){
    
    pdf(paste("Scatterplot_", colnames(Data)[k], "vs", colnames(Data)[i], ".pdf", sep=""), width=6, height=6)
    
    plot(Data[,k]~Data[,i], col=Data$LineColor, pch=Data$TreatmentPch, cex.lab=1.3,
         ylab=YLabelsForBoxplots_Hormones[k-47], 
         xlab=YLabelsForBoxplots_AllGroups[i-9])
    
    LRmodel<-lm(bLRData[,k]~bLRData[,i], data=bLRData)
    abline(LRmodel, col="red3", lwd=1.5)
    
    HRmodel<-lm(bHRData[,k]~bHRData[,i], data=bHRData)
    abline(HRmodel, col="forestgreen", lwd=1.5)
    
    Allmodel<-lm(Data[,k]~Data[,i], data=Data)
    abline(Allmodel, col="black", lwd=3)
    
    abline(a=LimitOfDetection_Hormones[k-47], b=0, lty=3, lwd=2)
    
    dev.off()
  }
}
    
