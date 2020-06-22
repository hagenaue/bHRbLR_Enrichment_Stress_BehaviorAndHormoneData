
#General Decoder for Symbols:

#No Defeat=Open, Defeated=Filled Black

Data$TreatmentPch[Data$Enrichment=="EE" & Data$Social_Defeat=="SD"]<-17
Data$TreatmentPch[Data$Enrichment=="EE" & Data$Social_Defeat=="NIL"]<-2

Data$TreatmentPch[Data$Enrichment=="EC" & Data$Social_Defeat=="SD"]<-15
Data$TreatmentPch[Data$Enrichment=="EC" & Data$Social_Defeat=="NIL"]<-0

Data$TreatmentPch[Data$Enrichment=="NIL" & Data$Social_Defeat=="SD"]<-16
Data$TreatmentPch[Data$Enrichment=="NIL" & Data$Social_Defeat=="NIL"]<-1

#General Decoder for Colors:
#LR levels of enrichment: NIL: "green3", EC: "forestgreen", EE: "darkgreen"
#HR levels of enrichment: NIL: "red1", EC: "red3", EE: "red4" 

#To Do:
#I vectorized the y-axis labels to match Angela's preferences, but some of her variables didn't have label preferences - double check.
#The treatment group labels should probably be made into some sort of info graphic. They're too much.
#How the heck are we going to mark off "significance"?  It feels overwhelming.
#Double-check limits of detection for the hormone plots with Angela - esp. IL6
#IL6 needs a dividing line calling out the fact that HR and LR are separate batches. Just do in graphics program.


setwd("~/Documents/Microarray Gen/Angela_HRLR_EE_Stress/Behav_Analysis_20200518/Boxplots")

colnames(Data)

colnames(Data[,c(10:21)])

DataColumnsForBoxplots_AllGroups<-c(10:21)
YLabelsForBoxplots_AllGroups<-c("Distance (cm)", "% time center", "% time open arms", "% time avoiding (Ethovision)", "% time interacting (Ethovision)", "% time other behavior (Ethovision)", "% time on top (Ethovision)", "% time on top (video)", "% time approaching stimulus animal (video)", "% time other behavior (video)", "% 18-32kHz", "% 32-96kHz")

for(i in DataColumnsForBoxplots_AllGroups){
 
  Temp<-Data[is.na(Data[,i])==F,]
  
  Temp$Enrichment_Factor<-droplevels(Temp$Enrichment_Factor)
  Temp$Generation<-droplevels(Temp$Generation) 
  
pdf(paste("Boxplot_", colnames(Temp)[i], "vsAllGroups.pdf", sep=""), width=6, height=4)
par(mar=c(6.5, 4.5, 1.1, 1.1), mgp=c(3, 1, 0))
boxplot(Temp[,i]~Enrichment_Factor*Social_Defeat_Factor*Line_Factor, data = Temp, col=c("green3", "forestgreen","darkgreen", "green3", "forestgreen","darkgreen", "red1", "red3", "red4", "red1", "red3", "red4"), las=3, ylab=paste(YLabelsForBoxplots_AllGroups[i-9], sep=""), cex.lab=1.1, cex.main=1.75, cex.axis=0.9, outline=F)

stripchart(Temp[,i]~Enrichment_Factor*Social_Defeat_Factor*Line_Factor, data = Temp, vertical = TRUE, 
           method = "jitter", add = TRUE, col = 'black', cex=1, pch=c(c(1,0,2, 16,15,17),c(1,0,2, 16,15,17)))
dev.off()
rm(Temp)
}




DataColumnsForBoxplots_Hormones<-c(48:51)
YLabelsForBoxplots_Hormones<-c("plasma corticosterone (pg/mL)", "plasma testosterone (pg/mL)", "plasma oxytocin (pg/mL)", "plasma IL-6 (pg/mL)")
LimitOfDetection_Hormones<-c(12.8, 30.6, 15.5, 78)
#Note - there are multiple kits with the same product numbers. Double check with Angela that I got the right one
#There was no limit of detection listed for the IL6 kit. As a place-holder, I put the value of the lowest concentration on the standard curve. Interestingly, *most of our values are below that* which may explain why the data is so messy.


for(i in DataColumnsForBoxplots_Hormones){
  
 
  Temp<-Data[is.na(Data[,i])==F,]
  
  Temp$Enrichment_Factor<-droplevels(Temp$Enrichment_Factor)
  Temp$Generation<-droplevels(Temp$Generation) 
  
  pdf(paste("Boxplot_", colnames(Temp)[i], "vsAllGroups.pdf", sep=""), width=4, height=5)
  par(mar=c(6.5, 4.5, 1.1, 1.1), mgp=c(3, 1, 0))
  boxplot(Temp[,i]~Enrichment_Factor*Social_Defeat_Factor*Line_Factor, data = Temp, col=c("green3", "darkgreen", "green3", "darkgreen", "red1", "red4", "red1", "red4"), las=3, ylab=paste(YLabelsForBoxplots_Hormones[i-47], sep=""), cex.lab=1.1, cex.main=1.75, cex.axis=0.9, outline=F)
  
  stripchart(Temp[,i]~Enrichment_Factor*Social_Defeat_Factor*Line_Factor, data = Temp, vertical = TRUE, 
             method = "jitter", add = TRUE, col = 'black', cex=1, pch=c(c(1,2, 16,17),c(1,2, 16,17)))
  abline(a=LimitOfDetection_Hormones[i-47], b=0, lty=3, lwd=2)
  dev.off()
  rm(Temp)
}

