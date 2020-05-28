#A version of the multipanel plot that accounts for generation:


pdf("AggressiveBehavior_Log2_vs_DefeatDay_ByGen.pdf", height=12, width=8.5)

par(mfrow=c(3,2), cex.lab=2, cex.axis=1.5, cex.main=2, mar=c(5.1, 5.1, 5.1, 3.1))

Temp<-Aggressive_Log2_Matrix[Data$Treatment_Group=="LR NIL + SD",]
TempGen<-as.character(Data$Generation[Data$Treatment_Group=="LR NIL + SD"])
TempGen[TempGen=="F49"]<-"red2"
TempGen[TempGen=="F53"]<-"red4"
TempGen[TempGen=="F56"]<-"red3"
plot(Temp[9,]~DefeatDay, ylab="Aggressive Behavior (Log2)", xlab="Defeat Day (intercept=Day4)", main="LR NIL + SD", ylim=c(min(cbind(Data$AggressiveLog2_Intercept,Aggressive_Log2_Matrix), na.rm=T), max(cbind(Data$AggressiveLog2_Intercept,Aggressive_Log2_Matrix), na.rm=T)))

for(i in c(1:length(Temp[,1]))){
  if(is.na(Temp[i,])==F){
    lines(Temp[i,]~DefeatDay, type="o", col="grey")
    FitLine<-lm(Temp[i,]~DefeatDay)
    abline(FitLine, col=TempGen[i])}else{}
}
rm(Temp)

abline(mean(Data$AggressiveLog2_Intercept[Data$Treatment_Group=="LR NIL + SD"], na.rm=T), mean(Data$AggressiveLog2_Slope[Data$Treatment_Group=="LR NIL + SD"], na.rm=T), col="black", lwd=4) 


Temp<-Aggressive_Log2_Matrix[Data$Treatment_Group=="HR NIL + SD",]
TempGen<-as.character(Data$Generation[Data$Treatment_Group=="HR NIL + SD"])
TempGen[TempGen=="F49"]<-"green2"
TempGen[TempGen=="F53"]<-"green4"
TempGen[TempGen=="F56"]<-"green3"
plot(Temp[1,]~DefeatDay, ylab="Aggressive Behavior (Log2)", xlab="Defeat Day (intercept=Day4)", main="HR NIL + SD", ylim=c(min(cbind(Data$AggressiveLog2_Intercept,Aggressive_Log2_Matrix), na.rm=T), max(cbind(Data$AggressiveLog2_Intercept,Aggressive_Log2_Matrix), na.rm=T)))

for(i in c(1:length(Temp[,1]))){
  if(is.na(Temp[i,])==F){
    lines(Temp[i,]~DefeatDay, type="o", col="grey")
    FitLine<-lm(Temp[i,]~DefeatDay)
    abline(FitLine, col=TempGen[i])}else{}
}
rm(Temp)

abline(mean(Data$AggressiveLog2_Intercept[Data$Treatment_Group=="HR NIL + SD"], na.rm=T), mean(Data$AggressiveLog2_Slope[Data$Treatment_Group=="HR NIL + SD"], na.rm=T), col="black", lwd=4) 


Temp<-Aggressive_Log2_Matrix[Data$Treatment_Group=="LR EC + SD",]
TempGen<-as.character(Data$Generation[Data$Treatment_Group=="LR EC + SD"])
TempGen[TempGen=="F49"]<-"red2"
TempGen[TempGen=="F53"]<-"red4"
TempGen[TempGen=="F56"]<-"red3"
plot(Temp[4,]~DefeatDay, ylab="Aggressive Behavior (Log2)", xlab="Defeat Day (intercept=Day4)", main="LR EC + SD", ylim=c(min(cbind(Data$AggressiveLog2_Intercept,Aggressive_Log2_Matrix), na.rm=T), max(cbind(Data$AggressiveLog2_Intercept,Aggressive_Log2_Matrix), na.rm=T)))

for(i in c(1:length(Temp[,1]))){
  if(is.na(Temp[i,])==F){
    lines(Temp[i,]~DefeatDay, type="o", col="grey")
    FitLine<-lm(Temp[i,]~DefeatDay)
    abline(FitLine, col=TempGen[i])}else{}
}
rm(Temp)

abline(mean(Data$AggressiveLog2_Intercept[Data$Treatment_Group=="LR EC + SD"], na.rm=T), mean(Data$AggressiveLog2_Slope[Data$Treatment_Group=="LR EC + SD"], na.rm=T), col="black", lwd=4) 


Temp<-Aggressive_Log2_Matrix[Data$Treatment_Group=="HR EC + SD",]
TempGen<-as.character(Data$Generation[Data$Treatment_Group=="HR EC + SD"])
TempGen[TempGen=="F49"]<-"green2"
TempGen[TempGen=="F53"]<-"green4"
TempGen[TempGen=="F56"]<-"green3"
plot(Temp[1,]~DefeatDay, ylab="Aggressive Behavior (Log2)", xlab="Defeat Day (intercept=Day4)", main="HR EC + SD", ylim=c(min(cbind(Data$AggressiveLog2_Intercept,Aggressive_Log2_Matrix), na.rm=T), max(cbind(Data$AggressiveLog2_Intercept,Aggressive_Log2_Matrix), na.rm=T)))

for(i in c(1:length(Temp[,1]))){
  if(is.na(Temp[i,])==F){
    lines(Temp[i,]~DefeatDay, type="o", col="grey")
    FitLine<-lm(Temp[i,]~DefeatDay)
    abline(FitLine, col=TempGen[i])}else{}
}
rm(Temp)

abline(mean(Data$AggressiveLog2_Intercept[Data$Treatment_Group=="HR EC + SD"], na.rm=T), mean(Data$AggressiveLog2_Slope[Data$Treatment_Group=="HR EC + SD"], na.rm=T), col="black", lwd=4) 


Temp<-Aggressive_Log2_Matrix[Data$Treatment_Group=="LR EE + SD",]
TempGen<-as.character(Data$Generation[Data$Treatment_Group=="LR EE + SD"])
TempGen[TempGen=="F49"]<-"red2"
TempGen[TempGen=="F53"]<-"red4"
TempGen[TempGen=="F56"]<-"red3"
plot(Temp[6,]~DefeatDay, ylab="Aggressive Behavior (Log2)", xlab="Defeat Day (intercept=Day4)", main="LR EE + SD", ylim=c(min(cbind(Data$AggressiveLog2_Intercept,Aggressive_Log2_Matrix), na.rm=T), max(cbind(Data$AggressiveLog2_Intercept,Aggressive_Log2_Matrix), na.rm=T)))

for(i in c(1:length(Temp[,1]))){
  if(is.na(Temp[i,])==F){
    lines(Temp[i,]~DefeatDay, type="o", col="grey")
    FitLine<-lm(Temp[i,]~DefeatDay)
    abline(FitLine, col=TempGen[i])}else{}
}
rm(Temp)

abline(mean(Data$AggressiveLog2_Intercept[Data$Treatment_Group=="LR EE + SD"], na.rm=T), mean(Data$AggressiveLog2_Slope[Data$Treatment_Group=="LR EE + SD"], na.rm=T), col="black", lwd=4) 


Temp<-Aggressive_Log2_Matrix[Data$Treatment_Group=="HR EE + SD",]
TempGen<-as.character(Data$Generation[Data$Treatment_Group=="HR EE + SD"])
TempGen[TempGen=="F49"]<-"green2"
TempGen[TempGen=="F53"]<-"green4"
TempGen[TempGen=="F56"]<-"green3"
plot(Temp[4,]~DefeatDay, ylab="Aggressive Behavior (Log2)", xlab="Defeat Day (intercept=Day4)", main="HR EE + SD", ylim=c(min(cbind(Data$AggressiveLog2_Intercept,Aggressive_Log2_Matrix), na.rm=T), max(cbind(Data$AggressiveLog2_Intercept,Aggressive_Log2_Matrix), na.rm=T)))

for(i in c(1:length(Temp[,1]))){
  if(is.na(Temp[i,])==F){
    lines(Temp[i,]~DefeatDay, type="o", col="grey")
    FitLine<-lm(Temp[i,]~DefeatDay)
    abline(FitLine, col=TempGen[i])}else{}
}
rm(Temp)

abline(mean(Data$AggressiveLog2_Intercept[Data$Treatment_Group=="HR EE + SD"], na.rm=T), mean(Data$AggressiveLog2_Slope[Data$Treatment_Group=="HR EE + SD"], na.rm=T), col="black", lwd=4) 

dev.off()
