#A version of the multipanel plot that accounts for generation:

#Starts the graphics device driver for producing the graphs in a pdf
pdf("AggressiveBehavior_Log2_vs_DefeatDay_ByGen.pdf", height=12, width=8.5)

#Sets up the graphical parameters for the graphs. 
    #mfrow causes subsequent figures to be drawn in an nr-by-nc array by rows [the array is specified using c(3,2)].
    #cex is a numerical value that gives the amount by which plotting text and symbols should be magnified relative to the default of 1.
        #cex.lab is the magnification to be used for x and y labels relative to the current setting of cex.
        #cex.axis is the magnification to be used for axis annotation relative to the current setting of cex.
        #cex.main is the magnification to be used for main titles relative to the current setting of cex.
    #mar is a numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot. The default is c(5, 4, 4, 2) + 0.1.
par(mfrow=c(3,2), cex.lab=2, cex.axis=1.5, cex.main=2, mar=c(5.1, 5.1, 5.1, 3.1))

#The following code segment (lines 19-43) is repeated for all of the treatment groups in Aggressive_Log2_Matrix. It is highly probable that the rest of this code could be functionalized and made more efficient. Doing so would
#also faciliate the graphing of other matrices. I (Liam) should start working on this as soon as possible. 

#Takes the data from the treatment group "LR NIL + SD" in "Aggressive_Log2_Matrix" and assigns it to Temp. 
Temp<-Aggressive_Log2_Matrix[Data$Treatment_Group=="LR NIL + SD",]
#Creates the character vector "TempGen" from from the "LR NIL + SD" treatment group generations.
TempGen<-as.character(Data$Generation[Data$Treatment_Group=="LR NIL + SD"])
#Relabels the generations in TempGen as "red2", "red4", or "red3".
TempGen[TempGen=="F49"]<-"red2"
TempGen[TempGen=="F53"]<-"red4"
TempGen[TempGen=="F56"]<-"red3"
#Creates the groundwork of the time plot for Aggressive Behavior (Log2) "LR NIL + SD" Treatment group using a generic X-Y plot.
plot(Temp[9,]~DefeatDay, ylab="Aggressive Behavior (Log2)", xlab="Defeat Day (intercept=Day4)", main="LR NIL + SD", ylim=c(min(cbind(Data$AggressiveLog2_Intercept,Aggressive_Log2_Matrix), na.rm=T), max(cbind(Data$AggressiveLog2_Intercept,Aggressive_Log2_Matrix), na.rm=T)))

#Cycles through the values in Temp[,1] and, after checking to make sure that the value is NOT NA, adds and fits the necessary lines to the plot created in line 27.
  #lines() adds connected line segments to the plot. I'm not sure what type="o" does, so I just ask Dr. Hagenauer for clarification. The help function did not help illuminate what it does.
  #FitLine is assigned the value of lm(Temp[i,]~DefeatDay), which is a function used to fit linear models (specifically, in this case, to fit a linear model using the data in Temp according to DefeatDay)
  #abline() adds straight lines to the plot. In this case, it uses FitLine to provide the coefficients (Intercept and DefeatDay). It assigns colors to the lines based on the colors listed in TempGen ("red2", etc).
for(i in c(1:length(Temp[,1]))){
  if(is.na(Temp[i,])==F){
    lines(Temp[i,]~DefeatDay, type="o", col="grey")
    FitLine<-lm(Temp[i,]~DefeatDay)
    abline(FitLine, col=TempGen[i])}else{}
}

#Removes "Temp" from the environment.
rm(Temp)
#Adds another straight line to the plot made in line 27. This line is black and represents the mean value over the defeat days. 
abline(mean(Data$AggressiveLog2_Intercept[Data$Treatment_Group=="LR NIL + SD"], na.rm=T), mean(Data$AggressiveLog2_Slope[Data$Treatment_Group=="LR NIL + SD"], na.rm=T), col="black", lwd=4) 

#This is a repeat of the code in lines 19-43. The only differences are that it uses the treatment group "HR NIL + SD" and that it uses green instead of red when assigning labels to TempGen.
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

#This is another repeat of the code in lines 19-43. The only difference is that it uses the treatment group "LR EC + SD".
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

#This is a repeat of the code in lines 19-43. The only differences are that it uses the treatment group "HR EC + SD" and that it uses green instead of red when assigning labels to TempGen.
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

#This is a repeat of the code in lines 19-43. The only difference is that it uses the treatment group "LR EE + SD".
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

#This is a repeat of the code in lines 19-43. The only differences are that it uses the treatment group "HR EE + SD" and that it uses green instead of red when assigning labels to TempGen.
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

#Shuts down the graphics device that was putting the plots into a pdf.
dev.off()
