#Megan's edits to Liam's functionalized version of TimeSeriesPlots. Second Edition.
#The goal was to make a version that matched other figure formatting for the paper.



TempCol<-as.character(Data$Treatment_Group)
TempPch<-as.character(Data$Treatment_Group)


#so that they match the colors/symbols used for the enrichment groups in the boxplots used for other variables:  
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

TempPch[TempPch=="HR NIL + SD"]<-16
TempPch[TempPch=="HR EC + SD"]<-15
TempPch[TempPch=="HR EE + SD"]<-17
TempPch[TempPch=="LR NIL + SD"]<-16
TempPch[TempPch=="LR EC + SD"]<-15
TempPch[TempPch=="LR EE + SD"]<-17

TempPch[TempPch=="HR NIL"]<-16
TempPch[TempPch=="HR EC"]<-15
TempPch[TempPch=="HR EE"]<-17
TempPch[TempPch=="LR NIL"]<-16
TempPch[TempPch=="LR EC"]<-15
TempPch[TempPch=="LR EE"]<-17

TempPch<-as.numeric(TempPch)

#Making the treatment group names match that used in the rest of the paper:
Data$Treatment_Group2<-Data$Treatment_Group

Data$Treatment_Group2[Data$Treatment_Group2=="HR EC"]<-"bHR SE"
Data$Treatment_Group2[Data$Treatment_Group2=="LR EC"]<-"bLR SE"
Data$Treatment_Group2[Data$Treatment_Group2=="HR EC + SD"]<-"bHR SE + SD"
Data$Treatment_Group2[Data$Treatment_Group2=="LR EC + SD"]<-"bLR SE + SD"

Data$Treatment_Group2[Data$Treatment_Group2=="HR EE"]<-"bHR EE"
Data$Treatment_Group2[Data$Treatment_Group2=="LR EE"]<-"bLR EE"
Data$Treatment_Group2[Data$Treatment_Group2=="HR EE + SD"]<-"bHR EE + SD"
Data$Treatment_Group2[Data$Treatment_Group2=="LR EE + SD"]<-"bLR EE + SD"

Data$Treatment_Group2[Data$Treatment_Group2=="HR NIL"]<-"bHR NIL"
Data$Treatment_Group2[Data$Treatment_Group2=="LR NIL"]<-"bLR NIL"
Data$Treatment_Group2[Data$Treatment_Group2=="HR NIL + SD"]<-"bHR NIL + SD"
Data$Treatment_Group2[Data$Treatment_Group2=="LR NIL + SD"]<-"bLR NIL + SD"



#Creates time plots and outputs them to a pdf. 
#time_series_matrix corresponds to the specific variable matrix the function will be graphing from. Example: Aggressive_Log2_Matrix
#time_series_intercept corresponds to the intercept stored in Data of the specified time_Series_matrix. Example: Data$AggressiveLog2_Intercept
#time_series_slope corresponds to the slope stored in Data of the specified time_Series_matrix. Example: Data$AggressiveLog2_Slope
#pdf_title corresponds to the title of the pdf that is outputted (DO NOT INCLUDE ".pdf" IN THIS TITLE). Example: "AggressiveBehavior_Log2"
#graph_y_label corresponds to the y label of the graphs. Example: "Aggressive Behavior (Log2)"
create_time_plots <- function(time_series_matrix, time_series_intercept, time_series_slope, pdf_title, graph_y_label) {
  
  #Starts the graphics device driver for producing the graphs in a pdf. 
  pdf(paste(pdf_title, "_vs_DefeatDay.pdf", sep = ""), height=12, width=8.5)
  
  #Sets up the graphical parameters for the graphs. 
  #mfrow causes subsequent figures to be drawn in an nr-by-nc array by rows [the array is specified using c(3,2)].
  #cex is a numerical value that gives the amount by which plotting text and symbols should be magnified relative to the default of 1.
  #cex.lab is the magnification to be used for x and y labels relative to the current setting of cex.
  #cex.axis is the magnification to be used for axis annotation relative to the current setting of cex.
  #cex.main is the magnification to be used for main titles relative to the current setting of cex.
  #mar is a numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot. The default is c(5, 4, 4, 2) + 0.1.
  par(mfrow=c(3,2), cex.lab=2, cex.axis=1.5, cex.main=2, mar=c(5.1, 5.1, 5.1, 3.1))
  
  #Determines the proper starting point when graphing a time series panel. "HR NIL + SD" & "HR EC + SD" are not included because they start at 1.
  #Note to Self (Liam): Rework this to automatically determine the starting point, as the various time series variables likely start at different spots as well
  determine_starting_point <- function(treatment_group) {
    
    starting_point <- 1
    
    if (treatment_group == "bLR NIL + SD") {
      starting_point <- 9
    } else if (treatment_group == "bLR SE + SD") {
      starting_point <- 4
    } else if (treatment_group == "bLR EE + SD") {
      starting_point <- 6
    } else if (treatment_group == "bHR EE + SD") {
      starting_point <- 4
    } 
    
    return(starting_point)
  }
  
  #A function that creates a time_series panel using the selected time series variable
  plot_time_series_panel <- function(treatment_group) {
    #stores the starting point for graphing the panel
    panel_starting_point <- determine_starting_point(treatment_group)
    #Takes the data from the specified treatment group in the selected time series variable and assigns it to Temp. 
    Temp<-time_series_matrix[Data$Treatment_Group2==treatment_group,]
    #Creates vectors with formatting info for the specified treatment groups.
    TempCol_T<-TempCol[which(Data$Treatment_Group2==treatment_group)]
    TempPch_T<-TempPch[which(Data$Treatment_Group2==treatment_group)]
    
    
    #Creates the groundwork of the time plot for the selected time series variable for the specified treatment group using a generic X-Y plot.
    #Note to Self (Liam): Rework the labeling system.
    plot(Temp[panel_starting_point,]~DefeatDay, ylab=graph_y_label, xlab="Defeat Day", main=treatment_group, ylim=c(min(cbind(time_series_intercept,time_series_matrix), na.rm=T), max(cbind(time_series_intercept,time_series_matrix), na.rm=T)), pch=TempPch_T[i], col="grey", xaxt="n")
    #relabeling the x-axis so that it is just social defeat day:
    xtick=c(1,2,3,4)
    xtickloc=c(-3,-2,-1,0)
    #axis(side=1, at=xtickloc, labels = FALSE)
    text(x=xtickloc,  par("usr")[3], labels = xtick, pos = 1, xpd = TRUE, cex=2)
    
    #Cycles through the values in Temp[,1] and, after checking to make sure that the value is NOT NA, adds and fits the necessary lines to the plot created in line 27.
    #lines() adds connected line segments to the plot. I'm not sure what type="o" does, so I just ask Dr. Hagenauer for clarification. The help function did not help illuminate what it does.
    #FitLine is assigned the value of lm(Temp[i,]~DefeatDay), which is a function used to fit linear models (specifically, in this case, to fit a linear model using the data in Temp according to DefeatDay)
    #abline() adds straight lines to the plot. In this case, it uses FitLine to provide the coefficients (Intercept and DefeatDay). It assigns colors to the lines based on the colors listed in TempGen ("red2", etc).
    for(i in c(1:length(Temp[,1]))){
      if(sum(is.na(Temp[i,]))<3){
        points(Temp[i,]~DefeatDay, pch=TempPch_T[i], col="grey")
        FitLine<-lm(Temp[i,]~DefeatDay)
        abline(FitLine, col=TempCol_T[i])}else{}
    }
    #Removes "Temp" from the environment.
    rm(Temp)
    #Adds another straight line to the plot made in line 27. This line is black and represents the mean value over the defeat days. 
    abline(mean(time_series_intercept[Data$Treatment_Group2==treatment_group], na.rm=T), mean(time_series_slope[Data$Treatment_Group2==treatment_group], na.rm=T), col="black", lwd=4) 
  }
  
  #Creates the graphs.
  for (i in c("bLR NIL + SD", "bHR NIL + SD", "bLR SE + SD", "bHR SE + SD", "bLR EE + SD", "bHR EE + SD")) {
    plot_time_series_panel(i)
  }
  
  #Shuts down the graphics device that was putting the plots into a pdf.
  dev.off()
}

#Creates the appropriate time series plots for each Social Defeat Time Series variable
create_time_plots(Aggressive_Matrix, Data$Aggressive_Intercept, Data$Aggressive_Slope, "Aggressive Behavior", "% time aggressive")
create_time_plots(Aggressive_Log2_Matrix, Data$AggressiveLog2_Intercept, Data$AggressiveLog2_Slope, "Aggressive Behavior (Log2)", "% time aggressive (log2)")
create_time_plots(Submissive_Matrix, Data$Submissive_Intercept, Data$Submissive_Slope, "Submissive Behavior", "% time submissive")
create_time_plots(Submissive_Log2_Matrix, Data$SubmissiveLog2_Intercept, Data$SubmissiveLog2_Slope, "Submissive Behavior (Log2)", "% time submissive (Log2)")
create_time_plots(OtherBehavior_Matrix, Data$OtherBehavior_Intercept, Data$OtherBehavior_Slope, "Other Behavior", "% time other behavior")
create_time_plots(DefeatScore_Matrix, Data$DefeatScore_Intercept, Data$DefeatScore_Slope, "Defeat Score", "defeat score")
create_time_plots(TimeCaged_Matrix, Data$TimeCaged_Intercept, Data$TimeCaged_Slope, "Time Caged", "time caged")

