#Megan's edits to Liam's functionalized version of TimeSeriesPlots. Third Edition: Open field data
#The goal was to make a version that matched other figure formatting for the paper.

Minute<-c(1,2,3,4,5)

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
#pdf_title corresponds to the title of the pdf that is outputted (DO NOT INCLUDE ".pdf" IN THIS TITLE). Example: "AggressiveBehavior_Log2"
#graph_y_label corresponds to the y label of the graphs. Example: "Aggressive Behavior (Log2)"


create_time_plots <- function(time_series_matrix, pdf_title, graph_y_label) {
    
  #Starts the graphics device driver for producing the graphs in a pdf. 
  pdf(paste(pdf_title, "_vs_Minute.pdf", sep = ""), height=12, width=12)
  
  #Sets up the graphical parameters for the graphs. 
  #mfrow causes subsequent figures to be drawn in an nr-by-nc array by rows [the array is specified using c(3,2)].
  #cex is a numerical value that gives the amount by which plotting text and symbols should be magnified relative to the default of 1.
  #cex.lab is the magnification to be used for x and y labels relative to the current setting of cex.
  #cex.axis is the magnification to be used for axis annotation relative to the current setting of cex.
  #cex.main is the magnification to be used for main titles relative to the current setting of cex.
  #mar is a numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot. The default is c(5, 4, 4, 2) + 0.1.
  par(mfrow=c(3,4), cex.lab=2, cex.axis=1.5, cex.main=2, mar=c(5.1, 5.1, 5.1, 3.1))
  
  #A function that creates a time_series panel using the selected time series variable
  plot_time_series_panel <- function(treatment_group) {
    panel_starting_point <-  1
    #Takes the data from the specified treatment group in the selected time series variable and assigns it to Temp. 
    Temp<-time_series_matrix[Data$Treatment_Group2==treatment_group,]
    #Creates vectors with formatting info for the specified treatment groups.
    TempCol_T<-TempCol[which(Data$Treatment_Group2==treatment_group)]
    TempPch_T<-TempPch[which(Data$Treatment_Group2==treatment_group)]
    
    
    #Creates the groundwork of the time plot for the selected time series variable for the specified treatment group using a generic X-Y plot.
    plot(Temp[panel_starting_point,]~Minute, ylab=graph_y_label, xlab="Minute", main=treatment_group, ylim=c(min(time_series_matrix), max(time_series_matrix)), pch=TempPch_T[i], col="grey")
  
    #Cycles through the values in Temp[,1] and, after checking to make sure that the value is NOT NA, adds and fits the necessary lines to the plot created in line 27.
    #lines() adds connected line segments to the plot. I'm not sure what type="o" does, so I just ask Dr. Hagenauer for clarification. The help function did not help illuminate what it does.
    #FitLine is assigned the value of lm(Temp[i,]~DefeatDay), which is a function used to fit linear models (specifically, in this case, to fit a linear model using the data in Temp according to DefeatDay)
    #abline() adds straight lines to the plot. In this case, it uses FitLine to provide the coefficients (Intercept and DefeatDay). 
    for(i in c(1:length(Temp[,1]))){
      if(sum(is.na(Temp[i,]))<3){
        points(Temp[i,]~Minute, pch=TempPch_T[i], col="grey")
        FitLine<-lm(Temp[i,]~Minute)
        abline(FitLine, col=TempCol_T[i])}else{}
    }
    #Removes "Temp" from the environment.
    rm(Temp)
    
    #This next section was cut by Megan for the sake of time - should probably be added back in.
    #Adds another straight line to the plot made in line 27. This line is black and represents the mean value over the defeat days. 
  #   abline(mean(time_series_intercept[Data$Treatment_Group2==treatment_group], na.rm=T), mean(time_series_slope[Data$Treatment_Group2==treatment_group], na.rm=T), col="black", lwd=4) 
  # }
  }
  
  #Creates the graphs.
  for (i in c("bLR NIL", "bLR NIL + SD", "bHR NIL", "bHR NIL + SD", "bLR SE", "bLR SE + SD", "bHR SE", "bHR SE + SD", "bLR EE",  "bLR EE + SD", "bHR EE", "bHR EE + SD")) {
    plot_time_series_panel(i)
  }
  
  #Shuts down the graphics device that was putting the plots into a pdf.
  dev.off()
}

#Creates the appropriate time series plots for each Time Series variable:
create_time_plots(OF_Distance_Matrix,  "OF_DistanceTraveled", "Open Field: Distance Traveled")
create_time_plots(OF_PercentTimeInCenter_Matrix,  "OF_PercentTimeInCenter", "Open Field: % Time in Center")



