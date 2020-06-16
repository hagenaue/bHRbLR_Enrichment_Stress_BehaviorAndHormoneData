<<<<<<< HEAD
#Liam's functionalized version of TimeSeriesPlots. Second Edition.
#Note to Self (Liam): Rework my titling system so that it functions automatically and isn't complete garbage. Remember to ask Dr. Hagenauer about naming conventions and whether it would be alright to rework how things are named.
=======

#Social Defeat Time Series variables to loop over (once the plotting function is done):
TimeSeriesVariables_Matrices<-list(Aggressive_Matrix, Aggressive_Log2_Matrix, Submissive_Matrix, Submissive_Log2_Matrix, OtherBehavior_Matrix, DefeatScore_Matrix, TimeCaged_Matrix)
TimeSeriesVariables_Names<-c("Aggressive Behavior", "Aggressive Behavior (Log2)", "Submissive Behavior", "Submissive Behavior (Log2)", "Other Behavior", "Defeat Score", "Time Caged") 
TimeSeriesVariables_Intercepts<-cbind(Data$Aggressive_Intercept, Data$AggressiveLog2_Intercept, Data$Submissive_Intercept, Data$SubmissiveLog2_Intercept, Data$OtherBehavior_Intercept,Data$DefeatScore_Intercept, Data$TimeCaged_Intercept)
TimeSeriesVariables_Slopes<-cbind(Data$Aggressive_Slope, Data$AggressiveLog2_Slope, Data$Submissive_Slope, Data$SubmissiveLog2_Slope, Data$OtherBehavior_Slope,Data$DefeatScore_Slope, Data$TimeCaged_Slope)

#Liam's functionalized version of TimeSeriesPlots
>>>>>>> ec2438ac683cb464b6746c8313b0be27fd8da37b


#Creates time plots and outputs them to a pdf. 
#time_series_matrix corresponds to the specific variable matrix the function will be graphing from. Example: Aggressive_Log2_Matrix
#time_series_intercept corresponds to the intercept stored in Data of the specified time_Series_matrix. Example: Data$AggressiveLog2_Intercept
#time_series_slope corresponds to the slope stored in Data of the specified time_Series_matrix. Example: Data$AggressiveLog2_Slope
#pdf_title corresponds to the title of the pdf that is outputted (DO NOT INCLUDE ".pdf" IN THIS TITLE). Example: "AggressiveBehavior_Log2"
#graph_y_label corresponds to the y label of the graphs. Example: "Aggressive Behavior (Log2)"
create_time_plots <- function(time_series_matrix, time_series_intercept, time_series_slope, pdf_title, graph_y_label) {
  
  #Starts the graphics device driver for producing the graphs in a pdf. 
  pdf(paste(pdf_title, "_vs_DefeatDay_ByGen.pdf", sep = ""), height=12, width=8.5)
  
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
    
    if (treatment_group == "LR NIL + SD") {
      starting_point <- 9
    } else if (treatment_group == "LR EC + SD") {
      starting_point <- 4
    } else if (treatment_group == "LR EE + SD") {
      starting_point <- 6
    } else if (treatment_group == "HR EE + SD") {
      starting_point <- 4
    } 
    
    return(starting_point)
  }
  
  #A function that creates a time_series panel using the selected time series variable
  plot_time_series_panel <- function(treatment_group) {
    #stores the starting point for graphing the panel
    panel_starting_point <- determine_starting_point(treatment_group)
    #Takes the data from the specified treatment group in the selected time series variable and assigns it to Temp. 
    Temp<-time_series_matrix[Data$Treatment_Group==treatment_group,]
    #Creates the character vector "TempGen" from from the specified treatment group generations.
    TempGen<-as.character(Data$Generation[Data$Treatment_Group==treatment_group])
    
    #Relabels the generations in TempGen according to their line (LR = red; HR = green)
    if(substring(treatment_group, 1, 2) == "LR") {
      TempGen[TempGen=="F49"]<-"red2"
      TempGen[TempGen=="F53"]<-"red4"
      TempGen[TempGen=="F56"]<-"red3"
    } else {
      TempGen[TempGen=="F49"]<-"green2"
      TempGen[TempGen=="F53"]<-"green4"
      TempGen[TempGen=="F56"]<-"green3"
    }
    
    #Creates the groundwork of the time plot for the selected time series variable for the specified treatment group using a generic X-Y plot.
    #Note to Self (Liam): Rework the labeling system.
    plot(Temp[panel_starting_point,]~DefeatDay, ylab=graph_y_label, xlab="Defeat Day (intercept=Day4)", main=treatment_group, ylim=c(min(cbind(time_series_intercept,time_series_matrix), na.rm=T), max(cbind(time_series_intercept,time_series_matrix), na.rm=T)))
    
    #Cycles through the values in Temp[,1] and, after checking to make sure that the value is NOT NA, adds and fits the necessary lines to the plot created in line 27.
    #lines() adds connected line segments to the plot. I'm not sure what type="o" does, so I just ask Dr. Hagenauer for clarification. The help function did not help illuminate what it does.
    #FitLine is assigned the value of lm(Temp[i,]~DefeatDay), which is a function used to fit linear models (specifically, in this case, to fit a linear model using the data in Temp according to DefeatDay)
    #abline() adds straight lines to the plot. In this case, it uses FitLine to provide the coefficients (Intercept and DefeatDay). It assigns colors to the lines based on the colors listed in TempGen ("red2", etc).
    for(i in c(1:length(Temp[,1]))){
      if(sum(is.na(Temp[i,]))<3){
        lines(Temp[i,]~DefeatDay, type="o", col="grey")
        FitLine<-lm(Temp[i,]~DefeatDay)
        abline(FitLine, col=TempGen[i])}else{}
    }
    #Removes "Temp" from the environment.
    rm(Temp)
    #Adds another straight line to the plot made in line 27. This line is black and represents the mean value over the defeat days. 
    abline(mean(time_series_intercept[Data$Treatment_Group==treatment_group], na.rm=T), mean(time_series_slope[Data$Treatment_Group==treatment_group], na.rm=T), col="black", lwd=4) 
  }
  
  #Creates the graphs.
  for (i in c("LR NIL + SD", "HR NIL + SD", "LR EC + SD", "HR EC + SD", "LR EE + SD", "HR EE + SD")) {
    plot_time_series_panel(i)
  }
  
  #Shuts down the graphics device that was putting the plots into a pdf.
  dev.off()
}

#Social Defeat Time Series variables to loop over (once the plotting function is done):
TimeSeriesVariables_Matrices<-list(Aggressive_Matrix, Aggressive_Log2_Matrix, Submissive_Matrix, Submissive_Log2_Matrix, OtherBehavior_Matrix, DefeatScore_Matrix, TimeCaged_Matrix)
TimeSeriesVariables_Names<-c("Aggressive Behavior", "Aggressive Behavior (Log2)", "Submissive Behavior", "Submissive Behavior (Log2)", "Other Behavior", "Defeat Score", "Time Caged") 
TimeSeriesVariables_Intercepts<-cbind(Data$Aggressive_Intercept, Data$AggressiveLog2_Intercept, Data$Submissive_Intercept, Data$SubmissiveLog2_Intercept, Data$OtherBehavior_Intercept,Data$DefeatScore_Intercept, Data$TimeCaged_Intercept)
TimeSeriesVariables_Slopes<-cbind(Data$Aggressive_Slope, Data$AggressiveLog2_Slope, Data$Submissive_Slope, Data$SubmissiveLog2_Slope, Data$OtherBehavior_Slope,Data$DefeatScore_Slope, Data$TimeCaged_Slope)

create_time_plots(Aggressive_Log2_Matrix, Data$AggressiveLog2_Intercept, Data$AggressiveLog2_Slope, "Aggressive Log2", "Aggressive Behavior (Log2)")
create_time_plots(Submissive_Log2_Matrix, Data$SubmissiveLog2_Intercept, Data$SubmissiveLog2_Slope, "Submissive Log2", "Submissive Behavior (Log2)")
