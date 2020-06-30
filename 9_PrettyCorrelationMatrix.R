library(reshape2)
library(ggplot2)


PrettyCorrelationMatrixFunction<-function(NumericDataToCorrelate, UseForCor=c("everything", "all.obs", "complete.obs", "na.or.complete","pairwise.complete.obs"), MethodForCor=c("pearson", "kendall", "spearman")){

cormat <- round(cor(NumericDataToCorrelate, use=UseForCor, method=MethodForCor),2)
  head(cormat)  
    
melted_cormat <- melt(cormat)
head(melted_cormat)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the correlation matrix

melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name=paste(MethodForCor, "correlation", sep=" ")) +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

#Originally I had the pdf outputted as part of the function, but for unknown reasons I couldn't get it to work like usual:
#pdf(file=OutputFileName, width=OutputFileSize, height=OutputFileSize)

# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name=paste(MethodForCor, "correlation", sep=" ")) +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)


ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 1.5) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal") + guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5))

#dev.off()

}

###################


#Applying the function:

PrettyCorrelationMatrixFunction(NumericDataToCorrelate=NumericData[, c(1:12, 33:36, 57:62,67:68)], UseForCor="pairwise.complete", MethodForCor="spearman")
dev.copy(pdf, "/Users/mhh/Documents/Microarray Gen/Angela_HRLR_EE_Stress/Behav_Analysis_20200518/CorrMatrices/CorrMatrix_AllVariables_NoLog.pdf")
dev.off()
dev.off()

#To Do:
##Once we have updated the open field time in center and USV data, we should adjust this to include the appropriate variables and output them.
