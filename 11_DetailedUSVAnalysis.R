#Quick code to see if we can easily perform an automated call type analysis on Angela's USV data.
#April 09 2020
#Megan Hagenauer

#An aside regarding the data:
#The 53 and 56 generation data were recorded and analyzed using slightly different methods:
## F53: Noise issues meant that calls had to be defined by hand for analysis.
## F56: Gain was set lower during recording to eliminate noise issues, calls defined automatically by avisoft using our automated protocol.

#F56 data
setwd("C:/Users/Frosty/Desktop/Research/Research during Summer of 2020/R Data/Angela_HRLR_StressEnrichData/USV data/F56")

filenames<-list.files()

filenames
# [1] "recording 103.xlsx" "recording 105.xlsx" "recording 107.xlsx" "recording 109.xlsx" "recording 110.xlsx" "recording 112.xlsx"
# [7] "recording 113.xlsx" "recording 114.xlsx" "recording 16.xlsx"  "recording 17.xlsx"  "recording 20.xlsx"  "recording 21.xlsx" 
# [13] "recording 22.xlsx"  "recording 26.xlsx"  "recording 27.xlsx"  "recording 28.xlsx"  "recording 38.xlsx"  "recording 39.xlsx" 
# [19] "recording 41.xlsx"  "recording 42.xlsx"  "recording 43.xlsx"  "recording 44.xlsx"  "recording 45.xlsx"  "recording 48.xlsx" 
# [25] "recording 50.xlsx"  "recording 53.xlsx"  "recording 54.xlsx"  "recording 56.xlsx"  "recording 63.xlsx"  "recording 64.xlsx" 
# [31] "recording 65.xlsx"  "recording 66.xlsx"  "recording 67.xlsx"  "recording 68.xlsx"  "recording 70.xlsx"  "recording 71.xlsx" 
# [37] "recording 72.xlsx"  "recording 74.xlsx"  "recording 75.xlsx"  "recording 76.xlsx"  "recording 77.xlsx"  "recording 79.xlsx" 
# [43] "recording 80.xlsx"  "recording 81.xlsx"  "recording 83.xlsx"  "recording 84.xlsx"  "recording 85.xlsx"  "recording 87.xlsx" 
# [49] "recording 88.xlsx"  "recording 91.xlsx"  "recording 92.xlsx"  "recording 97.xlsx"  "recording 98.xlsx" 

library(gdata)
concatenated<-do.call("rbind", lapply(filenames, read.xls, header=TRUE))

str(concatenated)

# 'data.frame':	149346 obs. of  14 variables:
#   $ number                    : int  1 2 3 4 5 6 7 8 9 10 ...
# $ duration                  : num  0.0025 0.0015 0.0025 0.0015 0.002 0.0015 0.002 0.0015 0.002 0.0025 ...
# $ interval.                 : Factor w/ 2824 levels "_","0.004","0.0046",..: 1 13 280 50 21 63 35 47 13 66 ...
# $ peak.freq.start           : int  52700 53700 52700 52700 52700 54600 53700 15600 51700 51700 ...
# $ peak.freq.end             : int  52700 52700 52700 52700 53700 53700 51700 15600 51700 51700 ...
# $ peak.freq.min.entire      : int  52700 52700 52700 51700 52700 53700 51700 15600 51700 50700 ...
# $ peak.freq.max.entire      : int  53700 53700 52700 52700 53700 54600 53700 16600 51700 51700 ...
# $ preak.freq.mean.entire    : int  53000 52900 52700 52400 52900 54100 52300 15800 51700 51500 ...
# $ Number.of.calls           : int  6010 6010 6010 6010 6010 6010 6010 6010 6010 6010 ...
# $ Number.of.18...32.kHz     : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Number.of.32...96.kHz     : int  1 1 1 1 1 1 1 0 1 1 ...
# $ average.max.peak.frequency: num  52431 52431 52431 52431 52431 ...
# $ duration.of.18...32.kHz   : num  NA NA NA NA NA NA NA NA NA NA ...
# $ duration.of.32...96.kHz   : num  0.0025 0.0015 0.0025 0.0015 0.002 0.0015 0.002 NA 0.002 0.0025 ...

setwd("C:/Users/Frosty/Desktop/Research/Research during Summer of 2020/R Data/Angela_HRLR_StressEnrichData/USV data")

pdf("F56_Histogram_preakfreqmeanentire.pdf", width=5, height=4)
hist(concatenated$preak.freq.mean.entire, breaks=100, main="F56: Histogram of Peak Frequency Mean Entire", col=2)
dev.off()

pdf("F56_Histogram_log10duration.pdf", width=5, height=4)
hist(log10(concatenated$duration), breaks=100, main="F56: Histogram Log10 Duration", col=2)
dev.off()

#... and apparently we are missing the standard deviation for the peak frequency, which is essential for defining FM vs. Flat. 
#So I'm just going to stop this analysis here for now.
