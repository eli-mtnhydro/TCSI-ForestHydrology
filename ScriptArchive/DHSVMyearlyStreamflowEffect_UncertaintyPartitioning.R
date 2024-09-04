
library(ggplot2)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Results"
setwd(dir)

# Setup
Scenarios = 1:6
Climates = c("cnrm", "miroc")
ClimateNames = c("CNRM-CM5 RCP 8.5", "MIROC5 RCP 8.5")
LANDISruns = c(1,4)

WaterYears = 2015:2099

WaterYearStreamData = read.csv("WaterYearWatershedStreamflowResults.csv")[,-1]













