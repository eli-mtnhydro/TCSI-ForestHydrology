
library(trend)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/"
setwd(dir)

GCMdata = read.csv("../TCSI_Results/WaterYear_MACAclimateStats_TCSIbasins.csv")

dim(GCMdata)

################################################################################

plot(GCMdata$Temp[GCMdata$Climate=="CNRM-CM5 RCP 8.5"],type="l") # Shit
plot(GCMdata$Temp[GCMdata$Climate=="MIROC5 RCP 8.5"],type="l") # Shit

sens.slope(GCMdata$Temp[GCMdata$Climate=="CNRM-CM5 RCP 8.5"])
0.06191637 * 85

sens.slope(GCMdata$Temp[GCMdata$Climate=="MIROC5 RCP 8.5"])
0.04582921 * 85

mean(GCMdata$Prec[GCMdata$Climate=="CNRM-CM5 RCP 8.5"])
mean(GCMdata$Prec[GCMdata$Climate=="MIROC5 RCP 8.5"])

(0.5967196 - 0.4560248) / 0.5967196











