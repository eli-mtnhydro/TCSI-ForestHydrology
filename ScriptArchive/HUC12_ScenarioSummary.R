
library(data.table)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Tradeoff_Figure/"
setwd(dir)

################################################################################

WatershedData = read.csv("CombinedLocalWatershedResults_WithAttributes.csv")

WatershedData = WatershedData[WatershedData$Scenario %in% c(1,3:6),]

head(WatershedData)

plot(WatershedData$S2RelContribStreamflow_mmd,
     WatershedData$S2RelYearlyPeakSensSlope_mmdyr)

for (Scenario in c(1,3:6)){
  
  CNRMyieldVal = weighted.mean(WatershedData$S2RelContribStreamflow_mmd[WatershedData$Scenario==Scenario &
                                                                 WatershedData$Climate=="cnrm"],
                               WatershedData$Area_km2[WatershedData$Scenario==Scenario &
                                                        WatershedData$Climate=="cnrm"])
  MIROCyieldVal = weighted.mean(WatershedData$S2RelContribStreamflow_mmd[WatershedData$Scenario==Scenario &
                                                                 WatershedData$Climate=="miroc"],
                                WatershedData$Area_km2[WatershedData$Scenario==Scenario &
                                                         WatershedData$Climate=="miroc"])
  
  CNRMpeakVal = weighted.mean(WatershedData$S2RelYearlyPeakSensSlope_mmdyr[WatershedData$Scenario==Scenario &
                                                                 WatershedData$Climate=="cnrm"],
                              WatershedData$Area_km2[WatershedData$Scenario==Scenario &
                                                       WatershedData$Climate=="cnrm"])
  MIROCpeakVal = weighted.mean(WatershedData$S2RelYearlyPeakSensSlope_mmdyr[WatershedData$Scenario==Scenario &
                                                                  WatershedData$Climate=="miroc"],
                               WatershedData$Area_km2[WatershedData$Scenario==Scenario &
                                                        WatershedData$Climate=="miroc"])
  
  AvgYieldVal = (MIROCyieldVal + CNRMyieldVal) / 2
  AvgPeakVal = (CNRMpeakVal + MIROCpeakVal) / 2
  
  print(paste(Scenario,"avg. yield:",signif(AvgYieldVal*365.25,4),"mm/yr"))
  print(paste(Scenario,"avg. peak:",signif(AvgPeakVal,4),"mm/d/yr"))
  
  # print(paste(Scenario,"CNRM yield:",signif(CNRMyieldVal*365.25),"mm/yr"))
  # print(paste(Scenario,"MIROC yield:",signif(MIROCyieldVal*365.25),"mm/yr"))
  # print(paste(Scenario,"CNRM peak:",signif(CNRMpeakVal),"mm/d/yr"))
  # print(paste(Scenario,"MIROC peak:",signif(MIROCpeakVal),"mm/d/yr"))
}

# Full restoration / partial restoration
mean(c(41.91,33.6)) / mean(c(8.629,8.157)) # Yield
mean(c(0.009423,0.006182)) / mean(c(0.003685,0.003582)) # Peaks


