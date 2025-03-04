
library(data.table)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Tradeoff_Figure/"
setwd(dir)

################################################################################

WatershedData = read.csv("CombinedLocalWatershedResults_WithAttributes.csv")
WatershedData2 = read.csv("WatershedData_WithMeanAnnualPeak.csv")

WatershedData$YearlyPeakAvg_mmd = WatershedData2$YearlyPeakAvg_mmd[match(paste0(WatershedData$WatershedNum,
                                                                                          WatershedData$Climate,
                                                                                          WatershedData$Scenario,
                                                                                          WatershedData$DHSVMmodel),
                                                                                   paste0(WatershedData2$WatershedNum,
                                                                                          WatershedData2$Climate,
                                                                                          WatershedData2$Scenario,
                                                                                          WatershedData2$DHSVMmodel))]

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
  
  CNRMpeakValAbsolute = mean(WatershedData$YearlyPeakAvg_mmd[WatershedData$Scenario==Scenario &
                                                               WatershedData$Climate=="cnrm"])
  MIROCpeakValAbsolute = mean(WatershedData$YearlyPeakAvg_mmd[WatershedData$Scenario==Scenario &
                                                                WatershedData$Climate=="miroc"])
  
  AvgYieldVal = (MIROCyieldVal + CNRMyieldVal) / 2
  AvgPeakVal = (CNRMpeakVal + MIROCpeakVal) / 2
  AvgPeakValAbsolute = (CNRMpeakValAbsolute + MIROCpeakValAbsolute) / 2
  
  print(paste(Scenario,"avg. yield:",signif(AvgYieldVal*365.25,4),"mm/yr"))
  print(paste(Scenario,"avg. peak:",signif(AvgPeakVal * 10,4),"mm/d/decade"))
  print(paste(Scenario,"avg. yearly peak:",signif(AvgPeakValAbsolute,4),"mm/d"))
  
  # print(paste(Scenario,"CNRM yield:",signif(CNRMyieldVal*365.25),"mm/yr"))
  # print(paste(Scenario,"MIROC yield:",signif(MIROCyieldVal*365.25),"mm/yr"))
  # print(paste(Scenario,"CNRM peak:",signif(CNRMpeakVal),"mm/d/yr"))
  # print(paste(Scenario,"MIROC peak:",signif(MIROCpeakVal),"mm/d/yr"))
}

# Full restoration / partial restoration
mean(c(41.91,33.6)) / mean(c(8.629,8.157)) # Yield differences
mean(c(0.09423,0.06182)) / mean(c(0.03685,0.03582)) # Peak trend differences
mean(c(40.39,40.13)) / mean(c(39.21,39.21)) # Yearly peaks




