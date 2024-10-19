
library(data.table)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Tradeoff_Figure/"
setwd(dir)

WatershedData = read.csv("CombinedLocalWatershedResults_WithAttributes.csv")

################################################################################
# Calculate actual mean annual peak flow in each watershed

SelectedModels = c(195,270,276)
Scenarios = 1:6
Climates = c("cnrm","miroc")
LANDISruns = c(1,4)

WaterYears = 2015:2099
DatesOnly = seq(as.Date(paste0("10-01-",WaterYears[1]-1), format="%m-%d-%Y"),
                as.Date(paste0("09-30-",rev(WaterYears)[1]), format="%m-%d-%Y"),
                by="days")

WatershedData$YearlyPeakAvg_cms = NA

for (SelectedModel in SelectedModels){
  for (Scenario in Scenarios){
    for (Climate in Climates){
      
      LANDISrun = LANDISruns[which(Climates==Climate)]
      RunCase = paste0("P",SelectedModel,"S",Scenario,Climate,"R",LANDISrun)
      
      # Streamflow data
      DHSVMstreamflow = as.data.frame(fread(paste0("../../../TCSI_Results/",RunCase,"_WSHDsaveMatrix.csv")))
      
      # Create timeseries of yearly 1-day peak
      for (wshd in unique(WatershedData$WatershedNum)){
        
        # Aggregate 3-hourly to daily
        DailyTimeseries = colSums(matrix(DHSVMstreamflow[,paste0("WSHD_",wshd)], nrow=8)) / 8
        
        YearlyPeak = c()
        for (yr in WaterYears){
          YearPtrs = which(DatesOnly >= as.Date(paste0("10-01-",yr-1), format="%m-%d-%Y") &
                             DatesOnly < as.Date(paste0("10-01-",yr), format="%m-%d-%Y"))
          YearlyPeak = c(YearlyPeak, max(DailyTimeseries[YearPtrs]))
        }
        
        WatershedData$YearlyPeakAvg_cms[WatershedData$WatershedNum==wshd &
                                          WatershedData$Climate==Climate &
                                          WatershedData$Scenario==Scenario &
                                          WatershedData$DHSVMmodel==SelectedModel] = mean(YearlyPeak)
      }
      print(paste(SelectedModel,Scenario,Climate))
    }
  }
}

WatershedData$YearlyPeakAvg_mmd = WatershedData$YearlyPeakAvg_cms * 1000 * (60*60*24) / (WatershedData$UpstreamArea_km2 * 1000^2)

################################################################################
# Comparison of mean annual peak flow between scenarios

WatershedData$RelS2YearlyPeakAvg_pct = NA
WatershedData$RelS2YearlyPeakAvg_mmd = NA

S2peaksCNRM = WatershedData$YearlyPeakAvg_mmd[(WatershedData$Scenario==2 & WatershedData$Climate=="cnrm")]
S2peaksMIROC = WatershedData$YearlyPeakAvg_mmd[(WatershedData$Scenario==2 & WatershedData$Climate=="miroc")]

for (Scenario in c(1,3:6)){
  
  Ptrs = which(WatershedData$Scenario==Scenario & WatershedData$Climate=="cnrm")
  WatershedData$RelS2YearlyPeakAvg_pct[Ptrs] = WatershedData$YearlyPeakAvg_mmd[Ptrs] / S2peaksCNRM - 1
  WatershedData$RelS2YearlyPeakAvg_mmd[Ptrs] = WatershedData$YearlyPeakAvg_mmd[Ptrs] - S2peaksCNRM
  
  Ptrs = which(WatershedData$Scenario==Scenario & WatershedData$Climate=="miroc")
  WatershedData$RelS2YearlyPeakAvg_pct[Ptrs] = WatershedData$YearlyPeakAvg_mmd[Ptrs] / S2peaksMIROC - 1
  WatershedData$RelS2YearlyPeakAvg_mmd[Ptrs] = WatershedData$YearlyPeakAvg_mmd[Ptrs] - S2peaksMIROC
}

plot(WatershedData$S2RelContribStreamflow_pct,WatershedData$RelS2YearlyPeakAvg_pct)
plot(WatershedData$S2RelContribStreamflow_mmd,WatershedData$RelS2YearlyPeakAvg_mmd)

write.csv(WatershedData,"WatershedData_WithMeanAnnualPeak.csv")

################################################################################
# HUC12 rasters

library(terra)

HUC12map = rast("../Overview_Maps/TCSI_HUC12ish_Watersheds.tif")
WSHDids = unique(unlist(HUC12map[,]))
WSHDids = sort(WSHDids[is.finite(WSHDids)])

PeakTrendCNRM = NA * HUC12map
PeakTrendMIROC = NA * HUC12map
PeakTrendS6S2CNRM = NA * HUC12map
PeakTrendS6S2MIROC = NA * HUC12map
YieldCNRM = NA * HUC12map
YieldMIROC = NA * HUC12map
YieldS6S2CNRM = NA * HUC12map
YieldS6S2MIROC = NA * HUC12map

for (WSHDid in WSHDids){
  Ptrs = which(HUC12map[,]==WSHDid)
  
  PeakTrendCNRM[Ptrs] = mean(WatershedData$YearlyPeakSensSlope_mmdyr[WatershedData$WatershedNum==WSHDid &
                                                                       WatershedData$Climate=="cnrm" &
                                                                       WatershedData$Scenario==2])
  
  PeakTrendMIROC[Ptrs] = mean(WatershedData$YearlyPeakSensSlope_mmdyr[WatershedData$WatershedNum==WSHDid &
                                                                   WatershedData$Climate=="miroc" &
                                                                   WatershedData$Scenario==2])
  
  PeakTrendS6S2CNRM[Ptrs] = mean(WatershedData$S2RelYearlyPeakSensSlope_mmdyr[WatershedData$WatershedNum==WSHDid &
                                                                      WatershedData$Climate=="cnrm" &
                                                                      WatershedData$Scenario==6])
  
  PeakTrendS6S2MIROC[Ptrs] = mean(WatershedData$S2RelYearlyPeakSensSlope_mmdyr[WatershedData$WatershedNum==WSHDid &
                                                                       WatershedData$Climate=="miroc" &
                                                                       WatershedData$Scenario==6])
  
  YieldCNRM[Ptrs] = mean(WatershedData$ContribStreamflow_mmd[WatershedData$WatershedNum==WSHDid &
                                                              WatershedData$Climate=="cnrm" &
                                                              WatershedData$Scenario==2])
  
  YieldMIROC[Ptrs] = mean(WatershedData$ContribStreamflow_mmd[WatershedData$WatershedNum==WSHDid &
                                                               WatershedData$Climate=="miroc" &
                                                               WatershedData$Scenario==2])
  
  YieldS6S2CNRM[Ptrs] = mean(WatershedData$S2RelContribStreamflow_mmd[WatershedData$WatershedNum==WSHDid &
                                                                  WatershedData$Climate=="cnrm" &
                                                                  WatershedData$Scenario==6])
  
  YieldS6S2MIROC[Ptrs] = mean(WatershedData$S2RelContribStreamflow_mmd[WatershedData$WatershedNum==WSHDid &
                                                                   WatershedData$Climate=="miroc" &
                                                                   WatershedData$Scenario==6])
  print(WSHDid)
}

writeRaster(PeakTrendCNRM,"SupplementaryFigures/PeakTrend_mmdyr_CNRM.tif",overwrite=TRUE)
writeRaster(PeakTrendMIROC,"SupplementaryFigures/PeakTrend_mmdyr_MIROC.tif",overwrite=TRUE)

writeRaster(PeakTrendS6S2CNRM,"SupplementaryFigures/PeakTrend_S6-S2_mmdyr_CNRM.tif",overwrite=TRUE)
writeRaster(PeakTrendS6S2MIROC,"SupplementaryFigures/PeakTrend_S6-S2_mmdyr_MIROC.tif",overwrite=TRUE)

writeRaster(YieldCNRM*365.25,"SupplementaryFigures/StreamflowGeneration_mmyr_CNRM.tif",overwrite=TRUE)
writeRaster(YieldMIROC*365.25,"SupplementaryFigures/StreamflowGeneration_mmyr_MIROC.tif",overwrite=TRUE)

writeRaster(YieldS6S2CNRM*365.25,"SupplementaryFigures/StreamflowGeneration_S6-S2_mmyr_CNRM.tif",overwrite=TRUE)
writeRaster(YieldS6S2MIROC*365.25,"SupplementaryFigures/StreamflowGeneration_S6-S2_mmyr_MIROC.tif",overwrite=TRUE)












