
library(data.table)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Tradeoff_Figure/"
setwd(dir)

WatershedData = read.csv("WatershedData_WithMeanAnnualPeak.csv")

head(WatershedData)

################################################################################
# Total variation in sub-watershed streamflow generation effect

quantile(WatershedData$S2RelContribStreamflow_mmd[WatershedData$Scenario==6 & WatershedData$Climate=="cnrm"],
         c(0,0.1,0.5,0.9,1)) * 365.25

quantile(WatershedData$S2RelContribStreamflow_mmd[WatershedData$Scenario==6 & WatershedData$Climate=="miroc"],
         c(0,0.1,0.5,0.9,1)) * 365.25

################################################################################
# Relative to average yearly peak

##########
# Actual Sen's slope

WatershedData$YearlyPeakSensSlopeRelToAvg_pct = WatershedData$YearlyPeakSensSlope_mmdyr / WatershedData$YearlyPeakAvg_mmd
hist(WatershedData$YearlyPeakSensSlopeRelToAvg_pct)

SensSlopeS2cnrmPct = 100 * WatershedData$YearlyPeakSensSlopeRelToAvg_pct[WatershedData$Climate=="cnrm" & WatershedData$Scenario==2]
quantile(SensSlopeS2cnrmPct,c(0.01,0.1,0.25,0.5,0.75,0.9,0.99))

SensSlopeS2mirocPct = 100 * WatershedData$YearlyPeakSensSlopeRelToAvg_pct[WatershedData$Climate=="miroc" & WatershedData$Scenario==2]
quantile(SensSlopeS2mirocPct,c(0.01,0.1,0.25,0.5,0.75,0.9,0.99))

length(which(SensSlopeS2cnrmPct > 0)) / length(SensSlopeS2cnrmPct)
length(which(SensSlopeS2mirocPct > 0)) / length(SensSlopeS2mirocPct)

t.test(SensSlopeS2cnrmPct)
t.test(SensSlopeS2mirocPct)

##########
# Comparison of mean annual peak flow between scenarios

mean(WatershedData$RelS2YearlyPeakAvg_pct[WatershedData$Scenario==6 & WatershedData$Climate=="cnrm"])
mean(WatershedData$RelS2YearlyPeakAvg_pct[WatershedData$Scenario==6 & WatershedData$Climate=="miroc"])

max(WatershedData$RelS2YearlyPeakAvg_pct[WatershedData$Scenario==6 & WatershedData$Climate=="cnrm"])
max(WatershedData$RelS2YearlyPeakAvg_pct[WatershedData$Scenario==6 & WatershedData$Climate=="miroc"])

t.test(WatershedData$RelS2YearlyPeakAvg_pct[WatershedData$Scenario==6 & WatershedData$Climate=="cnrm"])
t.test(WatershedData$RelS2YearlyPeakAvg_pct[WatershedData$Scenario==6 & WatershedData$Climate=="miroc"])

################################################################################
# Correlation of peak flow trend and water yield trend

WatershedDataSubsetCNRM = WatershedData[(WatershedData$Scenario %in% c(1,3,4,5,6) &
                                           WatershedData$Climate=="cnrm"),]

plot(WatershedDataSubsetCNRM$S2RelContribStreamflow_mmd,
     WatershedDataSubsetCNRM$S2RelYearlyPeakSensSlope_mmdyr)

WatershedDataSubsetMIROC = WatershedData[(WatershedData$Scenario %in% c(1,3,4,5,6) &
                                           WatershedData$Climate=="miroc"),]

plot(WatershedDataSubsetMIROC$S2RelContribStreamflow_mmd,
     WatershedDataSubsetMIROC$S2RelYearlyPeakSensSlope_mmdyr)

cor.test(WatershedDataSubsetCNRM$S2RelContribStreamflow_mmd,
    WatershedDataSubsetCNRM$S2RelYearlyPeakSensSlope_mmdyr)

cor.test(WatershedDataSubsetMIROC$S2RelContribStreamflow_mmd,
    WatershedDataSubsetMIROC$S2RelYearlyPeakSensSlope_mmdyr)

################################################################################
# Variability in effect vs. variability in models

EffectVarData = data.frame(WatershedNum=unique(WatershedData$WatershedNum),
                           YieldRangeModel=NA,YieldRangeClimate=NA,YieldMean=NA,
                           PeakRangeModel=NA,PeakRangeClimate=NA,PeakMean=NA)

for (wshd in unique(WatershedData$WatershedNum)){
  
  WatershedDataSubset = WatershedData[(WatershedData$Scenario==6 &
                                         WatershedData$WatershedNum==wshd),]
  
  Ptr = which(EffectVarData$WatershedNum==wshd)
  
  EffectVarData$YieldMean[Ptr] = mean(WatershedDataSubset$S2RelContribStreamflow_mmd)
  EffectVarData$PeakMean[Ptr] = mean(WatershedDataSubset$S2RelYearlyPeakSensSlope_mmdyr)
  
  # Variability between models in each climate (then average between climates)
  WatershedDataSubsetCNRM = WatershedDataSubset[WatershedDataSubset$Climate=="cnrm",]
  WatershedDataSubsetMIROC = WatershedDataSubset[WatershedDataSubset$Climate=="miroc",]
  
  YieldRangeCNRM = diff(range(WatershedDataSubsetCNRM$S2RelContribStreamflow_mmd))
  YieldRangeMIROC = diff(range(WatershedDataSubsetMIROC$S2RelContribStreamflow_mmd))
  EffectVarData$YieldRangeModel[Ptr] = mean(c(YieldRangeCNRM,YieldRangeMIROC))
  
  PeakRangeCNRM = diff(range(WatershedDataSubsetCNRM$S2RelYearlyPeakSensSlope_mmdyr))
  PeakRangeMIROC = diff(range(WatershedDataSubsetMIROC$S2RelYearlyPeakSensSlope_mmdyr))
  EffectVarData$PeakRangeModel[Ptr] = mean(c(PeakRangeCNRM,PeakRangeMIROC))
  
  # Variability between climates (average of models in each climate)
  YieldCNRM = mean(WatershedDataSubsetCNRM$S2RelContribStreamflow_mmd)
  YieldMIROC = mean(WatershedDataSubsetMIROC$S2RelContribStreamflow_mmd)
  EffectVarData$YieldRangeClimate[Ptr] = abs(YieldCNRM - YieldMIROC)
  
  PeakCNRM = mean(WatershedDataSubsetCNRM$S2RelYearlyPeakSensSlope_mmdyr)
  PeakMIROC = mean(WatershedDataSubsetMIROC$S2RelYearlyPeakSensSlope_mmdyr)
  EffectVarData$PeakRangeClimate[Ptr] = abs(PeakCNRM - PeakMIROC)
  
  # # By scenario: SD of models/climates within one scenario
  # SDsYield = c()
  # SDsPeak = c()
  # for (Scenario in unique(WatershedDataSubset$Scenario)){
  #   Ptrs = which(WatershedDataSubset$Scenario==Scenario)
  #   SDsYield = c(SDsYield,sd(WatershedDataSubset$S2RelContribStreamflow_mmd[Ptrs]))
  #   SDsPeak = c(SDsPeak,sd(WatershedDataSubset$S2RelYearlyPeakSensSlope_mmdyr[Ptrs]))
  # }
  # EffectVarData$SDyield1Scenario[Ptr] = mean(SDsYield)
  # EffectVarData$SDpeak1Scenario[Ptr] = mean(SDsPeak)
  # 
  # # By model: SD of climates in a single model
  # SDsYield = c()
  # SDsPeak = c()
  # for (DHSVMmodel in unique(WatershedDataSubset$DHSVMmodel)){
  #   Ptrs = which(WatershedDataSubset$DHSVMmodel==DHSVMmodel)
  #   SDsYield = c(SDsYield,sd(WatershedDataSubset$S2RelContribStreamflow_mmd[Ptrs]))
  #   SDsPeak = c(SDsPeak,sd(WatershedDataSubset$S2RelYearlyPeakSensSlope_mmdyr[Ptrs]))
  # }
  # EffectVarData$SDyield1Model[Ptr] = mean(SDsYield)
  # EffectVarData$SDpeak1Model[Ptr] = mean(SDsPeak)
  # 
  # # By climate: SD of scenarios/models in a single climate
  # SDsYield = c()
  # SDsPeak = c()
  # for (Climate in unique(WatershedDataSubset$Climate)){
  #   Ptrs = which(WatershedDataSubset$Climate==Climate)
  #   SDsYield = c(SDsYield,sd(WatershedDataSubset$S2RelContribStreamflow_mmd[Ptrs]))
  #   SDsPeak = c(SDsPeak,sd(WatershedDataSubset$S2RelYearlyPeakSensSlope_mmdyr[Ptrs]))
  # }
  # EffectVarData$SDyield1Climate[Ptr] = mean(SDsYield)
  # EffectVarData$SDpeak1Climate[Ptr] = mean(SDsPeak)
}

mean(EffectVarData$YieldRangeModel) * 365.25
mean(EffectVarData$YieldRangeClimate) * 365.25
mean(EffectVarData$YieldMean) * 365.25
sd(EffectVarData$YieldMean) * 365.25
max(EffectVarData$YieldMean) * 365.25

mean(EffectVarData$PeakRangeModel) * 10
mean(EffectVarData$PeakRangeClimate) * 10
mean(EffectVarData$PeakMean) * 10
sd(EffectVarData$PeakMean) * 10
max(EffectVarData$PeakMean) * 10

mean(EffectVarData$YieldRangeModel) / mean(EffectVarData$YieldMean)
mean(EffectVarData$YieldRangeClimate) / mean(EffectVarData$YieldMean)

mean(EffectVarData$PeakRangeModel) / mean(EffectVarData$PeakMean)
mean(EffectVarData$PeakRangeClimate) / mean(EffectVarData$PeakMean)


