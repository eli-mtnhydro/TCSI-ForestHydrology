
dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Water_Balance_Partitioning/"
setwd(dir)

RelativeWaterBalancePlotData = read.csv("RelativeWaterBalancePlotData.csv")

unique(RelativeWaterBalancePlotData$VarType)

################################################################################

WaterDataCNRM = RelativeWaterBalancePlotData[(RelativeWaterBalancePlotData$Scenario==6 &
                                                RelativeWaterBalancePlotData$Climate=="Wetter Future Climate"),]
WaterDataMIROC = RelativeWaterBalancePlotData[(RelativeWaterBalancePlotData$Scenario==6 &
                                                RelativeWaterBalancePlotData$Climate=="Drier Future Climate"),]

plot(WaterDataCNRM$WaterDepth_mmPerYr,WaterDataMIROC$WaterDepth_mmPerYr)
plot(WaterDataCNRM$WaterDepth_mmPerYr[WaterDataCNRM$VarType=="Overstory\nInterception Loss"],
     WaterDataMIROC$WaterDepth_mmPerYr[WaterDataMIROC$VarType=="Overstory\nInterception Loss"])

# Overstory transpiration
OverstoryTranspCNRM = mean(WaterDataCNRM$WaterDepth_mmPerYr[WaterDataCNRM$VarType=="Overstory\nTranspiration"])
OverstoryTranspMIROC = mean(WaterDataMIROC$WaterDepth_mmPerYr[WaterDataMIROC$VarType=="Overstory\nTranspiration"])
t.test(RelativeWaterBalancePlotData$WaterDepth_mmPerYr[RelativeWaterBalancePlotData$VarType=="Overstory\nTranspiration"])

# Understory transpiration
UnderstoryTranspCNRM = mean(WaterDataCNRM$WaterDepth_mmPerYr[WaterDataCNRM$VarType=="Understory\nTranspiration"])
UnderstoryTranspMIROC = mean(WaterDataMIROC$WaterDepth_mmPerYr[WaterDataMIROC$VarType=="Understory\nTranspiration"])
t.test(RelativeWaterBalancePlotData$WaterDepth_mmPerYr[RelativeWaterBalancePlotData$VarType=="Understory\nTranspiration"])

# Overstory interception loss
OverstoryIntLossCNRM = mean(WaterDataCNRM$WaterDepth_mmPerYr[WaterDataCNRM$VarType=="Overstory\nInterception Loss"])
OverstoryIntLossMIROC = mean(WaterDataMIROC$WaterDepth_mmPerYr[WaterDataMIROC$VarType=="Overstory\nInterception Loss"])
t.test(RelativeWaterBalancePlotData$WaterDepth_mmPerYr[RelativeWaterBalancePlotData$VarType=="Overstory\nInterception Loss"])

# Understory interception loss
UnderstoryIntLossCNRM = mean(WaterDataCNRM$WaterDepth_mmPerYr[WaterDataCNRM$VarType=="Understory\nInterception Loss"])
UnderstoryIntLossMIROC = mean(WaterDataMIROC$WaterDepth_mmPerYr[WaterDataMIROC$VarType=="Understory\nInterception Loss"])
t.test(RelativeWaterBalancePlotData$WaterDepth_mmPerYr[RelativeWaterBalancePlotData$VarType=="Understory\nInterception Loss"])

# Streamflow
StreamCNRM = mean(WaterDataCNRM$WaterDepth_mmPerYr[WaterDataCNRM$VarType=="Streamflow"])
StreamMIROC = mean(WaterDataMIROC$WaterDepth_mmPerYr[WaterDataMIROC$VarType=="Streamflow"])
t.test(RelativeWaterBalancePlotData$WaterDepth_mmPerYr[RelativeWaterBalancePlotData$VarType=="Streamflow"])

##########
# Compensation of overstory and understory

UnderstoryTranspCNRM / OverstoryTranspCNRM
UnderstoryTranspMIROC / OverstoryTranspMIROC

UnderstoryIntLossCNRM / OverstoryIntLossCNRM
UnderstoryIntLossMIROC / OverstoryIntLossMIROC

##########
# Fractional streamflow attribution

TotalGainCNRM = OverstoryTranspCNRM + UnderstoryTranspCNRM + OverstoryIntLossCNRM + UnderstoryIntLossCNRM
TotalGainMIROC = OverstoryTranspMIROC + UnderstoryTranspMIROC + OverstoryIntLossMIROC + UnderstoryIntLossMIROC

# ET gain
(OverstoryTranspCNRM + UnderstoryTranspCNRM) / TotalGainCNRM
(OverstoryTranspMIROC + UnderstoryTranspMIROC) / TotalGainMIROC

# Interception gain
(OverstoryIntLossCNRM + UnderstoryIntLossCNRM) / TotalGainCNRM
(OverstoryIntLossMIROC + UnderstoryIntLossMIROC) / TotalGainMIROC

################################################################################
# Snow vs. rain

AggDataYearly = read.csv("AggregatedYearlyData_AllBasinsModelsClimatesScenarios.csv")[,-1]
head(AggDataYearly)

BasinNames = unique(AggDataYearly$Basin)
BasinAreas = c()
for (BasinName in BasinNames){
  
  Mask = rast(paste0("TCSImask_",BasinName,"Domain.tif"))
  plot(Mask)
  
  BasinArea = length(which(Mask[,]==1)) * 90^2 / (1000^2)
  BasinAreas = c(BasinAreas,BasinArea)
}

library(ggplot2)

ggplot(AggDataYearly[AggDataYearly$DHSVMmodel==276 & AggDataYearly$Scenario==2,]) +
  geom_line(aes(x=WaterYear,y=Snow_m/Precip_m,color=Climate)) +
  facet_wrap(~Basin,nrow=2)

AggAggData = data.frame(WaterYear=sort(rep(2015:2099,2)),
                        Climate=c("CNRM","MIROC"),
                        Precip_m=NA,
                        Snow_m=NA)

for (yr in AggAggData$WaterYear){
  
  PtrsCNRM = which(AggDataYearly$Scenario==2 & AggDataYearly$WaterYear==yr & AggDataYearly$Climate=="cnrm")
  PtrsMIROC = which(AggDataYearly$Scenario==2 & AggDataYearly$WaterYear==yr & AggDataYearly$Climate=="miroc")
  
  PtrCNRM = which(AggAggData$WaterYear==yr & AggAggData$Climate=="CNRM")
  PtrMIROC = which(AggAggData$WaterYear==yr & AggAggData$Climate=="MIROC")
  
  AreasCNRM = BasinAreas[match(AggDataYearly$Basin[PtrsCNRM],BasinNames)]
  AreasMIROC = BasinAreas[match(AggDataYearly$Basin[PtrsMIROC],BasinNames)]
  
  AggAggData$Precip_m[PtrCNRM] = sum(AggDataYearly$Precip_m[PtrsCNRM] * 365.25 * (24/3) * AreasCNRM) / sum(AreasCNRM)
  AggAggData$Precip_m[PtrMIROC] = sum(AggDataYearly$Precip_m[PtrsMIROC] * 365.25 * (24/3) * AreasMIROC) / sum(AreasMIROC)
  AggAggData$Snow_m[PtrCNRM] = sum(AggDataYearly$Snow_m[PtrsCNRM] * 365.25 * (24/3) * AreasCNRM) / sum(AreasCNRM)
  AggAggData$Snow_m[PtrMIROC] = sum(AggDataYearly$Snow_m[PtrsMIROC] * 365.25 * (24/3) * AreasMIROC) / sum(AreasMIROC)
}

# Note on units:
# Starts as [depth, m] for each 3-hour timestep as output by DHSVM
# Then averaged to [depth/time, m/3hr, daily avg.] in CompileDHSVMresultsAggregatedValues.R
# Then averaged to [depth/time, m/3hr, yearly avg.] in VisualizeAggregatedResults.R
# Then here we convert back to [depth, m] by multiplying avg. [m/3hr] * 365.25 * 3 hrs.

ggplot(AggAggData) +
  geom_line(aes(x=WaterYear,y=Snow_m/Precip_m,color=Climate))

SnowCNRM = sum(AggAggData$Snow_m[AggAggData$Climate=="CNRM"])
SnowMIROC = sum(AggAggData$Snow_m[AggAggData$Climate=="MIROC"])
PrecipCNRM = sum(AggAggData$Precip_m[AggAggData$Climate=="CNRM"])
PrecipMIROC = sum(AggAggData$Precip_m[AggAggData$Climate=="MIROC"])

SnowCNRM / PrecipCNRM
SnowMIROC / PrecipMIROC

SnowCNRM = sum(AggAggData$Snow_m[AggAggData$Climate=="CNRM" & AggAggData$WaterYear >= 2050])
SnowMIROC = sum(AggAggData$Snow_m[AggAggData$Climate=="MIROC" & AggAggData$WaterYear >= 2050])
PrecipCNRM = sum(AggAggData$Precip_m[AggAggData$Climate=="CNRM" & AggAggData$WaterYear >= 2050])
PrecipMIROC = sum(AggAggData$Precip_m[AggAggData$Climate=="MIROC" & AggAggData$WaterYear >= 2050])

SnowCNRM / PrecipCNRM
SnowMIROC / PrecipMIROC

################################################################################
# ET comparison

setwd("../../NumericResults/")

AggData1yuba = read.table("CalValResults/Val_Params195_Yuba/Aggregated.Values",header=TRUE)
AggData2yuba = read.table("CalValResults/Val_Params270_Yuba/Aggregated.Values",header=TRUE)
AggData3yuba = read.table("CalValResults/Val_Params276_Yuba/Aggregated.Values",header=TRUE)

AggData1ameri = read.table("CalValResults/Val_Params195_American/Aggregated.Values",header=TRUE)
AggData2ameri = read.table("CalValResults/Val_Params270_American/Aggregated.Values",header=TRUE)
AggData3ameri = read.table("CalValResults/Val_Params276_American/Aggregated.Values",header=TRUE)

ETyuba = rowMeans(data.frame(AggData1yuba$TotalET,AggData2yuba$TotalET,AggData3yuba$TotalET))
ETameri = rowMeans(data.frame(AggData1ameri$TotalET,AggData2ameri$TotalET,AggData3ameri$TotalET))

# Drop 1 spinup year and 10/1 on last year
nTotal = length(ETyuba)
ETyuba = ETyuba[-c(1:(365*8),nTotal)]
ETameri = ETameri[-c(1:(365*8),nTotal)]

mean(ETyuba) * 8 * 365.25 * 1000 # m/3 hr to mm/yr
mean(ETameri) * 8 * 365.25 * 1000 # m/3 hr to mm/yr

AllYears = as.numeric(format(as.Date(AggData1yuba$Date[-c(1:(365*8),nTotal)],format="%m/%d/%Y"),"%Y"))
AllMonths = as.numeric(format(as.Date(AggData1yuba$Date[-c(1:(365*8),nTotal)],format="%m/%d/%Y"),"%m"))
AllYears[(AllMonths > 9)] = AllYears[(AllMonths > 9)] + 1

ETyubaYrly = aggregate(ETyuba,list(AllYears),sum)
ETameriYrly = aggregate(ETameri,list(AllYears),sum)

mean(ETyubaYrly$x) * 1000
mean(ETameriYrly$x) * 1000
range(ETyubaYrly$x) * 1000
range(ETameriYrly$x) * 1000

################################################################################
# Peak flow storm processes

PeakStormData = read.csv("../Peak_Flow_Processes/PeakFlowProcessData.csv")[,-1]
head(PeakStormData)

nBiggestStorms = 10
PeakFlowDataSubset = PeakStormData[(PeakStormData$PrecipIntensityRank <= nBiggestStorms),]

mean(PeakFlowDataSubset$Precip_DuringStorm_mm)
mean(PeakFlowDataSubset$PrecipIntensity_mmd)

t.test(PeakFlowDataSubset$IntEvapTotal_DuringStorm_RelS2_mm[PeakFlowDataSubset$Scenario==6])
t.test(PeakFlowDataSubset$SnowMelt_TotDuringStorm_RelS2_mm[PeakFlowDataSubset$Scenario==6])

mean(PeakFlowDataSubset$Snow_DuringStorm_mm)
mean(PeakFlowDataSubset$Precip_DuringStorm_mm)
33.25 / 404.08

PeakFlowDataSubset$SnowEnergyTotal_RelS2_wm2 = PeakFlowDataSubset$SnowSW_AvgDuringStorm_RelS2_wm2 +
  PeakFlowDataSubset$SnowLW_AvgDuringStorm_RelS2_wm2 +
  PeakFlowDataSubset$SnowSensible_AvgDuringStorm_RelS2_wm2 +
  PeakFlowDataSubset$SnowLatent_AvgDuringStorm_RelS2_wm2 +
  PeakFlowDataSubset$SnowAdvected_AvgDuringStorm_RelS2_wm2

t.test(PeakFlowDataSubset$SnowSW_AvgDuringStorm_RelS2_wm2[PeakFlowDataSubset$Scenario==6])
t.test(PeakFlowDataSubset$SnowLW_AvgDuringStorm_RelS2_wm2[PeakFlowDataSubset$Scenario==6])
t.test(PeakFlowDataSubset$SnowSensible_AvgDuringStorm_RelS2_wm2[PeakFlowDataSubset$Scenario==6])
t.test(PeakFlowDataSubset$SnowLatent_AvgDuringStorm_RelS2_wm2[PeakFlowDataSubset$Scenario==6])
t.test(PeakFlowDataSubset$SnowAdvected_AvgDuringStorm_RelS2_wm2[PeakFlowDataSubset$Scenario==6])
t.test(PeakFlowDataSubset$SnowEnergyTotal_RelS2_wm2[PeakFlowDataSubset$Scenario==6])

PeakFlowDataSubset = PeakFlowDataSubset[PeakFlowDataSubset$Scenario==6,]

length(which(PeakFlowDataSubset$IntEvapTotal_DuringStorm_RelS2_mm > 0))
length(which(PeakFlowDataSubset$SnowMelt_TotDuringStorm_RelS2_mm < 0))

4 / 240
27 / 240


