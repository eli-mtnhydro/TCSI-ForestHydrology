
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

# Understory transpiration
UnderstoryTranspCNRM = mean(WaterDataCNRM$WaterDepth_mmPerYr[WaterDataCNRM$VarType=="Understory\nTranspiration"])
UnderstoryTranspMIROC = mean(WaterDataMIROC$WaterDepth_mmPerYr[WaterDataMIROC$VarType=="Understory\nTranspiration"])

# Overstory interception loss
OverstoryIntLossCNRM = mean(WaterDataCNRM$WaterDepth_mmPerYr[WaterDataCNRM$VarType=="Overstory\nInterception Loss"])
OverstoryIntLossMIROC = mean(WaterDataMIROC$WaterDepth_mmPerYr[WaterDataMIROC$VarType=="Overstory\nInterception Loss"])

# Understory interception loss
UnderstoryIntLossCNRM = mean(WaterDataCNRM$WaterDepth_mmPerYr[WaterDataCNRM$VarType=="Understory\nInterception Loss"])
UnderstoryIntLossMIROC = mean(WaterDataMIROC$WaterDepth_mmPerYr[WaterDataMIROC$VarType=="Understory\nInterception Loss"])

# Streamflow
StreamCNRM = mean(WaterDataCNRM$WaterDepth_mmPerYr[WaterDataCNRM$VarType=="Streamflow"])
StreamMIROC = mean(WaterDataMIROC$WaterDepth_mmPerYr[WaterDataMIROC$VarType=="Streamflow"])

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



