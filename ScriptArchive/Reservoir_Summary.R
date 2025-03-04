
dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/NumericResults/"
setwd(dir)

ReservoirData = read.csv("../Figures/Reservoir_FDCs/Reservoirs_DailyStreamflowTimeseries.csv")

FlowDurationData = read.csv("../Figures/Reservoir_FDCs/FlowDurationData.csv")

WatershedData = read.csv("WatershedData_WithMeanAnnualPeak.csv")

################################################################################
# Peak flows

# Max
# S2maxCNRM.Bullards = max(FlowDurationData$DailyFlow_cms[(FlowDurationData$Reservoir=="New Bullards Bar Reservoir" &
#                                                           FlowDurationData$Scenario==2 &
#                                                           FlowDurationData$Climate=="CNRM-CM5 RCP 8.5")])
# S2maxMIROC.Bullards = max(FlowDurationData$DailyFlow_cms[(FlowDurationData$Reservoir=="New Bullards Bar Reservoir" &
#                                                            FlowDurationData$Scenario==2 &
#                                                            FlowDurationData$Climate=="MIROC5 RCP 8.5")])
# S2maxCNRM.Folsom = max(FlowDurationData$DailyFlow_cms[(FlowDurationData$Reservoir=="Folsom Lake" &
#                                                            FlowDurationData$Scenario==2 &
#                                                            FlowDurationData$Climate=="CNRM-CM5 RCP 8.5")])
# S2maxMIROC.Folsom = max(FlowDurationData$DailyFlow_cms[(FlowDurationData$Reservoir=="Folsom Lake" &
#                                                             FlowDurationData$Scenario==2 &
#                                                             FlowDurationData$Climate=="MIROC5 RCP 8.5")])
# S2maxCNRM.Bullards / S2maxMIROC.Bullards
# S2maxCNRM.Folsom / S2maxMIROC.Folsom

###########
# Climate effect

# Mean annual peak flow
S2yearlyCNRM.Bullards = mean(WatershedData$YearlyPeakAvg_mmd[WatershedData$WatershedNum==57 &
                                                               WatershedData$Climate=="cnrm" &
                                                               WatershedData$Scenario==2])
S2yearlyMIROC.Bullards = mean(WatershedData$YearlyPeakAvg_mmd[WatershedData$WatershedNum==57 &
                                                               WatershedData$Climate=="miroc" &
                                                               WatershedData$Scenario==2])
S2yearlyCNRM.Folsom = mean(WatershedData$YearlyPeakAvg_mmd[WatershedData$WatershedNum==3 &
                                                               WatershedData$Climate=="cnrm" &
                                                               WatershedData$Scenario==2])
S2yearlyMIROC.Folsom = mean(WatershedData$YearlyPeakAvg_mmd[WatershedData$WatershedNum==3 &
                                                               WatershedData$Climate=="miroc" &
                                                               WatershedData$Scenario==2])

S2yearlyCNRM.Bullards / S2yearlyMIROC.Bullards

S2yearlyCNRM.Folsom / S2yearlyMIROC.Folsom

###########
# Scenario effect

# Mean annual peak flow
S6yearlyCNRM.Bullards = mean(WatershedData$YearlyPeakAvg_mmd[WatershedData$WatershedNum==57 &
                                                               WatershedData$Climate=="cnrm" &
                                                               WatershedData$Scenario==6])
S6yearlyMIROC.Bullards = mean(WatershedData$YearlyPeakAvg_mmd[WatershedData$WatershedNum==57 &
                                                                WatershedData$Climate=="miroc" &
                                                                WatershedData$Scenario==6])
S6yearlyCNRM.Folsom = mean(WatershedData$YearlyPeakAvg_mmd[WatershedData$WatershedNum==3 &
                                                             WatershedData$Climate=="cnrm" &
                                                             WatershedData$Scenario==6])
S6yearlyMIROC.Folsom = mean(WatershedData$YearlyPeakAvg_mmd[WatershedData$WatershedNum==3 &
                                                              WatershedData$Climate=="miroc" &
                                                              WatershedData$Scenario==6])

S6yearlyCNRM.Bullards / S2yearlyCNRM.Bullards
S6yearlyMIROC.Bullards / S2yearlyMIROC.Bullards

S6yearlyCNRM.Folsom / S2yearlyCNRM.Folsom
S6yearlyMIROC.Folsom / S2yearlyMIROC.Folsom

###########
# Relative uncertainty

ClimateEffect.Bullards = (S2yearlyCNRM.Bullards / S2yearlyMIROC.Bullards) - 1
ClimateEffect.Folsom = (S2yearlyCNRM.Folsom / S2yearlyMIROC.Folsom) - 1

ScenarioEffect.Bullards = mean(c(S6yearlyCNRM.Bullards / S2yearlyCNRM.Bullards,
                                 S6yearlyMIROC.Bullards / S2yearlyMIROC.Bullards)) - 1
ScenarioEffect.Folsom = mean(c(S6yearlyCNRM.Folsom / S2yearlyCNRM.Folsom,
                                 S6yearlyMIROC.Folsom / S2yearlyMIROC.Folsom)) - 1

ClimateEffect.Bullards / ScenarioEffect.Bullards
ClimateEffect.Folsom / ScenarioEffect.Folsom

################################################################################
# Water yield

# Average across DHSVM models
DHSVMmodels = unique(ReservoirData$DHSVMmodel)

HydrographAvgData = data.frame()

for (Climate in c("CNRM-CM5 RCP 8.5","MIROC5 RCP 8.5")){
  for (Scenario in c(2,4,6)){
    for (Reservoir in c("New Bullards Bar Reservoir","Folsom Lake")){

      Ptrs = which(ReservoirData$Climate==Climate &
                     ReservoirData$Scenario==Scenario &
                     ReservoirData$Reservoir==Reservoir)

      # Average across DHSVM models
      HydrographAvg = rowMeans(matrix(ReservoirData$DailyAvgQ_cms[Ptrs], ncol=length(DHSVMmodels)))

      HydrographAvgData = rbind(HydrographAvgData,
                                data.frame(Date=unique(ReservoirData$Date),
                                           DailyAvgQ_cms=HydrographAvg,
                                           Reservoir=Reservoir,
                                           Scenario=Scenario,
                                           Climate=Climate))
    }
  }
}

dim(HydrographAvgData)

# Calculate mean instantaneous m3/s and convert to yearly yield in acre-ft.
ConversionFact = (60*60*24*365.25) / 1233.48

S2yieldCNRM.Bullards = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="New Bullards Bar Reservoir" &
                                                              HydrographAvgData$Scenario==2 &
                                                              HydrographAvgData$Climate=="CNRM-CM5 RCP 8.5"]) * ConversionFact
S2yieldMIROC.Bullards = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="New Bullards Bar Reservoir" &
                                                              HydrographAvgData$Scenario==2 &
                                                              HydrographAvgData$Climate=="MIROC5 RCP 8.5"]) * ConversionFact
S2yieldCNRM.Folsom = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="Folsom Lake" &
                                                              HydrographAvgData$Scenario==2 &
                                                              HydrographAvgData$Climate=="CNRM-CM5 RCP 8.5"]) * ConversionFact
S2yieldMIROC.Folsom = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="Folsom Lake" &
                                                              HydrographAvgData$Scenario==2 &
                                                              HydrographAvgData$Climate=="MIROC5 RCP 8.5"]) * ConversionFact


S6yieldCNRM.Bullards = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="New Bullards Bar Reservoir" &
                                                              HydrographAvgData$Scenario==6 &
                                                              HydrographAvgData$Climate=="CNRM-CM5 RCP 8.5"]) * ConversionFact
S6yieldMIROC.Bullards = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="New Bullards Bar Reservoir" &
                                                               HydrographAvgData$Scenario==6 &
                                                               HydrographAvgData$Climate=="MIROC5 RCP 8.5"]) * ConversionFact
S6yieldCNRM.Folsom = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="Folsom Lake" &
                                                            HydrographAvgData$Scenario==6 &
                                                            HydrographAvgData$Climate=="CNRM-CM5 RCP 8.5"]) * ConversionFact
S6yieldMIROC.Folsom = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="Folsom Lake" &
                                                             HydrographAvgData$Scenario==6 &
                                                             HydrographAvgData$Climate=="MIROC5 RCP 8.5"]) * ConversionFact

# Absolute difference
S6yieldCNRM.Bullards - S2yieldCNRM.Bullards
S6yieldMIROC.Bullards - S2yieldMIROC.Bullards

S6yieldCNRM.Folsom - S2yieldCNRM.Folsom
S6yieldMIROC.Folsom - S2yieldMIROC.Folsom

# Relative difference
S6yieldCNRM.Bullards / S2yieldCNRM.Bullards
S6yieldMIROC.Bullards / S2yieldMIROC.Bullards

S6yieldCNRM.Folsom / S2yieldCNRM.Folsom
S6yieldMIROC.Folsom / S2yieldMIROC.Folsom

# Variation between climates
S2yieldCNRM.Bullards / S2yieldMIROC.Bullards
S2yieldCNRM.Folsom / S2yieldMIROC.Folsom

(S6yieldCNRM.Bullards - S2yieldCNRM.Bullards) / (S6yieldMIROC.Bullards - S2yieldMIROC.Bullards)
(S6yieldCNRM.Folsom - S2yieldCNRM.Folsom) / (S6yieldMIROC.Folsom - S2yieldMIROC.Folsom)

##########
# 10 driest years in each climate

PrecipData = read.csv("../../TCSI_Results/WaterYear_MACAclimateStats_TCSIbasins.csv")

CNRMdryYrs = PrecipData$WaterYear[match(sort(PrecipData$Prec[PrecipData$Climate=="CNRM-CM5 RCP 8.5"])[1:10],
                                        PrecipData$Prec)]
MIROCdryYrs = PrecipData$WaterYear[match(sort(PrecipData$Prec[PrecipData$Climate=="MIROC5 RCP 8.5"])[1:10],
                                        PrecipData$Prec)]

HydrographAvgData$WaterYear = as.numeric(format(as.Date(HydrographAvgData$Date,format="%Y-%m-%d"),"%Y"))
OctNovDecPtrs = which(as.numeric(format(as.Date(HydrographAvgData$Date,format="%Y-%m-%d"),"%m")) >= 10)
HydrographAvgData$WaterYear[OctNovDecPtrs] = HydrographAvgData$WaterYear[OctNovDecPtrs] + 1

PrecipData$AvgQ.Bullards = NA
PrecipData$AvgQ.Folsom = NA
for (WaterYear in 2015:2099){
  PrecipData$AvgQ.Bullards[PrecipData$Climate=="CNRM-CM5 RCP 8.5" &
                             PrecipData$WaterYear==WaterYear] = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="New Bullards Bar Reservoir" &
                                                                                                       HydrographAvgData$Scenario==2 &
                                                                                                       HydrographAvgData$Climate=="CNRM-CM5 RCP 8.5" &
                                                                                                       HydrographAvgData$WaterYear==WaterYear]) * ConversionFact
  PrecipData$AvgQ.Bullards[PrecipData$Climate=="MIROC5 RCP 8.5" &
                             PrecipData$WaterYear==WaterYear] = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="New Bullards Bar Reservoir" &
                                                                                                       HydrographAvgData$Scenario==2 &
                                                                                                       HydrographAvgData$Climate=="MIROC5 RCP 8.5" &
                                                                                                       HydrographAvgData$WaterYear==WaterYear]) * ConversionFact
  PrecipData$AvgQ.Folsom[PrecipData$Climate=="CNRM-CM5 RCP 8.5" &
                             PrecipData$WaterYear==WaterYear] = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="Folsom Lake" &
                                                                                                       HydrographAvgData$Scenario==2 &
                                                                                                       HydrographAvgData$Climate=="CNRM-CM5 RCP 8.5" &
                                                                                                       HydrographAvgData$WaterYear==WaterYear]) * ConversionFact
  PrecipData$AvgQ.Folsom[PrecipData$Climate=="MIROC5 RCP 8.5" &
                             PrecipData$WaterYear==WaterYear] = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="Folsom Lake" &
                                                                                                       HydrographAvgData$Scenario==2 &
                                                                                                       HydrographAvgData$Climate=="MIROC5 RCP 8.5" &
                                                                                                       HydrographAvgData$WaterYear==WaterYear]) * ConversionFact
}
plot(PrecipData$Prec,PrecipData$AvgQ.Bullards)
plot(PrecipData$Prec,PrecipData$AvgQ.Folsom)

# Same statistics as above, but only 10 driest years in each climate

S2yieldCNRM.Bullards = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="New Bullards Bar Reservoir" &
                                                              HydrographAvgData$Scenario==2 &
                                                              HydrographAvgData$Climate=="CNRM-CM5 RCP 8.5" &
                                                              HydrographAvgData$WaterYear %in% CNRMdryYrs]) * ConversionFact
S2yieldMIROC.Bullards = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="New Bullards Bar Reservoir" &
                                                               HydrographAvgData$Scenario==2 &
                                                               HydrographAvgData$Climate=="MIROC5 RCP 8.5" &
                                                               HydrographAvgData$WaterYear %in% MIROCdryYrs]) * ConversionFact
S2yieldCNRM.Folsom = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="Folsom Lake" &
                                                            HydrographAvgData$Scenario==2 &
                                                            HydrographAvgData$Climate=="CNRM-CM5 RCP 8.5" &
                                                            HydrographAvgData$WaterYear %in% CNRMdryYrs]) * ConversionFact
S2yieldMIROC.Folsom = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="Folsom Lake" &
                                                             HydrographAvgData$Scenario==2 &
                                                             HydrographAvgData$Climate=="MIROC5 RCP 8.5" &
                                                             HydrographAvgData$WaterYear %in% MIROCdryYrs]) * ConversionFact

S4yieldCNRM.Bullards = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="New Bullards Bar Reservoir" &
                                                              HydrographAvgData$Scenario==4 &
                                                              HydrographAvgData$Climate=="CNRM-CM5 RCP 8.5" &
                                                              HydrographAvgData$WaterYear %in% CNRMdryYrs]) * ConversionFact
S4yieldMIROC.Bullards = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="New Bullards Bar Reservoir" &
                                                               HydrographAvgData$Scenario==4 &
                                                               HydrographAvgData$Climate=="MIROC5 RCP 8.5" &
                                                               HydrographAvgData$WaterYear %in% MIROCdryYrs]) * ConversionFact
S4yieldCNRM.Folsom = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="Folsom Lake" &
                                                            HydrographAvgData$Scenario==4 &
                                                            HydrographAvgData$Climate=="CNRM-CM5 RCP 8.5" &
                                                            HydrographAvgData$WaterYear %in% CNRMdryYrs]) * ConversionFact
S4yieldMIROC.Folsom = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="Folsom Lake" &
                                                             HydrographAvgData$Scenario==4 &
                                                             HydrographAvgData$Climate=="MIROC5 RCP 8.5" &
                                                             HydrographAvgData$WaterYear %in% MIROCdryYrs]) * ConversionFact

S6yieldCNRM.Bullards = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="New Bullards Bar Reservoir" &
                                                              HydrographAvgData$Scenario==6 &
                                                              HydrographAvgData$Climate=="CNRM-CM5 RCP 8.5" &
                                                              HydrographAvgData$WaterYear %in% CNRMdryYrs]) * ConversionFact
S6yieldMIROC.Bullards = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="New Bullards Bar Reservoir" &
                                                               HydrographAvgData$Scenario==6 &
                                                               HydrographAvgData$Climate=="MIROC5 RCP 8.5" &
                                                               HydrographAvgData$WaterYear %in% MIROCdryYrs]) * ConversionFact
S6yieldCNRM.Folsom = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="Folsom Lake" &
                                                            HydrographAvgData$Scenario==6 &
                                                            HydrographAvgData$Climate=="CNRM-CM5 RCP 8.5" &
                                                            HydrographAvgData$WaterYear %in% CNRMdryYrs]) * ConversionFact
S6yieldMIROC.Folsom = mean(HydrographAvgData$DailyAvgQ_cms[HydrographAvgData$Reservoir=="Folsom Lake" &
                                                             HydrographAvgData$Scenario==6 &
                                                             HydrographAvgData$Climate=="MIROC5 RCP 8.5" &
                                                             HydrographAvgData$WaterYear %in% MIROCdryYrs]) * ConversionFact

# Absolute difference
S6yieldCNRM.Bullards - S2yieldCNRM.Bullards
S6yieldMIROC.Bullards - S2yieldMIROC.Bullards

S6yieldCNRM.Folsom - S2yieldCNRM.Folsom
S6yieldMIROC.Folsom - S2yieldMIROC.Folsom

# Relative difference
S6yieldCNRM.Bullards / S2yieldCNRM.Bullards
S6yieldMIROC.Bullards / S2yieldMIROC.Bullards

S6yieldCNRM.Folsom / S2yieldCNRM.Folsom
S6yieldMIROC.Folsom / S2yieldMIROC.Folsom

# Relative difference: S4 vs. S2
S4yieldCNRM.Bullards / S2yieldCNRM.Bullards
S4yieldMIROC.Bullards / S2yieldMIROC.Bullards

S4yieldCNRM.Folsom / S2yieldCNRM.Folsom
S4yieldMIROC.Folsom / S2yieldMIROC.Folsom


