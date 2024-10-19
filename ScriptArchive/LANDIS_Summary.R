
library(ggplot2)
library(terra)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Overview_Maps/"
setwd(dir)

Mask = rast("TCSImask_MergedDomain.tif")

################################################################################
# Historical

Year0landscapeLAI = rast("LandscapeLAI_Historical.tif")

plot(Year0landscapeLAI)

mean(unlist(Year0landscapeLAI[which(Mask[,]==1)]))
median(unlist(Year0landscapeLAI[which(Mask[,]==1)]))
quantile(unlist(Year0landscapeLAI[which(Mask[,]==1)]),0.9)

MeanLAIFCyr0 = mean(unlist(Year0landscapeLAI[which(Mask[,]==1)]))

# Fraction with non-zero LAI
length(which(Mask[,]==1 & Year0landscapeLAI[,] > 0)) / length(which(Mask[,]==1))

################################################################################

ScenarioLANDISdata = data.frame(Scenarios=1:6,
                                LAImean=NA,
                                LAImedian=NA,
                                LAI90pct=NA)

for (Scenario in 1:6){
  
  LAImapCNRM = rast(paste0("../../../TCSI_Setup/LANDIS/Processed_FutureMaps/Scenario",
                           Scenario,"_CNRM-CM5-RCP8.5_Run1_2094_LAIsummer.tif"))
  FCmapCNRM = rast(paste0("../../../TCSI_Setup/LANDIS/Processed_FutureMaps/Scenario",
                          Scenario,"_CNRM-CM5-RCP8.5_Run1_2094_CanopyCover.tif"))
  LAIFCyr80CNRM = LAImapCNRM * abs(FCmapCNRM)
  
  LAImapMIROC = rast(paste0("../../../TCSI_Setup/LANDIS/Processed_FutureMaps/Scenario",
                            Scenario,"_MIROC5-RCP8.5_Run4_2094_LAIsummer.tif"))
  FCmapMIROC = rast(paste0("../../../TCSI_Setup/LANDIS/Processed_FutureMaps/Scenario",
                           Scenario,"_MIROC5-RCP8.5_Run4_2094_CanopyCover.tif"))
  LAIFCyr80MIROC = LAImapMIROC * abs(FCmapMIROC)
  
  LAIFCyr80 = mean(LAIFCyr80CNRM,LAIFCyr80MIROC)
  LAIFCyr80[which(LAIFCyr80[,] < 0)] = 0
  LAIFCyr80[which(!is.finite(LAIFCyr80[,]))] = 0
  LAIFCyr80[which(Mask[,]!=1)] = NA
  
  LAIFCvals = unlist(LAIFCyr80[which(Mask[,]==1)])
  
  ScenarioLANDISdata$LAImean[ScenarioLANDISdata$Scenarios==Scenario] = mean(LAIFCvals)
  ScenarioLANDISdata$LAImedian[ScenarioLANDISdata$Scenarios==Scenario] = median(LAIFCvals)
  ScenarioLANDISdata$LAI90pct[ScenarioLANDISdata$Scenarios==Scenario] = quantile(LAIFCvals,0.9)
}

ScenarioLANDISdata$LAImeanPctChange = (ScenarioLANDISdata$LAImean - MeanLAIFCyr0) / MeanLAIFCyr0

print(signif(ScenarioLANDISdata,3))

# Final S6 vs. S2
FinalLAIdiff = rast("LandscapeLAI_2099_S6climateAvg.tif") - rast("LandscapeLAI_2099_S2climateAvg.tif")

ScenarioLANDISdata$LAImean[ScenarioLANDISdata$Scenarios==6] - ScenarioLANDISdata$LAImean[ScenarioLANDISdata$Scenarios==2]

mean(unlist(FinalLAIdiff[which(Mask[,]==1)])) / mean(unlist(Year0landscapeLAI[which(Mask[,]==1)]))

# Maximum sub-watershed change

FinalLAIdiffWatersheds = rast("LandscapeLAI_2099_S6-S2climateAvg.tif")
sort(unique(unlist(FinalLAIdiffWatersheds[,])))

FinalLAIwatersheds = rast("LandscapeLAI_2099_S2climateAvgWatersheds.tif")
Ptr = which.min(FinalLAIdiffWatersheds[,])
FinalLAIdiffWatersheds[Ptr]
FinalLAIdiffWatersheds[Ptr] / FinalLAIwatersheds[Ptr]
