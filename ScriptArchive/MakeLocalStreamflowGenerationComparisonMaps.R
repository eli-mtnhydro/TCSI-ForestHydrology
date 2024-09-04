
library(ggplot2)
library(terra)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Streamflow_Generation_Maps/"
setwd(dir)

WaterYearStreamData = read.csv("WaterYearWatershedStreamflowResults.csv")[,-1]
YearlyClimateStats = read.csv("WaterYear_MACAclimateStats_TCSIbasins.csv")[,-1]

# Map of enumerated watersheds
WatershedRast = rast("TCSI_HUC12ish_Watersheds.tif")

# Look-up table of important information
HUCsavePoints = read.csv("TCSI_HUC12savePoints.csv")[,-1]

################################################################################
# Append yearly precip to each water year streamflow record

WaterYears = 2015:2099
ClimateNames = c("CNRM-CM5 RCP 8.5", "MIROC5 RCP 8.5")

WaterYearStreamData$YearlyAvgPrecip_m = NA

for (ClimateName in ClimateNames){
  for (WaterYear in WaterYears){
    
    Ptrs = which(WaterYearStreamData$WaterYear==WaterYear &
                   WaterYearStreamData$Climate==ClimateName)
    
    ClimPtr = which(YearlyClimateStats$WaterYear==WaterYear &
                      YearlyClimateStats$Climate==ClimateName)
    
    WaterYearStreamData$YearlyAvgPrecip_m[Ptrs] = YearlyClimateStats$Prec[ClimPtr]
    
  }
}

TestPtrs = which(WaterYearStreamData$WatershedNum==1 &
                   WaterYearStreamData$Scenario==1 &
                   WaterYearStreamData$Climate=="CNRM-CM5 RCP 8.5" &
                   WaterYearStreamData$DHSVMmodel==276)
plot(WaterYearStreamData$YearlyAvgPrecip_m[TestPtrs], WaterYearStreamData$YearlyAvgQ_mmd[TestPtrs])

################################################################################
# Identify droughts: 10 (?) years with least precipitation in each climate

nDroughtYears = 10

plot(as.factor(YearlyClimateStats$WaterYear), YearlyClimateStats$Temp)
# (Big yikes)

Climates = c("cnrm", "miroc")
ClimateNames = c("CNRM-CM5 RCP 8.5", "MIROC5 RCP 8.5")
LANDISruns = c(1,4)
WaterYears = 2015:2099

DroughtYearData = data.frame(matrix(nrow=nDroughtYears*length(ClimateNames), ncol=3))
names(DroughtYearData) = c("Climate","WaterYear","MeanPrecip_m")
DroughtYearData$Climate = rep(ClimateNames, nDroughtYears)

for (ClimateName in ClimateNames){
  
  ClimatePtrs = which(DroughtYearData$Climate==ClimateName)
  
  # Find n driest values
  DroughtVals = sort(YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName),"Prec"])[1:nDroughtYears]
  DroughtPtrs = match(DroughtVals, YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName),"Prec"])
  
  DroughtYearData$MeanPrecip_m[ClimatePtrs] = YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName),"Prec"][DroughtPtrs]
  
  # Find associated water years
  DroughtYearData$WaterYear[ClimatePtrs] = YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName),"WaterYear"][DroughtPtrs]
  
}

print(DroughtYearData)

hist(DroughtYearData[DroughtYearData$Climate=="CNRM-CM5 RCP 8.5","WaterYear"])
hist(DroughtYearData[DroughtYearData$Climate=="MIROC5 RCP 8.5","WaterYear"])

hist(DroughtYearData[DroughtYearData$Climate=="CNRM-CM5 RCP 8.5","MeanPrecip_m"])
hist(DroughtYearData[DroughtYearData$Climate=="MIROC5 RCP 8.5","MeanPrecip_m"])

################################################################################
# Make maps for the 10 driest vs. all 85 years

# Uncertainty for 3 variables
seq1 = c(rep(c(rep(1, 20),rep(2, 20),rep(3, 20)), 37), rep(1, 20), rep(2, 2247-60*37-20))
seq2 = c(rep(c(rep(2, 20),rep(3, 20),rep(1, 20)), 37), rep(2, 20), rep(3, 2247-60*37-20))
seq3 = c(rep(c(rep(3, 20),rep(1, 20),rep(2, 20)), 37), rep(3, 20), rep(1, 2247-60*37-20))
seq4 = c(rep(c(rep(seq1, 20), rep(seq2, 20), rep(seq3, 20)), 23), rep(seq1, 20), rep(seq2, 20), rep(seq3, (1436-60*23-40)))

hist(WaterYearStreamData$S2RelYearlyLocalAvgQ_pct)
min(WaterYearStreamData$S2RelYearlyLocalAvgQ_pct)

Scenarios = c(6)

for (Climate in Climates){
  ClimateName = ClimateNames[which(Climates==Climate)]
  LANDISrun = LANDISruns[which(Climates==Climate)]
  
  DroughtYears = DroughtYearData$WaterYear[DroughtYearData$Climate==ClimateName]
  
  for (Scenario in Scenarios){
    
    DroughtHUC12map = WatershedRast * NA
    DroughtHUC12map195 = WatershedRast * NA
    DroughtHUC12map270 = WatershedRast * NA
    DroughtHUC12map276 = WatershedRast * NA
    
    FullHUC12map = WatershedRast * NA
    FullHUC12map195 = WatershedRast * NA
    FullHUC12map270 = WatershedRast * NA
    FullHUC12map276 = WatershedRast * NA
    
    for (wshd in unique(HUCsavePoints$Watershed_Num)){
      
      DroughtPtrs = which(WaterYearStreamData$WaterYear %in% DroughtYears &
                            WaterYearStreamData$Scenario==Scenario &
                            WaterYearStreamData$Climate==ClimateName &
                            WaterYearStreamData$WatershedNum==wshd)
      
      DroughtPtrs195 = DroughtPtrs[which(WaterYearStreamData$DHSVMmodel[DroughtPtrs]==195)]
      DroughtPtrs270 = DroughtPtrs[which(WaterYearStreamData$DHSVMmodel[DroughtPtrs]==270)]
      DroughtPtrs276 = DroughtPtrs[which(WaterYearStreamData$DHSVMmodel[DroughtPtrs]==276)]
      
      DroughtHUC12map195[which(WatershedRast[,]==wshd)] = mean(WaterYearStreamData$S2RelYearlyLocalAvgQ_pct[DroughtPtrs195])
      DroughtHUC12map270[which(WatershedRast[,]==wshd)] = mean(WaterYearStreamData$S2RelYearlyLocalAvgQ_pct[DroughtPtrs270])
      DroughtHUC12map276[which(WatershedRast[,]==wshd)] = mean(WaterYearStreamData$S2RelYearlyLocalAvgQ_pct[DroughtPtrs276])
      
      FullPtrs = which(WaterYearStreamData$Scenario==Scenario &
                         WaterYearStreamData$Climate==ClimateName &
                         WaterYearStreamData$WatershedNum==wshd)
      
      FullPtrs195 = FullPtrs[which(WaterYearStreamData$DHSVMmodel[FullPtrs]==195)]
      FullPtrs270 = FullPtrs[which(WaterYearStreamData$DHSVMmodel[FullPtrs]==270)]
      FullPtrs276 = FullPtrs[which(WaterYearStreamData$DHSVMmodel[FullPtrs]==276)]
      
      FullHUC12map195[which(WatershedRast[,]==wshd)] = mean(WaterYearStreamData$S2RelYearlyLocalAvgQ_pct[FullPtrs195])
      FullHUC12map270[which(WatershedRast[,]==wshd)] = mean(WaterYearStreamData$S2RelYearlyLocalAvgQ_pct[FullPtrs270])
      FullHUC12map276[which(WatershedRast[,]==wshd)] = mean(WaterYearStreamData$S2RelYearlyLocalAvgQ_pct[FullPtrs276])
      
      print(wshd)
    }
    
    DroughtHUC12map[which(seq4==1)] = DroughtHUC12map195[which(seq4==1)]
    DroughtHUC12map[which(seq4==2)] = DroughtHUC12map270[which(seq4==2)]
    DroughtHUC12map[which(seq4==3)] = DroughtHUC12map276[which(seq4==3)]
    
    FullHUC12map[which(seq4==1)] = FullHUC12map195[which(seq4==1)]
    FullHUC12map[which(seq4==2)] = FullHUC12map270[which(seq4==2)]
    FullHUC12map[which(seq4==3)] = FullHUC12map276[which(seq4==3)]
    
    pal = colorRampPalette(c("darkred","red3","red2","red","firebrick1","indianred1","lightcoral","lightpink2","lightpink","mistyrose",
                             "white",
                             "lightsteelblue1","skyblue1","deepskyblue1","dodgerblue","dodgerblue3","blue","blue3","navy","midnightblue","black"))
    
    ########## Drought years only
    plot(DroughtHUC12map, range=c(-0.5,0.5),
         col=pal(1000), xlab="Easting", ylab="Northing", cex.lab=1.5, cex.axis=1.5)
    
    ########## Baseline (all years)
    plot(FullHUC12map, range=c(-0.5,0.5),
         col=pal(1000), xlab="Easting", ylab="Northing", cex.lab=1.5, cex.axis=1.5)
    
    ##########
    # Save data
    
    writeRaster(DroughtHUC12map195,paste0("P195_LocalQgeneration_S",Scenario,"vsS2pct_",
                                          Climate,"_Driest",nDroughtYears,"yrs.tif"),overwrite=TRUE)
    writeRaster(DroughtHUC12map270,paste0("P270_LocalQgeneration_S",Scenario,"vsS2pct_",
                                          Climate,"_Driest",nDroughtYears,"yrs.tif"),overwrite=TRUE)
    writeRaster(DroughtHUC12map276,paste0("P276_LocalQgeneration_S",Scenario,"vsS2pct_",
                                          Climate,"_Driest",nDroughtYears,"yrs.tif"),overwrite=TRUE)
    writeRaster(DroughtHUC12map,paste0("LocalQgeneration_S",Scenario,"vsS2pct_",
                                       Climate,"_Driest",nDroughtYears,"yrs.tif"),overwrite=TRUE)
    
    writeRaster(FullHUC12map195,paste0("P195_LocalQgeneration_S",Scenario,"vsS2pct_",
                                       Climate,"_AllYrs.tif"),overwrite=TRUE)
    writeRaster(FullHUC12map270,paste0("P270_LocalQgeneration_S",Scenario,"vsS2pct_",
                                       Climate,"_AllYrs.tif"),overwrite=TRUE)
    writeRaster(FullHUC12map276,paste0("P276_LocalQgeneration_S",Scenario,"vsS2pct_",
                                       Climate,"_AllYrs.tif"),overwrite=TRUE)
    writeRaster(FullHUC12map,paste0("LocalQgeneration_S",Scenario,"vsS2pct_",
                                    Climate,"_AllYrs.tif"),overwrite=TRUE)
  }
}
