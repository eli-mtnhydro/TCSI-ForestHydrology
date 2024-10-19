# Use random forest to investigate streamflow behavior for HUC12s (includes some other watersheds)

library(raster)
library(randomForest)
library(ggplot2)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Results"
setwd(dir)

################################################################################
# Set up data if necessary

# Map of enumerated watersheds
WatershedRast = raster("TCSI_HUC12ish_Watersheds.tif")
plot(WatershedRast)

# Features:
# 1. Watershed Area
# 2. Avg. elevation
# 3. Avg. precip. (historical)
# 4. Avg. LAI change (2095 map - year 0 map)
# 5. Avg. initial LAI (year 0 map)
# 6. Avg. slope
# 7. Climate (categorical factor)
# 8. DHSVM model (categorical factor)

# DEM used in DHSVM
DEM = raster("../TCSI_Setup/SRTM/TCSIdomain_90mDEMfilled.tif")
plot(DEM)

# Historical precip. (gridMet redistributed by PRISM, 1991-2020)
Precip1 = raster("../TCSI_Setup/GridMet/MetSim/GridMet_PRISMredistributed__1991-2020_MeanYearlyPrecip_DHSVM-Truckee.tif")
Precip2 = raster("../TCSI_Setup/GridMet/MetSim/GridMet_PRISMredistributed__1991-2020_MeanYearlyPrecip_DHSVM-Yuba.tif")
Precip3 = raster("../TCSI_Setup/GridMet/MetSim/GridMet_PRISMredistributed__1991-2020_MeanYearlyPrecip_DHSVM-Bear.tif")
Precip4 = raster("../TCSI_Setup/GridMet/MetSim/GridMet_PRISMredistributed__1991-2020_MeanYearlyPrecip_DHSVM-American.tif")
HistoricalPrecip = Precip1
HistoricalPrecip[which(is.finite(Precip2[,]))] = Precip2[which(is.finite(Precip2[,]))]
HistoricalPrecip[which(is.finite(Precip3[,]))] = Precip3[which(is.finite(Precip3[,]))]
HistoricalPrecip[which(is.finite(Precip4[,]))] = Precip4[which(is.finite(Precip4[,]))]
plot(HistoricalPrecip)

# Initial LAI
LAIinitial = raster("../TCSI_Setup/LANDIS/TCSI_Year0_LAIsummer.tif")
LAIinitial[LAIinitial[,] < 0] = 0 # Convert -9999 to 0
plot(LAIinitial)

# LAI change
LAIchangeMaps = list()
i = 1
for (ClimateName in c("CNRM-CM5-RCP8.5_Run1", "MIROC5-RCP8.5_Run4")){
  for (Scenario in 1:7){
    LAIfinalMap = raster(paste0("../TCSI_Setup/LANDIS/Processed_FutureMaps/Scenario",Scenario,
                                "_",ClimateName,"_2094_LAIsummer.tif"))
    LAIfinalMap[LAIfinalMap[,]<0] = 0
    LAIchangeMaps[[i]] = LAIfinalMap-LAIinitial
    i = i+1
  }
}
LAIchangeData = data.frame(Scenario=rep(1:7, 2), Climate=c(rep("cnrm", 7), rep("miroc", 7)))

# Terrain slope
Slope = raster("../TCSI_Setup/SRTM/TCSIdomain_90mDEMfilledSlope.tif")
plot(Slope)

################################################################################
# Aggregate features by watershed

FeatureData = data.frame(WatershedNum=sort(unique(WatershedRast[,])),
                         Elevation=NA, Precip=NA, InitialLAI=NA, Slope=NA,
                         UpstreamArea=NA)

for (wshd in 1:length(FeatureData$WatershedNum)){
  WshdPtrs = which(WatershedRast[,]==unique(FeatureData$WatershedNum)[wshd])
  
  FeatureData$Elevation[wshd] = mean(DEM[WshdPtrs])
  FeatureData$Precip[wshd] = mean(HistoricalPrecip[WshdPtrs])
  FeatureData$InitialLAI[wshd] = mean(LAIinitial[WshdPtrs])
  FeatureData$Slope[wshd] = mean(Slope[WshdPtrs])
  
  print(wshd)
}

################################################################################
# Function to calculate total upstream area

HUCsavePoints = read.csv("TCSI_HUC12savePoints.csv")[,-1]

WatershedArea = function(wshd){
  # Calculate local area
  LocalArea = length(which(WatershedRast[,]==wshd)) * 90^2 # m^2
  
  # Add area of all upstream watersheds recursively
  UpstreamArea = 0
  
  InboundWshds = c(HUCsavePoints$Inbound_1[wshd],
                   HUCsavePoints$Inbound_2[wshd],
                   HUCsavePoints$Inbound_3[wshd],
                   HUCsavePoints$Inbound_4[wshd])
  
  InboundWshds = InboundWshds[is.finite(InboundWshds)]
  
  # Tahoe gets special treatment because it has way too many inbound watersheds
  if (wshd==7){
    InboundWshds = c(8,30,9,10,29,28,11,26,24,12,23,22,21,31)
  }
  
  for (inbound in InboundWshds){
    UpstreamArea = UpstreamArea + WatershedArea(inbound)
  }
  
  TotalArea = LocalArea + UpstreamArea
  return(TotalArea)
}

################################################################################

DHSVMmodels = c(195,270,276)

for (DHSVMmodel in DHSVMmodels){
  
  # Load streamflow data aggregated in 1_ProcessSpatialWatersheds.R
  WatershedData = read.csv(paste0("CombinedLocalWatershedResults_P",DHSVMmodel,".csv"))[,-1]
  head(WatershedData)
  
  # Append appropriate data to each watershed record
  
  WatershedData$Elevation_m = NA
  WatershedData$HistoricPrecip_m = NA
  WatershedData$InitialLAI = NA
  WatershedData$LAIchange = NA
  WatershedData$Slope_deg = NA
  
  for (wshd in unique(WatershedData$WatershedNum)){
    WshdPtrs = which(WatershedData$WatershedNum==wshd)
    FeaturePtr = which(FeatureData$WatershedNum==wshd)
    
    WatershedData$Elevation_m[WshdPtrs] = FeatureData$Elevation[FeaturePtr]
    WatershedData$HistoricPrecip_m[WshdPtrs] = FeatureData$Precip[FeaturePtr]
    WatershedData$InitialLAI[WshdPtrs] = FeatureData$InitialLAI[FeaturePtr]
    WatershedData$Slope_deg[WshdPtrs] = FeatureData$Slope[FeaturePtr]
    
    # Calculate local LAI change in particular scenario / climate
    for (Climate in unique(WatershedData$Climate)){
      for (Scenario in unique(WatershedData$Scenario)){
        
        LAIchangeMapPtr = which(LAIchangeData$Scenario==Scenario & LAIchangeData$Climate==Climate)
        WshdPtr = which(WatershedData$WatershedNum==wshd & WatershedData$Scenario==Scenario & WatershedData$Climate==Climate)
        MapPtrs = which(WatershedRast[,]==wshd)
        
        WatershedData$LAIchange[WshdPtr] = mean(LAIchangeMaps[[LAIchangeMapPtr]][MapPtrs])
      }
    }
    print(wshd)
  }
  
  ##########
  # Total upstream area for each watershed
  WatershedData$UpstreamArea_km2 = NA
  
  # Calculate for each watershed
  for (wshd in unique(WatershedData$WatershedNum)){
    WshdPtrs = which(WatershedData$WatershedNum==wshd)
    FeaturePtr = which(FeatureData$WatershedNum==wshd)
    
    FeatureData$UpstreamArea[FeaturePtr] = WatershedArea(wshd) / (1000^2)
    
    WatershedData$UpstreamArea_km2[WshdPtrs] = FeatureData$UpstreamArea[FeaturePtr]
    
    print(wshd)
  }
  
  ################################################################################
  # Section 1: VOLUMETRIC RUNOFF
  
  # Calculate differences in streamflow between scenarios
  # StreamflowDiff = Streamflow - avg(Streamflow, all scenarios, 1 climate)
  # StreamflowDiff2 = Streamflow - Streamflow [Scenario 2]
  
  WatershedData$RelContribStreamflow_mmd = NA
  WatershedData$S2RelContribStreamflow_mmd = NA
  WatershedData$S2RelContribStreamflow_pct = NA
  
  for (wshd in unique(WatershedData$WatershedNum)){
    for (Climate in unique(WatershedData$Climate)){
      
      Ptrs = which(WatershedData$WatershedNum==wshd & WatershedData$Climate==Climate)
      WshdAvgQ = mean(WatershedData$ContribStreamflow_mmd[Ptrs])
      
      S2ptr = which(WatershedData$WatershedNum==wshd & WatershedData$Climate==Climate & WatershedData$Scenario==2)
      
      for (Scenario in unique(WatershedData$Scenario)){
        
        # Relative to mean
        Ptr = which(WatershedData$WatershedNum==wshd & WatershedData$Climate==Climate & WatershedData$Scenario==Scenario)
        WatershedData$RelContribStreamflow_mmd[Ptr] = WatershedData$ContribStreamflow_mmd[Ptr] - WshdAvgQ
        
        # Relative to Scenario 2
        WatershedData$S2RelContribStreamflow_mmd[Ptr] = WatershedData$ContribStreamflow_mmd[Ptr] - WatershedData$ContribStreamflow_mmd[S2ptr]
        WatershedData$S2RelContribStreamflow_pct[Ptr] = WatershedData$S2RelContribStreamflow_mmd[Ptr] / WatershedData$ContribStreamflow_mmd[S2ptr]
        
      }
    }
  }
  
  ################################################################################
  # Section 2: YEARLY PEAK FLOWS SEN'S SLOPE
  
  # Calculate average Sen's Slope for yearly peak flow in each watershed
  # Consider each scenario and each climate independently
  
  WatershedData$YearlyPeakSensSlope_mmdyr = NA
  
  for (Climate in unique(WatershedData$Climate)){
    
    # LANDIS run number
    if (Climate=="cnrm"){
      LANDISrun = 1
    } else if (Climate=="miroc"){
      LANDISrun = 4
    }
    
    for (Scenario in unique(WatershedData$Scenario)){
      
      PeakFlowSensSlopeRast = raster(paste0("StreamflowRasters/Yearly1-dayPeakSensSlope_mmd_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,".tif"))
      plot(PeakFlowSensSlopeRast, main=paste0("Yearly Peak Flow Sen's Slope\n",Climate," ",Scenario))
      
      for (wshd in unique(WatershedData$WatershedNum)){
        
        MapPtrs = which(WatershedRast[,]==wshd)
        
        Ptr = which(WatershedData$WatershedNum==wshd & WatershedData$Climate==Climate & WatershedData$Scenario==Scenario)
        WatershedData$YearlyPeakSensSlope_mmdyr[Ptr] = mean(PeakFlowSensSlopeRast[MapPtrs])

        print(wshd)
      }
    }
  }
  
  # Calculate differences in Sen's Slope between scenarios
  # PeakFlowSlopeDiff = Sen's Slope - avg(Sen's Slope, all scenarios, 1 climate)
  # PeakFlowSlopeDiff2 = Sen's Slope - Sen's Slope [Scenario 2]
  
  WatershedData$RelYearlyPeakSensSlope_mmdyr = NA
  WatershedData$S2RelYearlyPeakSensSlope_mmdyr = NA
  WatershedData$S2RelYearlyPeakSensSlope_pct = NA
  
  for (wshd in unique(WatershedData$WatershedNum)){
    for (Climate in unique(WatershedData$Climate)){
      
      Ptrs = which(WatershedData$WatershedNum==wshd & WatershedData$Climate==Climate)
      WshdAvgSlope = mean(WatershedData$YearlyPeakSensSlope_mmdyr[Ptrs])
      
      S2ptr = which(WatershedData$WatershedNum==wshd & WatershedData$Climate==Climate & WatershedData$Scenario==2)
      
      for (Scenario in unique(WatershedData$Scenario)){
        
        # Relative to mean
        Ptr = which(WatershedData$WatershedNum==wshd & WatershedData$Climate==Climate & WatershedData$Scenario==Scenario)
        WatershedData$RelYearlyPeakSensSlope_mmdyr[Ptr] = WatershedData$YearlyPeakSensSlope_mmdyr[Ptr] - WshdAvgSlope
        
        # Relative to Scenario 2
        WatershedData$S2RelYearlyPeakSensSlope_mmdyr[Ptr] = WatershedData$YearlyPeakSensSlope_mmdyr[Ptr] - WatershedData$YearlyPeakSensSlope_mmdyr[S2ptr]
        WatershedData$S2RelYearlyPeakSensSlope_pct[Ptr] = WatershedData$S2RelYearlyPeakSensSlope_mmdyr[Ptr] / WatershedData$YearlyPeakSensSlope_mmdyr[S2ptr]
        
      }
    }
  }
  
  ################################################################################
  # Reorganize and merge
  
  WatershedData = data.frame(WatershedNum=WatershedData$WatershedNum,
                             Area_km2=WatershedData$Area_km2,
                             UpstreamArea_km2=WatershedData$UpstreamArea_km2,
                             Elevation_m=WatershedData$Elevation_m,
                             Slope_deg=WatershedData$Slope_deg,
                             HistoricPrecip_m=WatershedData$HistoricPrecip_m,
                             InitialLAI=WatershedData$InitialLAI,
                             LAIchange=WatershedData$LAIchange,
                             Climate=WatershedData$Climate,
                             Scenario=WatershedData$Scenario,
                             DHSVMmodel=WatershedData$DHSVMmodel,
                             AvgStreamflow_cms=WatershedData$AvgStreamflow_cms,
                             ContribStreamflow_cms=WatershedData$ContribStreamflow_cms,
                             ContribStreamflow_mmd=WatershedData$ContribStreamflow_mmd,
                             RelContribStreamflow_mmd=WatershedData$RelContribStreamflow_mmd,
                             S2RelContribStreamflow_mmd=WatershedData$S2RelContribStreamflow_mmd,
                             S2RelContribStreamflow_pct=WatershedData$S2RelContribStreamflow_pct,
                             YearlyPeakSensSlope_mmdyr=WatershedData$YearlyPeakSensSlope_mmdyr,
                             RelYearlyPeakSensSlope_mmdyr=WatershedData$RelYearlyPeakSensSlope_mmdyr,
                             S2RelYearlyPeakSensSlope_mmdyr=WatershedData$S2RelYearlyPeakSensSlope_mmdyr,
                             S2RelYearlyPeakSensSlope_pct=WatershedData$S2RelYearlyPeakSensSlope_pct)
  
  if (!file.exists("CombinedLocalWatershedResults_WithAttributes.csv")){
    write.csv(WatershedData, "CombinedLocalWatershedResults_WithAttributes.csv")
  } else {
    ExistingData = read.csv("CombinedLocalWatershedResults_WithAttributes.csv")[,-1]
    
    if (all((names(ExistingData) == names(WatershedData)))){
      WatershedData = rbind(WatershedData, ExistingData)
      
      write.csv(WatershedData, "CombinedLocalWatershedResults_WithAttributes.csv")
    } else {
      print("Error with combining old and new data, nothing new saved!")
    }
  }
  
} # End of mega-loop over DHSVM models

################################################################################
################################################################################
################################################################################
# Fit and analyze random forest: volumetric runoff
################################################################################
################################################################################
################################################################################

# Can start here
WatershedData = read.csv("CombinedLocalWatershedResults_WithAttributes.csv")[,-1]
head(WatershedData)

WatershedData.RF = data.frame(RelStreamflow=WatershedData$RelContribStreamflow_mmd,
                              Area=WatershedData$Area_km2,
                              Elevation=WatershedData$Elevation_m,
                              Slope=WatershedData$Slope_deg,
                              HistoricPrecip=WatershedData$HistoricPrecip_m,
                              InitialLAI=WatershedData$InitialLAI,
                              LAIchange=WatershedData$LAIchange,
                              Climate=factor(WatershedData$Climate, levels=unique(WatershedData$Climate)),
                              Model=factor(WatershedData$DHSVMmodel, levels=unique(WatershedData$DHSVMmodel)))
head(WatershedData.RF)

plot(WatershedData.RF$LAIchange, WatershedData.RF$RelStreamflow)
plot(WatershedData.RF$Elevation, WatershedData.RF$RelStreamflow)
plot(WatershedData.RF$Model, WatershedData.RF$RelStreamflow)

set.seed(42)
mtry = tuneRF(WatershedData.RF[-1], WatershedData.RF$RelStreamflow, ntreeTry=1000,
              trace=TRUE, plot=TRUE)
bestM = mtry[mtry[,2] == min(mtry[,2]), 1]

set.seed(42)
rfModel = randomForest(RelStreamflow ~ ., data=WatershedData.RF,
                       mtry=bestM, ntree=1000, importance=TRUE)

print(rfModel)
importance(rfModel)
varImpPlot(rfModel)

par(mar=c(7,2,5,2))
varImpPlot(rfModel, type=1, # % increase in mean squared error
           labels=c("DHSVM Model",
                    "Catchment Area",
                    "Avg. Elevation",
                    "Avg. Slope",
                    "Historic Precip.",
                    "Climate (GCM)",
                    "Initial LAI",
                    "LAI Change"),
           main="Random Forest Variable Importance\nStreamflow Generation in Each Scenario, Difference from Mean",
           pch=19, pt.cex=2, lcolor="black", cex=1.5)


################################################################################
################################################################################
################################################################################
# Fit and analyze random forest: peak flows
################################################################################
################################################################################
################################################################################

WatershedData.RF = data.frame(RelPeakSlope=WatershedData$RelYearlyPeakSensSlope_mmdyr,
                              UpstreamArea=WatershedData$UpstreamArea_km2,
                              Area=WatershedData$Area_km2,
                              Elevation=WatershedData$Elevation_m,
                              Slope=WatershedData$Slope_deg,
                              HistoricPrecip=WatershedData$HistoricPrecip_m,
                              InitialLAI=WatershedData$InitialLAI,
                              LAIchange=WatershedData$LAIchange,
                              Climate=factor(WatershedData$Climate, levels=unique(WatershedData$Climate)),
                              Model=factor(WatershedData$DHSVMmodel, levels=unique(WatershedData$DHSVMmodel)))
head(WatershedData.RF)

plot(WatershedData.RF$Model, WatershedData.RF$RelPeakSlope)
plot(WatershedData.RF$HistoricPrecip, WatershedData.RF$RelPeakSlope)
plot(WatershedData.RF$UpstreamArea, WatershedData.RF$RelPeakSlope)
plot(WatershedData.RF$Climate, WatershedData.RF$RelPeakSlope)

set.seed(42)
mtry = tuneRF(WatershedData.RF[-1], WatershedData.RF$RelPeakSlope, ntreeTry=1000,
              trace=TRUE, plot=TRUE)
bestM = mtry[mtry[,2] == min(mtry[,2]), 1]

set.seed(42)
rfModel = randomForest(RelPeakSlope ~ ., data=WatershedData.RF,
                       mtry=bestM, ntree=1000, importance=TRUE)

print(rfModel)
importance(rfModel)
varImpPlot(rfModel)

par(mar=c(7,2,5,2))
varImpPlot(rfModel, type=1, # % increase in mean squared error
           labels=c("DHSVM Model",
                    "Catchment Area",
                    "Upstream Area",
                    "Avg. Elevation",
                    "Historic Precip.",
                    "Avg. Slope",
                    "Climate (GCM)",
                    "Initial LAI",
                    "LAI Change"),
           main="Random Forest Variable Importance\nSen's Slope of Yearly Peak Flows in Each Scenario, Difference from Mean",
           pch=19, pt.cex=2, lcolor="black", cex=1.5)

################################################################################
################################################################################
################################################################################
# Relationship between water yield change and peak flow change
################################################################################
################################################################################
################################################################################

# Can start here
WatershedData = read.csv("CombinedLocalWatershedResults_WithAttributes.csv")[,-1]
head(WatershedData)

plot(WatershedData$RelContribStreamflow_mmd, WatershedData$RelYearlyPeakSensSlope_mmdyr)
plot(WatershedData$Scenario, WatershedData$RelContribStreamflow_mmd)
plot(WatershedData$Scenario, WatershedData$RelYearlyPeakSensSlope_mmdyr)

cor(WatershedData$RelContribStreamflow_mmd, WatershedData$RelYearlyPeakSensSlope_mmdyr)

cor(WatershedData[WatershedData$DHSVMmodel==276,]$RelContribStreamflow_mmd,
    WatershedData[WatershedData$DHSVMmodel==276,]$RelYearlyPeakSensSlope_mmdyr)

cor(WatershedData[WatershedData$DHSVMmodel==270,]$RelContribStreamflow_mmd,
    WatershedData[WatershedData$DHSVMmodel==270,]$RelYearlyPeakSensSlope_mmdyr)

cor(WatershedData[WatershedData$DHSVMmodel==195,]$RelContribStreamflow_mmd,
    WatershedData[WatershedData$DHSVMmodel==195,]$RelYearlyPeakSensSlope_mmdyr)

# Change climate name
WatershedData$ClimateName = WatershedData$Climate
WatershedData$ClimateName = sub("cnrm", "CNRM-CM5 RCP 8.5", WatershedData$ClimateName)
WatershedData$ClimateName = sub("miroc", "MIROC5 RCP 8.5", WatershedData$ClimateName)

# Change model name and change to factor
WatershedData$Model = WatershedData$DHSVMmodel
WatershedData$Model = factor(WatershedData$Model, levels=sort(unique(WatershedData$Model)))

# Change scenario to factor
WatershedData$Scenario = factor(WatershedData$Scenario, levels=sort(unique(WatershedData$Scenario)))

# Change model name and change to factor
WatershedData$ModelName = paste0("DHSVM Model ",WatershedData$DHSVMmodel)
WatershedData$ModelName = factor(WatershedData$ModelName, levels=unique(WatershedData$ModelName))

# Add watershed x scenario column
WatershedData$WatershedScenario = paste0(WatershedData$WatershedNum,WatershedData$Scenario)

ggplot(data=WatershedData, aes(x=RelContribStreamflow_mmd, y=RelYearlyPeakSensSlope_mmdyr)) + 
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  geom_smooth(method="lm", formula=y~x, show.legend=FALSE, color="gray50") +
  geom_point(size=1, stroke=1, aes(color=Scenario, shape=Model)) +
  scale_shape_manual(values=c(0, 1, 2)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple","magenta")) +
  labs(x="Δ Streamflow Generation, mm / d", y="Δ Peak Flow, mm / d / yr",
       title="Water Yield vs. Flood Risk Tradeoff") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color="black"),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_text(size=20, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=20, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right") +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  guides(color=guide_legend(override.aes=list(size=6),byrow=TRUE),
         shape=guide_legend(override.aes=list(size=4, stroke=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName))

# Individual models

ggplot(data=WatershedData, aes(x=RelContribStreamflow_mmd, y=RelYearlyPeakSensSlope_mmdyr)) + 
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  geom_smooth(method="lm", formula=y~x, show.legend=FALSE, color="gray50") +
  geom_point(size=1.5, aes(color=Scenario)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple","magenta")) +
  labs(x="Δ Streamflow Generation, mm / d", y="Δ Peak Flow, mm / d / yr",
       title="Water Yield vs. Flood Risk Tradeoff") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color="black"),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_text(size=20, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=20, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right") +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  guides(color=guide_legend(override.aes=list(size=6),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(ModelName))

g1 = ggplot(data=WatershedData, aes(x=RelContribStreamflow_mmd, y=RelYearlyPeakSensSlope_mmdyr)) + 
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  geom_polygon(aes(group=WatershedScenario, color=Scenario), fill=NA, linewidth=0.25, show.legend=FALSE) +
  geom_point(size=1, stroke=0.5, aes(color=Scenario, shape=Model)) +
  geom_smooth(method="lm", formula=y~x, show.legend=FALSE, color="gray50") +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple","magenta")) +
  scale_shape_manual(values=c(0, 1, 2)) +
  scale_x_continuous(breaks=c(-0.2,-0.1,0,0.1,0.2,0.3),
                     labels=c("-0.2","-0.1","Mean","+0.1","+0.2","+0.3")) +
  scale_y_continuous(breaks=c(-0.08,-0.06,-0.04,-0.02,0,0.02,0.04),
                     labels=c("-0.08","-0.06","-0.04","-0.02","Mean","+0.02","+0.04")) +
  labs(x="Δ Streamflow Generation, mm / d", y="Δ Peak Flow, mm / d / yr",
       title="Water Yield vs. Flood Risk Tradeoff") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color="black"),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_text(size=20, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=20, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(size=6),byrow=TRUE, order=1),
         shape=guide_legend(override.aes=list(size=4, stroke=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName))

print(g1)

ggsave("WaterYieldVsPeakFlows.png", plot=g1, dpi=300)
ggsave("WaterYieldVsPeakFlowsLarge.png", plot=g1, dpi=900)

################################################################################
# More intuitive plot labels etc.

# Subset to fewer scenarios
SelectedScenarios = 1:6
WatershedDataSubset = WatershedData[(WatershedData$Scenario %in% SelectedScenarios),]

# Change model names and change to factor
WatershedDataSubset$Model = paste0(WatershedDataSubset$DHSVMmodel)
WatershedDataSubset$Model = sub("195","Shallow Soil",WatershedDataSubset$Model)
WatershedDataSubset$Model = sub("270","Deep Soil",WatershedDataSubset$Model)
WatershedDataSubset$Model = sub("276","Medium Soil",WatershedDataSubset$Model)
WatershedDataSubset$Model = factor(WatershedDataSubset$Model, levels=c("Shallow Soil","Medium Soil","Deep Soil"))

# Change scenario names and change to factor
WatershedDataSubset$Scenario = paste0(WatershedDataSubset$Scenario)
WatershedDataSubset$Scenario = sub("1","Business as Usual",WatershedDataSubset$Scenario)
WatershedDataSubset$Scenario = sub("2","Minor Thinning",WatershedDataSubset$Scenario)
WatershedDataSubset$Scenario = sub("3","Medium Thinning",WatershedDataSubset$Scenario)
WatershedDataSubset$Scenario = sub("4","Medium + Add Fire",WatershedDataSubset$Scenario)
WatershedDataSubset$Scenario = sub("5","Intensive Thinning",WatershedDataSubset$Scenario)
WatershedDataSubset$Scenario = sub("6","Intensive + Add Fire",WatershedDataSubset$Scenario)
WatershedDataSubset$Scenario = factor(WatershedDataSubset$Scenario, levels=c("Business as Usual",
                                                                             "Minor Thinning",
                                                                             "Medium Thinning",
                                                                             "Medium + Add Fire",
                                                                             "Intensive Thinning",
                                                                             "Intensive + Add Fire"))

g1 = ggplot(data=WatershedDataSubset, aes(x=RelContribStreamflow_mmd, y=RelYearlyPeakSensSlope_mmdyr)) + 
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  geom_polygon(aes(group=WatershedScenario, color=Scenario), fill=NA, linewidth=0.5, show.legend=FALSE) +
  geom_point(size=1.5, stroke=0.5, aes(color=Scenario, shape=Model)) +
  geom_smooth(method="lm", formula=y~x, show.legend=FALSE, color="gray50") +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_shape_manual(values=c(0, 1, 2)) +
  scale_x_continuous(breaks=c(-0.2,-0.1,0,0.1,0.2,0.3),
                     labels=c("-0.2","-0.1","Mean","+0.1","+0.2","+0.3")) +
  scale_y_continuous(breaks=c(-0.04,-0.02,0,0.02,0.04),
                     labels=c("-0.04","-0.02","Mean","+0.02","+0.04")) +
  labs(x="Rel. Avg. Streamflow, mm / d", y="Rel. Peak Flow Slope, mm / d / yr",
       title="Water Yield vs. Flood Risk Tradeoff") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color="black"),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_text(size=20, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=20, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(size=6),byrow=TRUE, order=1),
         shape=guide_legend(override.aes=list(size=4, stroke=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName))

print(g1)

ggsave("WaterYieldVsPeakFlows.png", plot=g1, dpi=300)
ggsave("WaterYieldVsPeakFlowsLarge.png", plot=g1, dpi=900)


################################################################################
# Alternative basis: relative to Scenario 2 instead of relative to mean

g2 = ggplot(data=WatershedData, aes(x=S2RelContribStreamflow_mmd, y=S2RelYearlyPeakSensSlope_mmdyr)) + 
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  geom_polygon(aes(group=WatershedScenario, color=Scenario), fill=NA, linewidth=0.25, show.legend=FALSE) +
  geom_point(size=1, stroke=0.5, aes(color=Scenario, shape=Model)) +
  geom_smooth(method="lm", formula=y~x, show.legend=FALSE, color="gray50") +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple","magenta")) +
  scale_shape_manual(values=c(0, 1, 2)) +
  scale_x_continuous(breaks=c(-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5),
                     labels=c("-0.1","-0.2","Scenario 2","+0.1","+0.2","+0.3","+0.4","+0.5")) +
  scale_y_continuous(breaks=c(-0.075,-0.05,-0.025,0,0.025,0.05,0.075),
                     labels=c("-0.075","-0.05","-0.025","Scenario 2","+0.025","+0.05","+0.075")) +
  labs(x="Δ Streamflow Generation, mm / d", y="Δ Peak Flow, mm / d / yr",
       title="Water Yield vs. Flood Risk Tradeoff") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color="black"),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_text(size=20, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=20, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(size=6),byrow=TRUE, order=1),
         shape=guide_legend(override.aes=list(size=4, stroke=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName))

print(g2)

ggsave("WaterYieldVsPeakFlows.png", plot=g2, dpi=300)
ggsave("WaterYieldVsPeakFlowsLarge.png", plot=g2, dpi=900)

























