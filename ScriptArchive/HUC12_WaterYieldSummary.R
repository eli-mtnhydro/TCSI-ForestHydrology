
library(terra)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Streamflow_Generation_Maps/"
setwd(dir)

Mask = rast("../../../TCSI_Setup/WatershedBoundaries/TCSImask_MergedDomain.tif")

################################################################################
# Total streamflow generation

WatershedData = read.csv("../Tradeoff_Figure/CombinedLocalWatershedResults_WithAttributes.csv")

head(WatershedData)

TotalQs2cnrm = sum(WatershedData$ContribStreamflow_cms[WatershedData$Climate=="cnrm" &
                                                         WatershedData$Scenario==2])
TotalQs2miroc = sum(WatershedData$ContribStreamflow_cms[WatershedData$Climate=="miroc" &
                                                         WatershedData$Scenario==2])
TotalQs6cnrm = sum(WatershedData$ContribStreamflow_cms[WatershedData$Climate=="cnrm" &
                                                         WatershedData$Scenario==6])
TotalQs6miroc = sum(WatershedData$ContribStreamflow_cms[WatershedData$Climate=="miroc" &
                                                         WatershedData$Scenario==6])

(TotalQs6cnrm - TotalQs2cnrm) / TotalQs2cnrm # 4.3%
(TotalQs6miroc - TotalQs2miroc) / TotalQs2miroc # 5.7%

################################################################################
# All years

QpctCNRM = mean(rast("P195_LocalQgeneration_S6vsS2pct_cnrm_AllYrs.tif"),
                rast("P270_LocalQgeneration_S6vsS2pct_cnrm_AllYrs.tif"),
                rast("P276_LocalQgeneration_S6vsS2pct_cnrm_AllYrs.tif"))

QpctMIROC = mean(rast("P195_LocalQgeneration_S6vsS2pct_miroc_AllYrs.tif"),
                rast("P270_LocalQgeneration_S6vsS2pct_miroc_AllYrs.tif"),
                rast("P276_LocalQgeneration_S6vsS2pct_miroc_AllYrs.tif"))

plot(QpctCNRM)

sort(unique(unlist(QpctCNRM[which(Mask[,]==1)]))) * 100
sort(unique(unlist(QpctMIROC[which(Mask[,]==1)]))) * 100

################################################################################
# Driest years

QpctCNRM = mean(rast("P195_LocalQgeneration_S6vsS2pct_cnrm_Driest10yrs.tif"),
                rast("P270_LocalQgeneration_S6vsS2pct_cnrm_Driest10yrs.tif"),
                rast("P276_LocalQgeneration_S6vsS2pct_cnrm_Driest10yrs.tif"))

QpctMIROC = mean(rast("P195_LocalQgeneration_S6vsS2pct_miroc_Driest10yrs.tif"),
                 rast("P270_LocalQgeneration_S6vsS2pct_miroc_Driest10yrs.tif"),
                 rast("P276_LocalQgeneration_S6vsS2pct_miroc_Driest10yrs.tif"))

plot(QpctCNRM)

sort(unique(unlist(QpctCNRM[which(Mask[,]==1)]))) * 100
sort(unique(unlist(QpctMIROC[which(Mask[,]==1)]))) * 100

quantile(unique(unlist(QpctCNRM[which(Mask[,]==1)])),c(0.01,0.1,0.25,0.5,0.75,0.9,0.99),na.rm=TRUE) * 100
quantile(unique(unlist(QpctMIROC[which(Mask[,]==1)])),c(0.01,0.1,0.25,0.5,0.75,0.9,0.99),na.rm=TRUE) * 100

################################################################################
# Correlations

WSHDrast = rast("TCSI_HUC12ish_Watersheds.tif")
WSHDids = as.vector(unique(WSHDrast[,]))
WSHDids = sort(WSHDids[is.finite(WSHDids)])

PrecipRast = rast("../Overview_Maps/TCSI-DHSVM_GridMet_PRISMredistributed_1991-2020_MeanYearlyPrecip.tif")

DEM = rast("../Overview_Maps/TCSIdomain_90mDEMfilled.tif")

QpctCNRM = mean(rast("P195_LocalQgeneration_S6vsS2pct_cnrm_AllYrs.tif"),
                rast("P270_LocalQgeneration_S6vsS2pct_cnrm_AllYrs.tif"),
                rast("P276_LocalQgeneration_S6vsS2pct_cnrm_AllYrs.tif"))

QpctMIROC = mean(rast("P195_LocalQgeneration_S6vsS2pct_miroc_AllYrs.tif"),
                 rast("P270_LocalQgeneration_S6vsS2pct_miroc_AllYrs.tif"),
                 rast("P276_LocalQgeneration_S6vsS2pct_miroc_AllYrs.tif"))

LAIdiff = rast("../Overview_Maps/LandscapeLAI_2099_S6-S2climateAvg.tif")

LAIinitial = rast("../Overview_Maps/TCSI_Year0_LAIsummer.tif")
LAIinitial = project(LAIinitial,WSHDrast,method="near")
LAIinitial[which(!is.finite(LAIinitial[,]))] = 0

plot(LAIdiff)
plot(QpctCNRM)
plot(LAIinitial)

QdiffData = data.frame(WSHDid=WSHDids,
                       QpctCNRM=NA,QpctMIROC=NA,
                       QmmdCNRM=NA,QmmdMIROC=NA,
                       LAIdiff=NA,
                       LAIinitial=NA,
                       Precip=NA,
                       Elev=NA)

for (i in WSHDids){
  Ptrs = which(WSHDrast[,]==i)
  
  QdiffData$QmmdCNRM[i] = mean(WatershedData$S2RelContribStreamflow_mmd[WatershedData$Scenario==6 &
                                                                          WatershedData$Climate=="cnrm" &
                                                                          WatershedData$WatershedNum==i])
  QdiffData$QmmdMIROC[i] = mean(WatershedData$S2RelContribStreamflow_mmd[WatershedData$Scenario==6 &
                                                                           WatershedData$Climate=="miroc" &
                                                                           WatershedData$WatershedNum==i])
  
  QdiffData$QpctCNRM[i] = mean(unlist(QpctCNRM[Ptrs]))
  QdiffData$QpctMIROC[i] = mean(unlist(QpctMIROC[Ptrs]))
  QdiffData$LAIdiff[i] = mean(unlist(LAIdiff[Ptrs]))
  QdiffData$LAIinitial[i] = mean(unlist(LAIinitial[Ptrs]))
  QdiffData$Precip[i] = mean(unlist(PrecipRast[Ptrs]))
  QdiffData$Elev[i] = mean(unlist(DEM[Ptrs]))
  
  print(i)
}

plot(QdiffData$QmmdCNRM,QdiffData$QmmdMIROC)

QdiffData$LAIdiff = -QdiffData$LAIdiff

# LAI initial
plot(c(QdiffData$LAIinitial,QdiffData$LAIinitial),
     c(QdiffData$QmmdCNRM,QdiffData$QmmdMIROC))
cor(c(QdiffData$LAIinitial,QdiffData$LAIinitial),
    c(QdiffData$QmmdCNRM,QdiffData$QmmdMIROC))

# LAI difference
plot(c(QdiffData$LAIdiff,QdiffData$LAIdiff),
     c(QdiffData$QmmdCNRM,QdiffData$QmmdMIROC))
cor(c(QdiffData$LAIdiff,QdiffData$LAIdiff),
    c(QdiffData$QmmdCNRM,QdiffData$QmmdMIROC))

# Precip
plot(c(QdiffData$Precip,QdiffData$Precip),
     c(QdiffData$QmmdCNRM,QdiffData$QmmdMIROC))
cor(c(QdiffData$Precip,QdiffData$Precip),
    c(QdiffData$QmmdCNRM,QdiffData$QmmdMIROC))

# Elevation
plot(c(QdiffData$Elev,QdiffData$Elev),
     c(QdiffData$QmmdCNRM,QdiffData$QmmdMIROC))
cor(c(QdiffData$Elev,QdiffData$Elev),
    c(QdiffData$QmmdCNRM,QdiffData$QmmdMIROC))

# # LAI initial
# plot(c(QdiffData$LAIinitial,QdiffData$LAIinitial),
#      c(QdiffData$QpctCNRM,QdiffData$QpctMIROC))
# cor(c(QdiffData$LAIinitial,QdiffData$LAIinitial),
#     c(QdiffData$QpctCNRM,QdiffData$QpctMIROC))
# 
# # LAI difference
# plot(c(QdiffData$LAIdiff,QdiffData$LAIdiff),
#      c(QdiffData$QpctCNRM,QdiffData$QpctMIROC))
# cor(c(QdiffData$LAIdiff,QdiffData$LAIdiff),
#     c(QdiffData$QpctCNRM,QdiffData$QpctMIROC))
# 
# # Precip
# plot(c(QdiffData$Precip,QdiffData$Precip),
#      c(QdiffData$QpctCNRM,QdiffData$QpctMIROC))
# cor(c(QdiffData$Precip,QdiffData$Precip),
#     c(QdiffData$QpctCNRM,QdiffData$QpctMIROC))
# 
# # Elevation
# plot(c(QdiffData$Elev,QdiffData$Elev),
#      c(QdiffData$QpctCNRM,QdiffData$QpctMIROC))
# cor(c(QdiffData$Elev,QdiffData$Elev),
#     c(QdiffData$QpctCNRM,QdiffData$QpctMIROC))








