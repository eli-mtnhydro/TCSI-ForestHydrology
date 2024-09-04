
library(terra)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Overview_Maps/"
setwd(dir)

Mask = rast("TCSImask_MergedDomain.tif")

################################################################################
# Make maps of landscape-average LAI (tree LAI * tree FC)

##########
# 2015 / year 0

LAImap = project(rast("TCSI_Year0_LAIsummer.tif"),Mask,method="near")
FCmap = rast("TCSI_Year0_CanopyCover.tif")

LAIFCyr0 = LAImap * FCmap
LAIFCyr0[which(LAIFCyr0[,] < 0)] = 0
LAIFCyr0[which(!is.finite(LAIFCyr0[,]))] = 0
LAIFCyr0[which(Mask[,]!=1)] = NA

writeRaster(LAIFCyr0,"LandscapeLAI_Historical.tif",overwrite=TRUE)

##########
# 2099 / year 80

# Scenario 2
LAImapCNRM = rast("Scenario2_CNRM-CM5-RCP8.5_Run1_2094_LAIsummer.tif")
FCmapCNRM = rast("Scenario2_CNRM-CM5-RCP8.5_Run1_2094_CanopyCover.tif")
LAIFCyr80CNRM = LAImapCNRM * abs(FCmapCNRM)

LAImapMIROC = rast("Scenario2_MIROC5-RCP8.5_Run4_2094_LAIsummer.tif")
FCmapMIROC = rast("Scenario2_MIROC5-RCP8.5_Run4_2094_CanopyCover.tif")
LAIFCyr80MIROC = LAImapMIROC * abs(FCmapMIROC)

LAIFCyr80s2 = mean(LAIFCyr80CNRM,LAIFCyr80MIROC)
LAIFCyr80s2[which(LAIFCyr80s2[,] < 0)] = 0
LAIFCyr80s2[which(!is.finite(LAIFCyr80s2[,]))] = 0
LAIFCyr80s2[which(Mask[,]!=1)] = NA

writeRaster(LAIFCyr80s2,"LandscapeLAI_2099_S2climateAvg.tif",overwrite=TRUE)

# Scenario 6
LAImapCNRM = rast("Scenario6_CNRM-CM5-RCP8.5_Run1_2094_LAIsummer.tif")
FCmapCNRM = rast("Scenario6_CNRM-CM5-RCP8.5_Run1_2094_CanopyCover.tif")
LAIFCyr80CNRM = LAImapCNRM * abs(FCmapCNRM)

LAImapMIROC = rast("Scenario6_MIROC5-RCP8.5_Run4_2094_LAIsummer.tif")
FCmapMIROC = rast("Scenario6_MIROC5-RCP8.5_Run4_2094_CanopyCover.tif")
LAIFCyr80MIROC = LAImapMIROC * abs(FCmapMIROC)

LAIFCyr80s6 = mean(LAIFCyr80CNRM,LAIFCyr80MIROC)
LAIFCyr80s6[which(LAIFCyr80s6[,] < 0)] = 0
LAIFCyr80s6[which(!is.finite(LAIFCyr80s6[,]))] = 0
LAIFCyr80s6[which(Mask[,]!=1)] = NA

writeRaster(LAIFCyr80s6,"LandscapeLAI_2099_S6climateAvg.tif",overwrite=TRUE)

################################################################################
# Aggregate difference in final maps by HUC12

HUC12map = rast("TCSI_HUC12ish_Watersheds.tif")
WSHDids = unique(unlist(HUC12map[,]))
WSHDids = sort(WSHDids[is.finite(WSHDids)])

LAIdiffMapS6vS2 = LAIFCyr80s6 - LAIFCyr80s2
LAIdiffMapS6vS2wshds = NA * Mask
LAImeanS2wshds = NA * Mask

for (WSHDid in WSHDids){
  Ptrs = which(HUC12map[,]==WSHDid)
  
  LAIdiffMapS6vS2wshds[Ptrs] = mean(unlist(LAIdiffMapS6vS2[Ptrs]))
  LAImeanS2wshds[Ptrs] = mean(unlist(LAIFCyr80s2[Ptrs]))
  
  print(WSHDid)
}

plot(LAIdiffMapS6vS2wshds)
plot(LAIdiffMapS6vS2wshds / LAImeanS2wshds)

writeRaster(LAIdiffMapS6vS2wshds,"LandscapeLAI_2099_S6-S2climateAvg.tif",overwrite=TRUE)
writeRaster(LAImeanS2wshds,"LandscapeLAI_2099_S2climateAvgWatersheds.tif",overwrite=TRUE)











