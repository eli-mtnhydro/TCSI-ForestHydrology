
library(terra)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Overview_Maps//"
setwd(dir)

################################################################################
# Make maps of understory LAI

##########
# 2015 / year 0

VegIDmap = rast("")

UnderstoryLAI = NA * VegIDmap

UnderstoryLAI[which(VegIDmap[,] < 29 & !(VegIDmap[,] %% 2))] = 3
UnderstoryLAI[which(VegIDmap[,] < 29 & (VegIDmap[,] %% 2))] = 1
UnderstoryLAI[which(VegIDmap[,] %in% c(29,32))] = 0
UnderstoryLAI[which(VegIDmap[,] %in% c(30,31,35))] = 1
UnderstoryLAI[which(VegIDmap[,] %in% c(33,34))] = 3

writeRaster(UnderstoryLAI,"")

UnderstoryLAI2 = mean(rast("UnderstoryLAI_2094_S2cnrm.tif"),
                      rast("UnderstoryLAI_2094_S2miroc.tif"))

UnderstoryLAI6 = mean(rast("UnderstoryLAI_2094_S6cnrm.tif"),
                      rast("UnderstoryLAI_2094_S6miroc.tif"))

plot(UnderstoryLAI6 - UnderstoryLAI2)

writeRaster(UnderstoryLAI2,"UnderstoryLAI_2094_S2_ClimateAverage.tif")
writeRaster(UnderstoryLAI6,"UnderstoryLAI_2094_S6_ClimateAverage.tif")

writeRaster(UnderstoryLAI2 - UnderstoryLAI6,"UnderstoryLAI_2094_S2-S6_ClimateAverage.tif")

################################################################################
# Aggregate by HUC12

WshdRast = rast("TCSI_HUC12ish_Watersheds.tif")

LAIchange = rast("UnderstoryLAI_2094_S2-S6_ClimateAverage.tif")

WshdIDs = sort(as.vector(unlist(unique(WshdRast[which(is.finite(WshdRast[,]))]))))

HUC12map = NA * WshdRast

for (i in WshdIDs){
  Ptrs = which(WshdRast[,]==i)
  HUC12map[Ptrs] = mean(unlist(LAIchange[Ptrs]))
  print(i)
}

writeRaster(HUC12map,"UnderstoryLAI_2094_S2-S6_ClimateWhsdAverage.tif",overwrite=TRUE)








