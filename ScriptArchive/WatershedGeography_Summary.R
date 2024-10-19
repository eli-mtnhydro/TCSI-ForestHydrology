
library(terra)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Overview_Maps/"
setwd(dir)

DEM = rast("TCSIdomain_90mDEMfilled.tif")

################################################################################

for (Watershed in c("Truckee","Yuba","Bear","American")){
  print(Watershed)
  
  Mask = rast(paste0("../../../TCSI_Setup/WatershedBoundaries/TCSImask_",Watershed,"Domain.tif"))
  plot(Mask)
  Ptrs = which(Mask[,]==1)
  
  Area = (length(Ptrs) * 90^2) / (1000^2)
  print(paste("Area:",round(Area),"km^2"))
  
  Elev = mean(unlist(DEM[Ptrs]))
  print(paste("Elev:",round(Elev),"m"))
  
  print("***")
}













