# Simple timelapse of April 1 SWE

library(raster)
library(rgdal)
library(magick)

dir = "E:/DHSVM_ScenarioResults"
setwd(dir)

OutputDir = "C:/Users/board/Desktop/DHSVM/TCSI_Results/SWEresults/SWEanimations/"

TahoeMask = raster("C:/Users/board/Desktop/DHSVM/TCSI_Setup/WatershedBoundaries/ModelingWatersheds/LakeTahoeMask.tif")
TahoePtrs = which(TahoeMask[,]==1)

TruckeeWatershed = readOGR("1-TruckeeModelingDomain")
YubaWatershed = readOGR("2-YubaModelingDomain")
BearWatershed = readOGR("3-BearModelingDomain")
AmericanWatershed = readOGR("4-AmericanModelingDomain")

TruckeeCenter = getSpPPolygonsLabptSlots(TruckeeWatershed)
YubaCenter = getSpPPolygonsLabptSlots(YubaWatershed)
BearCenter = getSpPPolygonsLabptSlots(BearWatershed)
AmericanCenter = getSpPPolygonsLabptSlots(AmericanWatershed)

################################################################################
# Basic SWE plots (April 1st)

Years = 2015:2099
DHSVMmodel = 195
Scenario = 1
#Climate = "cnrm"
Climate = "miroc"
#ClimateName = "CNRM-CM5 RCP 8.5"
ClimateName = "MIROC5 RCP 8.5"
LANDISrun = 1
LANDISrun = 4

for (yr in Years){
  SWErast = raster(paste0("SWEmaps/April1_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_",yr,".tif"))
  SWErast[SWErast[,]<=0] = NA
  SWErast[SWErast[,]>3] = 3 # Clip z range
  SWErast[TahoePtrs] = NA
  
  pal = colorRampPalette(c("white","snow3","lightcyan2","skyblue1","deepskyblue1","dodgerblue3","dodgerblue4","blue3","blue4","navyblue","navy","midnightblue","black","black"))
  par(mar=c(5,5,5,5))
  
  plot(SWErast, xlim=c(630000,780000), ylim=c(4270000,4420000), xaxs="i", yaxs="i",
       zlim=c(0,3), col=pal(1000), legend=FALSE,
       main=paste0("April 1 SWE: ", yr),
       xlab="Easting", ylab="Northing", legend.width=2,
       cex.main=3, cex.lab=1.5, cex.axis=1.5)
  
  lines(TruckeeWatershed, lwd=2)
  lines(YubaWatershed, lwd=2)
  lines(BearWatershed, lwd=2)
  lines(AmericanWatershed, lwd=2)
  
  plot(SWErast, zlim=c(0,3), legend.only=TRUE, col=pal(1000),
       legend.width=1, legend.shrink=0.75,
       axis.args=list(at=seq(0, 3, 0.5), labels=seq(0, 3, 0.5), cex.axis=1.5),
       legend.args=list(text="SWE Depth, m", side=2, line=1, cex=3))
  
  text(720000, 4395000, paste0("DHSVM Model: ",DHSVMmodel,
                               "\nClimate: ",ClimateName,
                               "\nScenario: ",Scenario,
                               "\nRun: ",LANDISrun),
       cex=1.25, pos=4)

  text(TruckeeCenter[1]+7000, TruckeeCenter[2]-7000, "Truckee", cex=1.5, font=2)
  text(YubaCenter[1], YubaCenter[2]+2000, "Yuba", cex=1.5, font=2)
  text(BearCenter[1]-2000, BearCenter[2]+2000, "Bear", cex=1.5, font=2)
  text(AmericanCenter[1], AmericanCenter[2]+3000, "American", cex=1.5, font=2)
  
  # Save plots
  dev.print(png, paste0(OutputDir,"April1_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_",yr,".png"),
            width = 1030, height = 878, units = "px", res=96)
}

# Convert to animated GIF
SWEimages = c()
for (yr in Years){
  SWEimages = c(SWEimages, image_read(paste0(OutputDir,"April1_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_",yr,".png")))
}
SWEimages = c(SWEimages, rep(image_read(paste0(OutputDir,"Blank.png")),4))

SWEanimation = image_animate(image_join(SWEimages), fps=2, dispose="background")

image_write(SWEanimation, paste0(OutputDir,"April1_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_Animation.gif"))

################################################################################
# Difference in April 1st SWE: Scenario 6 - Scenario 1

ScenarioA = 1
ScenarioB = 6

Years = 2015:2099
DHSVMmodel = 195
#Climate = "cnrm"
Climate = "miroc"
#ClimateName = "CNRM-CM5 RCP 8.5"
ClimateName = "MIROC5 RCP 8.5"
#LANDISrun = 1
LANDISrun = 4

for (yr in Years){
  SWErastA = raster(paste0("SWEmaps/April1_P",DHSVMmodel,"S",ScenarioA,Climate,"R",LANDISrun,"_",yr,".tif"))
  SWErastB = raster(paste0("SWEmaps/April1_P",DHSVMmodel,"S",ScenarioB,Climate,"R",LANDISrun,"_",yr,".tif"))
  
  SWEdiff = SWErastB - SWErastA
  SWEdiff[SWEdiff[,]<(-3)] = -3 # Clip z range
  SWEdiff[SWEdiff[,]>3] = 3   # Clip z range
  SWEdiff[TahoePtrs] = NA
  
  pal = colorRampPalette(c("darkred","red","firebrick1","lightcoral","lightpink","white","skyblue1","deepskyblue1","dodgerblue3","blue3","navy"))
  par(mar=c(5,5,7,5))
  
  plot(SWEdiff, xlim=c(630000,780000), ylim=c(4270000,4420000), xaxs="i", yaxs="i",
       zlim=c(-0.5,0.5), col=pal(1000), legend=FALSE,
       main=paste0("April 1 SWE: ", yr,"\nScenario ",ScenarioB," - Scenario ",ScenarioA),
       xlab="Easting", ylab="Northing", legend.width=2,
       cex.main=3, cex.lab=1.5, cex.axis=1.5)
  
  lines(TruckeeWatershed, lwd=2)
  lines(YubaWatershed, lwd=2)
  lines(BearWatershed, lwd=2)
  lines(AmericanWatershed, lwd=2)
  
  plot(SWEdiff, zlim=c(-0.5,0.5), legend.only=TRUE, col=pal(1000),
       legend.width=1, legend.shrink=0.75,
       axis.args=list(at=seq(-0.5,0.5,0.1), labels=c(seq(-0.5,0,0.1),paste0("+",seq(0.1,0.5,0.1))), cex.axis=1.5),
       legend.args=list(text="SWE Depth, m", side=2, line=1, cex=3))
  
  text(720000, 4395000, paste0("DHSVM Model: ",DHSVMmodel,
                               "\nClimate: ",ClimateName),
       cex=1.25, pos=4)
  
  text(TruckeeCenter[1]+7000, TruckeeCenter[2]-7000, "Truckee", cex=1.5, font=2)
  text(YubaCenter[1], YubaCenter[2]+2000, "Yuba", cex=1.5, font=2)
  text(BearCenter[1]-2000, BearCenter[2]+2000, "Bear", cex=1.5, font=2)
  text(AmericanCenter[1], AmericanCenter[2]+3000, "American", cex=1.5, font=2)
  
  # Save plots
  dev.print(png, paste0(OutputDir,"April1_P",DHSVMmodel,"S",ScenarioB,"-S",ScenarioA,Climate,"R",LANDISrun,"_",yr,".png"),
            width = 1030, height = 878, units = "px", res=96)
}

# Convert to animated GIF
SWEimages = c()
for (yr in Years){
  SWEimages = c(SWEimages, image_read(paste0(OutputDir,"April1_P",DHSVMmodel,"S",ScenarioB,"-S",ScenarioA,Climate,"R",LANDISrun,"_",yr,".png")))
}
SWEimages = c(SWEimages, rep(image_read(paste0(OutputDir,"Blank.png")),4))

SWEanimation = image_animate(image_join(SWEimages), fps=2, dispose="background")

image_write(SWEanimation, paste0(OutputDir,"April1_P",DHSVMmodel,"S",ScenarioB,"-S",ScenarioA,Climate,"R",LANDISrun,"_Animation.gif"))





