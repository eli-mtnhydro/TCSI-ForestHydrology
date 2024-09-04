# https://www.nature.com/articles/s41467-018-05743-y

library(raster)
library(rgdal)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/SoilDepth/CurvatureBased/"
setwd(dir)

DEM = raster("../../SRTM/TCSIdomain_90mDEMfilled.tif")
VegClass = raster("../../LANDIS/TCSI_Year0_VegID.tif")

# To compare histograms of chosen soil maps within North Yuba watershed
BasinShape = readOGR(dsn="NYubaShape")

################################################################################

# Create map of minimum soil depth based on vegetation rooting depth

# Must have constant minimum depth for LANDIS-active regions to enable switching between classes
MinVegDepth = 1.2 # 20 + 40 + 60 cm
MinRockDepth = 0.03 # 1 cm per layer
MinShrubDepth = 0.6 # 20 cm per layer
MinAlpineDepth = 0.3 # 10 cm per layer

Class = 1:35
Depth = c(rep(MinVegDepth,28), MinRockDepth, MinVegDepth, MinShrubDepth, MinRockDepth, MinShrubDepth, MinShrubDepth, MinAlpineDepth)
VegRoots = data.frame(Class,Depth)

MinSoilDepth = NA*VegClass
for (i in 1:length(VegRoots$Class)){
  MinSoilDepth[VegClass[,]==VegRoots$Class[i]] = VegRoots$Depth[i]
}

plot(MinSoilDepth)

writeRaster(MinSoilDepth,"MinSoilDepth.tif",overwrite=TRUE)

################################################################################

# Examine parameters from Nature paper Table 2
DEMres = c(1,3,5,10,20,30,50)
CurveSD = c(0.126,0.036,0.021,0.018,0.011,0.007,0.006)
FittedSlope = c(0.41,22.8,21.58,20.56,24.76,37.90,59.73)
FittedIntercept = c(1.05,1.01,1.04,1.00,0.98,0.96,0.95)
R2 = c(0.02,0.86,0.44,0.30,0.21,0.25,0.23)

plot(DEMres,R2,type="l")
plot(CurveSD,FittedSlope,type="l")
plot(DEMres,FittedSlope,type="l")
plot(DEMres,FittedIntercept,type="l")

SlopeVsDEMres = lm(FittedSlope~DEMres,data=data.frame(FittedSlope,DEMres))
print(summary(SlopeVsDEMres)$adj.r.squared)
PredSlopeDEMres = as.vector(predict(SlopeVsDEMres,newdata=data.frame(DEMres=90)))
print(PredSlopeDEMres)

plot(DEMres,FittedSlope,pch=16,cex=1.5,main="Slope of Curvature-Soil Depth Relationship\nFitted From Patton et al. (2018) Data",xlab="Resolution of DEM (m)",ylab="Fitted Slope",xlim=c(0,90),ylim=c(0,100),xaxp=c(0,90,9),col="red",cex.axis=1.2,cex.lab=1.2)
abline(SlopeVsDEMres,lwd=2)
text(20,80,expression("R"^2*" = "*0.83),cex=1.2)
text(20,70,"At 90 m Resolution, Slope = 96",cex=1.2)

# InterceptVsDEMres = lm(FittedIntercept~DEMres,data=data.frame(FittedIntercept,DEMres))
# summary(InterceptVsDEMres)$adj.r.squared
# plot(DEMres,FittedIntercept,pch=16)
# abline(InterceptVsDEMres)
# PredInterceptDEMres = as.vector(predict(InterceptVsDEMres,newdata=data.frame(DEMres=90)))
# print(PredInterceptDEMres)

# Attempt to set parameters from Nature paper Figure 2c
SlopeCurveGain = -446.3
SlopeCurveOffset = 30.3

hist(Curvature1[,]/-100)
LocalCurveSD = sd(Curvature1[,]/-100)
print(LocalCurveSD)

PredSlopeCurveSD = SlopeCurveGain * LocalCurveSD + SlopeCurveOffset
print(PredSlopeCurveSD)

# I think this method underpredicts the slope parameter because it assumes a 3 m DEM

################################################################################

# Must use 30 m DEM for congruence with published stats
# From Table 2: at 30 m, slope = 	37.90

Curvature1 = raster("TCSIdomain_30mNonFilledDEM_ArcGIScurvature.tif")

plot(Curvature1/-100)

CurveSlope = 37.90
#CurveSlope = 76 # Manual sensitivity tests
CurveOffset = 2

SoilDepth30m = CurveOffset + CurveSlope * (Curvature1/-100)
SoilDepth = aggregate(SoilDepth30m,3,fun=mean)

#plot(SoilDepth)

# Fix minimum depths
TooShallowPtrs = which(SoilDepth[,] < MinSoilDepth[,])
SoilDepth[TooShallowPtrs] = MinSoilDepth[TooShallowPtrs]

# Bare rock --> minimum soil (approximated by NLCD barren land or DHSVM class 32)
# Alpine shrub --> minimum soil (approximated by NLCD grass/crop/shrub already masked above 2500 m)
# Grassland --> minimum soil (NLCD grass/crop/shrub in high-elevation area is mostly alpine rock; DHSVM class 33)
BareRockPtrs = which(VegClass[,] == 32 | VegClass[,] == 35 | (VegClass[,] == 33 & DEM[,] > 2000))
SoilDepth[BareRockPtrs] = MinSoilDepth[BareRockPtrs]

D99th = sort(SoilDepth[!is.na(SoilDepth[,])])[round(length(SoilDepth[!is.na(SoilDepth[,])])*0.99)]
plot(SoilDepth,zlim=c(0,D99th))
print(paste0("Mean = ",round(mean(SoilDepth[,]),1)))
print(paste0("Std. Dev. = ",round(sd(SoilDepth[,]),1)))

DataDensity = density(unlist(extract(SoilDepth, BasinShape)))
plot(DataDensity$x,DataDensity$y,type="l",xlab="Soil Depth (m)",ylab="Fraction of Basin Area",xlim=c(0,6),lwd=2)

writeRaster(SoilDepth,paste0("SoilDepth_Slope",CurveSlope,"Offset",CurveOffset,".tif"),overwrite=TRUE)

################################################################################

# Convert to header-less DHSVM input format

nRows = 1436 # Number of rows in the domain, used to write text files
nHeaderLines = 6 # Number of rows in a .asc raster header

# Slopes = c(0,38,76)
# Offsets = c(1,3,5)
# 
# for (i in 1:length(Slopes)){
#   for (j in 1:length(Offsets)){
#     TiffRaster = raster(paste0("SoilDepth_Slope",Slopes[i],"Offset",Offsets[j],".tif"))
#     writeRaster(TiffRaster,"temp.asc",overwrite=TRUE)
#     TextRaster = readLines("temp.asc")
#     writeLines(TextRaster[(nHeaderLines+1):(nHeaderLines+nRows)],paste0("soild_s",Slopes[i],"o",Offsets[j],".txt"))
#   }
# }

TiffRaster = raster(paste0("SoilDepth_Slope37.9Offset2.tif"))
writeRaster(TiffRaster,"temp.asc",overwrite=TRUE)
TextRaster = readLines("temp.asc")
writeLines(TextRaster[(nHeaderLines+1):(nHeaderLines+nRows)],paste0("soild.txt"))

file.remove("temp.asc")

################################################################################

# Compare histograms of chosen soil maps within North Yuba watershed (30 m)

LineColors = c("deepskyblue2","goldenrod2","darkorchid1")

# Test slope effect, offset 3 m
CurveNames = c("Slope76","Slope38","Slope0")
CurveLongNames = c("Slope = 76","Slope = 38","Slope = 0")
Rasters = lapply(paste0("SoilDepth_",CurveNames,"Offset3.tif"), raster)

for (i in 1:length(CurveNames)){
  SoilDepthInBasin = unlist(extract(Rasters[[i]], BasinShape))
  DataDensity = density(SoilDepthInBasin)
  if (i == 1) {
    plot(DataDensity$x,DataDensity$y,type="l",main="Histogram of Curvature-Based Soil Depth\nCalculated at 30 m, Offset = 3 m\nNorth Yuba Watershed Above New Bullards Bar",xlab="Soil Depth (m)",ylab="Fractional Area",xlim=c(0,5),ylim=c(0,3.5),col=LineColors[i],lwd=3,cex.axis=1.2,cex.lab=1.2)
  } else {
    lines(DataDensity$x,DataDensity$y,col=LineColors[i],lwd=3)
  }
}
legend("topleft",inset=0.03,legend=CurveLongNames,col=LineColors,lty=rep(1,length(CurveNames)),lwd=rep(6,length(CurveNames)),cex=1.3)


# Test offset effect, slope 38
CurveNames = c("Offset5","Offset3","Offset1")
CurveLongNames = c("Offset = 5 m","Offset = 3 m","Offset = 1 m")
Rasters = lapply(paste0("SoilDepth_Slope38",CurveNames,".tif"), raster)

for (i in 1:length(CurveNames)){
  SoilDepthInBasin = unlist(extract(Rasters[[i]], BasinShape))
  DataDensity = density(SoilDepthInBasin)
  if (i == 1) {
    plot(DataDensity$x,DataDensity$y,type="l",main="Histogram of Curvature-Based Soil Depth\nCalculated at 30 m, Slope = 38\nNorth Yuba Watershed Above New Bullards Bar",xlab="Soil Depth (m)",ylab="Fractional Area",xlim=c(0,5),ylim=c(0,3.5),col=LineColors[i],lwd=3,cex.axis=1.2,cex.lab=1.2)
  } else {
    lines(DataDensity$x,DataDensity$y,col=LineColors[i],lwd=3)
  }
}
legend("topleft",inset=0.03,legend=CurveLongNames,col=LineColors,lty=rep(1,length(CurveNames)),lwd=rep(6,length(CurveNames)),cex=1.3)







