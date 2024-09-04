# Compute spatially variable lapse rates for DHSVM from gridMet and PRISM data

library(raster)

# Read a template for the domain extent
dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/PRISM-GridMet_Multiplier/"
setwd(dir)
DomainTemplate = raster("TCSIdomainTemplate.tif")
# Get list of final GridMet "stations," each of which will have its own lapse rate
GridMetPointsAdj = read.csv("Reprojected_GridMet_CellCenterPoints.csv")

# Read the PRISM DEM
dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/"
setwd(dir)
ElevMapRaw = raster("PRISM_us_dem_800m_asc.asc")

AggrFactor = 5

# Reproject PRISM DEM to match model coordinate system and extent
ElevMap = crop(ElevMapRaw,extent(projectRaster(DomainTemplate,crs=crs(ElevMapRaw))))
ElevMap = projectRaster(ElevMap,crs=crs(DomainTemplate))
ElevPoints = as.data.frame(ElevMap,xy=TRUE)
plot(ElevMap)
lines(extent(DomainTemplate))

# Set up desired final data structure
nStations = length(GridMetPointsAdj$x)
SpatialLapseRates = data.frame(Month=sort(rep(1:12,nStations)), StationX=rep(GridMetPointsAdj$x,12), StationY=rep(GridMetPointsAdj$y,12), TemperatureLapseRate=rep(0,12*nStations), TemperatureLapseRateUnits=rep("degC/m",12*nStations))

for (mon in 1:12){
  print("**************** MONTH ****************")
  print(mon)
  
  TprismRaw = raster(paste0("PRISM_tmean_30yr_normal_800mM3_",formatC(mon,width=2,flag="0"),"_asc.asc"))
  Tprism = projectRaster(TprismRaw, ElevMap)
  plot(Tprism)
  lines(extent(DomainTemplate))
  
  TprismPoints = as.data.frame(Tprism, xy=TRUE)
  
  # Calculate a lapse rate for each 5x5 cluster of cells and associate it with the appropriate GridMet station
  for (i in 1:nStations){
    # Find the 25 PRISM cells that are clustered around a particular GridMet station
    DistancesToStations = (TprismPoints$x - SpatialLapseRates$StationX[nStations*(mon-1)+i])^2 + (TprismPoints$y - SpatialLapseRates$StationY[nStations*(mon-1)+i])^2
    CloseToStation = head(sort(DistancesToStations),AggrFactor^2)
    ptrs = !is.na(match(DistancesToStations,CloseToStation))
    
    # Subset the closest 25 PRISM elevation and temperature cells
    ElevCluster = ElevPoints[ptrs,3]
    TprismCluster = TprismPoints[ptrs,3]
    #plot(ElevCluster,TprismCluster)
    
    # Calculate linear lapse rate
    LinModel = lm(Temp ~ Elev, data=data.frame(Temp=TprismCluster,Elev=ElevCluster))
    LapseRate = coef(LinModel)[2] # degC/m
    
    if(!is.na(LapseRate)){
      # Truncate positive lapse rates
      if (LapseRate > 0){
        LapseRate = 0
      }
      
      # Fix too-negative lapse rates over Lake Tahoe and other super-flat areas
      if (LapseRate < -0.01 && (max(ElevCluster)-min(ElevCluster) < 100)){
        LapseRate = 0
      }
    } else {
      if (max(ElevCluster)-min(ElevCluster) < 100){
        LapseRate = 0
      }
    }
    
    SpatialLapseRates$TemperatureLapseRate[nStations*(mon-1)+i] = LapseRate
    
    print(i/nStations)
  }
}

hist(SpatialLapseRates$TemperatureLapseRate,xlim=c(-0.02,0.02),breaks=seq(-100,100,0.001))
min(SpatialLapseRates$TemperatureLapseRate[!is.na(SpatialLapseRates$TemperatureLapseRate)])

Months = 1:12
MonthlyLapseRates = 0*Months
MeanLapseRatesByCell = rep(0,nStations)
for (mon in 1:12){
  MonthPtrs = which(SpatialLapseRates$Month==mon)
  NAptrs = which(is.na(SpatialLapseRates$TemperatureLapseRate[MonthPtrs]))
  NotNAptrs = which(!is.na(SpatialLapseRates$TemperatureLapseRate[MonthPtrs]))
  NegativePtrs = which(SpatialLapseRates$TemperatureLapseRate[MonthPtrs[NotNAptrs]]<0)
  
  #hist(SpatialLapseRates$TemperatureLapseRate[MonthPtrs[NotNAptrs]],xlim=c(-0.02,0.02),breaks=seq(-100,100,0.001))
  
  # Get median monthly "fill" value
  MedianLapse = median(SpatialLapseRates$TemperatureLapseRate[MonthPtrs[NotNAptrs[NegativePtrs]]])
  
  # Convert NAs (presumably in nearly flat areas) to median
  SpatialLapseRates$TemperatureLapseRate[MonthPtrs[NAptrs]] = MedianLapse
  
  PositivePtrs = which(SpatialLapseRates$TemperatureLapseRate[MonthPtrs] > 0)
  SpatialLapseRates$TemperatureLapseRate[MonthPtrs[PositivePtrs]] = 0
  
  # Fill too-negative lapse rates: minimum of -10 deg/km
  #TooNegativePtrs = which(SpatialLapseRates$TemperatureLapseRate[MonthPtrs] < -0.01)
  #SpatialLapseRates$TemperatureLapseRate[MonthPtrs[TooNegativePtrs]] = MedianLapse
  
  #hist(SpatialLapseRates$TemperatureLapseRate[MonthPtrs[NotNAptrs]],xlim=c(-0.02,0.02),breaks=seq(-2,1,0.001))
  
  # Calculate aggregate stats just for fun
  MonthlyLapseRates[mon] = mean(SpatialLapseRates[SpatialLapseRates$Month==mon,]$TemperatureLapseRate)
  MeanLapseRatesByCell = MeanLapseRatesByCell + SpatialLapseRates[SpatialLapseRates$Month==mon,]$TemperatureLapseRate/12
}

write.csv(SpatialLapseRates,"TCSImonthlyTemperatureLapseRatesPRISM_ByGridMetStation.csv")

plot(Months,MonthlyLapseRates,type="l")

# Create raster of average lapse rate
library(dismo)

LapsePoints = data.frame(x=GridMetPointsAdj$x, y=GridMetPointsAdj$y - 90/2, LapseRate=MeanLapseRatesByCell)
LapseDomains = SpatialPoints(LapsePoints[,1:2], proj4string=crs(DomainTemplate))
LapseDomains = SpatialPointsDataFrame(LapseDomains,LapsePoints)
LapseVoronoi = voronoi(LapseDomains)

LapseRaster = rasterize(LapseVoronoi, DomainTemplate, "LapseRate")
plot(LapseRaster)
lines(LapseVoronoi)

writeRaster(LapseRaster,"AvgTemperatureLapseRate.tif",overwrite=TRUE)

#######################################
# Add lapse rates to station meteorological forcing files

library(rgdal)

# Set up basin name for later use
BasinNumber = 1
BasinNames = c("Truckee","Yuba","Bear","American")
BasinName = BasinNames[BasinNumber]

# Get lapse rate data generated above
dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/"
setwd(dir)
SpatialLapseRates = read.csv("TCSImonthlyTemperatureLapseRatesPRISM_ByGridMetStation.csv")

# Re-create standardized UTM grid
AllowedX = unique(SpatialLapseRates$StationX)
AllowedY = unique(SpatialLapseRates$StationY)

# Get pre-existing files
dir = paste0("C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/MetSim/output/",BasinName,"/StationForcingFiles/")
setwd(dir)
StationFiles = list.files(dir)

for (i in 1:length(StationFiles)){
  OriginalData = read.table(StationFiles[i])
  
  # Only append a new column if it isn't already there
  if (dim(OriginalData)[2] == 7){
    # Convert coordinates in filename into UTM and snap to the allowed grid center points
    Lat = as.numeric(substr(StationFiles[i],6,13))
    Lon = as.numeric(substr(StationFiles[i],15,24))
    coords = SpatialPoints(cbind(Lon,Lat), proj4string=crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    coordsUTM = spTransform(coords, crs(DomainTemplate))
    x = as.data.frame(coordsUTM)$Lon
    y = as.data.frame(coordsUTM)$Lat
    xAdj = AllowedX[abs(AllowedX-x)==min(abs(AllowedX-x))]
    yAdj = AllowedY[abs(AllowedY-y)==min(abs(AllowedY-y))]
    
    Months = as.integer(substr(as.data.frame(OriginalData)$V1,1,2))
    TempLapseRates = rep(0,length(Months))
    
    for (mon in 1:12){
      TempLapseRates[Months==mon] = SpatialLapseRates[(SpatialLapseRates[,"StationX"]==xAdj & SpatialLapseRates[,"StationY"]==yAdj & SpatialLapseRates[,"Month"]==mon),]$TemperatureLapseRate
    }
    
    UpdatedData = cbind(OriginalData, TempLapseRates)
    
    OutFile = file(StationFiles[i], "wb")
    write.table(UpdatedData, file=OutFile, col.names=FALSE, row.names=FALSE, quote=FALSE)
    close(OutFile)
  }
  print(i/length(StationFiles))
}





