library(ncdf4)
library(raster)
library(sf)

# Set up basin name for later use
BasinNumber = 1
BasinNames = c("Truckee","Yuba","Bear","American")
BasinName = BasinNames[BasinNumber]
#BasinName = "TCSIsnow"

# Read basin shapefile
dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/"
setwd(dir)
BasinMask = read_sf(paste0(BasinName,"Shape"))
BasinMaskBuffered = st_buffer(st_transform(BasinMask,crs=CRS("+init=epsg:32610")),5000) # ~ 4 km cells plus some extra

# Set up the standardized grid for consistent nearest-neighbor interpolation in DHSVM and GDAL
# Must have previously run GridMetPRISMmultiplierForDHSVM.R
dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/PRISM-GridMet_Multiplier/"
setwd(dir)
GridMetCenterPoints = read.csv("Reprojected_GridMet_CellCenterPoints.csv")
xGrid = unique(GridMetCenterPoints$x)
yGrid = unique(GridMetCenterPoints$y)

# Read all the actual met station names from the final gridforcing folder
dir = "C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/DHSVM/gridforcing"
StationFiles = list.files(dir)

# Read MetSim domain file (already containing required elevation info) created by GridMetProcessForDHSVM.R
dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/MetSim/"
setwd(dir)
nc_domain = nc_open(paste0(BasinName,"Domain.nc"))

# Pull elevation and lat/lon data out of GridMet domain netCDF
elevation.array = ncvar_get(nc_domain,"elev")
lons = ncvar_get(nc_domain,"lon")
lats = ncvar_get(nc_domain,"lat")

nc_close(nc_domain)

# Plot the DEM to make sure it looks ok (does NOT have to line up yet, will be moved around)
elevation.rast = raster(t(elevation.array), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
plot(projectRaster(elevation.rast,crs=CRS("+init=epsg:32610")))
plot(BasinMaskBuffered[2],add=TRUE,col=NA,lwd=2)
plot(st_transform(BasinMask,crs=crs(BasinMaskBuffered))[2],add=TRUE,col=NA,lwd=2)

# Count number of stations inside watershed
StationCount = 0
for (i in 1:length(lons)){
  Lon = lons[i]
  for (j in 1:length(lats)){
    Lat = lats[j]
    
    if (paste0("data_",format(round(Lat,5),nsmall=5),"_",format(round(Lon,5),nsmall=5)) %in% StationFiles){
      # Reproject to UTM Zone 10
      coord.dec = SpatialPoints(cbind(Lon,Lat),proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
      coord.utm = spTransform(coord.dec, CRS("+init=epsg:32610"))
      Xoriginal = coordinates(coord.utm)[1]
      Yoriginal = coordinates(coord.utm)[2]
      
      # Snap to uniform UTM Zone 10 grid for consistent nearest-neighbor interpolation in DHSVM and GDAL
      Xgridded = xGrid[abs(xGrid-Xoriginal)==min(abs(xGrid-Xoriginal))]
      Ygridded = yGrid[abs(yGrid-Yoriginal)==min(abs(yGrid-Yoriginal))]
      
      # Check if station is inside (or nearly inside) watershed mask
      StationPoint = st_as_sf(data.frame("x"=Xgridded,"y"=Ygridded), coords=c("x","y"), crs=CRS("+init=epsg:32610"))
      StationInWatershed = st_intersects(StationPoint, BasinMaskBuffered)
      
      if (!is.na(as.integer(StationInWatershed))){
        StationCount = StationCount + 1
        
        points(Xgridded,Ygridded)
      } else {
        points(Xgridded,Ygridded,pch=19)
      }
      
    }
  }
  print(StationCount)
}

# Create text file to copy-paste into DHSVM config file
OutFile = file(paste0("MeteorologySection_",BasinName,".txt"),"w")
write(paste0("Number of Stations = ",StationCount), file=OutFile)

# Iterate through each grid cell and add it to the DHSVM config file as a station
StationNum = 1
for (i in 1:length(lons)){
  Lon = lons[i]
  for (j in 1:length(lats)){
    Lat = lats[j]
    Elev = elevation.array[i,j]
    
    if (paste0("data_",format(round(Lat,5),nsmall=5),"_",format(round(Lon,5),nsmall=5)) %in% StationFiles){
      # Reproject to UTM Zone 10
      coord.dec = SpatialPoints(cbind(Lon,Lat),proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
      coord.utm = spTransform(coord.dec, CRS("+init=epsg:32610"))
      Xoriginal = coordinates(coord.utm)[1]
      Yoriginal = coordinates(coord.utm)[2]
      
      # Snap to uniform UTM Zone 10 grid for consistent nearest-neighbor interpolation in DHSVM and GDAL
      Xgridded = xGrid[abs(xGrid-Xoriginal)==min(abs(xGrid-Xoriginal))]
      Ygridded = yGrid[abs(yGrid-Yoriginal)==min(abs(yGrid-Yoriginal))]
      
      # Check if station is inside (or nearly inside) watershed mask
      StationPoint = st_as_sf(data.frame("x"=Xgridded,"y"=Ygridded), coords=c("x","y"), crs=CRS("+init=epsg:32610"))
      StationInWatershed = st_intersects(StationPoint, BasinMaskBuffered)
      
      if (!is.na(as.integer(StationInWatershed))){
        cat(paste0("\nStation Name     ",StationNum," = ","data_",format(round(Lat,5),nsmall=5),"_",format(round(Lon,5),nsmall=5)), file=OutFile, append=TRUE)
        cat(paste0("\nNorth Coordinate ",StationNum," = ",Ygridded), file=OutFile, append=TRUE)
        cat(paste0("\nEast Coordinate  ",StationNum," = ",Xgridded), file=OutFile, append=TRUE)
        cat(paste0("\nElevation        ",StationNum," = ",format(round(Elev,1),nsmall=1)), file=OutFile, append=TRUE)
        cat(paste0("\nStation File     ",StationNum," = ",paste0("../gridforcing/data_",format(round(Lat,5),nsmall=5),"_",format(round(Lon,5),nsmall=5),"\n")), file=OutFile, append=TRUE)
        
        StationNum = StationNum + 1 
      }
    }
  }
  print(i/length(lons))
  
  if (i == length(lons)){
    close(OutFile)
  }
}

################################################################################
# Make actual "DHSVM-normal" precipitation map for visualization/justification
library(dismo)
library(sf)

FirstYr = 1991 # same as PRISM
LastYr = 2020

# Hack for now: just tell it which lines to use
FirstLine = 35065
LastLine = 122728

domaindir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/WatershedBoundaries/"

# Read basin domain template
dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/PRISM-GridMet_Multiplier/"
setwd(dir)
DomainTemplate = raster("TCSIdomainTemplate.tif")

shapedir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/"

stationdir = "C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/DHSVM/gridforcing/"
StationMonths = as.numeric(format(as.POSIXct(read.table(paste0(stationdir,"data_38.64758_-119.99928"))[,1],format="%m/%d/%Y-%H"),"%m"))
LinesEnum = 1:length(read.table(paste0(stationdir,"data_38.64758_-119.99928"))[,1])

redistrdir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/PRISM-GridMet_Multiplier/"

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/MetSim/"
setwd(dir)

BasinNames = c("Truckee","Yuba","Bear","American")

for (BasinNumber in 1:length(BasinNames)){
  BasinName = BasinNames[BasinNumber]
  # Read basin shape and mask
  BasinMask = read_sf(paste0(shapedir,BasinName,"Shape"))
  BasinMaskRaster = raster(paste0(domaindir,"TCSImask_",BasinName,"Domain.tif"))
  BasinMaskRaster[BasinMaskRaster==0] = NA
  
  MetSection = readLines(paste0("MeteorologySection_",BasinName,".txt"))
  
  StationName = c()
  StationNorth = c()
  StationEast = c()
  
  for (i in 1:length(MetSection)){
    if (grepl("Name",MetSection[i])){
      StationName = c(StationName, sub(".*= ","",MetSection[i]))
      StationNorth = c(StationNorth, sub(".*= ","",MetSection[i+1]))
      StationEast = c(StationEast, sub(".*= ","",MetSection[i+2]))
    }
  }
  
  # DHSVM keeps the first station only when 2 are equally "close"
  UniqueStationPtrs = match(unique(paste0(StationNorth,StationEast)), paste0(StationNorth,StationEast))
  nUniqueStations = length(UniqueStationPtrs)
  print(nUniqueStations)
  PointsNorth = c()
  PointsEast = c()
  PointsAccumPrecip = c()
  for (i in UniqueStationPtrs){
    StationFile = read.table(paste0(stationdir,StationName[i]))
    AccumPrecip = sum(StationFile[FirstLine:LastLine,7])/(LastYr-FirstYr+1)
    PointsAccumPrecip = c(PointsAccumPrecip, AccumPrecip)
    PointsNorth = c(PointsNorth, StationNorth[i])
    PointsEast = c(PointsEast, StationEast[i])
    print(i)
  }
  
  # The -90/2 is because of some funny business with the way DHSVM chooses nearest neighbor
  # It's supposed to be consistent with GridMetPRISMmultiplierForDHSVMadvanced.R
  RedistrPPoints = data.frame(x=as.numeric(PointsEast),y=as.numeric(PointsNorth)-90/2,PointsAccumPrecip)
  plot(DomainTemplate)
  plot(st_transform(BasinMask,crs=crs(DomainTemplate))[2],add=TRUE,col=NA,lwd=2)
  RedistrPDomains = SpatialPoints(RedistrPPoints[,1:2], proj4string=crs(DomainTemplate))
  RedistrPDomains = SpatialPointsDataFrame(RedistrPDomains,RedistrPPoints)
  plot(RedistrPDomains,add=TRUE)
  RedistrPVoronoi = voronoi(RedistrPDomains)
  plot(RedistrPVoronoi,add=TRUE)
  RedistrPMapModelDomain = rasterize(RedistrPVoronoi, DomainTemplate, "PointsAccumPrecip") * BasinMaskRaster
  plot(RedistrPMapModelDomain)
  
  writeRaster(RedistrPMapModelDomain, paste0("GridMet_1991-2020_MeanYearlyPrecip_DHSVM-",BasinName,".tif"), overwrite=TRUE)
  
  ##########################
  # For PRISM redistribution
  for (mon in 1:12){
    MultiplierMap = raster(paste0(redistrdir,"MultiplierMap_ForGridMet_FromPRISM_Month",mon,".tif"))
    
    MonPointers = which(StationMonths == mon & LinesEnum >= FirstLine & LinesEnum <= LastLine)
    UniqueStationPtrs = match(unique(paste0(StationNorth,StationEast)), paste0(StationNorth,StationEast))
    nUniqueStations = length(UniqueStationPtrs)
    print(nUniqueStations)
    PointsNorth = c()
    PointsEast = c()
    PointsAccumPrecip = c()
    for (i in UniqueStationPtrs){
      StationFile = read.table(paste0(stationdir,StationName[i]))
      AccumPrecip = sum(StationFile[MonPointers,7])/(LastYr-FirstYr+1)
      PointsAccumPrecip = c(PointsAccumPrecip, AccumPrecip)
      PointsNorth = c(PointsNorth, StationNorth[i])
      PointsEast = c(PointsEast, StationEast[i])
      print(i)
    }
    
    # The -90/2 is because of some funny business with the way DHSVM chooses nearest neighbor
    # It's supposed to be consistent with GridMetPRISMmultiplierForDHSVMadvanced.R
    RedistrPPoints = data.frame(x=as.numeric(PointsEast),y=as.numeric(PointsNorth)-90/2,PointsAccumPrecip)
    RedistrPDomains = SpatialPoints(RedistrPPoints[,1:2], proj4string=crs(DomainTemplate))
    RedistrPDomains = SpatialPointsDataFrame(RedistrPDomains,RedistrPPoints)
    RedistrPVoronoi = voronoi(RedistrPDomains)
    RedistrPMapModelDomain = rasterize(RedistrPVoronoi, DomainTemplate, "PointsAccumPrecip") * MultiplierMap * BasinMaskRaster
    plot(RedistrPMapModelDomain)
    
    # Compute yearly sum of redistributed precip
    if (mon==1){
      YearlyRedistrP = RedistrPMapModelDomain
    } else {
      YearlyRedistrP = YearlyRedistrP + RedistrPMapModelDomain
    }
    
    print(mon)
    print("**********")
  }
  
  writeRaster(YearlyRedistrP, paste0("GridMet_PRISMredistributed__1991-2020_MeanYearlyPrecip_DHSVM-",BasinName,".tif"), overwrite=TRUE)
  
}

