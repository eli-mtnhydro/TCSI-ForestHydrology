################################################################################
# Make actual DHSVM x MACA x PRISM-redistribution precipitation maps

# !!! Most of this is adapted from MACAprocessForDHSVM.R !!!

library(data.table)
library(dismo)
library(sf)

GCMs = c("CNRM-CM5","MIROC5")
Scenario = "RCP8.5"

WaterYears = 2015:2099

domaindir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/WatershedBoundaries/"

# Read basin domain template
dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/PRISM-GridMet_Multiplier/"
setwd(dir)
DomainTemplate = raster("TCSIdomainTemplate.tif")

shapedir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/"

redistrdir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/PRISM-GridMet_Multiplier/"

configdir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/MetSim/"

outputdir = "C:/Users/board/Desktop/DHSVM/TCSI_Results/AvgYearlyPrecipMaps/"

BasinNames = c("Truckee","Yuba","Bear","American")

# Load multiplier maps
MultiplierMaps = list()
for (mon in 1:12){
  MultiplierMaps[[mon]] = raster(paste0(redistrdir,"MultiplierMap_ForGridMet_FromPRISM_Month",mon,".tif"))
}

for (GCM in GCMs){
  
  stationdir = paste0("C:/Users/board/Desktop/DHSVM/TCSI_Setup/MACA/",GCM,"/StationForcingFiles/",Scenario,"/")
  StationMonths = as.numeric(format(as.POSIXct(read.table(paste0(stationdir,"data_39.77066_-121.41492"))[,1],format="%m/%d/%Y-%H"),"%m"))
  LinesEnum = 1:length(read.table(paste0(stationdir,"data_39.77066_-121.41492"))[,1])
  
  dir = paste0("C:/Users/board/Desktop/DHSVM/TCSI_Setup/MACA/",GCM,"/")
  setwd(dir)
  
  for (BasinNumber in 1:length(BasinNames)){
    BasinName = BasinNames[BasinNumber]
    # Read basin shape and mask
    BasinMask = read_sf(paste0(shapedir,BasinName,"Shape"))
    BasinMaskRaster = raster(paste0(domaindir,"TCSImask_",BasinName,"Domain.tif"))
    BasinMaskRaster[BasinMaskRaster==0] = NA
    
    MetSection = readLines(paste0(configdir,"MeteorologySection_",BasinName,".txt"))
    
    # Identify unique stations in current watershed
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
    #print(nUniqueStations)
    
    # Load all station datasets and associate with raster locations
    StationFiles = list()
    PointsNorth = c()
    PointsEast = c()
    PointsID = c()
    for (i in UniqueStationPtrs){
      StationFile = as.data.frame(fread(paste0(stationdir,StationName[i])))
      
      StationFiles[[i]] = StationFile
      
      PointsID = c(PointsID, i)
      PointsNorth = c(PointsNorth, StationNorth[i])
      PointsEast = c(PointsEast, StationEast[i])
      
      print(i)
    }
    
    # The -90/2 is because of some funny business with the way DHSVM chooses nearest neighbor
    # It's supposed to be consistent with GridMetPRISMmultiplierForDHSVMadvanced.R
    StationTempPoints = data.frame(x=as.numeric(PointsEast),y=as.numeric(PointsNorth)-90/2,PointsID)
    StationTempDomains = SpatialPoints(StationTempPoints[,1:2], proj4string=crs(DomainTemplate))
    StationTempDomains = SpatialPointsDataFrame(StationTempDomains,StationTempPoints)
    StationTempVoronoi = voronoi(StationTempDomains)
    StationTempMapModelDomain = rasterize(StationTempVoronoi, DomainTemplate, "PointsID") * BasinMaskRaster
    #plot(StationTempMapModelDomain)
    
    MapPtrList = list()
    for (i in UniqueStationPtrs){
      MapPtrList[[i]] = which(StationTempMapModelDomain[,]==i)
    }
    
    StationDates = as.Date(StationFile[,1],format="%m/%d/%Y")
    
    for (WaterYear in WaterYears){
      
      FirstLine = min(which(StationDates==as.Date(paste0(WaterYear-1,"-10-01"))))
      LastLine = max(which(StationDates==as.Date(paste0(WaterYear,"-09-30"))))
      
      YearlyRedistrPmerged = DomainTemplate
      YearlyRedistrPmerged[,] = 0
      RedistrPMapModelDomain = DomainTemplate
      
      ##########################
      # For PRISM redistribution
      for (mon in 1:12){
        
        MonPointers = which((StationMonths == mon) & (LinesEnum >= FirstLine) & (LinesEnum <= LastLine))
        
        RedistrPMapModelDomain[,] = 0
        
        for (i in UniqueStationPtrs){
          RedistrPMapModelDomain[MapPtrList[[i]]] = sum(StationFiles[[i]][MonPointers,7])
        }
        
        RedistrPMapModelDomain = RedistrPMapModelDomain * MultiplierMaps[[mon]]
        #plot(RedistrPMapModelDomain)
        
        # Compute yearly sum of redistributed precip
        if (mon==1){
          YearlyRedistrP = RedistrPMapModelDomain
        } else {
          YearlyRedistrP = YearlyRedistrP + RedistrPMapModelDomain
        }
        
        #print(mon)
      }
      
      plot(YearlyRedistrP, main=paste(GCM,WaterYear,BasinName))
      
      OutputFilename = paste0(outputdir,GCM,"-",Scenario,"_PRISMredistributed_WaterYear",WaterYear,"_TotalPrecip_m.tif")
      
      if (file.exists(OutputFilename)){
        
        PreexistingPmerged = raster(OutputFilename)
        
        writeRaster(max(YearlyRedistrP, PreexistingPmerged),
                    OutputFilename,
                    overwrite=TRUE)
      } else {
        writeRaster(YearlyRedistrP,
                    OutputFilename,
                    overwrite=TRUE)
      }
      
      print(WaterYear)
    }
    
    print(paste("**********",BasinName,"**********"))
  }
  
}

# Warnings about lost comment in CRS are ok


