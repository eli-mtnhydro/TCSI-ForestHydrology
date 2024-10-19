# Disaggregate, convert, merge, and collate all DHSVM SWE raster outputs

library(raster)

dir = "E:/DHSVM_ScenarioResults"
setwd(dir)

nRow = 1436
nCol = 2247
nCells = nRow*nCol

MaskRast = raster("TCSImask_MergedDomain.tif")

################################################################################
# Test: read and plot a single map

# RastName = "P195S1cnrmR1/American_Chunk2/Map.Snow.MaxSwe.bin"
# 
# RasterStack = readBin(con=RastName, what="double", n=1e9, size=4)
# 
# length(RasterStack)/(2247*1436)
# 
# SingleRast = raster(t(matrix(RasterStack[1:nCells], nrow=nCol, ncol=nRow)),
#                     template=MaskRast)
# 
# max(SingleRast[,])
# plot(SingleRast)

# Map.Snow.Swq: what="double", n=1e9, size=4
# Map.Snow.MaxSwe: what="double", n=1e9, size=4
# Map.Snow.MaxSweDate: what="integer", n=1e9, size=4
# Map.Snow.MeltOutDate: what="integer", n=1e9, size=4

################################################################################
# Create geotiff SWE maps for each year

WaterYears = 2015:2099

MapNames = c("April1","PeakSWE","PeakDate","MeltOutDate")
MapTypes = c("Swq","MaxSwe","MaxSweDate","MeltOutDate")
DataTypes = c("double","double","integer","integer")

BasinNames = c("Truckee", "Yuba", "Bear", "American")
BasinChunks = c(2, 4, 1, 8) # How the runs were split up

########## DHSVM run settings

SelectedModel = 276

Scenarios = c(7)

Climates = c("cnrm", "miroc")
ClimateNames = c("CNRM-CM5_RCP8.5", "MIROC5_RCP8.5")

LandisRuns = c(1, 4) # 1 for CNRM, 4 for MIROC since 1 wasn't fully available

for (Scenario in Scenarios){
  for (Climate in Climates){
    
    # Usable length only, not including overlap / spinup
    BasinChunkLengths = c(43,42,                   # Truckee 
                          22,22,22,19,             # Yuba
                          85,                      # Bear
                          11,11,11,11,11,11,11,8) # American
    
    # Number of duplicate maps output during spinup
    BasinChunkSkip = c(0,2,
                       0,2,2,2,
                       0,
                       0,2,2,2,2,2,2,2)
    
    # Special case: P195S7mirocR4 fails in American chunk 2 (negative soil moisture) on 10/1/24
    # Pull 9/30/2024 initial state from American chunk 1 in P195 S6miroc
    # Restart American chunk 2 in P195 S7miroc using initial state ^ and changing run time to [10/1/2024 to 9/30/2036]
    if ((Scenario==7) & (Climate=="miroc") & (SelectedModel==195)){
      
      BasinChunkLengths[8] = 10
      BasinChunkLengths[9] = 12
      
      BasinChunkSkip[9] = 0
      
    } # End special case
    
    ClimateName = ClimateNames[which(Climates==Climate)]
    LANDIsrun = LandisRuns[which(Climates==Climate)]
    
    # Generic setup for this climate + scenario combo
    
    RunCase = paste0("P",SelectedModel,"S",Scenario,Climate,"R",LANDIsrun)
    
    for (MapType in MapTypes){
      
      # Merge DHSVM run chunks and different basins into one mega raster stack
      AllBasinData = rep(0, nCells*length(WaterYears))
      BCidx = 1 # Keep track of the basin-chunks to facilitate indexing of overlap years etc.
      for (Basin in BasinNames){
        
        # Concatenate DHSVM run chunks, removing overlapping (spinup) years
        BasinData = c()
        for (Chunk in 1:BasinChunks[which(BasinNames==Basin)]){
          
          # Special case: P195S7mirocR4 fails in American chunk 2 (negative soil moisture) on 10/1/24
          if ((Scenario==7) & (Climate=="miroc") & (SelectedModel==195) & (Basin=="American") & (Chunk==2)){
            RastName = paste0(RunCase,"/",Basin,"_Chunk",Chunk,"fix/Map.Snow.",MapType,".bin")
          } else { # Normal case
            RastName = paste0(RunCase,"/",Basin,"_Chunk",Chunk,"/Map.Snow.",MapType,".bin")
          }
          
          RasterStack = readBin(con=RastName, what=DataTypes[which(MapTypes==MapType)], n=1e9, size=4)
          
          StartPtr = nCells*BasinChunkSkip[BCidx] + 1
          EndPtr = nCells*BasinChunkLengths[BCidx] + (StartPtr - 1)
          
          BasinData = c(BasinData, RasterStack[StartPtr:EndPtr])
          
          BCidx = BCidx + 1
        }
        
        # Merge basins
        AllBasinData = pmax(AllBasinData, BasinData)
        
        print(paste0("Done with ",Basin," in ",RunCase))
      }
      
      if (length(AllBasinData)/(2247*1436) - length(WaterYears)){
        print("Error with indexing!!!")
      }
      
      # Set up output directory
      MapName = MapNames[which(MapTypes==MapType)]
      OutputDir = "SWEmaps/"
      
      # Output final raster for each year in the current RunCase
      for (yr in 1:length(WaterYears)){
        
        StartPtr = nCells*(yr-1) + 1
        EndPtr = nCells + (StartPtr - 1)
        
        SingleRast = raster(t(matrix(AllBasinData[StartPtr:EndPtr], nrow=nCol, ncol=nRow)),
                            template=MaskRast)
        
        # Convert date from YYYYMMDD --> day of water year if necessary
        if (grepl("Date", MapType)){
          SingleRasterDates = as.Date(as.character(SingleRast[,]),format="%Y%m%d")
          DayOfYear = as.numeric(strftime(SingleRasterDates,format="%j"))
          DayOfWaterYear = DayOfYear + 92 # Day of year --> day of water year
          #DayOfWaterYear[is.na(DayOfWaterYear)] = 0
          WaterYear = WaterYears[yr]
          #EndOfWaterYear = as.numeric(strftime(as.Date(paste0(WaterYear,"-09-30"),format="%Y-%m-%d"),format="%j"))
          DaysThisYear = as.numeric(strftime(as.Date(paste0(WaterYear,"-12-31"),format="%Y-%m-%d"),format="%j"))
          Ptrs = which(DayOfWaterYear > DaysThisYear)
          DayOfWaterYear[Ptrs] = DayOfWaterYear[Ptrs] - DaysThisYear # Fix rollover where Oct-Jan are >365
          SingleRast[,] = DayOfWaterYear
        }
        
        plot(SingleRast, main=paste(MapName,RunCase,WaterYears[yr]))
        
        writeRaster(SingleRast, paste0(OutputDir,MapName,"_",RunCase,"_",WaterYears[yr],".tif"), overwrite=TRUE)
      }
      
    }
    
    print(paste0("********** Completely Done With ",RunCase," **********"))
    
    dev.off(dev.list()["RStudioGD"]) # Clear all plots
    
    ########## End of post-processing for particular climate + scenario combo
  }
}

