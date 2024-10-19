library(raster)

InputFile = "MaxSWEdate_2016.txt"
OutputFilePrefix = "MaxSWEdate_"
OutputYears = rep(2016,1)
# OutputMonths = 1:2
# OutputDays = rep(1,2)

# Set up basin name for later use
BasinNumber = 5
BasinNames = c("Truckee","Yuba","Bear","American","TCSI")
BasinName = BasinNames[BasinNumber]
if(BasinNumber==5){BasinNumber=0}

nRows = 1436

HeaderText = "ncols         2247\nnrows         1436\nxllcorner     620190\nyllcorner     4276890\ncellsize      90\nNODATA_value  -9999\n"

dir = paste0("C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/DHSVM/",BasinNumber,"_",BasinName,"/output/")
setwd(dir)

RawRasterStack = read.table(InputFile)

nRasters = dim(RawRasterStack)[1]/nRows

for (i in 1:nRasters){
  # Subset a raster from the stack
  SingleMap = RawRasterStack[((i-1)*1436+1):(1436*i),]

  # Write raster to single file with added header
  #OutFile = file(paste0(OutputFilePrefix,OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i],"_",BasinName,".asc"),"w")
  OutFile = file(paste0(OutputFilePrefix,OutputYears[i],"_",BasinName,".asc"),"w")
  cat(HeaderText, file=OutFile)
  write.table(SingleMap, file=OutFile, col.names=FALSE, row.names=FALSE)
  close(OutFile)
  remove(OutFile)
}



# Merge rasters

InputFile = "Map.Snow.MaxSweDate.asc"
InputFile = "Map.Snow.MeltOutDate.asc"

for (BasinNumber in 1:4){
  BasinName = BasinNames[BasinNumber]
  dir = paste0("C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/DHSVM/",BasinNumber,"_",BasinName,"/output/")
  setwd(dir)
  
  RawRasterStack = read.table(InputFile)
  
  for (i in 1:nRasters){
    # Subset a raster from the stack
    SingleMap = RawRasterStack[((i-1)*1436+1):(1436*i),]
    
    # Write raster to single file with added header
    #OutFile = file(paste0(OutputFilePrefix,OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i],"_",BasinName,".asc"),"w")
    OutFile = file(paste0(OutputFilePrefix,OutputYears[i],"_",BasinName,".asc"),"w")
    cat(HeaderText, file=OutFile)
    write.table(SingleMap, file=OutFile, col.names=FALSE, row.names=FALSE)
    close(OutFile)
    remove(OutFile)
  }
  
  RastFile = paste0(OutputFilePrefix,OutputYears[i],"_",BasinName,".asc")
  if (BasinNumber==1){
    RastMerged = raster(RastFile)
    crs(RastMerged) = CRS("+init=epsg:32610")
  } else {
    RastMerged = max(RastMerged, raster(RastFile))
  }
}

plot(RastMerged)

RastMerged = raster("DateMaxSWE_2016_TCSI.txt")

# Convert dates
RastMergedDates = as.Date(as.character(RastMerged[,]),format="%Y%m%d")
DayOfYear = as.numeric(strftime(RastMergedDates,format="%j"))
DayOfYear[is.na(DayOfYear)] = -9999
WaterYear = max(as.numeric(format(RastMergedDates[!is.na(RastMergedDates)],"%Y")))
EndOfWaterYear = as.numeric(strftime(as.Date(paste0(WaterYear,"-09-30"),format="%Y-%m-%d"),format="%j"))
DayOfYear[DayOfYear>EndOfWaterYear] = DayOfYear[DayOfYear>EndOfWaterYear] - 365

RastMergedDOY = RastMerged
RastMergedDOY[,] = DayOfYear
NAvalue(RastMergedDOY) = -9999

plot(RastMergedDOY,zlim=c(-50,180))

dir = paste0("C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/DHSVM/X.Results/")
setwd(dir)

writeRaster(RastMergedDOY, paste0("DateMaxSWE_",OutputYears[i],".tif"), NAflag=-9999, overwrite=TRUE)
writeRaster(RastMergedDOY, paste0("DateMeltOut_",OutputYears[i],".tif"), NAflag=-9999, overwrite=TRUE)















