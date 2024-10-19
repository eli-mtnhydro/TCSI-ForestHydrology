# DHSVM throws an error when !(porosity > field capacity > wilting point) for each pixel
# This arises because the porosity and field capacity are map-based and highly variable
# But the wilting point is a single (median) value for each soil class
# Here, we adjust the field capacity of each cell to be strictly greater than the lookup-table wilting point for that soil class
# And we adjust the porosity of each grid cell to be strictly greater than the field capacity of each cell

# Minimum amount (fractional volume) to make porosity larger than field capacity
MinDeltaPFC = 0.1
MinDeltaFCWP = 0.01

library(raster)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/POLARIS/"
setwd(dir)

# Read lookup-table data
SoilStats = read.csv("SoilStats_POLARISmedian_TCSI.csv")
print(SoilStats)

nClasses = length(SoilStats$SoilClass)
nLayers = 3
LayerNames = c("0-15","15-60","60-200")

# Soil class map
ClassRaster = raster("POLARIS_SoilTextureClassification.tif")
plot(ClassRaster)

# The offending raster inputs
RasterNames = c(paste0("Porosity_",LayerNames,"cm_p50_Meter3PerMeter3.tif"),paste0("FieldCapacity_",LayerNames,"cm_p50_Meter3PerMeter3.tif"))

# Read all of the required rasters
lookdir = "_ByLayer_"
Rasters = lapply(paste0(lookdir,"/",RasterNames), raster)

# Rasters 1-3 --> porosity, rasters 4-6 --> field capacity
Porosity1 = Rasters[[1]]
Porosity2 = Rasters[[2]]
Porosity3 = Rasters[[3]]
FieldCapacity1 = Rasters[[4]]
FieldCapacity2 = Rasters[[5]]
FieldCapacity3 = Rasters[[6]]

# Create dummy rasters for wilting point to fill by class
WiltingPoint1 = Rasters[[1]] * 0
WiltingPoint2 = Rasters[[1]] * 0
WiltingPoint3 = Rasters[[1]] * 0

for (cls in 1:nClasses){
  ClassPtrs = (ClassRaster[,] == cls)
  
  # Replace NAs in maps with median values
  Porosity1[is.na(Porosity1[,]) & ClassPtrs] = SoilStats$L1POROSITY[cls]
  Porosity2[is.na(Porosity2[,]) & ClassPtrs] = SoilStats$L2POROSITY[cls]
  Porosity3[is.na(Porosity3[,]) & ClassPtrs] = SoilStats$L3POROSITY[cls]
  FieldCapacity1[is.na(FieldCapacity1[,]) & ClassPtrs] = SoilStats$L1FIELDCAP[cls]
  FieldCapacity2[is.na(FieldCapacity2[,]) & ClassPtrs] = SoilStats$L2FIELDCAP[cls]
  FieldCapacity3[is.na(FieldCapacity3[,]) & ClassPtrs] = SoilStats$L3FIELDCAP[cls]
  
  # Create wilting point rasters
  WiltingPoint1[ClassPtrs] = SoilStats$L1WILTINGPOINT[cls]
  WiltingPoint2[ClassPtrs] = SoilStats$L2WILTINGPOINT[cls]
  WiltingPoint3[ClassPtrs] = SoilStats$L3WILTINGPOINT[cls]
  
}

plot(Porosity1)
plot(Porosity2)
plot(Porosity3)
plot(FieldCapacity1)
plot(FieldCapacity2)
plot(FieldCapacity3)
plot(WiltingPoint1)
plot(WiltingPoint2)
plot(WiltingPoint3)

# Identify problem cells in each field capacity layer
Ptrs1 = ((FieldCapacity1[,] - WiltingPoint1[,]) < MinDeltaFCWP)
Ptrs2 = ((FieldCapacity2[,] - WiltingPoint2[,]) < MinDeltaFCWP)
Ptrs3 = ((FieldCapacity3[,] - WiltingPoint3[,]) < MinDeltaFCWP)

# Replace problem cells (in field capacity maps) with minimum delta above wilting point
FieldCapacity1[Ptrs1] = WiltingPoint1[Ptrs1] + MinDeltaFCWP
FieldCapacity2[Ptrs2] = WiltingPoint2[Ptrs2] + MinDeltaFCWP
FieldCapacity3[Ptrs3] = WiltingPoint3[Ptrs3] + MinDeltaFCWP

# Identify problem cells in each porosity layer
Ptrs1 = ((Porosity1[,] - FieldCapacity1[,]) < MinDeltaPFC)
Ptrs2 = ((Porosity2[,] - FieldCapacity2[,]) < MinDeltaPFC)
Ptrs3 = ((Porosity3[,] - FieldCapacity3[,]) < MinDeltaPFC)

# Replace problem cells (in porosity maps) with minimum delta above field capacity
Porosity1[Ptrs1] = FieldCapacity1[Ptrs1] + MinDeltaPFC
Porosity2[Ptrs2] = FieldCapacity2[Ptrs2] + MinDeltaPFC
Porosity3[Ptrs3] = FieldCapacity3[Ptrs3] + MinDeltaPFC

# Keep track of how many elements have been updated
ChangedCells = data.frame(Porosity1=0,Porosity2=0,Porosity3=0,FieldCap1=0,FieldCap2=0,FieldCap3=0)

ChangedCells$Porosity1 = sum(as.numeric((Porosity1[,] - Rasters[[1]][,]) > 0),na.rm=T)
ChangedCells$Porosity2 = sum(as.numeric((Porosity2[,] - Rasters[[2]][,]) > 0),na.rm=T)
ChangedCells$Porosity3 = sum(as.numeric((Porosity3[,] - Rasters[[3]][,]) > 0),na.rm=T)
ChangedCells$FieldCap1 = sum(as.numeric((FieldCapacity1[,] - Rasters[[4]][,]) > 0),na.rm=T)
ChangedCells$FieldCap2 = sum(as.numeric((FieldCapacity2[,] - Rasters[[5]][,]) > 0),na.rm=T)
ChangedCells$FieldCap3 = sum(as.numeric((FieldCapacity3[,] - Rasters[[6]][,]) > 0),na.rm=T)

ChangedCells[,] = ChangedCells[,] / length(ClassRaster[,])
ChangedCells[,] = paste0(round(100*ChangedCells[,],1),"%")
print(ChangedCells)

# Export fixed maps

writeRaster(FieldCapacity1,"FieldCap_Layer1.asc",overwrite=TRUE)
writeRaster(FieldCapacity2,"FieldCap_Layer2.asc",overwrite=TRUE)
writeRaster(FieldCapacity3,"FieldCap_Layer3.asc",overwrite=TRUE)

writeRaster(Porosity1,"Porosity_Layer1.asc",overwrite=TRUE)
writeRaster(Porosity2,"Porosity_Layer2.asc",overwrite=TRUE)
writeRaster(Porosity3,"Porosity_Layer3.asc",overwrite=TRUE)

# For outputting DHSVM-style header-less ascii rasters
nRows = 1436 # Number of rows in the domain
nHeaderLines = 6 # Number of rows to delete from a .asc raster header

# Concatenate layers and output as header-less DHSVM input format
for (lyr in 1:length(LayerNames)){
  TextRasterPorosity = readLines(paste0("Porosity_Layer",lyr,".asc"))
  TextRasterFieldCap = readLines(paste0("FieldCap_Layer",lyr,".asc"))
  if (lyr == 1){
    MergedRasterPorosity = TextRasterPorosity[(nHeaderLines+1):(nHeaderLines+nRows)]
    MergedRasterFieldCap = TextRasterFieldCap[(nHeaderLines+1):(nHeaderLines+nRows)]
  } else {
    MergedRasterPorosity = c(MergedRasterPorosity,TextRasterPorosity[(nHeaderLines+1):(nHeaderLines+nRows)])
    MergedRasterFieldCap = c(MergedRasterFieldCap,TextRasterFieldCap[(nHeaderLines+1):(nHeaderLines+nRows)])
  }
}
writeLines(MergedRasterPorosity,"soil_porosity.txt")
writeLines(MergedRasterFieldCap,"soil_fcap.txt")

