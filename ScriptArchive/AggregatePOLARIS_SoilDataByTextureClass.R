# Take the raster outputs from ProcessPOLARIS_SoilData.R and generate the appropriate inputs for DHSVM

# DHSVM soil inputs
# Soil Description       1 = Clay                                               # From POLARIS sand/silt/clay classification map
# Lateral Conductivity   1 = 1.67e-7 # (m/s)                                    # From POLARIS ksat map
# Exponential Decrease   1 = 0.03 # Exponent for decrease in Ksat with depth    # From differences in POLARIS ksat by depth
# Depth Threshold        1 = 2.0 # Beyond which transmissivity decays linearly  # From assumption of 2 m saturated transmissivity when calculating exponential decrease
# Maximum Infiltration   1 = 1.67e-7 # (m/s)                                    # From POLARIS surface ksat and anisotropy ratio by class
# Surface Albedo         1 = 0.1                                                # MODIS average
# Number of Soil Layers  1 = 3                                                  # Set to 3 by default
# Porosity               1 = 0.475 0.475 0.475                                  # From POLARIS theta_s
# Pore Size Distribution 1 = 0.165 0.165 0.165                                  # From POLARIS lambda (Brooks-Corey)
# Bubbling Pressure      1 = .856 .856 .856                                     # From POLARIS hb
# Field Capacity         1 = .396 .396 .396 # (water retained at 0.33 bar)      # From POLARIS using Van-Genutchen eq.
# Wilting Point          1 = .272 .272 .272 # (water retained at 15 bar)        # From POLARIS using Van-Genutchen eq.
# Bulk Density           1 = 1540 1590 1740 # (kg/m3)                           # From POLARIS bd
# Vertical Conductivity  1 = 1.67e-7 1.67e-7 1.67e-7 # (m/s)                    # From POLARIS harmonic mean of ksat by layer and anisotropy ratio by class
# Thermal Conductivity   1 = 7.70 7.49 7.46    # (W/(m*K))                      # From POLARIS bd using empirical eq.
# Thermal Capacity       1 = 1.4e6 1.4e6 1.4e6 # (J/(m3*K))                     # From POLARIS bd using empirical eq.
# Residual Water Content 1 = 0.090 0.090 0.090                                  # From POLARIS theta_r

# Root zone depths: 0.2 0.4 0.6 (m)
# DHSVM --> 0-20, 20-60, 60-120 (cm)
# POLARIS --> 0-5 and 5-15, 15-30 and 30-60, 60-100 and 100-200 (cm)

################################################################################
# Create base-line config file using median values

library(raster)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/POLARIS/"
setwd(dir)

# For outputting DHSVM-style header-less ascii rasters
nRows = 1436 # Number of rows in the domain
nHeaderLines = 6 # Number of rows to delete from a .asc raster header

# Text files in DHSVM format
SoilConfig = readLines("SoilHeader.txt") # Append soil blocks to this
ConfigTemp = readLines("SoilBlockTemplate.txt")

# Mask for the study area to get the most-relevant local values
Mask = raster("TCSIstatsMask.tif") # Merged Truckee, Yuba, Bear, American minus NLCD water area
MaskPtrs = (Mask[,] == 1)

# Textural soil classes for DHSVM lookup-table
nSoilClasses = 9
ClassNames = c("Clay","Sandy Clay","Clay Loam","Sandy Clay Loam","Loam","Sandy Loam","Loamy Sand","Sand","Silty Soil")
ClassRaster = raster("POLARIS_SoilTextureClassification.tif")
plot(ClassRaster*Mask)

# For ease of creating many different lookup keys
nSoilLayers = 3
LayerNames = c("0-15","15-60","60-200")

# Lookup keys associated with raster data
RasterParamNames = c("SLHC","EXPDEC","MAXINFILTRATION")
RasterParamNames = c(RasterParamNames,paste0("L",1:nSoilLayers,"POROSITY"),paste0("L",1:nSoilLayers,"PSD"))
RasterParamNames = c(RasterParamNames,paste0("L",1:nSoilLayers,"BUBBLINGP"),paste0("L",1:nSoilLayers,"FIELDCAP"))
RasterParamNames = c(RasterParamNames,paste0("L",1:nSoilLayers,"WILTINGPOINT"),paste0("L",1:nSoilLayers,"DENSITY"))
RasterParamNames = c(RasterParamNames,paste0("L",1:nSoilLayers,"SVHC"),paste0("L",1:nSoilLayers,"THERMCONDUCT"))
RasterParamNames = c(RasterParamNames,paste0("L",1:nSoilLayers,"THERMCAPAC"),paste0("L",1:nSoilLayers,"RESWATCONT"))

# Raster files from which statistics will be derived
# Must be in same order as RasterParamNames!
RasterNames = c("KsatSurface_0-5cm_p50_MetersPerSec.tif","KsatExpDecreaseWithDepth_5-200cm_p50_Unitless.tif")
RasterNames = c(RasterNames,"InfiltrationRate_0-5cm_p50_MetersPerSec.tif",paste0("Porosity_",LayerNames,"cm_p50_Meter3PerMeter3.tif"))
RasterNames = c(RasterNames,paste0("PoreSizeDistGeometric_",LayerNames,"cm_p50_Unitless.tif"))
RasterNames = c(RasterNames,paste0("BubblingPressureGeometric_",LayerNames,"cm_p50_MetersH20.tif"))
RasterNames = c(RasterNames,paste0("FieldCapacity_",LayerNames,"cm_p50_Meter3PerMeter3.tif"))
RasterNames = c(RasterNames,paste0("WiltingPoint_",LayerNames,"cm_p50_Meter3PerMeter3.tif"))
RasterNames = c(RasterNames,paste0("BulkDensity_",LayerNames,"cm_p50_KilogramsPerMeter3.tif"))
RasterNames = c(RasterNames,paste0("VerticalKsat_",LayerNames,"cm_p50_MetersPerSecond.tif"))
RasterNames = c(RasterNames,paste0("ThermalConductivity_",LayerNames,"cm_p50_WattsPerMeterKelvin.tif"))
RasterNames = c(RasterNames,paste0("ThermalCapacity_",LayerNames,"cm_p50_JoulesPerMeter3Kelvin.tif"))
RasterNames = c(RasterNames,paste0("ResidualWaterContent_",LayerNames,"cm_p50_Meter3PerMeter3.tif"))

# Read all of the required rasters
lookdir = "_ByLayer_"
Rasters = lapply(paste0(lookdir,"/",RasterNames), raster)

# For lookup keys that have a single pre-defined global value
ValueParamNames = c("DEPTHTHRESH","ALBEDO","NLAYERS")
ValueParams = c("2.0","0.1",paste0(nSoilLayers))

# Create dataframe to save stats in an easy-to-access format
SoilStats = data.frame(matrix(nrow=nSoilClasses,ncol=length(RasterParamNames)+2))
names(SoilStats) = c("SoilClass","AreaFrac",RasterParamNames)

for (i in 1:nSoilClasses){
  # Associate the spatial distribution of a particular texture class with all other rasters
  ClassPtrs = (ClassRaster[,] == i)
  Ptrs = as.logical(MaskPtrs * ClassPtrs)
  SoilStats[i,1] = ClassNames[i]
  
  # Calculate fractional area of the study area covered by this soil class
  AreaFrac = signif(sum(as.numeric(Ptrs))/sum(Mask[,]),digits=3)
  SoilStats[i,2] = AreaFrac
  
  print(paste0("**************** Working on ",ClassNames[i],", Area Frac. ",AreaFrac," ****************"))
  
  # Visualize the spatial extent of the current class
  CurrentMap = Mask
  CurrentMap[Ptrs] = 2
  plot(CurrentMap)
  
  # Set up block for this texture class
  ClassBlock = ConfigTemp
  ClassBlock = gsub(pattern="NUM", replace=as.character(i), x=ClassBlock)
  ClassBlock = gsub(pattern="TEXTURECLASS", replace=ClassNames[i], x=ClassBlock)
  
  # Replace placeholder names with single parameter values
  for (j in 1:length(ValueParamNames)){
    ClassBlock = gsub(pattern=ValueParamNames[j], replace=ValueParams[j], x=ClassBlock)
  }
  
  # Replace placeholder names with aggregated raster stats for each parameter
  for (j in 1:length(RasterParamNames)){
    RasterParamValues = Rasters[[j]][Ptrs]
    RasterStat = signif(median(RasterParamValues[!is.na(RasterParamValues)]),digits=3)
    SoilStats[i,(j+2)] = RasterStat
    ClassBlock = gsub(pattern=RasterParamNames[j], replace=format(RasterStat, scientific=FALSE), x=ClassBlock)
    print(paste0(RasterParamNames[j],": ",RasterStat))
  }
  
  # Append to config file
  SoilConfig = c(SoilConfig,"",ClassBlock)
  
  print(paste0("**************** Done with ",ClassNames[i]," ****************"))
}

# Write base-line soil config section using median values
writeLines(SoilConfig, "SoilSection_POLARISmedian_TCSI.txt")
write.csv(SoilStats,"SoilStats_POLARISmedian_TCSI.csv")

plot(SoilStats$AreaFrac,type="l")
plot(log10(SoilStats$SLHC),type="l")
plot(SoilStats$EXPDEC,type="l")
plot(SoilStats$L3DENSITY,type="l")

################################################################################
# Calculate median full-domain field capacity to estimate vegetation moisture threshold

# MOISTURE THRESHOLD: soil moisture threshold above which soil moisture does not
# restrict transpiration for each vegetation layer (0-1)
# This is theta-* in equation 16 of Wigmosta et. al. (1994). Maidment (1993) estimates this, referred to
# as theta-d, as 50 to 80 percent of the field capacity.
# Reference: Maidment (1993)
# I finally found this original reference: Figure 4.4.3 of Maidment's Handbook of Hydrology

# So  we'll use 65% of the median field capacity

FieldCapRasters = lapply(c(paste0(lookdir,"/","FieldCapacity_",LayerNames,"cm_p50_Meter3PerMeter3.tif")),raster)

# Aggregate 3 DHSVM soil layers (based on how the POLARIS layers were combined) into one average value
LayerDepthWeights = c(15-0,60-15,200-60)/200
FieldCapAvg = 0*Mask
for (i in 1:nSoilLayers){
  FieldCapAvg = FieldCapAvg + FieldCapRasters[[i]] * LayerDepthWeights[i]
}

plot(FieldCapAvg)
hist(FieldCapAvg[,])

# Calculate moisture threshold
MoistureThreshMin = 0.5 * median(FieldCapAvg[is.finite(FieldCapAvg[,])])
MoistureThreshMid = 0.65 * median(FieldCapAvg[is.finite(FieldCapAvg[,])])
MoistureThreshMax = 0.8 * median(FieldCapAvg[is.finite(FieldCapAvg[,])])

print(paste0("Moisture Threshold Min Value: ",round(MoistureThreshMin,2)))
print(paste0("Moisture Threshold Mid Value: ",round(MoistureThreshMid,2)))
print(paste0("Moisture Threshold Max Value: ",round(MoistureThreshMax,2)))

################################################################################
# Create surface Ksat map in correct format
Ksat = raster(paste0(lookdir,"/","KsatSurface_0-5cm_p50_MetersPerSec.tif"))
KsatOriginal = Ksat

# Read lookup-table data
SoilStats = read.csv("SoilStats_POLARISmedian_TCSI.csv")

# Replace nulls with median value by class
for (cls in 1:nSoilClasses){
  Ptrs = is.na(Ksat[,]) & (ClassRaster[,] == cls)
  Ksat[Ptrs] = SoilStats$SLHC[cls]
}

ChangedCells = sum(as.numeric(is.na(KsatOriginal[,])))
ChangedCells = ChangedCells / length(Ksat[,])
print(paste0(round(100*ChangedCells,3),"%"))

# Convert format for DHSVM: m/s --> mm/s
Ksat = Ksat*1000

writeRaster(Ksat,"KsatSurface_MillimetersPerSec.asc",overwrite=TRUE)

# Convert to the DHSVM header-less format
TextRaster = readLines("KsatSurface_MillimetersPerSec.asc")
writeLines(TextRaster[(nHeaderLines+1):(nHeaderLines+nRows)],"soil_ksat.txt")
file.remove("KsatSurface_MillimetersPerSec.asc")

################################################################################
# Create soil type map in correct format

plot(ClassRaster)
hist(ClassRaster[,],breaks=seq(0.5,10,by=1))

writeRaster(ClassRaster,"soil.asc",datatype="INT2U",overwrite=TRUE) # Must write as integers to prevent adding decimal to first value

# Convert to the DHSVM header-less format
TextRaster = readLines("soil.asc")
writeLines(TextRaster[(nHeaderLines+1):(nHeaderLines+nRows)],"soil.txt")
file.remove("soil.asc")

################################################################################
# Create prior ranges for calibrated non-map variable(s)
# Currently only exponential decrease, as other calibrated soil variables are map-based
# Would need to edit script to work for more variables

######## IMPORTANT #########
# Note that since we're only calibrating the exponential decrease, which is a very sensitive derived parameter,
# We use the 50th percentile MAP only, calculating the ranges from the stats of each class WITHIN ONE MAP

library(raster)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/POLARIS/"
setwd(dir)

# Mask for the study area to get the most-relevant local values
Mask = raster("TCSIstatsMask.tif") # Merged Truckee, Yuba, Bear, American minus NLCD water area
MaskPtrs = (Mask[,] == 1)

# Textural soil classes for DHSVM lookup-table
nSoilClasses = 9
ClassNames = c("Clay","Sandy Clay","Clay Loam","Sandy Clay Loam","Loam","Sandy Loam","Loamy Sand","Sand","Silty Soil")
ClassRaster = raster("POLARIS_SoilTextureClassification.tif")
plot(ClassRaster*Mask)

# For ease of creating many different lookup keys
nSoilLayers = 3
LayerNames = c("0-15","15-60","60-200")

# Lookup keys associated with raster data
RasterParamNames = c("EXPDEC")

# Raster files from which statistics will be derived
# Must be in same order as RasterParamNames!
RasterNames = c("KsatExpDecreaseWithDepth_5-200cm_p50_Unitless.tif")

# Read all of the required rasters
lookdir = "_ByLayer_"
Rasters = lapply(paste0(lookdir,"/",RasterNames), raster)

# Create dataframe to save stats in an easy-to-access format
SoilStats = data.frame(matrix(nrow=nSoilClasses,ncol=3*length(RasterParamNames)+1))
names(SoilStats) = c("SoilClass",paste0(RasterParamNames,c("p5","p50","p95")))

for (i in 1:nSoilClasses){
  # Associate the spatial distribution of a particular texture class with all other rasters
  ClassPtrs = (ClassRaster[,] == i)
  Ptrs = as.logical(MaskPtrs * ClassPtrs)
  SoilStats[i,1] = ClassNames[i]
  
  # Replace placeholder names with aggregated raster stats for each parameter
  for (j in 1:length(RasterParamNames)){
    # Find cells in current class
    RasterParamValues = Rasters[[j]][Ptrs]
    
    # Calculate number of cells to determine which cell to choose (based on percentile of the sorted cells)
    nCells = length(RasterParamValues[!is.na(RasterParamValues)])
    
    # Pull out 5th percentile
    RasterStat5th = signif(sort(RasterParamValues[!is.na(RasterParamValues)])[round(0.05*nCells)],digits=3)
    # Pull out 50th percentile
    RasterStat50th = signif(sort(RasterParamValues[!is.na(RasterParamValues)])[round(0.5*nCells)],digits=3)
    # Pull out 95th percentile
    RasterStat95th = signif(sort(RasterParamValues[!is.na(RasterParamValues)])[round(0.95*nCells)],digits=3)
    
    SoilStats[i,(j+1)] = RasterStat5th
    SoilStats[i,(j+2)] = RasterStat50th
    SoilStats[i,(j+3)] = RasterStat95th
    
  }
  
  print(paste0("**************** Done with ",ClassNames[i]," ****************"))
}

print(SoilStats)

# Write base-line soil config section using median values
write.csv(SoilStats,"SoilStats_EXPDECrange_TCSI.csv")









