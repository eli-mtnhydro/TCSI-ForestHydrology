### MOBOhydro CALIBRATOR FOR DHSVM ###
# Session 2: Create config files and maps with varied calibration parameters given a design grid on [0,1]

################################################################################
# Universal adjustable settings
################################################################################

ModelGen = 6

# For looping over config files
BasinNames = c("Truckee","Yuba")

dir = "C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/Calibrate_MOBOhydro/"
setwd(dir)

# Create dataframe of limits for linear scaling of simple non-POLARIS parameters
# Units: meters soil depth, % of baseline, max degrees C for snowfall, unitless decay lambda for melt-season albedo
SimpleParamNames = c("SOILD","MINRES","SNOWTEMP","ALBMELTRATE")
SimpleParamLower = c(1.3, 0.7, 0, 0.7)
SimpleParamUpper = c(4, 1.3, 6.5, 0.99)
SimpleParamLimits = data.frame(ParamName=SimpleParamNames, LowerBound=SimpleParamLower, UpperBound=SimpleParamUpper)
print(SimpleParamLimits)

###############################################################################
# Get matrix of new designs
################################################################################

if (ModelGen > 1){
  # Read both current and last generation designs so we only make config files/maps for the new parameter sets
  design.grid.lastgen = read.csv(paste0("Generation",ModelGen-1,"_DesignGrid.csv"))[,-1]
  design.grid.thisgen = read.csv(paste0("Generation",ModelGen,"_DesignGrid.csv"))[,-1]
  
  nDesignsLastGen = dim(design.grid.lastgen)[1]
  nDesignsThisGen = dim(design.grid.thisgen)[1]
  
  # Subset novel designs
  design.grid = design.grid.thisgen[(nDesignsLastGen+1):(nDesignsThisGen),]
} else {
  # Read the first generation design grid
  design.grid = read.csv(paste0("Generation",ModelGen,"_DesignGrid.csv"))[,-1]
}

nNewDesigns = length(design.grid[,1])
print(nNewDesigns)

################################################################################
# Make map inputs for map-based calibration parameters
################################################################################

library(raster)

MapDir = "InputMaps/" # Where the outputs from this section will go

# For outputting DHSVM-style header-less ascii rasters
nRows = 1436 # Number of rows in the domain
nHeaderLines = 6 # Number of rows to delete from a .asc raster header

########################### PARAM 1: SOIL DEPTH ################################
# Code based on MakeCurvatureSoilDepthSimple.R

MinDeepLayer = 0.01

SoilDepthDir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/SoilDepth/CurvatureBased/"

# Raster of 30 m ArcGIS curvature
CurvatureMap = raster(paste0(SoilDepthDir,"TCSIdomain_30mNonFilledDEM_ArcGIScurvature.tif"))

CurveSlope = 37.90 # Table 2 from https://www.nature.com/articles/s41467-018-05743-y

# Scale factors for soil depth offset (used to map 1-->max depth, 0-->min depth)
MinOffset = SimpleParamLimits$LowerBound[SimpleParamLimits$ParamName=="SOILD"]
MaxOffset = SimpleParamLimits$UpperBound[SimpleParamLimits$ParamName=="SOILD"]

# To fix minimum rooting depths and force shallow soil in bare areas
MinSoilDepth = raster(paste0(SoilDepthDir,"MinSoilDepth.tif"))
VegClass = raster(paste0(SoilDepthDir,"../../LANDIS/TCSI_Year0_VegID.tif"))
DEM = raster(paste0(SoilDepthDir,"../../SRTM/TCSIdomain_90mDEMfilled.tif"))

# Bare rock --> minimum soil (approximated by NLCD barren land or DHSVM class 32)
# Alpine shrub --> minimum soil (approximated by NLCD grass/crop/shrub already masked above 2500 m)
# Grassland --> minimum soil (NLCD grass/crop/shrub in high-elevation area is mostly alpine rock; DHSVM class 33)
BareRockPtrs = which(VegClass[,] == 32 | VegClass[,] == 35 | (VegClass[,] == 33 & DEM[,] > 2000))

# Calculate the constant pattern that will be added to the calibrated offset
SoilDepth30mPattern = CurveSlope * (CurvatureMap/-100)
SoilDepthPattern = aggregate(SoilDepth30mPattern,3,fun=mean) # 30 m to 90 m, relies on 30 m map having same CRS/extent as model domain

# hist(SoilDepthPattern)

timeTotal = 0

for (i in 1:nNewDesigns){
  timeStart = Sys.time()
  
  # Add calibrated offset to soil depth pattern
  CurveOffset = design.grid$SOILD[i] * (MaxOffset - MinOffset) + MinOffset
  SoilDepth = SoilDepthPattern + CurveOffset
  
  # Fix minimum depths
  TooShallowPtrs = which(SoilDepth[,] < (MinSoilDepth[,] + MinDeepLayer))
  SoilDepth[TooShallowPtrs] = MinSoilDepth[TooShallowPtrs] + MinDeepLayer
  
  # Force bare regions to minimum depth
  SoilDepth[BareRockPtrs] = MinSoilDepth[BareRockPtrs] + MinDeepLayer
  
  # Round to maximum floating-point precision in C
  SoilDepth = round(SoilDepth,7)
  
  # Output as header-less DHSVM input format
  writeRaster(SoilDepth,"temp.asc",overwrite=TRUE)
  TextRaster = readLines("temp.asc")
  writeLines(TextRaster[(nHeaderLines+1):(nHeaderLines+nRows)],paste0(MapDir,"Params",design.grid$ParamSet[i],"_soild.txt"))
  
  timeEnd = Sys.time()
  timeElapsed = as.numeric(difftime(timeEnd,timeStart,units="secs"))
  timeTotal = timeTotal + timeElapsed
  timeAvg = timeTotal/i
  print("*")
  print(paste0("Soil depth processing took ",round(timeElapsed/60,1)," minutes."))
  print(paste0("Full evaluation expected to take another ",round(timeAvg*(nNewDesigns-i)/60)," minutes for ",nNewDesigns-i," more parameter sets."))
  
  print(paste0("Done with Params ",design.grid$ParamSet[i]," (",round(100*i/nNewDesigns,1),"%)"))
}

# plot(SoilDepth)
# hist(SoilDepth[,])

#################### PARAM 2: HYDRAULIC CONDUCTIVITY ###########################
# Code based partially on POLARIS scripts, partially developed here

PolarisDir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/POLARIS/"

# Read surface Ksat at different probability levels
Ksat5thMperS = raster(paste0(PolarisDir,"_ByLayer_/KsatSurface_0-5cm_p5_MetersPerSec.tif"))
Ksat50thMperS = raster(paste0(PolarisDir,"_ByLayer_/KsatSurface_0-5cm_p50_MetersPerSec.tif"))
Ksat95thMperS = raster(paste0(PolarisDir,"_ByLayer_/KsatSurface_0-5cm_p95_MetersPerSec.tif"))

# We'll work in log10 space so we can have a more meaningful sampling strategy
Ksat5th = log10(Ksat5thMperS)
Ksat50th = log10(Ksat50thMperS)
Ksat95th = log10(Ksat95thMperS)

# Fill NAs with median value by class
ClassRaster = raster(paste0(PolarisDir,"POLARIS_SoilTextureClassification.tif"))
SoilClasses = sort(unique(ClassRaster[is.finite(ClassRaster[,])]))
print(paste0("Replacing non-finite fraction: ",sum(as.numeric(!is.finite(Ksat50th[,])))/length(Ksat50th[,])))
for (cls in SoilClasses){
  Ptrs = !is.finite(Ksat5th[,]) & (ClassRaster[,] == cls)
  MedianPtrs = is.finite(Ksat5th[,]) & (ClassRaster[,] == cls)
  Ksat5th[Ptrs] = median(Ksat5th[MedianPtrs])
  
  Ptrs = !is.finite(Ksat50th[,]) & (ClassRaster[,] == cls)
  MedianPtrs = is.finite(Ksat50th[,]) & (ClassRaster[,] == cls)
  Ksat50th[Ptrs] = median(Ksat50th[MedianPtrs])
  
  Ptrs = !is.finite(Ksat95th[,]) & (ClassRaster[,] == cls)
  MedianPtrs = is.finite(Ksat95th[,]) & (ClassRaster[,] == cls)
  Ksat95th[Ptrs] = median(Ksat95th[MedianPtrs])
}

# Plot input Ksat maps (remember they're in log10 space now)
# par(mfrow=c(3,1))
# plot(Ksat5th,zlim=c(-8,-3))
# plot(Ksat50th,zlim=c(-8,-3))
# plot(Ksat95th,zlim=c(-8,-3))

# Compare the differences that will be used as prior ranges
# par(mfrow=c(2,1))
# plot(Ksat50th-Ksat5th)
# plot(Ksat95th-Ksat50th)

# Justify log10 sampling instead of regular sampling
# MperSmedians = c(median(Ksat5thMperS[is.finite(Ksat5thMperS[,])]),median(Ksat50thMperS[is.finite(Ksat50thMperS[,])]),median(Ksat95thMperS[is.finite(Ksat95thMperS[,])]))
# Log10medians = c(median(Ksat5th[,]),median(Ksat50th[,]),median(Ksat95th[,]))
# print(diff(MperSmedians))
# print(diff(Log10medians))

timeTotal = 0

for (i in 1:nNewDesigns){
  timeStart = Sys.time()
  
  # Uniform sampling from 5th-50th and 50th-95th percentile
  if (design.grid$SLHC[i] < 0.5){
    Ksat = design.grid$SLHC[i] * (Ksat50th - Ksat5th) + Ksat5th
  } else {
    Ksat = design.grid$SLHC[i] * (Ksat95th - Ksat50th) + Ksat50th
  }
  
  Ksat = 10^Ksat # Convert log10 m/s to m/s
  Ksat = Ksat*1000 # Convert format for DHSVM: m/s --> mm/s
  
  # Round to maximum floating-point precision in C
  Ksat = round(Ksat,7)
  
  # Output as header-less DHSVM input format
  writeRaster(Ksat,"temp.asc",overwrite=TRUE)
  TextRaster = readLines("temp.asc")
  writeLines(TextRaster[(nHeaderLines+1):(nHeaderLines+nRows)],paste0(MapDir,"Params",design.grid$ParamSet[i],"_soil_ksat.txt"))
  
  timeEnd = Sys.time()
  timeElapsed = as.numeric(difftime(timeEnd,timeStart,units="secs"))
  timeTotal = timeTotal + timeElapsed
  timeAvg = timeTotal/i
  print("*")
  print(paste0("Ksat processing took ",round(timeElapsed/60,1)," minutes."))
  print(paste0("Full evaluation expected to take another ",round(timeAvg*(nNewDesigns-i)/60)," minutes for ",nNewDesigns-i," more parameter sets."))
  
  print(paste0("Done with Params ",design.grid$ParamSet[i]," (",round(100*i/nNewDesigns,1),"%)"))
}

# par(mfrow=c(1,1))
# plot(Ksat)
# hist(Ksat[,])

########################### PARAM 3: POROSITY ##################################
# Code based partially on POLARIS scripts, mostly developed here

PolarisDir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/POLARIS/"

# Set up depth layers
DepthLayers = c("0-15","15-60","60-200")

# Read porosity at different probability levels for each depth layer
Porosity5th = lapply(paste0(PolarisDir,"/_ByLayer_/Porosity_",DepthLayers,"cm_p5_Meter3PerMeter3.tif"), raster)
Porosity50th = lapply(paste0(PolarisDir,"/_ByLayer_/Porosity_",DepthLayers,"cm_p50_Meter3PerMeter3.tif"), raster)
Porosity95th = lapply(paste0(PolarisDir,"/_ByLayer_/Porosity_",DepthLayers,"cm_p95_Meter3PerMeter3.tif"), raster)

# DHSVM throws an error when !(porosity > field capacity > wilting point) for each pixel
# This arises because the porosity and field capacity are map-based and highly variable
# But the wilting point is a single (median) value for each soil class
# Here, we adjust the field capacity of each cell to be strictly greater than the lookup-table wilting point for that soil class
# And we adjust the porosity of each grid cell to be strictly greater than the field capacity of each cell
# Based on RegularizePOLARIS_Porosity-FieldCap-WiltPoint.R

# Minimum amount (fractional volume) to make porosity larger than field capacity
MinDeltaPFC = 0.1

# Read field capacity rasters (IMPORTANT: must be pre-regularized with wilting point by class)
FieldCapacity = lapply(paste0(PolarisDir,"FieldCap_Layer",1:length(DepthLayers),".asc"), raster)

# Fill NAs with median value by class
ClassRaster = raster(paste0(PolarisDir,"POLARIS_SoilTextureClassification.tif"))
SoilClasses = sort(unique(ClassRaster[is.finite(ClassRaster[,])]))
print(paste0("Replacing non-finite fraction: ",sum(as.numeric(!is.finite(Porosity50th[[1]][,])))/length(Porosity50th[[1]][,])))
for (cls in SoilClasses){
  for (i in 1:length(DepthLayers)){
    Ptrs = !is.finite(Porosity5th[[i]][,]) & (ClassRaster[,] == cls)
    MedianPtrs = is.finite(Porosity5th[[i]][,]) & (ClassRaster[,] == cls)
    Porosity5th[[i]][Ptrs] = median(Porosity5th[[i]][MedianPtrs])
    
    Ptrs = !is.finite(Porosity50th[[i]][,]) & (ClassRaster[,] == cls)
    MedianPtrs = is.finite(Porosity50th[[i]][,]) & (ClassRaster[,] == cls)
    Porosity50th[[i]][Ptrs] = median(Porosity50th[[i]][MedianPtrs])
    
    Ptrs = !is.finite(Porosity95th[[i]][,]) & (ClassRaster[,] == cls)
    MedianPtrs = is.finite(Porosity95th[[i]][,]) & (ClassRaster[,] == cls)
    Porosity95th[[i]][Ptrs] = median(Porosity95th[[i]][MedianPtrs])
  }
}

# Plot input porosity maps
# par(mfrow=c(3,1))
# for (i in 1:length(DepthLayers)){
#   plot(Porosity5th[[i]],zlim=c(0.1,0.6))
#   plot(Porosity50th[[i]],zlim=c(0.1,0.6))
#   plot(Porosity95th[[i]],zlim=c(0.1,0.6))
# }

# Compare the differences that will be used as prior ranges
# par(mfrow=c(2,1))
# for (i in 1:length(DepthLayers)){
#   plot(Porosity50th[[i]]-Porosity5th[[i]])
#   plot(Porosity95th[[i]]-Porosity50th[[i]])
# }

timeTotal = 0

for (i in 1:nNewDesigns){
  timeStart = Sys.time()
  
  for (lyr in 1:length(DepthLayers)){
    # Uniform sampling from 5th-50th and 50th-95th percentile
    if (design.grid$POROSITY[i] < 0.5){
      Porosity = design.grid$POROSITY[i] * (Porosity50th[[lyr]] - Porosity5th[[lyr]]) + Porosity5th[[lyr]]
    } else {
      Porosity = design.grid$POROSITY[i] * (Porosity95th[[lyr]] - Porosity50th[[lyr]]) + Porosity50th[[lyr]]
    }
    
    # Increase porosity minimum amount above field capacity
    ProblemPtrs = which(Porosity[,] < (FieldCapacity[[lyr]][,] + MinDeltaPFC))
    Porosity[ProblemPtrs] = FieldCapacity[[lyr]][ProblemPtrs] + MinDeltaPFC
    # Print diagnostics
    print(paste0("Replacing problem fraction: ",length(ProblemPtrs)/length(Porosity[,])))
    if (length(ProblemPtrs)>0){print(paste0("Avg. porosity of replaced cells: ",mean(Porosity[ProblemPtrs])))}
    
    # Round to maximum floating-point precision in C
    Porosity = round(Porosity,7)
    
    writeRaster(Porosity,paste0("temp",lyr,".asc"),overwrite=TRUE)
  }
  
  # Concatenate layers and output as header-less DHSVM input format
  for (lyr in 1:length(DepthLayers)){
    TextRaster = readLines(paste0("temp",lyr,".asc"))
    if (lyr == 1){
      MergedRaster = TextRaster[(nHeaderLines+1):(nHeaderLines+nRows)]
    } else {
      MergedRaster = c(MergedRaster,TextRaster[(nHeaderLines+1):(nHeaderLines+nRows)])
    }
  }
  writeLines(MergedRaster,paste0(MapDir,"Params",design.grid$ParamSet[i],"_soil_porosity.txt"))
  
  timeEnd = Sys.time()
  timeElapsed = as.numeric(difftime(timeEnd,timeStart,units="secs"))
  timeTotal = timeTotal + timeElapsed
  timeAvg = timeTotal/i
  print("*")
  print(paste0("Porosity processing took ",round(timeElapsed/60,1)," minutes."))
  print(paste0("Full evaluation expected to take another ",round(timeAvg*(nNewDesigns-i)/60)," minutes for ",nNewDesigns-i," more parameter sets."))
  
  print(paste0("Done with Params ",design.grid$ParamSet[i]," (",round(100*i/nNewDesigns,1),"%)"))
}

# par(mfrow=c(1,1))
# plot(Porosity)
# hist(Porosity[,])

################################################################################
# Make dataframes of class-valued parameters for new designs
################################################################################

############################ PARAM 4: EXPDEC ###################################
# Code uses dataframe from ProcessPOLARIS_SoilData.R, otherwise developed here

PolarisDir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/POLARIS/"

EXPDECrange = read.csv(paste0(PolarisDir,"SoilStats_EXPDECrange_TCSI.csv"))
print(EXPDECrange)

nSoilClasses = length(EXPDECrange$SoilClass)

# Set up dataframe to store the value of each parameter set for each class
EXPDECsamples = as.data.frame(matrix(rep(design.grid$ParamSet,(1+nSoilClasses)),ncol=(1+nSoilClasses)))
names(EXPDECsamples) = c("ParamSet",paste0("SoilClass",1:nSoilClasses))

for (i in 1:nNewDesigns){
  for (cls in 1:nSoilClasses){
    # Uniform sampling from 5th-95th percentile since stats are noisy and somewhat bimodal
    EXPDECsamples[i,cls+1] = design.grid$EXPDEC[i] * (EXPDECrange$EXPDECp95[cls] - EXPDECrange$EXPDECp5[cls]) + EXPDECrange$EXPDECp5[cls]
  }
  print(paste0("Done with Params ",design.grid$ParamSet[i]," (",round(100*i/nNewDesigns,1),"%)"))
}

# head(EXPDECsamples)

par(mfrow=c(3,3))
for (cls in 1:nSoilClasses){
  hist(EXPDECsamples[,cls+1],main=EXPDECrange$SoilClass[cls])
}
par(mfrow=c(1,1))

############################ PARAM 5: MINRES ###################################
# Code uses dataframe from ProcessLANDISforDHSVM.R, otherwise developed here

LandisDir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/LANDIS/"

MINRESrange = read.csv(paste0(LandisDir,"MINRESbaseline_TCSI.csv"))
print(MINRESrange)

nVegClasses = length(MINRESrange$VegClass)

# Set up dataframe to store the value of each parameter set for each class
MINRESsamples = as.data.frame(matrix(rep(design.grid$ParamSet,(1+nVegClasses)),ncol=(1+nVegClasses)))
names(MINRESsamples) = c("ParamSet",paste0("VegClass",MINRESrange$VegClass))

for (i in 1:nNewDesigns){
  for (cls in 1:nVegClasses){
    # Linear sampling +/- 30%
    MinVal = SimpleParamLimits$LowerBound[SimpleParamLimits$ParamName=="MINRES"]
    MaxVal = SimpleParamLimits$UpperBound[SimpleParamLimits$ParamName=="MINRES"]
    ParamScale = design.grid$MINRES[i] * (MaxVal - MinVal) + MinVal # Must query design from correct column!
    MINRESsamples[i,cls+1] = MINRESrange$MINRES[cls] * ParamScale
    
    # Geometric sampling with a base of +/- 30% (MINRESbase set at top of script)
    # Amounts to multiplying or dividing by a scale factor of up to 1.3
    #MINRESexponent = (2 * design.grid$MINRES[i]) - 1 # Scale [0,1] --> [-1,1]
    #MINRESsamples[i,cls+1] = MINRESrange$MINRES[cls] * (MINRESbase ^ MINRESexponent)
  }
  print(paste0("Done with Params ",design.grid$ParamSet[i]," (",round(100*i/nNewDesigns,1),"%)"))
}

#ConiferClasses = c(7,9,11,13,15,19,21,23)

# head(MINRESsamples)

par(mfrow=c(3,3))
for (cls in 1:nVegClasses){
  hist(MINRESsamples[,cls+1],main=MINRESrange$VegClass[cls])
}
par(mfrow=c(1,1))

################################################################################
# Sample simple parameters and write new set of config files for each basin
################################################################################

ConfigDir = "ConfigFiles/" # Where the outputs from this section will go

# MetCount = 1
# MetFolder = 1

# Loop this entire section over each watershed
for (basin in 1:length(BasinNames)){
  
  # Read template config file
  ConfigTemp = readLines(paste0("Config_",BasinNames[basin],"_Template.txt"))
  
  # Loop over each new parameter set
  for (i in 1:nNewDesigns){
    # Replace placeholder names with parameter values
    ConfigParam = ConfigTemp
    
    ######################### PARAM SET ########################################
    # Puts the correct prefix on map filenames (SOILD, KSAT, POROSITY) and sets output directory
    
    ConfigParam = gsub(pattern="PARAMSNUM", replace=paste0("Params",as.character(design.grid$ParamSet[i])), x=ConfigParam)
    
    # Possible option to read from different folders if desired
    # ConfigParam = gsub(pattern="staticinput", replace=paste0("staticinput",as.character(MetFolder)), x=ConfigParam)
    # ConfigParam = gsub(pattern="gridforcing", replace=paste0("gridforcing",as.character(MetFolder)), x=ConfigParam)
    # if (!(MetCount %% 4)){
    #   MetFolder = MetFolder + 1
    # }
    # MetCount = MetCount + 1
    
    ######################### PARAM 4: EXPDEC ##################################
    # Loop through each class and fill from the dataframe created above
    # Dataframe is only stored locally, must have previously been created above
    
    for (cls in 1:nSoilClasses){
      ConfigParam = gsub(pattern=paste0("EXPDEC",cls), replace=as.character(round(EXPDECsamples[i,(cls+1)],3)), x=ConfigParam)
    }
    
    ######################### PARAM 5: MINRES ##################################
    # Loop through each class and fill from the dataframe created above
    # Dataframe is only stored locally, must have previously been created above
    
    # Need to include period after the number to prevent gsub from overwriting, e.g., MINRES28 with MINRES2
    for (cls in 1:nVegClasses){
      ConfigParam = gsub(pattern=paste0("MINRES",MINRESrange$VegClass[cls],"."), replace=as.character(round(MINRESsamples[i,(cls+1)],0)), x=ConfigParam, fixed=TRUE)
    }
    
    ######################### PARAM 6: SNOWTEMP ################################
    MinVal = SimpleParamLimits$LowerBound[SimpleParamLimits$ParamName=="SNOWTEMP"]
    MaxVal = SimpleParamLimits$UpperBound[SimpleParamLimits$ParamName=="SNOWTEMP"]
    ParamVal = design.grid$SNOWTEMP[i] * (MaxVal - MinVal) + MinVal # Must query design from correct column!
    ConfigParam = gsub(pattern="SNOWTEMP", replace=as.character(round(ParamVal,2)), x=ConfigParam)
    
    ######################### PARAM 7: ALBMELTRATE #############################
    MinVal = SimpleParamLimits$LowerBound[SimpleParamLimits$ParamName=="ALBMELTRATE"]
    MaxVal = SimpleParamLimits$UpperBound[SimpleParamLimits$ParamName=="ALBMELTRATE"]
    ParamVal = design.grid$ALBMELTRATE[i] * (MaxVal - MinVal) + MinVal # Must query design from correct column!
    ConfigParam = gsub(pattern="ALBMELTRATE", replace=as.character(round(ParamVal,3)), x=ConfigParam)
    
    # Write new config file for this basin and this parameter set
    writeLines(ConfigParam, con=paste0(ConfigDir,"Config_Params",design.grid$ParamSet[i],"_",BasinNames[basin],".txt"))
    
    print(paste0("Done with ",BasinNames[basin]," Params ",design.grid$ParamSet[i]," (",round((100*i/nNewDesigns)/length(BasinNames),1),"%)"))
  }
  print(paste0("****************************** ",BasinNames[basin]," Finished ******************************"))
}

# Clean up temporary ascii rasters
file.remove("temp.asc")
for (lyr in 1:length(DepthLayers)){file.remove(paste0("temp",lyr,".asc"))}
