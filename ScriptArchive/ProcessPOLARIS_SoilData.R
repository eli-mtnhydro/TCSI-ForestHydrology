# https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2018WR022797
# http://hydrology.cee.duke.edu/POLARIS/

# DHSVM soil inputs
# Soil Description       1 = Clay                                               # From POLARIS sand/silt/clay classification map
# Lateral Conductivity   1 = 1.67e-7 # (m/s)                                    # From POLARIS ksat map
# Exponential Decrease   1 = 0.03 # Exponent for decrease in Ksat with depth    # From differences in POLARIS ksat by depth
# Depth Threshold        1 = 1.5                                                # ?
# Maximum Infiltration   1 = 1.67e-7 # (m/s)                                    # Set to POLARIS surface ksat by class?
# Capillary Drive        1 = 0.05                                               # ?
# Surface Albedo         1 = 0.1                                                # Set to default?
# Number of Soil Layers  1 = 3                                                  # Set to 3 by default
# Porosity               1 = 0.475 0.475 0.475                                  # From POLARIS theta_s
# Pore Size Distribution 1 = 0.165 0.165 0.165                                  # From POLARIS lambda (Brooks-Corey)
# Bubbling Pressure      1 = .856 .856 .856                                     # From POLARIS hb
# Field Capacity         1 = .396 .396 .396 # (water retained at 0.33 bar)      # From POLARIS using Van-Genutchen eq.
# Wilting Point          1 = .272 .272 .272 # (water retained at 15 bar)        # From POLARIS using Van-Genutchen eq.
# Bulk Density           1 = 1540 1590 1740 # (kg/m3)                           # From POLARIS bd
# Vertical Conductivity  1 = 1.67e-7 1.67e-7 1.67e-7 # (m/s)                    # Set to POLARIS geometric mean ksat by class?
# Thermal Conductivity   1 = 7.70 7.49 7.46    # (W/(m*K))                      # From POLARIS bd using empirical eq.
# Thermal Capacity       1 = 1.4e6 1.4e6 1.4e6 # (J/(m3*K))                     # From POLARIS bd using empirical eq.
# Residual Water Content 1 = 0.090 0.090 0.090                                  # From POLARIS theta_r

# Root zone depths: 0.2 0.4 0.6 (m)
# DHSVM --> 0-20, 20-60, 60-120 (cm)
# POLARIS --> 0-5 and 5-15, 15-30 and 30-60, 60-100 and 100-200 (cm)

library(raster)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/POLARIS/"
setwd(dir)

tilelist = c("lat3839_lon-120-119","lat3839_lon-121-120","lat3839_lon-122-121","lat3940_lon-120-119","lat3940_lon-121-120","lat3940_lon-122-121")
paramlist = c("bd","theta_r","lambda","hb","n","alpha")
problist = c("p50")
depthlist = c("0_5","5_15","15_30","30_60","60_100","100_200")

Mask = raster("../WatershedBoundaries/TCSImask_MergedDomain.tif")

# For aggregating 6 POLARIS layers into 3 DHSVM layers
DepthBreakPoints = c(2,4,6)
DepthLayerWeights = c(c(5-0,15-5)/(15-0),c(30-15,60-30)/(60-15),c(100-60,200-100)/(200-60))
DepthLayers = c("0-15","15-60","60-200")

# For aggregating 6 POLARIS layers into 1 average layer
DepthWeights = c(5-0,15-5,30-15,60-30,100-60,200-100)/200
plot(DepthWeights,type="l")

################################################################################

# Download POLARIS tiles

i = 1
nFiles = length(paramlist)*length(problist)*length(depthlist)*length(tilelist)
for (param in 1:length(paramlist)){
  for (prob in 1:length(problist)){
    for (depth in 1:length(depthlist)){
      for (tile in 1:length(tilelist)){
        
        fileaddr = paste0("http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/",paramlist[param],"/",problist[prob],"/",depthlist[depth],"/",tilelist[tile],".tif")
        filedest = paste0("POLARIS-",paramlist[param],"-",problist[prob],"-",depthlist[depth],"-",tilelist[tile],".tif")
        filefolder = paramlist[param]
        
        download.file(fileaddr,paste0(filefolder,"/",filedest),mode="wb")
        
        print(i/nFiles)
        i = i+1
      }
    }
  }
}

# Merge POLARIS tiles

i = 1
nFiles = length(paramlist)*length(problist)*length(depthlist)
for (param in 1:length(paramlist)){
  for (prob in 1:length(problist)){
    for (depth in 1:length(depthlist)){
      for (tile in 1:length(tilelist)){
        # Gather the tiles
        fileorig = paste0("POLARIS-",paramlist[param],"-",problist[prob],"-",depthlist[depth],"-",tilelist[tile],".tif")
        filefolder = paramlist[param]
        
        if (tile == 1){
          filelist = c(paste0(filefolder,"/",fileorig))
        } else {
          filelist = c(filelist,paste0(filefolder,"/",fileorig))
        }
        
      }
      # Merge the tiles
      filemerged = paste0("POLARIS-",paramlist[param],"-",problist[prob],"-",depthlist[depth],".tif")
      filefolder = paste0(paramlist[param],"/merged")
      
      mergedraster = do.call(merge, lapply(filelist, raster))
      writeRaster(mergedraster,paste0(filefolder,"/",filemerged))
      plot(mergedraster)
      
      print(i/nFiles)
      i = i+1
    }
  }
}

################################################################################

# Make DHSVM-relevant files: soil texture classification

library(soiltexture)

lookdir = c("sand/merged","silt/merged","clay/merged")
problist = c("p50")
propertynames = c("Sand","Silt","Clay")

# Weighted average over all depths (note: introduces some small issues by not necessarily summing to 100%)

for (dir in 1:length(lookdir)){
  for (prob in 1:length(problist)){
    filelist = list.files(lookdir[dir],pattern=paste0("-",problist[prob],"-"))
    rasterlist = lapply(paste0(lookdir[dir],"/",filelist), raster)
    
    DepthAveragedRaster = 0*Mask
    
    # Use weighted arithmetic mean for sand, silt, clay percentages
    
    for (depth in 1:length(depthlist)){
      singlerast = (rasterlist[[depth]] * DepthWeights[depth])
      singlerast = projectRaster(singlerast, Mask, method="ngb")
      DepthAveragedRaster = DepthAveragedRaster + singlerast
      
      print(depth/length(depthlist))
    }
    
    writeRaster(DepthAveragedRaster,paste0(propertynames[dir],"_",problist[prob],"_Percentage.tif"),overwrite=TRUE)
    
    plot(DepthAveragedRaster)
    print(paste0("Done with ",problist[prob]," ",propertynames[dir]))
  }
}

# Classify into soil texture

SandRaster = raster("Sand_p50_Percentage.tif")
SiltRaster = raster("Silt_p50_Percentage.tif")
ClayRaster = raster("Clay_p50_Percentage.tif")

plot(SandRaster+SiltRaster+ClayRaster)

# Create dataframe from raster values
data = data.frame(SandRaster[,],SiltRaster[,],ClayRaster[,])
names(data) = c("SAND", "SILT", "CLAY")

# Fill NAs with average of each category
SandMean = mean(data$SAND[!is.na(data$SAND)])
SiltMean = mean(data$SILT[!is.na(data$SAND)])
ClayMean = mean(data$CLAY[!is.na(data$SAND)])

data$SAND[is.na(data$SAND)] = SandMean
data$SILT[is.na(data$SILT)] = SiltMean
data$CLAY[is.na(data$CLAY)] = ClayMean

# Normalize to 100%
total = data$SAND + data$SILT + data$CLAY
hist(total)
data = 100*data/total

head(data)

# Classify using USDA soil texture ternary diagram
class = soiltexture::TT.points.in.classes(tri.data  = data, class.sys = "USDA-NCSS.TT", PiC.type = 't', tri.sum.tst = FALSE)

# If a point falls on the boundary between classes, only assign the first class
data$CLASS = sapply(strsplit(class, ","), "[", 1)

head(data)
unique(data$CLASS)

data$CLASS[data$CLASS == "C"] = 1 # Clay
data$CLASS[data$CLASS == "SC"] = 2 # Sandy Clay
data$CLASS[data$CLASS == "CL"] = 3 # Clay Loam
data$CLASS[data$CLASS == "SCL"] = 4 # Sandy Clay Loam
data$CLASS[data$CLASS == "L"] = 5 # Loam
data$CLASS[data$CLASS == "SL"] = 6 # Sandy Loam
data$CLASS[data$CLASS == "LS"] = 7 # Loamy Sand
data$CLASS[data$CLASS == "S"] = 8 # Sand
# Combine all 4 silt-related classes into 1 since there are very few silty gridcells
data$CLASS[data$CLASS == "SIC"] = 9 # Silty Clay
data$CLASS[data$CLASS == "SICL"] = 9 # Silty Clay Loam
data$CLASS[data$CLASS == "SIL"] = 9 # Silty Loam
data$CLASS[data$CLASS == "SI"] = 9 # Silt

data$CLASS = as.numeric(data$CLASS)

# If any pixels have NA class, fill with the most common class
fracNAs = sum(as.numeric(is.na(data$CLASS)))/length(data$CLASS)
print(paste0(fracNAs,"% NA classification"))
if (fracNAs > 0){
  Mode = function(x) {
    ux = unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  ClassMode = Mode(data$CLASS)
  print(ClassMode)
  data$CLASS[is.na(data$CLASS)] = ClassMode
  data$CLASS[(data$SAND+data$SILT+data$CLAY) == 0] = Mode(data$CLASS)
}

head(data)
sort(unique(data$CLASS))

# Create raster of numeric soil texture classification

SoilClassRaster = 0*Mask
SoilClassRaster[,] = data$CLASS

plot(SoilClassRaster)

writeRaster(SoilClassRaster,"POLARIS_SoilTextureClassification.tif",overwrite=TRUE)

################################################################################

# Make DHSVM-relevant files: saturated lateral hydraulic conductivity and exponential decrease

lookdir = "ksat/merged"
problist = c("p5","p50","p95")

nSurfaceLayers = 1
surfaceweights = c(5-0)/(5-0)
SurfDepthName = "0-5"
depths = c(5-0,15-5,30-15,60-30,100-60,200-100)/100 # cm --> m
TotalDepth = 2 # m

for (prob in 1:length(problist)){
  filelist = list.files(lookdir,pattern=paste0("-",problist[prob],"-"))
  rasterlist = lapply(paste0(lookdir,"/",filelist), raster)
  
  SurfaceConductivity = 0*Mask
  TotalTransmissivity = 0*Mask
  
  # Calculate total transmissivity when the 0-2 m soil column is full saturated: T = K*d
  for (depth in 1:length(depthlist)){
    # Add layer to the surface if applicable
    if (depth <= nSurfaceLayers){
      singlerast = (10^rasterlist[[depth]])/(100*60*60) # log10(cm/hr) to m/s
      singlerast = projectRaster(singlerast, Mask, method="ngb")
      SurfaceConductivity = SurfaceConductivity + singlerast * surfaceweights[depth]
    }
    
    # Add all layers to the total transmissivity
    singlerast = (10^rasterlist[[depth]])/(100*60*60) # log10(cm/hr) to m/s
    singlerast = projectRaster(singlerast, Mask, method="ngb")
    TotalTransmissivity = TotalTransmissivity + singlerast * depths[depth] # conductivity --> transmissivity

    print(depth/length(depthlist))
  }
  
  plot(SurfaceConductivity)
  plot(TotalTransmissivity)
  
  writeRaster(SurfaceConductivity,paste0("_ByLayer_/","KsatSurface_",SurfDepthName,"cm_",problist[prob],"_MetersPerSec.tif"),overwrite=TRUE)
  #writeRaster(log10(SurfaceConductivity),paste0("_ByLayer_/","KsatSurface_",SurfDepthName,"cm_",problist[prob],"_Log10MetersPerSec.tif"),overwrite=TRUE)
  if (problist[prob]=="p50"){
    writeRaster(TotalTransmissivity,paste0("_ByLayer_/","SaturatedTransmissivity0-2m_",problist[prob],"_Meters2PerSec.tif"),overwrite=TRUE)
  }
  print(paste0("Done with ",problist[prob]))
}

# Calculate exponential decrease factor (equation from Mathematica solver)
library(lamW)
ExpDecrFac = function(K0,Ttot,d){
  # K0 = surface hydraulic conductivity
  # Ttot = total transmissivity when fully saturated
  # d = soil depth (fully saturated in this case)
  # z = depth to water table (zero in this case)
  # Ttot = (K0/f)*(exp(-f*z)-exp(-f*d))
  
  # Solution from Mathematica when z == 0
  f = (d*K0 + Ttot*lambertW0(-d*exp(-d*K0/Ttot)*K0/Ttot))/(d*Ttot)
  return(f)
}

plot(ExpDecrFac(1e-6,seq(0.1,2,0.001)*1e-6,2),seq(0.1,2,0.001)*1e-6,type="l",xlab="Exponential Decrease Factor",ylab="Transmissivity, m^2/s",main="Ksurface = 1e-6 m/s\nDepth = 2 m, Fully Saturated")
plot(ExpDecrFac(1e-6,seq(0.01,2,0.001)*1e-6,2),(seq(0.01,2,0.001)*1e-6)/2e-6,type="l",log="y",xlab="Exponential Decrease Factor",ylab="Fraction of Transmissivity with K = const.",main="Ksurface = 1e-6 m/s\nDepth = 2 m, Fully Saturated")

# Maximum allowed value for f value
fValMax = 100
fValMin = 0

# Whether to use the 0-5 cm or 0-15 cm "surface" conductivity
SurfDepthName = "0-5"
DepthName = "5-200"

# Min f value when K0-->small and Tsat-->large, max f value when K0-->large and Tsat-->small * experimental, not used *
# Exp_problist = c("p5","p50","p95")
# K0_problist = c("p5","p50","p95")
# Tsat_problist = c("p95","p50","p5")
Exp_problist = c("p50")
K0_problist = c("p50")
Tsat_problist = c("p50")

for (prob in 1:length(Exp_problist)){
  SurfaceConductivity = raster(paste0("_ByLayer_/","KsatSurface_",SurfDepthName,"cm_",K0_problist[prob],"_MetersPerSec.tif"))
  TotalTransmissivity = raster(paste0("_ByLayer_/","SaturatedTransmissivity0-2m_",Tsat_problist[prob],"_Meters2PerSec.tif"))
  
  ExpDecreaseFactor = 0*Mask
  ExpDecreaseFactor[,] = ExpDecrFac(SurfaceConductivity[,],TotalTransmissivity[,],rep(TotalDepth,length(Mask[,])))
  ExpDecreaseFactor[ExpDecreaseFactor[,]>fValMax] = fValMax
  ExpDecreaseFactor[ExpDecreaseFactor[,]<fValMin] = fValMin
  
  plot(ExpDecreaseFactor)
  hist(ExpDecreaseFactor[,])
  
  writeRaster(ExpDecreaseFactor,paste0("_ByLayer_/","KsatExpDecreaseWithDepth_",DepthName,"cm_",Exp_problist[prob],"_Unitless.tif"),overwrite=TRUE)
}

################################################################################

# Make DHSVM-relevant files: vertical conductivity and infiltration

# Vertical conductivity: based on vertical/lateral anisotropy ratio
# Values for each soil class from Fan & Miguez-Macho (2010)
# http://dx.doi.org/10.1007/s00382-010-0829-8

# 1 Clay --> 10
# 2 Sandy Clay --> 28
# 3 Clay Loam --> 24
# 4 Sandy Clay Loam --> 14
# 5 Loam --> 12
# 6 Sandy Loam --> 4
# 7 Loamy Sand --> 3
# 8 Sand --> 2
# 9 Silty Soil --> 40 (Silty Clay), 20 (Silty Clay Loam), 10 (Silty Loam)

# AnisotropyByClass = c(48,28,24,14,12,4,3,2,mean(c(40,20,10)))
# AnisotropyByClass = 1/AnisotropyByClass # Paper gives Kl/Kv, want Kv/Kl so that Kv = Kl * a

# Sensitivity tests show that anisotropy is relatively insensitive
# So we'll just use an anisotropy of 1 instead of counting on the empirical figures above
AnisotropyByClass = rep(1,9)

nSoilClasses = 9
SoilClassRaster = raster("POLARIS_SoilTextureClassification.tif")

lookdir = "ksat/merged"
problist = c("p50")

for (prob in 1:length(problist)){
  filelist = list.files(lookdir,pattern=paste0("-",problist[prob],"-"))
  rasterlist = lapply(paste0(lookdir,"/",filelist), raster)
  
  Depth1Raster = 0*Mask
  Depth2Raster = 0*Mask
  Depth3Raster = 0*Mask
  
  for (depth in 1:length(depthlist)){
    # Add to respective depth layer using weighted harmonic mean
    singlerast = (10^rasterlist[[depth]])/(100*60*60) # log10(cm/hr) to m/s
    singlerast = DepthLayerWeights[depth] / singlerast
    singlerast = projectRaster(singlerast, Mask, method="ngb")
    if (depth <= DepthBreakPoints[1]){
      Depth1Raster = Depth1Raster + singlerast
    } else if (depth <= DepthBreakPoints[2]) {
      Depth2Raster = Depth2Raster + singlerast
    } else if (depth <= DepthBreakPoints[3]) {
      Depth3Raster = Depth3Raster + singlerast
    }
    print(depth/length(depthlist))
  }
  
  # Finish harmonic mean
  Depth1Raster = 1/Depth1Raster
  Depth2Raster = 1/Depth2Raster
  Depth3Raster = 1/Depth3Raster
  
  # Apply anisotropy ratios by class
  for (i in 1:nSoilClasses){
    ClassPtrs = (SoilClassRaster[,] == i)
    Depth1Raster[ClassPtrs] = Depth1Raster[ClassPtrs] * AnisotropyByClass[i]
    Depth2Raster[ClassPtrs] = Depth2Raster[ClassPtrs] * AnisotropyByClass[i]
    Depth3Raster[ClassPtrs] = Depth3Raster[ClassPtrs] * AnisotropyByClass[i]
  }
  
  writeRaster(Depth1Raster,paste0("_ByLayer_/","VerticalKsat_0-15cm_",problist[prob],"_MetersPerSecond.tif"),overwrite=TRUE)
  writeRaster(Depth2Raster,paste0("_ByLayer_/","VerticalKsat_15-60cm_",problist[prob],"_MetersPerSecond.tif"),overwrite=TRUE)
  writeRaster(Depth3Raster,paste0("_ByLayer_/","VerticalKsat_60-200cm_",problist[prob],"_MetersPerSecond.tif"),overwrite=TRUE)
  
  #writeRaster(log10(Depth1Raster),paste0("_ByLayer_/","VerticalKsat_0-15cm_",problist[prob],"_Log10MetersPerSecond.tif"),overwrite=TRUE)
  #writeRaster(log10(Depth2Raster),paste0("_ByLayer_/","VerticalKsat_15-60cm_",problist[prob],"_Log10MetersPerSecond.tif"),overwrite=TRUE)
  #writeRaster(log10(Depth3Raster),paste0("_ByLayer_/","VerticalKsat_60-200cm_",problist[prob],"_Log10MetersPerSecond.tif"),overwrite=TRUE)
  
  plot(Depth1Raster)
  plot(Depth2Raster)
  plot(Depth3Raster)
  
  print(paste0("Done with ",problist[prob]))
}

# Infiltration ~ Kvertical of surface layer, same process as above

for (prob in 1:length(problist)){
  KsatSurf = raster(paste0(lookdir,"/","POLARIS-ksat-",problist,"-",depthlist[1],".tif"))
  KsatSurf = (10^KsatSurf)/(100*60*60) # log10(cm/hr) to m/s
  KsatSurf = projectRaster(KsatSurf, Mask, method="ngb")
  
  # Apply anisotropy ratios by class
  InfiltrationRaster = 0*KsatSurf
  for (i in 1:nSoilClasses){
    ClassPtrs = (SoilClassRaster[,] == i)
    InfiltrationRaster[ClassPtrs] = KsatSurf[ClassPtrs] * AnisotropyByClass[i]
  }
  
  writeRaster(InfiltrationRaster,paste0("_ByLayer_/","InfiltrationRate_0-5cm_",problist,"_MetersPerSec.tif"),overwrite=TRUE)
  #writeRaster(log10(InfiltrationRaster),paste0("_ByLayer_/","InfiltrationRate_0-5cm_",problist,"_Log10MetersPerSec.tif"),overwrite=TRUE)
  
  plot(InfiltrationRaster)
  
  print(paste0("Done with ",problist[prob]))
}

################################################################################

# Make DHSVM-relevant files: porosity (saturated soil water content, m3/m3)

lookdir = "theta_s/merged"
problist = c("p5","p50","p95")

for (prob in 1:length(problist)){
  filelist = list.files(lookdir,pattern=paste0("-",problist[prob],"-"))
  rasterlist = lapply(paste0(lookdir,"/",filelist), raster)
  
  Depth1Raster = 0*Mask
  Depth2Raster = 0*Mask
  Depth3Raster = 0*Mask
  DepthAveragedRaster = 0*Mask
  
  # Use weighted arithmetic mean for porosity?
  
  for (depth in 1:length(depthlist)){
    # Add to respective depth layer
    singlerast = rasterlist[[depth]] * DepthLayerWeights[depth]
    singlerast = projectRaster(singlerast, Mask, method="ngb")
    if (depth <= DepthBreakPoints[1]){
      Depth1Raster = Depth1Raster + singlerast
    } else if (depth <= DepthBreakPoints[2]) {
      Depth2Raster = Depth2Raster + singlerast
    } else if (depth <= DepthBreakPoints[3]) {
      Depth3Raster = Depth3Raster + singlerast
    }
    
    # Add to depth-averaged mean
    singlerast = rasterlist[[depth]] * DepthWeights[depth]
    singlerast = projectRaster(singlerast, Mask, method="ngb")
    DepthAveragedRaster = DepthAveragedRaster + singlerast
    
    print(depth/length(depthlist))
  }
  
  writeRaster(Depth1Raster,paste0("_ByLayer_/","Porosity_0-15cm_",problist[prob],"_Meter3PerMeter3.tif"),overwrite=TRUE)
  writeRaster(Depth2Raster,paste0("_ByLayer_/","Porosity_15-60cm_",problist[prob],"_Meter3PerMeter3.tif"),overwrite=TRUE)
  writeRaster(Depth3Raster,paste0("_ByLayer_/","Porosity_60-200cm_",problist[prob],"_Meter3PerMeter3.tif"),overwrite=TRUE)
  #writeRaster(DepthAveragedRaster,paste0("Porosity_",problist[prob],"_Meter3PerMeter3.tif"),overwrite=TRUE)
  
  plot(DepthAveragedRaster)
  print(paste0("Done with ",problist[prob]))
}

################################################################################

# Make DHSVM-relevant files: pore size distribution (Brooks-Corey lambda)

lookdir = "lambda/merged"
problist = c("p50")

for (prob in 1:length(problist)){
  filelist = list.files(lookdir,pattern=paste0("-",problist[prob],"-"))
  rasterlist = lapply(paste0(lookdir,"/",filelist), raster)
  
  Depth1Raster = 0*Mask
  Depth2Raster = 0*Mask
  Depth3Raster = 0*Mask
  DepthAveragedRaster = 0*Mask
  
  # Use weighted geometric mean for pore size distribution
  
  for (depth in 1:length(depthlist)){
    # Add to respective depth layer
    singlerast = log(rasterlist[[depth]]) * DepthLayerWeights[depth]
    singlerast = projectRaster(singlerast, Mask, method="ngb")
    if (depth <= DepthBreakPoints[1]){
      Depth1Raster = Depth1Raster + singlerast
    } else if (depth <= DepthBreakPoints[2]) {
      Depth2Raster = Depth2Raster + singlerast
    } else if (depth <= DepthBreakPoints[3]) {
      Depth3Raster = Depth3Raster + singlerast
    }
    
    # Add to depth-averaged mean
    singlerast = log(rasterlist[[depth]]) * DepthWeights[depth]
    singlerast = projectRaster(singlerast, Mask, method="ngb")
    DepthAveragedRaster = DepthAveragedRaster + singlerast
    
    print(depth/length(depthlist))
  }
  
  # Finish geometric mean
  Depth1Raster = exp(Depth1Raster/sum(DepthLayerWeights[1:2]))
  Depth2Raster = exp(Depth2Raster/sum(DepthLayerWeights[3:4]))
  Depth3Raster = exp(Depth3Raster/sum(DepthLayerWeights[5:6]))
  DepthAveragedRaster = exp(DepthAveragedRaster/sum(DepthWeights))
  
  writeRaster(Depth1Raster,paste0("_ByLayer_/","PoreSizeDistGeometric_0-15cm_",problist[prob],"_Unitless.tif"),overwrite=TRUE)
  writeRaster(Depth2Raster,paste0("_ByLayer_/","PoreSizeDistGeometric_15-60cm_",problist[prob],"_Unitless.tif"),overwrite=TRUE)
  writeRaster(Depth3Raster,paste0("_ByLayer_/","PoreSizeDistGeometric_60-200cm_",problist[prob],"_Unitless.tif"),overwrite=TRUE)
  #writeRaster(DepthAveragedRaster,paste0("PoreSizeDistGeometric_",problist[prob],"_Unitless.tif"),overwrite=TRUE)
  
  plot(DepthAveragedRaster)
  
  print(paste0("Done with ",problist[prob]))
}

################################################################################

# Make DHSVM-relevant files: bubbling pressure

lookdir = "hb/merged"
problist = c("p50")

for (prob in 1:length(problist)){
  filelist = list.files(lookdir,pattern=paste0("-",problist[prob],"-"))
  rasterlist = lapply(paste0(lookdir,"/",filelist), raster)
  
  Depth1Raster = 0*Mask
  Depth2Raster = 0*Mask
  Depth3Raster = 0*Mask
  DepthAveragedRaster = 0*Mask
  
  # Use weighted geometric mean for bubbling pressure
  # However, "the logarithm of the geometric mean is the weighted arithmetic mean of the logarithms of the individual values" (Wikipedia)
  # Since hb maps are already in units of log10(kPa), compute weighted arithmetic mean
  
  for (depth in 1:length(depthlist)){
    # Add to respective depth layer
    singlerast = rasterlist[[depth]] * DepthLayerWeights[depth]
    singlerast = projectRaster(singlerast, Mask, method="ngb")
    if (depth <= DepthBreakPoints[1]){
      Depth1Raster = Depth1Raster + singlerast
    } else if (depth <= DepthBreakPoints[2]) {
      Depth2Raster = Depth2Raster + singlerast
    } else if (depth <= DepthBreakPoints[3]) {
      Depth3Raster = Depth3Raster + singlerast
    }
    
    # Add to depth-averaged mean
    singlerast = rasterlist[[depth]] * DepthWeights[depth]
    singlerast = projectRaster(singlerast, Mask, method="ngb")
    DepthAveragedRaster = DepthAveragedRaster + singlerast
    
    print(depth/length(depthlist))
  }
  
  # Convert units
  Depth1Raster = (10^Depth1Raster)*1.09172 # log10(kPa) --> m H20
  Depth2Raster = (10^Depth2Raster)*1.09172 # log10(kPa) --> m H20
  Depth3Raster = (10^Depth3Raster)*1.09172 # log10(kPa) --> m H20
  DepthAveragedRaster = (10^DepthAveragedRaster)*1.09172 # log10(kPa) --> m H20
  
  writeRaster(Depth1Raster,paste0("_ByLayer_/","BubblingPressureGeometric_0-15cm_",problist[prob],"_MetersH20.tif"),overwrite=TRUE)
  writeRaster(Depth2Raster,paste0("_ByLayer_/","BubblingPressureGeometric_15-60cm_",problist[prob],"_MetersH20.tif"),overwrite=TRUE)
  writeRaster(Depth3Raster,paste0("_ByLayer_/","BubblingPressureGeometric_60-200cm_",problist[prob],"_MetersH20.tif"),overwrite=TRUE)
  #writeRaster(DepthAveragedRaster,paste0("BubblingPressureGeometric_",problist[prob],"_MetersH20.tif"),overwrite=TRUE)
  
  plot(DepthAveragedRaster)
  
  print(paste0("Done with ",problist[prob]))
}

################################################################################

# Make DHSVM-relevant files: residual water content

lookdir = "theta_r/merged"
problist = c("p50")

for (prob in 1:length(problist)){
  filelist = list.files(lookdir,pattern=paste0("-",problist[prob],"-"))
  rasterlist = lapply(paste0(lookdir,"/",filelist), raster)
  
  Depth1Raster = 0*Mask
  Depth2Raster = 0*Mask
  Depth3Raster = 0*Mask
  DepthAveragedRaster = 0*Mask
  
  # Use weighted arithmetic mean for residual water content
  
  for (depth in 1:length(depthlist)){
    # Add to respective depth layer
    singlerast = rasterlist[[depth]] * DepthLayerWeights[depth]
    singlerast = projectRaster(singlerast, Mask, method="ngb")
    if (depth <= DepthBreakPoints[1]){
      Depth1Raster = Depth1Raster + singlerast
    } else if (depth <= DepthBreakPoints[2]) {
      Depth2Raster = Depth2Raster + singlerast
    } else if (depth <= DepthBreakPoints[3]) {
      Depth3Raster = Depth3Raster + singlerast
    }
    
    # Add to depth-averaged mean
    singlerast = rasterlist[[depth]] * DepthWeights[depth]
    singlerast = projectRaster(singlerast, Mask, method="ngb")
    DepthAveragedRaster = DepthAveragedRaster + singlerast
    
    print(depth/length(depthlist))
  }
  
  writeRaster(Depth1Raster,paste0("_ByLayer_/","ResidualWaterContent_0-15cm_",problist[prob],"_Meter3PerMeter3.tif"),overwrite=TRUE)
  writeRaster(Depth2Raster,paste0("_ByLayer_/","ResidualWaterContent_15-60cm_",problist[prob],"_Meter3PerMeter3.tif"),overwrite=TRUE)
  writeRaster(Depth3Raster,paste0("_ByLayer_/","ResidualWaterContent_60-200cm_",problist[prob],"_Meter3PerMeter3.tif"),overwrite=TRUE)
  #writeRaster(DepthAveragedRaster,paste0("ResidualWaterContent_",problist[prob],"_Meter3PerMeter3.tif"),overwrite=TRUE)
  
  plot(DepthAveragedRaster)
  print(paste0("Done with ",problist[prob]))
}

################################################################################

# Make DHSVM-relevant files: bulk density

lookdir = "bd/merged"
problist = c("p50")

for (prob in 1:length(problist)){
  filelist = list.files(lookdir,pattern=paste0("-",problist[prob],"-"))
  rasterlist = lapply(paste0(lookdir,"/",filelist), raster)
  
  Depth1Raster = 0*Mask
  Depth2Raster = 0*Mask
  Depth3Raster = 0*Mask
  DepthAveragedRaster = 0*Mask
  
  # Use weighted arithmetic mean for bulk density
  
  for (depth in 1:length(depthlist)){
    # Add to respective depth layer
    singlerast = rasterlist[[depth]] * DepthLayerWeights[depth]
    singlerast = projectRaster(singlerast, Mask, method="ngb")
    if (depth <= DepthBreakPoints[1]){
      Depth1Raster = Depth1Raster + singlerast
    } else if (depth <= DepthBreakPoints[2]) {
      Depth2Raster = Depth2Raster + singlerast
    } else if (depth <= DepthBreakPoints[3]) {
      Depth3Raster = Depth3Raster + singlerast
    }
    
    # Add to depth-averaged mean
    singlerast = rasterlist[[depth]] * DepthWeights[depth]
    singlerast = projectRaster(singlerast, Mask, method="ngb")
    DepthAveragedRaster = DepthAveragedRaster + singlerast
    
    print(depth/length(depthlist))
  }
  
  # Convert units
  Depth1Raster = Depth1Raster * 1000 # g/cm^3 to kg/m^3
  Depth2Raster = Depth2Raster * 1000 # g/cm^3 to kg/m^3
  Depth3Raster = Depth3Raster * 1000 # g/cm^3 to kg/m^3
  DepthAveragedRaster = DepthAveragedRaster * 1000 # g/cm^3 to kg/m^3
  
  writeRaster(Depth1Raster,paste0("_ByLayer_/","BulkDensity_0-15cm_",problist[prob],"_KilogramsPerMeter3.tif"),overwrite=TRUE)
  writeRaster(Depth2Raster,paste0("_ByLayer_/","BulkDensity_15-60cm_",problist[prob],"_KilogramsPerMeter3.tif"),overwrite=TRUE)
  writeRaster(Depth3Raster,paste0("_ByLayer_/","BulkDensity_60-200cm_",problist[prob],"_KilogramsPerMeter3.tif"),overwrite=TRUE)
  #writeRaster(DepthAveragedRaster,paste0("BulkDensity_",problist[prob],"_KilogramsPerMeter3.tif"),overwrite=TRUE)
  
  plot(DepthAveragedRaster)
  print(paste0("Done with ",problist[prob]))
}

################################################################################

# Make DHSVM-relevant files: field capacity and wilting point

lookdir1 = "n/merged"
lookdir2 = "alpha/merged"
lookdir3 = "theta_r/merged"
lookdir4 = "theta_s/merged"

problist = c("p50")

# Van Genuchten model
VanGenuchten = function(thetaR,thetaS,alphaVal,nVal,SuctionPressure){
  thetaPsi = thetaR + (thetaS - thetaR)/((1+(alphaVal*SuctionPressure)^nVal)^(1-1/nVal))
  return(thetaPsi)
}

FCpress = 33 # Kpa (0.33 bar)
WPpress = 1500 # Kpa (15 bar)

for (prob in 1:length(problist)){
  filelist1 = list.files(lookdir1,pattern=paste0("-",problist[prob],"-"))
  rasterlistN = lapply(paste0(lookdir1,"/",filelist1), raster)
  
  filelist2 = list.files(lookdir2,pattern=paste0("-",problist[prob],"-"))
  rasterlistAlpha = lapply(paste0(lookdir2,"/",filelist2), raster)
  
  filelist3 = list.files(lookdir3,pattern=paste0("-",problist[prob],"-"))
  rasterlistThetaR = lapply(paste0(lookdir3,"/",filelist3), raster)
  
  filelist4 = list.files(lookdir4,pattern=paste0("-",problist[prob],"-"))
  rasterlistThetaS = lapply(paste0(lookdir4,"/",filelist4), raster)
  
  Depth1RasterFC = 0*Mask
  Depth2RasterFC = 0*Mask
  Depth3RasterFC = 0*Mask
  Depth1RasterWP = 0*Mask
  Depth2RasterWP = 0*Mask
  Depth3RasterWP = 0*Mask
  DepthAveragedRasterFC = 0*Mask
  DepthAveragedRasterWP = 0*Mask
  
  # Compute FC and WP at each depth, then calculate weighted arithmetic mean
  
  for (depth in 1:length(depthlist)){
    # Read in required parameters for a single depth layer
    singlerastN = projectRaster(rasterlistN[[depth]], Mask, method="ngb")
    singlerastAlpha = 10^projectRaster(rasterlistAlpha[[depth]], Mask, method="ngb") # Convert log10(kPa-1) to kPa-1
    singlerastThetaR = projectRaster(rasterlistThetaR[[depth]], Mask, method="ngb")
    singlerastThetaS = projectRaster(rasterlistThetaS[[depth]], Mask, method="ngb")
    
    # Calculate FC and WP for a single depth layer
    singlerastFC = 0*Mask
    singlerastWP = 0*Mask
    singlerastFC[,] = VanGenuchten(singlerastThetaR[,],singlerastThetaS[,],singlerastAlpha[,],singlerastN[,],rep(FCpress,length(singlerastN[,])))
    singlerastWP[,] = VanGenuchten(singlerastThetaR[,],singlerastThetaS[,],singlerastAlpha[,],singlerastN[,],rep(WPpress,length(singlerastN[,])))
    
    # Add to respective depth layer
    if (depth <= DepthBreakPoints[1]){
      Depth1RasterFC = Depth1RasterFC + singlerastFC * DepthLayerWeights[depth]
      Depth1RasterWP = Depth1RasterWP + singlerastWP * DepthLayerWeights[depth]
    } else if (depth <= DepthBreakPoints[2]) {
      Depth2RasterFC = Depth2RasterFC + singlerastFC * DepthLayerWeights[depth]
      Depth2RasterWP = Depth2RasterWP + singlerastWP * DepthLayerWeights[depth]
    } else if (depth <= DepthBreakPoints[3]) {
      Depth3RasterFC = Depth3RasterFC + singlerastFC * DepthLayerWeights[depth]
      Depth3RasterWP = Depth3RasterWP + singlerastWP * DepthLayerWeights[depth]
    }
    
    # Add to depth-averaged mean
    DepthAveragedRasterFC = DepthAveragedRasterFC + singlerastFC * DepthWeights[depth]
    DepthAveragedRasterWP = DepthAveragedRasterWP + singlerastWP * DepthWeights[depth]
    
    print(depth/length(depthlist))
  }
  
  writeRaster(Depth1RasterFC,paste0("_ByLayer_/","FieldCapacity_0-15cm_",problist[prob],"_Meter3PerMeter3.tif"),overwrite=TRUE)
  writeRaster(Depth2RasterFC,paste0("_ByLayer_/","FieldCapacity_15-60cm_",problist[prob],"_Meter3PerMeter3.tif"),overwrite=TRUE)
  writeRaster(Depth3RasterFC,paste0("_ByLayer_/","FieldCapacity_60-200cm_",problist[prob],"_Meter3PerMeter3.tif"),overwrite=TRUE)
  writeRaster(Depth1RasterWP,paste0("_ByLayer_/","WiltingPoint_0-15cm_",problist[prob],"_Meter3PerMeter3.tif"),overwrite=TRUE)
  writeRaster(Depth2RasterWP,paste0("_ByLayer_/","WiltingPoint_15-60cm_",problist[prob],"_Meter3PerMeter3.tif"),overwrite=TRUE)
  writeRaster(Depth3RasterWP,paste0("_ByLayer_/","WiltingPoint_60-200cm_",problist[prob],"_Meter3PerMeter3.tif"),overwrite=TRUE)
  #writeRaster(DepthAveragedRasterFC,paste0("FieldCapacity_",problist[prob],"_Meter3PerMeter3.tif"),overwrite=TRUE)
  #writeRaster(DepthAveragedRasterWP,paste0("WiltingPoint_",problist[prob],"_Meter3PerMeter3.tif"),overwrite=TRUE)
  
  plot(DepthAveragedRasterFC)
  plot(DepthAveragedRasterWP)
  print(paste0("Done with ",problist[prob]))
}


################################################################################

# Make DHSVM-relevant files: estimate thermal properties from density

# Volumetric heat capacity: Cv [J/(m^3*K)]
# Thermal conductivity: k [W/(m*K)]
# Assume the moisture content is zero, as specified by DHSVM for these parameters

# Values for specific heat from Van Wijk (1963), stated in Farouki (1981)
# Quartz and many soil minerals: 0.175 cal/(g*K) = 733 J/(kg*K)
# Assuming 100% mineral soil (no organic matter, dry)
# Cv [J/(m^3*K)] = Soil Density [kg/m^3] * 733 [J/(kg*K)]

# Empirical equation for thermal conductivity from Johansen (1975), stated in Farouki (1981)
# kDry [W/(m*K)] = (0.135 * Soil Density [kg/m^3] + 64.7)/(2700 - 0.947 * Soil Density [kg/m^3])

# See also: Lu (2007) and Brutsaert (1982)

HeatCapacity = function(DryBulkDensity){
  Cv = 733 * DryBulkDensity
  return(Cv)
}
ThermalConductivity = function(DryBulkDensity){
  kDry = (0.135*DryBulkDensity + 64.7)/(2700 - 0.947*DryBulkDensity)
  return(kDry)
}

lookdir = "_ByLayer_"

for (depth in 1:length(DepthLayers)){
  DryBulkDensityRaster = raster(paste0(lookdir,"/BulkDensity_",DepthLayers[depth],"cm_p50_KilogramsPerMeter3.tif"))
  
  HeatCapacityRaster = 0*Mask
  ThermalConductivityRaster = 0*Mask
  
  HeatCapacityRaster[,] = HeatCapacity(DryBulkDensityRaster[,])
  ThermalConductivityRaster[,] = ThermalConductivity(DryBulkDensityRaster[,])
  
  writeRaster(HeatCapacityRaster,paste0(lookdir,"/ThermalCapacity_",DepthLayers[depth],"cm_p50_JoulesPerMeter3Kelvin.tif"))
  writeRaster(ThermalConductivityRaster,paste0(lookdir,"/ThermalConductivity_",DepthLayers[depth],"cm_p50_WattsPerMeterKelvin.tif"))
  
  plot(HeatCapacityRaster)
  plot(ThermalConductivityRaster)
}


























