# Convert LANDIS output maps into usable inputs for DHSVM

library(raster)
library(bigleaf)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/LANDIS/"
setwd(dir)

ClassSettings = read.csv("VegClassSettings.csv")
print(ClassSettings)

nLANDISclasses = 28

Mask = raster("../WatershedBoundaries/TCSImask_MergedDomain.tif")
DEM = raster("../SRTM/TCSIdomain_90mDEMfilled.tif")

nRows = 1436 # Number of rows in the domain, used to write text files
nHeaderLines = 6 # Number of rows in a .asc raster header

################################################################################
# Part 1: Veg ID
################################################################################

VegIDmap = NA*Mask # Keep track of which pixels haven't been satisfactorily filled

# Reclassify LANDIS and NLCD numbering system to DHSVM numbering system
# This excludes the Alpine Shrub class, which is handled later
# -999 signifies that LANDIS or NLCD do not include the respective DHSVM class
VegClasses = read.csv("ReclassifyVeg.csv")
print(VegClasses)

# Use LANDIS data wherever it is available
IDmapLANDIS = raster("veg-ID-0.tif")
IDmapLANDIS = projectRaster(IDmapLANDIS, Mask, method="ngb")
plot(IDmapLANDIS)

for (i in 1:length(VegClasses$ClassLANDIS)){
  Ptrs = which(IDmapLANDIS[,] == VegClasses$ClassLANDIS[i])
  VegIDmap[Ptrs] = VegClasses$ClassDHSVM[i]
  print(sum(as.numeric(Ptrs)))
}

plot(VegIDmap)

# Even-valued LANDIS classes have heavy understory
UnderstoryFraction = length(which((IDmapLANDIS[,] %% 2) == 0))/length(IDmapLANDIS[,])
print(paste0("Fraction of LANDIS pixels with heavy understory: ",round(UnderstoryFraction,2)))
hist(VegIDmap[,],breaks=seq(0.5,28.5,by=1))

# Fill missing values with data from NLCD
IDmapNLCD = raster("../NLCD/NLCD_2019_Land_Cover_L48_20210604_XECcUYCCDqvXC8b44Boa.tiff")
IDmapNLCD = projectRaster(IDmapNLCD, Mask, method="ngb")
plot(IDmapNLCD)

for (i in 1:length(VegClasses$ClassNLCD)){
  Ptrs = which(IDmapNLCD[,]==VegClasses$ClassNLCD[i] & is.na(VegIDmap[,]))
  VegIDmap[Ptrs] = VegClasses$ClassDHSVM[i]
  print(sum(as.numeric(Ptrs)))
}

# Add Alpine Shrub class above 2500 m
Ptrs = which(DEM[,]>2500 & VegIDmap[,]==33)
length(Ptrs)/sum(Mask[,]) # Alpine Shrub fraction
VegIDmap[Ptrs] = 35

plot(VegIDmap)
sort(unique(VegIDmap[,]))
sum(as.numeric(is.na(VegIDmap[,])))

# Fill remaining NAs with GrassCropShrub (mainly an unmodeled area in the top right corner just for completeness)
VegIDmap[is.na(VegIDmap[,])] = 33

plot(VegIDmap)

writeRaster(VegIDmap,"TCSI_Year0_VegID.tif",overwrite=TRUE)

# Convert to the DHSVM header-less format
writeRaster(VegIDmap,"veg.asc",datatype="INT2U",overwrite=TRUE) # Must write as integers to prevent adding decimal to first value
TextRaster = readLines("veg.asc")
writeLines(TextRaster[(nHeaderLines+1):(nHeaderLines+nRows)],"veg.txt")
file.remove("veg.asc")

# For convenient use later, create masks of overstory and understory presence
OverstoryPresent = 0*Mask
UnderstoryPresent = 0*Mask

for (i in 1:length(ClassSettings$ClassDHSVM)){
  Ptrs = which(VegIDmap[,]==ClassSettings$ClassDHSVM[i])
  if (ClassSettings$Overstory[i]){
    OverstoryPresent[Ptrs] = 1
  }
  if (ClassSettings$Understory[i]){
    UnderstoryPresent[Ptrs] = 1
  }
}

plot(OverstoryPresent)
plot(UnderstoryPresent)

writeRaster(UnderstoryPresent,"TCSI_Year0_UnderstoryMask.tif",overwrite=TRUE)
writeRaster(OverstoryPresent,"TCSI_Year0_OverstoryMask.tif",overwrite=TRUE)

################################################################################
# Part 2: Canopy Cover
################################################################################

CanopyCoverMap = NA*Mask

# Use LANDIS data wherever it is available
CanopyMapLANDIS = raster("cc-0.tif")
CanopyMapLANDIS = projectRaster(CanopyMapLANDIS, Mask, method="ngb")
plot(CanopyMapLANDIS)
hist(CanopyMapLANDIS[,])
max(CanopyMapLANDIS[which(!is.na(CanopyMapLANDIS[,]))])
print(1/max(CanopyMapLANDIS[which(!is.na(CanopyMapLANDIS[,]))]))

Ptrs = which(!is.na(CanopyMapLANDIS[,]))
CanopyCoverMap[Ptrs] = CanopyMapLANDIS[Ptrs]

# Fill missing values with data from NLCD
CanopyMapNLCD = raster("../NLCD/NLCD_2016_Tree_Canopy_L48_20190831_XECcUYCCDqvXC8b44Boa.tiff")
CanopyMapNLCD = projectRaster(CanopyMapNLCD, Mask, method="ngb")
plot(CanopyMapNLCD)

# Fill with NLCD data if it indicates canopy presence (don't allow 0.0 canopy cover in cells where overstory = present)
Ptrs = which(is.na(CanopyCoverMap[,]) & OverstoryPresent[,]==1 & CanopyMapNLCD[,]>0)
CanopyCoverMap[Ptrs] = CanopyMapNLCD[Ptrs]/100 # Convert percentage to fraction

plot(CanopyCoverMap)
hist(CanopyCoverMap[,])
sum(as.numeric(is.na(CanopyCoverMap[,] & OverstoryPresent[,]==1)))/length(CanopyCoverMap[,])

# Set canopy cover = 0.1 where overstory is present but NLCD shows 0.0 canopy cover
CanopyCoverMap[is.na(CanopyCoverMap[,] & OverstoryPresent[,]==1)] = 0.1
hist(CanopyCoverMap[,])

# Replace any class that doesn't have overstory with -9999
Ptrs = which(OverstoryPresent[,]==0)
CanopyCoverMap[Ptrs] = -9999

plot(CanopyCoverMap,zlim=c(0,1))

writeRaster(CanopyCoverMap,"TCSI_Year0_CanopyCover.tif",overwrite=TRUE)

# Convert to the DHSVM header-less format
writeRaster(CanopyCoverMap,"veg_fc.asc",overwrite=TRUE)
TextRaster = readLines("veg_fc.asc")
writeLines(TextRaster[(nHeaderLines+1):(nHeaderLines+nRows)],"veg_fc.txt")
file.remove("veg_fc.asc")

################################################################################
# Part 3: Veg Height (overstory and understory)
################################################################################

OverstoryHeightMap = NA*Mask

# Use LANDIS data wherever it is available
HeightMapLANDIS = raster("can-ht-0.tif")
HeightMapLANDIS = projectRaster(HeightMapLANDIS, Mask, method="ngb")
plot(HeightMapLANDIS)

Ptrs = which(!is.na(HeightMapLANDIS[,]))
OverstoryHeightMap[Ptrs] = HeightMapLANDIS[Ptrs]

# Identify overstory classes in need of filling
OverstoryClassesWithNAs = sort(unique(VegIDmap[which(is.na(OverstoryHeightMap[,]) & OverstoryPresent[,]==1)]))

for (i in OverstoryClassesWithNAs){
  # The cells to be filled
  Ptrs = which(VegIDmap[,]==i & is.na(OverstoryHeightMap[,]))
  
  if (i <= nLANDISclasses){
    # Fill missing values with median height of the corresponding LANDIS class
    MedianPtrs = which(VegIDmap[,]==i & is.finite(OverstoryHeightMap[,]))
    # Also compare cells with the same overstory class but with/without understory
    if (i %% 2){
      # Current class is odd (no understory), also compare cells with understory
      MedianPtrs = sort(c(MedianPtrs, which(VegIDmap[,]==(i+1) & is.finite(OverstoryHeightMap[,]))))
    } else {
      # Current class is even (with understory), also compare cells without understory
      MedianPtrs = sort(c(MedianPtrs, which(VegIDmap[,]==(i-1) & is.finite(OverstoryHeightMap[,]))))
    }
  } else {
    # Otherwise, fill missing values with median height of all original LANDIS cells, since there is no direct correspondence
    MedianPtrs = which(is.finite(HeightMapLANDIS[,]))
  }
  
  OverstoryHeightMap[Ptrs] = median(OverstoryHeightMap[MedianPtrs])
  print(i)
  print(length(Ptrs))
  print(median(OverstoryHeightMap[MedianPtrs]))
}

plot(OverstoryHeightMap)
hist(OverstoryHeightMap[,])

# Replace any class that doesn't have overstory with -9999
Ptrs = which(OverstoryPresent[,]==0)
OverstoryHeightMap[Ptrs] = -9999
sum(as.numeric(is.na(OverstoryHeightMap[,])))

plot(OverstoryHeightMap,zlim=c(0,50))

# Print max veg height so we can determine the wind reference level (max height + 10 m)
print(round(max(OverstoryHeightMap[,])))

################################################################################
# Make an understory height map: heavy understory --> 1 m, light understory --> 0.5 m

UnderstoryHeightMap = NA*Mask

for (i in 1:length(ClassSettings$ClassDHSVM)){
  Ptrs = which(VegIDmap[,]==ClassSettings$ClassDHSVM[i])
  # Light understory
  if (ClassSettings$Understory[i]==1){
    UnderstoryHeightMap[Ptrs] = 0.5
  }
  # Heavy understory
  if (ClassSettings$Understory[i]==2){
    # Wetland has high LAI (heavy understory) but low height
    if (ClassSettings$ClassName[i]=="Wetland"){
      UnderstoryHeightMap[Ptrs] = 0.5
    } else {
      UnderstoryHeightMap[Ptrs] = 1
    }
  }
}

# Replace any class that doesn't have understory with -9999
Ptrs = which(UnderstoryPresent[,]==0)
UnderstoryHeightMap[Ptrs] = -9999

plot(UnderstoryHeightMap,zlim=c(0,1))

writeRaster(OverstoryHeightMap,"TCSI_Year0_CanopyHeightOverstory.tif",overwrite=TRUE)
writeRaster(UnderstoryHeightMap,"TCSI_Year0_CanopyHeightUnderstory.tif",overwrite=TRUE)

# Convert to the DHSVM header-less format and concatenate overstory/understory
writeRaster(OverstoryHeightMap,"veg_height_overstory.asc",overwrite=TRUE)
writeRaster(UnderstoryHeightMap,"veg_height_understory.asc",overwrite=TRUE)
TextRaster1 = readLines("veg_height_overstory.asc")
TextRaster2 = readLines("veg_height_understory.asc")
writeLines(c(TextRaster1[(nHeaderLines+1):(nHeaderLines+nRows)],TextRaster2[(nHeaderLines+1):(nHeaderLines+nRows)]),"veg_height.txt")
file.remove("veg_height_overstory.asc")
file.remove("veg_height_understory.asc")

################################################################################
# Part 4: Veg LAI (monthly)
################################################################################

LAIsummerMap = NA*Mask

# Use LANDIS data wherever it is available
LAIsummerMapLANDIS = raster("LAI-0.tif")
LAIsummerMapLANDIS = projectRaster(LAIsummerMapLANDIS, Mask, method="ngb")
plot(LAIsummerMapLANDIS)

# Fill with LANDIS data if it indicates positive LAI (don't allow 0.0 LAI in cells where overstory = present)
LAIsummerMapLANDIS[!(LAIsummerMapLANDIS[,]>0)] = NA
Ptrs = which(!is.na(LAIsummerMapLANDIS[,]))
LAIsummerMap[Ptrs] = LAIsummerMapLANDIS[Ptrs]
plot(LAIsummerMap)

# Identify classes in need of filling
OverstoryClassesWithNAs = sort(unique(VegIDmap[which(is.na(LAIsummerMap[,]) & OverstoryPresent[,]==1)]))
sum(as.numeric(is.na(LAIsummerMap[,] & OverstoryPresent[,]==1)))/length(LAIsummerMap[,]) # Fraction of all cells to be filled
sum(as.numeric(is.na(LAIsummerMap[,] & is.finite(IDmapLANDIS[,]))))/length(LAIsummerMap[,]) # Fractional mismatch between LANDIS ID and LANDIS LAI

for (i in OverstoryClassesWithNAs){
  # The cells to be filled
  Ptrs = which(VegIDmap[,]==i & is.na(LAIsummerMap[,]) & OverstoryPresent[,]==1)
  
  if (i <= nLANDISclasses){
    # Fill missing values with median LAI of the corresponding LANDIS class
    MedianPtrs = which(VegIDmap[,]==i & is.finite(LAIsummerMap[,]))
    # Also compare cells with the same overstory class but with/without understory
    if (i %% 2){
      # Current class is odd (no understory), also compare cells with understory
      MedianPtrs = sort(c(MedianPtrs, which(VegIDmap[,]==(i+1) & is.finite(LAIsummerMap[,]))))
    } else {
      # Current class is even (with understory), also compare cells without understory
      MedianPtrs = sort(c(MedianPtrs, which(VegIDmap[,]==(i-1) & is.finite(LAIsummerMap[,]))))
    }
  }
  
  if (length(MedianPtrs)==0 | i>nLANDISclasses){
    # Otherwise, fill missing values with median LAI of all original LANDIS cells, since there is no direct correspondence
    MedianPtrs = which(is.finite(LAIsummerMap[,]))
  }
  
  LAIsummerMap[Ptrs] = median(LAIsummerMap[MedianPtrs])
  print(i)
  print(length(Ptrs))
  print(median(LAIsummerMap[MedianPtrs]))
}

plot(LAIsummerMap)
hist(LAIsummerMap[,])

# Replace any class that doesn't have overstory with -9999
Ptrs = which(OverstoryPresent[,]==0)
LAIsummerMap[Ptrs] = -9999
sum(as.numeric(is.na(LAIsummerMap[,])))

plot(LAIsummerMap,zlim=c(0,14))

################################################################################
# Apply scale factors from Sam/LANDIS to convert summer LAI into winter LAI

LAIwinterMap = NA*Mask

# LAI annual fluctuation data directly from LANDIS stored LAIwinterScale column
# His note: LAI in NECN is only calculated from leaves -- I replaced any zero values with 0.2 (Fang et al. 2019).
head(ClassSettings)

for (i in ClassSettings$ClassDHSVM){
  # Only calculate LAI changes for overstory, since only overstory LAI is read from the map
  if (ClassSettings$Overstory[i]){
    Ptrs = which(VegIDmap[,]==i)
    LAIwinterMap[Ptrs] = LAIsummerMap[Ptrs] * ClassSettings$LAIwinterScale[i]
  }
}

# Replace any class that doesn't have overstory with -9999
Ptrs = which(OverstoryPresent[,]==0)
LAIwinterMap[Ptrs] = -9999

plot(LAIwinterMap,zlim=c(0,14))
plot(LAIsummerMap-LAIwinterMap)
hist(LAIwinterMap[OverstoryPresent[,]==1]/LAIsummerMap[OverstoryPresent[,]==1])

writeRaster(LAIsummerMap,"TCSI_Year0_LAIsummer.tif",overwrite=TRUE)
writeRaster(LAIwinterMap,"TCSI_Year0_LAIwinter.tif",overwrite=TRUE)

# Convert to the DHSVM header-less format and concatenate overstory/understory
writeRaster(LAIsummerMap,"veg_lai_summer.asc",overwrite=TRUE)
writeRaster(LAIwinterMap,"veg_lai_winter.asc",overwrite=TRUE)
TextRaster1 = readLines("veg_lai_summer.asc")
TextRaster2 = readLines("veg_lai_winter.asc")
SummerRaster = TextRaster1[(nHeaderLines+1):(nHeaderLines+nRows)]
WinterRaster = TextRaster2[(nHeaderLines+1):(nHeaderLines+nRows)]

# Combine according to the fluctuation sheet from LANDIS
# Oct-Nov --> x1, Dec-Mar --> lower, Apr-Sep --> x1
# Note that DHSVM reads LAI month-by-month (concatenated rasters) starting with January, NOT the water year!
WinterMons = c(12,1:3)
TextRaster = c()
for (mon in 1:12){
  if (mon %in% WinterMons){
    TextRaster = c(TextRaster,WinterRaster)
  } else {
    TextRaster = c(TextRaster,SummerRaster)
  }
}

length(TextRaster)/12

writeLines(TextRaster,"veg_lai.txt")
file.remove("veg_lai_summer.asc")
file.remove("veg_lai_winter.asc")

################################################################################
# Part 5: Estimate interception parameters
################################################################################

LAIsummerMapLANDIS = raster("LAI-0.tif")
CanopyMapLANDIS = raster("cc-0.tif")

# MaxSnowIntCap = (MaxSnowIntCap > LAI * F * LAI_SNOW_MULTIPLIER) ? (LAI * F * LAI_SNOW_MULTIPLIER) : MaxSnowIntCap;
# Snow interception parameters from Martin et al. (2013) https://doi.org/10.1002/wrcr.20271
# Values from Table 1; each study is weighted equally

InterceptionEfficiency = c(45,66,28,58,mean(50,45,45),mean(60,45),50,mean(45,30),60,83)/100 # percent --> fractional

MaxSnowInterception = c(30,30,22,mean(3.5,7),30,40,50)/1000 # mm --> m
SnowIntCap = mean(MaxSnowInterception)

LAIptrs = which(LAIsummerMapLANDIS[,] > 0)
hist(LAIsummerMapLANDIS[LAIptrs])
meanLAI = mean(LAIsummerMapLANDIS[LAIptrs])

FCptrs = which(CanopyMapLANDIS[,] > 0)
hist(CanopyMapLANDIS[FCptrs])
meanFC = mean(CanopyMapLANDIS[FCptrs])

LAImultiplierSnow = SnowIntCap/(meanLAI * meanFC)

SnowIntCap90th = as.numeric(quantile(LAIsummerMapLANDIS[LAIptrs], 0.9)) * as.numeric(quantile(CanopyMapLANDIS[FCptrs], 0.9)) * LAImultiplierSnow

# Rain interception parameters from Link et al. (2004) https://doi.org/10.1016/j.agrformet.2004.01.010
# Values from Table 1, plus last value is from Table 4 (result of this study)

MaxRainInterception = c(mean(2.7,4.3),1.2,2.4,1,1.02,0.8,mean(0.5,0.55),2,0.75,1.2,3.6)/1000 # mm --> m
RainIntCap = mean(MaxRainInterception)

#LAImultiplierRain = RainIntCap/(meanLAI * meanFC)
LAImultiplierRain = sort(MaxRainInterception)[round(0.9*length(MaxRainInterception))]/meanLAI

res1 = paste0("Mean rain LAI multiplier: ",format(signif(LAImultiplierRain,1),scientific=FALSE),"\n")
res2 = paste0("Mean snow LAI multiplier: ",signif(LAImultiplierSnow,1),"\n")
res3 = paste0("Mean snow interception efficiency: ",signif(mean(InterceptionEfficiency),1),"\n")
res4 = paste0("Max observed snow interception: ",max(MaxSnowInterception)," m\n")
res5 = paste0("Inferred 90th percentile snow interception: ",round(SnowIntCap90th,3)," m\n")
cat(c(res1,res2,res3,res4,res5))

cor(LAIsummerMapLANDIS[LAIptrs], CanopyMapLANDIS[LAIptrs])

################################################################################
# Part 6: Write LANDIS-based vegetation section of config file
################################################################################

################################################################################
# Preprocess albedo data from MODIS

AlbedoData = read.csv("AlbedoByMonthNIR.csv")

# Make albedo map for illustration purposes and to calculate the mean total albedo (proxy for ground albedo)
AlbedoMap = NA*Mask

for (i in unique(AlbedoData$ClassLANDIS)){
  Ptrs = which(IDmapLANDIS[,]==i)
  AlbedoMap[Ptrs] = mean(AlbedoData[AlbedoData$ClassLANDIS==i,]$Albedo)
}

plot(AlbedoMap)

writeRaster(AlbedoMap,"TCSI_Year0_Albedo.tif",overwrite=TRUE)

print(mean(AlbedoMap[is.finite(AlbedoMap[,])]))
print(median(AlbedoMap[is.finite(AlbedoMap[,])]))

# Make mean monthly albedo for each LANDIS overstory class, regardless of understory
MonthlyAlbedo = matrix(nrow=nLANDISclasses,ncol=12)
BaseClasses = as.integer(unique(ClassSettings$ClassLANDIS)[seq(1, nLANDISclasses, by=2)])
print(BaseClasses)
idx = 1
for (i in 1:length(BaseClasses)){
  cls = BaseClasses[i]
  for (mon in 1:12){
    Alb1 = AlbedoData[(AlbedoData[,"ClassLANDIS"]==cls & AlbedoData[,"Month"]==mon),]$Albedo
    Alb2 = AlbedoData[(AlbedoData[,"ClassLANDIS"]==(cls+1) & AlbedoData[,"Month"]==mon),]$Albedo
    AlbedoVal = mean(c(Alb1, Alb2))
    MonthlyAlbedo[idx,mon] = AlbedoVal
    MonthlyAlbedo[(idx+1),mon] = AlbedoVal
  }
  idx = idx + 2
}

################################################################################
# Preprocess stomatal conductance data

DEM = raster("../SRTM/TCSIdomain_90mDEMfilled.tif")
MedElev = median(DEM[Mask[,]==1])
MeanPress = pressure.from.elevation(elev=MedElev, Tair=25)

# Wetland minimum stomatal resistance estimated from average of streamside and mid-meadow locations from Svejcar (1998)
WetlandConduc = mean(c(357, 327))/1000 # mmol/(m^2-s) --> mol/(m^2-s)
WetlandResist = 1/mol.to.ms(G_mol=WetlandConduc, Tair=25, pressure=MeanPress) # mol/(m^2-s) --> s/m
print(paste0("Wetland minimum resistance = ",round(WetlandResist)," s/m"))

# Following values provided by Running (1976) in conductance units of cm/s
# Convert <back> to the mol/(m^2-s) units for easy incorporation in the processing pipeline below
DouglasFir_cm_s = 0.333
PonderosaPine_cm_s = 0.323
RedFir_cm_s = 0.476
print(paste0("Douglas Fir max conductance, mmol/(m^2-s) = ",round(ms.to.mol(G_ms=DouglasFir_cm_s/100, Tair=25, pressure=MeanPress)*1000)))
print(paste0("Ponderosa Pine max conductance, mmol/(m^2-s) = ",round(ms.to.mol(G_ms=PonderosaPine_cm_s/100, Tair=25, pressure=MeanPress)*1000)))
print(paste0("Red Fir max conductance, mmol/(m^2-s) = ",round(ms.to.mol(G_ms=RedFir_cm_s/100, Tair=25, pressure=MeanPress)*1000)))

# Following values provided by Carter et al. (1988) in conductance units of mm/s
# Convert <back> to the mol/(m^2-s) units for easy incorporation in the processing pipeline below
LodgepolePine_mm_s = 5.5
SubalpineFire_mm_s = 4
print(paste0("Lodgepole Pine max conductance, mmol/(m^2-s) = ",round(ms.to.mol(G_ms=LodgepolePine_mm_s/1000, Tair=25, pressure=MeanPress)*1000)))
print(paste0("Subalpine Fir max conductance, mmol/(m^2-s) = ",round(ms.to.mol(G_ms=SubalpineFire_mm_s/1000, Tair=25, pressure=MeanPress)*1000)))

# Following values provided by Conrad et al. (1997) in conductance units of cm/s
# Convert <back> to the mol/(m^2-s) units for easy incorporation in the processing pipeline below
Apatula_cm_s = 1
Hdiscolor_cm_s = 0.8
print(paste0("A. patula max conductance, mmol/(m^2-s) = ",round(ms.to.mol(G_ms=Apatula_cm_s/100, Tair=25, pressure=MeanPress)*1000)))
print(paste0("H. discolor max conductance, mmol/(m^2-s) = ",round(ms.to.mol(G_ms=Hdiscolor_cm_s/100, Tair=25, pressure=MeanPress)*1000)))

# Following values provided by Yoder (1983) in conductance units of cm/s
# Convert <back> to the mol/(m^2-s) units for easy incorporation in the processing pipeline below
WhiteFirCACO_cm_s = 0.56
WhiteFirCOCO_cm_s = 0.33
print(paste0("White Fir CAxCO max conductance, mmol/(m^2-s) = ",round(ms.to.mol(G_ms=WhiteFirCACO_cm_s/100, Tair=25, pressure=MeanPress)*1000)))
print(paste0("White Fir COxCO max conductance, mmol/(m^2-s) = ",round(ms.to.mol(G_ms=WhiteFirCOCO_cm_s/100, Tair=25, pressure=MeanPress)*1000)))

# Following value provided by Tan et al. (1978) in <resistance> units of s/cm
# Convert <back> to the mol/(m^2-s) units for easy incorporation in the processing pipeline below
DouglasFir_s_cm = 4
print(paste0("Douglas Fir max conductance, mmol/(m^2-s) = ",round(ms.to.mol(G_ms=(1/DouglasFir_s_cm)/100, Tair=25, pressure=MeanPress)*1000)))

# Following value provided by Fetcher (1976) in <resistance> units of s/cm
# Convert <back> to the mol/(m^2-s) units for easy incorporation in the processing pipeline below
LodgepolePine_s_cm = 1.3
print(paste0("Lodgepole Pine max conductance, mmol/(m^2-s) = ",round(ms.to.mol(G_ms=(1/LodgepolePine_s_cm)/100, Tair=25, pressure=MeanPress)*1000)))

################################################################################
# Minimum conductance values are analyzed and aggregated by hand in MaxConductance.xlsx
# These values were then manually inserted in VegClassSettings.csv
ClassSettings = read.csv("VegClassSettings.csv")
print(ClassSettings)

# Create datafrom of minimum resistance by class
MinRes = rep(NA,nLANDISclasses)
for (i in 1:nLANDISclasses){
  ClassConduc = as.numeric(ClassSettings$MaxConductance[i])/1000 # mmol/(m^2-s) --> mol/(m^2-s)
  MinRes[i] = 1/mol.to.ms(G_mol=ClassConduc, Tair=25, pressure=MeanPress) # mol/(m^2-s) --> s/m
}

MinimumResistance = data.frame(VegClass=1:nLANDISclasses,MINRES=MinRes)

# Add entry for wetland resistance -- CAUTION this is somewhat hard-coded!
MinimumResistance = rbind(MinimumResistance, c(ClassSettings$ClassDHSVM[ClassSettings$ClassName=="Wetland"], WetlandResist))

# Round to the nearest 1 s/m
MinimumResistance$MINRES = round(MinimumResistance$MINRES)

print(MinimumResistance)
hist(MinimumResistance$MINRES)

write.csv(MinimumResistance,"MINRESbaseline_TCSI.csv")

################################################################################
# Write config section with remaining placeholders for calibration of minimum stomatal resistance

ConfigTemplateLANDIS = readLines("ConfigTemplateLANDIS.txt")

ConfigSection = c()

for (i in seq(1, nLANDISclasses, by=2)){
  # Substitute class-wise values into the template
  ConfigTemp = ConfigTemplateLANDIS
  
  # Class number
  ConfigTemp = gsub(pattern="NUM1", replace=as.character(i), x=ConfigTemp)
  ConfigTemp = gsub(pattern="NUM2", replace=as.character(i+1), x=ConfigTemp)
  
  # Class name (vegetation type and understory presence)
  ConfigTemp = gsub(pattern="NAME1", replace=ClassSettings$ClassName[i], x=ConfigTemp)
  ConfigTemp = gsub(pattern="NAME2", replace=ClassSettings$ClassName[i+1], x=ConfigTemp)
  
  # Trunk space
  ConfigTemp = gsub(pattern="TRUNK", replace=as.character(ClassSettings$TrunkSpace[i]), x=ConfigTemp)
  
  # Overstory albedo
  for (mon in 1:12){
    ConfigTemp = gsub(pattern=paste0("ALB",mon,"."), replace=as.character(round(MonthlyAlbedo[i,mon],2)), x=ConfigTemp, fixed=TRUE)
    #print(ConfigTemp)
    # Need to include period after the number to prevent gsub from overwriting ALB10 with ALB1
  }
  
  # Rooting depths
  ConfigTemp = gsub(pattern="RD1", replace=as.character(ClassSettings$RootThickness1[i]), x=ConfigTemp)
  ConfigTemp = gsub(pattern="RD2", replace=as.character(ClassSettings$RootThickness2[i]), x=ConfigTemp)
  ConfigTemp = gsub(pattern="RD3", replace=as.character(ClassSettings$RootThickness3[i]), x=ConfigTemp)
  
  # Root fractions
  ConfigTemp = gsub(pattern="RF1", replace=as.character(ClassSettings$RootFrac1[i]), x=ConfigTemp)
  ConfigTemp = gsub(pattern="RF2", replace=as.character(ClassSettings$RootFrac2[i]), x=ConfigTemp)
  ConfigTemp = gsub(pattern="RF3", replace=as.character(ClassSettings$RootFrac3[i]), x=ConfigTemp)
  
  # Light extinction coefficient
  ConfigTemp = gsub(pattern="LEX", replace=as.character(ClassSettings$LightExtinctionCoeff[i]), x=ConfigTemp)
  
  # The Mixed Riparian class is assumed to have wetland-type understory
  if (grepl("Riparian",ClassSettings$ClassName[i])){
    ConfigTemp = gsub(pattern="MINRES28", replace="MINRES34", x=ConfigTemp)
  }
  
  # Append new class to config section
  ConfigSection = c(ConfigSection,"\n",ConfigTemp)
}

writeLines(c(readLines("ConfigVegHeader.txt"),ConfigSection,readLines("ConfigVegFooter.txt")), "ConfigSectionLANDIS_Calibrate.txt")

################################################################################
# Add minimum stomatal resistance values to veg section

# ConfigSectionLANDIS_Calibrate.txt will be manually merged with remaining config file, including non-LANDIS veg sections

ConfigParam = readLines("ConfigSectionLANDIS_Calibrate.txt")

MINRESbaseline = read.csv("MINRESbaseline_TCSI.csv")
print(MINRESbaseline)

nVegClasses = length(MINRESbaseline$VegClass) # Only refers to the number of stomatal resistance parameters to adjust

for (cls in 1:nVegClasses){
  ClassMinRes = MINRESbaseline$MINRES[cls]
  ConfigParam = gsub(pattern=paste0("MINRES",MINRESbaseline$VegClass[cls],"."), replace=as.character(round(ClassMinRes,0)), x=ConfigParam, fixed=TRUE)
}

writeLines(ConfigParam, "ConfigSectionLANDIS_Baseline.txt")


