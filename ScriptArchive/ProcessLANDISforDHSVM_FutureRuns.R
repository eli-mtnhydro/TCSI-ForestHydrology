# Convert LANDIS output maps into usable inputs for DHSVM

library(raster)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/LANDIS/"
setwd(dir)

landisdir = "LANDIS_FutureMaps/"
processdir = "Processed_FutureMaps/"
dhsvmdir = "DHSVM_FutureMaps/"

ClassSettings = read.csv("VegClassSettings.csv")
print(ClassSettings)

nLANDISclasses = 28

Mask = raster("../WatershedBoundaries/TCSImask_MergedDomain.tif")
DEM = raster("../SRTM/TCSIdomain_90mDEMfilled.tif")

nRows = 1436 # Number of rows in the domain, used to write text files
nHeaderLines = 6 # Number of rows in a .asc raster header

################################################################################
# Set up LANDIS map sets, e.g., combos of scenarios/climates/run/year
################################################################################

# LANDIS output maps have spaces in filenames; fix this if necessary first:
# https://stackoverflow.com/questions/11270453/how-to-remove-spaces-from-file-names-in-bulk
# In Windows:
# Open a Command Prompt.
# Go to the folder with the cd command (eg.: cd "path of your folder").
# Open a powershell by typing: powershell
# Then input this: get-childitem *.tif | foreach {rename-item $_ $_.name.replace(" ","")}

# For Scenario 7, also running the following commands for name consistency:
# get-childitem *.tif | foreach {rename-item $_ $_.name.replace("sdi35-cnrm","cnrm-Run1")}
# get-childitem *.tif | foreach {rename-item $_ $_.name.replace("sdi35-miroc","miroc-Run4")}

StartYear = 2020 - 5 - 1 # Shift back halfway, then shift to water year (10/1/2014 is WY 2015)

MapScenarios = 7
# Climates = c("cnrm")
# ClimateNames = c("CNRM-CM5_RCP8.5")
Climates = c("miroc")
ClimateNames = c("MIROC5_RCP8.5")
# LANDISruns = 1
LANDISruns = 4
Years = seq(0,80,10)

MapSets = apply(expand.grid("Scenario", MapScenarios, "_",
                            ClimateNames, "_",
                            "Run", LANDISruns, "_",
                            StartYear + Years),
                1, paste0, collapse="")

InFilePrefixes = rep(apply(expand.grid("Scenario", MapScenarios, "-",
                                     Climates, "-",
                                     "Run", LANDISruns, "-"),
                           1, paste0, collapse=""),
                   length(Years))

InFileSuffixes = paste0("-",sort(rep(Years, length(InFilePrefixes)/length(Years))),".tif")

DHSVMprefixes = rep(apply(expand.grid("S", MapScenarios, Climates, "R", LANDISruns,
                                   "/Vegetation."),
                       1, paste0, collapse=""),
                 length(Years))

DHSVMsuffixes = paste0(".10.01.",sort(rep(StartYear + Years, length(InFilePrefixes)/length(Years))),".00.00.00.txt")

length(MapSets)
length(InFilePrefixes)
length(InFileSuffixes)
length(DHSVMprefixes)
length(DHSVMsuffixes)

# Make DHSVM directories
DHSVMdirs = sub("/.*", "", unique(DHSVMprefixes))
for (i in 1:length(DHSVMdirs)){
  dir.create(paste0(dhsvmdir,DHSVMdirs[i]))
}

################################################################################
# Final setup and mega-loop over each map in MapSets
################################################################################

# Set up LANDIS and NLCD numbering systems
# This excludes the Alpine Shrub class, which is handled later
# -999 signifies that LANDIS or NLCD do not include the respective DHSVM class
VegClasses = read.csv("ReclassifyVeg.csv")
print(VegClasses)

# Set up static filler data (where LANDIS ~ NA)
VegIDfiller = raster("TCSI_Year0_VegID.tif")
CanopyCoverFiller = raster("TCSI_Year0_CanopyCover.tif")
HeightFiller = raster("TCSI_Year0_CanopyHeightOverstory.tif")
LAIfiller = raster("TCSI_Year0_LAIsummer.tif")

# Set up a consistent mask to make sure the LANDIS vegetation stays where it should
LANDISmask = raster("TCSI_Year0_VegID.tif")
LANDISmask[LANDISmask < 29] = 1
LANDISmask[LANDISmask >= 29] = NA
plot(LANDISmask)

for (map in 1:length(MapSets)){
  
  print(paste0("*** Input map set: ", InFilePrefixes[map], "X", InFileSuffixes[map], " ***"))
  print(paste0("*** Output map set: ", DHSVMprefixes[map], "X", DHSVMsuffixes[map], " ***"))
  
  ##############################################################################
  # Part 1: Veg ID
  ##############################################################################
  
  VegIDmap = NA*Mask # Keep track of which pixels haven't been satisfactorily filled
  
  # Use LANDIS data wherever it is available
  IDmapLANDIS = raster(paste0(landisdir, InFilePrefixes[map], "veg-ID", InFileSuffixes[map]))
  IDmapLANDIS = suppressWarnings(projectRaster(IDmapLANDIS, Mask, method="ngb")) # Warning is fine--same CRS, different extent
  IDmapLANDIS = IDmapLANDIS * LANDISmask # Fix variable reprojection issues
  #plot(IDmapLANDIS)
  
  # Reclassify numbering scheme and fill map
  for (i in 1:length(VegClasses$ClassLANDIS)){
    Ptrs = which(IDmapLANDIS[,] == VegClasses$ClassLANDIS[i])
    VegIDmap[Ptrs] = VegClasses$ClassDHSVM[i]
    #print(sum(as.numeric(Ptrs)))
  }
  
  # Fill with static data
  StaticPtrs = which(is.na(VegIDmap[,]))
  VegIDmap[StaticPtrs] = VegIDfiller[StaticPtrs]
  
  #plot(VegIDmap)
  
  writeRaster(VegIDmap,paste0(processdir,MapSets[map],"_VegID.tif"),overwrite=TRUE)
  
  # Convert to the DHSVM header-less format
  writeRaster(VegIDmap,"veg.asc",datatype="INT2U",overwrite=TRUE) # Must write as integers to prevent adding decimal to first value
  TextRaster = readLines("veg.asc")
  writeLines(TextRaster[(nHeaderLines+1):(nHeaderLines+nRows)],paste0(dhsvmdir,DHSVMprefixes[map],"Type",DHSVMsuffixes[map]))
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
  
  # plot(OverstoryPresent)
  # plot(UnderstoryPresent)
  
  # writeRaster(UnderstoryPresent,"TCSI_Year0_UnderstoryMask.tif",overwrite=TRUE)
  # writeRaster(OverstoryPresent,"TCSI_Year0_OverstoryMask.tif",overwrite=TRUE)
  
  ##############################################################################
  # Part 2: Canopy Cover
  ##############################################################################
  
  CanopyCoverMap = NA*Mask
  
  # Use LANDIS data wherever it is available
  CanopyMapLANDIS = raster(paste0(landisdir, InFilePrefixes[map], "cc_continuous", InFileSuffixes[map]))
  CanopyMapLANDIS = suppressWarnings(projectRaster(CanopyMapLANDIS, Mask, method="ngb")) # Warning is fine--same CRS, different extent
  CanopyMapLANDIS = CanopyMapLANDIS * LANDISmask # Fix variable reprojection issues
  #plot(CanopyMapLANDIS)
  #hist(CanopyMapLANDIS[,])
  #max(CanopyMapLANDIS[which(!is.na(CanopyMapLANDIS[,]))])
  #print(1/max(CanopyMapLANDIS[which(!is.na(CanopyMapLANDIS[,]))]))
  
  Ptrs = which(!is.na(CanopyMapLANDIS[,]))
  CanopyCoverMap[Ptrs] = CanopyMapLANDIS[Ptrs]
  
  # Fill with static data
  StaticPtrs = which(is.na(CanopyCoverMap[,]))
  CanopyCoverMap[StaticPtrs] = CanopyCoverFiller[StaticPtrs]
  
  #plot(CanopyCoverMap)
  #hist(CanopyCoverMap[,])
  #sum(as.numeric(is.na(CanopyCoverMap[,] & OverstoryPresent[,]==1)))/length(CanopyCoverMap[,])
  
  writeRaster(CanopyCoverMap,paste0(processdir,MapSets[map],"_CanopyCover.tif"),overwrite=TRUE)
  
  # Convert to the DHSVM header-less format
  writeRaster(CanopyCoverMap,"veg_fc.asc",overwrite=TRUE)
  TextRaster = readLines("veg_fc.asc")
  writeLines(TextRaster[(nHeaderLines+1):(nHeaderLines+nRows)],paste0(dhsvmdir,DHSVMprefixes[map],"FC",DHSVMsuffixes[map]))
  file.remove("veg_fc.asc")
  
  ##############################################################################
  # Part 3: Veg Height (overstory and understory)
  ##############################################################################
  
  OverstoryHeightMap = NA*Mask
  
  # Use LANDIS data wherever it is available
  HeightMapLANDIS = raster(paste0(landisdir, InFilePrefixes[map], "can-ht", InFileSuffixes[map]))
  HeightMapLANDIS = suppressWarnings(projectRaster(HeightMapLANDIS, Mask, method="ngb")) # Warning is fine--same CRS, different extent
  HeightMapLANDIS = HeightMapLANDIS * LANDISmask # Fix variable reprojection issues
  #plot(HeightMapLANDIS)
  
  Ptrs = which(!is.na(HeightMapLANDIS[,]))
  OverstoryHeightMap[Ptrs] = HeightMapLANDIS[Ptrs]
  
  # Fill with static data
  StaticPtrs = which(is.na(OverstoryHeightMap[,]))
  OverstoryHeightMap[StaticPtrs] = HeightFiller[StaticPtrs]
  
  #plot(OverstoryHeightMap,zlim=c(0,50))
  
  # Print max veg height so we can determine the wind reference level (max height + 10 m)
  #print(round(max(OverstoryHeightMap[,])))
  
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
  
  #plot(UnderstoryHeightMap,zlim=c(0,1))
  
  writeRaster(OverstoryHeightMap,paste0(processdir,MapSets[map],"_CanopyHeightOverstory.tif"),overwrite=TRUE)
  writeRaster(UnderstoryHeightMap,paste0(processdir,MapSets[map],"_CanopyHeightUnderstory.tif"),overwrite=TRUE)
  
  # Convert to the DHSVM header-less format and concatenate overstory/understory
  writeRaster(OverstoryHeightMap,"veg_height_overstory.asc",overwrite=TRUE)
  writeRaster(UnderstoryHeightMap,"veg_height_understory.asc",overwrite=TRUE)
  TextRaster1 = readLines("veg_height_overstory.asc")
  TextRaster2 = readLines("veg_height_understory.asc")
  writeLines(c(TextRaster1[(nHeaderLines+1):(nHeaderLines+nRows)],TextRaster2[(nHeaderLines+1):(nHeaderLines+nRows)]),
             paste0(dhsvmdir,DHSVMprefixes[map],"Height",DHSVMsuffixes[map]))
  file.remove("veg_height_overstory.asc")
  file.remove("veg_height_understory.asc")
  
  ##############################################################################
  # Part 4: Veg LAI (monthly)
  ##############################################################################
  
  LAIsummerMap = NA*Mask
  
  # Use LANDIS data wherever it is available
  LAIsummerMapLANDIS = raster(paste0(landisdir, InFilePrefixes[map], "LAI_tree", InFileSuffixes[map]))
  LAIsummerMapLANDIS = suppressWarnings(projectRaster(LAIsummerMapLANDIS, Mask, method="ngb")) # Warning is fine--same CRS, different extent
  LAIsummerMapLANDIS = LAIsummerMapLANDIS * LANDISmask # Fix variable reprojection issues
  #plot(LAIsummerMapLANDIS)
  
  # Fill with LANDIS data if it indicates positive LAI (don't allow 0.0 LAI in cells where overstory = present)
  LAIsummerMapLANDIS[!(LAIsummerMapLANDIS[,]>0)] = NA
  Ptrs = which(!is.na(LAIsummerMapLANDIS[,]))
  LAIsummerMap[Ptrs] = LAIsummerMapLANDIS[Ptrs]
  #plot(LAIsummerMap)
  
  # Fill with static data
  StaticPtrs = which(is.na(LAIsummerMap[,]))
  LAIsummerMap[StaticPtrs] = LAIfiller[StaticPtrs]
  
  #plot(LAIsummerMap,zlim=c(0,14))
  
  ################################################################################
  # Apply scale factors from Sam/LANDIS to convert summer LAI into winter LAI
  
  LAIwinterMap = NA*Mask
  
  # LAI annual fluctuation data directly from LANDIS stored LAIwinterScale column
  # His note: LAI in NECN is only calculated from leaves -- I replaced any zero values with 0.2 (Fang et al. 2019).
  #head(ClassSettings)
  
  for (i in ClassSettings$ClassDHSVM){
    # Only calculate LAI changes for overstory, since only overstory LAI is read from the map
    if (ClassSettings$Overstory[i]){
      Ptrs = which(VegIDmap[,]==i)
      LAIwinterMap[Ptrs] = LAIsummerMap[Ptrs] * ClassSettings$LAIwinterScale[i]
    }
  }
  
  #plot(LAIwinterMap,zlim=c(0,14))
  #plot(LAIsummerMap-LAIwinterMap)
  #hist(LAIwinterMap[OverstoryPresent[,]==1]/LAIsummerMap[OverstoryPresent[,]==1])
  
  writeRaster(LAIsummerMap,paste0(processdir,MapSets[map],"_LAIsummer.tif"),overwrite=TRUE)
  writeRaster(LAIwinterMap,paste0(processdir,MapSets[map],"_LAIwinter.tif"),overwrite=TRUE)
  
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
  
  #length(TextRaster)/12
  
  writeLines(TextRaster,paste0(dhsvmdir,DHSVMprefixes[map],"LAI",DHSVMsuffixes[map]))
  file.remove("veg_lai_summer.asc")
  file.remove("veg_lai_winter.asc")
  
  # End of mega loop over map sets
  print(paste0("*** Done with ", MapSets[map], " ***"))
  print("*")
}


