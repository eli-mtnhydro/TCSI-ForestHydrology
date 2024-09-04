### MOBOhydro CALIBRATOR FOR DHSVM ###
# Session 4: Post-process DHSVM outputs to calculate objective functions

################################################################################
# Universal adjustable settings

ModelGen = 6

ParamNames = c("SOILD","SLHC","POROSITY","EXPDEC","MINRES","SNOWTEMP","ALBMELTRATE")
nParams = length(ParamNames) # Number of parameters in the calibration

ObjFunNames = c("PeakSWE_RMSE","Total_NSE","LowBase_LogNSE","HighBase_LogNSE","WaterYield_RMSE","PeakFlow_RMSE")
nObjFuns = length(ObjFunNames)

BasinNames = c("Truckee","Yuba")

# Set up the water years over which different objective functions will be evaluated
SnowYears = 2011:2016
StreamYears = 2012:2017
nSnowYears = length(SnowYears)
nStreamYears = length(StreamYears)

ModelInterval = 3 # hours

ResultsDir = "Results/" # Where the files created by DHSVM are stored
ReferenceDir = "ReferenceData/" # Where the files used as ground-truth are stored

dir = "C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/Calibrate_MOBOhydro/"
setwd(dir)

# For converting DHSVM-style header-less ascii rasters to regular rasters
ASCIIheader = readLines("ASCIIheader.txt")
nRows = 1436
nCols = 2247

################################################################################
# Get matrix of new designs and previous results (if applicable)
################################################################################

if (ModelGen > 1){
  # Read both current and last generation designs so we only make config files/maps for the new parameter sets
  design.grid.lastgen = read.csv(paste0("Generation",ModelGen-1,"_DesignGrid.csv"))[,-1]
  design.grid.thisgen = read.csv(paste0("Generation",ModelGen,"_DesignGrid.csv"))[,-1]
  
  nDesignsLastGen = dim(design.grid.lastgen)[1]
  nDesignsThisGen = dim(design.grid.thisgen)[1]
  
  # Subset novel designs
  design.grid = design.grid.thisgen[(nDesignsLastGen+1):(nDesignsThisGen),]
  
  # Read previous responses to avoid recalculating them (in the native units, pre-scaling)
  results.grid.lastgen = read.csv(paste0("Generation",ModelGen-1,"_ResultsGrid.csv"))[,-1]
  
} else {
  # Read the first generation design grid
  design.grid = read.csv(paste0("Generation",ModelGen,"_DesignGrid.csv"))[,-1]
  nNewDesigns = length(design.grid[,1])
}

nNewDesigns = length(design.grid[,1])
print(nNewDesigns)

# Set up the structure to save data from the current generation
results.grid.thisgen = data.frame(matrix(nrow=nNewDesigns,ncol=(nObjFuns+1)))
names(results.grid.thisgen) = c("ParamSet",ObjFunNames)
results.grid.thisgen$ParamSet = design.grid$ParamSet
head(results.grid.thisgen)

################################################################################
# Post-process DHSVM results to get objective function values (pre-scaling)
################################################################################

####################### PART 1: SNOW MAP PROCESSING ############################

# Only keep max SWE dates if max SWE > 5 cm to exclude small storms that could have competing peaks far apart
# MaxSWEthresh = 0.05

library(raster)

# Read mask of calibration watersheds to make sure everything stays in the right CRS/extent/etc.
# NOTE also that this is how we determine which pixels get compared--this mask ONLY includes the calibration watersheds!
MaskDir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/WatershedBoundaries/"
WatershedMasks = lapply(paste0(MaskDir,"TCSImask_",BasinNames,"Domain.tif"), raster)
Mask = WatershedMasks[[1]]
for (bsn in 1:length(BasinNames)){
  Mask = max(Mask,WatershedMasks[[bsn]])
}
plot(Mask)

# Extract crs of the study domain for application to the ascii rasters later
MaskCRS = crs(Mask)

# Also read the vegetation ID map so we can mask out water (e.g., Lake Tahoe) from SWE comparisons
WaterClass = 29
VegDir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/LANDIS/"
VegIDmap = raster(paste0(VegDir,"TCSI_Year0_VegID.tif"))
WaterMask = VegIDmap*0 + (VegIDmap[,] != 29)*1

Mask = Mask * WaterMask
plot(Mask)

# One-time setup of Margulis SWE rasters
if (FALSE){
  # Read original Margulis pixel-wise max SWE rasters and max SWE date rasters
  ReferenceSWErasters = lapply(paste0(ReferenceDir,"max_swe_WY",SnowYears,".tif"), raster)
  # ReferenceMaxDateRasters = lapply(paste0(ReferenceDir,"max_swe_dowy_WY",SnowYears,".tif"), raster)
  
  # Reproject to model domain
  ReferenceSWErasters = lapply(ReferenceSWErasters, function(x) projectRaster(x, to=Mask, method="bilinear"))
  # ReferenceMaxDateRasters = lapply(ReferenceMaxDateRasters, function(x) projectRaster(x, to=Mask, method="ngb"))
  
  # Convert mm --> m
  print(max(ReferenceSWErasters[[1]][,],na.rm=TRUE))
  ReferenceSWErasters = lapply(ReferenceSWErasters, function(x) x/1000)
  print(max(ReferenceSWErasters[[1]][,],na.rm=TRUE))
  
  par(mfrow=c(3,2))
  for (yr in 1:nSnowYears){
    plot(ReferenceSWErasters[[yr]],main=paste0("Margulis Max SWE WY ",SnowYears[yr]),zlim=c(0,3))
  }
  # for (yr in 1:nSnowYears){
  #   plot(ReferenceMaxDateRasters[[yr]],main=paste0("Margulis Max SWE Date WY ",SnowYears[yr]),zlim=c(50,250))
  # }
  
  for (yr in 1:nSnowYears){
    writeRaster(ReferenceSWErasters[[yr]], paste0(ReferenceDir,"TCSI_max_swe_WY",SnowYears[yr],".tif"))
    # writeRaster(ReferenceMaxDateRasters[[yr]], paste0(ReferenceDir,"TCSI_max_swe_dowy_WY",SnowYears[yr],".tif"))
  }
}

# Read pre-processed Margulis pixel-wise max SWE rasters and max SWE date rasters
ReferenceSWErasters = lapply(paste0(ReferenceDir,"TCSI_max_swe_WY",SnowYears,".tif"), raster)
# ReferenceMaxDateRasters = lapply(paste0(ReferenceDir,"TCSI_max_swe_dowy_WY",SnowYears,".tif"), raster)

# Find which cells in the modeled basins have calibration data available for each water year
# Also make vectors containing reference values to be compared with RMSE
SWEptrsByYear = list()
SWEreferenceVals = c()
# DatePtrsByYear = list()
# DateReferenceVals = c()
for (yr in 1:nSnowYears){
  SWEptrsByYear[[yr]] = which(is.finite(ReferenceSWErasters[[yr]][,]) & (Mask[,] > 0))
  SWEreferenceVals = c(SWEreferenceVals, ReferenceSWErasters[[yr]][SWEptrsByYear[[yr]]])
  print(length(SWEptrsByYear[[yr]]))
  
  # DatePtrsByYear[[yr]] = which(is.finite(ReferenceMaxDateRasters[[yr]][,]) & (ReferenceSWErasters[[yr]][,] > MaxSWEthresh) & (Mask[,] > 0))
  # DateReferenceVals = c(DateReferenceVals, ReferenceMaxDateRasters[[yr]][DatePtrsByYear[[yr]]])
  # print(length(DatePtrsByYear[[yr]]))
}

# Make mask for nice plotting
VisMask = Mask*0
VisMask[SWEptrsByYear[[1]]] = 1
plot(VisMask)

timeTotal = 0

# Loop over each parameter set and calculate the objective function value
for (i in results.grid.thisgen$ParamSet){
  
  # Check whether results are available for this parameter set
  if (!all(dir.exists(paste0(ResultsDir,"Params",i,"_",BasinNames)))){
    
    results.grid.thisgen[results.grid.thisgen$ParamSet==i,]$PeakSWE_RMSE = NA
    # results.grid.thisgen[results.grid.thisgen$ParamSet==i,]$PeakDate_RMSE = NA
    
    print("*")
    print(paste0("Param Set: ",i," Not Found"))
    
  } else {
    timeStart = Sys.time()
    
    # Reset the vectors containing modeled values to be compared with reference data
    DHSVMsweVals = c()
    # DHSVMdateVals = c()
    
    # Read the concatenated ascii files output by myconvert.c
    # Each basin's raster stack is stored together in a list
    RawRasterStackSWE = lapply(paste0(ResultsDir,"Params",i,"_",BasinNames,"/Map.Snow.MaxSwe.txt"), readLines)
    # RawRasterStackDate = lapply(paste0(ResultsDir,"Params",i,"_",BasinNames,"/Map.Snow.MaxSweDate.txt"), readLines)
    
    # Loop over each year with snow data
    for (yr in 1:nSnowYears){
      # Loop over each watershed
      for (bsn in 1:length(BasinNames)){
        # Subset a raster from the stack
        SingleMapSWE = RawRasterStackSWE[[bsn]][((yr-1)*nRows+1):(nRows*yr)]
        # SingleMapDate = RawRasterStackDate[[bsn]][((yr-1)*nRows+1):(nRows*yr)]
        
        # Write single ascii raster and read it back in as a raster object
        writeLines(c(ASCIIheader,SingleMapSWE),paste0("tempSWE",bsn,".asc"))
        SingleRasterSWE = raster(paste0("tempSWE",bsn,".asc"), crs=as.character(MaskCRS))
        
        # writeLines(c(ASCIIheader,SingleMapDate),paste0("tempDate",bsn,".asc"))
        # SingleRasterDate = raster(paste0("tempDate",bsn,".asc"), crs=as.character(MaskCRS))
        
        # Convert date as YYYYMMDD --> day of water year
        # SingleRasterDates = as.Date(as.character(SingleRasterDate[,]),format="%Y%m%d")
        # DayOfYear = as.numeric(strftime(SingleRasterDates,format="%j"))
        # DayOfWaterYear = DayOfYear + 92 # Day of year --> day of water year
        # DayOfWaterYear[is.na(DayOfWaterYear)] = 0
        # WaterYear = max(as.numeric(format(SingleRasterDates[!is.na(SingleRasterDates)],"%Y")))
        # EndOfWaterYear = as.numeric(strftime(as.Date(paste0(WaterYear,"-09-30"),format="%Y-%m-%d"),format="%j"))
        # DaysThisYear = as.numeric(strftime(as.Date(paste0(WaterYear,"-12-31"),format="%Y-%m-%d"),format="%j"))
        # DayOfWaterYear[DayOfWaterYear > EndOfWaterYear] = DayOfWaterYear[DayOfWaterYear > EndOfWaterYear] - DaysThisYear # Fix rollover where Oct-Jan are >365
        # SingleRasterDate[,] = DayOfWaterYear
        
        # Merge watersheds by keeping the maximum value
        if (bsn==1){
          MergedRasterSWE = SingleRasterSWE
          # MergedRasterDate = SingleRasterDate
        } else {
          MergedRasterSWE = max(MergedRasterSWE, SingleRasterSWE)
          # MergedRasterDate = max(MergedRasterDate, SingleRasterDate)
        }
        
        print(paste0("Processed ",BasinNames[bsn]," Water Year ",SnowYears[yr]))
      }
      
      # Append the modeled values to the vector that will be compared with the refence values
      DHSVMsweVals = c(DHSVMsweVals, MergedRasterSWE[SWEptrsByYear[[yr]]])
      # DHSVMdateVals = c(DHSVMdateVals, MergedRasterDate[DatePtrsByYear[[yr]]])
      
      # par(mfrow=c(2,2), mar=c(2,2,5,2))
      # plot(VisMask*MergedRasterSWE,main=paste0("DHSVM Max SWE WY ",SnowYears[yr]),zlim=c(0,3))
      # plot(VisMask*ReferenceSWErasters[[yr]],main=paste0("Margulis Max SWE WY ",SnowYears[yr]),zlim=c(0,3))
      # plot(VisMask*MergedRasterDate,main=paste0("DHSVM Max SWE Date WY ",SnowYears[yr]),zlim=c(0,250))
      # plot(VisMask*ReferenceMaxDateRasters[[yr]],main=paste0("Margulis Max SWE Date WY ",SnowYears[yr]),zlim=c(0,250))
      # 
    }
    
    # Calculate RMSE for each pixel-year pair and save it to the results grid
    RMSEvalSWE = sqrt(mean((DHSVMsweVals - SWEreferenceVals)^2))
    results.grid.thisgen[results.grid.thisgen$ParamSet==i,]$PeakSWE_RMSE = RMSEvalSWE
    
    # RMSEvalDate = sqrt(mean((DHSVMdateVals - DateReferenceVals)^2))
    # results.grid.thisgen[results.grid.thisgen$ParamSet==i,]$PeakDate_RMSE = RMSEvalDate
    
    print("*")
    print(paste0("Max SWE RMSE = ",signif(RMSEvalSWE,2)," m for Param Set: ",i))
    # print(paste0("Max SWE Date RMSE = ",round(RMSEvalDate)," days for Param Set: ",i))
    
    timeEnd = Sys.time()
    timeElapsed = as.numeric(difftime(timeEnd,timeStart,units="secs"))
    timeTotal = timeTotal + timeElapsed
    timeAvg = timeTotal/i
    print("*")
    print(paste0("Snow evaluation took ",round(timeElapsed/60,1)," minutes."))
    print(paste0("Full evaluation expected to take another ",round(timeAvg*(tail(design.grid$ParamSet,1)-i)/60)," minutes for ",tail(design.grid$ParamSet,1)-i," more parameter sets."))
  }
  
  # par(mfrow=c(3,1), mar=rep(4,4))
  # hist(SWEreferenceVals,main="Reference Values")
  # hist(DHSVMsweVals,main="DHSVM Values")
  # hist(abs(DHSVMsweVals - SWEreferenceVals),main="Absolute Error")
  # 
  # hist(DateReferenceVals,main="Reference Values")
  # hist(DHSVMdateVals,main="DHSVM Values")
  # hist(abs(DHSVMdateVals - DateReferenceVals),main="Absolute Error")
  
  print(paste0("************* DONE WITH SNOW FOR PARAMS ",i," *************"))
  print("*")
}

# Clean up ASCII rasters
TempFiles = list.files(pattern = ".asc")
for (fil in 1:length(TempFiles)){
  file.remove(TempFiles[[fil]])
}

######################## PART 2: STREAMFLOW PROCESSING #########################

library(hydroGOF)

GaugeIDs = c(10343500, 10336676, 10336660, 10336645, 103366092, 10336610, 10336780, 10336730, 11413000, 11413300)
GaugeNames = c("Sagehen Creek", "Ward Creek", "Blackwood Creek", "General Creek", "Upper Truckee #1", "Upper Truckee #2", "Trout Creek", "Glenbrook Creek", "North Yuba River", "Slate Creek (Reconstructed)")
CatchmentAreas = c(10.5, 9.7, 11.2, 7.44, 39.3, 54.9, 36.7, 4.11, 250, 49.4) # Areas in mi^2 from USGS

# Read dataframe of streamflow data generated by ProcessStreamflowForDHSVM.R
ReferenceStreamflow = read.csv(paste0(ReferenceDir,"USGS_StreamflowData.csv"))[,-1]
head(ReferenceStreamflow)

# Set up dataframe to store NSE results for each watershed to summarize later
NSEdata = data.frame(matrix(nrow=length(GaugeIDs),ncol=7))
names(NSEdata) = c("GaugeID","GaugeName","Area_km2","Weight","NSE","YearlyNSE","LogNSE")
NSEdata$GaugeID = GaugeIDs
NSEdata$GaugeName = GaugeNames
NSEdata$Area_km2 = CatchmentAreas * 2.58999 # mi^2 --> km^2

# Weights are relative to area, with nested catchments counted proportionally
TotalArea = sum(NSEdata$Area_km2[which(GaugeNames != "Upper Truckee #1")])
NSEdata$Weight = NSEdata$Area_km2/TotalArea

# Fix nested Truckee catchments
Truckee1Area = NSEdata$Area_km2[which(GaugeNames == "Upper Truckee #1")]
Truckee2Area = NSEdata$Area_km2[which(GaugeNames == "Upper Truckee #2")]
TotalTruckeeWeight = Truckee2Area/TotalArea
NSEdata$Weight[which(GaugeNames == "Upper Truckee #1")] = (Truckee1Area/(Truckee1Area + Truckee2Area))*TotalTruckeeWeight
NSEdata$Weight[which(GaugeNames == "Upper Truckee #2")] = (Truckee2Area/(Truckee1Area + Truckee2Area))*TotalTruckeeWeight

# Relative weight scaling when only considering a particular group of basins
# HeadwaterGaugeIDs = c(10343500, 10336676, 10336660, 10336645, 10336730)
# DownstreamGaugeIDs = c(103366092, 10336610, 10336780, 11413000, 11413300)
# HeadwatersFracArea = sum(NSEdata$Weight[match(HeadwaterGaugeIDs,GaugeIDs)])
# DownstreamFracArea = sum(NSEdata$Weight[match(DownstreamGaugeIDs,GaugeIDs)])
# print(HeadwatersFracArea)
# print(DownstreamFracArea)

LowBaseGaugeIDs = c(10336676, 10336660, 10336645, 103366092, 10336610)
HighBaseGaugeIDs = c(10343500, 10336780, 10336730, 11413000, 11413300)
LowBaseFracArea = sum(NSEdata$Weight[match(LowBaseGaugeIDs,GaugeIDs)])
HighBaseFracArea = sum(NSEdata$Weight[match(HighBaseGaugeIDs,GaugeIDs)])
print(LowBaseFracArea)
print(HighBaseFracArea)

print(NSEdata)
print(sum(NSEdata$Weight))

# Date indices for yearly NSE
# YearlyIndices = list()
# CalendarYear = as.numeric(format(as.Date(ReferenceStreamflow$Date), "%Y"))
# Month = as.numeric(format(as.Date(ReferenceStreamflow$Date), "%m"))
# WaterYear = CalendarYear
# WaterYear[which(Month > 9)] = CalendarYear[which(Month > 9)] + 1
# for (yr in 1:length(StreamYears)){
#   YearlyIndices[[yr]] = which(WaterYear==StreamYears[[yr]])
# }

# Set up dataframe to store yearly bulk water yield
WaterYieldDHSVM = data.frame(matrix(0, nrow=nStreamYears,ncol=2))
names(WaterYieldDHSVM) = c("WaterYear","TotalRunoff_km3")
WaterYieldDHSVM$WaterYear = StreamYears

# Read pre-calculated water yields from non-nested gauges plus the Yuba FNF point
NonNestedGaugeIDs = c(10343500, 10336676, 10336660, 10336645, 10336610, 10336780, 10336730)
FNFname = "YRS"
FNFlongName = "Yuba River Near Smartville"
WaterYieldReference = read.csv(paste0(ReferenceDir,"YearlyTotalRunoff_YubaAndTruckee.csv"))[,-1]
print(WaterYieldReference)

# Read pre-calculated data from Yuba FNF point, with a selected number of the highest flows
HighFlowReference = read.csv(paste0(ReferenceDir,"YubaFNF_95thPercentileFlows.csv"))[,-1]
head(HighFlowReference)
HighFlowReferenceVals = rev(sort(HighFlowReference$DailyAvgQ))

timeTotal = 0

# Loop over each parameter set and calculate the objective function value
for (i in results.grid.thisgen$ParamSet){
  
  # Check whether results are available for this parameter set
  if (!all(dir.exists(paste0(ResultsDir,"Params",i,"_",BasinNames)))){
    
    # results.grid.thisgen[results.grid.thisgen$ParamSet==i,]$Headwaters_NSE = NA
    # results.grid.thisgen[results.grid.thisgen$ParamSet==i,]$Downstream_NSE = NA
    results.grid.thisgen[results.grid.thisgen$ParamSet==i,]$Total_NSE = NA
    # results.grid.thisgen[results.grid.thisgen$ParamSet==i,]$Yearly_NSE = NA
    results.grid.thisgen[results.grid.thisgen$ParamSet==i,]$LowBase_LogNSE = NA
    results.grid.thisgen[results.grid.thisgen$ParamSet==i,]$HighBase_LogNSE = NA
    results.grid.thisgen[results.grid.thisgen$ParamSet==i,]$WaterYield_RMSE = NA
    results.grid.thisgen[results.grid.thisgen$ParamSet==i,]$PeakFlow_RMSE = NA
    
    print("*")
    print(paste0("Param Set: ",i," Not Found"))
    
  } else {
    timeStart = Sys.time()
    
    # Reset dataframes
    NSEdata$NSE = NA
    # NSEdata$YearlyNSE = NA
    NSEdata$LogNSE = NA
    WaterYieldDHSVM$TotalRunoff_km3 = 0 # Must be zero since it's used in an iterative sum
    
    for (bsn in 1:length(BasinNames)){
      # Read the streamflow data output by DHSVM (drop first line that is date-only)
      DHSVMheader = read.table(paste0(ResultsDir,"Params",i,"_",BasinNames[bsn],"/Streamflow.Only"), nrows=1)
      DHSVMstreamflow = read.table(paste0(ResultsDir,"Params",i,"_",BasinNames[bsn],"/Streamflow.Only"), skip=2)
      names(DHSVMstreamflow) = DHSVMheader
      DHSVMstreamflow$DATE = as.Date(DHSVMstreamflow$DATE, format="%m.%d.%Y") # Convert to day only
      
      # Loop over each gauge
      for (gauge in 1:length(GaugeIDs)){
        
        # Only proceed if this gauge is in the current watershed
        if (paste0("Gauge_",GaugeIDs[gauge]) %in% DHSVMheader){
          
          # Reset the vector containing modeled values to be compared with reference data
          DHSVMvals = c()
          # Parse the corresponding reference values into a vector
          ReferenceVals = ReferenceStreamflow[,paste0("Gauge_",GaugeIDs[gauge])]
          
          # Aggregate DHSVM output from m^3/timestep to m^3/s as a daily average for the current gauge
          for (day in 1:length(ReferenceStreamflow$Date)){
            Ptrs = which(DHSVMstreamflow$DATE == ReferenceStreamflow$Date[day])
            DailyQ = sum(DHSVMstreamflow[,paste0("Gauge_",GaugeIDs[gauge])][Ptrs])/(24*60*60) # m^3/timestep --> m^3/s
            
            # Append to hydrograph for NSE
            DHSVMvals = c(DHSVMvals, DailyQ)
            
            # Add to bulk water yield of the current water year
            # Only if it's a non-nested basin! (Don't double count Yuba FNF)
            if (GaugeIDs[gauge] %in% NonNestedGaugeIDs){
              if (as.numeric(format(as.Date(ReferenceStreamflow$Date[day]), "%m")) < 10){
                WaterYear = as.numeric(format(as.Date(ReferenceStreamflow$Date[day]), "%Y"))
              } else {
                WaterYear = as.numeric(format(as.Date(ReferenceStreamflow$Date[day]), "%Y")) + 1
              }
              WaterYieldDHSVM[WaterYieldDHSVM$WaterYear==WaterYear,]$TotalRunoff_km3 = WaterYieldDHSVM[WaterYieldDHSVM$WaterYear==WaterYear,]$TotalRunoff_km3 + DailyQ * (24*60*60)/(10^9)
            }
            
          } # End of daily gauge aggregation loop
          
          # Calculate NSE for the current gauge
          NSEval = NSE(sim=DHSVMvals, obs=ReferenceVals)
          NSEdata[which(NSEdata$GaugeID==GaugeIDs[gauge]),]$NSE = NSEval
          
          # Calculate yearly NSE and record average (emphasizes peaks of low-water years)
          # NSEyrly = c()
          # for (yr in 1:length(StreamYears)){
          #   NSEyrly = c(NSEyrly, NSE(sim=DHSVMvals[YearlyIndices[[yr]]], obs=ReferenceVals[YearlyIndices[[yr]]]))
          # }
          # NSEdata[which(NSEdata$GaugeID==GaugeIDs[gauge]),]$YearlyNSE = mean(NSEyrly)
          
          # for (yrN in 1:6){
          #   print(NSE(sim=DHSVMvals[(365*(yrN-1)):(365*yrN)], obs=ReferenceVals[(365*(yrN-1)):(365*yrN)]))
          #   plot(ReferenceVals[(365*(yrN-1)):(365*yrN)], type="l")
          #   lines(DHSVMvals[(365*(yrN-1)):(365*yrN)], type="l", col="red")
          # }
          
          # Enforce minimum floor of 0.01 m^3/s (0.35 cfs) before performing log transformation
          DHSVMvals[DHSVMvals<0.01] = 0.01
          ReferenceVals[ReferenceVals<0.01] = 0.01
          LogNSEval = NSE(sim=DHSVMvals, obs=ReferenceVals, FUN=log)
          NSEdata[which(NSEdata$GaugeID==GaugeIDs[gauge]),]$LogNSE = LogNSEval
          
          #########
          # Diagnostic plots
          
          # minQ = max(c(min(c(ReferenceVals,DHSVMvals)),0.001))
          # maxQ = max(c(ReferenceVals,DHSVMvals))
          # 
          # par(mfrow=c(1,1), mar=c(4,5,3,2))
          # plot(as.Date(ReferenceStreamflow$Date),DHSVMvals,type="l",ylim=c(0,maxQ),col="red",lwd=1,main=paste0(GaugeNames[gauge]," - Params ",i),xlab="Water Year",ylab=expression("Streamflow, "*m^3*"/s"),cex.axis=1.5,cex.lab=1.5,cex.main=2)
          # lines(as.Date(ReferenceStreamflow$Date),ReferenceVals,col="black",lwd=1)
          # legend("topleft",inset=0.03,legend=c("Modeled","Observed"),lty=c(1,1),lwd=c(4,4),col=c("red","black"),cex=1.5)
          # text(as.Date(ReferenceStreamflow$Date[round(length(ReferenceStreamflow$Date)*0.5)]),maxQ*0.92,paste0("NSE = ",round(NSEval,2)),cex=1.5)
          # 
          # plot(as.Date(ReferenceStreamflow$Date),DHSVMvals,type="l",log="y",ylim=c(minQ,maxQ),col="red",lwd=1,main=paste0(GaugeNames[gauge]," - Params ",i),xlab="Water Year",ylab=expression("Streamflow, "*m^3*"/s"),cex.axis=1.5,cex.lab=1.5,cex.main=2)
          # lines(as.Date(ReferenceStreamflow$Date),ReferenceVals,col="black",lwd=1)
          # legend("topleft",inset=0.03,legend=c("Modeled","Observed"),lty=c(1,1),lwd=c(4,4),col=c("red","black"),cex=1.5)
          # text(as.Date(ReferenceStreamflow$Date[round(length(ReferenceStreamflow$Date)*0.5)]),exp(log(maxQ)*0.92),paste0("Log NSE = ",round(LogNSEval,2)),cex=1.5)
          # 
          # par(pty="s")
          # plot(ReferenceVals,DHSVMvals,log="xy",pch=3,main=paste0(GaugeNames[gauge]," - Params ",i),xlab=expression("Observed Streamflow, "*m^3*"/s"),ylab=expression("Modeled Streamflow, "*m^3*"/s"),xlim=c(minQ,maxQ),ylim=c(minQ,maxQ),asp=1,cex.axis=1.5,cex.lab=1.5,cex.main=2)
          # lines(seq(0.0001,1.5*maxQ,by=0.1),seq(0.0001,1.5*maxQ,by=0.1),col="red",lwd=2)
          # par(pty="m")
          # 
        }
      } # End of loop over USGS gauges
      
      # Add bulk runoff from full natural flow point and find FNF peak flows
      # Only proceed if the FNF point is in the current watershed
      if (paste0("Gauge_",FNFname) %in% DHSVMheader){
        
        # Compile a vector of all daily FNF values to later sort for high flows
        DHSVMfnfVals = c()
        
        # Aggregate DHSVM output from m^3/timestep to m^3/s as a daily average for the current gauge
        # Note that days are iterated over ReferenceStreamflow for convenience since it already has a daily calendar
        for (day in 1:length(ReferenceStreamflow$Date)){
          Ptrs = which(DHSVMstreamflow$DATE == ReferenceStreamflow$Date[day])
          DailyQ = sum(DHSVMstreamflow[,paste0("Gauge_",FNFname)][Ptrs])/(24*60*60) # m^3/timestep --> m^3/s
          
          # Add to bulk water yield of the current water year
          if (as.numeric(format(as.Date(ReferenceStreamflow$Date[day]), "%m")) < 10){
            WaterYear = as.numeric(format(as.Date(ReferenceStreamflow$Date[day]), "%Y"))
          } else {
            WaterYear = as.numeric(format(as.Date(ReferenceStreamflow$Date[day]), "%Y")) + 1
          }
          WaterYieldDHSVM[WaterYieldDHSVM$WaterYear==WaterYear,]$TotalRunoff_km3 = WaterYieldDHSVM[WaterYieldDHSVM$WaterYear==WaterYear,]$TotalRunoff_km3 + DailyQ * (24*60*60)/(10^9)
          
          DHSVMfnfVals = c(DHSVMfnfVals, DailyQ)
        }
      }
      
    } # End of loop over watersheds
    
    # Compute weighted NSE and LogNSE, then save the summarized results for this parameter set
    WeightedTotalNSE = sum(NSEdata$NSE * NSEdata$Weight)
    # WeightedYearlyNSE = sum(NSEdata$YearlyNSE * NSEdata$Weight)
    
    WeightedLowBaseLogNSE = sum(NSEdata$LogNSE[match(LowBaseGaugeIDs,GaugeIDs)] * NSEdata$Weight[match(LowBaseGaugeIDs,GaugeIDs)])/LowBaseFracArea
    WeightedHighBaseLogNSE = sum(NSEdata$LogNSE[match(HighBaseGaugeIDs,GaugeIDs)] * NSEdata$Weight[match(HighBaseGaugeIDs,GaugeIDs)])/HighBaseFracArea
    
    results.grid.thisgen[results.grid.thisgen$ParamSet==i,]$Total_NSE = WeightedTotalNSE
    # results.grid.thisgen[results.grid.thisgen$ParamSet==i,]$Yearly_NSE = WeightedYearlyNSE
    results.grid.thisgen[results.grid.thisgen$ParamSet==i,]$LowBase_LogNSE = WeightedLowBaseLogNSE
    results.grid.thisgen[results.grid.thisgen$ParamSet==i,]$HighBase_LogNSE = WeightedHighBaseLogNSE
    
    print(paste0("Total NSE = ",signif(WeightedTotalNSE,2)," for Param Set: ",i))
    # print(paste0("Yearly NSE = ",signif(WeightedYearlyNSE,2)," for Param Set: ",i))
    print(paste0("Low Base LogNSE = ",signif(WeightedLowBaseLogNSE,2)," for Param Set: ",i))
    print(paste0("High Base LogNSE = ",signif(WeightedHighBaseLogNSE,2)," for Param Set: ",i))
    
    # Compute RMSE of yearly bulk water yield, then save the summarized results for this parameter set
    BulkRMSE = sqrt(mean((WaterYieldDHSVM$TotalRunoff_km3 - WaterYieldReference$TotalRunoff_km3)^2))
    results.grid.thisgen[results.grid.thisgen$ParamSet==i,]$WaterYield_RMSE = BulkRMSE
    
    print(paste0("Yearly Water Yield RMSE = ",signif(BulkRMSE,2)," km^3 for Param Set: ",i))
    
    # Compute RMSE of daily high flows at the FNF point, then save the summarized results for this parameter set
    HighFlowDHSVMvals = rev(sort(DHSVMfnfVals))[1:length(HighFlowReference$DailyAvgQ)]
    PeakRMSE = sqrt(mean((HighFlowDHSVMvals - HighFlowReferenceVals)^2))
    results.grid.thisgen[results.grid.thisgen$ParamSet==i,]$PeakFlow_RMSE = PeakRMSE
    
    print(paste0("Daily Peak Flows RMSE = ",signif(PeakRMSE,2)," m^3/s for Param Set: ",i))
    
    # par(pty="s",mar=c(5,5,5,5))
    # maxQ = max(c(HighFlowDHSVMvals,HighFlowReferenceVals))
    # minQ = min(c(HighFlowDHSVMvals,HighFlowReferenceVals))
    # plot(HighFlowReferenceVals,HighFlowDHSVMvals,log="xy",pch=3,main=paste0(FNFlongName," - Params ",i,"\n95th Percentile Daily Flows"),xlab=expression("Observed High Flows, "*m^3*"/s"),ylab=expression("Modeled High Flows, "*m^3*"/s"),xlim=c(minQ,maxQ),ylim=c(minQ,maxQ),asp=1,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
    # lines(seq(0.0001,1.5*maxQ,by=0.1),seq(0.0001,1.5*maxQ,by=0.1),col="red",lwd=2)
    # par(pty="m")
    
    timeEnd = Sys.time()
    timeElapsed = as.numeric(difftime(timeEnd,timeStart,units="secs"))
    timeTotal = timeTotal + timeElapsed
    timeAvg = timeTotal/i
    print("*")
    print(paste0("Streamflow evaluation took ",round(timeElapsed/60,1)," minutes."))
    print(paste0("Full evaluation expected to take another ",round(timeAvg*(tail(design.grid$ParamSet,1)-i)/60)," minutes for ",tail(design.grid$ParamSet,1)-i," more parameter sets."))
    
  }
  
  print(paste0("************* DONE WITH STREAMFLOW FOR PARAMS ",i," *************"))
  print("*")
}

#plot(results.grid.thisgen$Total_NSE, results.grid.thisgen$HighBase_LogNSE, ylim=c(0,1))

################################################################################
# Concatenate results from this generation with previous generations
# And save the actual objective function values (not scaled)

if (ModelGen > 1){
  results.grid = rbind(results.grid.lastgen, results.grid.thisgen)
} else {
  results.grid = results.grid.thisgen
}

write.csv(results.grid,paste0("Generation",ModelGen,"_ResultsGrid.csv"))

################################################################################
# Re-scale the results grid to generate the response grid
################################################################################

design.grid.data = read.csv(paste0("Generation",ModelGen,"_DesignGrid.csv"))[,-1]
results.grid.data = read.csv(paste0("Generation",ModelGen,"_ResultsGrid.csv"))[,-1]
print(head(design.grid.data))
print(head(results.grid.data))

# Simplify the structure into pure matrices of numbers
design.grid = unname(as.matrix(design.grid.data[,2:(1+nParams)],ncol=nParams))
results.grid = unname(as.matrix(results.grid.data[,2:(1+nObjFuns)],ncol=nObjFuns))

nDesigns = length(design.grid[,1]) # For easy usage elsewhere

# NSE is natively scaled -Inf --> worst, 1 --> best
# Convert to 1-NSE so it's scaled 0 --> best, Inf --> worst
for (obj in 1:nObjFuns){
  if (grepl("NSE",names(results.grid.data)[obj+1])){
    results.grid[,obj] = 1 - results.grid[,obj]
  }
}

# Replace NAs (failed models) with the worst value from all classes
for (obj in 1:nObjFuns){
  WorstObsVal = max(results.grid[,obj],na.rm=TRUE)
  results.grid[is.na(results.grid[,obj]),obj] = WorstObsVal
}

# Objective scaling is based on the current worst end-members of the Pareto front

library(GPareto)

# Calculate current Pareto front
ParetoFront = t(nondominated_points(t(results.grid)))

# Find parameter sets that are part of the current Pareto front
parIndx = rep(FALSE,nDesigns)
for (i in 1:nDesigns){
  for (j in 1:length(ParetoFront[,1])){
    if (identical(ParetoFront[j,],results.grid[i,])){
      parIndx[i] = TRUE
    }
  }
}

# Rescale so that 0 --> theoretical best, 1 --> current worst on Pareto front

response.grid.data = data.frame(ParamSet=results.grid.data$ParamSet) # Empty until scaled objectives are added

for (obj in 1:nObjFuns){
  MaxParetoVal = max(results.grid[parIndx,obj])
  response.grid.data = cbind(response.grid.data, results.grid[,obj]/MaxParetoVal)
}

names(response.grid.data) = c("ParamSet",paste0("Scaled_",names(results.grid.data)[2:(nObjFuns+1)]))

write.csv(response.grid.data,paste0("Generation",ModelGen,"_ResponseGrid.csv"))

# Save the Pareto front at the current generation
# Includes param set numbers, designs, results, and responses

NonDominatedModels = cbind(design.grid.data[parIndx,], response.grid.data[parIndx,-1], results.grid.data[parIndx,-1])

print(NonDominatedModels$ParamSet)
print(length(NonDominatedModels[,1]))

write.csv(NonDominatedModels, paste0("Generation",ModelGen,"_ParetoFront.csv"))
