### GPEHRL CALIBRATOR FOR DHSVM ###
# Validation: analyze new runs

################################################################################
# Universal adjustable settings

ValModels = c(195, 270, 276)

ObjFunNames = c("PeakSWE_RMSE","Total_NSE","LowBase_LogNSE","HighBase_LogNSE","WaterYield_RMSE","PeakFlow_RMSE")
ObjFunLongNames = c("Peak SWE","Total NSE","Low-Baseflow Log NSE","High-Baseflow Log NSE","Yearly Water Yield","95th-Percentile Peak Flows")
ObjFunLongNamesTight = c("Peak\nSWE","Total\nNSE","Low-Baseflow\nLog NSE","High-Baseflow\nLog NSE","Yearly\nWater Yield","95th-Percentile\nPeak Flows")
nObjFuns = length(ObjFunNames)

ParamNames = c("SOILD","SLHC","POROSITY","EXPDEC","MINRES","SNOWTEMP","ALBMELTRATE")
ParamLongNames = c("Soil Depth","Hydraulic Conductivity","Porosity","Ksat Exponential Decrease","Minimum Stomatal Resistance","Snow Temperature Threshold","Melt-Season Albedo Decay Rate")
ParamLongNamesTight = c("Soil\nDepth","Hydraulic\nConductivity","Porosity","Ksat Exp.\nDecrease","Min. Stomatal\nResistance","Snow Temp.\nThreshold","Melt-Season\nAlbedo Decay")
nParams = length(ParamNames)

BasinNames = c("Truckee","Yuba","American") # Adding the American for validation!

# Extra information to visualize particular objective functions
SnowYears = 2005:2010
StreamYears = 2006:2011
nSnowYears = length(SnowYears)
nStreamYears = length(StreamYears)

ModelInterval = 3 # hours

ResultsDir = "Val_Results/" # Where the files created by DHSVM are stored
ReferenceDir = "Val_ReferenceData/" # Where the files used as ground-truth are stored

dir = "C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/Calibrate_MOBOhydro/Validation/"
setwd(dir)

# For converting DHSVM-style header-less ascii rasters to regular rasters
ASCIIheader = readLines("../ASCIIheader.txt")
nRows = 1436
nCols = 2247

library(GPareto)
library(GGally)
library(scatterplot3d)
library(ggplot2)
library(patchwork)
library(viridis)
library(scales)
library(hydroGOF)

################################################################################
# Plot SWE and SWE date maps for user-selected individual parameter sets
################################################################################

library(raster)

# Read mask of calibration watersheds to make sure everything stays in the right CRS/extent/etc.
# NOTE also that this is how we determine which pixels get compared--this mask ONLY includes the calibration watersheds!
MaskDir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/WatershedBoundaries/"
WatershedMasks = lapply(paste0(MaskDir,"TCSImask_",BasinNames,"Domain.tif"), raster)
Mask = WatershedMasks[[1]]
for (bsn in 1:length(BasinNames)){
  Mask = max(Mask,WatershedMasks[[bsn]])
}

# Extract crs of the study domain for application to the ascii rasters later
MaskCRS = crs(Mask)

# Also read the vegetation ID map so we can mask out water (e.g., Lake Tahoe) from SWE comparisons
WaterClass = 29
VegDir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/LANDIS/"
VegIDmap = raster(paste0(VegDir,"TCSI_Year0_VegID.tif"))
WaterMask = VegIDmap*0 + (VegIDmap[,] != 29)*1
Mask = Mask * WaterMask

# Read pre-processed Margulis pixel-wise max SWE rasters and max SWE date rasters
ReferenceSWErasters = lapply(paste0(ReferenceDir,"TCSI_max_swe_WY",SnowYears,".tif"), raster)
# ReferenceMaxDateRasters = lapply(paste0(ReferenceDir,"TCSI_max_swe_dowy_WY",SnowYears,".tif"), raster)

# Only keep max SWE dates if max SWE > 5 cm to exclude small storms that could have competing peaks far apart
# MaxSWEthresh = 0.05

SWEptrsByYear = list()
# DatePtrsByYear = list()
for (yr in 1:nSnowYears){
  SWEptrsByYear[[yr]] = which(is.finite(ReferenceSWErasters[[yr]][,]) & (Mask[,] > 0))
  print(length(SWEptrsByYear[[yr]]))
  
  # DatePtrsByYear[[yr]] = which(is.finite(ReferenceMaxDateRasters[[yr]][,]) & (ReferenceSWErasters[[yr]][,] > MaxSWEthresh) & (Mask[,] > 0))
  # print(length(DatePtrsByYear[[yr]]))
}

# Make mask for nice plotting
VisMask = Mask*NA
VisMask[SWEptrsByYear[[1]]] = 1
plot(VisMask)

# For faster plotting
AggrFactor = 6

# Choose which parameter sets are of interest
VisualizeParameters = c(195,270,276)

# Loop over each parameter set and calculate the objective function value
for (i in VisualizeParameters){
  # Read the concatenated ascii files output by myconvert.c
  # Each basin's raster stack is stored together in a list
  RawRasterStackSWE = lapply(paste0(ResultsDir,"Val_Params",i,"_",BasinNames,"/Map.Snow.MaxSwe.txt"), readLines)
  # RawRasterStackDate = lapply(paste0(ResultsDir,"Val_Params",i,"_",BasinNames,"/Map.Snow.MaxSweDate.txt"), readLines)
  
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
      # # Fix rollover where Oct-Jan are >365
      # WaterYear = max(as.numeric(format(SingleRasterDates[!is.na(SingleRasterDates)],"%Y")))
      # EndOfWaterYear = as.numeric(strftime(as.Date(paste0(WaterYear,"-09-30"),format="%Y-%m-%d"),format="%j"))
      # DaysThisYear = as.numeric(strftime(as.Date(paste0(WaterYear,"-12-31"),format="%Y-%m-%d"),format="%j"))
      # DayOfWaterYear[DayOfWaterYear > EndOfWaterYear] = DayOfWaterYear[DayOfWaterYear > EndOfWaterYear] - DaysThisYear
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
    
    ##############################################
    # Set up dataframes of SWE rasters for ggplot
    RastDHSVM_DF = as.data.frame(as(aggregate(MergedRasterSWE*VisMask,AggrFactor),"SpatialPixelsDataFrame"))
    RastDHSVM_DF$Date = rep(SnowYears[yr],length(RastDHSVM_DF$layer))
    RastDHSVM_DF$Type = rep(paste0("DHSVM\nModel ",i),length(RastDHSVM_DF$layer))
    
    RastReference_DF = as.data.frame(as(aggregate(ReferenceSWErasters[[yr]]*VisMask,AggrFactor),"SpatialPixelsDataFrame"))
    RastReference_DF$Date = rep(SnowYears[yr],length(RastReference_DF$layer))
    RastReference_DF$Type = rep("Margulis et. al\nReanalysis",length(RastReference_DF$layer))
    
    # RMSE for this year only
    DHSVMvals = MergedRasterSWE[SWEptrsByYear[[yr]]]
    ReferenceVals = ReferenceSWErasters[[yr]][SWEptrsByYear[[yr]]]
    SWErmse = sqrt(mean((DHSVMvals-ReferenceVals)^2))
    
    if (yr==1){
      CombinedDataSWE = rbind(RastDHSVM_DF,RastReference_DF)
      DataTextSWE = data.frame(label=c(paste0("RMSE\n",round(SWErmse*100)," cm"),paste0("Mean\n",round(mean(ReferenceVals)*100)," cm")),Date=rep(SnowYears[yr],2),Type=c(paste0("DHSVM\nModel ",i),"Margulis et. al\nReanalysis"))
    } else {
      CombinedDataSWE = rbind(CombinedDataSWE,RastDHSVM_DF,RastReference_DF)
      DataTextSWE = rbind(DataTextSWE,data.frame(label=c(paste0("RMSE\n",round(SWErmse*100)," cm"),paste0("Mean\n",round(mean(ReferenceVals)*100)," cm")),Date=rep(SnowYears[yr],2),Type=c(paste0("DHSVM\nModel ",i),"Margulis et. al\nReanalysis")))
    }
    
    ##############################################
    # Set up dataframes of date rasters for ggplot
    
    # Make special VisMask that excludes low-snow areas from the peak date analysis (same as the actual objective functions)
    # VisMaskDate = Mask*NA
    # VisMaskDate[DatePtrsByYear[[yr]]] = 1
    # 
    # RastDHSVM_DF = as.data.frame(as(aggregate(MergedRasterDate*VisMaskDate,AggrFactor),"SpatialPixelsDataFrame"))
    # RastDHSVM_DF$Date = rep(SnowYears[yr],length(RastDHSVM_DF$layer))
    # RastDHSVM_DF$Type = rep(paste0("DHSVM\nModel ",i),length(RastDHSVM_DF$layer))
    # 
    # RastReference_DF = as.data.frame(as(aggregate(ReferenceMaxDateRasters[[yr]]*VisMaskDate,AggrFactor),"SpatialPixelsDataFrame"))
    # RastReference_DF$Date = rep(SnowYears[yr],length(RastReference_DF$layer))
    # RastReference_DF$Type = rep("Margulis et. al\nReanalysis",length(RastReference_DF$layer))
    # 
    # # RMSE for this year only
    # DHSVMvals = MergedRasterDate[DatePtrsByYear[[yr]]]
    # ReferenceVals = ReferenceMaxDateRasters[[yr]][DatePtrsByYear[[yr]]]
    # DateRMSE = sqrt(mean((DHSVMvals-ReferenceVals)^2))
    # 
    # YearlyMeanDate = as.Date(round(mean(ReferenceVals))-1, origin=paste0(WaterYear-1,"-10-01")) # Origin is zero-based
    # 
    # if (yr==1){
    #   CombinedDataDate = rbind(RastDHSVM_DF,RastReference_DF)
    #   DataTextDate = data.frame(label=c(paste0("RMSE\n",round(DateRMSE)," days"),paste0("Mean\n",format(YearlyMeanDate,"%b. %d"))),Date=rep(SnowYears[yr],2),Type=c(paste0("DHSVM\nModel ",i),"Margulis et. al\nReanalysis"))
    #   #DataTextDate = data.frame(label=c(paste0("RMSE\n",round(DateRMSE)," days"),paste0("Mean\n",round(mean(ReferenceVals))," (DOWY)")),Date=rep(SnowYears[yr],2),Type=c(paste0("DHSVM\nModel: ",i),"Margulis et. al\nReanalysis"))
    # } else {
    #   CombinedDataDate = rbind(CombinedDataDate,RastDHSVM_DF,RastReference_DF)
    #   DataTextDate = rbind(DataTextDate,data.frame(label=c(paste0("RMSE\n",round(DateRMSE)," days"),paste0("Mean\n",format(YearlyMeanDate,"%b. %d"))),Date=rep(SnowYears[yr],2),Type=c(paste0("DHSVM\nModel ",i),"Margulis et. al\nReanalysis")))
    # }
    
  }
  
  # SWE plots
  g1 = ggplot() + geom_tile(data=CombinedDataSWE, aes(x=x,y=y,fill=layer)) +
    geom_text(data=DataTextSWE, mapping=aes(x=680000,y=4300000,label=label),hjust=0,vjust=0.5,size=6) +
    scale_fill_gradientn(colors=c("snow3","lightcyan2","skyblue1","deepskyblue1","dodgerblue3","dodgerblue4","blue3","navy","midnightblue","black"),limits=c(0,3),breaks=seq(0,3,0.5),labels=paste0(seq(0,3,0.5)," m")) +
    guides(fill=guide_colorbar(ticks.colour=NA,frame.colour="black")) +
    coord_equal() +
    labs(fill="SWE",title="Pixel-Wise Peak SWE") +
    theme_bw() + theme(plot.title=element_text(color="black",size=24,hjust=0.5),plot.margin=margin(1,1,1,1),axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank()) +
    theme(legend.position="right",legend.key.height=unit(2,"cm"),legend.title=element_text(size=18), legend.text=element_text(size=18)) +
    theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA)) +
    scale_x_continuous(expand=c(0,0),limits=c(660000,770000)) +
    scale_y_continuous(expand=c(0,0),limits=c(4275000,4407000)) +
    facet_grid(cols=vars(Date),rows=vars(Type),switch="y")
  
  # SWE date plots
  # g2 = ggplot() + geom_tile(data=CombinedDataDate, aes(x=x,y=y,fill=layer)) +
  #   geom_text(data=DataTextDate, mapping=aes(x=680000,y=4300000,label=label),hjust=0,vjust=0.5,size=6) +
  #   scale_fill_gradientn(colors=plasma(256),limits=c(60,240),breaks=seq(60,240,30),labels=paste0(seq(60,240,30))) +
  #   guides(fill=guide_colorbar(ticks.colour=NA,frame.colour="black")) +
  #   coord_equal() +
  #   labs(fill="DOWY",title="Pixel-Wise Peak SWE Date") +
  #   theme_bw() + theme(plot.title=element_text(color="black",size=24,hjust=0.5),plot.margin=margin(1,1,1,1),axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank()) +
  #   theme(legend.position="right",legend.key.height=unit(2,"cm"),legend.title=element_text(size=18), legend.text=element_text(size=18)) +
  #   theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA)) +
  #   scale_x_continuous(expand=c(0,0),limits=c(660000,770000)) +
  #   scale_y_continuous(expand=c(0,0),limits=c(4275000,4407000)) +
  #   facet_grid(cols=vars(Date),rows=vars(Type),switch="y")
  
  print(g1)
  # Sys.sleep(20)
  # print(g2)
  
}

# Clean up ASCII rasters
TempFiles = list.files(pattern = ".asc")
for (fil in 1:length(TempFiles)){
  file.remove(TempFiles[[fil]])
}

################################################################################
# Plot hydrographs for user-selected individual parameter sets
################################################################################

GaugePlotOrder = c("Glenbrook Creek", "General Creek", "Ward Creek", "Duncan Canyon Creek", "Sagehen Creek",
                   "Blackwood Creek", "Trout Creek", "Upper Truckee #1", "Upper Truckee #2",
                   "Slate Creek (Reconstructed)", "North Yuba River", "N.F. American River")

GaugeIDs = c(10343500, 10336676, 10336660, 10336645, 103366092, 10336610, 10336780, 10336730,
             11413000, 11413300,
             11427000, 11427700)

GaugeNames = c("Sagehen Creek", "Ward Creek", "Blackwood Creek", "General Creek", "Upper Truckee #1", "Upper Truckee #2", "Trout Creek", "Glenbrook Creek",
               "North Yuba River", "Slate Creek (Reconstructed)",
               "N.F. American River", "Duncan Canyon Creek")

CatchmentAreas = c(10.5, 9.7, 11.2, 7.44, 39.3, 54.9, 36.7, 4.11,
                   250, 49.4,
                   342, 9.9) # Areas in mi^2 from USGS

# Read dataframe of streamflow data generated by ProcessStreamflowForDHSVM.R
ReferenceStreamflow = read.csv(paste0(ReferenceDir,"USGS_StreamflowData_Val.csv"))[,-1]
head(ReferenceStreamflow)

# Read pre-calculated water yields from non-nested gauges plus the Yuba FNF point
# NonNestedGaugeIDs = c(10343500, 10336676, 10336660, 10336645, 10336610, 10336780, 10336730)
# FNFname = "YRS"
# FNFlongName = "Yuba River Near Smartville"
# WaterYieldReference = read.csv(paste0(ReferenceDir,"YearlyTotalRunoff_YubaAndTruckee.csv"))[,-1]
# print(WaterYieldReference)
WaterYieldReferenceYubaFNF = read.csv(paste0(ReferenceDir,"YearlyTotalRunoff_YubaFNF_Val.csv"))[,-1]
WaterYieldReferenceAmericanFNF = read.csv(paste0(ReferenceDir,"YearlyTotalRunoff_AmericanFNF_Val.csv"))[,-1]

# Read pre-calculated data from Yuba FNF point, with a selected number of the highest flows
HighFlowReferenceYuba = read.csv(paste0(ReferenceDir,"YubaFNF_95thPercentileFlows_Val.csv"))[,-1]
head(HighFlowReferenceYuba)
HighFlowReferenceYubaVals = rev(sort(HighFlowReferenceYuba$DailyAvgQ))
FNFnameYuba = "YRS"
FNFlongNameYuba = "Yuba River Near Smartville"

# Read pre-calculated data from American FNF point, with a selected number of the highest flows
HighFlowReferenceAmerican = read.csv(paste0(ReferenceDir,"AmericanFNF_95thPercentileFlows_Val.csv"))[,-1]
head(HighFlowReferenceAmerican)
HighFlowReferenceAmericanVals = rev(sort(HighFlowReferenceAmerican$DailyAvgQ))
FNFnameAmerican = "NAT"
FNFlongNameAmerican = "American River at Lake Natoma"

SubplotOption = FALSE # Whether to print a bunch of per-gauge plots

# Choose which parameter sets are of interest
#VisualizeParameters = c(276)
VisualizeParameters = ValModels

# Loop over each parameter set and calculate the objective function value
for (i in VisualizeParameters){
  # Loop over modeled watersheds
  for (bsn in 1:length(BasinNames)){
    # Read the streamflow data output by DHSVM (drop first line that is date-only)
    DHSVMheader = read.table(paste0(ResultsDir,"Val_Params",i,"_",BasinNames[bsn],"/Streamflow.Only"), nrows=1)
    DHSVMstreamflow = read.table(paste0(ResultsDir,"Val_Params",i,"_",BasinNames[bsn],"/Streamflow.Only"), skip=2)
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
        
        # Store basin-specific water yield
        DHSVMyield = 0
        ReferenceYield = sum(ReferenceVals)
        
        # Aggregate DHSVM output from m^3/timestep to m^3/s as a daily average for the current gauge
        for (day in 1:length(ReferenceStreamflow$Date)){
          Ptrs = which(DHSVMstreamflow$DATE == ReferenceStreamflow$Date[day])
          DailyQ = sum(DHSVMstreamflow[,paste0("Gauge_",GaugeIDs[gauge])][Ptrs])/(24*60*60) # m^3/timestep --> m^3/s
          
          # Append to hydrograph for NSE
          DHSVMvals = c(DHSVMvals, DailyQ)
          
          # Add to bulk water yield of the current catchment
          DHSVMyield = DHSVMyield + DailyQ * (24*60*60)/(10^9)
          
        } # End of daily gauge aggregation loop
        
        # Calculate NSE for the current gauge
        NSEval = NSE(sim=DHSVMvals, obs=ReferenceVals)
        
        WaterYieldErr = sum(DHSVMvals)/sum(ReferenceVals) - 1
        Qmean = mean(ReferenceVals)
        
        # Enforce minimum floor of 0.01 m^3/s (0.35 cfs) before performing log transformation
        DHSVMvals[DHSVMvals<0.01] = 0.01
        ReferenceVals[ReferenceVals<0.01] = 0.01
        LogNSEval = NSE(sim=DHSVMvals, obs=ReferenceVals, FUN=log)
        
        # Convert m^3/s to mm/d
        DHSVMvals = 1000 * DHSVMvals * (60*60*24) / (CatchmentAreas[gauge] * 2.58999 * 1000^2)
        ReferenceVals = 1000 * ReferenceVals * (60*60*24) / (CatchmentAreas[gauge] * 2.58999 * 1000^2)
        
        # Add to dataframe for regular-scale plots
        DHSVM_DF = data.frame(Streamflow=DHSVMvals)
        DHSVM_DF$Date = as.Date(ReferenceStreamflow$Date)
        DHSVM_DF$Type = rep("DHSVM",length(DHSVM_DF$Streamflow))
        DHSVM_DF$Watershed = rep(GaugeNames[gauge],length(DHSVM_DF$Streamflow))
        
        Reference_DF = data.frame(Streamflow=ReferenceVals)
        Reference_DF$Date = as.Date(ReferenceStreamflow$Date)
        Reference_DF$Type = rep("USGS",length(Reference_DF$Streamflow))
        Reference_DF$Watershed = rep(GaugeNames[gauge],length(Reference_DF$Streamflow))
        
        # Label positions
        MaxQ = max(ReferenceVals)
        TickList = pretty(c(0, MaxQ*1.05), n=4)
        UpperLab = 0.9*tail(TickList,1)
        MiddleLab = 0.7*tail(TickList,1)
        LowerLab = 0.5*tail(TickList,1)

        UpperLabLog = 10^(log10(0.02) + 0.9*(log10(100*1.5)-log10(0.02)))
        
        # UpperLab = 0.9*max(c(DHSVMvals,ReferenceVals))
        # MiddleLab = 0.7*max(c(DHSVMvals,ReferenceVals))
        # LowerLab = 0.5*max(c(DHSVMvals,ReferenceVals))
        # 
        # UpperLabLog = 10^(log10(min(c(DHSVMvals,ReferenceVals))) + 0.9*(log10(max(c(DHSVMvals,ReferenceVals)))-log10(min(c(DHSVMvals,ReferenceVals)))))
        
        if (gauge==1){
          CombinedData = rbind(DHSVM_DF,Reference_DF)
          DataText = data.frame(label=c(paste0("\"NSE = ",round(NSEval,2),"\""),paste0("\"Log NSE = ",round(LogNSEval,2),"\""),rep(paste0("\"Bulk Runoff Error = ",round(100*WaterYieldErr),"%\""),1),rep(paste0("\"Area = \"*",signif(CatchmentAreas[gauge]*2.58999,2),"*\" km\"^2"),1)),PlotType=c("Regular","Log","Regular","Regular"),LabelPos=c(UpperLab,UpperLabLog,MiddleLab,LowerLab),Watershed=rep(GaugeNames[gauge],4))
          #DataText = data.frame(label=c(paste0("\"NSE = ",round(NSEval,2),"\""),paste0("\"Log NSE = ",round(LogNSEval,2),"\""),rep(paste0("\"Bulk Runoff Error = ",round(100*WaterYieldErr),"%\""),1),rep(paste0("\"Mean = \"*",signif(Qmean,1),"*\" m\"^3*\"/s\""),1)),PlotType=c("Regular","Log","Regular","Regular"),LabelPos=c(UpperLab,UpperLabLog,MiddleLab,LowerLab),Watershed=rep(GaugeNames[gauge],4))
        } else {
          CombinedData = rbind(CombinedData,DHSVM_DF,Reference_DF)
          DataText = rbind(DataText,data.frame(label=c(paste0("\"NSE = ",round(NSEval,2),"\""),paste0("\"Log NSE = ",round(LogNSEval,2),"\""),rep(paste0("\"Bulk Runoff Error = ",round(100*WaterYieldErr),"%\""),1),rep(paste0("\"Area = \"*",signif(CatchmentAreas[gauge]*2.58999,2),"*\" km\"^2"),1)),PlotType=c("Regular","Log","Regular","Regular"),LabelPos=c(UpperLab,UpperLabLog,MiddleLab,LowerLab),Watershed=rep(GaugeNames[gauge],4)))
        }
        
        #########
        # Diagnostic plots
        if (SubplotOption){
          minQ = max(c(min(c(ReferenceVals,DHSVMvals)),0.001))
          maxQ = max(c(ReferenceVals,DHSVMvals))
          
          par(mfrow=c(1,1), mar=c(4,5,3,2))
          plot(as.Date(ReferenceStreamflow$Date),DHSVMvals,type="l",ylim=c(0,maxQ),col="red",lwd=1,main=paste0(GaugeNames[gauge]," - Params ",i),xlab="Water Year",ylab=expression("Streamflow, "*m^3*"/s"),cex.axis=1.5,cex.lab=1.5,cex.main=2)
          lines(as.Date(ReferenceStreamflow$Date),ReferenceVals,col="black",lwd=1)
          legend("topleft",inset=0.03,legend=c("Modeled","Observed"),lty=c(1,1),lwd=c(4,4),col=c("red","black"),cex=1.5)
          text(as.Date(ReferenceStreamflow$Date[round(length(ReferenceStreamflow$Date)*0.5)]),maxQ*0.92,paste0("NSE = ",round(NSEval,2)),cex=1.5)
          
          plot(as.Date(ReferenceStreamflow$Date),DHSVMvals,type="l",log="y",ylim=c(minQ,maxQ),col="red",lwd=1,main=paste0(GaugeNames[gauge]," - Params ",i),xlab="Water Year",ylab=expression("Streamflow, "*m^3*"/s"),cex.axis=1.5,cex.lab=1.5,cex.main=2)
          lines(as.Date(ReferenceStreamflow$Date),ReferenceVals,col="black",lwd=1)
          legend("topleft",inset=0.03,legend=c("Modeled","Observed"),lty=c(1,1),lwd=c(4,4),col=c("red","black"),cex=1.5)
          text(as.Date(ReferenceStreamflow$Date[round(length(ReferenceStreamflow$Date)*0.5)]),exp(log(maxQ)*0.92),paste0("Log NSE = ",round(LogNSEval,2)),cex=1.5)
          
          par(pty="s")
          plot(ReferenceVals,DHSVMvals,log="xy",pch=3,main=paste0(GaugeNames[gauge]," - Params ",i),xlab=expression("Observed Streamflow, "*m^3*"/s"),ylab=expression("Modeled Streamflow, "*m^3*"/s"),xlim=c(minQ,maxQ),ylim=c(minQ,maxQ),asp=1,cex.axis=1.5,cex.lab=1.5,cex.main=2)
          lines(seq(0.0001,1.5*maxQ,by=0.1),seq(0.0001,1.5*maxQ,by=0.1),col="red",lwd=2)
          par(pty="m")
        }
      }
    } # End of loop over USGS gauges
    
    # Add bulk runoff from full natural flow point and find FNF peak flows
    # Only proceed if the FNF point is in the current watershed
    if (paste0("Gauge_",FNFnameYuba) %in% DHSVMheader){

      # Compile a vector of all daily FNF values to later sort for high flows
      DHSVMfnfValsYuba = c()

      # Aggregate DHSVM output from m^3/timestep to m^3/s as a daily average for the current gauge
      # Note that days are iterated over ReferenceStreamflow for convenience since it already has a daily calendar
      for (day in 1:length(ReferenceStreamflow$Date)){
        Ptrs = which(DHSVMstreamflow$DATE == ReferenceStreamflow$Date[day])
        DailyQ = sum(DHSVMstreamflow[,paste0("Gauge_",FNFnameYuba)][Ptrs])/(24*60*60) # m^3/timestep --> m^3/s
        
        DHSVMfnfValsYuba = c(DHSVMfnfValsYuba, DailyQ)
      }
    }
    if (paste0("Gauge_",FNFnameAmerican) %in% DHSVMheader){
      
      # Compile a vector of all daily FNF values to later sort for high flows
      DHSVMfnfValsAmerican = c()
      
      # Aggregate DHSVM output from m^3/timestep to m^3/s as a daily average for the current gauge
      # Note that days are iterated over ReferenceStreamflow for convenience since it already has a daily calendar
      for (day in 1:length(ReferenceStreamflow$Date)){
        Ptrs = which(DHSVMstreamflow$DATE == ReferenceStreamflow$Date[day])
        DailyQ = sum(DHSVMstreamflow[,paste0("Gauge_",FNFnameAmerican)][Ptrs])/(24*60*60) # m^3/timestep --> m^3/s
        
        DHSVMfnfValsAmerican = c(DHSVMfnfValsAmerican, DailyQ)
      }
    }
    
  } # End of loop over watersheds
  
  # Streamflow plots
  PlotList = list()
  for (j in 1:length(GaugeIDs)){
    #MinQ = min(CombinedData[(CombinedData[,"Watershed"]==GaugePlotOrder[j] & CombinedData[,"Type"]=="USGS"),]$Streamflow)
    MaxQ = max(CombinedData[(CombinedData[,"Watershed"]==GaugePlotOrder[j] & CombinedData[,"Type"]=="USGS"),]$Streamflow)
    TickList = pretty(c(0, MaxQ*1.05), n=4)
    
    PlotList[[j]] = ggplot() + geom_line(data=CombinedData[CombinedData$Watershed==GaugePlotOrder[j],], aes(x=Date,y=Streamflow,col=Type), size=0.5) +
      geom_text(data=DataText[(DataText[,"PlotType"]=="Regular" & DataText[,"Watershed"]==GaugePlotOrder[j]),], mapping=aes(x=as.Date(paste0(median(c(StreamYears[1]-1,StreamYears)),"-10-01")),y=LabelPos,label=label),parse="TRUE",hjust=0.5,vjust=0.5,size=4) +
      scale_color_manual(breaks=c("USGS","DHSVM"),labels=c("USGS\nGauged",paste0("DHSVM\nModel ",i)),values=c("black","dodgerblue")) +
      scale_x_date(breaks=as.Date(paste0(c(StreamYears[1]-1,StreamYears),"-10-01")),date_labels="%b-%y") + 
      scale_y_continuous(limits=c(min(TickList),max(TickList)),breaks=TickList,labels=sprintf("%g",TickList)) +
      labs(y=expression("Streamflow, mm/d"), title=GaugePlotOrder[j]) +
      theme_bw() +
      theme(axis.text.x=element_text(size=8,hjust=0.5,color="black"), axis.title.x=element_blank(), axis.title.y=element_text(size=10,margin=margin(0,2,0,0)), axis.text.y=element_text(size=8,color="black"), legend.title=element_blank(), legend.text=element_text(size=14)) +
      theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(5,5,5,5),panel.background=element_rect("white", "black"), panel.grid=element_blank()) +
      theme(plot.title=element_text(color="black",size=12,hjust=0.5,face="bold"),legend.position="bottom") +
      guides(color=guide_legend(override.aes=list(size=2),byrow=TRUE))
  }
  
  g1 = patchwork::wrap_plots(PlotList, nrow=4, guides="collect") & theme(legend.position = "bottom")
  
  print(g1)
  Sys.sleep(5)
  
  # Log streamflow plots
  PlotList = list()
  for (j in 1:length(GaugeIDs)){
    #MinQ = min(CombinedData[(CombinedData[,"Watershed"]==GaugePlotOrder[j] & CombinedData[,"Type"]=="USGS"),]$Streamflow)
    #MaxQ = max(CombinedData[(CombinedData[,"Watershed"]==GaugePlotOrder[j] & CombinedData[,"Type"]=="USGS"),]$Streamflow)
    #TickList = signif(10^seq(log10(MinQ)-0.5, log10(MaxQ)+0.5, length.out=5), 1)
    #TickList = signif(10^seq(floor(log10(MinQ*0.8)), ceiling(log10(MaxQ*1.2)), length.out=5), 1)
    TickList = signif(c(0.02,0.1,1,10,100),1)
    
    PlotList[[j]] = ggplot() + geom_line(data=CombinedData[CombinedData$Watershed==GaugePlotOrder[j],], aes(x=Date,y=Streamflow,col=Type), size=0.5) +
      geom_text(data=DataText[(DataText[,"PlotType"]=="Log" & DataText[,"Watershed"]==GaugePlotOrder[j]),], mapping=aes(x=as.Date(paste0(median(c(StreamYears[1]-1,StreamYears)),"-10-01")),y=LabelPos,label=label),parse="TRUE",hjust=0.5,vjust=0.5,size=4) +
      scale_color_manual(breaks=c("USGS","DHSVM"),labels=c("USGS\nGauged",paste0("DHSVM\nModel ",i)),values=c("black","dodgerblue")) +
      scale_x_date(breaks=as.Date(paste0(c(StreamYears[1]-1,StreamYears),"-10-01")),date_labels="%b-%y") + 
      scale_y_continuous(trans="log10",limits=c(min(TickList),max(TickList)*1.5),breaks=TickList,labels=sprintf("%g",TickList)) +
      labs(y=expression("Streamflow, mm/d"), title=GaugePlotOrder[j]) +
      theme_bw() +
      theme(axis.text.x=element_text(size=8,hjust=0.5,color="black"), axis.title.x=element_blank(), axis.title.y=element_text(size=10,margin=margin(0,2,0,0)), axis.text.y=element_text(size=8,color="black"), legend.title=element_blank(), legend.text=element_text(size=14)) +
      theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(5,5,5,5),panel.background=element_rect("white", "black"), panel.grid=element_blank()) +
      theme(plot.title=element_text(color="black",size=12,hjust=0.5,face="bold"),legend.position="bottom") +
      guides(color=guide_legend(override.aes=list(size=2),byrow=TRUE))
  }
  
  g2 = patchwork::wrap_plots(PlotList, nrow=4, guides="collect") & theme(legend.position = "bottom")
  
  print(g2)
  Sys.sleep(5)
  
  ################ Yuba FNF
  
  # FNF peak flow plot
  HighFlowDHSVMvals = rev(sort(DHSVMfnfValsYuba))[1:length(HighFlowReferenceYuba$DailyAvgQ)]
  PeakRMSE = sqrt(mean((HighFlowDHSVMvals - HighFlowReferenceYubaVals)^2))
  # Convert list of daily modeled values (m^3/s) into cumulative yield (km^3)
  FNFtotalDHSVM = sum(DHSVMfnfValsYuba)*(60*60*24)/(10^9)
  # Subtract sum of all non-nested USGS gauges from the total pre-calculated water yield to get FNF-only water yield
  FNFtotalReference = sum(WaterYieldReferenceYubaFNF$TotalRunoff_km3)

  par(pty="s",mar=c(5,5,5,5))
  maxQ = max(c(HighFlowDHSVMvals,HighFlowReferenceYubaVals))
  minQ = min(c(HighFlowDHSVMvals,HighFlowReferenceYubaVals))
  plot(HighFlowReferenceYubaVals,HighFlowDHSVMvals,log="xy",pch=3,cex=1.5,lwd=2,main=paste0(FNFlongNameYuba,": DHSVM Model ",i,"\n95th Percentile Daily Flows"),xlab=expression("Highest Daily Full Natural Flows, "*m^3*"/s"),ylab=expression("Highest Daily Modeled Flows, "*m^3*"/s"),xlim=c(minQ,maxQ),ylim=c(minQ,maxQ),asp=1,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
  lines(seq(0.0001,1.5*maxQ,by=0.1),seq(0.0001,1.5*maxQ,by=0.1),col="red",lwd=2,lty=2)
  legend("bottomright",inset=0.1,legend=c("1:1 Line"),lty=2,lwd=2,col="red",cex=1.5)

  # Add text in different positions
  xPos = 10^(log10(minQ) + 0.02*(log10(maxQ)-log10(minQ)))
  yPos = 10^(log10(minQ) + 0.9*(log10(maxQ)-log10(minQ)))
  text(xPos,yPos,bquote("High-Flow RMSE = "*.(round(PeakRMSE))*" m"^3*"/s"),cex=1.5,pos=4)

  xPos = 10^(log10(minQ) + 0.02*(log10(maxQ)-log10(minQ)))
  yPos = 10^(log10(minQ) + 0.8*(log10(maxQ)-log10(minQ)))
  text(xPos,yPos,paste0("Total Runoff Error = ",round(100*(FNFtotalDHSVM/FNFtotalReference-1)),"%"),cex=1.5,pos=4)
  
  ################ American FNF
  
  # FNF peak flow plot
  HighFlowDHSVMvals = rev(sort(DHSVMfnfValsAmerican))[1:length(HighFlowReferenceAmerican$DailyAvgQ)]
  PeakRMSE = sqrt(mean((HighFlowDHSVMvals - HighFlowReferenceAmericanVals)^2))
  # Convert list of daily modeled values (m^3/s) into cumulative yield (km^3)
  FNFtotalDHSVM = sum(DHSVMfnfValsAmerican)*(60*60*24)/(10^9)
  # Subtract sum of all non-nested USGS gauges from the total pre-calculated water yield to get FNF-only water yield
  FNFtotalReference = sum(WaterYieldReferenceAmericanFNF$TotalRunoff_km3)
  
  par(pty="s",mar=c(5,5,5,5))
  maxQ = max(c(HighFlowDHSVMvals,HighFlowReferenceAmericanVals))
  minQ = min(c(HighFlowDHSVMvals,HighFlowReferenceAmericanVals))
  plot(HighFlowReferenceAmericanVals,HighFlowDHSVMvals,log="xy",pch=3,cex=1.5,lwd=2,main=paste0(FNFlongNameAmerican,": DHSVM Model ",i,"\n95th Percentile Daily Flows"),xlab=expression("Highest Daily Full Natural Flows, "*m^3*"/s"),ylab=expression("Highest Daily Modeled Flows, "*m^3*"/s"),xlim=c(minQ,maxQ),ylim=c(minQ,maxQ),asp=1,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
  lines(seq(0.0001,1.5*maxQ,by=0.1),seq(0.0001,1.5*maxQ,by=0.1),col="red",lwd=2,lty=2)
  legend("bottomright",inset=0.1,legend=c("1:1 Line"),lty=2,lwd=2,col="red",cex=1.5)
  
  # Add text in different positions
  xPos = 10^(log10(minQ) + 0.02*(log10(maxQ)-log10(minQ)))
  yPos = 10^(log10(minQ) + 0.9*(log10(maxQ)-log10(minQ)))
  text(xPos,yPos,bquote("High-Flow RMSE = "*.(round(PeakRMSE))*" m"^3*"/s"),cex=1.5,pos=4)
  
  xPos = 10^(log10(minQ) + 0.02*(log10(maxQ)-log10(minQ)))
  yPos = 10^(log10(minQ) + 0.8*(log10(maxQ)-log10(minQ)))
  text(xPos,yPos,paste0("Total Runoff Error = ",round(100*(FNFtotalDHSVM/FNFtotalReference-1)),"%"),cex=1.5,pos=4)
  
  par(pty="m")
}

################################################################################
# Evaluate the Bayesian credible intervals over the validation period
################################################################################

# Additional setup

errdir = "../ErrorModels/" # Where the files with Bayesian samples are stored

################################################################################

nBayesianSamples = 1000 # Should match length(ModelPars$xA)

SelectedModels = c(239, 270, 276)

# All Q units will end up in mm/d
HydrographData = data.frame(Date=seq(as.Date(paste0(min(StreamYears)-1,"-10-01")),
                                     as.Date(paste0(max(StreamYears),"-09-30")), by="days"),
                            QsimMed=NA,
                            Qsim10th=NA,
                            Qsim90th=NA,
                            Qmeas=NA)

HydrographEnsemble = matrix(nrow=length(SelectedModels)*nBayesianSamples,
                            ncol=length(HydrographData$Date))

# Set up basins
GaugeIDs = c(10343500, 10336676, 10336660, 10336645, 103366092, 10336610, 10336780, 10336730,
             11413000, 11413300,
             11427000, 11427700)

GaugeNames = c("Sagehen Creek", "Ward Creek", "Blackwood Creek", "General Creek", "Upper Truckee #1", "Upper Truckee #2", "Trout Creek", "Glenbrook Creek",
               "North Yuba River", "Slate Creek (Reconstructed)",
               "N.F. American River", "Duncan Canyon Creek")

CatchmentAreas = c(10.5, 9.7, 11.2, 7.44, 39.3, 54.9, 36.7, 4.11,
                   250, 49.4,
                   342, 9.9) # Areas in mi^2 from USGS

Wshd = "North Yuba River"
gauge = which(GaugeNames==Wshd)

# Read dataframe of streamflow data generated by ProcessStreamflowForDHSVM.R
ReferenceStreamflow = read.csv(paste0(ReferenceDir,"USGS_StreamflowData_Val.csv"))[,-1]
if (all(HydrographData$Date == ReferenceStreamflow$Date)){
  HydrographData$Qmeas = ReferenceStreamflow[,paste0("Gauge_",GaugeIDs[gauge])]
}

# Set up parametric uncertainty model
sdMeasFun = function(QmodVal, aVal, bVal, cVal){
  sdMeasVal = aVal / (1 + exp(-(bVal + cVal * log10(QmodVal)))) # Logistic with min --> 0, max --> d
  return(sdMeasVal)
}

Qsimulate = function(QmodVal, aVal, bVal, cVal){
  # Vectorized input of params or QmodVal only, not both
  nOut = length(QmodVal)*length(aVal)
  
  meanVal = log10(QmodVal)
  
  sdVal = sdMeasFun(QmodVal, aVal, bVal, cVal)
  
  QsimVal = 10 ^ rnorm(nOut, mean=rep(meanVal,nOut), sd=rep(sdVal,nOut))
  return(pmax(0, QsimVal))
}

# Create massive ensemble of DHSVM models and random errors
for (model in 1:length(SelectedModels)){
  
  ############## Read deterministic model output ##############
  # Loop over modeled watersheds to find which one contains the gauge of interest
  for (bsn in 1:length(BasinNames)){
    # Read the streamflow data output by DHSVM (drop first line that is date-only)
    DHSVMheader = read.table(paste0(ResultsDir,"Val_Params",SelectedModels[model],"_",BasinNames[bsn],"/Streamflow.Only"), nrows=1)
    DHSVMstreamflow = read.table(paste0(ResultsDir,"Val_Params",SelectedModels[model],"_",BasinNames[bsn],"/Streamflow.Only"), skip=2)
    names(DHSVMstreamflow) = DHSVMheader
    DHSVMstreamflow$DATE = as.Date(DHSVMstreamflow$DATE, format="%m.%d.%Y") # Convert to day only
    
    # Only proceed if the gauge of interest is in the current watershed
    if (paste0("Gauge_",GaugeIDs[gauge]) %in% DHSVMheader){
      
      # Reset the vector containing modeled values to be compared with reference data
      DHSVMvals = c()
      
      # Aggregate DHSVM output from m^3/timestep to m^3/s as a daily average for the current gauge
      for (day in 1:length(ReferenceStreamflow$Date)){
        Ptrs = which(DHSVMstreamflow$DATE == ReferenceStreamflow$Date[day])
        DailyQ = sum(DHSVMstreamflow[,paste0("Gauge_",GaugeIDs[gauge])][Ptrs])/(24*60*60) # m^3/timestep --> m^3/s
        
        # Append to hydrograph
        DHSVMvals = c(DHSVMvals, DailyQ)
        
      } # End of daily gauge aggregation loop
      
    }
  } # End of loop over watersheds
  # End of reading deterministic model output
  
  ############## Propagate random error ##############
  
  ModelPars = read.csv(paste0(errdir,"NormalSigmaErrorParamSamples_Model",SelectedModels[model],".csv"))[,-1]
  
  # Create distribution for each day
  startIdx = (model - 1) * nBayesianSamples + 1
  endIdx = startIdx + nBayesianSamples - 1
  
  for (d in 1:length(HydrographData$Qmeas)){
    # Make sure DHSVMvals are still in m^3/s units at this point!
    HydrographEnsemble[startIdx:endIdx,d] = Qsimulate(DHSVMvals[d], ModelPars$xA, ModelPars$xB, ModelPars$xC)
  }
  
}

# Convert m^3/s to mm/d
HydrographEnsemble = 1000 * HydrographEnsemble * (60*60*24) / (CatchmentAreas[gauge] * 2.58999 * 1000^2)
HydrographData$Qmeas = 1000 * HydrographData$Qmeas * (60*60*24) / (CatchmentAreas[gauge] * 2.58999 * 1000^2)

# Calculate daily quantiles
HydrographData$QsimMed = apply(HydrographEnsemble, 2, median)
HydrographData$Qsim10th = apply(HydrographEnsemble, 2, quantile, prob=0.1)
HydrographData$Qsim90th = apply(HydrographEnsemble, 2, quantile, prob=0.9)

################################################################################
# Plots

# Just for fun
matplot(t(HydrographEnsemble[1:100,]), type="l", log="")
lines(HydrographData$QsimMed, col="black", lwd=2)

######

PlotStart = as.Date("10-01-2005",format="%m-%d-%Y")
PlotEnd = as.Date("10-01-2006",format="%m-%d-%Y")

MinQ = 0.0
MaxQ = 85
LineWd = 3
par(mar=c(4,8,4,4),mgp=c(5,2,0),xaxs="i")
plot(HydrographData$Date, HydrographData$QsimMed, log="",
     type="l", col="blue", lwd=LineWd, ylim=c(MinQ,MaxQ), xlim=c(PlotStart,PlotEnd),
     xlab=NA,ylab="Streamflow, mm/d", cex.lab=3, xaxt="n", yaxt="n")
AxesMonths = seq(PlotStart, PlotEnd, by="months")
axis.Date(1, at=AxesMonths, format="%b-%Y", cex.axis=2, tck=-0.03, lwd=2) # start of water year
# AxesDays = seq(PlotStart, PlotEnd, by="days")
# axis.Date(1, at=AxesDays, format="%d-%b", cex.axis=2, tck=-0.03, lwd=2) # start of water year
axis(2, at=seq(0,MaxQ,10), lab=sprintf("%g",seq(0,MaxQ,10)), cex.axis=2, lwd=2)

lines(HydrographData$Date, HydrographData$Qsim10th,
      col="red", lwd=LineWd, lty=3)
lines(HydrographData$Date, HydrographData$Qsim90th,
      col="red", lwd=LineWd, lty=3)
lines(HydrographData$Date, HydrographData$Qmeas,
      col="black", lwd=LineWd, lty=1)

legend("topright",inset=0.05,legend=c("DHSVM Median","DHSVM 10-90%","Observed"),
       col=c("blue","red","black"), lty=c(1,3,1), lwd=rep(4,3), cex=1.5)
title(main=paste0(Wshd," Bayesian Simulation\nModels ",paste0(SelectedModels,collapse=", ")), cex.main=2)
box(lwd=2)





















