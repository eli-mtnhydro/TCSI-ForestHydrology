### MOBOhydro CALIBRATOR FOR DHSVM ###
# Session 5: Analyze current results and provide information to aid decision-maker

################################################################################
# Universal adjustable settings

ModelGen = 6

ObjFunNames = c("PeakSWE_RMSE","Total_NSE","LowBase_LogNSE","HighBase_LogNSE","WaterYield_RMSE","PeakFlow_RMSE")
ObjFunLongNames = c("Peak SWE","Total NSE","Low-Baseflow Log NSE","High-Baseflow Log NSE","Yearly Water Yield","95th-Percentile Peak Flows")
ObjFunLongNamesTight = c("Peak\nSWE","Total\nNSE","Low-Baseflow\nLog NSE","High-Baseflow\nLog NSE","Yearly\nWater Yield","95th-Percentile\nPeak Flows")
nObjFuns = length(ObjFunNames)

ParamNames = c("SOILD","SLHC","POROSITY","EXPDEC","MINRES","SNOWTEMP","ALBMELTRATE")
ParamLongNames = c("Soil Depth","Hydraulic Conductivity","Porosity","Ksat Exponential Decrease","Minimum Stomatal Resistance","Snow Temperature Threshold","Melt-Season Albedo Decay Rate")
ParamLongNamesTight = c("Soil\nDepth","Hydraulic\nConductivity","Porosity","Ksat Exp.\nDecrease","Min. Stomatal\nResistance","Snow Temp.\nThreshold","Melt-Season\nAlbedo Decay")
nParams = length(ParamNames)

BasinNames = c("Truckee","Yuba")

# Extra information to visualize particular objective functions
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
# Find current Pareto front based on all previous designs and responses
################################################################################

library(GPareto)
library(GGally)
library(scatterplot3d)
library(ggplot2)
library(patchwork)
library(viridis)
library(scales)
library(hydroGOF)

design.grid.data = read.csv(paste0("Generation",ModelGen,"_DesignGrid.csv"))[,-1]
results.grid.data = read.csv(paste0("Generation",ModelGen,"_ResultsGrid.csv"))[,-1]
response.grid.data = read.csv(paste0("Generation",ModelGen,"_ResponseGrid.csv"))[,-1]
ParetoFront = read.csv(paste0("Generation",ModelGen,"_ParetoFront.csv"))[,-1]

print(head(design.grid.data))
print(head(response.grid.data))

# Simplify the structure into pure matrices of numbers
design.grid = unname(as.matrix(design.grid.data[,2:(1+nParams)],ncol=nParams))
response.grid = unname(as.matrix(response.grid.data[,2:(1+nObjFuns)],ncol=nObjFuns))

nDesigns = length(design.grid[,1]) # For easy usage elsewhere

# Find parameter sets that are part of the current Pareto front
parIndx = ParetoFront$ParamSet

################################################################################
# Basic visualization of some results on the current Pareto front
################################################################################

# Note most of these plot layouts are hard-coded to the number of objective functions etc.

# Plot histograms of all prior designs
par(mar=c(0,1,1,1),pty="m")
layout(mat=matrix(c(rep(1,4),2:9),ncol=4,byrow=TRUE), heights=c(0.1,0.45,0.45))
plot.new()
text(0.5,0.5,"All Designs",cex=4,font=2)
par(mar=c(5,5,5,1))
BreakLocs = seq(0,1,0.1)
for (i in 1:nParams){
  hist(design.grid[,i],main=paste0(ParamLongNames[i]),xlim=c(0,1),breaks=BreakLocs,axes=FALSE,xlab="Parameter Scale",ylab="",cex.lab=2,cex.main=2.5)
  axis(side=1,at=BreakLocs,cex.axis=2)
  axis(side=2,cex.axis=2)
}
plot.new()
text(0.5,0.5,paste0("Total Number of\nDesigns:\n",length(design.grid[,i]),"\n\nAfter Generation:\n",ModelGen),cex=2.5,font=2)

# Plot histograms of the current Pareto front in design space
par(mar=c(0,1,1,1),pty="m")
layout(mat=matrix(c(rep(1,4),2:9),ncol=4,byrow=TRUE), heights=c(0.1,0.45,0.45))
plot.new()
text(0.5,0.5,"Non-Dominated Designs",cex=4,font=2)
par(mar=c(5,5,5,1))
BreakLocs = seq(0,1,0.1)
for (i in 1:nParams){
  hist(design.grid[parIndx,i],main=paste0(ParamLongNames[i]),xlim=c(0,1),breaks=BreakLocs,axes=FALSE,xlab="Parameter Scale",ylab="",cex.lab=2,cex.main=2.5)
  axis(side=1,at=BreakLocs,cex.axis=2)
  axis(side=2,cex.axis=2)
}
plot.new()
text(0.5,0.5,paste0("Number of\nNon-Dominated Designs:\n",length(parIndx),"\n\nAfter Generation:\n",ModelGen),cex=2.5,font=2)

# Plot histograms of the current Pareto set in objective space
par(mar=c(0,1,1,1),pty="m")
layout(mat=matrix(c(rep(1,3),2:7),ncol=3,byrow=TRUE), heights=c(0.1,0.45,0.45))
plot.new()
text(0.5,0.5,"Pareto Set in Objective Space (Non-Dominated Responses)",cex=4,font=2)
par(mar=c(5,5,5,1))
BreakLocs = seq(0,1,0.1)
for (i in 1:nObjFuns){
  hist(response.grid[parIndx,i],main=paste0(ObjFunLongNames[i]),xlim=c(0,1),breaks=BreakLocs,axes=FALSE,xlab="",ylab="",cex.lab=2,cex.main=2.5)
  axis(side=1,at=BreakLocs,cex.axis=2)
  axis(side=2,cex.axis=2)
}

##########################

# Way oversimplified 1d plots of design-response relationships because why not?
BreakLocs = seq(0,1,0.1)
for (i in 1:nParams){
  par(mar=c(2,5,0,2),mfrow=c(2,4),pty="s")
  for (j in 1:nObjFuns){
    plot(design.grid[parIndx,i],response.grid[parIndx,j],pch=8,cex=1.5,xlab=ParamLongNames[i],ylab=ObjFunLongNames[j],xlim=c(0,1),ylim=c(0,1),asp=1,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
  }
}

##########################

# Correlogram of objective functions
par(mfrow=c(1,1),pty="m")

CorrelData = data.frame(response.grid.data[parIndx,-1])
#CorrelData = data.frame(response.grid.data[,-1])
names(CorrelData) = ObjFunLongNamesTight

# Duplicated values are indicative of missing parameter sets, since those were given all the same worst values
CorrelData[duplicated(CorrelData),] = NA

p1 = ggpairs(CorrelData, upper=list(continuous=wrap("cor", digits=1, size=6, col="black")), lower=list(continuous=wrap("smooth", alpha=0.3))) +
  scale_x_continuous(breaks=breaks_pretty(n=3)) +
  theme(strip.text.x = element_text(size=12,face="bold"), strip.text.y = element_text(size=12,face="bold")) +
  theme(axis.text.x=element_text(size=8,color="black"), axis.text.y=element_text(size=8,color="black"))

p2 = ggcorr(CorrelData, low="#F21A00", mid="#EEEEEE", high="#3B9AB2", label=TRUE, label_round=1)

# Change background color to tiles in the upper triangular matrix of plots
g2 = ggplotGrob(p2)
colors = g2$grobs[[6]]$children[[3]]$gp$fil

idx = 1
for (k1 in 1:(nObjFuns-1)) {
  for (k2 in (k1+1):nObjFuns) {
    plt = getPlot(p1,k1,k2) +
      theme(panel.background=element_rect(fill=colors[idx], color="white"),
            panel.grid.major=element_line(color=colors[idx]))
    p1 = putPlot(p1,plt,k1,k2)
    idx = idx+1
  }
}

print(p1)

# Correlogram of Pareto-optimal parameters
par(mfrow=c(1,1),pty="m")

CorrelData = data.frame(design.grid.data[parIndx,-1])
#CorrelData = data.frame(design.grid.data[which(results.grid.data$Total_NSE>0.8),-1])
names(CorrelData) = ParamLongNamesTight

# Duplicated values are indicative of missing parameter sets, since those were given all the same worst values
CorrelData[duplicated(CorrelData),] = NA

p1 = ggpairs(CorrelData, upper=list(continuous=wrap("cor", digits=1, size=6, col="black")), lower=list(continuous=wrap("smooth", alpha=0.3))) +
  scale_x_continuous(breaks=breaks_pretty(n=3)) +
  theme(strip.text.x = element_text(size=12,face="bold"), strip.text.y = element_text(size=12,face="bold")) +
  theme(axis.text.x=element_text(size=8,color="black"), axis.text.y=element_text(size=8,color="black"))

p2 = ggcorr(CorrelData, low="#F21A00", mid="#EEEEEE", high="#3B9AB2", label=TRUE, label_round=1)

# Change background color to tiles in the upper triangular matrix of plots
g2 = ggplotGrob(p2)
colors = g2$grobs[[6]]$children[[3]]$gp$fil

idx = 1
for (k1 in 1:(nParams-1)) {
  for (k2 in (k1+1):nParams) {
    plt = getPlot(p1,k1,k2) +
      theme(panel.background=element_rect(fill=colors[idx], color="white"),
            panel.grid.major=element_line(color=colors[idx]))
    p1 = putPlot(p1,plt,k1,k2)
    idx = idx+1
  }
}

print(p1)

##########################

# 2d slices of Pareto front
par(mar=c(5,4,4,2),mfrow=c(1,1),pty="s")
plotResp = list(c(1,2),c(2,3),c(2,4),c(2,5),c(2,6),c(3,4),c(3,5),c(3,6),c(4,5),c(4,6),c(5,6))
for (i in 1:length(plotResp)){
  plot(response.grid[parIndx,plotResp[[i]][1]],response.grid[parIndx,plotResp[[i]][2]],pch=8,cex=1.5,main=paste0(ObjFunLongNames[plotResp[[i]][1]]," vs. ",ObjFunLongNames[plotResp[[i]][2]],"\nNon-Dominated Models"),xlab=ObjFunLongNames[plotResp[[i]][1]],ylab=ObjFunLongNames[plotResp[[i]][2]],xlim=c(0,1),ylim=c(0,1),asp=1,cex.axis=1.5,cex.lab=2.5,cex.main=2)
}

# Illustrative examples
par(mar=c(5,5,5,2),mfrow=c(1,1),pty="s")
plot(results.grid.data[,2+1],results.grid.data[,5+1],pch=8,cex=1.5,main=paste0(ObjFunLongNames[2]," vs. ",ObjFunLongNames[5],"\nPareto Frontier, All Models"),xlab=ObjFunLongNames[2],ylab=expression("Yearly Water Yield RMSE, "*km^3/yr),cex.axis=1.5,cex.lab=2.5,cex.main=2)
plot(results.grid.data[,3+1],results.grid.data[,4+1],pch=8,cex=1.5,main=paste0(ObjFunLongNames[3]," vs. ",ObjFunLongNames[4],"\nPareto Frontier, All Models"),xlab=ObjFunLongNames[3],ylab=ObjFunLongNames[4],xlim=c(0,1),ylim=c(0,1),cex.axis=1.5,cex.lab=2.5,cex.main=2)

# 3d slices of Pareto front
plotResp = c(1,2,5)
scatterplot3d(response.grid[parIndx,plotResp[1]],response.grid[parIndx,plotResp[2]],response.grid[parIndx,plotResp[3]], main=paste0("Obj. Fun. ",plotResp[1]," vs. Obj. Fun. ",plotResp[2]," vs. Obj. Fun. ",plotResp[3],"\nNon-Dominated Models"),xlim=c(0,1),ylim=c(0,1),zlim=c(0,1))

################################################################################
# Plot un-scaled objective function ranges in this new funky way I invented
################################################################################

ObjFunTypes = c("RMSE","NSE","NSE","NSE","RMSE","RMSE")

ObjFun = c()
ObjFunType = c()
Generation = c()
ErrMode = c() # 1 --> worst, 2 --> median, 3 --> best
ErrVal = c()

for (obj in 1:nObjFuns){
  for (gen in 1:ModelGen){
    # Read Pareto front data for a previous (or current) generation
    ParetoGenData = read.csv(paste0("Generation",gen,"_ParetoFront.csv"))[,-1]
    
    # Collate salient info
    ObjFun = c(ObjFun, rep(ObjFunLongNames[obj], 3))
    ObjFunType = c(ObjFunType, rep(ObjFunTypes[obj], 3))
    Generation = c(Generation, rep(gen, 3))
    ErrMode = c(ErrMode, c("Worst","Median","Best"))
    
    # Stats over all param sets in a particular Pareto front
    # Column indexing skips ParamSet, parameter scaling values, and scaled responses to get the right response value
    
    if (ObjFunTypes[obj]=="NSE"){
      # Bigger values are better
      BestVal = max(ParetoGenData[,(1+nParams+nObjFuns+obj)])
      MedianVal = median(ParetoGenData[,(1+nParams+nObjFuns+obj)])
      WorstVal = min(ParetoGenData[,(1+nParams+nObjFuns+obj)])
      
    } else {
      # Smaller values are better (RMSE)
      BestVal = min(ParetoGenData[,(1+nParams+nObjFuns+obj)])
      MedianVal = median(ParetoGenData[,(1+nParams+nObjFuns+obj)])
      WorstVal = max(ParetoGenData[,(1+nParams+nObjFuns+obj)])
      
    }
    ErrVal = c(ErrVal,WorstVal,MedianVal,BestVal) # Append new data
  }
}

ParetoData = data.frame(ObjFun, ObjFunType, Generation, ErrMode, ErrVal)

# Replace names with plot names that include newlines
for (obj in 1:nObjFuns){
  Ptrs = which(ParetoData$ObjFun == ObjFunLongNames[obj])
  ParetoData$ObjFun[Ptrs] = ObjFunLongNamesTight[obj]
}

# Plot order of objective functions
ObjFunPlotOrder = c("Peak\nSWE","Yearly\nWater Yield","95th-Percentile\nPeak Flows","Total\nNSE","Low-Baseflow\nLog NSE","High-Baseflow\nLog NSE")
Ylabels = c("Pixel-Wise RMSE, m",expression("Yearly RMSE, "~km^3),expression("Daily RMSE, "~m^3*"/s"),"Area-Weighted NSE","Area-Weighted Log NSE","Area-Weighted Log NSE")

barwidth = 0.9
gen_colors = c("gray75","gray50","gray25")
obj_colors = c("darkturquoise","seagreen3","darkgoldenrod2","dodgerblue","mediumorchid1","darkorchid3")
stat_colors = c("green4","black","red2")

PlotList = list()
for (i in 1:nObjFuns){
  PlotList[[i]] = ggplot() + geom_bar(data=ParetoData[ParetoData$ObjFun==ObjFunPlotOrder[i],], mapping=aes(x=ErrMode, y=ErrVal, fill=as.factor(Generation)), stat="identity", position="dodge", width=barwidth) +
    scale_fill_manual(values=gen_colors[1:ModelGen],labels=paste0("Gen. ",1:ModelGen)) +
    scale_y_continuous(limits=c(NA,NA), breaks=pretty(c(0, ParetoData[ParetoData$ObjFun==ObjFunPlotOrder[i],]$ErrVal), n = 6)) +
    labs(fill="Generation") + labs(x="", y=Ylabels[i]) +
    facet_wrap(~ObjFun) +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45,size=16,hjust=1,color=stat_colors), axis.title.y=element_text(size=16,margin=margin(0,5,0,0)), axis.text.y=element_text(size=16,color="black"), legend.title=element_text(size=24), legend.text=element_text(size=16)) +
    theme(legend.spacing.y=unit(0.2,"cm"),axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(5,5,1,5),panel.background=element_rect("white", "black"), panel.grid=element_blank()) +
    theme(strip.text = element_text(color=obj_colors[i],size=24,face="bold"),strip.background=element_rect(color="black",fill=NA)) +
    guides(fill=guide_legend(byrow=TRUE))
}

patchwork::wrap_plots(PlotList, nrow=2, guides="collect")

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
ReferenceMaxDateRasters = lapply(paste0(ReferenceDir,"TCSI_max_swe_dowy_WY",SnowYears,".tif"), raster)

# Only keep max SWE dates if max SWE > 5 cm to exclude small storms that could have competing peaks far apart
MaxSWEthresh = 0.05

SWEptrsByYear = list()
DatePtrsByYear = list()
for (yr in 1:nSnowYears){
  SWEptrsByYear[[yr]] = which(is.finite(ReferenceSWErasters[[yr]][,]) & (Mask[,] > 0))
  print(length(SWEptrsByYear[[yr]]))
  
  DatePtrsByYear[[yr]] = which(is.finite(ReferenceMaxDateRasters[[yr]][,]) & (ReferenceSWErasters[[yr]][,] > MaxSWEthresh) & (Mask[,] > 0))
  print(length(DatePtrsByYear[[yr]]))
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
  RawRasterStackSWE = lapply(paste0(ResultsDir,"Params",i,"_",BasinNames,"/Map.Snow.MaxSwe.txt"), readLines)
  RawRasterStackDate = lapply(paste0(ResultsDir,"Params",i,"_",BasinNames,"/Map.Snow.MaxSweDate.txt"), readLines)
  
  # Loop over each year with snow data
  for (yr in 1:nSnowYears){
    # Loop over each watershed
    for (bsn in 1:length(BasinNames)){
      # Subset a raster from the stack
      SingleMapSWE = RawRasterStackSWE[[bsn]][((yr-1)*nRows+1):(nRows*yr)]
      SingleMapDate = RawRasterStackDate[[bsn]][((yr-1)*nRows+1):(nRows*yr)]
      
      # Write single ascii raster and read it back in as a raster object
      writeLines(c(ASCIIheader,SingleMapSWE),paste0("tempSWE",bsn,".asc"))
      SingleRasterSWE = raster(paste0("tempSWE",bsn,".asc"), crs=as.character(MaskCRS))
      
      writeLines(c(ASCIIheader,SingleMapDate),paste0("tempDate",bsn,".asc"))
      SingleRasterDate = raster(paste0("tempDate",bsn,".asc"), crs=as.character(MaskCRS))
      
      # Convert date as YYYYMMDD --> day of water year
      SingleRasterDates = as.Date(as.character(SingleRasterDate[,]),format="%Y%m%d")
      DayOfYear = as.numeric(strftime(SingleRasterDates,format="%j"))
      DayOfWaterYear = DayOfYear + 92 # Day of year --> day of water year
      DayOfWaterYear[is.na(DayOfWaterYear)] = 0
      # Fix rollover where Oct-Jan are >365
      WaterYear = max(as.numeric(format(SingleRasterDates[!is.na(SingleRasterDates)],"%Y")))
      EndOfWaterYear = as.numeric(strftime(as.Date(paste0(WaterYear,"-09-30"),format="%Y-%m-%d"),format="%j"))
      DaysThisYear = as.numeric(strftime(as.Date(paste0(WaterYear,"-12-31"),format="%Y-%m-%d"),format="%j"))
      DayOfWaterYear[DayOfWaterYear > EndOfWaterYear] = DayOfWaterYear[DayOfWaterYear > EndOfWaterYear] - DaysThisYear
      SingleRasterDate[,] = DayOfWaterYear
      
      # Merge watersheds by keeping the maximum value
      if (bsn==1){
        MergedRasterSWE = SingleRasterSWE
        MergedRasterDate = SingleRasterDate
      } else {
        MergedRasterSWE = max(MergedRasterSWE, SingleRasterSWE)
        MergedRasterDate = max(MergedRasterDate, SingleRasterDate)
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
    VisMaskDate = Mask*NA
    VisMaskDate[DatePtrsByYear[[yr]]] = 1
    
    RastDHSVM_DF = as.data.frame(as(aggregate(MergedRasterDate*VisMaskDate,AggrFactor),"SpatialPixelsDataFrame"))
    RastDHSVM_DF$Date = rep(SnowYears[yr],length(RastDHSVM_DF$layer))
    RastDHSVM_DF$Type = rep(paste0("DHSVM\nModel ",i),length(RastDHSVM_DF$layer))
    
    RastReference_DF = as.data.frame(as(aggregate(ReferenceMaxDateRasters[[yr]]*VisMaskDate,AggrFactor),"SpatialPixelsDataFrame"))
    RastReference_DF$Date = rep(SnowYears[yr],length(RastReference_DF$layer))
    RastReference_DF$Type = rep("Margulis et. al\nReanalysis",length(RastReference_DF$layer))
    
    # RMSE for this year only
    DHSVMvals = MergedRasterDate[DatePtrsByYear[[yr]]]
    ReferenceVals = ReferenceMaxDateRasters[[yr]][DatePtrsByYear[[yr]]]
    DateRMSE = sqrt(mean((DHSVMvals-ReferenceVals)^2))
    
    YearlyMeanDate = as.Date(round(mean(ReferenceVals))-1, origin=paste0(WaterYear-1,"-10-01")) # Origin is zero-based
    
    if (yr==1){
      CombinedDataDate = rbind(RastDHSVM_DF,RastReference_DF)
      DataTextDate = data.frame(label=c(paste0("RMSE\n",round(DateRMSE)," days"),paste0("Mean\n",format(YearlyMeanDate,"%b. %d"))),Date=rep(SnowYears[yr],2),Type=c(paste0("DHSVM\nModel ",i),"Margulis et. al\nReanalysis"))
      #DataTextDate = data.frame(label=c(paste0("RMSE\n",round(DateRMSE)," days"),paste0("Mean\n",round(mean(ReferenceVals))," (DOWY)")),Date=rep(SnowYears[yr],2),Type=c(paste0("DHSVM\nModel: ",i),"Margulis et. al\nReanalysis"))
    } else {
      CombinedDataDate = rbind(CombinedDataDate,RastDHSVM_DF,RastReference_DF)
      DataTextDate = rbind(DataTextDate,data.frame(label=c(paste0("RMSE\n",round(DateRMSE)," days"),paste0("Mean\n",format(YearlyMeanDate,"%b. %d"))),Date=rep(SnowYears[yr],2),Type=c(paste0("DHSVM\nModel ",i),"Margulis et. al\nReanalysis")))
    }
    
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
  g2 = ggplot() + geom_tile(data=CombinedDataDate, aes(x=x,y=y,fill=layer)) +
    geom_text(data=DataTextDate, mapping=aes(x=680000,y=4300000,label=label),hjust=0,vjust=0.5,size=6) +
    scale_fill_gradientn(colors=plasma(256),limits=c(60,240),breaks=seq(60,240,30),labels=paste0(seq(60,240,30))) +
    guides(fill=guide_colorbar(ticks.colour=NA,frame.colour="black")) +
    coord_equal() +
    labs(fill="DOWY",title="Pixel-Wise Peak SWE Date") +
    theme_bw() + theme(plot.title=element_text(color="black",size=24,hjust=0.5),plot.margin=margin(1,1,1,1),axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank()) +
    theme(legend.position="right",legend.key.height=unit(2,"cm"),legend.title=element_text(size=18), legend.text=element_text(size=18)) +
    theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA)) +
    scale_x_continuous(expand=c(0,0),limits=c(660000,770000)) +
    scale_y_continuous(expand=c(0,0),limits=c(4275000,4407000)) +
    facet_grid(cols=vars(Date),rows=vars(Type),switch="y")
  
  print(g1)
  Sys.sleep(20)
  print(g2)
  
}

# Clean up ASCII rasters
TempFiles = list.files(pattern = ".asc")
for (fil in 1:length(TempFiles)){
  file.remove(TempFiles[[fil]])
}

################################################################################
# Plot hydrographs for user-selected individual parameter sets
################################################################################

GaugePlotOrder = c("Glenbrook Creek", "General Creek", "Ward Creek", "Sagehen Creek", "Blackwood Creek", "Trout Creek", "Upper Truckee #1", "Upper Truckee #2", "Slate Creek (Reconstructed)", "North Yuba River")

GaugeIDs = c(10343500, 10336676, 10336660, 10336645, 103366092, 10336610, 10336780, 10336730, 11413000, 11413300)
GaugeNames = c("Sagehen Creek", "Ward Creek", "Blackwood Creek", "General Creek", "Upper Truckee #1", "Upper Truckee #2", "Trout Creek", "Glenbrook Creek", "North Yuba River", "Slate Creek (Reconstructed)")
CatchmentAreas = c(10.5, 9.7, 11.2, 7.44, 39.3, 54.9, 36.7, 4.11, 250, 49.4) # Areas in mi^2 from USGS

# Read dataframe of streamflow data generated by ProcessStreamflowForDHSVM.R
ReferenceStreamflow = read.csv(paste0(ReferenceDir,"USGS_StreamflowData.csv"))[,-1]
head(ReferenceStreamflow)

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

SubplotOption = FALSE # Whether to print a bunch of per-gauge plots

# Choose which parameter sets are of interest
VisualizeParameters = c(195,270,276)

# Loop over each parameter set and calculate the objective function value
for (i in VisualizeParameters){
  # Loop over modeled watersheds
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
    if (paste0("Gauge_",FNFname) %in% DHSVMheader){

      # Compile a vector of all daily FNF values to later sort for high flows
      DHSVMfnfVals = c()

      # Aggregate DHSVM output from m^3/timestep to m^3/s as a daily average for the current gauge
      # Note that days are iterated over ReferenceStreamflow for convenience since it already has a daily calendar
      for (day in 1:length(ReferenceStreamflow$Date)){
        Ptrs = which(DHSVMstreamflow$DATE == ReferenceStreamflow$Date[day])
        DailyQ = sum(DHSVMstreamflow[,paste0("Gauge_",FNFname)][Ptrs])/(24*60*60) # m^3/timestep --> m^3/s
        
        DHSVMfnfVals = c(DHSVMfnfVals, DailyQ)
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
  
  g1 = patchwork::wrap_plots(PlotList, nrow=5, guides="collect") & theme(legend.position = "bottom")
  
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
  
  g2 = patchwork::wrap_plots(PlotList, nrow=5, guides="collect") & theme(legend.position = "bottom")
  
  print(g2)
  Sys.sleep(5)
  
  # FNF peak flow plot
  HighFlowDHSVMvals = rev(sort(DHSVMfnfVals))[1:length(HighFlowReference$DailyAvgQ)]
  PeakRMSE = sqrt(mean((HighFlowDHSVMvals - HighFlowReferenceVals)^2))
  # Convert list of daily modeled values (m^3/s) into cumulative yield (km^3)
  FNFtotalDHSVM = sum(DHSVMfnfVals)*(60*60*24)/(10^9)
  # Subtract sum of all non-nested USGS gauges from the total pre-calculated water yield to get FNF-only water yield
  FNFtotalReference = sum(WaterYieldReference$TotalRunoff_km3) - sum(ReferenceStreamflow[,-1][,(GaugeIDs %in% NonNestedGaugeIDs)])*(60*60*24)/(10^9)

  par(pty="s",mar=c(5,5,5,5))
  maxQ = max(c(HighFlowDHSVMvals,HighFlowReferenceVals))
  minQ = min(c(HighFlowDHSVMvals,HighFlowReferenceVals))
  plot(HighFlowReferenceVals,HighFlowDHSVMvals,log="xy",pch=3,cex=1.5,lwd=2,main=paste0(FNFlongName,": DHSVM Model ",i,"\n95th Percentile Daily Flows"),xlab=expression("Highest Daily Full Natural Flows, "*m^3*"/s"),ylab=expression("Highest Daily Modeled Flows, "*m^3*"/s"),xlim=c(minQ,maxQ),ylim=c(minQ,maxQ),asp=1,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
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
# Plotting individual (enlarged) hydrographs on custom timeperiods
################################################################################

GaugeOfInterest = 11413000
BasinOfInterest = 2

PlotStart = as.Date("01-01-2017",format="%m-%d-%Y")
PlotEnd = as.Date("03-15-2017",format="%m-%d-%Y")

#PlotStart = as.Date("10-01-2015",format="%m-%d-%Y")
#PlotEnd = as.Date("10-01-2017",format="%m-%d-%Y")

GaugeIDs = c(10343500, 10336676, 10336660, 10336645, 103366092, 10336610, 10336780, 10336730, 11413000, 11413300)
GaugeNames = c("Sagehen Creek", "Ward Creek", "Blackwood Creek", "General Creek", "Upper Truckee #1", "Upper Truckee #2", "Trout Creek", "Glenbrook Creek", "North Yuba River", "Slate Creek (Reconstructed)")
CatchmentAreas = c(10.5, 9.7, 11.2, 7.44, 39.3, 54.9, 36.7, 4.11, 250, 49.4) # Areas in mi^2 from USGS

# Read dataframe of streamflow data generated by ProcessStreamflowForDHSVM.Rhttp://127.0.0.1:17109/graphics/7eb237bc-1492-4d37-b5ab-48989765d50b.png
ReferenceStreamflow = read.csv(paste0(ReferenceDir,"USGS_StreamflowData.csv"))[,-1]
head(ReferenceStreamflow)

# Convert m^3/s to mm/d
ReferenceVals = ReferenceStreamflow[,paste0("Gauge_",GaugeOfInterest)]
ReferenceVals = 1000 * ReferenceVals * (60*60*24) / (CatchmentAreas[which(GaugeIDs==GaugeOfInterest)] * 2.58999 * 1000^2)

# Create initial plot
MinQ = 0
MaxQ = 80
par(mar=c(4,8,4,4),mgp=c(5,2,0),xaxs="i",yaxs="i")
#par(mar=c(4,8,6,6),mgp=c(5,2,0),xaxs="i",yaxs="i")
plot(as.Date(ReferenceStreamflow$Date), ReferenceVals, type="l", col="black", lwd=3, ylim=c(MinQ,MaxQ), xlim=c(PlotStart,PlotEnd),
     xlab=NA,ylab="Streamflow, mm/d", cex.lab=3, xaxt="n", yaxt="n")
AxesMonths = seq(PlotStart, PlotEnd, by="months")
axis.Date(1, at=AxesMonths, format="%b-%Y", cex.axis=2, tck=-0.03, lwd=2) # start of water year
#AxesDays = seq(PlotStart, PlotEnd, by="1 days")
#axis.Date(1, at=AxesDays, format="%b-%d", cex.axis=2, tck=-0.03, lwd=2) # start of water year
#axis(2, at=c(0.1,1,10,100), lab=sprintf("%g",c(0.1,1,10,100)), cex.axis=2.5, lwd=2)
axis(2, at=seq(0,MaxQ,10), lab=sprintf("%g",seq(0,MaxQ,10)), cex.axis=2, lwd=2)

ColIdx = 1
#ColorPal = c("dodgerblue","hotpink","olivedrab","turquoise3","darkgoldenrod2","deepskyblue2","darkorchid2","cyan","sienna2","slateblue2","seagreen","red","green3","blue","yellow")
#ColorPal = c("turquoise","darkorchid2","darkgoldenrod2","dodgerblue","red","yellow")
#ColorPal = c("dodgerblue","hotpink","olivedrab","darkgoldenrod2","darkorchid2","cyan","red","green3","blue","yellow")
ColorPal = c("cyan","darkorchid2","olivedrab","dodgerblue","red","yellow")

# All non-gray named colors from R graphical devices
CrazyColors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

# Cond1 = (results.grid.data$Total_NSE > 0.80)
# Cond2 = (results.grid.data$PeakFlow_RMSE < 80)
# Cond3 = (results.grid.data$WaterYield_RMSE < 0.4)
# Cond4 = (results.grid.data$LowBase_LogNSE > 0.6)
# Cond5 = (results.grid.data$HighBase_LogNSE > 0.0)
# CondOR = (results.grid.data$Total_NSE > 0.84)

#VisualizeParameters = design.grid.data$ParamSet
#VisualizeParameters = parIndx
#VisualizeParameters = which(results.grid.data$ParamSet %in% parIndx & ((Cond1 & Cond2 & Cond3 & Cond4 & Cond5) | CondOR))
VisualizeParameters = c(195,239,244,270,276,294)
#VisualizeParameters = c(239,270,276)
VisualizeParameters = 276

length(VisualizeParameters)

NSEavg = 0

# Loop over each parameter set and calculate the objective function value
for (i in VisualizeParameters){
  
  # Check whether results are available for this parameter set
  if (dir.exists(paste0(ResultsDir,"Params",i,"_",BasinNames[BasinOfInterest]))){
    # Read the streamflow data output by DHSVM (drop first line that is date-only)
    DHSVMheader = read.table(paste0(ResultsDir,"Params",i,"_",BasinNames[BasinOfInterest],"/Streamflow.Only"), nrows=1)
    DHSVMstreamflow = read.table(paste0(ResultsDir,"Params",i,"_",BasinNames[BasinOfInterest],"/Streamflow.Only"), skip=2)
    names(DHSVMstreamflow) = DHSVMheader
    DHSVMstreamflow$DATE = as.Date(DHSVMstreamflow$DATE, format="%m.%d.%Y") # Convert to day only
    
    # Reset the vector containing modeled values to be compared with reference data
    DHSVMvals = c()
    
    # Aggregate DHSVM output from m^3/timestep to m^3/s as a daily average for the current gauge
    for (day in 1:length(ReferenceStreamflow$Date)){
      Ptrs = which(DHSVMstreamflow$DATE == ReferenceStreamflow$Date[day])
      DailyQ = sum(DHSVMstreamflow[,paste0("Gauge_",GaugeOfInterest)][Ptrs])/(24*60*60) # m^3/timestep --> m^3/s
      
      # Append to hydrograph
      DHSVMvals = c(DHSVMvals, DailyQ)
      
    } # End of daily gauge aggregation loop
    
    # Convert m^3/s to mm/d
    DHSVMvals = 1000 * DHSVMvals * (60*60*24) / (CatchmentAreas[which(GaugeIDs==GaugeOfInterest)] * 2.58999 * 1000^2)
    
    NSEval = NSE(sim=DHSVMvals, obs=ReferenceVals)
    NSEavg = NSEavg + (1/length(VisualizeParameters)) * NSEval
    
    # Plot
    if (length(VisualizeParameters) > length(ColorPal)){
      lines(as.Date(ReferenceStreamflow$Date), DHSVMvals, col=sample(CrazyColors), lwd=2)
    } else if (length(VisualizeParameters)==1){
      lines(as.Date(ReferenceStreamflow$Date), DHSVMvals, col="dodgerblue", lwd=4)
    } else {
      lines(as.Date(ReferenceStreamflow$Date), DHSVMvals, col=ColorPal[ColIdx], lwd=8-6*ColIdx/length(VisualizeParameters))
      ColIdx = ColIdx + 1
    }
    
  }
  print(i)
  print(NSEval)
}

# Align to here:
#**********************************************************

# Add desired components
lines(as.Date(ReferenceStreamflow$Date), ReferenceVals, col="white", lwd=4, lty=1) # 8 for all, 4 for select
lines(as.Date(ReferenceStreamflow$Date), ReferenceVals, col="black", lwd=4, lty=3)

box(lwd=2)

legend("topright",inset=0.05,legend=c("USGS Gauged",paste0("DHSVM Model ",VisualizeParameters)),col=c("black","dodgerblue"),lty=c(3,1),lwd=rep(6,2),cex=2,seg.len=2)
#legend("topright",inset=0.05,legend=c("USGS Gauged",paste0("DHSVM Model ",VisualizeParameters)),col=c("black",ColorPal),lty=c(3,rep(1,length(ColorPal))),lwd=rep(6,length(ColorPal)+1),cex=1.5,seg.len=2.5)
#legend("topright",inset=0.05,legend=c("USGS Gauged","","","All DHSVM Models","",""),col=c("black","red","green","blue","darkorchid2","darkgoldenrod2"),lty=c(3,rep(1,5)),lwd=rep(6,5+1),cex=1.5,seg.len=2.5)
#legend("topleft",inset=0.05,legend=c("Measured"),col=c("black"),lty=c(3),lwd=8,cex=2,seg.len=2.5)

#text(as.Date("4-01-2017",format="%m-%d-%Y"),0.4,"Pareto-Optimal\n& Downstream NSE > 0.8\n& Headwaters NSE > 0.6\n& High-Baseflow Log NSE > 0.6\n& Low-Baseflow Log NSE > 0.6", cex=1.5)
#text(as.Date("4-01-2017",format="%m-%d-%Y"),0.4,"\"Knee Point\" in\nPareto Front Between\nHeadwaters / Downstream NSE\n& High / Low-Baseflow Log NSE", cex=1.5)

#text(as.Date("12-06-2016",format="%m-%d-%Y"),9.4,"Pareto-Optimal\n& Downstream NSE > 0.8\n& Headwaters NSE > 0.6\n& High-Baseflow Log NSE > 0.6\n& Low-Baseflow Log NSE > 0.6", cex=1.5)
#text(as.Date("12-06-2016",format="%m-%d-%Y"),9.4,"\"Knee Point\" in\nPareto Front Between\nHeadwaters / Downstream NSE\n& High / Low-Baseflow Log NSE", cex=1.5)

# AreaStr = paste0("Area: ", signif(CatchmentAreas[which(GaugeIDs==GaugeOfInterest)] * 2.58999, 2), " km")
# text(as.Date("01-01-2017",format="%m-%d-%Y"),0.95*MaxQ,bquote(.(AreaStr)^2),cex=2.5)
# AreaStr = paste0("Mean NSE = ", round(NSEavg, 2))
# text(as.Date("01-01-2017",format="%m-%d-%Y"),0.85*MaxQ,AreaStr,cex=2.5)

# Edit title!
#title(main=paste0(GaugeNames[which(GaugeIDs==GaugeOfInterest)]," Daily Hydrograph"), cex.main=3)
#title(main=paste0(GaugeNames[which(GaugeIDs==GaugeOfInterest)]," Daily Hydrograph\n2012 December Peak Flow"), cex.main=2.5)

#title(main=paste0(GaugeNames[which(GaugeIDs==GaugeOfInterest)],": All ",length(VisualizeParameters)," Tested Models"), cex.main=3)
#title(main=paste0(GaugeNames[which(GaugeIDs==GaugeOfInterest)],": ",length(VisualizeParameters)," Pareto-Optimal Models"), cex.main=3)
#title(main=paste0(GaugeNames[which(GaugeIDs==GaugeOfInterest)],": ",length(VisualizeParameters)," Selected Models"), cex.main=3)
#title(main=paste0(GaugeNames[which(GaugeIDs==GaugeOfInterest)],": 1 \"Best\" Model"), cex.main=3)
title(main=paste0(GaugeNames[which(GaugeIDs==GaugeOfInterest)],": Winter Peak Flows"), cex.main=3)

# text(as.Date("2-26-2017",format="%m-%d-%Y"),30,"Peak SWE RMSE < 0.21 m", adj=0, cex=1)
# text(as.Date("2-26-2017",format="%m-%d-%Y"),27,"*NSE > 0.80", adj=0, cex=1)
# text(as.Date("2-26-2017",format="%m-%d-%Y"),24,"*Low-Base Log-NSE > 0.60", adj=0, cex=1)
# text(as.Date("2-26-2017",format="%m-%d-%Y"),21,"High-Base Log-NSE > 0.86", adj=0, cex=1)
# text(as.Date("2-26-2017",format="%m-%d-%Y"),18,expression("Water Yield RMSE < 0.36 km"^3*"/yr"), adj=0, cex=1)
# text(as.Date("2-26-2017",format="%m-%d-%Y"),15,expression("*Peak Daily Flows RMSE < 77 m"^3*"/s"), adj=0, cex=1)

text(as.Date("2-26-2017",format="%m-%d-%Y"),30,"Peak SWE RMSE < 0.23 m", adj=0, cex=1)
text(as.Date("2-26-2017",format="%m-%d-%Y"),27,"*NSE > 0.84", adj=0, cex=1)
text(as.Date("2-26-2017",format="%m-%d-%Y"),24,"Low-Base Log-NSE > 0.44", adj=0, cex=1)
text(as.Date("2-26-2017",format="%m-%d-%Y"),21,"High-Base Log-NSE > 0.88", adj=0, cex=1)
text(as.Date("2-26-2017",format="%m-%d-%Y"),18,expression("Water Yield RMSE < 0.36 km"^3*"/yr"), adj=0, cex=1)
text(as.Date("2-26-2017",format="%m-%d-%Y"),15,expression("Peak Daily Flows RMSE < 74 m"^3*"/s"), adj=0, cex=1)

################################################################################
# Plotting all hydrographs, just for kicks (or uncertainty quantification idk)
################################################################################

library(zoo)
library(RColorBrewer)

GaugeOfInterest = 11413000
BasinOfInterest = 2

PlotStart = as.Date("10-01-2016",format="%m-%d-%Y")
PlotEnd = as.Date("10-01-2017",format="%m-%d-%Y")

GaugeIDs = c(10343500, 10336676, 10336660, 10336645, 103366092, 10336610, 10336780, 10336730, 11413000, 11413300)
GaugeNames = c("Sagehen Creek", "Ward Creek", "Blackwood Creek", "General Creek", "Upper Truckee #1", "Upper Truckee #2", "Trout Creek", "Glenbrook Creek", "North Yuba River", "Slate Creek (Reconstructed)")
CatchmentAreas = c(10.5, 9.7, 11.2, 7.44, 39.3, 54.9, 36.7, 4.11, 250, 49.4) # Areas in mi^2 from USGS

# Read dataframe of streamflow data generated by ProcessStreamflowForDHSVM.Rhttp://127.0.0.1:17109/graphics/7eb237bc-1492-4d37-b5ab-48989765d50b.png
ReferenceStreamflow = read.csv(paste0(ReferenceDir,"USGS_StreamflowData.csv"))[,-1]
head(ReferenceStreamflow)

# Convert m^3/s to mm/d
ReferenceVals = ReferenceStreamflow[,paste0("Gauge_",GaugeOfInterest)]
ReferenceVals = 1000 * ReferenceVals * (60*60*24) / (CatchmentAreas[which(GaugeIDs==GaugeOfInterest)] * 2.58999 * 1000^2)

# Create initial plot
MinQ = 0.1
MaxQ = 200
par(mar=c(4,8,4,6),mgp=c(5,2,0),xaxs="i",yaxs="i")
plot(as.Date(ReferenceStreamflow$Date), ReferenceVals, log="y", type="l", col="black", lwd=1, ylim=c(MinQ,MaxQ), xlim=c(PlotStart,PlotEnd),
     xlab=NA,ylab="Streamflow, mm/d", cex.lab=3, xaxt="n", yaxt="n")
AxesMonths = seq(PlotStart, PlotEnd, by="months")
axis.Date(1, at=AxesMonths, format="%b-%Y", cex.axis=2, tck=-0.03, lwd=2) # start of water year
axis(2, at=c(0.1,1,10,100,200), lab=sprintf("%g",c(0.1,1,10,100,200)), cex.axis=2, lwd=2, tck=-0.03)
LogTicks = c(seq(0.1,0.9,0.1),seq(1,9,1),seq(10,100,10),200)
axis(2, at=LogTicks, labels=rep("",length(LogTicks)), lwd=2, tck=-0.015)
#axis(2, at=seq(0,MaxQ,3), lab=sprintf("%g",seq(0,MaxQ,3)), cex.axis=2, lwd=2)

# All non-gray named colors from R graphical devices
CrazyColors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

# brewer.pal.info[brewer.pal.info$category == 'qual',]
ColIdx = 1
ColorPal = brewer.pal(12,"Set3")
ColorPal = c("dodgerblue","hotpink","olivedrab","darkgoldenrod2","darkorchid2","cyan","sienna2","slateblue2","lightseagreen","red","green3","blue","yellow")

Cond1 = (results.grid.data$Total_NSE > 0.8)
Cond2 = (results.grid.data$PeakFlow_RMSE < Inf)
Cond3 = (results.grid.data$WaterYield_RMSE < Inf)
Cond4 = (results.grid.data$LowBase_LogNSE > 0.0)
Cond5 = (results.grid.data$HighBase_LogNSE > 0.0)

#VisualizeParameters = design.grid.data$ParamSet
#VisualizeParameters = parIndx
#VisualizeParameters = which(results.grid.data$ParamSet %in% parIndx & Cond1 & Cond2 & Cond3 & Cond4 & Cond5)
VisualizeParameters = c(276)

# Loop over each parameter set and calculate the objective function value
for (i in VisualizeParameters){
  
  # Check whether results are available for this parameter set
  if (dir.exists(paste0(ResultsDir,"Params",i,"_",BasinNames[BasinOfInterest]))){
    # Read the streamflow data output by DHSVM (drop first line that is date-only)
    DHSVMheader = read.table(paste0(ResultsDir,"Params",i,"_",BasinNames[BasinOfInterest],"/Streamflow.Only"), nrows=1)
    DHSVMstreamflow = read.table(paste0(ResultsDir,"Params",i,"_",BasinNames[BasinOfInterest],"/Streamflow.Only"), skip=2)
    names(DHSVMstreamflow) = DHSVMheader
    DHSVMstreamflow$DATE = as.Date(DHSVMstreamflow$DATE, format="%m.%d.%Y") # Convert to day only
    
    # Reset the vector containing modeled values to be compared with reference data
    DHSVMvals = c()
    
    # Aggregate DHSVM output from m^3/timestep to m^3/s as a daily average for the current gauge
    for (day in 1:length(ReferenceStreamflow$Date)){
      Ptrs = which(DHSVMstreamflow$DATE == ReferenceStreamflow$Date[day])
      DailyQ = sum(DHSVMstreamflow[,paste0("Gauge_",GaugeOfInterest)][Ptrs])/(24*60*60) # m^3/timestep --> m^3/s
      
      # Append to hydrograph
      DHSVMvals = c(DHSVMvals, DailyQ)
      
    } # End of daily gauge aggregation loop
    
    # Convert m^3/s to mm/d
    DHSVMvals = 1000 * DHSVMvals * (60*60*24) / (CatchmentAreas[which(GaugeIDs==GaugeOfInterest)] * 2.58999 * 1000^2)
    
    # Plot
    if (length(VisualizeParameters) > 13){
      lines(as.Date(ReferenceStreamflow$Date), DHSVMvals, col=sample(CrazyColors), lwd=2)
    } else if (length(VisualizeParameters)==1){
      lines(as.Date(ReferenceStreamflow$Date), DHSVMvals, col=ColorPal[1], lwd=4)
    } else {
      lines(as.Date(ReferenceStreamflow$Date), DHSVMvals, col=ColorPal[ColIdx], lwd=6-4*ColIdx/length(VisualizeParameters))
      ColIdx = ColIdx + 1
    }
  }
  
  print(i)
}

box(lwd=2)

# Add desired components
lines(as.Date(ReferenceStreamflow$Date), ReferenceVals, col="black", lwd=4)

legend("topright",inset=0.05,legend=c("Measured"),col=c("black"),lty=c(1),lwd=8,cex=2.5)
#legend("topleft",inset=0.05,legend=c("Measured"),col=c("black"),lty=c(1),lwd=12,cex=3)

#text(as.Date("4-01-2017",format="%m-%d-%Y"),0.4,"Pareto-Optimal\n& Downstream NSE > 0.8\n& Headwaters NSE > 0.6\n& High-Baseflow Log NSE > 0.6\n& Low-Baseflow Log NSE > 0.6", cex=1.5)
#text(as.Date("4-01-2017",format="%m-%d-%Y"),0.4,"\"Knee Point\" in\nPareto Front Between\nHeadwaters / Downstream NSE\n& High / Low-Baseflow Log NSE", cex=1.5)

#text(as.Date("12-06-2016",format="%m-%d-%Y"),9.4,"Pareto-Optimal\n& Downstream NSE > 0.8\n& Headwaters NSE > 0.6\n& High-Baseflow Log NSE > 0.6\n& Low-Baseflow Log NSE > 0.6", cex=1.5)
#text(as.Date("12-06-2016",format="%m-%d-%Y"),9.4,"\"Knee Point\" in\nPareto Front Between\nHeadwaters / Downstream NSE\n& High / Low-Baseflow Log NSE", cex=1.5)

# Edit title!
#title(main=paste0(GaugeNames[which(GaugeIDs==GaugeOfInterest)],": All ",length(VisualizeParameters)," Tested Models"), cex.main=3)
#title(main=paste0(GaugeNames[which(GaugeIDs==GaugeOfInterest)],": ",length(VisualizeParameters)," Pareto-Optimal Models"), cex.main=3)
#title(main=paste0(GaugeNames[which(GaugeIDs==GaugeOfInterest)],": ",length(VisualizeParameters)," Models with NSE > 0.8"), cex.main=3)
#title(main=paste0(GaugeNames[which(GaugeIDs==GaugeOfInterest)],": 1 Highest-NSE Model"), cex.main=3)



#VisualizeParameters = design.grid.data$ParamSet
#VisualizeParameters = parIndx
VisualizeParameters = c(195, 239, 244, 270, 276, 194)
#VisualizeParameters = c(157)

# Plot histograms of the selected designs to illustrate convergence
par(mar=c(0,1,1,1),pty="m")
layout(mat=matrix(c(rep(1,4),2:9),ncol=4,byrow=TRUE), heights=c(0.1,0.45,0.45))
plot.new()
#text(0.5,0.5,paste0("All ",length(VisualizeParameters)," Tested Models"),cex=4,font=2)
#text(0.5,0.5,paste0(length(VisualizeParameters)," Pareto-Optimal Models"),cex=4,font=2)
text(0.5,0.5,paste0(length(VisualizeParameters)," Selected Models"),cex=4,font=2)
#text(0.5,0.5,paste0("1 \"Best\" Model"),cex=4,font=2)
par(mar=c(5,5,5,1))
BreakLocs = seq(0,1,0.1)
for (i in 1:nParams){
  hist(design.grid[VisualizeParameters,i],main=paste0(ParamLongNames[i]),xlim=c(0,1),breaks=BreakLocs,axes=FALSE,xlab="Parameter Scale",ylab="",cex.lab=2,cex.main=2.5)
  axis(side=1,at=BreakLocs,cex.axis=2)
  axis(side=2,cex.axis=2)
}
plot.new()
text(0.5,0.5,paste0("Fraction of \nTotal Number of\nTested Designs:\n",round(100*length(VisualizeParameters)/length(design.grid.data$ParamSet)),"%"),cex=2.5,font=2)

















