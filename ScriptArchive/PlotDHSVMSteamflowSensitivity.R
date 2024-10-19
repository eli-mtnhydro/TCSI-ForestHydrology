# To plot output from DHSVM

library(zoo)
library(hydroGOF)

# Read auxillary data on gauge watersheds
dir = paste0("C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/DHSVM/X.SensitivityAnalysis/StreamflowSensitivity/")
setwd(dir)
GaugeWatershedData = read.csv("TCSI_GaugeParameters.csv")
print(GaugeWatershedData)

i = which(GaugeWatershedData$GaugeID == 11413000)

BasinArea = GaugeWatershedData$AreaSqMi[i] * 2.59E6 # convert to m^2

# Read streamflow data
StreamflowMeas = read.csv("TCSI_DailyStreamflow.csv") 
head(StreamflowMeas)

################################################################################
# Part 1: Long runs

StreamflowModBaseline = read.table("Params1/Streamflow.Only",header=T) # must delete first line with no values
head(StreamflowModBaseline)

# Set up desired daily framework for plotting
StartDateTime = as.POSIXct("2010-10-01 00:00:00",tz="GMT")
EndDateTime = as.POSIXct("2020-09-30 00:00:00",tz="GMT")
ModelInterval = 3 # hours
DateTime = seq(StartDateTime, EndDateTime, by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))

# Read out the date/time of each model timestep
RawYears = substr(StreamflowModBaseline$DATE,7,10)
RawMonths = substr(StreamflowModBaseline$DATE,1,2)
RawDays = substr(StreamflowModBaseline$DATE,4,5)
RawTime = substr(StreamflowModBaseline$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
ModelDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

ParamLabels = c("Snow Temp. = 0 °C","Snow Temp. = 2.8 °C","Snow Temp. = 5.6 °C")
ParamLabels = c(ParamLabels,"Fresh Albedo = 0.8","Fresh Albedo = 0.84","Fresh Albedo = 0.9")
ParamLabels = c(ParamLabels,"Porosity = x1.25","Porosity = x1","Porosity = x0.8")
ParamLabels = c(ParamLabels,"Mannings N = 0.03","Mannings N = 0.05","Mannings N = 0.10")
ParamLabels = c(ParamLabels,"Min. Stomatal Res. = 230 s/m","Min Stomatal Res. = 460 s/m","Min Stomatal Res. = 920 s/m")

ParamSet = 1
for (params in c(2,4,8,10,12)){
  
  StreamflowModLowVar = read.table(paste0("Params",params,"/Streamflow.Only"),header=T) # must delete first line with no values
  StreamflowModHighVar = read.table(paste0("Params",params+1,"/Streamflow.Only"),header=T) # must delete first line with no values
  
  RunoffMeas = 0*1:length(DateTime)
  RunoffMod = 0*1:length(DateTime)
  RunoffModLowVar = 0*1:length(DateTime)
  RunoffModHighVar = 0*1:length(DateTime)
  
  # Subset measured and modeled data for the current gauge
  GaugeDateTime = as.POSIXct(StreamflowMeas[[paste0("DATE_",GaugeWatershedData$GaugeID[i])]],tz="GMT","%m/%d/%Y")
  GaugeDateTime[is.na(GaugeDateTime)] = as.POSIXct("0000-01-01",tz="GMT") # Replace missing rows so nothing breaks later
  StreamflowMeasThisGauge = StreamflowMeas[[paste0("Qcfs_",GaugeWatershedData$GaugeID[i])]]
  StreamflowModBaselineThisGauge = StreamflowModBaseline[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModLowVarThisGauge = StreamflowModLowVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModHighVarThisGauge = StreamflowModHighVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  
  # Compute the average streamflow for each day
  for (j in 1:length(DateTime)){
    if (sum(as.integer(GaugeDateTime == DateTime[j])) > 0){
      # cfs to mm/d
      RunoffMeas[j] = 1000*24*60*60*(StreamflowMeasThisGauge[GaugeDateTime == DateTime[j]]/35.315)/BasinArea
    }
    else {
      RunoffMeas[j] = NA # No measured streamflow for this day
    }
    
    # m^3/timestep to mm/d
    RunoffMod[j] = 1000*24*60*60*(mean(StreamflowModBaselineThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModLowVar[j] = 1000*24*60*60*(mean(StreamflowModLowVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModHighVar[j] = 1000*24*60*60*(mean(StreamflowModHighVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea 

  }
  
  # Check aggregate water balance
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModLowVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModLowVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffMod[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModHighVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModHighVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
  
  # Check Nash-Sutcliffe Efficiency
  nse = NSE(sim = RunoffModLowVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," NSE = ",round(nse,2)))
  nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," NSE = ",round(nse,2)))
  nse = NSE(sim = RunoffModHighVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," NSE = ",round(nse,2)))
  
  # Plot the data
  PlotColors = c("black","deepskyblue2","goldenrod2","darkorchid1")
  LineWidth = 2
  
  MovingAvgPer = 1 # days
  PlottingPeriod = c(as.POSIXct("2016-01-01 00:00:00",tz="GMT"),as.POSIXct("2016-06-01 00:00:00",tz="GMT"))
  YLimits = c(0,50)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\nDaily Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 7 # days
  PlottingPeriod = c(as.POSIXct("2015-10-01 00:00:00",tz="GMT"),as.POSIXct("2016-10-01 00:00:00",tz="GMT"))
  YLimits = c(0,25)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\n7-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 30 # days
  PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
  YLimits = c(0.2,30)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,log="y",ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Years 2015-2017\n30-Day Moving Average"),xlab="Water Year",ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
  axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topleft",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  ParamSet = ParamSet + 1
}

################################################################################
# Part 2: Re-runs and shorter tests

StreamflowModBaseline = read.table("Params1_shorter/Streamflow.Only",header=T) # must delete first line with no values
head(StreamflowModBaseline)

# Set up desired daily framework for plotting
StartDateTime = as.POSIXct("2013-10-01 00:00:00",tz="GMT")
EndDateTime = as.POSIXct("2017-09-30 00:00:00",tz="GMT")
ModelInterval = 3 # hours
DateTime = seq(StartDateTime, EndDateTime, by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))

# Read out the date/time of each model timestep
RawYears = substr(StreamflowModBaseline$DATE,7,10)
RawMonths = substr(StreamflowModBaseline$DATE,1,2)
RawDays = substr(StreamflowModBaseline$DATE,4,5)
RawTime = substr(StreamflowModBaseline$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
ModelDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

ParamLabels = c("Hydr. Cond. = x10","Hydr. Cond. = x1","Hydr. Cond. = x0.1")
ParamLabels = c(ParamLabels,"Soil Depth = x1.5","Soil Depth = x1","Soil Depth = x0.8")
ParamLabels = c(ParamLabels,"Klat Exp. Decrease = 0.03","Klat Exp. Decrease = 0.3","Klat Exp. Decrease = 3")
ParamLabels = c(ParamLabels,"Max. Infiltration = x10","Max Infiltration = x1", "Max Infiltration = x0.1")
ParamLabels = c(ParamLabels,"Rain LAI Mult. = 0.0001","Rain LAI Mult. = 0.0005","Rain LAI Mult. = 0.001")
ParamLabels = c(ParamLabels,"Min Inter. Snow = 0.001","Min Inter. Snow = 0.005","Min Inter. Snow = 0.01")
ParamLabels = c(ParamLabels,"Snow Water cap. = 0.01","Snow Water cap. = 0.05","Snow Water cap. = 0.1")
ParamLabels = c(ParamLabels,"Aero. Attenuation = 0.5","Aero. Attenuation = 2.5","Aero. Attenuation = 5.0")
ParamLabels = c(ParamLabels,"Mass Release Drip Rat. = 0.1","Mass Release Drip Rat. = 0.4","Mass Release Drip Rat. = 0.9")

ParamSet = 1
for (params in c(6,16,18,22,36,40,42,44,46)){
  
  StreamflowModLowVar = read.table(paste0("Params",params,"/Streamflow.Only"),header=T) # must delete first line with no values
  StreamflowModHighVar = read.table(paste0("Params",params+1,"/Streamflow.Only"),header=T) # must delete first line with no values
  
  RunoffMeas = 0*1:length(DateTime)
  RunoffMod = 0*1:length(DateTime)
  RunoffModLowVar = 0*1:length(DateTime)
  RunoffModHighVar = 0*1:length(DateTime)
  
  # Subset measured and modeled data for the current gauge
  GaugeDateTime = as.POSIXct(StreamflowMeas[[paste0("DATE_",GaugeWatershedData$GaugeID[i])]],tz="GMT","%m/%d/%Y")
  GaugeDateTime[is.na(GaugeDateTime)] = as.POSIXct("0000-01-01",tz="GMT") # Replace missing rows so nothing breaks later
  StreamflowMeasThisGauge = StreamflowMeas[[paste0("Qcfs_",GaugeWatershedData$GaugeID[i])]]
  StreamflowModBaselineThisGauge = StreamflowModBaseline[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModLowVarThisGauge = StreamflowModLowVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModHighVarThisGauge = StreamflowModHighVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  
  # Compute the average streamflow for each day
  for (j in 1:length(DateTime)){
    if (sum(as.integer(GaugeDateTime == DateTime[j])) > 0){
      # cfs to mm/d
      RunoffMeas[j] = 1000*24*60*60*(StreamflowMeasThisGauge[GaugeDateTime == DateTime[j]]/35.315)/BasinArea
    }
    else {
      RunoffMeas[j] = NA # No measured streamflow for this day
    }
    
    # m^3/timestep to mm/d
    RunoffMod[j] = 1000*24*60*60*(mean(StreamflowModBaselineThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModLowVar[j] = 1000*24*60*60*(mean(StreamflowModLowVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModHighVar[j] = 1000*24*60*60*(mean(StreamflowModHighVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea 
    
  }
  
  # DHSVM default config values were not always in the middle of tested range
  if (params == 18 | params == 20 | params == 36 | params == 40 | params == 42){
    # Swap "low" and "normal" order since "normal" value was actually the lowest
    RunoffTemp = RunoffModLowVar
    RunoffModLowVar = RunoffMod
    RunoffMod = RunoffTemp
  }
  
  # Check aggregate water balance
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModLowVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModLowVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffMod[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModHighVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModHighVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
  
  # Check Nash-Sutcliffe Efficiency
  nse = NSE(sim = RunoffModLowVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," NSE = ",round(nse,2)))
  nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," NSE = ",round(nse,2)))
  nse = NSE(sim = RunoffModHighVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," NSE = ",round(nse,2)))
  
  # Plot the data
  PlotColors = c("black","deepskyblue2","goldenrod2","darkorchid1")
  LineWidth = 2
  
  MovingAvgPer = 1 # days
  PlottingPeriod = c(as.POSIXct("2016-01-01 00:00:00",tz="GMT"),as.POSIXct("2016-06-01 00:00:00",tz="GMT"))
  YLimits = c(0,50)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\nDaily Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 7 # days
  PlottingPeriod = c(as.POSIXct("2015-10-01 00:00:00",tz="GMT"),as.POSIXct("2016-10-01 00:00:00",tz="GMT"))
  YLimits = c(0,25)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\n7-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 30 # days
  PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
  YLimits = c(0.2,30)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,log="y",ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Years 2015-2017\n30-Day Moving Average"),xlab="Water Year",ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
  axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topleft",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  ParamSet = ParamSet + 1
}

################################################################################
# Part 3: Shorter tests with switched baseline exponential decrease in hydraulic conductivity

StreamflowModBaseline = read.table("Params1_2hydrcondexp/Streamflow.Only",header=T) # must delete first line with no values
head(StreamflowModBaseline)
#StreamflowModBaseline$X11413000 = NA

# Set up desired daily framework for plotting
StartDateTime = as.POSIXct("2013-10-01 00:00:00",tz="GMT")
EndDateTime = as.POSIXct("2017-09-30 00:00:00",tz="GMT")
ModelInterval = 3 # hours
DateTime = seq(StartDateTime, EndDateTime, by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))

# Read out the date/time of each model timestep
RawYears = substr(StreamflowModBaseline$DATE,7,10)
RawMonths = substr(StreamflowModBaseline$DATE,1,2)
RawDays = substr(StreamflowModBaseline$DATE,4,5)
RawTime = substr(StreamflowModBaseline$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
ModelDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

ParamLabels = c("Albedo Accu. L = 0.87","Albedo Accu. L = 0.93","Albedo Accu. L = 0.99")
ParamLabels = c(ParamLabels,"Albedo Melt L = 0.82","Albedo Melt L = 0.88","Albedo Melt L = 0.99")
ParamLabels = c(ParamLabels,"Moisture Threshold = 0.1","Moisture Threshold = 0.2","Moisture Threshold = 0.33")
ParamLabels = c(ParamLabels,"VPD for Restr. ET = 1000 Pa","VPD for Restr. ET = 2000 Pa","VPD for Restr. ET = 4000 Pa")
ParamLabels = c(ParamLabels,"Radiation Attenuation = 0.1","Radiation Attenuation = 0.2","Radiation Attenuation = 0.4")

ParamSet = 1
for (params in c(24,26,28,30,32)){
  
  StreamflowModLowVar = read.table(paste0("Params",params,"/Streamflow.Only"),header=T) # must delete first line with no values
  StreamflowModHighVar = read.table(paste0("Params",params+1,"/Streamflow.Only"),header=T) # must delete first line with no values
  
  RunoffMeas = 0*1:length(DateTime)
  RunoffMod = 0*1:length(DateTime)
  RunoffModLowVar = 0*1:length(DateTime)
  RunoffModHighVar = 0*1:length(DateTime)
  
  # Subset measured and modeled data for the current gauge
  GaugeDateTime = as.POSIXct(StreamflowMeas[[paste0("DATE_",GaugeWatershedData$GaugeID[i])]],tz="GMT","%m/%d/%Y")
  GaugeDateTime[is.na(GaugeDateTime)] = as.POSIXct("0000-01-01",tz="GMT") # Replace missing rows so nothing breaks later
  StreamflowMeasThisGauge = StreamflowMeas[[paste0("Qcfs_",GaugeWatershedData$GaugeID[i])]]
  StreamflowModBaselineThisGauge = StreamflowModBaseline[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModLowVarThisGauge = StreamflowModLowVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModHighVarThisGauge = StreamflowModHighVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  
  # Compute the average streamflow for each day
  for (j in 1:length(DateTime)){
    if (sum(as.integer(GaugeDateTime == DateTime[j])) > 0){
      # cfs to mm/d
      RunoffMeas[j] = 1000*24*60*60*(StreamflowMeasThisGauge[GaugeDateTime == DateTime[j]]/35.315)/BasinArea
    }
    else {
      RunoffMeas[j] = NA # No measured streamflow for this day
    }
    
    # m^3/timestep to mm/d
    RunoffMod[j] = 1000*24*60*60*(mean(StreamflowModBaselineThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModLowVar[j] = 1000*24*60*60*(mean(StreamflowModLowVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModHighVar[j] = 1000*24*60*60*(mean(StreamflowModHighVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea 
    
  }
  
  # DHSVM default config values were not always in the middle of tested range
  if (params == 34){
    # Swap "low" and "normal" order since "normal" value was actually the lowest
    RunoffTemp = RunoffModLowVar
    RunoffModLowVar = RunoffMod
    RunoffMod = RunoffTemp
  }
  if (params == 28 | params == 30){
    # Swap "low" and "normal" order since "normal" value was actually the highest
    RunoffTemp = RunoffModHighVar
    RunoffModHighVar = RunoffMod
    RunoffMod = RunoffTemp
  }
  
  # Check aggregate water balance
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModLowVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModLowVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffMod[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModHighVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModHighVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
  
  # Check Nash-Sutcliffe Efficiency
  nse = NSE(sim = RunoffModLowVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," NSE = ",round(nse,2)))
  nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," NSE = ",round(nse,2)))
  nse = NSE(sim = RunoffModHighVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," NSE = ",round(nse,2)))
  
  # Plot the data
  PlotColors = c("black","deepskyblue2","goldenrod2","darkorchid1")
  LineWidth = 2
  
  MovingAvgPer = 1 # days
  PlottingPeriod = c(as.POSIXct("2016-01-01 00:00:00",tz="GMT"),as.POSIXct("2016-06-01 00:00:00",tz="GMT"))
  YLimits = c(0,50)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\nDaily Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 7 # days
  PlottingPeriod = c(as.POSIXct("2015-10-01 00:00:00",tz="GMT"),as.POSIXct("2016-10-01 00:00:00",tz="GMT"))
  YLimits = c(0,25)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\n7-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 30 # days
  PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
  YLimits = c(0.2,30)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,log="y",ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Years 2015-2017\n30-Day Moving Average"),xlab="Water Year",ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
  axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topleft",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  ParamSet = ParamSet + 1
}

################################################################################
# Part 4: Shorter tests with fixed snow interception code

StreamflowModBaseline = read.table("Params1_3intercept/Streamflow.Only",header=T) # must delete first line with no values
head(StreamflowModBaseline)
#StreamflowModBaseline$X11413000 = NA

# Set up desired daily framework for plotting
StartDateTime = as.POSIXct("2013-10-01 00:00:00",tz="GMT")
EndDateTime = as.POSIXct("2017-09-30 00:00:00",tz="GMT")
ModelInterval = 3 # hours
DateTime = seq(StartDateTime, EndDateTime, by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))

# Read out the date/time of each model timestep
RawYears = substr(StreamflowModBaseline$DATE,7,10)
RawMonths = substr(StreamflowModBaseline$DATE,1,2)
RawDays = substr(StreamflowModBaseline$DATE,4,5)
RawTime = substr(StreamflowModBaseline$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
ModelDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

# Since param sets 14-15 and 34-35 used a baseline snow lai mult. of 0.001 instead of 0.002, they're only compared to each other

ParamLabels = c("Snow Inter. Eff. = 0.3","Snow Inter. Eff. = 0.9")
ParamLabels = c(ParamLabels,"Max Snow Int. Capac. = 0.01","Max Snow Int. Capac. = 0.06")
ParamLabels = c(ParamLabels,"Snow LAI Mult. = 0.0002","Snow LAI Mult. = 0.002","Snow LAI Mult. = 0.01")

ParamSet = 1
for (params in c(14,34,38)){

  StreamflowModLowVar = read.table(paste0("Params",params,"/Streamflow.Only"),header=T) # must delete first line with no values
  StreamflowModHighVar = read.table(paste0("Params",params+1,"/Streamflow.Only"),header=T) # must delete first line with no values

  RunoffMeas = 0*1:length(DateTime)
  RunoffMod = 0*1:length(DateTime)
  RunoffModLowVar = 0*1:length(DateTime)
  RunoffModHighVar = 0*1:length(DateTime)

  # Subset measured and modeled data for the current gauge
  GaugeDateTime = as.POSIXct(StreamflowMeas[[paste0("DATE_",GaugeWatershedData$GaugeID[i])]],tz="GMT","%m/%d/%Y")
  GaugeDateTime[is.na(GaugeDateTime)] = as.POSIXct("0000-01-01",tz="GMT") # Replace missing rows so nothing breaks later
  StreamflowMeasThisGauge = StreamflowMeas[[paste0("Qcfs_",GaugeWatershedData$GaugeID[i])]]
  StreamflowModBaselineThisGauge = StreamflowModBaseline[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModLowVarThisGauge = StreamflowModLowVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModHighVarThisGauge = StreamflowModHighVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]

  # Compute the average streamflow for each day
  for (j in 1:length(DateTime)){
    if (sum(as.integer(GaugeDateTime == DateTime[j])) > 0){
      # cfs to mm/d
      RunoffMeas[j] = 1000*24*60*60*(StreamflowMeasThisGauge[GaugeDateTime == DateTime[j]]/35.315)/BasinArea
    }
    else {
      RunoffMeas[j] = NA # No measured streamflow for this day
    }

    # m^3/timestep to mm/d
    RunoffMod[j] = 1000*24*60*60*(mean(StreamflowModBaselineThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModLowVar[j] = 1000*24*60*60*(mean(StreamflowModLowVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModHighVar[j] = 1000*24*60*60*(mean(StreamflowModHighVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea

  }

  if (params == 38){
    # Check aggregate water balance
    Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModLowVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
    SumMod = sum(RunoffModLowVar[Ptrs])
    SumMeas = sum(RunoffMeas[Ptrs])
    print(paste0(ParamLabels[5]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
    Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime) + 60*60*24*365)))
    SumMod = sum(RunoffMod[Ptrs])
    SumMeas = sum(RunoffMeas[Ptrs])
    print(paste0(ParamLabels[6]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
    Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModHighVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
    SumMod = sum(RunoffModHighVar[Ptrs])
    SumMeas = sum(RunoffMeas[Ptrs])
    print(paste0(ParamLabels[7]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))

    # Check Nash-Sutcliffe Efficiency
    nse = NSE(sim = RunoffModLowVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
    print(paste0(ParamLabels[5]," NSE = ",round(nse,2)))
    nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
    print(paste0(ParamLabels[6]," NSE = ",round(nse,2)))
    nse = NSE(sim = RunoffModHighVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
    print(paste0(ParamLabels[7]," NSE = ",round(nse,2)))
  } else {
    # Check aggregate water balance
    Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModLowVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
    SumMod = sum(RunoffModLowVar[Ptrs])
    SumMeas = sum(RunoffMeas[Ptrs])
    print(paste0(ParamLabels[(1+2*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
    Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModHighVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
    SumMod = sum(RunoffModHighVar[Ptrs])
    SumMeas = sum(RunoffMeas[Ptrs])
    print(paste0(ParamLabels[(2+2*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))

    # Check Nash-Sutcliffe Efficiency
    nse = NSE(sim = RunoffModLowVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
    print(paste0(ParamLabels[(1+2*(ParamSet-1))]," NSE = ",round(nse,2)))
    nse = NSE(sim = RunoffModHighVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
    print(paste0(ParamLabels[(2+2*(ParamSet-1))]," NSE = ",round(nse,2)))
  }

  # Plot the data
  PlotColors = c("black","deepskyblue2","goldenrod2","darkorchid1")
  LineWidth = 2

  MovingAvgPer = 1 # days
  PlottingPeriod = c(as.POSIXct("2016-01-01 00:00:00",tz="GMT"),as.POSIXct("2016-06-01 00:00:00",tz="GMT"))
  YLimits = c(0,50)

  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\nDaily Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  if (params == 38){
    lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  }
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  if (params == 38){
    legend("topright",inset=0.03,legend=c("Measured",ParamLabels[5:7]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  } else {
    legend("topright",inset=0.03,legend=c("Measured",ParamLabels[(1+2*(ParamSet-1)):(2+2*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[4]),lty=c(1,1),lwd=rep(2,4))
  }

  MovingAvgPer = 7 # days
  PlottingPeriod = c(as.POSIXct("2015-10-01 00:00:00",tz="GMT"),as.POSIXct("2016-10-01 00:00:00",tz="GMT"))
  YLimits = c(0,25)

  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\n7-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  if (params == 38){
    lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  }
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  if (params == 38){
    legend("topright",inset=0.03,legend=c("Measured",ParamLabels[5:7]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  } else {
    legend("topright",inset=0.03,legend=c("Measured",ParamLabels[(1+2*(ParamSet-1)):(2+2*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[4]),lty=c(1,1),lwd=rep(2,4))
  }

  MovingAvgPer = 30 # days
  PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
  YLimits = c(0.2,30)

  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,log="y",ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Years 2015-2017\n30-Day Moving Average"),xlab="Water Year",ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
  axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  if (params == 38){
    lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  }
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  if (params == 38){
    legend("topleft",inset=0.03,legend=c("Measured",ParamLabels[5:7]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  } else {
    legend("topleft",inset=0.03,legend=c("Measured",ParamLabels[(1+2*(ParamSet-1)):(2+2*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[4]),lty=c(1,1),lwd=rep(2,4))
  }

  ParamSet = ParamSet + 1
}

################################################################################
# Part 5: Shorter tests with fixed snow interception code and updated snow interception baseline

StreamflowModBaseline = read.table("Params1_4snow/Streamflow.Only",header=T) # must delete first line with no values
head(StreamflowModBaseline)
#StreamflowModBaseline$X11413000 = NA

# Set up desired daily framework for plotting
StartDateTime = as.POSIXct("2013-10-01 00:00:00",tz="GMT")
EndDateTime = as.POSIXct("2017-09-30 00:00:00",tz="GMT")
ModelInterval = 3 # hours
DateTime = seq(StartDateTime, EndDateTime, by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))

# Read out the date/time of each model timestep
RawYears = substr(StreamflowModBaseline$DATE,7,10)
RawMonths = substr(StreamflowModBaseline$DATE,1,2)
RawDays = substr(StreamflowModBaseline$DATE,4,5)
RawTime = substr(StreamflowModBaseline$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
ModelDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

ParamLabels = c("Max Snow Int. Capac. = 0.01","Max Snow Int. Capac. = 0.03","Max Snow Int. Capac. = 0.10")
ParamLabels = c(ParamLabels,"Snow LAI Mult. = 0.0002","Snow LAI Mult. = 0.002","Snow LAI Mult. = 0.02")

ParamSet = 1
for (params in c(48,50)){
  
  StreamflowModLowVar = read.table(paste0("Params",params,"/Streamflow.Only"),header=T) # must delete first line with no values
  StreamflowModHighVar = read.table(paste0("Params",params+1,"/Streamflow.Only"),header=T) # must delete first line with no values
  
  RunoffMeas = 0*1:length(DateTime)
  RunoffMod = 0*1:length(DateTime)
  RunoffModLowVar = 0*1:length(DateTime)
  RunoffModHighVar = 0*1:length(DateTime)
  
  # Subset measured and modeled data for the current gauge
  GaugeDateTime = as.POSIXct(StreamflowMeas[[paste0("DATE_",GaugeWatershedData$GaugeID[i])]],tz="GMT","%m/%d/%Y")
  GaugeDateTime[is.na(GaugeDateTime)] = as.POSIXct("0000-01-01",tz="GMT") # Replace missing rows so nothing breaks later
  StreamflowMeasThisGauge = StreamflowMeas[[paste0("Qcfs_",GaugeWatershedData$GaugeID[i])]]
  StreamflowModBaselineThisGauge = StreamflowModBaseline[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModLowVarThisGauge = StreamflowModLowVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModHighVarThisGauge = StreamflowModHighVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  
  # Compute the average streamflow for each day
  for (j in 1:length(DateTime)){
    if (sum(as.integer(GaugeDateTime == DateTime[j])) > 0){
      # cfs to mm/d
      RunoffMeas[j] = 1000*24*60*60*(StreamflowMeasThisGauge[GaugeDateTime == DateTime[j]]/35.315)/BasinArea
    }
    else {
      RunoffMeas[j] = NA # No measured streamflow for this day
    }
    
    # m^3/timestep to mm/d
    RunoffMod[j] = 1000*24*60*60*(mean(StreamflowModBaselineThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModLowVar[j] = 1000*24*60*60*(mean(StreamflowModLowVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModHighVar[j] = 1000*24*60*60*(mean(StreamflowModHighVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea 
    
  }
  
  # Check aggregate water balance
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModLowVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModLowVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,5),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffMod[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,5),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModHighVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModHighVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,5),"%"))
  
  # Check Nash-Sutcliffe Efficiency
  nse = NSE(sim = RunoffModLowVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," NSE = ",round(nse,5)))
  nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," NSE = ",round(nse,5)))
  nse = NSE(sim = RunoffModHighVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," NSE = ",round(nse,5)))
  
  # Plot the data
  PlotColors = c("black","deepskyblue2","goldenrod2","darkorchid1")
  LineWidth = 2
  
  MovingAvgPer = 1 # days
  PlottingPeriod = c(as.POSIXct("2016-01-01 00:00:00",tz="GMT"),as.POSIXct("2016-06-01 00:00:00",tz="GMT"))
  YLimits = c(0,50)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\nDaily Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 7 # days
  PlottingPeriod = c(as.POSIXct("2015-10-01 00:00:00",tz="GMT"),as.POSIXct("2016-10-01 00:00:00",tz="GMT"))
  YLimits = c(0,25)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\n7-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 30 # days
  PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
  YLimits = c(0.2,30)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,log="y",ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Years 2015-2017\n30-Day Moving Average"),xlab="Water Year",ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
  axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topleft",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  ParamSet = ParamSet + 1
}

################################################################################
# Part 6: Semi-unrelated test of soil depth sensitivity as a proof-of-concept for the new POLARIS data

# Set up desired daily framework for plotting
StartDateTime = as.POSIXct("2013-10-01 00:00:00",tz="GMT")
EndDateTime = as.POSIXct("2017-09-30 00:00:00",tz="GMT")
ModelInterval = 3 # hours
DateTime = seq(StartDateTime, EndDateTime, by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))

# Read out the date/time of each model timestep
StreamflowModBaseline = read.table("Params52/Streamflow.Only",header=T) # must delete first line with no values
RawYears = substr(StreamflowModBaseline$DATE,7,10)
RawMonths = substr(StreamflowModBaseline$DATE,1,2)
RawDays = substr(StreamflowModBaseline$DATE,4,5)
RawTime = substr(StreamflowModBaseline$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
ModelDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

ParamLabels = c("Soil Depth = x1.5","Soil Depth = x1","Soil Depth = x0.8")
ParamLabels = c(ParamLabels,"Soil Depth: Curve S0O0","Soil Depth: Curve S3O0","Soil Depth: Curve S6O0")
ParamLabels = c(ParamLabels,"Soil Depth: Curve S0O3","Soil Depth: Curve S3O3","Soil Depth: Curve S6O3")
ParamLabels = c(ParamLabels,"Soil Depth: Curve S0O6","Soil Depth: Curve S3O6","Soil Depth: Curve S6O6")

ParamSet = 1
# Note that these go by 3s since each set has its own middle "baseline"
for (params in c(52,58,59,60)){
  
  if (params == 52){ # Baseline, deeper, shallower
    StreamflowModBaseline = read.table(paste0("Params",params,"/Streamflow.Only"),header=T) # must delete first line with no values
    StreamflowModLowVar = read.table(paste0("Params",params+1,"/Streamflow.Only"),header=T) # must delete first line with no values
    StreamflowModHighVar = read.table(paste0("Params",params+2,"/Streamflow.Only"),header=T) # must delete first line with no values
  } else { # In slope-based order originally, see ParamCombos.txt
    StreamflowModBaseline = read.table(paste0("Params",params,"/Streamflow.Only"),header=T) # must delete first line with no values
    StreamflowModLowVar = read.table(paste0("Params",params-3,"/Streamflow.Only"),header=T) # must delete first line with no values
    StreamflowModHighVar = read.table(paste0("Params",params+3,"/Streamflow.Only"),header=T) # must delete first line with no values
  }
  
  RunoffMeas = 0*1:length(DateTime)
  RunoffMod = 0*1:length(DateTime)
  RunoffModLowVar = 0*1:length(DateTime)
  RunoffModHighVar = 0*1:length(DateTime)
  
  # Subset measured and modeled data for the current gauge
  GaugeDateTime = as.POSIXct(StreamflowMeas[[paste0("DATE_",GaugeWatershedData$GaugeID[i])]],tz="GMT","%m/%d/%Y")
  GaugeDateTime[is.na(GaugeDateTime)] = as.POSIXct("0000-01-01",tz="GMT") # Replace missing rows so nothing breaks later
  StreamflowMeasThisGauge = StreamflowMeas[[paste0("Qcfs_",GaugeWatershedData$GaugeID[i])]]
  StreamflowModBaselineThisGauge = StreamflowModBaseline[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModLowVarThisGauge = StreamflowModLowVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModHighVarThisGauge = StreamflowModHighVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  
  # Compute the average streamflow for each day
  for (j in 1:length(DateTime)){
    if (sum(as.integer(GaugeDateTime == DateTime[j])) > 0){
      # cfs to mm/d
      RunoffMeas[j] = 1000*24*60*60*(StreamflowMeasThisGauge[GaugeDateTime == DateTime[j]]/35.315)/BasinArea
    }
    else {
      RunoffMeas[j] = NA # No measured streamflow for this day
    }
    
    # m^3/timestep to mm/d
    RunoffMod[j] = 1000*24*60*60*(mean(StreamflowModBaselineThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModLowVar[j] = 1000*24*60*60*(mean(StreamflowModLowVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModHighVar[j] = 1000*24*60*60*(mean(StreamflowModHighVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea 
    
  }
  
  # Check aggregate water balance
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModLowVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModLowVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,5),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffMod[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,5),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModHighVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModHighVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,5),"%"))
  
  # Check Nash-Sutcliffe Efficiency
  nse = NSE(sim = RunoffModLowVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," NSE = ",round(nse,5)))
  nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," NSE = ",round(nse,5)))
  nse = NSE(sim = RunoffModHighVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," NSE = ",round(nse,5)))
  
  # Plot the data
  PlotColors = c("black","deepskyblue2","goldenrod2","darkorchid1")
  LineWidth = 2
  
  MovingAvgPer = 1 # days
  PlottingPeriod = c(as.POSIXct("2016-01-01 00:00:00",tz="GMT"),as.POSIXct("2016-06-01 00:00:00",tz="GMT"))
  YLimits = c(0,50)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2017\nDaily Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 7 # days
  PlottingPeriod = c(as.POSIXct("2015-10-01 00:00:00",tz="GMT"),as.POSIXct("2016-10-01 00:00:00",tz="GMT"))
  YLimits = c(0,25)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\n7-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))

  # Extra plot just for a powerpoint
  # plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col="black",lwd=3,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph\nWater Year 2016: 7-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
  # AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  # axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  # lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col="darkorchid1",lwd=3)
  # legend("topright",inset=0.03,legend=c("Measured","Modeled by DHSVM"),col=c("black","darkorchid1"),lty=c(1,1),lwd=rep(5,2),seg.len=3,cex=1.3)

  MovingAvgPer = 30 # days
  PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
  YLimits = c(0.2,30)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,log="y",ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Years 2015-2017\n30-Day Moving Average"),xlab="Water Year",ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
  axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topleft",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  ParamSet = ParamSet + 1
}

################################################################################
# Part 7: Soil depth sensitivity tests for Truckee subbasins

# Set up desired daily framework for plotting
StartDateTime = as.POSIXct("2013-10-01 00:00:00",tz="GMT")
EndDateTime = as.POSIXct("2017-09-30 00:00:00",tz="GMT")
ModelInterval = 3 # hours
DateTime = seq(StartDateTime, EndDateTime, by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))

# Read out the date/time of each model timestep
StreamflowModBaseline = read.table("Truckee/Params55/Streamflow.Only",header=T) # must delete first line with no values
RawYears = substr(StreamflowModBaseline$DATE,7,10)
RawMonths = substr(StreamflowModBaseline$DATE,1,2)
RawDays = substr(StreamflowModBaseline$DATE,4,5)
RawTime = substr(StreamflowModBaseline$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
ModelDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

ParamLabels = c("Soil Depth: Curve S0O0","Soil Depth: Curve S3O0","Soil Depth: Curve S6O0")
ParamLabels = c(ParamLabels,"Soil Depth: Curve S0O3","Soil Depth: Curve S3O3","Soil Depth: Curve S6O3")
ParamLabels = c(ParamLabels,"Soil Depth: Curve S0O6","Soil Depth: Curve S3O6","Soil Depth: Curve S6O6")

# Choose which basin
# 10343500 --> Sagehen
# 10336676 --> Ward
# 10336660 --> Blackwood
# 10336645 --> General
# 10336610 --> Upper Truckee #2 (larger)
i = which(GaugeWatershedData$GaugeID == 10343500)
BasinArea = GaugeWatershedData$AreaSqMi[i] * 2.59E6 # convert to m^2

ParamSet = 1
# Note that these go by 3s since each set has its own middle "baseline"
for (params in c(58,59,60)){
  # In slope-based order originally, see ParamCombos.txt
  StreamflowModBaseline = read.table(paste0("Truckee/Params",params,"/Streamflow.Only"),header=T) # must delete first line with no values
  StreamflowModLowVar = read.table(paste0("Truckee/Params",params-3,"/Streamflow.Only"),header=T) # must delete first line with no values
  StreamflowModHighVar = read.table(paste0("Truckee/Params",params+3,"/Streamflow.Only"),header=T) # must delete first line with no values
  
  RunoffMeas = 0*1:length(DateTime)
  RunoffMod = 0*1:length(DateTime)
  RunoffModLowVar = 0*1:length(DateTime)
  RunoffModHighVar = 0*1:length(DateTime)
  
  # Subset measured and modeled data for the current gauge
  GaugeDateTime = as.POSIXct(StreamflowMeas[[paste0("DATE_",GaugeWatershedData$GaugeID[i])]],tz="GMT","%m/%d/%Y")
  GaugeDateTime[is.na(GaugeDateTime)] = as.POSIXct("0000-01-01",tz="GMT") # Replace missing rows so nothing breaks later
  StreamflowMeasThisGauge = StreamflowMeas[[paste0("Qcfs_",GaugeWatershedData$GaugeID[i])]]
  StreamflowModBaselineThisGauge = StreamflowModBaseline[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModLowVarThisGauge = StreamflowModLowVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModHighVarThisGauge = StreamflowModHighVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  
  # Compute the average streamflow for each day
  for (j in 1:length(DateTime)){
    if (sum(as.integer(GaugeDateTime == DateTime[j])) > 0){
      # cfs to mm/d
      RunoffMeas[j] = 1000*24*60*60*(StreamflowMeasThisGauge[GaugeDateTime == DateTime[j]]/35.315)/BasinArea
    }
    else {
      RunoffMeas[j] = NA # No measured streamflow for this day
    }
    
    # m^3/timestep to mm/d
    RunoffMod[j] = 1000*24*60*60*(mean(StreamflowModBaselineThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModLowVar[j] = 1000*24*60*60*(mean(StreamflowModLowVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModHighVar[j] = 1000*24*60*60*(mean(StreamflowModHighVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea 
    
  }
  
  print(GaugeWatershedData$Name[i])
  
  # Check aggregate water balance
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModLowVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModLowVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,5),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffMod[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,5),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModHighVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModHighVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,5),"%"))
  
  # Check Nash-Sutcliffe Efficiency
  nse = NSE(sim = RunoffModLowVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," NSE = ",round(nse,5)))
  nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," NSE = ",round(nse,5)))
  nse = NSE(sim = RunoffModHighVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," NSE = ",round(nse,5)))
  
  # Plot the data
  PlotColors = c("black","deepskyblue2","goldenrod2","darkorchid1")
  LineWidth = 2
  
  MovingAvgPer = 1 # days
  PlottingPeriod = c(as.POSIXct("2016-01-01 00:00:00",tz="GMT"),as.POSIXct("2016-06-01 00:00:00",tz="GMT"))
  Ptrs = which(DateTime > PlottingPeriod[1] & DateTime < PlottingPeriod[2] & is.finite(RunoffMeas))
  YLimits = c(0,1.2*max(RunoffMeas[Ptrs]))
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2017\nDaily Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topleft",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 7 # days
  PlottingPeriod = c(as.POSIXct("2015-10-01 00:00:00",tz="GMT"),as.POSIXct("2016-10-01 00:00:00",tz="GMT"))
  Ptrs = which(DateTime > PlottingPeriod[1] & DateTime < PlottingPeriod[2] & is.finite(RunoffMeas))
  YLimits = c(0,max(RunoffMeas[Ptrs]))
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\n7-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topleft",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 30 # days
  PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
  Ptrs = which(DateTime > PlottingPeriod[1] & DateTime < PlottingPeriod[2] & is.finite(RunoffMeas))
  YLimits = c(0.2,0.8*max(RunoffMeas[Ptrs]))
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,log="y",ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Years 2015-2017\n30-Day Moving Average"),xlab="Water Year",ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
  axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topleft",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  ParamSet = ParamSet + 1
}

################################################################################
# Part 8: Further soil depth (curvature/offset) sensitivity tests with various fixes

# Placeholder only to set up other stuff
StreamflowModBaseline = read.table("Params64/Streamflow.Only",header=T) # must delete first line with no values
# Set up desired daily framework for plotting
StartDateTime = as.POSIXct("2013-10-01 00:00:00",tz="GMT")
EndDateTime = as.POSIXct("2017-09-30 00:00:00",tz="GMT")
ModelInterval = 3 # hours
DateTime = seq(StartDateTime, EndDateTime, by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))

# Read out the date/time of each model timestep
RawYears = substr(StreamflowModBaseline$DATE,7,10)
RawMonths = substr(StreamflowModBaseline$DATE,1,2)
RawDays = substr(StreamflowModBaseline$DATE,4,5)
RawTime = substr(StreamflowModBaseline$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
ModelDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

ParamLabels = c("Soil Depth: Curve S0O1","Soil Depth: Curve S38O1","Soil Depth: Curve S76O1")
ParamLabels = c(ParamLabels,"Soil Depth: Curve S0O3","Soil Depth: Curve S38O3","Soil Depth: Curve S76O3")
ParamLabels = c(ParamLabels,"Soil Depth: Curve S0O5","Soil Depth: Curve S38O5","Soil Depth: Curve S76O5")

ParamSet = 1
# Note that these go by 3s since each set has its own middle "baseline"
for (params in c(64,67,70)){
  
  StreamflowModLowVar = read.table(paste0("Params",params,"/Streamflow.Only"),header=T) # must delete first line with no values
  StreamflowModBaseline = read.table(paste0("Params",params+1,"/Streamflow.Only"),header=T) # must delete first line with no values
  StreamflowModHighVar = read.table(paste0("Params",params+2,"/Streamflow.Only"),header=T) # must delete first line with no values
  
  RunoffMeas = 0*1:length(DateTime)
  RunoffMod = 0*1:length(DateTime)
  RunoffModLowVar = 0*1:length(DateTime)
  RunoffModHighVar = 0*1:length(DateTime)
  
  # Subset measured and modeled data for the current gauge
  GaugeDateTime = as.POSIXct(StreamflowMeas[[paste0("DATE_",GaugeWatershedData$GaugeID[i])]],tz="GMT","%m/%d/%Y")
  GaugeDateTime[is.na(GaugeDateTime)] = as.POSIXct("0000-01-01",tz="GMT") # Replace missing rows so nothing breaks later
  StreamflowMeasThisGauge = StreamflowMeas[[paste0("Qcfs_",GaugeWatershedData$GaugeID[i])]]
  StreamflowModBaselineThisGauge = StreamflowModBaseline[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModLowVarThisGauge = StreamflowModLowVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModHighVarThisGauge = StreamflowModHighVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  
  # Compute the average streamflow for each day
  for (j in 1:length(DateTime)){
    if (sum(as.integer(GaugeDateTime == DateTime[j])) > 0){
      # cfs to mm/d
      RunoffMeas[j] = 1000*24*60*60*(StreamflowMeasThisGauge[GaugeDateTime == DateTime[j]]/35.315)/BasinArea
    }
    else {
      RunoffMeas[j] = NA # No measured streamflow for this day
    }
    
    # m^3/timestep to mm/d
    RunoffMod[j] = 1000*24*60*60*(mean(StreamflowModBaselineThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModLowVar[j] = 1000*24*60*60*(mean(StreamflowModLowVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModHighVar[j] = 1000*24*60*60*(mean(StreamflowModHighVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea 
    
  }
  
  # Check aggregate water balance
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModLowVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModLowVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,5),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffMod[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,5),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModHighVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModHighVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,5),"%"))
  
  # Check Nash-Sutcliffe Efficiency
  nse = NSE(sim = RunoffModLowVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," NSE = ",round(nse,5)))
  nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," NSE = ",round(nse,5)))
  nse = NSE(sim = RunoffModHighVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," NSE = ",round(nse,5)))
  
  # Plot the data
  PlotColors = c("black","deepskyblue2","goldenrod2","darkorchid1")
  LineWidth = 2
  
  MovingAvgPer = 1 # days
  PlottingPeriod = c(as.POSIXct("2016-01-01 00:00:00",tz="GMT"),as.POSIXct("2016-06-01 00:00:00",tz="GMT"))
  YLimits = c(0,50)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\nDaily Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 7 # days
  PlottingPeriod = c(as.POSIXct("2015-10-01 00:00:00",tz="GMT"),as.POSIXct("2016-10-01 00:00:00",tz="GMT"))
  YLimits = c(0,25)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\n7-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 30 # days
  PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
  YLimits = c(0.2,30)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,log="y",ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Years 2015-2017\n30-Day Moving Average"),xlab="Water Year",ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
  axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topleft",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  ParamSet = ParamSet + 1
}

################################################################################
# Part 9: Normal-style sensitivity test for vertical conductivity and infiltration rate

StreamflowModBaseline = read.table("Params68/Streamflow.Only",header=T) # must delete first line with no values
head(StreamflowModBaseline)
#StreamflowModBaseline$X11413000 = NA

# Set up desired daily framework for plotting
StartDateTime = as.POSIXct("2013-10-01 00:00:00",tz="GMT")
EndDateTime = as.POSIXct("2017-09-30 00:00:00",tz="GMT")
ModelInterval = 3 # hours
DateTime = seq(StartDateTime, EndDateTime, by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))

# Read out the date/time of each model timestep
RawYears = substr(StreamflowModBaseline$DATE,7,10)
RawMonths = substr(StreamflowModBaseline$DATE,1,2)
RawDays = substr(StreamflowModBaseline$DATE,4,5)
RawTime = substr(StreamflowModBaseline$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
ModelDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

ParamLabels = c("Anisotropy = Variable","Anisotropy = 0.1","Anisotropy = 1")

ParamSet = 1
for (params in c(73)){
  
  StreamflowModLowVar = read.table(paste0("Params",params+1,"/Streamflow.Only"),header=T) # must delete first line with no values
  StreamflowModHighVar = read.table(paste0("Params",params,"/Streamflow.Only"),header=T) # must delete first line with no values
  
  RunoffMeas = 0*1:length(DateTime)
  RunoffMod = 0*1:length(DateTime)
  RunoffModLowVar = 0*1:length(DateTime)
  RunoffModHighVar = 0*1:length(DateTime)
  
  # Subset measured and modeled data for the current gauge
  GaugeDateTime = as.POSIXct(StreamflowMeas[[paste0("DATE_",GaugeWatershedData$GaugeID[i])]],tz="GMT","%m/%d/%Y")
  GaugeDateTime[is.na(GaugeDateTime)] = as.POSIXct("0000-01-01",tz="GMT") # Replace missing rows so nothing breaks later
  StreamflowMeasThisGauge = StreamflowMeas[[paste0("Qcfs_",GaugeWatershedData$GaugeID[i])]]
  StreamflowModBaselineThisGauge = StreamflowModBaseline[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModLowVarThisGauge = StreamflowModLowVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModHighVarThisGauge = StreamflowModHighVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  
  # Compute the average streamflow for each day
  for (j in 1:length(DateTime)){
    if (sum(as.integer(GaugeDateTime == DateTime[j])) > 0){
      # cfs to mm/d
      RunoffMeas[j] = 1000*24*60*60*(StreamflowMeasThisGauge[GaugeDateTime == DateTime[j]]/35.315)/BasinArea
    }
    else {
      RunoffMeas[j] = NA # No measured streamflow for this day
    }
    
    # m^3/timestep to mm/d
    RunoffMod[j] = 1000*24*60*60*(mean(StreamflowModBaselineThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModLowVar[j] = 1000*24*60*60*(mean(StreamflowModLowVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModHighVar[j] = 1000*24*60*60*(mean(StreamflowModHighVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea 
    
  }
  
  # Check aggregate water balance
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModLowVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModLowVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,5),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffMod[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,5),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModHighVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModHighVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,5),"%"))
  
  # Check Nash-Sutcliffe Efficiency
  nse = NSE(sim = RunoffModLowVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," NSE = ",round(nse,5)))
  nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," NSE = ",round(nse,5)))
  nse = NSE(sim = RunoffModHighVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," NSE = ",round(nse,5)))
  
  # Plot the data
  PlotColors = c("black","deepskyblue2","goldenrod2","darkorchid1")
  LineWidth = 2
  
  MovingAvgPer = 1 # days
  PlottingPeriod = c(as.POSIXct("2016-01-01 00:00:00",tz="GMT"),as.POSIXct("2016-06-01 00:00:00",tz="GMT"))
  YLimits = c(0,50)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\nDaily Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 7 # days
  PlottingPeriod = c(as.POSIXct("2015-10-01 00:00:00",tz="GMT"),as.POSIXct("2016-10-01 00:00:00",tz="GMT"))
  YLimits = c(0,25)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\n7-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 30 # days
  PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
  YLimits = c(0.2,30)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,log="y",ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Years 2015-2017\n30-Day Moving Average"),xlab="Water Year",ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
  axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topleft",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  ParamSet = ParamSet + 1
}

################################################################################
# Part 10a: Regular-style tests of calibration parameters with new baseline

StreamflowModBaseline = read.table("Params75/Streamflow.Only",header=T) # must delete first line with no values
head(StreamflowModBaseline)

# Set up desired daily framework for plotting
StartDateTime = as.POSIXct("2013-10-01 00:00:00",tz="GMT")
EndDateTime = as.POSIXct("2017-09-30 00:00:00",tz="GMT")
ModelInterval = 3 # hours
DateTime = seq(StartDateTime, EndDateTime, by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))

# Read out the date/time of each model timestep
RawYears = substr(StreamflowModBaseline$DATE,7,10)
RawMonths = substr(StreamflowModBaseline$DATE,1,2)
RawDays = substr(StreamflowModBaseline$DATE,4,5)
RawTime = substr(StreamflowModBaseline$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
ModelDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

ParamLabels = c("Rain LAI Mult. = 0.001","Rain LAI Mult. = 0.01","Rain LAI Mult. = 0.1")
ParamLabels = c(ParamLabels,"Snow LAI Mult. = 0.001","Snow LAI Mult. = 0.01","Snow LAI Mult. = 0.1")
ParamLabels = c(ParamLabels,"Albedo Melt L = 0.70","Albedo Melt L = 0.88","Albedo Melt L = 0.95")
ParamLabels = c(ParamLabels,"Hydr. Cond. = 0.1fs","Hydr. Cond. = 0.5fs","Hydr. Cond. = 0.9fs")
ParamLabels = c(ParamLabels,"Snow Temp. = 2 °C","Snow Temp. = 4 °C","Snow Temp. = 6 °C")
ParamLabels = c(ParamLabels,"Klat Exp. Decrease = 0.1fs","Klat Exp. Decrease = 0.5fs","Klat Exp. Decrease = 0.9fs")
ParamLabels = c(ParamLabels,"Porosity = 0.1fs","Porosity = 0.5fs","Porosity = 0.9fs")
ParamLabels = c(ParamLabels,"Soil Depth: Curve S38O1","Soil Depth: Curve S38O2","Soil Depth: Curve S38O3")
ParamLabels = c(ParamLabels,"Min. Stomatal Res. = 300 s/m","Min Stomatal Res. = 500 s/m","Min Stomatal Res. = 700 s/m")

ParamSet = 1
for (params in c(76,78,80,82,84,86,88,90,92)){
  
  StreamflowModLowVar = read.table(paste0("Params",params,"/Streamflow.Only"),header=T) # must delete first line with no values
  StreamflowModHighVar = read.table(paste0("Params",params+1,"/Streamflow.Only"),header=T) # must delete first line with no values
  
  RunoffMeas = 0*1:length(DateTime)
  RunoffMod = 0*1:length(DateTime)
  RunoffModLowVar = 0*1:length(DateTime)
  RunoffModHighVar = 0*1:length(DateTime)
  
  # Subset measured and modeled data for the current gauge
  GaugeDateTime = as.POSIXct(StreamflowMeas[[paste0("DATE_",GaugeWatershedData$GaugeID[i])]],tz="GMT","%m/%d/%Y")
  GaugeDateTime[is.na(GaugeDateTime)] = as.POSIXct("0000-01-01",tz="GMT") # Replace missing rows so nothing breaks later
  StreamflowMeasThisGauge = StreamflowMeas[[paste0("Qcfs_",GaugeWatershedData$GaugeID[i])]]
  StreamflowModBaselineThisGauge = StreamflowModBaseline[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModLowVarThisGauge = StreamflowModLowVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModHighVarThisGauge = StreamflowModHighVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  
  # Compute the average streamflow for each day
  for (j in 1:length(DateTime)){
    if (sum(as.integer(GaugeDateTime == DateTime[j])) > 0){
      # cfs to mm/d
      RunoffMeas[j] = 1000*24*60*60*(StreamflowMeasThisGauge[GaugeDateTime == DateTime[j]]/35.315)/BasinArea
    }
    else {
      RunoffMeas[j] = NA # No measured streamflow for this day
    }
    
    # m^3/timestep to mm/d
    RunoffMod[j] = 1000*24*60*60*(mean(StreamflowModBaselineThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModLowVar[j] = 1000*24*60*60*(mean(StreamflowModLowVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModHighVar[j] = 1000*24*60*60*(mean(StreamflowModHighVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea 
    
  }
  
  # Check aggregate water balance
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModLowVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModLowVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffMod[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModHighVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModHighVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
  
  # Check Nash-Sutcliffe Efficiency
  nse = NSE(sim = RunoffModLowVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," NSE = ",round(nse,2)))
  nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," NSE = ",round(nse,2)))
  nse = NSE(sim = RunoffModHighVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," NSE = ",round(nse,2)))
  
  # Plot the data
  PlotColors = c("black","deepskyblue2","goldenrod2","darkorchid1")
  LineWidth = 2
  
  MovingAvgPer = 1 # days
  PlottingPeriod = c(as.POSIXct("2016-01-01 00:00:00",tz="GMT"),as.POSIXct("2016-06-01 00:00:00",tz="GMT"))
  YLimits = c(0,50)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\nDaily Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 7 # days
  PlottingPeriod = c(as.POSIXct("2015-10-01 00:00:00",tz="GMT"),as.POSIXct("2016-10-01 00:00:00",tz="GMT"))
  YLimits = c(0,25)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\n7-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 30 # days
  PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
  YLimits = c(0.2,30)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,log="y",ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Years 2015-2017\n30-Day Moving Average"),xlab="Water Year",ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
  axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topleft",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  ParamSet = ParamSet + 1
}

################################################################################
# Part 10b: One more test of the rain LAI interception parameter
# Note: only comparing PARAMS 76 (quasi-baseline) and PARAMS 94

StreamflowModBaseline = read.table("Params76/Streamflow.Only",header=T) # must delete first line with no values
head(StreamflowModBaseline)

# Set up desired daily framework for plotting
StartDateTime = as.POSIXct("2013-10-01 00:00:00",tz="GMT")
EndDateTime = as.POSIXct("2017-09-30 00:00:00",tz="GMT")
ModelInterval = 3 # hours
DateTime = seq(StartDateTime, EndDateTime, by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))

# Read out the date/time of each model timestep
RawYears = substr(StreamflowModBaseline$DATE,7,10)
RawMonths = substr(StreamflowModBaseline$DATE,1,2)
RawDays = substr(StreamflowModBaseline$DATE,4,5)
RawTime = substr(StreamflowModBaseline$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
ModelDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

ParamLabels = c("Rain LAI Mult. = 0.001","Rain LAI Mult. = 0.005")

ParamSet = 1
for (params in c(94)){
  
  StreamflowModHighVar = read.table(paste0("Params",params,"/Streamflow.Only"),header=T) # must delete first line with no values
  
  RunoffMeas = 0*1:length(DateTime)
  RunoffMod = 0*1:length(DateTime)
  RunoffModHighVar = 0*1:length(DateTime)
  
  # Subset measured and modeled data for the current gauge
  GaugeDateTime = as.POSIXct(StreamflowMeas[[paste0("DATE_",GaugeWatershedData$GaugeID[i])]],tz="GMT","%m/%d/%Y")
  GaugeDateTime[is.na(GaugeDateTime)] = as.POSIXct("0000-01-01",tz="GMT") # Replace missing rows so nothing breaks later
  StreamflowMeasThisGauge = StreamflowMeas[[paste0("Qcfs_",GaugeWatershedData$GaugeID[i])]]
  StreamflowModBaselineThisGauge = StreamflowModBaseline[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModHighVarThisGauge = StreamflowModHighVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  
  # Compute the average streamflow for each day
  for (j in 1:length(DateTime)){
    if (sum(as.integer(GaugeDateTime == DateTime[j])) > 0){
      # cfs to mm/d
      RunoffMeas[j] = 1000*24*60*60*(StreamflowMeasThisGauge[GaugeDateTime == DateTime[j]]/35.315)/BasinArea
    }
    else {
      RunoffMeas[j] = NA # No measured streamflow for this day
    }
    
    # m^3/timestep to mm/d
    RunoffMod[j] = 1000*24*60*60*(mean(StreamflowModBaselineThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModHighVar[j] = 1000*24*60*60*(mean(StreamflowModHighVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea 
    
  }
  
  # Check aggregate water balance
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffMod[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[1]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModHighVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModHighVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[2]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
  
  # Check Nash-Sutcliffe Efficiency
  nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[1]," NSE = ",round(nse,2)))
  nse = NSE(sim = RunoffModHighVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[2]," NSE = ",round(nse,2)))
  
  # Plot the data
  PlotColors = c("black","deepskyblue2","goldenrod2","darkorchid1")
  LineWidth = 2
  
  MovingAvgPer = 1 # days
  PlottingPeriod = c(as.POSIXct("2016-01-01 00:00:00",tz="GMT"),as.POSIXct("2016-06-01 00:00:00",tz="GMT"))
  YLimits = c(0,50)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\nDaily Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels),col=c(PlotColors[1],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 7 # days
  PlottingPeriod = c(as.POSIXct("2015-10-01 00:00:00",tz="GMT"),as.POSIXct("2016-10-01 00:00:00",tz="GMT"))
  YLimits = c(0,25)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\n7-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels),col=c(PlotColors[1],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 30 # days
  PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
  YLimits = c(0.2,30)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,log="y",ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Years 2015-2017\n30-Day Moving Average"),xlab="Water Year",ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
  axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topleft",inset=0.03,legend=c("Measured",ParamLabels),col=c(PlotColors[1],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  ParamSet = ParamSet + 1
}

################################################################################
# Part 10c: Testing negligible understory theory
# Note: only comparing PARAMS 95 (new baseline) and PARAMS 96

StreamflowModBaseline = read.table("Params95/Streamflow.Only",header=T) # must delete first line with no values
head(StreamflowModBaseline)

# Set up desired daily framework for plotting
StartDateTime = as.POSIXct("2013-10-01 00:00:00",tz="GMT")
EndDateTime = as.POSIXct("2017-09-30 00:00:00",tz="GMT")
ModelInterval = 3 # hours
DateTime = seq(StartDateTime, EndDateTime, by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))

# Read out the date/time of each model timestep
RawYears = substr(StreamflowModBaseline$DATE,7,10)
RawMonths = substr(StreamflowModBaseline$DATE,1,2)
RawDays = substr(StreamflowModBaseline$DATE,4,5)
RawTime = substr(StreamflowModBaseline$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
ModelDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

ParamLabels = c("No Understory","Negligible Understory")

ParamSet = 1
for (params in c(96)){
  
  StreamflowModHighVar = read.table(paste0("Params",params,"/Streamflow.Only"),header=T) # must delete first line with no values
  
  RunoffMeas = 0*1:length(DateTime)
  RunoffMod = 0*1:length(DateTime)
  RunoffModHighVar = 0*1:length(DateTime)
  
  # Subset measured and modeled data for the current gauge
  GaugeDateTime = as.POSIXct(StreamflowMeas[[paste0("DATE_",GaugeWatershedData$GaugeID[i])]],tz="GMT","%m/%d/%Y")
  GaugeDateTime[is.na(GaugeDateTime)] = as.POSIXct("0000-01-01",tz="GMT") # Replace missing rows so nothing breaks later
  StreamflowMeasThisGauge = StreamflowMeas[[paste0("Qcfs_",GaugeWatershedData$GaugeID[i])]]
  StreamflowModBaselineThisGauge = StreamflowModBaseline[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModHighVarThisGauge = StreamflowModHighVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  
  # Compute the average streamflow for each day
  for (j in 1:length(DateTime)){
    if (sum(as.integer(GaugeDateTime == DateTime[j])) > 0){
      # cfs to mm/d
      RunoffMeas[j] = 1000*24*60*60*(StreamflowMeasThisGauge[GaugeDateTime == DateTime[j]]/35.315)/BasinArea
    }
    else {
      RunoffMeas[j] = NA # No measured streamflow for this day
    }
    
    # m^3/timestep to mm/d
    RunoffMod[j] = 1000*24*60*60*(mean(StreamflowModBaselineThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModHighVar[j] = 1000*24*60*60*(mean(StreamflowModHighVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea 
    
  }
  
  # Check aggregate water balance
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffMod[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[1]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModHighVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModHighVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[2]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
  
  # Check Nash-Sutcliffe Efficiency
  nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[1]," NSE = ",round(nse,2)))
  nse = NSE(sim = RunoffModHighVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[2]," NSE = ",round(nse,2)))
  
  # Plot the data
  PlotColors = c("black","deepskyblue2","goldenrod2","darkorchid1")
  LineWidth = 2
  
  MovingAvgPer = 1 # days
  PlottingPeriod = c(as.POSIXct("2016-01-01 00:00:00",tz="GMT"),as.POSIXct("2016-06-01 00:00:00",tz="GMT"))
  YLimits = c(0,50)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\nDaily Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels),col=c(PlotColors[1],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 7 # days
  PlottingPeriod = c(as.POSIXct("2015-10-01 00:00:00",tz="GMT"),as.POSIXct("2016-10-01 00:00:00",tz="GMT"))
  YLimits = c(0,25)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\n7-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels),col=c(PlotColors[1],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 30 # days
  PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
  YLimits = c(0.2,30)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,log="y",ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Years 2015-2017\n30-Day Moving Average"),xlab="Water Year",ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
  axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topleft",inset=0.03,legend=c("Measured",ParamLabels),col=c(PlotColors[1],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  ParamSet = ParamSet + 1
}

#############################################
# Extra plots using some best-guess parameters
StreamflowModBaseline = read.table("Params94/Streamflow.Only",header=T) # must delete first line with no values
StreamflowModBaselineThisGauge = StreamflowModBaseline[[paste0("X",GaugeWatershedData$GaugeID[i])]]

# Compute the average streamflow for each day
for (j in 1:length(DateTime)){
  # m^3/timestep to mm/d
  RunoffMod[j] = 1000*24*60*60*(mean(StreamflowModBaselineThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
}

# Check aggregate water balance
Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime) + 60*60*24*365)))
SumMod = sum(RunoffMod[Ptrs])
SumMeas = sum(RunoffMeas[Ptrs])
print(paste0("Params 94 Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))

# Check Nash-Sutcliffe Efficiency
nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
print(paste0("Params 94 NSE = ",round(nse,2)))

MovingAvgPer = 7
YLimits = c(0,45)
PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
plot(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col="deepskyblue2",lwd=2,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph\nWater Years 2015-2017: 7-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
lines(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),col="black",lwd=2)
legend("topleft",inset=0.03,legend=c("Measured","Modeled by DHSVM"),col=c("black","deepskyblue2"),lty=c(1,1),lwd=rep(5,2),seg.len=3,cex=1.3)

MovingAvgPer = 30
YLimits = c(0.1,20)
PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
plot(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col="deepskyblue2",lwd=2,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph (Log Scale)\nWater Years 2015-2017: 30-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
lines(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),col="black",lwd=2)
legend("topleft",inset=0.03,legend=c("Measured","Modeled by DHSVM"),col=c("black","deepskyblue2"),lty=c(1,1),lwd=rep(5,2),seg.len=3,cex=1.3)

MovingAvgPer = 1
YLimits = c(0,100)
PlottingPeriod = c(as.POSIXct("2016-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
plot(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col="deepskyblue2",lwd=2,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph\nWater Year 2017: 1-Day Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
lines(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),col="black",lwd=2)
legend("topright",inset=0.03,legend=c("Measured","Modeled by DHSVM"),col=c("black","deepskyblue2"),lty=c(1,1),lwd=rep(5,2),seg.len=3,cex=1.3)

MovingAvgPer = 1
YLimits = c(0,35)
PlottingPeriod = c(as.POSIXct("2015-10-01 00:00:00",tz="GMT"),as.POSIXct("2016-10-01 00:00:00",tz="GMT"))
plot(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col="deepskyblue2",lwd=2,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph\nWater Year 2016: 1-Day Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
lines(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),col="black",lwd=2)
legend("topright",inset=0.03,legend=c("Measured","Modeled by DHSVM"),col=c("black","deepskyblue2"),lty=c(1,1),lwd=rep(5,2),seg.len=3,cex=1.3)

################################################################################
# Part 10d: Testing Rpc
# Note: only comparing PARAMS 99 PARAMS 100

StreamflowModBaseline = read.table("Params99/Streamflow.Only",header=T) # must delete first line with no values
head(StreamflowModBaseline)

# Set up desired daily framework for plotting
StartDateTime = as.POSIXct("2013-10-01 00:00:00",tz="GMT")
EndDateTime = as.POSIXct("2017-09-30 00:00:00",tz="GMT")
ModelInterval = 3 # hours
DateTime = seq(StartDateTime, EndDateTime, by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))

# Read out the date/time of each model timestep
RawYears = substr(StreamflowModBaseline$DATE,7,10)
RawMonths = substr(StreamflowModBaseline$DATE,1,2)
RawDays = substr(StreamflowModBaseline$DATE,4,5)
RawTime = substr(StreamflowModBaseline$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
ModelDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

ParamLabels = c("Rpc = 0.108","Rpc = 30 W/m^2")

ParamSet = 1
for (params in c(100)){
  
  StreamflowModHighVar = read.table(paste0("Params",params,"/Streamflow.Only"),header=T) # must delete first line with no values
  
  RunoffMeas = 0*1:length(DateTime)
  RunoffMod = 0*1:length(DateTime)
  RunoffModHighVar = 0*1:length(DateTime)
  
  # Subset measured and modeled data for the current gauge
  GaugeDateTime = as.POSIXct(StreamflowMeas[[paste0("DATE_",GaugeWatershedData$GaugeID[i])]],tz="GMT","%m/%d/%Y")
  GaugeDateTime[is.na(GaugeDateTime)] = as.POSIXct("0000-01-01",tz="GMT") # Replace missing rows so nothing breaks later
  StreamflowMeasThisGauge = StreamflowMeas[[paste0("Qcfs_",GaugeWatershedData$GaugeID[i])]]
  StreamflowModBaselineThisGauge = StreamflowModBaseline[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModHighVarThisGauge = StreamflowModHighVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  
  # Compute the average streamflow for each day
  for (j in 1:length(DateTime)){
    if (sum(as.integer(GaugeDateTime == DateTime[j])) > 0){
      # cfs to mm/d
      RunoffMeas[j] = 1000*24*60*60*(StreamflowMeasThisGauge[GaugeDateTime == DateTime[j]]/35.315)/BasinArea
    }
    else {
      RunoffMeas[j] = NA # No measured streamflow for this day
    }
    
    # m^3/timestep to mm/d
    RunoffMod[j] = 1000*24*60*60*(mean(StreamflowModBaselineThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModHighVar[j] = 1000*24*60*60*(mean(StreamflowModHighVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea 
    
  }
  
  # Check aggregate water balance
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffMod[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[1]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModHighVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModHighVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[2]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
  
  # Check Nash-Sutcliffe Efficiency
  nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[1]," NSE = ",round(nse,2)))
  nse = NSE(sim = RunoffModHighVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[2]," NSE = ",round(nse,2)))
  
  # Plot the data
  PlotColors = c("black","deepskyblue2","goldenrod2","darkorchid1")
  LineWidth = 2
  
  MovingAvgPer = 1 # days
  PlottingPeriod = c(as.POSIXct("2016-01-01 00:00:00",tz="GMT"),as.POSIXct("2016-06-01 00:00:00",tz="GMT"))
  YLimits = c(0,50)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\nDaily Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels),col=c(PlotColors[1],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 7 # days
  PlottingPeriod = c(as.POSIXct("2015-10-01 00:00:00",tz="GMT"),as.POSIXct("2016-10-01 00:00:00",tz="GMT"))
  YLimits = c(0,25)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\n7-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topright",inset=0.03,legend=c("Measured",ParamLabels),col=c(PlotColors[1],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 30 # days
  PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
  YLimits = c(0.2,30)
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,log="y",ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Years 2015-2017\n30-Day Moving Average"),xlab="Water Year",ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
  axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topleft",inset=0.03,legend=c("Measured",ParamLabels),col=c(PlotColors[1],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  ParamSet = ParamSet + 1
}

################################################################################
# Part 10e: Testing Vf_Adjust

StreamflowModBaseline = read.table("Vf1.0_Streamflow.Only",header=T) # must delete first line with no values
head(StreamflowModBaseline)

# Set up desired daily framework for plotting
StartDateTime = as.POSIXct("2013-10-01 00:00:00",tz="GMT")
EndDateTime = as.POSIXct("2017-09-30 00:00:00",tz="GMT")
ModelInterval = 3 # hours
DateTime = seq(StartDateTime, EndDateTime, by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))

# Read out the date/time of each model timestep
RawYears = substr(StreamflowModBaseline$DATE,7,10)
RawMonths = substr(StreamflowModBaseline$DATE,1,2)
RawDays = substr(StreamflowModBaseline$DATE,4,5)
RawTime = substr(StreamflowModBaseline$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
ModelDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

ParamLabels = c("Vf Adj = 1.0","Vf Adj = 1.7")

StreamflowModHighVar = read.table(paste0("Vf1.7_Streamflow.Only"),header=T) # must delete first line with no values

RunoffMeas = 0*1:length(DateTime)
RunoffMod = 0*1:length(DateTime)
RunoffModHighVar = 0*1:length(DateTime)

# Subset measured and modeled data for the current gauge
GaugeDateTime = as.POSIXct(StreamflowMeas[[paste0("DATE_",GaugeWatershedData$GaugeID[i])]],tz="GMT","%m/%d/%Y")
GaugeDateTime[is.na(GaugeDateTime)] = as.POSIXct("0000-01-01",tz="GMT") # Replace missing rows so nothing breaks later
StreamflowMeasThisGauge = StreamflowMeas[[paste0("Qcfs_",GaugeWatershedData$GaugeID[i])]]
StreamflowModBaselineThisGauge = StreamflowModBaseline[[paste0("Gauge_",GaugeWatershedData$GaugeID[i])]]
StreamflowModHighVarThisGauge = StreamflowModHighVar[[paste0("Gauge_",GaugeWatershedData$GaugeID[i])]]

# Compute the average streamflow for each day
for (j in 1:length(DateTime)){
  if (sum(as.integer(GaugeDateTime == DateTime[j])) > 0){
    # cfs to mm/d
    RunoffMeas[j] = 1000*24*60*60*(StreamflowMeasThisGauge[GaugeDateTime == DateTime[j]]/35.315)/BasinArea
  }
  else {
    RunoffMeas[j] = NA # No measured streamflow for this day
  }
  
  # m^3/timestep to mm/d
  RunoffMod[j] = 1000*24*60*60*(mean(StreamflowModBaselineThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
  RunoffModHighVar[j] = 1000*24*60*60*(mean(StreamflowModHighVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea 
  
}

# Check aggregate water balance
Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime) + 60*60*24*365)))
SumMod = sum(RunoffMod[Ptrs])
SumMeas = sum(RunoffMeas[Ptrs])
print(paste0(ParamLabels[1]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModHighVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
SumMod = sum(RunoffModHighVar[Ptrs])
SumMeas = sum(RunoffMeas[Ptrs])
print(paste0(ParamLabels[2]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))

# Check Nash-Sutcliffe Efficiency
nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
print(paste0(ParamLabels[1]," NSE = ",round(nse,2)))
nse = NSE(sim = RunoffModHighVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
print(paste0(ParamLabels[2]," NSE = ",round(nse,2)))

# Plot the data
PlotColors = c("black","deepskyblue2","goldenrod2","darkorchid1")
LineWidth = 2

MovingAvgPer = 1 # days
PlottingPeriod = c(as.POSIXct("2016-01-01 00:00:00",tz="GMT"),as.POSIXct("2016-06-01 00:00:00",tz="GMT"))
YLimits = c(0,50)

plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\nDaily Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
legend("topright",inset=0.03,legend=c("Measured",ParamLabels),col=c(PlotColors[1],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))

MovingAvgPer = 7 # days
PlottingPeriod = c(as.POSIXct("2015-10-01 00:00:00",tz="GMT"),as.POSIXct("2016-10-01 00:00:00",tz="GMT"))
YLimits = c(0,25)

plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\n7-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
legend("topright",inset=0.03,legend=c("Measured",ParamLabels),col=c(PlotColors[1],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))

MovingAvgPer = 30 # days
PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
YLimits = c(0.2,30)

plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,log="y",ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Years 2015-2017\n30-Day Moving Average"),xlab="Water Year",ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
legend("topleft",inset=0.03,legend=c("Measured",ParamLabels),col=c(PlotColors[1],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))

################################################################################
# Part 10f: Testing snow interception code

StreamflowModBaseline = read.table("Params276_Yuba_Original/Streamflow.Only",header=T) # must delete first line with no values
head(StreamflowModBaseline)

# Set up desired daily framework for plotting
StartDateTime = as.POSIXct("2013-10-01 00:00:00",tz="GMT")
EndDateTime = as.POSIXct("2017-09-30 00:00:00",tz="GMT")
ModelInterval = 3 # hours
DateTime = seq(StartDateTime, EndDateTime, by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))

# Read out the date/time of each model timestep
RawYears = substr(StreamflowModBaseline$DATE,7,10)
RawMonths = substr(StreamflowModBaseline$DATE,1,2)
RawDays = substr(StreamflowModBaseline$DATE,4,5)
RawTime = substr(StreamflowModBaseline$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
ModelDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

ParamLabels = c("With FC in Snow Int.", "Without FC in Snow Int.")

StreamflowModHighVar = read.table(paste0("Params276_Yuba_NoFCinSnowInt/Streamflow.Only"),header=T) # must delete first line with no values

RunoffMeas = 0*1:length(DateTime)
RunoffMod = 0*1:length(DateTime)
RunoffModHighVar = 0*1:length(DateTime)

# Subset measured and modeled data for the current gauge
GaugeDateTime = as.POSIXct(StreamflowMeas[[paste0("DATE_",GaugeWatershedData$GaugeID[i])]],tz="GMT","%m/%d/%Y")
GaugeDateTime[is.na(GaugeDateTime)] = as.POSIXct("0000-01-01",tz="GMT") # Replace missing rows so nothing breaks later
StreamflowMeasThisGauge = StreamflowMeas[[paste0("Qcfs_",GaugeWatershedData$GaugeID[i])]]
StreamflowModBaselineThisGauge = StreamflowModBaseline[[paste0("Gauge_",GaugeWatershedData$GaugeID[i])]]
StreamflowModHighVarThisGauge = StreamflowModHighVar[[paste0("Gauge_",GaugeWatershedData$GaugeID[i])]]

# Compute the average streamflow for each day
for (j in 1:length(DateTime)){
  if (sum(as.integer(GaugeDateTime == DateTime[j])) > 0){
    # cfs to mm/d
    RunoffMeas[j] = 1000*24*60*60*(StreamflowMeasThisGauge[GaugeDateTime == DateTime[j]]/35.315)/BasinArea
  }
  else {
    RunoffMeas[j] = NA # No measured streamflow for this day
  }
  
  # m^3/timestep to mm/d
  RunoffMod[j] = 1000*24*60*60*(mean(StreamflowModBaselineThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
  RunoffModHighVar[j] = 1000*24*60*60*(mean(StreamflowModHighVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea 
  
}

# Check aggregate water balance
Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime) + 60*60*24*365)))
SumMod = sum(RunoffMod[Ptrs])
SumMeas = sum(RunoffMeas[Ptrs])
print(paste0(ParamLabels[1]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModHighVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
SumMod = sum(RunoffModHighVar[Ptrs])
SumMeas = sum(RunoffMeas[Ptrs])
print(paste0(ParamLabels[2]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))

# Check Nash-Sutcliffe Efficiency
nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
print(paste0(ParamLabels[1]," NSE = ",round(nse,6)))
nse = NSE(sim = RunoffModHighVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
print(paste0(ParamLabels[2]," NSE = ",round(nse,6)))

# Plot the data
PlotColors = c("black","deepskyblue2","goldenrod2","darkorchid1")
LineWidth = 2

MovingAvgPer = 1 # days
PlottingPeriod = c(as.POSIXct("2016-01-01 00:00:00",tz="GMT"),as.POSIXct("2016-06-01 00:00:00",tz="GMT"))
YLimits = c(0,50)

plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\nDaily Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
legend("topright",inset=0.03,legend=c("Measured",ParamLabels),col=c(PlotColors[1],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))

MovingAvgPer = 7 # days
PlottingPeriod = c(as.POSIXct("2015-10-01 00:00:00",tz="GMT"),as.POSIXct("2016-10-01 00:00:00",tz="GMT"))
YLimits = c(0,25)

plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\n7-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
legend("topright",inset=0.03,legend=c("Measured",ParamLabels),col=c(PlotColors[1],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))

MovingAvgPer = 30 # days
PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
YLimits = c(0.2,30)

plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,log="y",ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Years 2015-2017\n30-Day Moving Average"),xlab="Water Year",ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
legend("topleft",inset=0.03,legend=c("Measured",ParamLabels),col=c(PlotColors[1],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))

plot(DateTime, RunoffMod/RunoffModHighVar, ylab="Original / Modified")

################################################################################
# Part 11: Testing snow LAI multiplier in Truckee

# Set up desired daily framework for plotting
StartDateTime = as.POSIXct("2013-10-01 00:00:00",tz="GMT")
EndDateTime = as.POSIXct("2017-09-30 00:00:00",tz="GMT")
ModelInterval = 3 # hours
DateTime = seq(StartDateTime, EndDateTime, by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))

# Read out the date/time of each model timestep
StreamflowModBaseline = read.table("Truckee/Params75/Streamflow.Only",header=T) # must delete first line with no values
RawYears = substr(StreamflowModBaseline$DATE,7,10)
RawMonths = substr(StreamflowModBaseline$DATE,1,2)
RawDays = substr(StreamflowModBaseline$DATE,4,5)
RawTime = substr(StreamflowModBaseline$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
ModelDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

ParamLabels = c("Snow LAI Mult. = 0.001","Snow LAI Mult. = 0.01","Snow LAI Mult. = 0.1")

# Choose which basin
# 10343500 --> Sagehen
# 10336676 --> Ward
# 10336660 --> Blackwood
# 10336645 --> General
# 10336610 --> Upper Truckee #2 (larger)
i = which(GaugeWatershedData$GaugeID == 10336610)
BasinArea = GaugeWatershedData$AreaSqMi[i] * 2.59E6 # convert to m^2

ParamSet = 1
for (params in c(78)){
  StreamflowModLowVar = read.table(paste0("Truckee/Params",params,"/Streamflow.Only"),header=T) # must delete first line with no values
  StreamflowModHighVar = read.table(paste0("Truckee/Params",params+1,"/Streamflow.Only"),header=T) # must delete first line with no values
  
  RunoffMeas = 0*1:length(DateTime)
  RunoffMod = 0*1:length(DateTime)
  RunoffModLowVar = 0*1:length(DateTime)
  RunoffModHighVar = 0*1:length(DateTime)
  
  # Subset measured and modeled data for the current gauge
  GaugeDateTime = as.POSIXct(StreamflowMeas[[paste0("DATE_",GaugeWatershedData$GaugeID[i])]],tz="GMT","%m/%d/%Y")
  GaugeDateTime[is.na(GaugeDateTime)] = as.POSIXct("0000-01-01",tz="GMT") # Replace missing rows so nothing breaks later
  StreamflowMeasThisGauge = StreamflowMeas[[paste0("Qcfs_",GaugeWatershedData$GaugeID[i])]]
  StreamflowModBaselineThisGauge = StreamflowModBaseline[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModLowVarThisGauge = StreamflowModLowVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  StreamflowModHighVarThisGauge = StreamflowModHighVar[[paste0("X",GaugeWatershedData$GaugeID[i])]]
  
  # Compute the average streamflow for each day
  for (j in 1:length(DateTime)){
    if (sum(as.integer(GaugeDateTime == DateTime[j])) > 0){
      # cfs to mm/d
      RunoffMeas[j] = 1000*24*60*60*(StreamflowMeasThisGauge[GaugeDateTime == DateTime[j]]/35.315)/BasinArea
    }
    else {
      RunoffMeas[j] = NA # No measured streamflow for this day
    }
    
    # m^3/timestep to mm/d
    RunoffMod[j] = 1000*24*60*60*(mean(StreamflowModBaselineThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModLowVar[j] = 1000*24*60*60*(mean(StreamflowModLowVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
    RunoffModHighVar[j] = 1000*24*60*60*(mean(StreamflowModHighVarThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea 
    
  }
  
  print(GaugeWatershedData$Name[i])
  
  # Check aggregate water balance
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModLowVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModLowVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,5),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffMod[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,5),"%"))
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffModHighVar))*(DateTime > (min(DateTime) + 60*60*24*365)))
  SumMod = sum(RunoffModHighVar[Ptrs])
  SumMeas = sum(RunoffMeas[Ptrs])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," Overpredicting runoff by ",100*round(SumMod/SumMeas-1,5),"%"))
  
  # Check Nash-Sutcliffe Efficiency
  nse = NSE(sim = RunoffModLowVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(1+3*(ParamSet-1))]," NSE = ",round(nse,5)))
  nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(2+3*(ParamSet-1))]," NSE = ",round(nse,5)))
  nse = NSE(sim = RunoffModHighVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
  print(paste0(ParamLabels[(3+3*(ParamSet-1))]," NSE = ",round(nse,5)))
  
  # Plot the data
  PlotColors = c("black","deepskyblue2","goldenrod2","darkorchid1")
  LineWidth = 2
  
  MovingAvgPer = 1 # days
  PlottingPeriod = c(as.POSIXct("2016-01-01 00:00:00",tz="GMT"),as.POSIXct("2016-06-01 00:00:00",tz="GMT"))
  Ptrs = which(DateTime > PlottingPeriod[1] & DateTime < PlottingPeriod[2] & is.finite(RunoffMeas))
  YLimits = c(0,1.2*max(RunoffMeas[Ptrs]))
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2017\nDaily Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topleft",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 7 # days
  PlottingPeriod = c(as.POSIXct("2015-10-01 00:00:00",tz="GMT"),as.POSIXct("2016-10-01 00:00:00",tz="GMT"))
  Ptrs = which(DateTime > PlottingPeriod[1] & DateTime < PlottingPeriod[2] & is.finite(RunoffMeas))
  YLimits = c(0,max(RunoffMeas[Ptrs]))
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Year 2016\n7-Day Moving Average"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
  axis.POSIXct(1, at=AxesMonths, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topleft",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  MovingAvgPer = 30 # days
  PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
  Ptrs = which(DateTime > PlottingPeriod[1] & DateTime < PlottingPeriod[2] & is.finite(RunoffMeas))
  YLimits = c(0.2,0.8*max(RunoffMeas[Ptrs]))
  
  plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,log="y",ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Years 2015-2017\n30-Day Moving Average"),xlab="Water Year",ylab="Streamflow, mm/d",cex.axis=1.2,cex.lab=1.2)
  AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
  axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
  lines(DateTime,rollmean(RunoffModLowVar, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
  lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[4],lwd=LineWidth)
  legend("topleft",inset=0.03,legend=c("Measured",ParamLabels[(1+3*(ParamSet-1)):(3+3*(ParamSet-1))]),col=c(PlotColors[1],PlotColors[2],PlotColors[3],PlotColors[4]),lty=c(1,1),lwd=rep(3,4))
  
  ParamSet = ParamSet + 1
}
