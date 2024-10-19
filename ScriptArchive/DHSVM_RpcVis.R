RpFactor = function(Rp,Rpc){
  RpFac = (1 + Rp/Rpc)/(200/5000 + Rp/Rpc)
  return(RpFac)
}

plot(2*seq(0,500,by=0.01),200*RpFactor(seq(0,500,by=0.01),30),type="l",lwd=3,col="deepskyblue2",ylim=c(0,600),main="Effect of Rpc on Canopy Resistance in DHSVM",xlab=expression("Incoming Solar (Shortwave) Radiation, W/"*m^2),ylab="Canopy Resistance, s/m",cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
lines(2*seq(0,500,by=0.01),200*RpFactor(seq(0,500,by=0.01),0.108),type="l",lwd=3,col="goldenrod2",lty=2)
legend("topright",inset=0.05,legend=c(expression("Rpc = 30 W/"*m^2),"Rpc = 0.108 [unitless]"),lty=c(1,2),lwd=c(3,3),col=c("deepskyblue2","goldenrod2"),cex=1.2)
text(500,100,"Assumptions for Typical Forest:\nMinimum Resistance = 200 s/m, Maximum Resistance = 5000 s/m\nVisible Radiation Flux = 0.5x Incoming Shortwave Radiation",cex=1.2)

################################################################################
# Hydrograph comparison

ParamLabels = c("Rpc = 0.108 [unitless]",expression("Rpc = 30, 100 W/"*m^2))

library(hydroGOF)

# Read auxillary data on gauge watersheds
dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/StreamGauges/"
setwd(dir)
GaugeWatershedData = read.csv("TCSI_GaugeParameters.csv")
print(GaugeWatershedData)

i = which(GaugeWatershedData$GaugeID == 11413000)

BasinArea = GaugeWatershedData$AreaSqMi[i] * 2.59E6 # convert to m^2

# Set up desired daily framework for plotting
StartDateTime = as.POSIXct("2010-10-01 00:00:00",tz="GMT")
EndDateTime = as.POSIXct("2017-09-30 00:00:00",tz="GMT")
ModelInterval = 3 # hours
DateTime = seq(StartDateTime, EndDateTime, by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))

# Read streamflow data
dir = paste0("C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/DHSVM/X.SensitivityAnalysis/StreamflowSensitivity/")
setwd(dir)
StreamflowMeas = read.csv("TCSI_DailyStreamflow.csv") 
head(StreamflowMeas)

StreamflowModBaseline = read.table("Params99/Streamflow.Only",header=T) # must delete first line with no values
head(StreamflowModBaseline)

StreamflowModHighVar = read.table(paste0("Params100/Streamflow.Only"),header=T) # must delete first line with no values
head(StreamflowModHighVar)

# Read out the date/time of each model timestep
RawYears = substr(StreamflowModBaseline$DATE,7,10)
RawMonths = substr(StreamflowModBaseline$DATE,1,2)
RawDays = substr(StreamflowModBaseline$DATE,4,5)
RawTime = substr(StreamflowModBaseline$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
ModelDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

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

# Effect of Rpc on water balance
print(paste0("Ratio of water yield using Rpc30/Rpc0.108 = ",100*round(sum(RunoffModHighVar[Ptrs])/sum(RunoffMod[Ptrs]),2),"%"))

# Check Nash-Sutcliffe Efficiency
nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
print(paste0(ParamLabels[1]," NSE = ",round(nse,3)))
nse = NSE(sim = RunoffModHighVar[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
print(paste0(ParamLabels[2]," NSE = ",round(nse,3)))

# Plot the data
PlotColors = c("black","goldenrod2","deepskyblue2")
LineWidth = 2

MovingAvgPer = 1 # days
PlottingPeriod = c(as.POSIXct("2014-10-01 00:00:00",tz="GMT"),as.POSIXct("2015-09-30 00:00:00",tz="GMT"))
YLimits = c(0.1,25)

plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),log="y",xlim=PlottingPeriod,xaxs="i",ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph (USGS 11413000)\nDaily Average, Water Year 2015"),xlab=NA,ylab="Streamflow, mm/d",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
AxesMonths = seq(PlottingPeriod[1], PlottingPeriod[2], by="months")
axis.POSIXct(1, at=AxesMonths, cex.axis=1.5) # start of water year
lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
legend("topright",inset=0.05,cex=1.2,legend=c("Measured",ParamLabels),col=c(PlotColors[1],PlotColors[2],PlotColors[3]),lty=c(1,1),lwd=rep(3,4))

MovingAvgPer = 30 # days
PlottingPeriod = c(as.POSIXct("2011-10-01 00:00:00",tz="GMT"),as.POSIXct("2017-10-01 00:00:00",tz="GMT"))
YLimits = c(0.15,25)

plot(DateTime,rollmean(RunoffMeas, MovingAvgPer, fill=NA),xlim=PlottingPeriod,xaxs="i",log="y",ylim=YLimits,col=PlotColors[1],lwd=LineWidth,type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph (USGS 11413000)\n30-Day Moving Average, Water Years 2012-2017"),xlab="Water Year",ylab="Streamflow, mm/d",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1, cex.axis=1.2) # start of water year
lines(DateTime,rollmean(RunoffMod, MovingAvgPer, fill=NA),col=PlotColors[2],lwd=LineWidth)
lines(DateTime,rollmean(RunoffModHighVar, MovingAvgPer, fill=NA),col=PlotColors[3],lwd=LineWidth)
legend("top",inset=0.05,cex=1.2,legend=c("Measured",ParamLabels),col=c(PlotColors[1],PlotColors[2],PlotColors[3]),lty=c(1,1),lwd=rep(3,4))

################################################################################

# Check water balance by year
for (yr in 2012:2017){
  
  StartDateTime = as.POSIXct(paste0(yr-1,"-10-01 00:00:00"),tz="GMT")
  EndDateTime = as.POSIXct(paste0(yr,"-09-30 00:00:00"),tz="GMT")
  ModelInterval = 3 # hours
  DateTime = seq(StartDateTime, EndDateTime, by="days")
  Year = as.integer(format(DateTime,"%Y"))
  Month = as.integer(format(DateTime,"%m"))
  Day = as.integer(format(DateTime,"%d"))
  
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
  
  Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime))))
  print(paste0("Water Year: ",yr," Ratio of water yield using Rpc30/Rpc0.108 = ",100*round(sum(RunoffModHighVar[Ptrs])/sum(RunoffMod[Ptrs]),2),"%"))
}




