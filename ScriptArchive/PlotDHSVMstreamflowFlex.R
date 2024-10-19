# To plot output from DHSVM

library(hydroGOF)

BasinNum = 4
StartDateTime = as.POSIXct("2013-10-01 00:00:00",tz="GMT")
EndDateTime = as.POSIXct("2020-09-30 00:00:00",tz="GMT")
ModelInterval = 3 # hours

BasinNames = c("Truckee","Yuba","Bear","American")

# Read streamflow data from DHSVM output
dir = paste0("C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/DHSVM/",BasinNum,"_",BasinNames[BasinNum],"/output/")
setwd(dir)
StreamflowMod = read.table("Streamflow.Only",header=T) # must delete first line with no values
head(StreamflowMod)

# Read observed streamflow data
dir2 = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/StreamflowData/"
setwd(dir2)
StreamflowMeas = read.csv("TCSI_DailyStreamflow.csv") 
head(StreamflowMeas)

# Read auxillary data on gauge watersheds
dir3 = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/StreamGauges/"
setwd(dir3)
GaugeWatershedData = read.csv("TCSI_GaugeParameters.csv")
print(GaugeWatershedData)

# Set up desired daily framework for plotting
DateTime = seq(StartDateTime, EndDateTime, by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))

# Read out the date/time of each model timestep
RawYears = substr(StreamflowMod$DATE,7,10)
RawMonths = substr(StreamflowMod$DATE,1,2)
RawDays = substr(StreamflowMod$DATE,4,5)
RawTime = substr(StreamflowMod$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
ModelDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

for (i in 1:length(GaugeWatershedData$GaugeID)){
  # Get the data into the right form for each gauge identified within the current DHSVM basin
  if (GaugeWatershedData$Basin[i] == BasinNames[BasinNum]){
    print(paste0("Plotting ",GaugeWatershedData$Name[i]))
    
    BasinArea = GaugeWatershedData$AreaSqMi[i] * 2.59E6 # convert to m^2
    RunoffMod = 0*1:length(DateTime)
    RunoffMeas = 0*1:length(DateTime)
    
    # Subset measured and modeled data for the current gauge
    GaugeDateTime = as.POSIXct(StreamflowMeas[[paste0("DATE_",GaugeWatershedData$GaugeID[i])]],tz="GMT","%m/%d/%Y")
    GaugeDateTime[is.na(GaugeDateTime)] = as.POSIXct("0000-01-01",tz="GMT") # Replace missing rows so nothing breaks later
    StreamflowMeasThisGauge = StreamflowMeas[[paste0("Qcfs_",GaugeWatershedData$GaugeID[i])]]
    StreamflowModThisGauge = StreamflowMod[[paste0("X",GaugeWatershedData$GaugeID[i])]]
    
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
      RunoffMod[j] = 1000*24*60*60*(mean(StreamflowModThisGauge[ModelDateOnly == DateTime[j]])/(ModelInterval*60*60))/BasinArea
      
      #print(j/length(DateTime))
    }
    
    # Check aggregate water balance
    Ptrs = as.logical((!is.na(RunoffMeas))*(!is.na(RunoffMod))*(DateTime > (min(DateTime) + 60*60*24*365)))
    SumMod = sum(RunoffMod[Ptrs])
    SumMeas = sum(RunoffMeas[Ptrs])
    print(paste0("Overpredicting runoff by ",100*round(SumMod/SumMeas-1,2),"%"))
    
    # Check Nash-Sutcliffe Efficiency
    nse = NSE(sim = RunoffMod[DateTime > min(DateTime) + 60*60*24*365], obs = RunoffMeas[DateTime > min(DateTime) + 60*60*24*365])
    print(paste0("NSE = ",round(nse,2)))
    
    # Plot the data
    maxQ = max(c(RunoffMeas[!is.na(RunoffMeas)],RunoffMod))
    plot(DateTime,RunoffMod,ylim=c(0,maxQ),col="red",type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Years 2014-2020"),xlab="Water Year",ylab="Streamflow, mm/d")
    AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
    axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1) # start of water year
    lines(DateTime,RunoffMeas,col="blue")
    legend(min(AxesYears),0.9*maxQ,legend=c("Modeled","Measured"),col=c("red","blue"),lty=c(1,1),lwd=c(3,3))
    text(max(AxesYears)-60*60*24*365,0.95*maxQ,paste0("NSE = ",round(nse,2)),cex=1.5)
    if (SumMod > SumMeas){
      text(max(AxesYears)-60*60*24*365,0.85*maxQ,paste0("Overpredicting +",100*round(SumMod/SumMeas-1,2),"%"),cex=1.5)
    } else {
      text(max(AxesYears)-60*60*24*365,0.85*maxQ,paste0("Underpredicting ",100*round(SumMod/SumMeas-1,2),"%"),cex=1.5)
    }
    
    plot(DateTime,RunoffMod,log="y",ylim=c(0.1,maxQ),col="red",type="l",xaxt="n",main=paste0(GaugeWatershedData$Name[i]," Hydrograph: Water Years 2014-2020"),xlab="Water Year",ylab="Streamflow, mm/d")
    AxesYears = seq(StartDateTime, EndDateTime+60*60*24*365, by="years")
    axis.POSIXct(1, at=AxesYears,labels=as.integer(format(AxesYears, "%Y"))+1) # start of water year
    lines(DateTime,RunoffMeas,col="blue")
    legend(min(AxesYears),0.9*maxQ,legend=c("Modeled","Measured"),col=c("red","blue"),lty=c(1,1),lwd=c(3,3))
    text(max(AxesYears)-60*60*24*365,0.95*maxQ,paste0("NSE = ",round(nse,2)),cex=1.5)
    if (SumMod > SumMeas){
      text(max(AxesYears)-60*60*24*365,0.6*maxQ,paste0("Overpredicting +",100*round(SumMod/SumMeas-1,2),"%"),cex=1.5)
    } else {
      text(max(AxesYears)-60*60*24*365,0.6*maxQ,paste0("Underpredicting ",100*round(SumMod/SumMeas-1,2),"%"),cex=1.5)
    }

  }
}


