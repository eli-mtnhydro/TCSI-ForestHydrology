# To plot output from DHSVM

library(hydroGOF)

BasinName = "Truckee"
BasinNum = 1

# Read data from DHSVM output
dir = paste0("C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/DHSVM/",BasinNum,"_",BasinName,"/output/")
setwd(dir)
StreamflowMod = read.table("Streamflow.Only",header=T) # must delete first line with no values
head(StreamflowMod)

# Read data from stream gauges
dir2 = paste0("C:/Users/board/Desktop/DHSVM/TCSI_Setup/StreamflowData/")
setwd(dir2)
StreamflowMeas = read.table("SagehenCreek_USGS10343500.txt",header=T) 
head(StreamflowMeas)


# cfs
plot(StreamflowMeas$X7647_00060_00003[1:(length(StreamflowMod$SAGEHEN)/8)],type="l",log="y",ylim=c(1,500))
plot(35.315*StreamflowMod$SAGEHEN/(3*60*60),type="l",log="y",ylim=c(1,500))


DateTime = seq(as.POSIXct("1979-10-01 00:00:00",tz="GMT"), as.POSIXct("1985-09-30 00:00:00",tz="GMT"),by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))
RunoffMod = 0*1:length(DateTime)
RunoffMeas = 0*1:length(DateTime)

RawYears = substr(StreamflowMod$DATE,7,10)
RawMonths = substr(StreamflowMod$DATE,1,2)
RawDays = substr(StreamflowMod$DATE,4,5)
RawTime = substr(StreamflowMod$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
head(ModelDateTime)

GaugeDateTime = as.POSIXct(StreamflowMeas$datetime,tz="GMT")

ModDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

SagehenArea = 10.5 * 2.59E6

for (i in 1:length(DateTime)){
  RunoffMod[i] = mean(StreamflowMod$SAGEHEN[ModDateOnly == DateTime[i]])
  RunoffMeas[i] = StreamflowMeas$X7647_00060_00003[(GaugeDateTime == DateTime[i])]
  
  # mm/d
  RunoffMod[i] = 1000*24*60*60*(RunoffMod[i]/(3*60*60))/SagehenArea
  RunoffMeas[i] = 1000*24*60*60*(RunoffMeas[i]/35.315)/SagehenArea
  
  print(i/length(DateTime))
}

plot(DateTime,RunoffMeas,log="y",ylim=c(0.1,100),col="blue",type="l",xaxt="n",main="Sagehen Creek Hydrograph: Water Years 1980-1985",xlab="Water Year",ylab="Streamflow, mm/d")
AxesYears = c(as.POSIXct("1979-10-01",tz="GMT"),as.POSIXct("1980-10-01",tz="GMT"),as.POSIXct("1981-10-01",tz="GMT"),as.POSIXct("1982-10-01",tz="GMT"),as.POSIXct("1983-10-01",tz="GMT"),as.POSIXct("1984-10-01",tz="GMT"),as.POSIXct("1985-10-01",tz="GMT"))
axis.POSIXct(1, at=AxesYears, labels=format(AxesYears, "%Y"))
lines(DateTime,RunoffMod,col="red")
legend(as.POSIXct("1979-10-01",tz="GMT"),100,legend=c("Measured","Modeled"),col=c("blue","red"),lty=c(1,1))

# Check aggregate water balance
SumMod = sum(RunoffMod)/length(unique(Year))
SumMeas = sum(RunoffMeas)/length(unique(Year))
SumMod/SumMeas

##############################################################################

StreamflowMeas = read.table("BlackwoodCreek_USGS10336660.txt",header=T,fill=TRUE)
head(StreamflowMeas)

# cfs
plot(StreamflowMeas$X7528_00060_00003,type="l",log="y",ylim=c(1,500))
plot(35.315*StreamflowMod$BLACKWOOD/(3*60*60),type="l",log="y",ylim=c(1,500))

DateTime = seq(as.POSIXct("1979-10-01 00:00:00",tz="GMT"), as.POSIXct("1985-09-30 00:00:00",tz="GMT"),by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))
RunoffMod = 0*1:length(DateTime)
RunoffMeas = 0*1:length(DateTime)

RawYears = substr(StreamflowMod$DATE,7,10)
RawMonths = substr(StreamflowMod$DATE,1,2)
RawDays = substr(StreamflowMod$DATE,4,5)
RawTime = substr(StreamflowMod$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
head(ModelDateTime)

GaugeDateTime = as.POSIXct(StreamflowMeas$datetime,tz="GMT")

ModDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

BlackwoodArea = 11.2 * 2.59E6

for (i in 1:length(DateTime)){
  RunoffMod[i] = mean(StreamflowMod$BLACKWOOD[ModDateOnly == DateTime[i]])
  RunoffMeas[i] = StreamflowMeas$X7528_00060_00003[(GaugeDateTime == DateTime[i])]
  
  # mm/d
  RunoffMod[i] = 1000*24*60*60*(RunoffMod[i]/(3*60*60))/BlackwoodArea
  RunoffMeas[i] = 1000*24*60*60*(RunoffMeas[i]/35.315)/BlackwoodArea
  
  print(i/length(DateTime))
}

plot(DateTime,RunoffMeas,log="y",ylim=c(0.1,100),col="blue",type="l",xaxt="n",main="Blackwood Creek Hydrograph: Water Years 1980-1985",xlab="Water Year",ylab="Streamflow, mm/d",cex.main=1)
AxesYears = c(as.POSIXct("1979-10-01",tz="GMT"),as.POSIXct("1980-10-01",tz="GMT"),as.POSIXct("1981-10-01",tz="GMT"),as.POSIXct("1982-10-01",tz="GMT"),as.POSIXct("1983-10-01",tz="GMT"),as.POSIXct("1984-10-01",tz="GMT"),as.POSIXct("1985-10-01",tz="GMT"))
axis.POSIXct(1, at=AxesYears, labels=format(AxesYears, "%Y"))
lines(DateTime,RunoffMod,col="red")
legend(as.POSIXct("1979-10-01",tz="GMT"),100,legend=c("Measured","Modeled"),col=c("blue","red"),lty=c(1,1))

# Check aggregate water balance
SumMod = sum(RunoffMod)/length(unique(Year))
SumMeas = sum(RunoffMeas)/length(unique(Year))
SumMod/SumMeas
(SumMeas-SumMod)/SumMeas

##############################################################################

StreamflowMeas = read.table("WardCreek_USGS10336676.txt",header=T,fill=TRUE)
head(StreamflowMeas)

# cfs
plot(StreamflowMeas$X7549_00060_00003,type="l",log="y",ylim=c(1,500))
plot(35.315*StreamflowMod$WARD/(3*60*60),type="l",log="y",ylim=c(1,500))

DateTime = seq(as.POSIXct("1979-10-01 00:00:00",tz="GMT"), as.POSIXct("1985-09-30 00:00:00",tz="GMT"),by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))
RunoffMod = 0*1:length(DateTime)
RunoffMeas = 0*1:length(DateTime)

RawYears = substr(StreamflowMod$DATE,7,10)
RawMonths = substr(StreamflowMod$DATE,1,2)
RawDays = substr(StreamflowMod$DATE,4,5)
RawTime = substr(StreamflowMod$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
head(ModelDateTime)

GaugeDateTime = as.POSIXct(StreamflowMeas$datetime,tz="GMT")

ModDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

WardArea = 9.7 * 2.59E6

for (i in 1:length(DateTime)){
  RunoffMod[i] = mean(StreamflowMod$WARD[ModDateOnly == DateTime[i]])
  RunoffMeas[i] = StreamflowMeas$X7549_00060_00003[(GaugeDateTime == DateTime[i])]
  
  # mm/d
  RunoffMod[i] = 1000*24*60*60*(RunoffMod[i]/(3*60*60))/WardArea
  RunoffMeas[i] = 1000*24*60*60*(RunoffMeas[i]/35.315)/WardArea
  
  print(i/length(DateTime))
}

plot(DateTime,RunoffMeas,log="y",ylim=c(0.1,100),col="blue",type="l",xaxt="n",main="Ward Creek Hydrograph: Water Years 1980-1985",xlab="Water Year",ylab="Streamflow, mm/d",cex.main=1)
AxesYears = c(as.POSIXct("1979-10-01",tz="GMT"),as.POSIXct("1980-10-01",tz="GMT"),as.POSIXct("1981-10-01",tz="GMT"),as.POSIXct("1982-10-01",tz="GMT"),as.POSIXct("1983-10-01",tz="GMT"),as.POSIXct("1984-10-01",tz="GMT"),as.POSIXct("1985-10-01",tz="GMT"))
axis.POSIXct(1, at=AxesYears, labels=format(AxesYears, "%Y"))
lines(DateTime,RunoffMod,col="red")
legend(as.POSIXct("1979-10-01",tz="GMT"),100,legend=c("Measured","Modeled"),col=c("blue","red"),lty=c(1,1))

# Check aggregate water balance
SumMod = sum(RunoffMod)/length(unique(Year))
SumMeas = sum(RunoffMeas)/length(unique(Year))
SumMod/SumMeas
(SumMeas-SumMod)/SumMeas

##############################################################################

StreamflowMeas = read.table("GeneralCreek_USGS10336645.txt",header=T,fill=TRUE)
head(StreamflowMeas)

# cfs
plot(StreamflowMeas$X7505_00060_00003,type="l",log="y",ylim=c(1,500))
plot(35.315*StreamflowMod$GENERAL/(3*60*60),type="l",log="y",ylim=c(1,500))

DateTime = seq(as.POSIXct("1980-10-01 00:00:00",tz="GMT"), as.POSIXct("1985-09-30 00:00:00",tz="GMT"),by="days")
Year = as.integer(format(DateTime,"%Y"))
Month = as.integer(format(DateTime,"%m"))
Day = as.integer(format(DateTime,"%d"))
RunoffMod = 0*1:length(DateTime)
RunoffMeas = 0*1:length(DateTime)

RawYears = substr(StreamflowMod$DATE,7,10)
RawMonths = substr(StreamflowMod$DATE,1,2)
RawDays = substr(StreamflowMod$DATE,4,5)
RawTime = substr(StreamflowMod$DATE,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")
head(ModelDateTime)

GaugeDateTime = as.POSIXct(StreamflowMeas$datetime,tz="GMT")

ModDateOnly = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," 00:00:00"),tz="GMT")

GeneralArea = 7.44 * 2.59E6

for (i in 1:length(DateTime)){
  RunoffMod[i] = mean(StreamflowMod$GENERAL[ModDateOnly == DateTime[i]])
  RunoffMeas[i] = StreamflowMeas$X7505_00060_00003[(GaugeDateTime == DateTime[i])]
  
  # mm/d
  RunoffMod[i] = 1000*24*60*60*(RunoffMod[i]/(3*60*60))/WardArea
  RunoffMeas[i] = 1000*24*60*60*(RunoffMeas[i]/35.315)/WardArea
  
  print(i/length(DateTime))
}

plot(DateTime,RunoffMeas,log="y",ylim=c(0.1,60),col="blue",type="l",xaxt="n",main="General Creek Hydrograph: Water Years 1981-1985",xlab="Water Year",ylab="Streamflow, mm/d",cex.main=1)
AxesYears = c(as.POSIXct("1980-10-01",tz="GMT"),as.POSIXct("1981-10-01",tz="GMT"),as.POSIXct("1982-10-01",tz="GMT"),as.POSIXct("1983-10-01",tz="GMT"),as.POSIXct("1984-10-01",tz="GMT"),as.POSIXct("1985-10-01",tz="GMT"))
axis.POSIXct(1, at=AxesYears, labels=format(AxesYears, "%Y"))
lines(DateTime,RunoffMod,col="red")
legend(as.POSIXct("1980-10-01",tz="GMT"),60,legend=c("Measured","Modeled"),col=c("blue","red"),lty=c(1,1))

# Check aggregate water balance
SumMod = sum(RunoffMod)/length(unique(Year))
SumMeas = sum(RunoffMeas)/length(unique(Year))
SumMod/SumMeas











