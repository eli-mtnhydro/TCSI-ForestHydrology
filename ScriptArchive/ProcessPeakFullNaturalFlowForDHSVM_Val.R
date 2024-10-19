# Process daily full natural flows to derive a high-flow dataset

library(zoo)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/StreamGauges/"
setwd(dir)

HighFlowQuantile = 0.95

ValidationWaterYears = 2006:2011
nWaterYears = length(ValidationWaterYears)
BeginDate = as.Date(paste0(ValidationWaterYears[1]-1,"-10-01"), format="%Y-%m-%d")
EndDate = as.Date(paste0(ValidationWaterYears[nWaterYears],"-09-01"), format="%Y-%m-%d")
Dates = seq(BeginDate, EndDate, by=1)

########## Part 1: Yuba

# Set up dataframe for results
PeakFlowData = data.frame(Date=as.Date(""), DailyAvgQ=NA)

# Read daily full natural flow data
MeasData = read.csv("FullNaturalFlow/YRS_DailyFNF_CDEC-Download-2-24-2023.csv")
MeasDataDates = as.Date(MeasData$DATE.TIME, format="%Y%m%d")
MeasData$DATE.TIME = MeasDataDates
MeasQ = as.numeric(MeasData$VALUE) # Make numeric, ok to ignore missing values --> NAs
MeasData$VALUE = MeasQ/(3.28084^3) # ft^3/s --> m^3/s

# Find appropriate number of days during the calibration period to match the specified quantile
nHighFlowDays = round(length(Dates)*(1-HighFlowQuantile))

# Find the flow level corresponding to this percentile (doing it initially based on dates gets rid of any NA problems)
Ptrs = which(MeasData$DATE.TIME >= BeginDate & MeasData$DATE.TIME <= EndDate)
HighFlowThresh = rev(sort(MeasData[Ptrs,]$VALUE))[nHighFlowDays]

print(nHighFlowDays)
print(HighFlowThresh)

par(mar=c(5,5,3,2))
plot(Dates, MeasData$VALUE[Ptrs],type="l",main="Daily Full Natural Flow, Yuba River Near Smartville",xlab="Calendar Year",ylab=expression("Daily FNF, "*m^3*"/s"),cex.main=1.5,cex.lab=1.5,cex.axis=1.5)
lines(Dates,rep(HighFlowThresh,length(Ptrs)),col="red",lwd=2,lty=2)
legend("topright",inset=0.1,legend=c("Reconstructed Hydrograph",paste0(100*HighFlowQuantile,"th Percentile Threshold")),lty=c(1,2),lwd=c(2,2),col=c("black","red"),cex=1.2)

par(mar=c(5,5,3,2))
plot(Dates, MeasData$VALUE[Ptrs],type="l",log="y",main="Daily Full Natural Flow, Yuba River Near Smartville",xlab="Calendar Year",ylab=expression("Daily FNF, "*m^3*"/s"),cex.main=1.5,cex.lab=1.5,cex.axis=1.5)
lines(Dates,rep(HighFlowThresh,length(Ptrs)),col="red",lwd=2,lty=2)
legend("bottomright",inset=0.05,legend=c("Reconstructed Hydrograph","90th Percentile Threshold"),lty=c(1,2),lwd=c(2,2),col=c("black","red"),cex=1.2)

# Append a row specifying the date and value of all high flows during the calibration period
i = 1
for (day in 1:length(Dates)){
  if (is.finite(MeasData[MeasData$DATE.TIME==Dates[day],]$VALUE) & MeasData[MeasData$DATE.TIME==Dates[day],]$VALUE >= HighFlowThresh) {
    PeakFlowData[i,1] = MeasData[MeasData$DATE.TIME==Dates[day],]$DATE.TIME
    PeakFlowData[i,2] = MeasData[MeasData$DATE.TIME==Dates[day],]$VALUE
    i = i+1
  }
}

hist(PeakFlowData$DailyAvgQ)

write.csv(PeakFlowData, paste0("YubaFNF_",100*HighFlowQuantile,"thPercentileFlows_Val.csv"))

########## Part 2: American

# Set up dataframe for results
PeakFlowData = data.frame(Date=as.Date(""), DailyAvgQ=NA)

# Read daily full natural flow data
MeasData = read.csv("FullNaturalFlow/NAT_DailyFNF_CDEC-Download-4-5-2023.csv")
MeasDataDates = as.Date(MeasData$DATE.TIME, format="%Y%m%d")
MeasData$DATE.TIME = MeasDataDates
MeasQ = as.numeric(MeasData$VALUE) # Make numeric, ok to ignore missing values --> NAs
MeasData$VALUE = MeasQ/(3.28084^3) # ft^3/s --> m^3/s

# Find appropriate number of days during the calibration period to match the specified quantile
nHighFlowDays = round(length(Dates)*(1-HighFlowQuantile))

# Find the flow level corresponding to this percentile (doing it initially based on dates gets rid of any NA problems)
Ptrs = which(MeasData$DATE.TIME >= BeginDate & MeasData$DATE.TIME <= EndDate)
HighFlowThresh = rev(sort(MeasData[Ptrs,]$VALUE))[nHighFlowDays]

print(nHighFlowDays)
print(HighFlowThresh)

par(mar=c(5,5,3,2))
plot(Dates, MeasData$VALUE[Ptrs],type="l",main="Daily Full Natural Flow, American River at Natoma",xlab="Calendar Year",ylab=expression("Daily FNF, "*m^3*"/s"),cex.main=1.5,cex.lab=1.5,cex.axis=1.5)
lines(Dates,rep(HighFlowThresh,length(Ptrs)),col="red",lwd=2,lty=2)
legend("topright",inset=0.1,legend=c("Reconstructed Hydrograph",paste0(100*HighFlowQuantile,"th Percentile Threshold")),lty=c(1,2),lwd=c(2,2),col=c("black","red"),cex=1.2)

par(mar=c(5,5,3,2))
plot(Dates, MeasData$VALUE[Ptrs],type="l",log="y",main="Daily Full Natural Flow, American River at Natoma",xlab="Calendar Year",ylab=expression("Daily FNF, "*m^3*"/s"),cex.main=1.5,cex.lab=1.5,cex.axis=1.5)
lines(Dates,rep(HighFlowThresh,length(Ptrs)),col="red",lwd=2,lty=2)
legend("bottomright",inset=0.05,legend=c("Reconstructed Hydrograph","90th Percentile Threshold"),lty=c(1,2),lwd=c(2,2),col=c("black","red"),cex=1.2)

# Append a row specifying the date and value of all high flows during the calibration period
i = 1
for (day in 1:length(Dates)){
  if (is.finite(MeasData[MeasData$DATE.TIME==Dates[day],]$VALUE) & MeasData[MeasData$DATE.TIME==Dates[day],]$VALUE >= HighFlowThresh) {
    PeakFlowData[i,1] = MeasData[MeasData$DATE.TIME==Dates[day],]$DATE.TIME
    PeakFlowData[i,2] = MeasData[MeasData$DATE.TIME==Dates[day],]$VALUE
    i = i+1
  }
}

hist(PeakFlowData$DailyAvgQ)

write.csv(PeakFlowData, paste0("AmericanFNF_",100*HighFlowQuantile,"thPercentileFlows_Val.csv"))

