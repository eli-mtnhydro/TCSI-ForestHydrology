

dir = "C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/DHSVM/X.SensitivityAnalysis/StreamflowSensitivity/"
setwd(dir)

Data.NoUnd = read.table("NoUnderstory_Pixel.SierraMixedConiferPixel.txt", header=TRUE)
head(Data.NoUnd)

Data.NegUnd = read.table("NegligibleUnderstory_Pixel.SierraMixedConiferPixel.txt", header=TRUE)
head(Data.NegUnd)

names(Data.NoUnd)

RawYears = substr(Data.NoUnd$Date,7,10)
RawMonths = substr(Data.NoUnd$Date,1,2)
RawDays = substr(Data.NoUnd$Date,4,5)
RawTime = substr(Data.NoUnd$Date,12,19)
ModelDateTime = as.POSIXct(paste0(RawYears,"-",RawMonths,"-",RawDays," ",RawTime,":0"),tz="GMT")

AxesMonths = seq(ModelDateTime[1], tail(ModelDateTime,1), by="months")

PlotColors = c("goldenrod2","darkorchid1")

# SWE
LineWidth = 3
plot(ModelDateTime, Data.NoUnd$Swq,col=PlotColors[1],lwd=LineWidth,type="l",main="Snow Water Equivalent",xaxt="n",xlab="",ylab="SWE, m",cex.axis=1.2,cex.lab=1.2)
lines(ModelDateTime, Data.NegUnd$Swq,col=PlotColors[2],lwd=LineWidth,lty=3)
axis.POSIXct(1, at=AxesMonths, labels=format(AxesMonths, "%b"), cex.axis=1.2) # start of water year
legend("topleft",inset=0.03,legend=c("No Understory","Negligible Understory"),col=c(PlotColors[1],PlotColors[2]),lty=c(1,3),lwd=rep(3,2))

# ET
# Convert m/3hr to mm/d
LineWidth = 1
plot(ModelDateTime, Data.NoUnd$TotalET*1000*24/3,col=PlotColors[1],lwd=LineWidth,type="l",ylim=c(0,40),main="Total Evapotranspiration",xaxt="n",xlab="",ylab="ET, mm/d",cex.axis=1.2,cex.lab=1.2)
lines(ModelDateTime, Data.NegUnd$TotalET*1000*24/3,col=PlotColors[2],lwd=LineWidth,lty=3)
axis.POSIXct(1, at=AxesMonths, labels=format(AxesMonths, "%b"), cex.axis=1.2) # start of water year
legend("topright",inset=0.03,legend=c("No Understory","Negligible Understory"),col=c(PlotColors[1],PlotColors[2]),lty=c(1,3),lwd=rep(3,2))

# Water table depth
# Convert to positive-down
LineWidth = 3
plot(ModelDateTime, Data.NoUnd$TableDepth,col=PlotColors[1],lwd=LineWidth,type="l",ylim=c(1.2,0),main="Water Table Depth",xaxt="n",xlab="",ylab="Water Table Depth, m",cex.axis=1.2,cex.lab=1.2)
lines(ModelDateTime, Data.NegUnd$TableDepth,col=PlotColors[2],lwd=LineWidth,lty=3)
axis.POSIXct(1, at=AxesMonths, labels=format(AxesMonths, "%b"), cex.axis=1.2) # start of water year
legend("bottomright",inset=0.03,legend=c("No Understory","Negligible Understory"),col=c(PlotColors[1],PlotColors[2]),lty=c(1,3),lwd=rep(3,2))




