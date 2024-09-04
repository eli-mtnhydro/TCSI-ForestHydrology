# Collate USGS streamflow data

library(zoo)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/StreamGauges/"
setwd(dir)

GaugeIDs = c(10343500, 10336676, 10336660, 10336645, 103366092, 10336610, 10336780, 10336730, 11413000, 11413300)
GaugeNames = c("Sagehen Creek", "Ward Creek", "Blackwood Creek", "General Creek", "Upper Truckee #1", "Upper Truckee #2", "Trout Creek", "Glenbrook Creek", "North Yuba River", "Slate Creek (Reconstructed)")

# Time period to output results for calibration
BeginDate = as.Date("2011-10-01", format="%Y-%m-%d")
EndDate = as.Date("2017-09-30", format="%Y-%m-%d")
Dates = seq(BeginDate, EndDate, by=1)

# Set up dataframe for results
StreamflowData = data.frame(matrix(nrow=length(Dates),ncol=length(GaugeIDs)+1))
names(StreamflowData) = c("Date",paste0("Gauge_",GaugeIDs))
StreamflowData$Date = Dates
head(StreamflowData)

par(mfrow=c(5,2),mar=c(3,4,2,1))

for (i in 1:length(GaugeIDs)){
  MeasData = read.table(paste0("USGSdata/USGS_",GaugeIDs[i],".txt"), sep="\t")
  MeasData = MeasData[-(1:2),] # Drop 2 header lines
  names(MeasData) = c("Agency", "GaugeID", "Date", "Streamflow_cfs", "Flag")
  MeasData$Date = as.Date(MeasData$Date, format="%Y-%m-%d")
  MeasData$Streamflow_cfs = as.numeric(MeasData$Streamflow_cfs)
  #print(head(MeasData))
  
  # Fill daily streamflow values
  Ptrs = match(StreamflowData$Date,MeasData$Date)
  Ptrs = as.integer(Ptrs) # Convert NAs to NA_integer_ for proper behavior when indexing (NAs preserved in results)
  StreamflowData[,paste0("Gauge_",GaugeIDs[i])] = MeasData$Streamflow_cfs[Ptrs]/(3.28084^3) # ft^3/s --> m^3/s
  
  # One of the gauges has a diversion, so it gets special treatment
  if (GaugeIDs[i]==11413300){
    DivertData = read.table(paste0("USGSdata/USGS_11413250.txt"), sep="\t")
    DivertData = DivertData[-(1:2),] # Drop 2 header lines
    names(DivertData) = c("Agency", "GaugeID", "Date", "Streamflow_cfs", "Flag")
    DivertData$Date = as.Date(DivertData$Date, format="%Y-%m-%d")
    DivertData$Streamflow_cfs = as.numeric(DivertData$Streamflow_cfs)
    
    # Fill daily streamflow values
    Ptrs = match(StreamflowData$Date,DivertData$Date)
    Ptrs = as.integer(Ptrs) # Convert NAs to NA_integer_ for proper behavior when indexing (NAs preserved in results)
    StreamflowData[,paste0("Gauge_",GaugeIDs[i])] = StreamflowData[,paste0("Gauge_",GaugeIDs[i])] + DivertData$Streamflow_cfs[Ptrs]/(3.28084^3) # ft^3/s --> m^3/s
  }
  
  NAnum = sum(as.numeric(is.na(StreamflowData[,paste0("Gauge_",GaugeIDs[i])])))
  print(paste0(GaugeNames[i]," initially had ",signif(NAnum,2)," NAs"))
  
  if (NAnum > 0){
    # Since very few NAs are present in the calibration period, simply perform linear interpolation
    StreamflowData[,paste0("Gauge_",GaugeIDs[i])] = na.approx(StreamflowData[,paste0("Gauge_",GaugeIDs[i])], rule=2)
    
    NAnum = sum(as.numeric(is.na(StreamflowData[,paste0("Gauge_",GaugeIDs[i])])))
    print(paste0(GaugeNames[i]," now has ",signif(NAnum,2)," NAs"))
  }
  
  plot(StreamflowData$Date,StreamflowData[,paste0("Gauge_",GaugeIDs[i])],type="l",main=GaugeNames[i],xlab=NA,ylab="Streamflow, m^3/s")
}

# Make second set of log-scale plots and print max value just for fun
for (i in 1:length(GaugeIDs)){
  plot(StreamflowData$Date,StreamflowData[,paste0("Gauge_",GaugeIDs[i])],log="y",type="l",main=GaugeNames[i],xlab=NA,ylab="Streamflow, m^3/s")
  print(paste0(GaugeNames[i]," max flow = ",signif(max(StreamflowData[,paste0("Gauge_",GaugeIDs[i])]),3)," m^3/s"))
}

write.csv(StreamflowData, "USGS_StreamflowData.csv")
