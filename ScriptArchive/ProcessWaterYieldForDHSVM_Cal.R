# Aggregate yearly water yield for all non-nested basins, including Yuba full natural flow

library(zoo)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/StreamGauges/"
setwd(dir)

CalibrationWaterYears = 2012:2017

# Set up dataframe for results
nWaterYears = length(CalibrationWaterYears)
WaterYieldData = data.frame(matrix(0, nrow=nWaterYears,ncol=2))
names(WaterYieldData) = c("WaterYear","TotalRunoff_km3")
WaterYieldData$WaterYear = CalibrationWaterYears
print(WaterYieldData)

# First, aggregate daily runoff from USGS gauges

GaugeIDs = c(10343500, 10336676, 10336660, 10336645, 10336610, 10336780, 10336730)
GaugeNames = c("Sagehen Creek", "Ward Creek", "Blackwood Creek", "General Creek", "Upper Truckee #2", "Trout Creek", "Glenbrook Creek")

for (yr in 1:nWaterYears){
  # Daily calendar corresponding to current water year
  BeginDate = as.Date(paste0(CalibrationWaterYears[yr]-1,"-10-01"), format="%Y-%m-%d")
  EndDate = as.Date(paste0(CalibrationWaterYears[yr],"-09-30"), format="%Y-%m-%d")
  Dates = seq(BeginDate, EndDate, by=1)
  
  for (i in 1:length(GaugeIDs)){
    MeasData = read.table(paste0("USGSdata/USGS_",GaugeIDs[i],".txt"), sep="\t")
    MeasData = MeasData[-(1:2),] # Drop 2 header lines
    names(MeasData) = c("Agency", "GaugeID", "Date", "Streamflow_cfs", "Flag")
    MeasData$Date = as.Date(MeasData$Date, format="%Y-%m-%d")
    MeasData$Streamflow_cfs = as.numeric(MeasData$Streamflow_cfs)
    #print(head(MeasData))
    
    # Fill daily streamflow values
    Ptrs = match(Dates,MeasData$Date)
    Ptrs = as.integer(Ptrs) # Convert NAs to NA_integer_ for proper behavior when indexing (NAs preserved in results)
    YearlyHydrograph = MeasData$Streamflow_cfs[Ptrs]/(3.28084^3) # ft^3/s --> m^3/s
    
    NAnum = sum(as.numeric(is.na(YearlyHydrograph)))
    print(paste0(GaugeNames[i]," initially had ",signif(NAnum,2)," NAs"))
    
    if (NAnum > 0){
      # Since very few NAs are present in the calibration period, simply perform linear interpolation
      YearlyHydrograph = na.approx(YearlyHydrograph, rule=2)
      
      NAnum = sum(as.numeric(is.na(YearlyHydrograph)))
      print(paste0(GaugeNames[i]," now has ",signif(NAnum,2)," NAs"))
    }
    
    # Add runoff from current gauge to yearly total
    YearlyHydrograph = (YearlyHydrograph * 60*60*24)/(10^9) # m^3/s avg each day --> km^3 each day
    WaterYieldData[yr,]$TotalRunoff_km3 = WaterYieldData[yr,]$TotalRunoff_km3 + sum(YearlyHydrograph)
  }
}

print(WaterYieldData)

plot(CalibrationWaterYears,WaterYieldData$TotalRunoff_km3,type="l",ylim=c(0,8))

# Second, aggregate monthly runoff from CDEC full natural flow point

MeasData = read.csv("FullNaturalFlow/YRS_MonthlyFNF_CDEC-Download-2-24-2023.csv")
MeasDataDates = as.Date(MeasData$DATE.TIME, format="%Y%m%d")

for (yr in 1:nWaterYears){
  # Monthly calendar corresponding to current water year
  BeginDate = as.Date(paste0(CalibrationWaterYears[yr]-1,"-10-01"), format="%Y-%m-%d")
  EndDate = as.Date(paste0(CalibrationWaterYears[yr],"-09-01"), format="%Y-%m-%d")
  Dates = seq(BeginDate, EndDate, by="month")
  
  Ptrs = match(Dates,MeasDataDates)
  YearlyHydrograph = MeasData$VALUE[Ptrs]*1233.5/(10^9) # acre-ft --> km^3
  
  # Add runoff from full natural flow to yearly total
  WaterYieldData[yr,]$TotalRunoff_km3 = WaterYieldData[yr,]$TotalRunoff_km3 + sum(YearlyHydrograph)
}

print(WaterYieldData)

lines(CalibrationWaterYears,WaterYieldData$TotalRunoff_km3)
lines(CalibrationWaterYears,rep(0,nWaterYears))

write.csv(WaterYieldData, "YearlyTotalRunoff_YubaAndTruckee.csv")
