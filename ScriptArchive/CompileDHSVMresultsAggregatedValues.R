# Gather the basin-wide aggregated values (ET, soil moisture, snow, etc.)

library(data.table)

dir = "E:/DHSVM_ScenarioResults"
setwd(dir)

ValueNames = c("W_mm","Precip_m","Snow_m","IExcess_m","HasSnow",
               "SnowCover","LastSnow","Swq","Melt","PackWater",
               "TPack","TotalET","PotTranspStory0","PotTranspStory1",
               "PotTranspStory2","ActTranspStory0","ActTranspStory1",
               "ActTranspStory2","EvapCanopyIntStory0","EvapCanopyIntStory1",
               "ActTranspStory0Soil0","ActTranspStory0Soil1","ActTranspStory0Soil2",
               "ActTranspStory1Soil0","ActTranspStory1Soil1","ActTranspStory1Soil2",
               "SoilEvap","IntRainStory0","IntRainStory1","IntSnowStory0",
               "IntSnowStory1","SoilMoist1","SoilMoist2","SoilMoist3","SoilMoist4",
               "Perc1","Perc2","Perc3","TableDepth","SatFlow","DetentionStorage",
               "NetShortStory1","NetShortStory2","NetShortStory3",
               "LongInStory1","LongInStory2","LongInStory3",
               "PixelNetShort","SoilQnet","SoilQs","SoilQe","SoilQg","SoilQst",
               "Ra","SnowQsw","SnowQlw","SnowQs","SnowQe","SnowQp",
               "SnowMeltEnergy","Tair")

BasinNames = c("Truckee", "Yuba", "Bear", "American")
BasinChunks = c(2, 4, 1, 8) # How the runs were split up

########## DHSVM run settings

DHSVMmodels = c(195,270,276)

Scenarios = c(7)

Climates = c("cnrm", "miroc")
ClimateNames = c("CNRM-CM5_RCP8.5", "MIROC5_RCP8.5")

LandisRuns = c(1, 4) # 1 for CNRM, 4 for MIROC since 1 wasn't fully available

########## DHSVM time settings

RecordStartTime = as.POSIXct("2014-10-01 03:00:00", tz="UTC")
RecordEndTime = as.POSIXct("2099-10-01 00:00:00", tz="UTC")
ModelInterval = 3 # hours
RecordTimes = seq(RecordStartTime, RecordEndTime, by="3 hours")

WaterYears = 2015:2099
DatesOnly = seq(as.Date(paste0("10-01-",WaterYears[1]-1), format="%m-%d-%Y"),
                as.Date(paste0("09-30-",rev(WaterYears)[1]), format="%m-%d-%Y"),
                by="days")

if (exists("AggregatedData")){
  remove(AggregatedData)
}

for (DHSVMmodel in DHSVMmodels){
  for (Scenario in Scenarios){
    for (Climate in Climates){
      
      ChunkStartPoints = list(as.POSIXct(c("2014-10-01 03:00:00", "2057-10-01 00:00:00"), tz="UTC"), # Truckee
                              as.POSIXct(c("2014-10-01 03:00:00", "2036-10-01 00:00:00", "2058-10-01 00:00:00", "2080-10-01 00:00:00"), tz="UTC"), # Yuba
                              as.POSIXct(c("2014-10-01 03:00:00"), tz="UTC"), # Bear
                              as.POSIXct(c("2014-10-01 03:00:00", "2025-10-01 00:00:00", "2036-10-01 00:00:00", "2047-10-01 00:00:00",
                                           "2058-10-01 00:00:00", "2069-10-01 00:00:00", "2080-10-01 00:00:00", "2091-10-01 00:00:00"), tz="UTC")) # American
      
      ChunkEndPoints = list(as.POSIXct(c("2057-09-30 21:00:00", "2099-10-01 00:00:00"), tz="UTC"), # Truckee
                            as.POSIXct(c("2036-09-30 21:00:00", "2058-09-30 21:00:00", "2080-09-30 21:00:00", "2099-10-01 00:00:00"), tz="UTC"), # Yuba
                            as.POSIXct(c("2099-10-01 00:00:00"), tz="UTC"), # Bear
                            as.POSIXct(c("2025-09-30 21:00:00", "2036-09-30 21:00:00", "2047-09-30 21:00:00", "2058-09-30 21:00:00",
                                         "2069-09-30 21:00:00", "2080-09-30 21:00:00", "2091-09-30 21:00:00", "2099-10-01 00:00:00"), tz="UTC")) # American
      
      # Special case: P195S7mirocR4 fails in American chunk 2 (negative soil moisture) on 10/1/24
      # Pull 9/30/2024 initial state from American chunk 1 in P195 S6miroc
      # Restart American chunk 2 in P195 S7miroc using initial state ^ and changing run time to [10/1/2024 to 9/30/2036]
      if ((Scenario==7) & (Climate=="miroc") & (DHSVMmodel==195)){
        
        ChunkEndPoints[[4]][1] = as.POSIXct("2024-09-30 21:00:00", tz="UTC")
        ChunkStartPoints[[4]][2] = as.POSIXct("2024-10-01 00:00:00", tz="UTC")
        
      } # End special case
      
      ClimateName = ClimateNames[which(Climates==Climate)]
      LANDIsrun = LandisRuns[which(Climates==Climate)]
      
      ########## Generic setup for this climate + scenario combo
      
      RunCase = paste0("P",DHSVMmodel,"S",Scenario,Climate,"R",LANDIsrun)
      ResultsDir = paste0(RunCase,"/")
      
      ########## Read 3-hourly Aggregated.Values for each basin, average to daily values, and fill dataframe
      
      for (BasinName in BasinNames){
        
        BasinNum = which(BasinNames==BasinName)
        
        # Loop over each temporal chunk in a given watershed
        for (BasinChunk in 1:BasinChunks[BasinNum]){
          
          # Read the streamflow data output by DHSVM (drop first line that is date-only)
          
          # Special case: P195S7mirocR4 fails in American chunk 2 (negative soil moisture) on 10/1/24
          if ((Scenario==7) & (Climate=="miroc") & (DHSVMmodel==195) & (BasinName=="American") & (BasinChunk==2)){
            DHSVMheader = read.table(paste0(ResultsDir,BasinName,"_Chunk",BasinChunk,"fix/Aggregated.Values"), nrows=1)
            DHSVMdata = read.table(paste0(ResultsDir,BasinName,"_Chunk",BasinChunk,"fix/Aggregated.Values"), skip=1)
          } else { # Normal case
            DHSVMheader = read.table(paste0(ResultsDir,BasinName,"_Chunk",BasinChunk,"/Aggregated.Values"), nrows=1)
            DHSVMdata = read.table(paste0(ResultsDir,BasinName,"_Chunk",BasinChunk,"/Aggregated.Values"), skip=1)
          }
          
          names(DHSVMdata) = DHSVMheader
          DHSVMdata$Date = as.POSIXct(DHSVMdata$Date, format="%m/%d/%Y-%H:%M:%S", tz="UTC")
          
          # Set up date interval that will be used from this chunk (e.g., discard overlap/spin-up)
          ChunkStartTime = ChunkStartPoints[[BasinNum]][BasinChunk]
          ChunkEndTime = ChunkEndPoints[[BasinNum]][BasinChunk]
          
          ChunkStartPtr = which(DHSVMdata$Date == ChunkStartTime)
          ChunkEndPtr = which(DHSVMdata$Date == ChunkEndTime)
          ChunkPtrs = ChunkStartPtr:ChunkEndPtr
          
          if (BasinChunk==1){
            UnchunkedData = DHSVMdata[ChunkPtrs,]
          } else {
            UnchunkedData = rbind(UnchunkedData, DHSVMdata[ChunkPtrs,])
          }
          
        } # End of loop over basin chunks
        
        # Aggregate from 3-hourly to daily
        ResultsMatrix = data.frame(matrix(nrow=length(DatesOnly),
                                          ncol=length(ValueNames)+5)) # Additional columns: datetime, model, scenario, climate, basin
        names(ResultsMatrix) = c("Date","DHSVMmodel","Scenario","Climate","Basin",ValueNames)
        ResultsMatrix$Date = DatesOnly
        
        for (i in 1:length(ValueNames)){
          
          ResultsMatrix[,(i+5)] = colSums(matrix(UnchunkedData[,(i+1)], nrow=8)) / 8
          
        }
        
        ResultsMatrix[,"DHSVMmodel"] = DHSVMmodel
        ResultsMatrix[,"Scenario"] = Scenario
        ResultsMatrix[,"Climate"] = Climate
        ResultsMatrix[,"Basin"] = BasinName
        
        # Append data from all basins / models / scenarios / climates
        if (!exists("AggregatedData")){
          AggregatedData = ResultsMatrix
        } else {
          AggregatedData = rbind(AggregatedData,ResultsMatrix)
        }
        
        print(paste0("Done with ",BasinName," ",RunCase))
        
      } # End of loop over basins
    } # Climate
  } # Scenario
} # Model

length(which(is.na(AggregatedData[,])))

PreProcessedAggregatedData = as.data.frame(fread("AggregatedDailyData_AllBasinsModelsClimatesScenarios.csv"))[,-1]
AggregatedData = rbind(PreProcessedAggregatedData, AggregatedData)

write.csv(AggregatedData, "AggregatedDailyData_AllBasinsModelsClimatesScenarios.csv")
