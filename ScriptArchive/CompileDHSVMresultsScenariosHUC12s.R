# Gather all streamflow records for HUC12s (includes some other watersheds)

dir = "E:/DHSVM_ScenarioResults"
setwd(dir)

HUCsavePoints = read.csv("TCSI_HUC12savePoints.csv")[,-1]
head(HUCsavePoints)

################################################################################
# Set up HUC12 datasheet

# options(scipen=99) # For dealing with HUC12 IDs
# 
# # Add "HUC12_" to DHSVM_ID, as used in Stream.Flow file
# HUC12ptrs = which(HUCsavePoints$HUC_ID > 9999)
# HUCsavePoints$DHSVM_ID[HUC12ptrs] = paste0("HUC12_",HUCsavePoints$HUC_ID[HUC12ptrs])
# 
# # Identify contributing (upstream) basin numbers (already associated with HUC ID)
# for (i in HUCsavePoints$HUC_ID){
#   Inbound1ptrs = which(HUCsavePoints$InboundHUC_1==i)
#   Inbound2ptrs = which(HUCsavePoints$InboundHUC_2==i)
#   Inbound3ptrs = which(HUCsavePoints$InboundHUC_3==i)
#   Inbound4ptrs = which(HUCsavePoints$InboundHUC_4==i)
#   
#   WatershedNum = HUCsavePoints$Watershed_Num[which(HUCsavePoints$HUC_ID==i)]
#   
#   HUCsavePoints$Inbound_1[Inbound1ptrs] = WatershedNum
#   HUCsavePoints$Inbound_2[Inbound2ptrs] = WatershedNum
#   HUCsavePoints$Inbound_3[Inbound3ptrs] = WatershedNum
#   HUCsavePoints$Inbound_4[Inbound4ptrs] = WatershedNum
# }
# 
# print(HUCsavePoints)
# write.csv(HUCsavePoints, "TCSI_HUC12savePoints.csv")

################################################################################
# Create CSV for each scenario/climate with each column ~ HUC12

BasinNames = c("Truckee", "Yuba", "Bear", "American")
BasinChunks = c(2, 4, 1, 8) # How the runs were split up

# Which sub-basins do we care about today?
GaugeIDs = HUCsavePoints$DHSVM_ID

# Which DHSVM basin are the sub-basins in?
# 1 --> Truckee
# 2 --> Yuba
# 3 --> Bear
# 4 --> American
GaugeBasins = HUCsavePoints$Basin
GaugeBasins[GaugeBasins=="Truckee"] = 1
GaugeBasins[GaugeBasins=="Yuba"] = 2
GaugeBasins[GaugeBasins=="Bear"] = 3
GaugeBasins[GaugeBasins=="American"] = 4
GaugeBasins = as.numeric(GaugeBasins)

########## DHSVM run settings

SelectedModel = 276

Scenarios = c(7)

Climates = c("cnrm", "miroc")
ClimateNames = c("CNRM-CM5_RCP8.5", "MIROC5_RCP8.5")

LandisRuns = c(1, 4) # 1 for CNRM, 4 for MIROC since 1 wasn't fully available

########## DHSVM time settings

RecordStartTime = as.POSIXct("2014-10-01 03:00:00", tz="UTC")
RecordEndTime = as.POSIXct("2099-10-01 00:00:00", tz="UTC")
ModelInterval = 3 # hours
RecordTimes = seq(RecordStartTime, RecordEndTime, by="3 hours")

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
    if ((Scenario==7) & (Climate=="miroc") & (SelectedModel==195)){
      
      ChunkEndPoints[[4]][1] = as.POSIXct("2024-10-01 00:00:00", tz="UTC")
      ChunkStartPoints[[4]][2] = as.POSIXct("2024-10-01 03:00:00", tz="UTC")
      
    } # End special case
    
    ClimateName = ClimateNames[which(Climates==Climate)]
    LANDIsrun = LandisRuns[which(Climates==Climate)]
    
    ########## Generic setup for this climate + scenario combo
    
    RunCase = paste0("P",SelectedModel,"S",Scenario,Climate,"R",LANDIsrun)
    ResultsDir = paste0(RunCase,"/")
    #ResultsDir = paste0(RunCase,"stream/")
    
    ResultsMatrix = data.frame(matrix(nrow=length(RecordTimes),
                                            ncol=length(HUCsavePoints$Watershed_Num)+1))
    names(ResultsMatrix) = c("DateTime", paste0("WSHD_",HUCsavePoints$Watershed_Num))
    ResultsMatrix$DateTime = RecordTimes
    
    ########## Create and export 3-hourly csv for each gauge of interest
    
    for (gauge in 1:length(GaugeIDs)){
      
      BasinNum = GaugeBasins[gauge]
      BasinName = BasinNames[BasinNum]
      
      # Timeseries indexing of results (keep track of which chunks have been filled)
      ResultsIdx = 1
      
      # Loop over each temporal chunk in a given watershed
      for (BasinChunk in 1:BasinChunks[BasinNum]){
        
        # Read the streamflow data output by DHSVM (drop first line that is date-only)
        
        # Special case: P195S7mirocR4 fails in American chunk 2 (negative soil moisture) on 10/1/24
        if ((Scenario==7) & (Climate=="miroc") & (SelectedModel==195) & (BasinName=="American") & (BasinChunk==2)){
          DHSVMheader = read.table(paste0(ResultsDir,BasinName,"_Chunk",BasinChunk,"fix/Streamflow.Only"), nrows=1)
          DHSVMstreamflow = read.table(paste0(ResultsDir,BasinName,"_Chunk",BasinChunk,"fix/Streamflow.Only"), skip=2)
        } else { # Normal case
          DHSVMheader = read.table(paste0(ResultsDir,BasinName,"_Chunk",BasinChunk,"/Streamflow.Only"), nrows=1)
          DHSVMstreamflow = read.table(paste0(ResultsDir,BasinName,"_Chunk",BasinChunk,"/Streamflow.Only"), skip=2)
        }
        
        names(DHSVMstreamflow) = DHSVMheader
        DHSVMstreamflow$DATE = as.POSIXct(DHSVMstreamflow$DATE, format="%m.%d.%Y-%H:%M:%S", tz="UTC")
        
        # Set up date interval that will be used from this chunk (e.g., discard overlap/spin-up)
        ChunkStartTime = ChunkStartPoints[[BasinNum]][BasinChunk]
        ChunkEndTime = ChunkEndPoints[[BasinNum]][BasinChunk]
        
        ChunkStartPtr = which(DHSVMstreamflow$DATE == ChunkStartTime)
        ChunkEndPtr = which(DHSVMstreamflow$DATE == ChunkEndTime)
        ChunkPtrs = ChunkStartPtr:ChunkEndPtr
        ResultPtrs = ResultsIdx:(ResultsIdx + length(ChunkPtrs) - 1)
        
        # Double check everything is lining up right
        if (all(DHSVMstreamflow$DATE[ChunkPtrs] == RecordTimes[ResultPtrs])){
          
          ########## Put streamflow values in results dataframe
          
          ResultsMatrix[ResultPtrs,paste0("WSHD_",gauge)] = DHSVMstreamflow[ChunkPtrs, GaugeIDs[gauge]] / (ModelInterval*60*60) # m^3/interval --> m^3/s
          
        } else {
          print("Error in indexing!")
        }
        
        # Advance pointers to next chunk
        ResultsIdx = ResultsIdx + length(ChunkPtrs)
        
      } # End of loop over basin chunks
      
      # Double check
      if (length(which(is.na(ResultsMatrix[,paste0("WSHD_",gauge)]))) > 0){
        print("!!!!!!!!!! ERROR IN ALIGNMENT !!!!!!!!!!")
      }
      
      print(paste0("Done with ", HUCsavePoints$DHSVM_ID[gauge]))
      
    } # End of loop over gauges / watersheds
    
    # Round to reasonable numeric precision (~0.03 cfs)
    ResultsMatrix[,names(ResultsMatrix)!="DateTime"] = round(ResultsMatrix[,names(ResultsMatrix)!="DateTime"], 3)
    
    ########## Output results
    
    # Streamflow saved in m^3/s
    write.csv(ResultsMatrix, paste0(RunCase,"_WSHDsaveMatrix.csv"))
    print(paste0("********** Saving data for ",RunCase," **********"))
    
    ########## End of post-processing for particular climate + scenario combo
  }
}

