# Recombine temporal chunks/basins and output CSVs of data

################################################################################
# Scenario runs compilation
################################################################################

BasinChunks = c("Truckee_Chunk1", "Truckee_Chunk2",
                       "Yuba_Chunk1", "Yuba_Chunk2", "Yuba_Chunk3", "Yuba_Chunk4",
                       "Bear_Chunk1",
                       "American_Chunk1", "American_Chunk2", "American_Chunk3", "American_Chunk4",
                       "American_Chunk5", "American_Chunk6", "American_Chunk7", "American_Chunk8")

ChunkLengths = c(rep(40, 2), rep(20, 4), 80, rep(10, 8)) # Years

GaugeIDs = c(10343000, 10344300, 10344500, 10340500, 10338500,   # Truckee
             10343500, 10338000, 10337500, 10336676, 10336660,   # Truckee
             10336645, 103366092, 10336610, 10336780, 10336730,  # Truckee
             11413000, 11413300, 11413517, 11413510, 11409400,   # Yuba
             11408880, 11418500, 11414250, "OCC", "NYS", "YRS",  # Yuba
             11421710, 11421770, 11421790, 11422500,             # Bear
             11427000, 11433790, 11427500, 11427760, 11433300,   # American
             11427700, 11427960, 11441900, 11435100, 11439500,   # American
             11442500, 11443500, 11444500, 11446030, "AMF")      # American

GaugeNames = c("Independence Creek", "Stampede Outflow", "Boca Outflow", "Prosser Outflow", "Donner Creek",
               "Sagehen Creek", "Truckee River", "Tahoe Outflow", "Ward Creek", "Blackwood Creek",
               "General Creek", "Upper Truckee #1", "Upper Truckee #2", "Trout Creek", "Glenbrook Creek",
               "North Yuba River #1", "Slate Creek", "Bullards Bar Outflow", "New Colgate", "Oregon Creek #2",
               "Middle Yuba River", "Deer Creek", "South Yuba River", "Oregon Creek #1", "North Yuba River #2", "Yuba River Marysville",
               "Bear River #1", "Bear River #2", "Bear River #3", "Bear River #4",
               "N.F. American River #1", "N.F. American River #2", "M.F. American River #1", "M.F. American River #2", "M.F. American River #3",
               "Duncan Canyon Creek", "Rubicon River", "Silver Creek", "Pyramid Creek", "S.F. American River #1",
               "S.F. American River #2", "S.F. American River #3", "S.F. American River #4", "S.F. American River #5", "American River Folsom")






################################################################################
# Static veg compilation
################################################################################

dir = "C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/DHSVM/results/"
setwd(dir)

########## DHSVM run settings

Scenario = "StaticVeg"

# Climate = "cnrm"
# ClimateName = "CNRM-CM5_RCP8.5"
Climate = "miroc"
ClimateName = "MIROC5_RCP8.5"

SelectedModel = 276

LANDIsrun = 1

########## Generic setup

ResultsDir = paste0("StaticVeg_P",SelectedModel,Climate,"R",LANDIsrun,"/")
OutputDir = "StreamflowResults_StaticVeg/"

RecordStartTime = as.POSIXct("2019-10-01 00:00:00", tz="UTC")
RecordEndTime = as.POSIXct("2099-09-30 21:00:00", tz="UTC")
ModelInterval = 3 # hours
RecordTimes = seq(RecordStartTime, RecordEndTime, by="3 hours")

BasinNames = c("Truckee", "Yuba", "Bear")

BasinChunks = c(2, 4, 1)

ChunkLengths = 80/BasinChunks # Years

GaugeIDs = c(10343000, 10344300, 10344500, 10340500, 10338500,                     # Truckee
             10343500, 10338000, 10337500, 10336676, 10336660,                     # Truckee
             10336645, 103366092, 10336610, 10336780, 10336730, "Truckee_OUTLET",  # Truckee
             11413000, 11413300, 11413517, 11413510, 11409400,                     # Yuba
             11408880, 11418500, 11414250, "OCC", "NYS", "YRS", "Yuba_OUTLET",     # Yuba
             11421710, 11421770, 11421790, 11422500, "Bear_OUTLET")                # Bear
GaugeIDs[!grepl("OUTLET", GaugeIDs)] = paste0("Gauge_", GaugeIDs[!grepl("OUTLET", GaugeIDs)])

GaugeNames = c("Independence Creek", "Stampede Outflow", "Boca Outflow", "Prosser Outflow", "Donner Creek",
               "Sagehen Creek", "Truckee River", "Tahoe Outflow", "Ward Creek", "Blackwood Creek",
               "General Creek", "Upper Truckee #1", "Upper Truckee #2", "Trout Creek", "Glenbrook Creek", "Truckee Outlet",
               "North Yuba River #1", "Slate Creek", "Bullards Bar Outflow", "New Colgate", "Oregon Creek #2",
               "Middle Yuba River", "Deer Creek", "South Yuba River", "Oregon Creek #1", "North Yuba River #2", "Yuba River Marysville", "Yuba Outlet",
               "Bear River #1", "Bear River #2", "Bear River #3", "Bear River #4", "Bear Outlet")

GaugeBasins = c(rep(1, 16), rep(2, 12), rep(3, 5))

########## Create and export 3-hourly csv for each gauge

for (gauge in 1:length(GaugeIDs)){
  
  BasinNum = GaugeBasins[gauge]
  BasinName = BasinNames[BasinNum]
  
  # Pointers to the current position in the results dataframe
  ResultsIdxStart = 1
  ResultsIdxInterval = length(RecordTimes) / BasinChunks[BasinNum] - 1
  
  # Reset results dataframe
  ResultColNames = c("DateTime", "Streamflow_cms", "GaugeID", "GaugeName", "Climate", "Scenario", "DHSVMmodel", "LANDISrun")
  StreamflowResults = data.frame(matrix(nrow=length(RecordTimes), ncol=length(ResultColNames)))
  names(StreamflowResults) = ResultColNames
  StreamflowResults$DateTime = RecordTimes
  StreamflowResults$GaugeID = GaugeIDs[gauge]
  StreamflowResults$GaugeName = GaugeNames[gauge]
  StreamflowResults$Climate = ClimateName
  StreamflowResults$Scenario = Scenario
  StreamflowResults$DHSVMmodel = SelectedModel
  StreamflowResults$LANDISrun = LANDIsrun
  
  # Loop over each temporal chunk in a given watershed
  for (BasinChunk in 1:BasinChunks[BasinNum]){
    
    # Read the streamflow data output by DHSVM (drop first line that is date-only)
    DHSVMheader = read.table(paste0(ResultsDir,BasinName,"_Chunk",BasinChunk,"/Streamflow.Only"), nrows=1)
    DHSVMstreamflow = read.table(paste0(ResultsDir,BasinName,"_Chunk",BasinChunk,"/Streamflow.Only"), skip=2)
    names(DHSVMstreamflow) = DHSVMheader
    DHSVMstreamflow$DATE = as.POSIXct(DHSVMstreamflow$DATE, format="%m.%d.%Y-%H:%M:%S", tz="UTC")
    
    # Set up date interval that will be used from this chunk (e.g., discard overlap/spin-up)
    ChunkEndTime = max(DHSVMstreamflow$DATE)
    StartYr = as.numeric(format(ChunkEndTime,"%Y")) - ChunkLengths[BasinNum]
    ChunkStartTime = as.POSIXct(paste0(StartYr, "-10-01 00:00:00"), tz="UTC")
    
    ChunkStartPtr = which(DHSVMstreamflow$DATE == ChunkStartTime)
    ChunkEndPtr = which(DHSVMstreamflow$DATE == ChunkEndTime)
    ChunkPtrs = ChunkStartPtr:ChunkEndPtr
    ResultPtrs = ResultsIdxStart:(ResultsIdxStart+ResultsIdxInterval)
    
    # Double check everything is lining up right
    if (all(DHSVMstreamflow$DATE[ChunkPtrs] == RecordTimes[ResultPtrs])){
      
      ########## Put streamflow values in results dataframe
      
      # Special case: the routing is funky in DHSVM, so gauge 10344300 is a combo of 2 segments
      if (GaugeIDs[gauge] == "Gauge_10344300"){
        StreamflowResults$Streamflow_cms[ResultPtrs] = DHSVMstreamflow[ChunkPtrs, "Gauge_10344300-a"] / (ModelInterval*60*60) # m^3/interval --> m^3/s
        StreamflowResults$Streamflow_cms[ResultPtrs] = StreamflowResults$Streamflow_cms[ResultPtrs] + DHSVMstreamflow[ChunkPtrs, "Gauge_10344300-b"] / (ModelInterval*60*60)
      } else {
        StreamflowResults$Streamflow_cms[ResultPtrs] = DHSVMstreamflow[ChunkPtrs, GaugeIDs[gauge]] / (ModelInterval*60*60) # m^3/interval --> m^3/s
      }
      
    } else {
      print("Error in indexing!")
    }
    
    # Advance pointers to next chunk
    ResultsIdxStart = ResultsIdxStart + ResultsIdxInterval + 1
    
  } # End of loop over basin chunks
  
  # Round to reasonable numeric precision (~0.03 cfs)
  StreamflowResults$Streamflow_cms = round(StreamflowResults$Streamflow_cms, 3)
  
  ########## Output results
  
  # Set up filename, changing order of "Basin_OUTLET" to "Outlet_Basin" for better organization
  if (grepl("OUTLET", GaugeIDs[gauge])){
    OutFilename = paste0(OutputDir, "Outlet_", BasinName, "_3-hourly_streamflow.csv")
  } else {
    OutFilename = paste0(OutputDir, GaugeIDs[gauge], "_3-hourly_streamflow.csv")
  }
  
  # Check if there is already a file for this gauge, and if so, append the new data, otherwise create new file
  if (file.exists(OutFilename)){
    PreviousResults = read.csv(OutFilename)
    # Check that the current combination of things hasn't already been filled
    FilledCases = unique(paste0(PreviousResults$Climate, PreviousResults$Scenario, PreviousResults$DHSVMmodel, PreviousResults$LANDISrun))
    ThisCase = unique(paste0(StreamflowResults$Climate, StreamflowResults$Scenario, StreamflowResults$DHSVMmodel, StreamflowResults$LANDISrun))
    if (!(ThisCase %in% FilledCases)){
      StreamflowResults$DateTime = as.character(StreamflowResults$DateTime) # Make POSIXct behave nicely
      StreamflowResults = rbind(PreviousResults, StreamflowResults)
      write.csv(StreamflowResults, OutFilename, row.names=FALSE)
      print(paste0("Appending to file for ", GaugeNames[gauge]))
    } else {
      print(paste0("These data may already exist for ", GaugeNames[gauge], ", not writing anything!"))
    }
    
  } else {
    write.csv(StreamflowResults, OutFilename, row.names=FALSE)
    print(paste0("Writing new file for ", GaugeNames[gauge]))
  }
  
}

################################################################################
########## Aggregate outlet points to daily streamflow

SelectedGaugeIDs = c("Outlet_Truckee", "Outlet_Yuba", "Outlet_Bear") # As specified in the csv filenames above
SelectedGaugeIDsOriginal = c("Truckee_OUTLET", "Yuba_OUTLET", "Bear_OUTLET") # As specified in the DHSVM files
SelectedAreas = c(280558, 424083, 72275) * 90^2 # Cell counts to m^2

# Set up empty vectors to be filled
DateVec = c()
StreamflowVec = c()
GaugeIDvec = c()
GaugeNameVec = c()
ClimateVec = c()
ScenarioVec = c()
DHSVMmodelVec = c()
LANDISrunVec = c()

for (gauge in 1:length(SelectedGaugeIDs)){
  
  # Set up pointers based on the knowledge that sub-daily values are concatenated in sets of 8
  Ptrs = 1:(24/ModelInterval)
  PtrInterval = max(Ptrs)
  
  StreamflowResults = read.csv(paste0(OutputDir, SelectedGaugeIDs[gauge], "_3-hourly_streamflow.csv"))
  StreamflowResults$Date = as.Date(StreamflowResults$DateTime)
  
  while (Ptrs[1] < length(StreamflowResults$Date)){
    
    StreamflowVec = c(StreamflowVec, mean(StreamflowResults[Ptrs,"Streamflow_cms"]))
    
    DateVec = c(DateVec, StreamflowResults[Ptrs[1],"Date"])
    GaugeIDvec = c(GaugeIDvec, StreamflowResults[Ptrs[1],"GaugeID"])
    GaugeNameVec = c(GaugeNameVec, StreamflowResults[Ptrs[1],"GaugeName"])
    ClimateVec = c(ClimateVec, StreamflowResults[Ptrs[1],"Climate"])
    ScenarioVec = c(ScenarioVec, StreamflowResults[Ptrs[1],"Scenario"])
    DHSVMmodelVec = c(DHSVMmodelVec, StreamflowResults[Ptrs[1],"DHSVMmodel"])
    LANDISrunVec = c(LANDISrunVec, StreamflowResults[Ptrs[1],"LANDISrun"])
    
    Ptrs = Ptrs + PtrInterval
  }
  
  print(paste0("Done with ", SelectedGaugeIDs[gauge]))
}

CombinedResults = data.frame(Date=as.Date(DateVec, origin="1970-01-01"), Streamflow_cms=StreamflowVec,
                             GaugeID=GaugeIDvec, GaugeName=GaugeNameVec,
                             Climate=ClimateVec, Scenario=ScenarioVec,
                             DHSVMmodel=DHSVMmodelVec, LANDISrun=LANDISrunVec)

# m^3/s to mm/d
CombinedResults$Area = SelectedAreas[match(CombinedResults$GaugeID, SelectedGaugeIDsOriginal)]
CombinedResults$Streamflow_mmd = CombinedResults$Streamflow_cms * (60*60*24) / CombinedResults$Area

# Remove "Outlet" from names
CombinedResults$GaugeName = sub(" Outlet", "", CombinedResults$GaugeName)

# Replace "_" with " " in climates
CombinedResults$Climate = sub("_", " ", CombinedResults$Climate)

write.csv(CombinedResults, "StaticVeg_TCSI_outlets_daily_streamflow.csv", row.names=FALSE)

################################################################################
########## Aggregate other selected points to daily streamflow

SelectedGaugeIDs = c("Gauge_11413000") # As specified in the csv filenames above
SelectedGaugeIDsOriginal = c("Gauge_11413000") # As specified in the DHSVM files
# SelectedAreas = 2872463760 # m^2, YRS
SelectedAreas = 644502788 # m^2, 11413000

# Set up empty vectors to be filled
DateVec = c()
StreamflowVec = c()
GaugeIDvec = c()
GaugeNameVec = c()
ClimateVec = c()
ScenarioVec = c()
DHSVMmodelVec = c()
LANDISrunVec = c()

for (gauge in 1:length(SelectedGaugeIDs)){
  
  # Set up pointers based on the knowledge that sub-daily values are concatenated in sets of 8
  Ptrs = 1:(24/ModelInterval)
  PtrInterval = max(Ptrs)
  
  StreamflowResults = read.csv(paste0(OutputDir, SelectedGaugeIDs[gauge], "_3-hourly_streamflow.csv"))
  StreamflowResults$Date = as.Date(StreamflowResults$DateTime)
  
  while (Ptrs[1] < length(StreamflowResults$Date)){
    
    StreamflowVec = c(StreamflowVec, mean(StreamflowResults[Ptrs,"Streamflow_cms"]))
    
    DateVec = c(DateVec, StreamflowResults[Ptrs[1],"Date"])
    GaugeIDvec = c(GaugeIDvec, StreamflowResults[Ptrs[1],"GaugeID"])
    GaugeNameVec = c(GaugeNameVec, StreamflowResults[Ptrs[1],"GaugeName"])
    ClimateVec = c(ClimateVec, StreamflowResults[Ptrs[1],"Climate"])
    ScenarioVec = c(ScenarioVec, StreamflowResults[Ptrs[1],"Scenario"])
    DHSVMmodelVec = c(DHSVMmodelVec, StreamflowResults[Ptrs[1],"DHSVMmodel"])
    LANDISrunVec = c(LANDISrunVec, StreamflowResults[Ptrs[1],"LANDISrun"])
    
    Ptrs = Ptrs + PtrInterval
  }
  
  print(paste0("Done with ", SelectedGaugeIDs[gauge]))
}

CombinedResults = data.frame(Date=as.Date(DateVec, origin="1970-01-01"), Streamflow_cms=StreamflowVec,
                             GaugeID=GaugeIDvec, GaugeName=GaugeNameVec,
                             Climate=ClimateVec, Scenario=ScenarioVec,
                             DHSVMmodel=DHSVMmodelVec, LANDISrun=LANDISrunVec)

# m^3/s to mm/d
CombinedResults$Area = SelectedAreas[match(CombinedResults$GaugeID, SelectedGaugeIDsOriginal)]
CombinedResults$Streamflow_mmd = CombinedResults$Streamflow_cms * (60*60*24) / CombinedResults$Area

# Remove "Outlet" from names
CombinedResults$GaugeName = sub(" Outlet", "", CombinedResults$GaugeName)

# Replace "_" with " " in climates
CombinedResults$Climate = sub("_", " ", CombinedResults$Climate)

write.csv(CombinedResults, "StaticVeg_TCSI_Gauge-11413000_daily_streamflow.csv", row.names=FALSE)

########## Test plots just for fun

CombinedResults = read.csv("StaticVeg_TCSI_outlets_daily_streamflow.csv")
CombinedResults$Date = as.Date(CombinedResults$Date)

library(ggplot2)

ggplot(data=CombinedResults, aes(x=Date,y=Streamflow_mmd, group=1)) + 
  geom_line(color="deepskyblue2") +
  scale_x_date(breaks=seq(as.Date("2020-01-01"),as.Date("2100-01-01"),by="10 year"),date_labels="%Y")  +
  labs(y="Streamflow, mm/d", title="Streamflow Projections for TCSI\nStatic Vegetation") +
  theme_bw() +
  theme(axis.text.x=element_text(size=18, angle=45, vjust=0.5,color="black"), axis.title.x=element_blank(), axis.title.y=element_text(size=24), axis.text.y=element_text(size=18,color="black"), legend.title=element_blank(), legend.text=element_text(size=24)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,1,0.5,0.5,"cm"),panel.background=element_rect("white", "black"), panel.grid=element_blank()) +
  theme(plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),legend.position="bottom") +
  theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA)) +
  guides(color=guide_legend(override.aes=list(size=3),byrow=TRUE)) +
  facet_grid(cols=vars(Climate),rows=vars(GaugeName))

# FDC

nLen = length(unique(CombinedResults$Date))
QofInterestCNRM = CombinedResults[(CombinedResults$GaugeName=="Yuba" & CombinedResults$Climate=="CNRM-CM5 RCP8.5"),"Streamflow_cms"]
QofInterestMIROC = CombinedResults[(CombinedResults$GaugeName=="Yuba" & CombinedResults$Climate=="MIROC5 RCP8.5"),"Streamflow_cms"]
QofInterestCNRM = rev(sort(QofInterestCNRM))
QofInterestMIROC = rev(sort(QofInterestMIROC))
RankVals = 1:nLen
FreqVals = nLen/RankVals
ExceedanceVals = 1/FreqVals
par(mar=c(5,6,5,3))
plot(ExceedanceVals, QofInterestCNRM, type="l", log="y", lwd=5, col="deepskyblue",
     xlab="Probability of Exceedance", ylab=expression("Streamflow, m"^3*"/s"),
     main="Yuba River Flow Duration Curve, 2020-2100\nStatic Vegetation", cex.main=2, cex.lab=2, cex.axis=1.5)
lines(ExceedanceVals, QofInterestMIROC, lwd=5, col="darkgoldenrod2")
legend("topright", inset=0.1, legend=c("CNRM-CM5 RCP8.5", "MIROC5 RCP8.5"),
       lwd=c(10,10), col=c("deepskyblue","darkgoldenrod2"), cex=1.5)
box(lwd=1)
# End












# 
# 
# 
# ################################################################################
# # Create NetCDF-4 data structure
# 
# # Dimensions
# timeDim = ncdim_def(name="Time", units="Seconds since 1970-01-01 00:00:00",
#                     vals=as.numeric(RecordTimes), unlim=TRUE, calendar="standard")
# 
# gaugeDim = ncdim_def(name="GaugeNumber", units="Local categorical: see GaugeID and GaugeName variables",
#                      vals=1:length(GaugeIDs), unlim=TRUE)
# 
# climateDim = ncdim_def(name="ClimateNumber", units="Local categorical: see ClimateName variable",
#                        vals=1:length(ClimateNames), unlim=TRUE)
# 
# modelDim = ncdim_def(name="DHSVMmodel", units="Categorical: calibrated DHSVM models",
#                      vals=SelectedModel, unlim=TRUE)
# 
# runDim = ncdim_def(name="LANDISrun", units="Categorical: stochastic LANDIS runs",
#                    vals=LANDIsrun, unlim=TRUE)
# 
# # Variables
# streamflowVar = ncvar_def(name="Streamflow", units="m^3/s",
#                           dim=list(timeDim, gaugeDim, climateDim, modelDim, runDim), missval=NA)
# 
# gaugeIDvar = ncvar_def(name="GaugeID", units="USGS or CDEC gauge identifier",
#                        dim=list(gaugeDim), missval=NA)
# 
# gaugeNameVar = ncvar_def(name="GaugeName", units="Human-intelligble gauge name",
#                          dim=list(gaugeDim), missval=NA)
# 
# climateNameVar = ncvar_def(name="ClimateName", units="CMIP-5 GCM name and RCP scenario",
#                            dim=list(climateDim), missval=NA)
# 
# # Create new NetCDF file
# ncNew = nc_create(filename="StreamflowResults_StaticVegetation.nc",
#                   vars=list(streamflowVar, gaugeIDvar, gaugeNameVar, climateNameVar))
# nc_close(ncNew)
# 
# ################################################################################
# # Read data from Streamflow.Only into netCDF
# 
# ncResults = nc_open("StreamflowResults_StaticVegetation.nc", write=TRUE)
# 
# ncvar_put(ncResults, varid=gaugeDim, vals=GaugeIDs)
# 
# 
# 
# 
# scenarioDim = ncdim_def(name="ManagementScenario", units="Categorical: see Forest Service scenario descriptions",
#                         vals=, unlim=TRUE)

