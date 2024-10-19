
library(terra)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Reservoir_FDCs"
setwd(dir)

# Look-up table of important information
HUCsavePoints = read.csv("TCSI_HUC12savePoints.csv")[,-1]
head(HUCsavePoints)

# Map of enumerated watersheds
WatershedRast = rast("TCSI_HUC12ish_Watersheds.tif")

##########
# Choose reservoir pour points

# New Bullards Bar: ID 57, HUC12 180201250405
# Folsom Lake: ID 3, (American Outlet)

ReservoirWatersheds = c(57,3)
ReservoirNames = c("New Bullards Bar Reservoir","Folsom Lake")

##########
# Make map of upstream area to confirm

MapUpstreamWshds = function(wshd){
  
  InboundWshds = c(HUCsavePoints$Inbound_1[wshd],
                   HUCsavePoints$Inbound_2[wshd],
                   HUCsavePoints$Inbound_3[wshd],
                   HUCsavePoints$Inbound_4[wshd])
  
  InboundWshds = InboundWshds[is.finite(InboundWshds)]
  
  # Tahoe gets special treatment because it has way too many inbound watersheds
  if (wshd==7){
    InboundWshds = c(8,30,9,10,29,28,11,26,24,12,23,22,21,31)
  }
  
  for (inbound in InboundWshds){
    InboundWshds = c(InboundWshds, MapUpstreamWshds(inbound))
  }
  
  return(unique(c(wshd,InboundWshds)))
}

ReservoirWshds = MapUpstreamWshds(57)
ReservoirMap = 0*WatershedRast
ReservoirMap[which(WatershedRast[,] %in% ReservoirWshds)] = 1
plot(ReservoirMap)

ReservoirWshds = MapUpstreamWshds(3)
ReservoirMap = 0*WatershedRast
ReservoirMap[which(WatershedRast[,] %in% ReservoirWshds)] = 1
plot(ReservoirMap)

################################################################################
# Process daily data

###########
# Calculate daily average streamflow for each watershed in each run case

DHSVMmodels = c(195,270,276)
Scenarios = 1:6
Climates = c("cnrm", "miroc")
ClimateNames = c("CNRM-CM5 RCP 8.5", "MIROC5 RCP 8.5")
LANDIsruns = c(1,4)

DailyStreamData = data.frame()

for (DHSVMmodel in DHSVMmodels){
  for (Scenario in Scenarios){
    for (Climate in Climates){
      
      LANDIsrun = LANDIsruns[which(Climates==Climate)]
      ClimateName = ClimateNames[which(Climates==Climate)]
      RunCase = paste0("P",DHSVMmodel,"S",Scenario,Climate,"R",LANDIsrun)
      
      # Load streamflow data
      DHSVMstreamflow = read.csv(paste0("../../../TCSI_Results/",RunCase,"_WSHDsaveMatrix.csv"))
      
      Dates = as.Date(matrix(DHSVMstreamflow$DateTime, nrow=8)[1,])
      
      for (wshd in ReservoirWatersheds){
        
        # Aggregate 3-hourly to daily
        DailyTimeseries = colSums(matrix(DHSVMstreamflow[,paste0("WSHD_",wshd)], nrow=8)) / 8
        
        DailyStreamData = rbind(DailyStreamData,
                                data.frame(Reservoir=ReservoirNames[which(ReservoirWatersheds==wshd)],
                                           WatershedNum=wshd,
                                           DHSVMmodel=DHSVMmodel,
                                           Scenario=Scenario,
                                           Climate=ClimateName,
                                           Date=Dates,
                                           DailyAvgQ_cms=DailyTimeseries))
        
        plot(Dates,DailyTimeseries,type="l")
      }
      print(paste("Done with",Climate,Scenario,DHSVMmodel))
    }
  }
}

write.csv(DailyStreamData,"Reservoirs_DailyStreamflowTimeseries.csv")

################################################################################
# Calculate daily FDCs, averaging across DHSVM models

FlowDurationData = data.frame()

for (Scenario in Scenarios){
  for (ClimateName in ClimateNames){
    for (wshd in ReservoirWatersheds){
      
      Ptrs = which(DailyStreamData$Scenario==Scenario &
                     DailyStreamData$Climate==ClimateName &
                     DailyStreamData$WatershedNum==wshd)
      
      # Average across DHSVM models
      HydrographAvg = rowMeans(matrix(DailyStreamData$DailyAvgQ_cms[Ptrs], ncol=length(DHSVMmodels)))
      
      SortedDailyFlows = rev(sort(HydrographAvg))
      
      ExceedanceVals = 1:length(SortedDailyFlows) / (length(SortedDailyFlows) + 1)
      
      FlowDurationData = rbind(FlowDurationData,
                              data.frame(Reservoir=ReservoirNames[which(ReservoirWatersheds==wshd)],
                                         WatershedNum=wshd,
                                         Scenario=Scenario,
                                         Climate=ClimateName,
                                         ExceedanceProbability=ExceedanceVals,
                                         DailyFlow_cms=SortedDailyFlows))
      
      plot(ExceedanceVals, SortedDailyFlows, log="x")
      print(paste("Done with",ClimateName,Scenario,wshd))
    }
  }
}

##########
# Area normalization

# Function to calculate total upstream area
WatershedArea = function(wshd){
  # Calculate local area
  LocalArea = length(which(WatershedRast[,]==wshd)) * 90^2 # m^2
  
  # Add area of all upstream watersheds recursively
  UpstreamArea = 0
  
  InboundWshds = c(HUCsavePoints$Inbound_1[wshd],
                   HUCsavePoints$Inbound_2[wshd],
                   HUCsavePoints$Inbound_3[wshd],
                   HUCsavePoints$Inbound_4[wshd])
  
  InboundWshds = InboundWshds[is.finite(InboundWshds)]
  
  # Tahoe gets special treatment because it has way too many inbound watersheds
  if (wshd==7){
    InboundWshds = c(8,30,9,10,29,28,11,26,24,12,23,22,21,31)
  }
  
  for (inbound in InboundWshds){
    UpstreamArea = UpstreamArea + WatershedArea(inbound)
  }
  
  TotalArea = LocalArea + UpstreamArea
  return(TotalArea)
}

FlowDurationData$DailyFlow_mmd = NA

for (wshd in ReservoirWatersheds){
  
  Area = WatershedArea(wshd)
  Ptrs = which(FlowDurationData$WatershedNum==wshd)
  FlowDurationData$DailyFlow_mmd[Ptrs] = 1000 * FlowDurationData$DailyFlow_cms[Ptrs] * (60*60*24) / Area
}

write.csv(FlowDurationData,"FlowDurationData.csv")
