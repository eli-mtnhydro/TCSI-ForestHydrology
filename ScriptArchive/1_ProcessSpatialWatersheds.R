# Visualize streamflow records for HUC12s (includes some other watersheds)

library(raster)
library(trend)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Results"
setwd(dir)

# Setup
SelectedModel = 276
Scenarios = 7
Climates = c("cnrm", "miroc")
ClimateNames = c("CNRM-CM5 RCP 8.5", "MIROC5 RCP 8.5")
LANDIsruns = c(1,4)

# Look-up table of important information
HUCsavePoints = read.csv("TCSI_HUC12savePoints.csv")[,-1]
head(HUCsavePoints)

# Map of enumerated watersheds
WatershedRast = raster("TCSI_HUC12ish_Watersheds.tif")
TahoePtrs = which(WatershedRast[,]==7)

################################################################################
# Aggregate data if necessary
################################################################################

WatershedDataCombined = data.frame()

for (Scenario in Scenarios){
  for (Climate in Climates){
    
    LANDIsrun = LANDIsruns[which(Climates==Climate)]
    ClimateName = ClimateNames[which(Climates==Climate)]
    RunCase = paste0("P",SelectedModel,"S",Scenario,Climate,"R",LANDIsrun)
    
    # Streamflow data
    DHSVMstreamflow = read.csv(paste0(RunCase,"_WSHDsaveMatrix.csv"))
    
    # Results dataframe
    WatershedData = data.frame(WatershedNum=HUCsavePoints$Watershed_Num)
    
    ############################################################################
    # Streamflow in each watershed (absolute m^3/s)
    
    WatershedData$AvgStreamflow_cms = NA
    AvgStreamflowRast = WatershedRast * NA
    
    for (wshd in 1:length(WatershedData$WatershedNum)){
      
      # Calculate average from timeseries
      AvgQ = mean(DHSVMstreamflow[,paste0("WSHD_",wshd)])
      WatershedData$AvgStreamflow_cms[wshd] = AvgQ
      
      # Fill values into raster
      AvgStreamflowRast[WatershedRast[,]==wshd] = AvgQ
      
      #print(wshd)
    }
    
    #plot(AvgStreamflowRast)
    #plot(log(AvgStreamflowRast))
    
    ############################################################################
    # Streamflow generation from each watershed (absolute m^3/s)
    # Subtract upstream contributions
    
    WatershedData$ContribStreamflow_cms = NA
    ContribStreamflowRast = WatershedRast * NA
    
    for (wshd in 1:length(WatershedData$WatershedNum)){
      # Take out upstream watershed(s)
      ContribQ = WatershedData$AvgStreamflow_cms[wshd]
      
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
        ContribQ = ContribQ - WatershedData$AvgStreamflow_cms[inbound]
      }
      
      WatershedData$ContribStreamflow_cms[wshd] = ContribQ
      
      # Fill values into raster
      ContribStreamflowRast[WatershedRast[,]==wshd] = ContribQ
      
      #print(wshd)
    }
    
    #plot(ContribStreamflowRast)
    #plot(log(ContribStreamflowRast))
    
    ############################################################################
    # Streamflow generation from each watershed (relative mm/d)
    # Calculate area and divide
    
    WatershedData$ContribStreamflow_mmd = NA
    WatershedData$Area_km2 = NA
    RelContribStreamflowRast = WatershedRast * NA
    
    for (wshd in 1:length(WatershedData$WatershedNum)){
      # Calculate local watershed area
      WshdArea = length(which(WatershedRast[,]==wshd)) * 90^2 # m^2
      
      ContribQ = WatershedData$ContribStreamflow_cms[wshd] # m^3/s
      ContribQrel = (ContribQ / WshdArea) * 1000 * 60 * 60 * 24 # mm/d
      
      WatershedData$ContribStreamflow_mmd[wshd] = ContribQrel
      WatershedData$Area_km2[wshd] = WshdArea / (1000^2)
      
      # Fill values into raster
      RelContribStreamflowRast[WatershedRast[,]==wshd] = ContribQrel
      
      #print(wshd)
    }
    
    #plot(RelContribStreamflowRast)
    #plot(log(RelContribStreamflowRast))
    
    ############################################################################
    # Add metadata and append to master dataframe
    
    WatershedData$Scenario = rep(Scenario, length(WatershedData$WatershedNum))
    WatershedData$Climate = rep(Climate, length(WatershedData$WatershedNum))
    WatershedData$DHSVMmodel = rep(SelectedModel, length(WatershedData$WatershedNum))
    
    WatershedDataCombined = rbind(WatershedDataCombined, WatershedData)
    
    ############################################################################
    # Save local relative streamflow raster for current scenario/climate
    
    writeRaster(RelContribStreamflowRast, paste0("StreamflowRasters/LocalStreamflowGeneration-mmd_",RunCase,".tif"), overwrite=TRUE)
    
    # Fancier figure
    plot(RelContribStreamflowRast, xlim=c(630000,780000), ylim=c(4270000,4420000), zlim=c(0,5), xaxs="i", yaxs="i",
         main=paste0("Average Local Streamflow Generation, 2020-2099\nScenario ", Scenario,", ",ClimateName),
         xlab="Easting", ylab="Northing",
         legend.args=list(text="Streamflow, mm/d", side=4, font=2, line=2.5, cex=1.5),
         cex.main=1.5)
    
  }
}

PreProcessedWatershedData = read.csv(paste0("CombinedLocalWatershedResults_P",SelectedModel,".csv"))[,-1]

WatershedDataCombined = rbind(PreProcessedWatershedData, WatershedDataCombined)

write.csv(WatershedDataCombined, paste0("CombinedLocalWatershedResults_P",SelectedModel,".csv"))

################################################################################
# Compare relative streamflow contribution by scenario with uncertainty
################################################################################

# For 3 variables
seq1 = c(rep(c(rep(1, 20),rep(2, 20),rep(3, 20)), 37), rep(1, 20), rep(2, 2247-60*37-20))
seq2 = c(rep(c(rep(2, 20),rep(3, 20),rep(1, 20)), 37), rep(2, 20), rep(3, 2247-60*37-20))
seq3 = c(rep(c(rep(3, 20),rep(1, 20),rep(2, 20)), 37), rep(3, 20), rep(1, 2247-60*37-20))
seq4 = c(rep(c(rep(seq1, 20), rep(seq2, 20), rep(seq3, 20)), 23), rep(seq1, 20), rep(seq2, 20), rep(seq3, (1436-60*23-40)))

TestRast = WatershedRast
TestRast[seq4==1] = 1 * TestRast[seq4==1]
TestRast[seq4==2] = 2 * TestRast[seq4==2]
TestRast[seq4==3] = 3 * TestRast[seq4==3]
plot(TestRast)

pal = colorRampPalette(c("darkred","red","darkgoldenrod2","lightgoldenrod","gray90","skyblue","dodgerblue","blue","darkorchid","magenta"))
par(mar=c(5,5,5,5))

########## Ratio

# CNRM
ClimateName = "CNRM-CM5 RCP 8.5"

RelContribStreamflowRast2P195 = raster("StreamflowRasters/LocalStreamflowGeneration-mmd_P195S2cnrmR1.tif")
RelContribStreamflowRast2P270 = raster("StreamflowRasters/LocalStreamflowGeneration-mmd_P270S2cnrmR1.tif")
RelContribStreamflowRast2P276 = raster("StreamflowRasters/LocalStreamflowGeneration-mmd_P276S2cnrmR1.tif")

for (SC in c(4,6,7)){
  
  RelContribStreamflowRastXP195 = raster(paste0("StreamflowRasters/LocalStreamflowGeneration-mmd_P195S",SC,"cnrmR1.tif"))
  RelContribStreamflowRastXP270 = raster(paste0("StreamflowRasters/LocalStreamflowGeneration-mmd_P270S",SC,"cnrmR1.tif"))
  RelContribStreamflowRastXP276 = raster(paste0("StreamflowRasters/LocalStreamflowGeneration-mmd_P276S",SC,"cnrmR1.tif"))
  
  RelDiffQP270 = RelContribStreamflowRastXP270 / RelContribStreamflowRast2P270
  RelDiffQP276 = RelContribStreamflowRastXP276 / RelContribStreamflowRast2P276
  
  RelDiffQcombined = RelContribStreamflowRastXP195 / RelContribStreamflowRast2P195
  RelDiffQcombined[seq4==2] = RelDiffQP270[seq4==2]
  RelDiffQcombined[seq4==3] = RelDiffQP276[seq4==3]
  
  plot(RelDiffQcombined, xlim=c(630000,780000), ylim=c(4270000,4420000), xaxs="i", yaxs="i",
       zlim=c(0.8,1.25), col=pal(1000),
       main=paste0("Relative Local Streamflow Generation, 2015-2099\nScenario ", SC," / Scenario 2\n",ClimateName,", DHSVM Models 195, 270, 276"),
       xlab="Easting", ylab="Northing", legend.width=2,
       legend.args=list(text=paste0("Streamflow, S",SC," / S2, mm / d"), side=2, font=2, line=1.5, cex=1.5),
       cex.main=1.5)
}

# MIROC
ClimateName = "MIROC5 RCP 8.5"

RelContribStreamflowRast2P195 = raster("StreamflowRasters/LocalStreamflowGeneration-mmd_P195S2mirocR4.tif")
RelContribStreamflowRast2P270 = raster("StreamflowRasters/LocalStreamflowGeneration-mmd_P270S2mirocR4.tif")
RelContribStreamflowRast2P276 = raster("StreamflowRasters/LocalStreamflowGeneration-mmd_P276S2mirocR4.tif")

for (SC in c(4,6,7)){
  
  RelContribStreamflowRastXP195 = raster(paste0("StreamflowRasters/LocalStreamflowGeneration-mmd_P195S",SC,"mirocR4.tif"))
  RelContribStreamflowRastXP270 = raster(paste0("StreamflowRasters/LocalStreamflowGeneration-mmd_P270S",SC,"mirocR4.tif"))
  RelContribStreamflowRastXP276 = raster(paste0("StreamflowRasters/LocalStreamflowGeneration-mmd_P276S",SC,"mirocR4.tif"))
  
  RelDiffQP270 = RelContribStreamflowRastXP270 / RelContribStreamflowRast2P270
  RelDiffQP276 = RelContribStreamflowRastXP276 / RelContribStreamflowRast2P276
  
  RelDiffQcombined = RelContribStreamflowRastXP195 / RelContribStreamflowRast2P195
  RelDiffQcombined[seq4==2] = RelDiffQP270[seq4==2]
  RelDiffQcombined[seq4==3] = RelDiffQP276[seq4==3]
  
  plot(RelDiffQcombined, xlim=c(630000,780000), ylim=c(4270000,4420000), xaxs="i", yaxs="i",
       zlim=c(0.8,1.25), col=pal(1000),
       main=paste0("Relative Local Streamflow Generation, 2015-2099\nScenario ", SC," / Scenario 2\n",ClimateName,", DHSVM Models 195, 270, 276"),
       xlab="Easting", ylab="Northing", legend.width=2,
       legend.args=list(text=paste0("Streamflow, S",SC," / S2, mm / d"), side=2, font=2, line=1.5, cex=1.5),
       cex.main=1.5)
}

################################################################################
# Compare Sen's slope of yearly peak flows by scenario / climate
################################################################################

WaterYears = 2015:2099
DatesOnly = seq(as.Date(paste0("10-01-",WaterYears[1]-1), format="%m-%d-%Y"),
                as.Date(paste0("09-30-",rev(WaterYears)[1]), format="%m-%d-%Y"),
                by="days")

################################################################################
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

################################################################################
# Peak flow stats by watershed

SelectedModels = c(195,270,276)

for (SelectedModel in SelectedModels){
  for (Scenario in Scenarios){
    for (Climate in Climates){
      
      LANDIsrun = LANDIsruns[which(Climates==Climate)]
      ClimateName = ClimateNames[which(Climates==Climate)]
      RunCase = paste0("P",SelectedModel,"S",Scenario,Climate,"R",LANDIsrun)
      
      # Streamflow data
      DHSVMstreamflow = read.csv(paste0(RunCase,"_WSHDsaveMatrix.csv"))
      
      # # Local daily streamflow data
      # LocalStreamflow_mmd = data.frame(matrix(nrow=length(DatesOnly),ncol=length(HUCsavePoints$Watershed_Num)+1))
      # names(LocalStreamflow_mmd) = c("Date", paste0("WSHD_",HUCsavePoints$Watershed_Num))
      # LocalStreamflow_mmd$Date = DatesOnly
      
      # Results dataframe
      WatershedData = data.frame(WatershedNum=HUCsavePoints$Watershed_Num)
      WatershedData$YrPkSensSlopeMean_cmsYr = NA
      WatershedData$YrPkSensSlopeLower95_cmsYr = NA
      WatershedData$YrPkSensSlopeUpper95_cmsYr = NA
      WatershedData$YrPkSensSlopeMean_mmdYr = NA
      WatershedData$YrPkSensSlopeLower95_mmdYr = NA
      WatershedData$YrPkSensSlopeUpper95_mmdYr = NA
      
      # Results rasters
      SensSlope_cms_MeanRast = WatershedRast * NA
      SensSlope_cms_Lower95Rast = WatershedRast * NA
      SensSlope_cms_Upper95Rast = WatershedRast * NA
      SensSlope_mmd_MeanRast = WatershedRast * NA
      SensSlope_mmd_Lower95Rast = WatershedRast * NA
      SensSlope_mmd_Upper95Rast = WatershedRast * NA
      
      ############################################################################
      # Create timeseries of yearly 1-day peak
      
      for (wshd in 1:length(WatershedData$WatershedNum)){
        
        # Aggregate 3-hourly to daily
        DailyTimeseries = colSums(matrix(DHSVMstreamflow[,paste0("WSHD_",wshd)], nrow=8)) / 8
        
        ########## Taking out upstream watersheds results in timing mismatch--negative streamflow!
        
        # # Take out upstream watershed(s)
        # InboundWshds = c(HUCsavePoints$Inbound_1[wshd],
        #                  HUCsavePoints$Inbound_2[wshd],
        #                  HUCsavePoints$Inbound_3[wshd],
        #                  HUCsavePoints$Inbound_4[wshd])
        # 
        # InboundWshds = InboundWshds[is.finite(InboundWshds)]
        # 
        # # Tahoe gets special treatment because it has way too many inbound watersheds
        # if (wshd==7){
        #   InboundWshds = c(8,30,9,10,29,28,11,26,24,12,23,22,21,31)
        # }
        # 
        # for (inbound in InboundWshds){
        #   InboundDailyTimeseries = colSums(matrix(DHSVMstreamflow[,paste0("WSHD_",inbound)], nrow=8)) / 8
        #   DailyTimeseries = DailyTimeseries - InboundDailyTimeseries
        # }
        # 
        # plot(DailyTimeseries,type="l")
        
        YearlyPeak = c()
        for (yr in WaterYears){
          YearPtrs = which(DatesOnly >= as.Date(paste0("10-01-",yr-1), format="%m-%d-%Y") &
                             DatesOnly < as.Date(paste0("10-01-",yr), format="%m-%d-%Y"))
          YearlyPeak = c(YearlyPeak, max(DailyTimeseries[YearPtrs]))
        }
        
        # Find Sen's slope of yearly peak
        SensSlope = sens.slope(YearlyPeak)
        
        # Fill into dataframes, cms
        WatershedData$YrPkSensSlopeMean_cmsYr[wshd] = SensSlope$estimates
        WatershedData$YrPkSensSlopeLower95_cmsYr[wshd] = SensSlope$conf.int[1]
        WatershedData$YrPkSensSlopeUpper95_cmsYr[wshd] = SensSlope$conf.int[2]
        
        # Fill into rasters, cms
        # SensSlope_cms_MeanRast[WatershedRast[,]==wshd] = SensSlope$estimates
        # SensSlope_cms_Lower95Rast[WatershedRast[,]==wshd] = SensSlope$conf.int[1]
        # SensSlope_cms_Upper95Rast[WatershedRast[,]==wshd] = SensSlope$conf.int[2]
        
        # Calculate <TOTAL UPSTREAM> area and divide
        TotalWatershedArea = WatershedArea(wshd)
        
        # Fill into dataframes, mmd
        WatershedData$YrPkSensSlopeMean_mmdYr[wshd] = SensSlope$estimates * 1000 * 60 * 60 * 24 / TotalWatershedArea
        WatershedData$YrPkSensSlopeLower95_mmdYr[wshd] = SensSlope$conf.int[1] * 1000 * 60 * 60 * 24 / TotalWatershedArea
        WatershedData$YrPkSensSlopeUpper95_mmdYr[wshd] = SensSlope$conf.int[2] * 1000 * 60 * 60 * 24 / TotalWatershedArea
        
        # Fill into rasters, mmd
        SensSlope_mmd_MeanRast[WatershedRast[,]==wshd] = SensSlope$estimates * 1000 * 60 * 60 * 24 / TotalWatershedArea
        SensSlope_mmd_Lower95Rast[WatershedRast[,]==wshd] = SensSlope$conf.int[1] * 1000 * 60 * 60 * 24 / TotalWatershedArea
        SensSlope_mmd_Upper95Rast[WatershedRast[,]==wshd] = SensSlope$conf.int[2] * 1000 * 60 * 60 * 24 / TotalWatershedArea
        
        print(wshd)
      }
      
      plot(SensSlope_mmd_MeanRast)
      #plot(log(SensSlope_mmd_MeanRast))
      
      ############################################################################
      # Save peak flow Sen's slope rasters for current scenario/climate
      
      writeRaster(SensSlope_mmd_MeanRast, paste0("StreamflowRasters/Yearly1-dayPeakSensSlope_mmd_",RunCase,".tif"), overwrite=TRUE)
      writeRaster(SensSlope_mmd_Lower95Rast, paste0("StreamflowRasters/Yearly1-dayPeakSensSlopeLower95_mmd_",RunCase,".tif"), overwrite=TRUE)
      writeRaster(SensSlope_mmd_Upper95Rast, paste0("StreamflowRasters/Yearly1-dayPeakSensSlopeUpper95_mmd_",RunCase,".tif"), overwrite=TRUE)
      
    }
  }
}

################################################################################
# Plot Sen's slope

########## Simple (actual Sen's slope value)

pal = colorRampPalette(c("darkgoldenrod2","gray90","skyblue","dodgerblue","blue","darkorchid","magenta"))
par(mar=c(5,5,5,5))

# CNRM
ClimateName = "CNRM-CM5 RCP 8.5"

for (SC in 1:7){
  # PeakRastXlower95 = raster(paste0("StreamflowRasters/Yearly1-dayPeakSensSlopeLower95_mmd_P276S",SC,"cnrmR1.tif"))
  # PeakRastXupper95 = raster(paste0("StreamflowRasters/Yearly1-dayPeakSensSlopeUpper95_mmd_P276S",SC,"cnrmR1.tif"))
  # 
  # SensRast = PeakRastXlower95
  # SensRast[as.logical(seq3)] = PeakRastXupper95[as.logical(seq3)]
  
  SensRast = raster(paste0("StreamflowRasters/Yearly1-dayPeakSensSlope_mmd_P",SelectedModel,"S",SC,"cnrmR1.tif"))
  
  plot(SensRast, xlim=c(630000,780000), ylim=c(4270000,4420000), xaxs="i", yaxs="i",
       zlim=c(-0.2, 1), col=pal(1000),
       main=paste0("Sen's Slope for Yearly Peak Daily Flows, 2015-2099\nScenario ", SC,", ",ClimateName),
       xlab="Easting", ylab="Northing", legend.width=2,
       legend.args=list(text=paste0("Streamflow, mm / d / yr"), side=2, font=2, line=1.5, cex=1.5),
       cex.main=1.5)
}

# MIROC
ClimateName = "MIROC5 RCP 8.5"

for (SC in 1:7){
  # PeakRastXlower95 = raster(paste0("StreamflowRasters/Yearly1-dayPeakSensSlopeLower95_mmd_P276S",SC,"mirocR4.tif"))
  # PeakRastXupper95 = raster(paste0("StreamflowRasters/Yearly1-dayPeakSensSlopeUpper95_mmd_P276S",SC,"mirocR4.tif"))
  # 
  # SensRast = PeakRastXlower95
  # SensRast[as.logical(seq3)] = PeakRastXupper95[as.logical(seq3)]
  
  SensRast = raster(paste0("StreamflowRasters/Yearly1-dayPeakSensSlope_mmd_P",SelectedModel,"S",SC,"mirocR4.tif"))
  
  plot(SensRast, xlim=c(630000,780000), ylim=c(4270000,4420000), xaxs="i", yaxs="i",
       zlim=c(-0.2, 1), col=pal(1000),
       main=paste0("Sen's Slope for Yearly Peak Daily Flows, 2015-2099\nScenario ", SC,", ",ClimateName),
       xlab="Easting", ylab="Northing", legend.width=2,
       legend.args=list(text=paste0("Streamflow, mm / d / yr"), side=2, font=2, line=1.5, cex=1.5),
       cex.main=1.5)
}

################################################################################
# Compare Sen's slope by scenario with uncertainty
################################################################################

# For 3 variables
seq1 = c(rep(c(rep(1, 20),rep(2, 20),rep(3, 20)), 37), rep(1, 20), rep(2, 2247-60*37-20))
seq2 = c(rep(c(rep(2, 20),rep(3, 20),rep(1, 20)), 37), rep(2, 20), rep(3, 2247-60*37-20))
seq3 = c(rep(c(rep(3, 20),rep(1, 20),rep(2, 20)), 37), rep(3, 20), rep(1, 2247-60*37-20))
seq4 = c(rep(c(rep(seq1, 20), rep(seq2, 20), rep(seq3, 20)), 23), rep(seq1, 20), rep(seq2, 20), rep(seq3, (1436-60*23-40)))

TestRast = WatershedRast
TestRast[seq4==1] = 1 * TestRast[seq4==1]
TestRast[seq4==2] = 2 * TestRast[seq4==2]
TestRast[seq4==3] = 3 * TestRast[seq4==3]
plot(TestRast)

pal = colorRampPalette(c("red","darkgoldenrod2","gray90","dodgerblue","blue","darkorchid","magenta"))
par(mar=c(5,5,5,5))

########## Difference

# CNRM
ClimateName = "CNRM-CM5 RCP 8.5"

PeakRast2P195 = raster("StreamflowRasters/Yearly1-dayPeakSensSlope_mmd_P195S2cnrmR1.tif")
PeakRast2P270 = raster("StreamflowRasters/Yearly1-dayPeakSensSlope_mmd_P270S2cnrmR1.tif")
PeakRast2P276 = raster("StreamflowRasters/Yearly1-dayPeakSensSlope_mmd_P276S2cnrmR1.tif")

for (SC in c(4,6,7)){
  PeakRastXP195 = raster(paste0("StreamflowRasters/Yearly1-dayPeakSensSlope_mmd_P195S",SC,"cnrmR1.tif"))
  PeakRastXP270 = raster(paste0("StreamflowRasters/Yearly1-dayPeakSensSlope_mmd_P270S",SC,"cnrmR1.tif"))
  PeakRastXP276 = raster(paste0("StreamflowRasters/Yearly1-dayPeakSensSlope_mmd_P276S",SC,"cnrmR1.tif"))
  
  DiffRast = PeakRastXP195 - PeakRast2P195
  DiffRast[seq4==2] = (PeakRastXP270 - PeakRast2P270)[seq4==2]
  DiffRast[seq4==3] = (PeakRastXP276 - PeakRast2P276)[seq4==3]
  
  plot(DiffRast, xlim=c(630000,780000), ylim=c(4270000,4420000), xaxs="i", yaxs="i",
       zlim=c(-0.1,0.2), col=pal(1000),
       main=paste0("Sen's Slope for Yearly Peak Daily Flows, 2015-2099\nScenario ", SC," - Scenario 2\n",ClimateName,", DHSVM Models 195, 270, 276"),
       xlab="Easting", ylab="Northing", legend.width=2,
       legend.args=list(text=paste0("Yearly Peak Streamflow, mm / d / yr"), side=2, font=2, line=1.5, cex=1.5),
       cex.main=1.5)
  
  print(max(DiffRast[,], na.rm=TRUE))
  print(min(DiffRast[,], na.rm=TRUE))
}

# MIROC
ClimateName = "MIROC5 RCP 8.5"

PeakRast2P195 = raster("StreamflowRasters/Yearly1-dayPeakSensSlope_mmd_P195S2mirocR4.tif")
PeakRast2P270 = raster("StreamflowRasters/Yearly1-dayPeakSensSlope_mmd_P270S2mirocR4.tif")
PeakRast2P276 = raster("StreamflowRasters/Yearly1-dayPeakSensSlope_mmd_P276S2mirocR4.tif")

for (SC in c(4,6,7)){
  PeakRastXP195 = raster(paste0("StreamflowRasters/Yearly1-dayPeakSensSlope_mmd_P195S",SC,"mirocR4.tif"))
  PeakRastXP270 = raster(paste0("StreamflowRasters/Yearly1-dayPeakSensSlope_mmd_P270S",SC,"mirocR4.tif"))
  PeakRastXP276 = raster(paste0("StreamflowRasters/Yearly1-dayPeakSensSlope_mmd_P276S",SC,"mirocR4.tif"))
  
  DiffRast = PeakRastXP195 - PeakRast2P195
  DiffRast[seq4==2] = (PeakRastXP270 - PeakRast2P270)[seq4==2]
  DiffRast[seq4==3] = (PeakRastXP276 - PeakRast2P276)[seq4==3]
  
  plot(DiffRast, xlim=c(630000,780000), ylim=c(4270000,4420000), xaxs="i", yaxs="i",
       zlim=c(-0.1,0.2), col=pal(1000),
       main=paste0("Sen's Slope for Yearly Peak Daily Flows, 2015-2099\nScenario ", SC," - Scenario 2\n",ClimateName,", DHSVM Models 195, 270, 276"),
       xlab="Easting", ylab="Northing", legend.width=2,
       legend.args=list(text=paste0("Yearly Peak Streamflow, mm / d / yr"), side=2, font=2, line=1.5, cex=1.5),
       cex.main=1.5)
  
  print(max(DiffRast[,], na.rm=TRUE))
  print(min(DiffRast[,], na.rm=TRUE))
}




