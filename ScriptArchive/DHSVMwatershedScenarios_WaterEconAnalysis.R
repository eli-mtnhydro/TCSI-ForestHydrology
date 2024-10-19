
library(data.table)
library(raster)
library(ggplot2)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Results"
setwd(dir)

# Setup of DHSVM runs
DHSVMmodels = c(195,270,276)
Scenarios = 1:6
Climates = c("cnrm","miroc")
ClimateNames = c("CNRM-CM5 RCP 8.5", "MIROC5 RCP 8.5")
LANDISruns = c(1,4)

# Model time duration information
WaterYears = 2015:2099
DatesOnly = seq(as.Date(paste0("10-01-",WaterYears[1]-1), format="%m-%d-%Y"),
                as.Date(paste0("09-30-",rev(WaterYears)[1]), format="%m-%d-%Y"),
                by="days")
nRunoffDays = length(DatesOnly)

# Look-up table of important information
HUCsavePoints = read.csv("TCSI_HUC12savePoints.csv")[,-1]
head(HUCsavePoints)

# Map of enumerated watersheds
WatershedRast = raster("TCSI_HUC12ish_Watersheds.tif")
plot(WatershedRast)

################################################################################

# Create raster of area values for each HUC12
WatershedAreaRast = WatershedRast * NA
for (wshd in HUCsavePoints$Watershed_Num){
  
  Ptrs = which(WatershedRast[,]==wshd)
  
  WshdArea = ((length(Ptrs) * 90^2) / 4046.856) / 1000 # cells --> m^2 --> acres x1000
  
  WatershedAreaRast[Ptrs] = WshdArea
  print(WshdArea)
}
plot(WatershedAreaRast)

for (Climate in Climates){
  for (Scenario in Scenarios){
    
    # Reset map
    RelContribStreamflowRast = WatershedRast * 0
    
    for (DHSVMmodel in DHSVMmodels){
      
      LANDISrun = LANDISruns[which(Climates==Climate)]
      RunCase = paste0("P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun)
      
      # Add map from current model
      RelContribStreamflowRast = RelContribStreamflowRast + raster(paste0("LocalStreamflowGeneration-mmd_",RunCase,".tif"))
    }
    
    # Average maps from all DHSVM models
    RelContribStreamflowRast = RelContribStreamflowRast / length(DHSVMmodels)
    
    # Convert mm/d to thousand acre-ft. per year
    RelContribStreamflowRast = (RelContribStreamflowRast / 1000) * 3.28084 # mm/d --> m/d --> ft./d
    RelContribStreamflowRast = RelContribStreamflowRast * nRunoffDays # ft./d --> ft. cumulative
    RelContribStreamflowRast = RelContribStreamflowRast * WatershedAreaRast # ft. --> thousand acre-ft. (TAF)
    RelContribStreamflowRast = RelContribStreamflowRast / length(WaterYears) # TAF --> TAF per year (avg.)
    
    writeRaster(RelContribStreamflowRast, paste0("LocalStreamflowGeneration-TAFperYr_S",Scenario,Climate,".tif"), overwrite=TRUE)
    print(paste(Climate,Scenario))
  }
}

################################################################################
# Difference relative to Scenario 2

pal = colorRampPalette(c("red","darkgoldenrod2","gray90","skyblue","dodgerblue","blue","darkorchid","magenta"))
par(mar=c(5,5,5,5))

for (Climate in Climates){
  
  ClimateName = ClimateNames[which(Climates==Climate)]
  
  # Business-as-usual baseline
  RunoffRastS2 = raster(paste0("LocalStreamflowGeneration-TAFperYr_S2",Climate,".tif"))
  
  for (Scenario in Scenarios){
    
    RunoffRast = raster(paste0("LocalStreamflowGeneration-TAFperYr_S",Scenario,Climate,".tif"))
    DiffRast = RunoffRast - RunoffRastS2
    
    plot(DiffRast, xlim=c(630000,780000), ylim=c(4270000,4420000), xaxs="i", yaxs="i",
         col=pal(1000), zlim=c(-6,15),
         main=paste0("Local Runoff Generation, 2015-2099\nScenario ", Scenario," - Scenario 2, ",ClimateName),
         xlab="Easting", ylab="Northing", legend.width=2,
         legend.args=list(text="TAF / yr.", side=2, font=2, line=1.5, cex=1.5),
         cex.main=1.5)
    
  }
}

################################################################################
# Watersheds above New Bullards Bar
NBBwshds = c(13, 14, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56)
NBBptrs = which(WatershedRast[,] %in% NBBwshds)
NBBrast = WatershedRast
NBBrast[NBBptrs] = 200
plot(NBBrast)

Climate = "miroc"
Scenario = 6

ClimateName = ClimateNames[which(Climates==Climate)]
RunoffRastS2 = raster(paste0("LocalStreamflowGeneration-TAFperYr_S2",Climate,".tif"))
RunoffRast = raster(paste0("LocalStreamflowGeneration-TAFperYr_S",Scenario,Climate,".tif"))
DiffRast = RunoffRast - RunoffRastS2

# Total extra inflow to New Bullards Bar
sum(unique(DiffRast[NBBptrs]))
90000 / 969600

################################################################################
# Create CSV of yearly HUC12 water yield data

WatershedDataCombined = data.frame()

for (Climate in Climates){
  
  LANDISrun = LANDISruns[which(Climates==Climate)]
  ClimateName = ClimateNames[which(Climates==Climate)]
  
  for (Scenario in Scenarios){
    for (DHSVMmodel in DHSVMmodels){
      
      RunCase = paste0("P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun)
      
      # Streamflow data
      #DHSVMstreamflow = read.csv(paste0(RunCase,"_WSHDsaveMatrix.csv"))
      DHSVMstreamflow = as.data.frame(fread(paste0(RunCase,"_WSHDsaveMatrix.csv")))[,-1]
      
      # Add water year
      DHSVMstreamflow$DateTime = as.Date(DHSVMstreamflow$DateTime) # Loses hours, but only care about years anyway
      DHSVMstreamflow$WaterYear = as.numeric(format(DHSVMstreamflow$DateTime,"%Y"))
      OctNovDecPtrs = which(as.numeric(format(DHSVMstreamflow$DateTime,"%m")) >= 10)
      DHSVMstreamflow$WaterYear[OctNovDecPtrs] = DHSVMstreamflow$WaterYear[OctNovDecPtrs] + 1
      
      for (WaterYear in WaterYears){
        
        WaterYearPtrs = which(DHSVMstreamflow$WaterYear == WaterYear)
        
        # Results dataframe
        WatershedDataChunk = data.frame(WatershedNum=HUCsavePoints$Watershed_Num)
        WatershedDataChunk$WaterYear = WaterYear
        WatershedDataChunk$Climate = ClimateName
        WatershedDataChunk$Scenario = Scenario
        WatershedDataChunk$DHSVMmodel = DHSVMmodel
        WatershedDataChunk$LANDISrun = LANDISrun
        WatershedDataChunk$AvgStreamflow_cms = NA
        WatershedDataChunk$ContribStreamflow_cms = NA
        
        for (wshd in 1:length(WatershedDataChunk$WatershedNum)){
          
          ######################################################################
          # Streamflow in each watershed (absolute m^3/s)
          #plot(DHSVMstreamflow[WaterYearPtrs,paste0("WSHD_",wshd)], type="l")
          # Water year average
          AvgQ = mean(DHSVMstreamflow[WaterYearPtrs,paste0("WSHD_",wshd)])
          WatershedDataChunk$AvgStreamflow_cms[wshd] = AvgQ
          
        } # End 1st loop over watersheds
        
        for (wshd in 1:length(WatershedDataChunk$WatershedNum)){
          
          ######################################################################
          # Streamflow generation from each watershed (absolute m^3/s)
          # Subtract upstream contributions
          ContribQ = WatershedDataChunk$AvgStreamflow_cms[wshd]
          
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
            ContribQ = ContribQ - WatershedDataChunk$AvgStreamflow_cms[inbound]
          }
          
          WatershedDataChunk$ContribStreamflow_cms[wshd] = ContribQ
        } # End 2nd loop over watersheds
        
        WatershedDataCombined = rbind(WatershedDataCombined, WatershedDataChunk)
      } # End loop over water years
      
      print(paste(Climate,Scenario,DHSVMmodel))
    }
  }
}

########### Convert units to TAF

# Get length of each water year to sum over 365 or 366 days for total yield
WaterYearStart = as.Date(paste0(WatershedDataCombined$WaterYear-1,"-10-01"))
WaterYearEnd = as.Date(paste0(WatershedDataCombined$WaterYear,"-09-30"))
DaysInWaterYear = as.numeric(difftime(WaterYearEnd, WaterYearStart, units="days") + 1)

# m^3/s --> m^3 (total volume) --> ft^3 --> ft * acre --> thousand acre-ft. (TAF)
ConversionFactors = (DaysInWaterYear * 24 * 60 * 60) * (3.28084 ^ 3) * (1 / 43560) * (1 / 1000)
WatershedDataCombined$AvgStreamflow_TAF = WatershedDataCombined$AvgStreamflow_cms * ConversionFactors
WatershedDataCombined$ContribStreamflow_TAF = WatershedDataCombined$ContribStreamflow_cms * ConversionFactors

write.csv(WatershedDataCombined, "HUC12_YearlyStreamflow_AllBasinsModelsClimatesScenarios.csv")

WatershedDataCombined = read.csv("HUC12_YearlyStreamflow_AllBasinsModelsClimatesScenarios.csv")[,-1]

################################################################################
# Average across DHSVM models

WatershedDataSimplified = WatershedDataCombined[WatershedDataCombined$DHSVMmodel==DHSVMmodels[1],]
WatershedDataSimplified = subset(WatershedDataSimplified, select=-c(7,8))
WatershedDataSimplified$AvgStreamflow_TAF = 0
WatershedDataSimplified$ContribStreamflow_TAF = 0

nModels = length(DHSVMmodels)
WatershedDataSimplified$DHSVMmodel = "Avg."

for (DHSVMmodel in DHSVMmodels){
  
  Ptrs = which(WatershedDataCombined$DHSVMmodel==DHSVMmodel)
  
  WatershedDataSimplified$AvgStreamflow_TAF = WatershedDataSimplified$AvgStreamflow_TAF + WatershedDataCombined$AvgStreamflow_TAF[Ptrs] / nModels
  WatershedDataSimplified$ContribStreamflow_TAF = WatershedDataSimplified$ContribStreamflow_TAF + WatershedDataCombined$ContribStreamflow_TAF[Ptrs] / nModels
  
}

# Relative to business-as-usual (Scenario 2)

WatershedDataSimplified$AvgStreamflow_RelS2_TAF = NA
WatershedDataSimplified$ContribStreamflow_RelS2_TAF = NA

for (wshd in HUCsavePoints$Watershed_Num){
  for (WaterYear in WaterYears){
    for (Climate in ClimateNames){
      
      S2ptr = which(WatershedDataSimplified$WatershedNum==wshd &
                      WatershedDataSimplified$WaterYear==WaterYear &
                      WatershedDataSimplified$Climate==Climate &
                      WatershedDataSimplified$Scenario==2)
      
      for (Scenario in Scenarios){
        
        Ptr = which(WatershedDataSimplified$WatershedNum==wshd &
                      WatershedDataSimplified$WaterYear==WaterYear &
                      WatershedDataSimplified$Climate==Climate &
                      WatershedDataSimplified$Scenario==Scenario)
        
        WatershedDataSimplified$AvgStreamflow_RelS2_TAF[Ptr] = WatershedDataSimplified$AvgStreamflow_TAF[Ptr] - WatershedDataSimplified$AvgStreamflow_TAF[S2ptr]
        WatershedDataSimplified$ContribStreamflow_RelS2_TAF[Ptr] = WatershedDataSimplified$ContribStreamflow_TAF[Ptr] - WatershedDataSimplified$ContribStreamflow_TAF[S2ptr]
      }
    }
  }
  print(wshd)
}

################################################################################
# Aggregate precip stats for each HUC12 in each water year

ClimateFilenames = c("CNRM-CM5-RCP8.5","MIROC5-RCP8.5")
PrecipDir = "AvgYearlyPrecipMaps/"

# Prepare pointers to raster cells for each watershed
WshdPtrs = list()
for (wshd in HUCsavePoints$Watershed_Num){
  WshdPtrs[[wshd]] = which(WatershedRast[,]==wshd)
  print(wshd)
}

PrecipData = data.frame()

for (Climate in Climates){
  
  ClimateFilename = ClimateFilenames[which(Climates==Climate)]
  ClimateName = ClimateNames[which(Climates==Climate)]
  
  for (WaterYear in WaterYears){
    
    PrecipRaster = raster(paste0(PrecipDir,ClimateFilename,"_PRISMredistributed_WaterYear",WaterYear,"_TotalPrecip_m.tif"))
    
    PrecipDataChunk = data.frame(WatershedNum=HUCsavePoints$Watershed_Num,
                                 WaterYear=WaterYear,
                                 Climate=ClimateName,
                                 YearlyPrecip_m=NA)
    
    for (wshd in HUCsavePoints$Watershed_Num){
      
      PrecipDataChunk[PrecipDataChunk$WatershedNum==wshd,"YearlyPrecip_m"] = mean(PrecipRaster[WshdPtrs[[wshd]]])
      
      print(wshd)
    }
    
    PrecipData = rbind(PrecipData, PrecipDataChunk)
    
    print(paste(Climate, WaterYear))
  }
}

which(is.na(PrecipData$YearlyPrecip_m)) # Should return integer(0)

write.csv(PrecipData, "HUC12_YearlyPrecip_AllBasinsClimates.csv")

PrecipData = read.csv("HUC12_YearlyPrecip_AllBasinsClimates.csv")[,-1]

# Merge with watershed data
WatershedDataSimplified$YearlyPrecip_m = NA

# Match based on "case," which is unique combo of watershed number, water year, and climate
WatershedCase = paste0(WatershedDataSimplified$WatershedNum,WatershedDataSimplified$WaterYear,WatershedDataSimplified$Climate)
PrecipCase = paste0(PrecipData$WatershedNum,PrecipData$WaterYear,PrecipData$Climate)
Ptrs = match(WatershedCase, PrecipCase)

identical(WatershedCase, PrecipCase[Ptrs]) # Should return TRUE

WatershedDataSimplified$YearlyPrecip_m = PrecipData$YearlyPrecip_m[Ptrs]

################################################################################
# Calculate and merge watershed area

WshdAreaData = data.frame(WatershedNum=HUCsavePoints$Watershed_Num,
                          Area_AcresX1000=NA)

for (wshd in HUCsavePoints$Watershed_Num){
  
  Ptrs = which(WatershedRast[,]==wshd)
  WshdArea = ((length(Ptrs) * 90^2) / 4046.856) / 1000 # cells --> m^2 --> acres x1000
  
  WshdAreaData[WshdAreaData$WatershedNum==wshd,"Area_AcresX1000"] = WshdArea
}

Ptrs = match(WatershedDataSimplified$WatershedNum, WshdAreaData$WatershedNum)

identical(WatershedDataSimplified$WatershedNum, WshdAreaData$WatershedNum[Ptrs]) # Should return TRUE

WatershedDataSimplified$WatershedArea_AcresX1000 = WshdAreaData$Area_AcresX1000[Ptrs]

########## Test plots just for fun

plot(WatershedDataSimplified$YearlyPrecip_m, WatershedDataSimplified$ContribStreamflow_TAF/WatershedDataSimplified$WatershedArea_AcresX1000)
plot(WatershedDataSimplified$YearlyPrecip_m, WatershedDataSimplified$ContribStreamflow_RelS2_TAF/WatershedDataSimplified$WatershedArea_AcresX1000)

################################################################################
# Compile everything

# m depth --> ft. depth --> thousand acre-ft. (TAF)
PrecipConversionFactors =  3.28084 * WatershedDataSimplified$WatershedArea_AcresX1000

WaterEconData = data.frame(WatershedNumber=WatershedDataSimplified$WatershedNum,
                           WatershedArea_AcresX1000=WatershedDataSimplified$WatershedArea_AcresX1000,
                           WaterYear=WatershedDataSimplified$WaterYear,
                           Climate=WatershedDataSimplified$Climate,
                           Scenario=WatershedDataSimplified$Scenario,
                           LANDISrun=WatershedDataSimplified$LANDISrun,
                           Precipitation_TAF=WatershedDataSimplified$YearlyPrecip_m * PrecipConversionFactors,
                           Streamflow_TAF=WatershedDataSimplified$AvgStreamflow_TAF,
                           StreamflowContribution_TAF=WatershedDataSimplified$ContribStreamflow_TAF,
                           Streamflow_RelToS2_TAF=WatershedDataSimplified$AvgStreamflow_RelS2_TAF,
                           StreamflowContribution_RelToS2_TAF=WatershedDataSimplified$ContribStreamflow_RelS2_TAF)

head(WaterEconData)

write.csv(WaterEconData, "YearlyHUC12data_ForWaterEconAnalysis.csv")

################################################################################
# Just for fun: runoff ratios

WatershedData = WaterEconData
WatershedData$RunoffRatio = WatershedData$StreamflowContribution_TAF / WatershedData$Precipitation_TAF

hist(WatershedData$RunoffRatio)

RunoffRatioRastS2cnrm = WatershedRast * NA
RunoffRatioRastS2miroc = WatershedRast * NA
RunoffRatioRastS6cnrm = WatershedRast * NA
RunoffRatioRastS6miroc = WatershedRast * NA

for (wshd in HUCsavePoints$Watershed_Num){
  RastPtrs = which(WatershedRast[,]==wshd)
  
  Ptrs = which(WatershedData$WatershedNumber==wshd &
                 WatershedData$Scenario==2 &
                 WatershedData$Climate==ClimateNames[1])
  PrecipVal = sum(WatershedData$Precipitation_TAF[Ptrs])
  RunoffVal = sum(WatershedData$StreamflowContribution_TAF[Ptrs])
  RunoffRatioRastS2cnrm[RastPtrs] = RunoffVal / PrecipVal
  
  Ptrs = which(WatershedData$WatershedNumber==wshd &
                 WatershedData$Scenario==6 &
                 WatershedData$Climate==ClimateNames[1])
  PrecipVal = sum(WatershedData$Precipitation_TAF[Ptrs])
  RunoffVal = sum(WatershedData$StreamflowContribution_TAF[Ptrs])
  RunoffRatioRastS6cnrm[RastPtrs] = RunoffVal / PrecipVal
  
  Ptrs = which(WatershedData$WatershedNumber==wshd &
                 WatershedData$Scenario==2 &
                 WatershedData$Climate==ClimateNames[2])
  PrecipVal = sum(WatershedData$Precipitation_TAF[Ptrs])
  RunoffVal = sum(WatershedData$StreamflowContribution_TAF[Ptrs])
  RunoffRatioRastS2miroc[RastPtrs] = RunoffVal / PrecipVal
  
  Ptrs = which(WatershedData$WatershedNumber==wshd &
                 WatershedData$Scenario==6 &
                 WatershedData$Climate==ClimateNames[2])
  PrecipVal = sum(WatershedData$Precipitation_TAF[Ptrs])
  RunoffVal = sum(WatershedData$StreamflowContribution_TAF[Ptrs])
  RunoffRatioRastS6miroc[RastPtrs] = RunoffVal / PrecipVal
  
  print(wshd)
}

plot(RunoffRatioRastS2cnrm - RunoffRatioRastS2miroc)

plot(RunoffRatioRastS6cnrm - RunoffRatioRastS2cnrm)

plot(RunoffRatioRastS6miroc - RunoffRatioRastS2miroc)














