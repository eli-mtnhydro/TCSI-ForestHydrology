
library(ggplot2)
library(data.table)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Results"
setwd(dir)

WaterYears = 2015:2099
DatesOnly = seq(as.Date(paste0("10-01-",WaterYears[1]-1), format="%m-%d-%Y"),
                as.Date(paste0("09-30-",rev(WaterYears)[1]), format="%m-%d-%Y"),
                by="days")

BasinNames = c("Truckee", "Yuba", "Bear", "American")
DHSVMmodels = c(195,270,276)
Scenarios = 1:6
Climates = c("cnrm","miroc")
LANDIsruns = c(1,4)

################################################################################
# Create HUC8-only daily streamflow records

# Look-up table of important information
HUCsavePoints = read.csv("TCSI_HUC12savePoints.csv")[,-1]
HUCsavePoints$DHSVM_ID

# Watersheds: 1 (Yuba), 2 (Bear), 3 (American), 38 + 44 (Truckee)

if (exists("StreamflowData")){
  remove(StreamflowData)
}

for (DHSVMmodel in DHSVMmodels){
  for (Scenario in Scenarios){
    for (Climate in Climates){
      
      LANDIsrun = LANDIsruns[which(Climates==Climate)]
      RunCase = paste0("P",DHSVMmodel,"S",Scenario,Climate,"R",LANDIsrun)
      
      # Load streamflow data
      #DHSVMstreamflow = read.csv(paste0(RunCase,"_WSHDsaveMatrix.csv"))
      DHSVMstreamflow = as.data.frame(fread(paste0(RunCase,"_WSHDsaveMatrix.csv")))[,-1]
      
      for (Basin in BasinNames){
        
        if (Basin=="Truckee"){
          # Aggregate 3-hourly to daily
          DailyTimeseries1 = colSums(matrix(DHSVMstreamflow[,paste0("WSHD_",38)], nrow=8)) / 8
          DailyTimeseries2 = colSums(matrix(DHSVMstreamflow[,paste0("WSHD_",44)], nrow=8)) / 8
          DailyTimeseries = DailyTimeseries1 + DailyTimeseries2
          
        } else if (Basin=="Yuba"){
          # Aggregate 3-hourly to daily
          DailyTimeseries = colSums(matrix(DHSVMstreamflow[,paste0("WSHD_",1)], nrow=8)) / 8
        } else if (Basin=="Bear"){
          # Aggregate 3-hourly to daily
          DailyTimeseries = colSums(matrix(DHSVMstreamflow[,paste0("WSHD_",2)], nrow=8)) / 8
        } else if (Basin=="American"){
          # Aggregate 3-hourly to daily
          DailyTimeseries = colSums(matrix(DHSVMstreamflow[,paste0("WSHD_",3)], nrow=8)) / 8
        }
        
        if (!exists("StreamflowData")){
          StreamflowData = data.frame(Date=DatesOnly,
                                      DHSVMmodel=rep(DHSVMmodel,length(DatesOnly)),
                                      Scenario=rep(Scenario,length(DatesOnly)),
                                      Climate=rep(Climate,length(DatesOnly)),
                                      Basin=rep(Basin,length(DatesOnly)),
                                      DailyQ_cms=DailyTimeseries)
        } else {
          StreamflowData = rbind(StreamflowData,
                                 data.frame(Date=DatesOnly,
                                            DHSVMmodel=rep(DHSVMmodel,length(DatesOnly)),
                                            Scenario=rep(Scenario,length(DatesOnly)),
                                            Climate=rep(Climate,length(DatesOnly)),
                                            Basin=rep(Basin,length(DatesOnly)),
                                            DailyQ_cms=DailyTimeseries))
        }
        
      } # End of loop over basins
      
      print(paste("Done with",Climate,Scenario,DHSVMmodel))
    }
  }
}

write.csv(StreamflowData, "HUC8_StreamflowTimeseries_AllBasinsModelsClimatesScenarios.csv")

################################################################################
# Get daily aggregated state/flux variables and daily streamflow

AggregatedData = as.data.frame(fread("AggregatedDailyData_AllBasinsModelsClimatesScenarios.csv"))[,-1]

ValueNames = names(AggregatedData)[-(1:5)]
print(ValueNames)
# Overstory is "Story0"
# Undestory is "Story1"

StreamflowData = as.data.frame(fread("HUC8_StreamflowTimeseries_AllBasinsModelsClimatesScenarios.csv"))[,-1]
head(StreamflowData)

# Numeric water year corresponding to each date
WaterYearOfDate = as.numeric(format(StreamflowData$Date,"%Y"))
OctNovDecPtrs = which(as.numeric(format(StreamflowData$Date,"%m")) >= 10)
WaterYearOfDate[OctNovDecPtrs] = WaterYearOfDate[OctNovDecPtrs] + 1
StreamflowData$WaterYear = WaterYearOfDate

################################################################################
# Gather data on peak flow process dynamics

NoPrecipThreshold = 0.001 # 1 mm/d

if (exists("PeakFlowData")){
  remove(PeakFlowData)
}

for (DHSVMmodel in DHSVMmodels){
  for (Scenario in Scenarios){
    for (Climate in Climates){
      for (Basin in BasinNames){
        
        PeakFlowDataChunk = data.frame(WaterYear=WaterYears,
                                       DHSVMmodel=rep(DHSVMmodel,length(WaterYears)),
                                       Scenario=rep(Scenario,length(WaterYears)),
                                       Climate=rep(Climate,length(WaterYears)),
                                       Basin=rep(Basin,length(WaterYears)),
                                       PeakDate=as.Date(NA),
                                       PeakFlow_cms=NA,
                                       StormStartDate=as.Date(NA),
                                       StormDuration_days=NA,
                                       Precip_DuringStorm_mm=NA,
                                       Snow_DuringStorm_mm=NA,
                                       IntSnowOverstory_PeakDate_mm=NA,
                                       IntRainOverstory_PeakDate_mm=NA,
                                       IntRainUnderstory_PeakDate_mm=NA,
                                       IntSnowOverstory_BeforeStorm_mm=NA,
                                       IntRainOverstory_BeforeStorm_mm=NA,
                                       IntRainUnderstory_BeforeStorm_mm=NA,
                                       IntEvapOverstory_DuringStorm_mm=NA,
                                       IntEvapUnderstory_DuringStorm_mm=NA,
                                       SnowSW_AvgDuringStorm_wm2=NA,
                                       SnowLW_AvgDuringStorm_wm2=NA,
                                       SnowSensible_AvgDuringStorm_wm2=NA,
                                       SnowLatent_AvgDuringStorm_wm2=NA,
                                       SnowAdvected_AvgDuringStorm_wm2=NA,
                                       SnowMelt_TotDuringStorm_mm=NA)
        
        i = 1
        for (WaterYear in WaterYears){
          
          Ptrs = which(StreamflowData$WaterYear==WaterYear &
                         StreamflowData$DHSVMmodel==DHSVMmodel &
                         StreamflowData$Scenario==Scenario &
                         StreamflowData$Climate==Climate &
                         StreamflowData$Basin==Basin)
          
          PeakFlow = max(StreamflowData$DailyQ_cms[Ptrs])
          PeakPtr = Ptrs[which(StreamflowData$DailyQ_cms[Ptrs]==PeakFlow)]
          
          PeakFlowDataChunk[i,"PeakDate"] = StreamflowData$Date[PeakPtr]
          PeakFlowDataChunk[i,"PeakFlow_cms"] = StreamflowData$DailyQ_cms[PeakPtr]
          
          # Find the last date with no precip before the current peak flow
          AggrPtrs = which(AggregatedData$DHSVMmodel==DHSVMmodel &
                             AggregatedData$Scenario==Scenario &
                             AggregatedData$Climate==Climate &
                             AggregatedData$Basin==Basin &
                             AggregatedData$Date < PeakFlowDataChunk[i,"PeakDate"] &
                             AggregatedData$Precip_m < NoPrecipThreshold)
          
          StormStartPtr = max(AggrPtrs) # Last day with < NoPrecipThreshold of precip
          
          PeakDatePtr = which(AggregatedData$DHSVMmodel==DHSVMmodel &
                                AggregatedData$Scenario==Scenario &
                                AggregatedData$Climate==Climate &
                                AggregatedData$Basin==Basin &
                                AggregatedData$Date==PeakFlowDataChunk[i,"PeakDate"])
          
          StormPtrs = (StormStartPtr + 1):PeakDatePtr # Begins with first > NoPrecipThreshold day
          
          # Collect relevant state/flux variables and convert units as applicable
          
          ##########
          ########## Important: "start date" with < NoPrecipThreshold is NOT included in storm averages, but IS used for "pre-storm" interception
          ##########
          
          PeakFlowDataChunk[i,"StormStartDate"] = AggregatedData$Date[StormStartPtr + 1] # First day with precip > NoPrecipThreshold
          PeakFlowDataChunk[i,"StormDuration_days"] = length(StormPtrs)
          PeakFlowDataChunk[i,"Precip_DuringStorm_mm"] = sum(AggregatedData$Precip_m[StormPtrs]) * 1000
          PeakFlowDataChunk[i,"Snow_DuringStorm_mm"] = sum(AggregatedData$Snow_m[StormPtrs]) * 1000
          PeakFlowDataChunk[i,"IntSnowOverstory_PeakDate_mm"] = AggregatedData$IntSnowStory0[PeakDatePtr] * 1000
          PeakFlowDataChunk[i,"IntRainOverstory_PeakDate_mm"] = AggregatedData$IntRainStory0[PeakDatePtr] * 1000
          PeakFlowDataChunk[i,"IntRainUnderstory_PeakDate_mm"] = AggregatedData$IntRainStory1[PeakDatePtr] * 1000
          PeakFlowDataChunk[i,"IntSnowOverstory_BeforeStorm_mm"] = AggregatedData$IntSnowStory0[StormStartPtr] * 1000
          PeakFlowDataChunk[i,"IntRainOverstory_BeforeStorm_mm"] = AggregatedData$IntRainStory0[StormStartPtr] * 1000
          PeakFlowDataChunk[i,"IntRainUnderstory_BeforeStorm_mm"] = AggregatedData$IntRainStory1[StormStartPtr] * 1000
          PeakFlowDataChunk[i,"IntEvapOverstory_DuringStorm_mm"] = sum(AggregatedData$EvapCanopyIntStory0[StormPtrs]) * 1000
          PeakFlowDataChunk[i,"IntEvapUnderstory_DuringStorm_mm"] = sum(AggregatedData$EvapCanopyIntStory1[StormPtrs]) * 1000
          PeakFlowDataChunk[i,"SnowSW_AvgDuringStorm_wm2"] = mean(AggregatedData$SnowQsw[StormPtrs])
          PeakFlowDataChunk[i,"SnowLW_AvgDuringStorm_wm2"] = mean(AggregatedData$SnowQlw[StormPtrs])
          PeakFlowDataChunk[i,"SnowSensible_AvgDuringStorm_wm2"] = mean(AggregatedData$SnowQs[StormPtrs])
          PeakFlowDataChunk[i,"SnowLatent_AvgDuringStorm_wm2"] = mean(AggregatedData$SnowQe[StormPtrs])
          PeakFlowDataChunk[i,"SnowAdvected_AvgDuringStorm_wm2"] = mean(AggregatedData$SnowQp[StormPtrs])
          PeakFlowDataChunk[i,"SnowMelt_TotDuringStorm_mm"] = sum(AggregatedData$Melt[StormPtrs]) * 1000
          
          i = i + 1 # Advance index of water year in PeakFlowDataChunk
          #print(WaterYear)
        } # End loop over water years
        
        if (!exists("PeakFlowData")){
          PeakFlowData = PeakFlowDataChunk
        } else {
          PeakFlowData = rbind(PeakFlowData, PeakFlowDataChunk)
        }
       
        print(paste("Done with",Climate,Scenario,DHSVMmodel)) 
      }
    }
  }
}

########## Add derivative variables

# Total interception: peak date
PeakFlowData$IntTotal_PeakDate_mm = PeakFlowData$IntSnowOverstory_PeakDate_mm +
  PeakFlowData$IntRainOverstory_PeakDate_mm +
  PeakFlowData$IntRainUnderstory_PeakDate_mm

# Total interception: before storm
PeakFlowData$IntTotal_BeforeStorm_mm = PeakFlowData$IntSnowOverstory_BeforeStorm_mm +
  PeakFlowData$IntRainOverstory_BeforeStorm_mm +
  PeakFlowData$IntRainUnderstory_BeforeStorm_mm

# Total interception change
PeakFlowData$IntTotal_DeltaStorm_mm = PeakFlowData$IntTotal_PeakDate_mm - PeakFlowData$IntTotal_BeforeStorm_mm

# Total interception loss during storm
PeakFlowData$IntEvapTotal_DuringStorm_mm = PeakFlowData$IntEvapOverstory_DuringStorm_mm + PeakFlowData$IntEvapUnderstory_DuringStorm_mm

# Average SW + LW snowpack radiation during storm
PeakFlowData$SnowRadTotal_AvgDuringStorm_wm2 = PeakFlowData$SnowSW_AvgDuringStorm_wm2 + PeakFlowData$SnowLW_AvgDuringStorm_wm2

PeakFlowData$PrecipIntensity_mmd = PeakFlowData$Precip_DuringStorm_mm / PeakFlowData$StormDuration_days

# Calculate ranks (out of 85-year record) for each event
PeakFlowData$PeakFlowRank = NA
PeakFlowData$PrecipRank = NA
PeakFlowData$PrecipIntensityRank = NA

for (DHSVMmodel in DHSVMmodels){
  for (Scenario in Scenarios){
    for (Climate in Climates){
      for (Basin in BasinNames){
        
        Ptrs = which(PeakFlowData$DHSVMmodel==DHSVMmodel &
                       PeakFlowData$Scenario==Scenario &
                       PeakFlowData$Climate==Climate &
                       PeakFlowData$Basin==Basin)
        
        PeakFlows = sort(PeakFlowData$PeakFlow_cms[Ptrs], decreasing=TRUE)
        
        PrecipVals = sort(PeakFlowData$Precip_DuringStorm_mm[Ptrs], decreasing=TRUE)
        
        IntensityVals = sort(PeakFlowData$PrecipIntensity_mmd[Ptrs], decreasing=TRUE)
        
        for (WaterYear in WaterYears){
          
          Ptr = Ptrs[which(PeakFlowData$WaterYear[Ptrs]==WaterYear)]
          
          PeakRank = which(PeakFlows==PeakFlowData$PeakFlow_cms[Ptr])
          PeakFlowData$PeakFlowRank[Ptr] = min(PeakRank) # Handle ties
          
          PrecipRank = which(PrecipVals==PeakFlowData$Precip_DuringStorm_mm[Ptr])
          PeakFlowData$PrecipRank[Ptr] = min(PrecipRank) # Handle ties
          
          IntensityRank = which(IntensityVals==PeakFlowData$PrecipIntensity_mmd[Ptr])
          PeakFlowData$PrecipIntensityRank[Ptr] = min(IntensityRank) # Handle ties
          
          # print(WaterYear)
          # print(PeakPercentile)
          # print(PeakFlowData$PeakFlowPercentile[Ptr])
        }
      }
    }
  }
}

########## Compute relative scenario effects for selected variables

# Absolute
PeakFlowData$IntTotal_DeltaStorm_RelS2_mm = NA
PeakFlowData$IntEvapTotal_DuringStorm_RelS2_mm = NA
PeakFlowData$SnowRadTotal_AvgDuringStorm_RelS2_wm2 = NA
PeakFlowData$SnowMelt_TotDuringStorm_RelS2_mm = NA
PeakFlowData$SnowSW_AvgDuringStorm_RelS2_wm2 = NA
PeakFlowData$SnowLW_AvgDuringStorm_RelS2_wm2 = NA
PeakFlowData$SnowSensible_AvgDuringStorm_RelS2_wm2 = NA
PeakFlowData$SnowLatent_AvgDuringStorm_RelS2_wm2 = NA
PeakFlowData$SnowAdvected_AvgDuringStorm_RelS2_wm2 = NA

# Percentage
PeakFlowData$IntTotal_DeltaStorm_RelS2pct = NA
PeakFlowData$IntEvapTotal_DuringStorm_RelS2pct = NA
PeakFlowData$SnowRadTotal_AvgDuringStorm_RelS2pct = NA
PeakFlowData$SnowMelt_TotDuringStorm_RelS2pct = NA
PeakFlowData$SnowSW_AvgDuringStorm_RelS2pct = NA
PeakFlowData$SnowLW_AvgDuringStorm_RelS2pct = NA
PeakFlowData$SnowSensible_AvgDuringStorm_RelS2pct = NA
PeakFlowData$SnowLatent_AvgDuringStorm_RelS2pct = NA
PeakFlowData$SnowAdvected_AvgDuringStorm_RelS2pct = NA

for (BasinName in BasinNames){
  for (Climate in Climates){
    for (DHSVMmodel in DHSVMmodels){
      for (WaterYear in WaterYears){
        
        Ptrs = which(PeakFlowData$WaterYear==WaterYear & 
                       PeakFlowData$DHSVMmodel==DHSVMmodel &
                       PeakFlowData$Climate==Climate &
                       PeakFlowData$Basin==BasinName)
        
        S2Ptr = which(PeakFlowData$WaterYear==WaterYear & 
                        PeakFlowData$DHSVMmodel==DHSVMmodel &
                        PeakFlowData$Climate==Climate &
                        PeakFlowData$Basin==BasinName &
                        PeakFlowData$Scenario==2)
        
        # Avg_IntTotal_DeltaStorm_mm = mean(PeakFlowData$IntTotal_DeltaStorm_mm[Ptrs])
        # Avg_SnowRadTotal_AvgDuringStorm_wm2 = mean(PeakFlowData$SnowRadTotal_AvgDuringStorm_wm2[Ptrs])
        # Avg_SnowMelt_TotDuringStorm_mm = mean(PeakFlowData$SnowMelt_TotDuringStorm_mm[Ptrs])
        
        S2_IntTotal_DeltaStorm_mm = PeakFlowData$IntTotal_DeltaStorm_mm[S2Ptr]
        S2_IntEvapTotal_DuringStorm_mm = PeakFlowData$IntEvapTotal_DuringStorm_mm[S2Ptr]
        S2_SnowRadTotal_AvgDuringStorm_wm2 = PeakFlowData$SnowRadTotal_AvgDuringStorm_wm2[S2Ptr]
        S2_SnowMelt_TotDuringStorm_mm = PeakFlowData$SnowMelt_TotDuringStorm_mm[S2Ptr]
        S2_SnowSW_AvgDuringStorm_wm2 = PeakFlowData$SnowSW_AvgDuringStorm_wm2[S2Ptr]
        S2_SnowLW_AvgDuringStorm_wm2 = PeakFlowData$SnowLW_AvgDuringStorm_wm2[S2Ptr]
        S2_SnowSensible_AvgDuringStorm_wm2 = PeakFlowData$SnowSensible_AvgDuringStorm_wm2[S2Ptr]
        S2_SnowLatent_AvgDuringStorm_wm2 = PeakFlowData$SnowLatent_AvgDuringStorm_wm2[S2Ptr]
        S2_SnowAdvected_AvgDuringStorm_wm2 = PeakFlowData$SnowAdvected_AvgDuringStorm_wm2[S2Ptr]
        
        for (Scenario in Scenarios){
          
          Ptr = Ptrs[which(PeakFlowData$Scenario[Ptrs]==Scenario)]
          
          # Absolute
          PeakFlowData$IntTotal_DeltaStorm_RelS2_mm[Ptr] = PeakFlowData$IntTotal_DeltaStorm_mm[Ptr] - S2_IntTotal_DeltaStorm_mm
          PeakFlowData$IntEvapTotal_DuringStorm_RelS2_mm[Ptr] = PeakFlowData$IntEvapTotal_DuringStorm_mm[Ptr] - S2_IntEvapTotal_DuringStorm_mm
          PeakFlowData$SnowRadTotal_AvgDuringStorm_RelS2_wm2[Ptr] = PeakFlowData$SnowRadTotal_AvgDuringStorm_wm2[Ptr] - S2_SnowRadTotal_AvgDuringStorm_wm2
          PeakFlowData$SnowMelt_TotDuringStorm_RelS2_mm[Ptr] = PeakFlowData$SnowMelt_TotDuringStorm_mm[Ptr] - S2_SnowMelt_TotDuringStorm_mm
          PeakFlowData$SnowSW_AvgDuringStorm_RelS2_wm2[Ptr] = PeakFlowData$SnowSW_AvgDuringStorm_wm2[Ptr] - S2_SnowSW_AvgDuringStorm_wm2
          PeakFlowData$SnowLW_AvgDuringStorm_RelS2_wm2[Ptr] = PeakFlowData$SnowLW_AvgDuringStorm_wm2[Ptr] - S2_SnowLW_AvgDuringStorm_wm2
          PeakFlowData$SnowSensible_AvgDuringStorm_RelS2_wm2[Ptr] = PeakFlowData$SnowSensible_AvgDuringStorm_wm2[Ptr] - S2_SnowSensible_AvgDuringStorm_wm2
          PeakFlowData$SnowLatent_AvgDuringStorm_RelS2_wm2[Ptr] = PeakFlowData$SnowLatent_AvgDuringStorm_wm2[Ptr] - S2_SnowLatent_AvgDuringStorm_wm2
          PeakFlowData$SnowAdvected_AvgDuringStorm_RelS2_wm2[Ptr] = PeakFlowData$SnowAdvected_AvgDuringStorm_wm2[Ptr] - S2_SnowAdvected_AvgDuringStorm_wm2
          
          # Percentage
          PeakFlowData$IntTotal_DeltaStorm_RelS2pct[Ptr] = PeakFlowData$IntTotal_DeltaStorm_mm[Ptr] / S2_IntTotal_DeltaStorm_mm - 1
          PeakFlowData$IntEvapTotal_DuringStorm_RelS2pct[Ptr] = PeakFlowData$IntEvapTotal_DuringStorm_mm[Ptr] / S2_IntEvapTotal_DuringStorm_mm - 1
          PeakFlowData$SnowRadTotal_AvgDuringStorm_RelS2pct[Ptr] = PeakFlowData$SnowRadTotal_AvgDuringStorm_wm2[Ptr] / S2_SnowRadTotal_AvgDuringStorm_wm2 - 1
          PeakFlowData$SnowMelt_TotDuringStorm_RelS2pct[Ptr] = PeakFlowData$SnowMelt_TotDuringStorm_mm[Ptr] / S2_SnowMelt_TotDuringStorm_mm - 1
          PeakFlowData$SnowSW_AvgDuringStorm_RelS2pct[Ptr] = PeakFlowData$SnowSW_AvgDuringStorm_wm2[Ptr] / S2_SnowSW_AvgDuringStorm_wm2 - 1
          PeakFlowData$SnowLW_AvgDuringStorm_RelS2pct[Ptr] = PeakFlowData$SnowLW_AvgDuringStorm_wm2[Ptr] / S2_SnowLW_AvgDuringStorm_wm2 - 1
          PeakFlowData$SnowSensible_AvgDuringStorm_RelS2pct[Ptr] = PeakFlowData$SnowSensible_AvgDuringStorm_wm2[Ptr] / S2_SnowSensible_AvgDuringStorm_wm2 - 1
          PeakFlowData$SnowLatent_AvgDuringStorm_RelS2pct[Ptr] = PeakFlowData$SnowLatent_AvgDuringStorm_wm2[Ptr] / S2_SnowLatent_AvgDuringStorm_wm2 - 1
          PeakFlowData$SnowAdvected_AvgDuringStorm_RelS2pct[Ptr] = PeakFlowData$SnowAdvected_AvgDuringStorm_wm2[Ptr] / S2_SnowAdvected_AvgDuringStorm_wm2 - 1
        }
      }
      print(paste(BasinName,Climate,DHSVMmodel))
    }
  }
}

write.csv(PeakFlowData, "PeakFlowProcessData.csv")

################################################################################
# Visualize results (can start here)

PeakFlowData = read.csv("PeakFlowProcessData.csv")[,-1]

# Change categorical variables to factors for predictable plotting
PeakFlowData$Model = PeakFlowData$DHSVMmodel
PeakFlowData$Model = factor(PeakFlowData$Model, levels=sort(unique(PeakFlowData$Model)))
PeakFlowData$Scenario = factor(PeakFlowData$Scenario, levels=sort(unique(PeakFlowData$Scenario)))
PeakFlowData$Climate = factor(PeakFlowData$Climate, levels=unique(PeakFlowData$Climate))
PeakFlowData$Basin = factor(PeakFlowData$Basin, levels=unique(PeakFlowData$Basin))

# Change climate name
PeakFlowData$ClimateName = PeakFlowData$Climate
PeakFlowData$ClimateName = sub("cnrm", "CNRM-CM5 RCP 8.5", PeakFlowData$ClimateName)
PeakFlowData$ClimateName = sub("miroc", "MIROC5 RCP 8.5", PeakFlowData$ClimateName)

# Trim percentiles to a reasonable range
MaxPercentileRange = 0.25

PeakFlowData$IntTotal_DeltaStorm_Rel_pct[PeakFlowData$IntTotal_DeltaStorm_Rel_pct > MaxPercentileRange] = MaxPercentileRange
PeakFlowData$IntTotal_DeltaStorm_Rel_pct[PeakFlowData$IntTotal_DeltaStorm_Rel_pct < -MaxPercentileRange] = -MaxPercentileRange

PeakFlowData$SnowRadTotal_AvgDuringStorm_Rel_pct[PeakFlowData$SnowRadTotal_AvgDuringStorm_Rel_pct > MaxPercentileRange] = MaxPercentileRange
PeakFlowData$SnowRadTotal_AvgDuringStorm_Rel_pct[PeakFlowData$SnowRadTotal_AvgDuringStorm_Rel_pct < -MaxPercentileRange] = -MaxPercentileRange

PeakFlowData$SnowMelt_TotDuringStorm_Rel_pct[PeakFlowData$SnowMelt_TotDuringStorm_Rel_pct > MaxPercentileRange] = MaxPercentileRange
PeakFlowData$SnowMelt_TotDuringStorm_Rel_pct[PeakFlowData$SnowMelt_TotDuringStorm_Rel_pct < -MaxPercentileRange] = -MaxPercentileRange

PercentileThreshold = 0.9

PeakFlowDataSubset = PeakFlowData[(PeakFlowData$PeakFlowPercentile > PercentileThreshold),]

# Interception storage change during peak flow storms

ggplot(data=PeakFlowDataSubset, aes(x=Scenario, y=IntTotal_DeltaStorm_Rel_pct)) + 
  geom_hline(yintercept=0, linewidth=1) +
  geom_violin(scale="width", width=0.9, linewidth=0.5, color="black", aes(fill=Scenario)) +
  scale_fill_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_y_continuous(limits=c(-MaxPercentileRange,MaxPercentileRange),
                     breaks=c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3),
                     labels=c("-30%","-20%","-10%","Mean","+10%","+20%","+30%")) +
  labs(x="Increasing Forest Thinning →",
       y="Rel. Change in Interception Storage",
       title="Forest Thinning Effect on Storm Interception\nPrior to >90th Percentile Peak Flow Events") +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_text(size=20, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=20, margin=margin(0,0.5,0,0,"cm")),
        axis.ticks.length.x=unit(0,"cm"),
        axis.ticks.length.y=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20),
        legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5,
        strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  guides(fill=guide_legend(byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

# Net snowpack radiation during peak flow storms

ggplot(data=PeakFlowDataSubset, aes(x=Scenario, y=SnowRadTotal_AvgDuringStorm_Rel_pct)) + 
  geom_hline(yintercept=0, linewidth=1) +
  geom_violin(scale="width", width=0.9, linewidth=0.5, color="black", aes(fill=Scenario)) +
  scale_fill_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_y_continuous(limits=c(-MaxPercentileRange,MaxPercentileRange),
                     breaks=c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3),
                     labels=c("-30%","-20%","-10%","Mean","+10%","+20%","+30%")) +
  labs(x="Increasing Forest Thinning →",
       y="Rel. Avg. Net Snowpack Radiation Flux",
       title="Forest Thinning Effect on Snowpack Radiation\nPrior to >90th Percentile Peak Flow Events") +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_text(size=20, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=20, margin=margin(0,0.5,0,0,"cm")),
        axis.ticks.length.x=unit(0,"cm"),
        axis.ticks.length.y=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20),
        legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5,
        strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  guides(fill=guide_legend(byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

# Total snowmelt during peak flow storms

ggplot(data=PeakFlowDataSubset, aes(x=Scenario, y=SnowMelt_TotDuringStorm_Rel_pct)) + 
  geom_hline(yintercept=0, linewidth=1) +
  geom_violin(scale="width", width=0.9, linewidth=0.5, color="black", aes(fill=Scenario)) +
  scale_fill_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_y_continuous(limits=c(-MaxPercentileRange,MaxPercentileRange),
                     breaks=c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3),
                     labels=c("-30%","-20%","-10%","Mean","+10%","+20%","+30%")) +
  labs(x="Increasing Forest Thinning →",
       y="Rel. Total Snowmelt",
       title="Forest Thinning Effect on Snowmelt\nPrior to >90th Percentile Peak Flow Events") +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_text(size=20, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=20, margin=margin(0,0.5,0,0,"cm")),
        axis.ticks.length.x=unit(0,"cm"),
        axis.ticks.length.y=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20),
        legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5,
        strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  guides(fill=guide_legend(byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

################################################################################

names(PeakFlowData)

ggplot(data=PeakFlowDataSubset, aes(x=Scenario, y=SnowLW_AvgDuringStorm_wm2)) + 
  geom_hline(yintercept=0, linewidth=1) +
  geom_point(size=3, stroke=0.1, aes(color=Scenario, shape=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_text(size=20, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=20, margin=margin(0,0.5,0,0,"cm")),
        axis.ticks.length.x=unit(0,"cm"),
        axis.ticks.length.y=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20),
        legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5,
        strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  guides(fill=guide_legend(byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

