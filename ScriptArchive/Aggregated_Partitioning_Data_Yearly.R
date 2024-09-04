
library(terra)
library(data.table)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Water_Balance_Partitioning/"
setwd(dir)

################################################################################
# Aggregated values

YearlyAggregatedData = read.csv("AggregatedYearlyData_AllBasinsModelsClimatesScenarios.csv")[,-1]
YearlyAggregatedData = YearlyAggregatedData[,-(67:93)] # Not using relative data here
AggVarNames = names(YearlyAggregatedData)[-(1:5)]
FluxPtrs = which((AggVarNames %in%
                   c("Precip_m","Snow_m","Melt","TotalET")) |
                   grepl("Evap",AggVarNames) |
                   grepl("Transp",AggVarNames))

WaterYears = unique(YearlyAggregatedData$WaterYear)
BasinNames = unique(YearlyAggregatedData$Basin)
DHSVMmodels = unique(YearlyAggregatedData$DHSVMmodel)
Scenarios = unique(YearlyAggregatedData$Scenario)
Climates = unique(YearlyAggregatedData$Climate)

# Collapse to water year averages
# Average over DHSVM models and basins (area-weighted)

AggregatedData = data.frame(matrix(0,
                                   nrow=length(Scenarios)*length(Climates)*length(WaterYears),
                                   ncol=3+length(AggVarNames)))
names(AggregatedData) = c("WaterYear","Scenario","Climate",AggVarNames)
AggregatedData$WaterYear = sort(rep(WaterYears,length(Climates)*length(Scenarios)))
AggregatedData$Scenario = rep(Scenarios,length(Climates)*length(WaterYears))
AggregatedData$Climate = rep(sort(rep(Climates,length(Scenarios))),length(WaterYears))

BasinAreas = c()
for (BasinName in BasinNames){
  
  Mask = rast(paste0("TCSImask_",BasinName,"Domain.tif"))
  plot(Mask)
  
  BasinArea = length(which(Mask[,]==1)) * 90^2 / (1000^2)
  BasinAreas = c(BasinAreas,BasinArea)
}

# Note on units:
# Starts as [depth, m] for each 3-hour timestep as output by DHSVM
# Then averaged to [depth/time, m/3hr, daily avg.] in CompileDHSVMresultsAggregatedValues.R
# Then averaged to [depth/time, m/3hr, yearly avg.] in VisualizeAggregatedResults.R
# Then here we convert back to [depth, m] by multiplying avg. [m/3hr] * 365.25 * 3 hrs.

for (WaterYear in WaterYears){
  for (Scenario in Scenarios){
    for (Climate in Climates){
      
      Ptr = which(AggregatedData$WaterYear==WaterYear &
                    AggregatedData$Scenario==Scenario &
                    AggregatedData$Climate==Climate)
      
      for (BasinName in BasinNames){
        
        Ptrs = which(YearlyAggregatedData$WaterYear==WaterYear &
                       YearlyAggregatedData$Scenario==Scenario &
                       YearlyAggregatedData$Climate==Climate &
                       YearlyAggregatedData$Basin==BasinName)
        
        NewData = colMeans(YearlyAggregatedData[Ptrs,AggVarNames]) # Average over DHSVM models
        NewData[FluxPtrs] = NewData[FluxPtrs] * 365.25 * (24/3) # Final units: m per year avg.
        NewData = NewData * BasinAreas[which(BasinNames==BasinName)] / sum(BasinAreas)
        AggregatedData[Ptr,AggVarNames] = AggregatedData[Ptr,AggVarNames] + NewData
      }
    }
  }
  print(WaterYear)
}

################################################################################
# Streamflow

StreamflowData = as.data.frame(fread("HUC8_StreamflowTimeseries_AllBasinsModelsClimatesScenarios.csv"))[,-1]
StreamflowData$WaterYear = as.numeric(format(as.Date(StreamflowData$Date,"%Y-%m-%d"),"%Y"))
OctNovDecPtrs = which(as.numeric(format(as.Date(StreamflowData$Date,"%Y-%m-%d"),"%m")) > 9)
StreamflowData$WaterYear[OctNovDecPtrs] = StreamflowData$WaterYear[OctNovDecPtrs] + 1

# Collapse to yearly averages
# Average over DHSVM models and basins (area-weighted)

AggregatedStreamflowData = data.frame(matrix(0,
                                             nrow=length(Scenarios)*length(Climates)*length(WaterYears),
                                             ncol=4))
names(AggregatedStreamflowData) = c("WaterYear","Scenario","Climate","Streamflow_m")
AggregatedStreamflowData$WaterYear = sort(rep(WaterYears,length(Climates)*length(Scenarios)))
AggregatedStreamflowData$Scenario = rep(Scenarios,length(Climates)*length(WaterYears))
AggregatedStreamflowData$Climate = rep(sort(rep(Climates,length(Scenarios))),length(WaterYears))

for (WaterYear in WaterYears){
  for (Scenario in Scenarios){
    for (Climate in Climates){
      
      Ptr = which(AggregatedStreamflowData$WaterYear==WaterYear &
                    AggregatedStreamflowData$Scenario==Scenario &
                    AggregatedStreamflowData$Climate==Climate)
      
      for (BasinName in BasinNames){
        
        Ptrs = which(StreamflowData$WaterYear==WaterYear &
                       StreamflowData$Scenario==Scenario &
                       StreamflowData$Climate==Climate &
                       StreamflowData$Basin==BasinName)
        
        NewData = mean(StreamflowData[Ptrs,"DailyQ_cms"]) # Average over DHSVM models
        NewData = NewData * (60*60*24*365.25) / (BasinAreas[which(BasinNames==BasinName)] * 1000^2) # Final units: m per year avg.
        NewData = NewData * BasinAreas[which(BasinNames==BasinName)] / sum(BasinAreas)
        AggregatedStreamflowData[Ptr,"Streamflow_m"] = AggregatedStreamflowData[Ptr,"Streamflow_m"] + NewData
      }
    }
  }
  print(WaterYear)
}

################################################################################

WaterBalanceData = data.frame(WaterYear=AggregatedData$WaterYear,
                              Scenario=AggregatedData$Scenario,
                              Climate=AggregatedData$Climate,
                              Precip_mPerYr=AggregatedData$Precip_m,
                              TotalET_mPerYr=AggregatedData$TotalET,
                              OverstoryTranspiration_mPerYr=AggregatedData$ActTranspStory0,
                              UnderstoryTranspiration_mPerYr=AggregatedData$ActTranspStory1,
                              OverstoryInterceptionLoss_mPerYr=AggregatedData$EvapCanopyIntStory0,
                              UnderstoryInterceptionLoss_mPerYr=AggregatedData$EvapCanopyIntStory1,
                              SoilEvaporation_mPerYr=AggregatedData$SoilEvap,
                              Runoff_mPerYr=AggregatedStreamflowData$Streamflow_m)

# Logic check
Residuals = WaterBalanceData$Precip_mPerYr - (WaterBalanceData$OverstoryTranspiration_mPerYr +
                                                WaterBalanceData$UnderstoryTranspiration_mPerYr +
                                                WaterBalanceData$OverstoryInterceptionLoss_mPerYr +
                                                WaterBalanceData$UnderstoryInterceptionLoss_mPerYr +
                                                WaterBalanceData$SoilEvaporation_mPerYr +
                                                WaterBalanceData$Runoff_mPerYr)
print(paste0(signif(100*mean(Residuals / WaterBalanceData$Precip_mPerYr),2),"% Error"))
print(paste0(signif(1000*mean(abs(Residuals)),2)," mm/yr avg. Storage Change (+/-)"))
plot(Residuals,type="l")

write.csv(WaterBalanceData,"AverageWaterBalanceData_Yearly.csv")
