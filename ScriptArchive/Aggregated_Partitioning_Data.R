
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

# Collapse to full-period averages
# Average over water years, DHSVM models, and basins (area-weighted)

AggregatedData = data.frame(matrix(0,
                                   nrow=length(Scenarios)*length(Climates),
                                   ncol=2+length(AggVarNames)))
names(AggregatedData) = c("Scenario","Climate",AggVarNames)
AggregatedData$Scenario = rep(Scenarios,length(Climates))
AggregatedData$Climate = sort(rep(Climates,length(Scenarios)))

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

for (Scenario in Scenarios){
  for (Climate in Climates){
    
    Ptr = which(AggregatedData$Scenario==Scenario &
                  AggregatedData$Climate==Climate)
    
    for (BasinName in BasinNames){
      
      Ptrs = which(YearlyAggregatedData$Scenario==Scenario &
                     YearlyAggregatedData$Climate==Climate &
                     YearlyAggregatedData$Basin==BasinName)
      
      NewData = colMeans(YearlyAggregatedData[Ptrs,AggVarNames])
      NewData[FluxPtrs] = NewData[FluxPtrs] * 365.25 * (24/3) # Final units: m per year avg.
      NewData = NewData * BasinAreas[which(BasinNames==BasinName)] / sum(BasinAreas)
      AggregatedData[Ptr,AggVarNames] = AggregatedData[Ptr,AggVarNames] + NewData
    }
  }
}

################################################################################
# Streamflow

StreamflowData = as.data.frame(fread("HUC8_StreamflowTimeseries_AllBasinsModelsClimatesScenarios.csv"))[,-1]

# Collapse to full-period averages
# Average over water years, DHSVM models, and basins (area-weighted)

AggregatedStreamflowData = data.frame(matrix(0,
                                             nrow=length(Scenarios)*length(Climates),
                                             ncol=3))
names(AggregatedStreamflowData) = c("Scenario","Climate","Streamflow_m")
AggregatedStreamflowData$Scenario = rep(Scenarios,length(Climates))
AggregatedStreamflowData$Climate = sort(rep(Climates,length(Scenarios)))

for (Scenario in Scenarios){
  for (Climate in Climates){
    
    Ptr = which(AggregatedStreamflowData$Scenario==Scenario &
                  AggregatedStreamflowData$Climate==Climate)
    
    for (BasinName in BasinNames){
      
      Ptrs = which(StreamflowData$Scenario==Scenario &
                     StreamflowData$Climate==Climate &
                     StreamflowData$Basin==BasinName)
      
      NewData = mean(StreamflowData[Ptrs,"DailyQ_cms"])
      NewData = NewData * (60*60*24*365.25) / (BasinAreas[which(BasinNames==BasinName)] * 1000^2) # Final units: m per year avg.
      NewData = NewData * BasinAreas[which(BasinNames==BasinName)] / sum(BasinAreas)
      AggregatedStreamflowData[Ptr,"Streamflow_m"] = AggregatedStreamflowData[Ptr,"Streamflow_m"] + NewData
    }
  }
}


################################################################################

WaterBalanceData = data.frame(Scenario=AggregatedData$Scenario,
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
print(paste0(signif(100*mean(Residuals),2)," cm/yr avg. Error"))

write.csv(WaterBalanceData,"AverageWaterBalanceData.csv")
