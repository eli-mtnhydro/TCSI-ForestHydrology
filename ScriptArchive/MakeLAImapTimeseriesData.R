# Convert LANDIS output maps into usable inputs for DHSVM

library(terra)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/LAI_Timeseries"
setwd(dir)

StartYear = 2020 - 5 - 1 # Shift back halfway, then shift to water year (10/1/2014 is WY 2015)
Years = seq(StartYear, StartYear+80, 10)

MapScenarios = 1:6
ClimateNames = c("CNRM-CM5-RCP8.5","MIROC5-RCP8.5")
LANDISruns = c(1,4)

##########
# Aggregate to HUC12

HUC12map = rast("TCSI_HUC12ish_Watersheds.tif")
HUC12data = read.csv("TCSI_HUC12savePoints.csv")[,-1]

HUC12data$Area = NA
for (wshd in HUC12data$Watershed_Num){
  Ptrs = which(HUC12map[,]==wshd)
  HUC12data$Area[which(HUC12data$Watershed_Num==wshd)] = length(Ptrs) * 90^2 / (1000^2) # km^2
}

LAItimeseriesData = data.frame()

for (Year in Years){
  for (Scenario in MapScenarios){
    for (ClimateName in ClimateNames){
      
      LANDISrun = LANDISruns[which(ClimateNames==ClimateName)]
      
      LAItimeseriesDataChunk = data.frame(WatershedNum=HUC12data$Watershed_Num,
                                          Year=Year+6,
                                          Scenario=Scenario,
                                          ClimateName=gsub("-"," ",ClimateName),
                                          MeanLAI=NA)
      
      LAImap = rast(paste0("LAImaps/Scenario",Scenario,"_",ClimateName,"_Run",LANDISrun,"_",Year,"_LAIsummer.tif"))
      LAImap[which(LAImap[,]<0)] = 0
      plot(LAImap, main=paste(Year+6,Scenario,ClimateName))
      
      for (wshd in HUC12data$Watershed_Num){
        
        Ptrs = which(HUC12map[,]==wshd)
        MeanLAI = mean(unlist(LAImap[Ptrs]))
        
        LAItimeseriesDataChunk$MeanLAI[which(LAItimeseriesDataChunk$WatershedNum==wshd)] = MeanLAI
      }
      
      LAItimeseriesData = rbind(LAItimeseriesData, LAItimeseriesDataChunk)
      
      print(paste("Done with:",Year+6,Scenario,ClimateName))
    }
  }
}

write.csv(LAItimeseriesData, "LAItimeseriesDataHUC12.csv")

##########
# Aggregate to HUC8

WatershedNames = unique(HUC12data$Basin)

LAItimeseriesDataHUC8 = data.frame()

for (Year in (Years+6)){
  for (Scenario in MapScenarios){
    for (ClimateName in gsub("-"," ",ClimateNames)){
      
      LAItimeseriesDataHUC8Chunk = data.frame(WatershedName=WatershedNames,
                                              Year=Year,
                                              Scenario=Scenario,
                                              ClimateName=gsub("-"," ",ClimateName),
                                              MeanLAI=0)
      
      for (WatershedName in WatershedNames){
        
        WatershedWatersheds = HUC12data$Watershed_Num[which(HUC12data$Basin==WatershedName)]
        WatershedAreas = HUC12data$Area[which(HUC12data$Basin==WatershedName)]
        WatershedWeights = WatershedAreas / sum(WatershedAreas)
        
        HUC8ptr = which(LAItimeseriesDataHUC8Chunk$WatershedName==WatershedName &
                          LAItimeseriesDataHUC8Chunk$Year==Year &
                          LAItimeseriesDataHUC8Chunk$Scenario==Scenario &
                          LAItimeseriesDataHUC8Chunk$ClimateName==ClimateName)
        
        for (wshd in WatershedWatersheds){
          
          HUC12ptr = which(LAItimeseriesData$WatershedNum == wshd &
                             LAItimeseriesData$Year==Year &
                             LAItimeseriesData$Scenario==Scenario &
                             LAItimeseriesData$ClimateName==ClimateName)
          
          WeightedLAI = LAItimeseriesData$MeanLAI[HUC12ptr] * WatershedWeights[which(WatershedWatersheds==wshd)]
          
          LAItimeseriesDataHUC8Chunk$MeanLAI[HUC8ptr] = LAItimeseriesDataHUC8Chunk$MeanLAI[HUC8ptr] + WeightedLAI
        }
      }
      
      LAItimeseriesDataHUC8 = rbind(LAItimeseriesDataHUC8, LAItimeseriesDataHUC8Chunk)
      
      print(paste("Done with:",Year,Scenario,ClimateName))
    }
  }
}

write.csv(LAItimeseriesDataHUC8, "LAItimeseriesDataHUC8.csv")
