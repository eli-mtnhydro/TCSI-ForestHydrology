
library(raster)
library(ggplot2)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Results"
setwd(dir)

# Look-up table of important information
HUCsavePoints = read.csv("TCSI_HUC12savePoints.csv")[,-1]
head(HUCsavePoints)

# Map of enumerated watersheds
WatershedRast = raster("TCSI_HUC12ish_Watersheds.tif")

################################################################################
# Process daily data if necessary

########## Make day-of-water-year info

WaterYears = 2015:2099
DatesOnly = seq(as.Date(paste0("10-01-",WaterYears[1]-1), format="%m-%d-%Y"),
                as.Date(paste0("09-30-",rev(WaterYears)[1]), format="%m-%d-%Y"),
                by="days")

DayOfYear = as.numeric(format(DatesOnly,"%j"))
DayOfWaterYear = DayOfYear + 92 # Day of year --> day of water year
DaysThisYear = as.numeric(format(as.Date(paste0(format(DatesOnly,"%Y"),"-12-31"),format="%Y-%m-%d"),format="%j"))
Ptrs = which(DayOfWaterYear > DaysThisYear)
DayOfWaterYear[Ptrs] = DayOfWaterYear[Ptrs] - DaysThisYear[Ptrs]

PtrsMax365 = which(DayOfWaterYear < 366)
DayOfWaterYear365max = DayOfWaterYear[PtrsMax365]

########## Function to calculate total upstream area

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

########### Calculate daily average streamflow for each watershed in each run case

DHSVMmodels = c(195,270,276)
Scenarios = 1:6
Climates = c("cnrm", "miroc")
ClimateNames = c("CNRM-CM5 RCP 8.5", "MIROC5 RCP 8.5")
LANDIsruns = c(1,4)

if (exists("DailyStreamData")){
  remove(DailyStreamData)
}

for (DHSVMmodel in DHSVMmodels){
  for (Scenario in Scenarios){
    for (Climate in Climates){
      
      LANDIsrun = LANDIsruns[which(Climates==Climate)]
      ClimateName = ClimateNames[which(Climates==Climate)]
      RunCase = paste0("P",DHSVMmodel,"S",Scenario,Climate,"R",LANDIsrun)
      
      # Load streamflow data
      DHSVMstreamflow = read.csv(paste0(RunCase,"_WSHDsaveMatrix.csv"))
      
      for (wshd in sort(unique(HUCsavePoints$Watershed_Num))){
        
        # Aggregate 3-hourly to daily
        DailyTimeseries = colSums(matrix(DHSVMstreamflow[,paste0("WSHD_",wshd)], nrow=8)) / 8
        
        # Drop day 366 to simplify things and organize into daily matrix
        DailyQmatrix = matrix(DailyTimeseries[PtrsMax365], nrow=365)
        
        DailyAvgQ = rowSums(DailyQmatrix) / length(WaterYears)
        
        # plot(DHSVMstreamflow[,paste0("WSHD_",wshd)], type="l")
        # plot(DailyTimeseries, type="l")
        # plot(DayOfWaterYear365max, DailyTimeseries[PtrsMax365], log="y")
        # lines(DailyAvgQ, col="blue")
        
        ########## Take out upstream watershed(s)
        DailyLocalTimeseries = DailyTimeseries
        
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
          InboundDailyTimeseries = colSums(matrix(DHSVMstreamflow[,paste0("WSHD_",inbound)], nrow=8)) / 8
          DailyLocalTimeseries = DailyLocalTimeseries - InboundDailyTimeseries
        }
        
        # Drop day 366 to simplify things and organize into daily matrix
        DailyLocalQmatrix = matrix(DailyLocalTimeseries[PtrsMax365], nrow=365)
        
        DailyLocalAvgQ = rowSums(DailyLocalQmatrix) / length(WaterYears)
        
        # plot(DailyLocalTimeseries, type="l")
        # plot(DayOfWaterYear365max, DailyLocalTimeseries[PtrsMax365])
        # lines(DailyLocalAvgQ, col="blue")
        
        if (!exists("DailyStreamData")){
          DailyStreamData = data.frame(DayOfWaterYear=1:365,
                                       WatershedNum=rep(wshd,365),
                                       DHSVMmodel=rep(DHSVMmodel,365),
                                       Scenario=rep(Scenario,365),
                                       Climate=rep(ClimateName,365),
                                       DailyAvgQ_cms=DailyAvgQ,
                                       DailyLocalAvgQ_cms=DailyLocalAvgQ)
        } else {
          DailyStreamData = rbind(DailyStreamData,
                                  data.frame(DayOfWaterYear=1:365,
                                             WatershedNum=rep(wshd,365),
                                             DHSVMmodel=rep(DHSVMmodel,365),
                                             Scenario=rep(Scenario,365),
                                             Climate=rep(ClimateName,365),
                                             DailyAvgQ_cms=DailyAvgQ,
                                             DailyLocalAvgQ_cms=DailyLocalAvgQ))
        }
        
      }
      print(paste("Done with",Climate,Scenario,DHSVMmodel))
    }
  }
}

write.csv(DailyStreamData,"DailyWatershedStreamflowResults.csv")

################################################################################
# Aggregate to monthly values

DailyStreamData = read.csv("DailyWatershedStreamflowResults.csv")[,-1]

# Numeric month (10, 11, 12, 1, ..., 9) corresponding to each day-of-water-year (365 max)
MonthsOfWaterYear = as.numeric(format(seq(as.Date("2014-10-01",format="%Y-%m-%d"),as.Date("2015-09-30",format="%Y-%m-%d"),by="days"),"%m"))

Months = c(10, 11, 12, 1:9)

DHSVMmodels = c(195,270,276)
Scenarios = 1:6
ClimateNames = c("CNRM-CM5 RCP 8.5", "MIROC5 RCP 8.5")

if (exists("MonthlyStreamData")){
  remove(MonthlyStreamData)
}

for (DHSVMmodel in DHSVMmodels){
  for (Scenario in Scenarios){
    for (ClimateName in ClimateNames){
      for (wshd in sort(unique(HUCsavePoints$Watershed_Num))){
        
        Ptrs = which(DailyStreamData$WatershedNum==wshd &
                       DailyStreamData$Climate==ClimateName &
                       DailyStreamData$Scenario==Scenario &
                       DailyStreamData$DHSVMmodel==DHSVMmodel)
        
        for (Month in Months){
          
          MonthPtrs = Ptrs[which(MonthsOfWaterYear==Month)]
          
          if (!exists("MonthlyStreamData")){
            MonthlyStreamData = data.frame(Month=Month,
                                           WatershedNum=wshd,
                                           DHSVMmodel=DHSVMmodel,
                                           Scenario=Scenario,
                                           Climate=ClimateName,
                                           MonthlyAvgQ_cms=mean(DailyStreamData$DailyAvgQ_cms[MonthPtrs]),
                                           MonthlyLocalAvgQ_cms=mean(DailyStreamData$DailyLocalAvgQ_cms[MonthPtrs]))
          } else {
            MonthlyStreamData = rbind(MonthlyStreamData,
                                      data.frame(Month=Month,
                                                 WatershedNum=wshd,
                                                 DHSVMmodel=DHSVMmodel,
                                                 Scenario=Scenario,
                                                 Climate=ClimateName,
                                                 MonthlyAvgQ_cms=mean(DailyStreamData$DailyAvgQ_cms[MonthPtrs]),
                                                 MonthlyLocalAvgQ_cms=mean(DailyStreamData$DailyLocalAvgQ_cms[MonthPtrs])))
          }
          
        }
      }
      print(paste("Done with",Climate,Scenario,DHSVMmodel))
    }
  }
}

########## Normalize by area and compute scenario effect relative to mean

# Function to calculate total upstream area

HUCsavePoints = read.csv("TCSI_HUC12savePoints.csv")[,-1]

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

MonthlyStreamData$MonthlyAvgQ_mmd = NA
MonthlyStreamData$RelMonthlyAvgQ_mmd = NA
MonthlyStreamData$RelMonthlyAvgQ_pct = NA

MonthlyStreamData$MonthlyLocalAvgQ_mmd = NA
MonthlyStreamData$RelMonthlyLocalAvgQ_mmd = NA
MonthlyStreamData$RelMonthlyLocalAvgQ_pct = NA

for (wshd in sort(unique(HUCsavePoints$Watershed_Num))){
  
  UpstreamArea = WatershedArea(wshd)
  WshdArea = length(which(WatershedRast[,]==wshd)) * 90^2 # m^2
  
  WshdPtrs = which(MonthlyStreamData$WatershedNum==wshd)
  
  # Convert to mm/d
  MonthlyStreamData$MonthlyAvgQ_mmd[WshdPtrs] = (MonthlyStreamData$MonthlyAvgQ_cms[WshdPtrs] / UpstreamArea) * 1000 * 60 * 60 * 24
  MonthlyStreamData$MonthlyLocalAvgQ_mmd[WshdPtrs] = (MonthlyStreamData$MonthlyLocalAvgQ_cms[WshdPtrs] / WshdArea) * 1000 * 60 * 60 * 24
  
  for (ClimateName in ClimateNames){
    for (Month in Months){
      for (DHSVMmodel in DHSVMmodels){
        
        Ptrs = which(MonthlyStreamData$DHSVMmodel==DHSVMmodel &
                       MonthlyStreamData$Climate==ClimateName &
                       MonthlyStreamData$Month==Month &
                       MonthlyStreamData$WatershedNum==wshd)
        
        AvgMonthlyQ = mean(MonthlyStreamData$MonthlyAvgQ_mmd[Ptrs])
        AvgLocalMonthlyQ = mean(MonthlyStreamData$MonthlyLocalAvgQ_mmd[Ptrs])
        
        for (Scenario in Scenarios){
          
          Ptr = Ptrs[which(MonthlyStreamData$Scenario[Ptrs]==Scenario)]
          
          MonthlyStreamData$RelMonthlyAvgQ_mmd[Ptr] = MonthlyStreamData$MonthlyAvgQ_mmd[Ptr] - AvgMonthlyQ
          MonthlyStreamData$RelMonthlyLocalAvgQ_mmd[Ptr] = MonthlyStreamData$MonthlyLocalAvgQ_mmd[Ptr] - AvgLocalMonthlyQ
          
          MonthlyStreamData$RelMonthlyAvgQ_pct[Ptr] = MonthlyStreamData$MonthlyAvgQ_mmd[Ptr] / AvgMonthlyQ - 1
          MonthlyStreamData$RelMonthlyLocalAvgQ_pct[Ptr] = MonthlyStreamData$MonthlyLocalAvgQ_mmd[Ptr] / AvgLocalMonthlyQ - 1
          
        }
      }
    }
  }
  print(wshd)
}

write.csv(MonthlyStreamData, "MonthlyWatershedStreamflowResults.csv")

################################################################################
# Aggregate to seasonal values

DailyStreamData = read.csv("DailyWatershedStreamflowResults.csv")[,-1]

# Numeric month (10, 11, 12, 1, ..., 9) corresponding to each day-of-water-year (365 max)
MonthsOfWaterYear = as.numeric(format(seq(as.Date("2014-10-01",format="%Y-%m-%d"),as.Date("2015-09-30",format="%Y-%m-%d"),by="days"),"%m"))

Seasons = c("Oct.-Dec.","Jan.-Mar.","Apr.-Jun.","Jul.-Sep.")

DHSVMmodels = c(195,270,276)
Scenarios = 1:6
ClimateNames = c("CNRM-CM5 RCP 8.5", "MIROC5 RCP 8.5")

if (exists("SeasonalStreamData")){
  remove(SeasonalStreamData)
}

for (DHSVMmodel in DHSVMmodels){
  for (Scenario in Scenarios){
    for (ClimateName in ClimateNames){
      for (wshd in sort(unique(HUCsavePoints$Watershed_Num))){
        
        Ptrs = which(DailyStreamData$WatershedNum==wshd &
                       DailyStreamData$Climate==ClimateName &
                       DailyStreamData$Scenario==Scenario &
                       DailyStreamData$DHSVMmodel==DHSVMmodel)
        
        for (Season in Seasons){
          
          if (Season=="Oct.-Dec."){
            Months = c(10,11,12)
          } else if (Season=="Jan.-Mar.") {
            Months = c(1,2,3)
          } else if (Season=="Apr.-Jun.") {
            Months = c(4,5,6)
          } else if (Season=="Jul.-Sep.") {
            Months = c(7,8,9)
          }
          
          SeasonPtrs = Ptrs[which(MonthsOfWaterYear %in% Months)]
          
          if (!exists("SeasonalStreamData")){
            SeasonalStreamData = data.frame(Season=Season,
                                           WatershedNum=wshd,
                                           DHSVMmodel=DHSVMmodel,
                                           Scenario=Scenario,
                                           Climate=ClimateName,
                                           SeasonalAvgQ_cms=mean(DailyStreamData$DailyAvgQ_cms[SeasonPtrs]),
                                           SeasonalLocalAvgQ_cms=mean(DailyStreamData$DailyLocalAvgQ_cms[SeasonPtrs]))
          } else {
            SeasonalStreamData = rbind(SeasonalStreamData,
                                      data.frame(Season=Season,
                                                 WatershedNum=wshd,
                                                 DHSVMmodel=DHSVMmodel,
                                                 Scenario=Scenario,
                                                 Climate=ClimateName,
                                                 SeasonalAvgQ_cms=mean(DailyStreamData$DailyAvgQ_cms[SeasonPtrs]),
                                                 SeasonalLocalAvgQ_cms=mean(DailyStreamData$DailyLocalAvgQ_cms[SeasonPtrs])))
          }
          
        }
      }
      print(paste("Done with",Climate,Scenario,DHSVMmodel))
    }
  }
}

########## Normalize by area and compute scenario effect relative to mean

SeasonalStreamData$SeasonalAvgQ_mmd = NA
SeasonalStreamData$RelSeasonalAvgQ_mmd = NA
SeasonalStreamData$RelSeasonalAvgQ_pct = NA
SeasonalStreamData$S1RelSeasonalAvgQ_mmd = NA
SeasonalStreamData$S1RelSeasonalAvgQ_pct = NA

SeasonalStreamData$SeasonalLocalAvgQ_mmd = NA
SeasonalStreamData$RelSeasonalLocalAvgQ_mmd = NA
SeasonalStreamData$RelSeasonalLocalAvgQ_pct = NA
SeasonalStreamData$S1RelSeasonalLocalAvgQ_mmd = NA
SeasonalStreamData$S1RelSeasonalLocalAvgQ_pct = NA

for (wshd in sort(unique(HUCsavePoints$Watershed_Num))){
  
  UpstreamArea = WatershedArea(wshd)
  WshdArea = length(which(WatershedRast[,]==wshd)) * 90^2 # m^2
  
  WshdPtrs = which(SeasonalStreamData$WatershedNum==wshd)
  
  # Convert to mm/d
  SeasonalStreamData$SeasonalAvgQ_mmd[WshdPtrs] = (SeasonalStreamData$SeasonalAvgQ_cms[WshdPtrs] / UpstreamArea) * 1000 * 60 * 60 * 24
  SeasonalStreamData$SeasonalLocalAvgQ_mmd[WshdPtrs] = (SeasonalStreamData$SeasonalLocalAvgQ_cms[WshdPtrs] / WshdArea) * 1000 * 60 * 60 * 24
  
  for (ClimateName in ClimateNames){
    for (Season in Seasons){
      for (DHSVMmodel in DHSVMmodels){
        
        Ptrs = which(SeasonalStreamData$DHSVMmodel==DHSVMmodel &
                       SeasonalStreamData$Climate==ClimateName &
                       SeasonalStreamData$Season==Season &
                       SeasonalStreamData$WatershedNum==wshd)
        
        AvgSeasonalQ = mean(SeasonalStreamData$SeasonalAvgQ_mmd[Ptrs])
        AvgLocalSeasonalQ = mean(SeasonalStreamData$SeasonalLocalAvgQ_mmd[Ptrs])
        
        for (Scenario in Scenarios){
          
          Ptr = Ptrs[which(SeasonalStreamData$Scenario[Ptrs]==Scenario)]
          
          S1Ptr = Ptrs[which(SeasonalStreamData$Scenario[Ptrs]==1)]
          
          # Relative to mean
          SeasonalStreamData$RelSeasonalAvgQ_mmd[Ptr] = SeasonalStreamData$SeasonalAvgQ_mmd[Ptr] - AvgSeasonalQ
          SeasonalStreamData$RelSeasonalLocalAvgQ_mmd[Ptr] = SeasonalStreamData$SeasonalLocalAvgQ_mmd[Ptr] - AvgLocalSeasonalQ
          
          SeasonalStreamData$RelSeasonalAvgQ_pct[Ptr] = SeasonalStreamData$SeasonalAvgQ_mmd[Ptr] / AvgSeasonalQ - 1
          SeasonalStreamData$RelSeasonalLocalAvgQ_pct[Ptr] = SeasonalStreamData$SeasonalLocalAvgQ_mmd[Ptr] / AvgLocalSeasonalQ - 1
          
          # Relative to Scenario 1
          SeasonalStreamData$S1RelSeasonalAvgQ_mmd[Ptr] = SeasonalStreamData$SeasonalAvgQ_mmd[Ptr] - SeasonalStreamData$SeasonalAvgQ_mmd[S1Ptr]
          SeasonalStreamData$S1RelSeasonalLocalAvgQ_mmd[Ptr] = SeasonalStreamData$SeasonalLocalAvgQ_mmd[Ptr] - SeasonalStreamData$SeasonalLocalAvgQ_mmd[S1Ptr]
          
          SeasonalStreamData$S1RelSeasonalAvgQ_pct[Ptr] = SeasonalStreamData$SeasonalAvgQ_mmd[Ptr] / SeasonalStreamData$SeasonalAvgQ_mmd[S1Ptr] - 1
          SeasonalStreamData$S1RelSeasonalLocalAvgQ_pct[Ptr] = SeasonalStreamData$SeasonalLocalAvgQ_mmd[Ptr] / SeasonalStreamData$SeasonalLocalAvgQ_mmd[S1Ptr] - 1
          
        }
      }
    }
  }
  print(wshd)
}

write.csv(SeasonalStreamData, "SeasonalWatershedStreamflowResults.csv")

################################################################################
# Make plots

# Can start here

DHSVMmodels = c(195,270,276)
Scenarios = 1:6
Climates = c("cnrm", "miroc")
ClimateNames = c("CNRM-CM5 RCP 8.5", "MIROC5 RCP 8.5")
LANDISruns = c(1,4)

Months = c(10, 11, 12, 1:9)

MonthlyStreamData = read.csv("MonthlyWatershedStreamflowResults.csv")[,-1]
head(MonthlyStreamData)

# # Potential interesting thing to consider: negative streamflow generation in some months
# NegativeLocalWatersheds = unique(MonthlyStreamData[which(MonthlyStreamData$MonthlyLocalAvgQ_mmd<0),"WatershedNum"])
# HUCsavePoints[HUCsavePoints$Watershed_Num %in% NegativeLocalWatersheds,]
# NegativeWatersheds = WatershedRast * NA
# NegativeWatersheds[WatershedRast[,] > 0] = 0
# NegativeWatersheds[WatershedRast[,] %in% NegativeLocalWatersheds] = 1
# plot(NegativeWatersheds)

# Change categorical variables to factors for predictable plotting
MonthlyStreamData$Month = factor(MonthlyStreamData$Month, levels=Months)
MonthlyStreamData$Model = MonthlyStreamData$DHSVMmodel
MonthlyStreamData$Model = factor(MonthlyStreamData$Model, levels=sort(unique(MonthlyStreamData$Model)))
MonthlyStreamData$Scenario = factor(MonthlyStreamData$Scenario, levels=sort(unique(MonthlyStreamData$Scenario)))
MonthlyStreamData$Climate = factor(MonthlyStreamData$Climate, levels=unique(MonthlyStreamData$Climate))

plot(MonthlyStreamData$Month, MonthlyStreamData$MonthlyLocalAvgQ_mmd)
plot(MonthlyStreamData$Month, MonthlyStreamData$RelMonthlyLocalAvgQ_mmd)
plot(MonthlyStreamData$Month, MonthlyStreamData$RelMonthlyLocalAvgQ_pct, ylim=c(-1,1))
plot(MonthlyStreamData$Scenario, MonthlyStreamData$RelMonthlyLocalAvgQ_mmd)

# Monthly HUC-12 hydrographs

MonthlyStreamData$WshdScClimMod = paste0(MonthlyStreamData$WatershedNum,MonthlyStreamData$Scenario,MonthlyStreamData$Climate,MonthlyStreamData$Model)
MonthlyStreamData$MonSc = paste0(MonthlyStreamData$Scenario,MonthlyStreamData$Month)

ggplot(data=MonthlyStreamData, aes(x=Month, y=RelMonthlyLocalAvgQ_pct)) + 
  geom_hline(yintercept=0) +
  geom_violin(aes(color=Scenario, group=MonSc)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_discrete(breaks=Months,
                     labels=c("Oct.","Nov.","Dec.","Jan.","Feb.","Mar.","Apr.","May","Jun.","Jul.","Aug.","Sep.")) +
  scale_y_continuous(limits=c(-0.3,0.3)) +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color="black"),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_text(size=20, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=20, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  guides(color=guide_legend(override.aes=list(size=6),byrow=TRUE, order=1),
         shape=guide_legend(override.aes=list(size=4, stroke=2),byrow=TRUE)) +
  facet_grid(rows=vars(Climate))

########## Same thing, but seasonally

SeasonalStreamData = read.csv("SeasonalWatershedStreamflowResults.csv")[,-1]
head(SeasonalStreamData)

# Add spaces to season names
Seasons = c("Oct.-Dec.","Jan.-Mar.","Apr.-Jun.","Jul.-Sep.")
SeasonNames = c("Oct. - Dec.","Jan. - Mar.","Apr. - Jun.","Jul. - Sep.")
SeasonalStreamData$SeasonName = gsub("-"," - ",SeasonalStreamData$Season)

# Change categorical variables to factors for predictable plotting
SeasonalStreamData$SeasonName = factor(SeasonalStreamData$SeasonName, levels=SeasonNames)
SeasonalStreamData$Model = SeasonalStreamData$DHSVMmodel
SeasonalStreamData$Model = factor(SeasonalStreamData$Model, levels=sort(unique(SeasonalStreamData$Model)))
SeasonalStreamData$Scenario = factor(SeasonalStreamData$Scenario, levels=sort(unique(SeasonalStreamData$Scenario)))
SeasonalStreamData$Climate = factor(SeasonalStreamData$Climate, levels=unique(SeasonalStreamData$Climate))

plot(SeasonalStreamData$SeasonName, SeasonalStreamData$SeasonalLocalAvgQ_mmd)
plot(SeasonalStreamData$SeasonName, SeasonalStreamData$RelSeasonalLocalAvgQ_mmd)
plot(SeasonalStreamData$SeasonName, SeasonalStreamData$RelSeasonalLocalAvgQ_pct, ylim=c(-1,1))
plot(SeasonalStreamData$SeasonName, SeasonalStreamData$S1RelSeasonalLocalAvgQ_pct, ylim=c(-1,1))

# Seasonal HUC-12 hydrographs

SeasonalStreamData$WshdScClimMod = paste0(SeasonalStreamData$WatershedNum,SeasonalStreamData$Scenario,SeasonalStreamData$Climate,SeasonalStreamData$Model)
SeasonalStreamData$SeasSc = paste0(SeasonalStreamData$Scenario,SeasonalStreamData$Season)

# Relative values (%)
ggplot(data=SeasonalStreamData, aes(x=SeasonName, y=RelSeasonalLocalAvgQ_pct)) + 
  geom_hline(yintercept=0) +
  geom_violin(scale="width", width=0.9, linewidth=0.5, color="black", aes(fill=Scenario, group=SeasSc)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_fill_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_discrete(breaks=SeasonNames,
                   labels=SeasonNames) +
  scale_y_continuous(limits=c(-0.3,0.3),
                     breaks=c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3),
                     labels=c("-30%","-20%","-10%","Mean","+10%","+20%","+30%")) +
  labs(x="", y="Δ Water Yield",
       title="Timing of Local Streamflow Generation") +
  theme_bw() +
  theme(axis.text.x=element_text(size=20, vjust=0.5,color="black"),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=20, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(fill=guide_legend(override.aes=list(linewidth=1),byrow=TRUE, order=1)) +
  facet_grid(rows=vars(Climate))

# Absolute values (mm/d)
ggplot(data=SeasonalStreamData, aes(x=SeasonName, y=RelSeasonalLocalAvgQ_mmd)) + 
  geom_hline(yintercept=0) +
  geom_violin(scale="width", width=0.9, linewidth=0.5, color="black", aes(fill=Scenario, group=SeasSc)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_fill_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_discrete(breaks=SeasonNames,
                   labels=SeasonNames) +
  scale_y_continuous(breaks=c(-0.4,-0.2,0,0.2,0.4,0.6),
                     labels=c("-0.4","-0.2","Mean","+0.2","+0.4","+0.6")) +
  labs(x="", y="Δ Water Yield, mm/d",
       title="Timing of Local Streamflow Generation") +
  theme_bw() +
  theme(axis.text.x=element_text(size=20, vjust=0.5,color="black"),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=20, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(fill=guide_legend(override.aes=list(linewidth=1),byrow=TRUE, order=1)) +
  facet_grid(rows=vars(Climate))

################################################################################
# Seasonal HUC12 water yield change visualized as a map

OutputDir = "HUC12timingMaps/"

plot(WatershedRast)

library(rgdal)

TruckeeWatershed = readOGR("E:/DHSVM_ScenarioResults/1-TruckeeModelingDomain")
YubaWatershed = readOGR("E:/DHSVM_ScenarioResults/2-YubaModelingDomain")
BearWatershed = readOGR("E:/DHSVM_ScenarioResults/3-BearModelingDomain")
AmericanWatershed = readOGR("E:/DHSVM_ScenarioResults/4-AmericanModelingDomain")

TruckeeCenter = getSpPPolygonsLabptSlots(TruckeeWatershed)
YubaCenter = getSpPPolygonsLabptSlots(YubaWatershed)
BearCenter = getSpPPolygonsLabptSlots(BearWatershed)
AmericanCenter = getSpPPolygonsLabptSlots(AmericanWatershed)

# Uncertainty for 3 variables
seq1 = c(rep(c(rep(1, 20),rep(2, 20),rep(3, 20)), 37), rep(1, 20), rep(2, 2247-60*37-20))
seq2 = c(rep(c(rep(2, 20),rep(3, 20),rep(1, 20)), 37), rep(2, 20), rep(3, 2247-60*37-20))
seq3 = c(rep(c(rep(3, 20),rep(1, 20),rep(2, 20)), 37), rep(3, 20), rep(1, 2247-60*37-20))
seq4 = c(rep(c(rep(seq1, 20), rep(seq2, 20), rep(seq3, 20)), 23), rep(seq1, 20), rep(seq2, 20), rep(seq3, (1436-60*23-40)))

# Clip % difference to reasonable range
SeasonalStreamData$S1RelSeasonalLocalAvgQ_pct[SeasonalStreamData$S1RelSeasonalLocalAvgQ_pct > 0.2] = 0.2
SeasonalStreamData$S1RelSeasonalLocalAvgQ_pct[SeasonalStreamData$S1RelSeasonalLocalAvgQ_pct < (-0.2)] = -0.2

hist(SeasonalStreamData$S1RelSeasonalLocalAvgQ_pct)

for (Climate in Climates){
  ClimateName = ClimateNames[which(Climates==Climate)]
  LANDISrun = LANDISruns[which(Climates==Climate)]
  for (Scenario in 2:length(Scenarios)){
    for (Season in Seasons){
      
      SeasonHUC12map = WatershedRast * NA
      SeasonHUC12map195 = WatershedRast * NA
      SeasonHUC12map270 = WatershedRast * NA
      SeasonHUC12map276 = WatershedRast * NA
      
      for (wshd in unique(HUCsavePoints$Watershed_Num)){
        
        Ptrs = which(SeasonalStreamData$Season==Season &
                       SeasonalStreamData$Scenario==Scenario &
                       SeasonalStreamData$Climate==ClimateName &
                       SeasonalStreamData$WatershedNum==wshd)
        
        Ptr195 = Ptrs[which(SeasonalStreamData$DHSVMmodel[Ptrs]==195)]
        Ptr270 = Ptrs[which(SeasonalStreamData$DHSVMmodel[Ptrs]==270)]
        Ptr276 = Ptrs[which(SeasonalStreamData$DHSVMmodel[Ptrs]==276)]
        
        SeasonHUC12map195[which(WatershedRast[,]==wshd)] = SeasonalStreamData$S1RelSeasonalLocalAvgQ_pct[Ptr195]
        SeasonHUC12map270[which(WatershedRast[,]==wshd)] = SeasonalStreamData$S1RelSeasonalLocalAvgQ_pct[Ptr270]
        SeasonHUC12map276[which(WatershedRast[,]==wshd)] = SeasonalStreamData$S1RelSeasonalLocalAvgQ_pct[Ptr276]
        
        print(wshd)
      }
      
      SeasonHUC12map[seq4==1] = SeasonHUC12map195[seq4==1]
      SeasonHUC12map[seq4==2] = SeasonHUC12map270[seq4==2]
      SeasonHUC12map[seq4==3] = SeasonHUC12map276[seq4==3]
      
      pal = colorRampPalette(c("darkred","red","firebrick1","lightcoral","lightpink","white","skyblue1","deepskyblue1","dodgerblue3","blue3","navy"))
      par(mar=c(5,5,4,5))
      
      # plot(SeasonHUC12map, xlim=c(630000,775000), ylim=c(4200000,4500000), xaxs="i", yaxs="i",
      #      zlim=c(-0.2, 0.2), col=pal(1000),
      #      main=paste0("Local Streamflow Generation (2015-2099), ",Season," Only"),
      #      xlab="Easting", ylab="Northing",
      #      legend.width=2, legend.shrink=0.75,
      #      axis.args=list(at=seq(-0.2, 0.2, 0.05),
      #                     labels=c("-20%","-15%","-10%","-5%","Mean","+5%","+10%","+15%","+20%"),
      #                     cex.axis=1.2),
      #      legend.args=list(text=paste0("Δ Streamflow (Relative to Scenario 1)"), side=2, font=2, line=1.5, cex=2),
      #      cex.main=2)
      # text(743000, 4396000, paste0("Scenario ",Scenario," - Scenario 1\n",ClimateName,"\nDHSVM Models 195, 270, 276"), cex=1.2)
      
      par(mar=c(5,5,2,0))
      plot(SeasonHUC12map, xlim=c(630000,775000), ylim=c(4200000,4500000), xaxs="i", yaxs="i",
           zlim=c(-0.2,0.2), col=pal(1000), legend=FALSE,
           xlab="Easting", ylab="Northing", legend.width=-2,
           cex.lab=1.5, cex.axis=1.5)
      
      lines(TruckeeWatershed, lwd=2)
      lines(YubaWatershed, lwd=2)
      lines(BearWatershed, lwd=2)
      lines(AmericanWatershed, lwd=2)
      
      text(TruckeeCenter[1]+5000, TruckeeCenter[2]-7000, "Truckee", cex=1.5, font=2)
      text(YubaCenter[1], YubaCenter[2]+2000, "Yuba", cex=1.5, font=2)
      text(BearCenter[1]-2000, BearCenter[2]+2000, "Bear", cex=1.5, font=2)
      text(AmericanCenter[1]-2000, AmericanCenter[2]+1000, "American", cex=1.5, font=2)
      
      text(745000, 4396000, paste0(Season), cex=4, font=2)
      
      # Save plots
      dev.print(png, paste0(OutputDir,Season,"_LocalStreamflow_P195-270-276_S",Scenario,"-S1_",Climate,"R",LANDISrun,"_PctDiff.png"),
                width = 1000, height = 878, units = "px", res=96)
    }
  }
}

# Save legend

par(mar=c(5,5,2,5))
plot(SeasonHUC12map, xlim=c(630000,775000), ylim=c(4200000,4500000), xaxs="i", yaxs="i",
     zlim=c(-0.2,0.2), col=pal(1000), legend=FALSE,
     xlab="Easting", ylab="Northing", legend.width=2,
     cex.lab=1.5, cex.axis=1.5)

plot(SeasonHUC12map, zlim=c(-0.2,0.2), legend.only=TRUE, col=pal(1000),
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=seq(-0.2, 0.2, 0.05),
                    labels=c("-20%","-15%","-10%","-5%","S1","+5%","+10%","+15%","+20%"),
                    cex.axis=1.5),
     legend.args=list(text="Δ Streamflow Generation", side=2, line=1, cex=2))

dev.print(png, paste0(OutputDir,"RelativeStreamflowGeneration_Legend.png"),
          width = 1000, height = 878, units = "px", res=96)

################################################################################
# Make plot: timing of full domain hydrograph change

DailyStreamData = read.csv("DailyWatershedStreamflowResults.csv")[,-1]
head(DailyStreamData)

# Watersheds: 1 (Yuba), 2 (Bear), 3 (American), 38 + 44 (Truckee)

FullDomainData = DailyStreamData[which(DailyStreamData$WatershedNum==1),c(1,3,4,5,6)]
FullDomainData$DailyAvgQ_cms = FullDomainData$DailyAvgQ_cms + DailyStreamData$DailyAvgQ_cms[which(DailyStreamData$WatershedNum==2)]
FullDomainData$DailyAvgQ_cms = FullDomainData$DailyAvgQ_cms + DailyStreamData$DailyAvgQ_cms[which(DailyStreamData$WatershedNum==3)]
FullDomainData$DailyAvgQ_cms = FullDomainData$DailyAvgQ_cms + DailyStreamData$DailyAvgQ_cms[which(DailyStreamData$WatershedNum==38)]
FullDomainData$DailyAvgQ_cms = FullDomainData$DailyAvgQ_cms + DailyStreamData$DailyAvgQ_cms[which(DailyStreamData$WatershedNum==44)]

head(FullDomainData)

# Change categorical variables to factors for predictable plotting
#FullDomainData$DayOfWaterYear = factor(FullDomainData$DayOfWaterYear, levels=1:365)
FullDomainData$Model = FullDomainData$DHSVMmodel
FullDomainData$Model = factor(FullDomainData$Model, levels=sort(unique(FullDomainData$Model)))
FullDomainData$Scenario = factor(FullDomainData$Scenario, levels=sort(unique(FullDomainData$Scenario)))
FullDomainData$Climate = factor(FullDomainData$Climate, levels=unique(FullDomainData$Climate))

plot(FullDomainData$DayOfWaterYear, FullDomainData$DailyAvgQ_cms)
plot(FullDomainData$Climate, FullDomainData$DailyAvgQ_cms)
plot(FullDomainData$Scenario, FullDomainData$DailyAvgQ_cms)

########## Normalize by area and compute scenario effect relative to mean

FullDomainData$DailyAvgQ_mmd = NA
FullDomainData$RelDailyAvgQ_mmd = NA
FullDomainData$RelDailyAvgQ_pct = NA

WshdArea = length(which(is.finite(WatershedRast[,]))) * 90^2 # m^2

# Convert to mm/d
FullDomainData$DailyAvgQ_mmd = (FullDomainData$DailyAvgQ_cms / WshdArea) * 1000 * 60 * 60 * 24

for (ClimateName in ClimateNames){
  for (DayOfWaterYear in 1:365){
    for (DHSVMmodel in DHSVMmodels){
      
      Ptrs = which(FullDomainData$DHSVMmodel==DHSVMmodel &
                     FullDomainData$Climate==ClimateName &
                     FullDomainData$DayOfWaterYear==DayOfWaterYear)
      
      AvgDailyQ = mean(FullDomainData$DailyAvgQ_mmd[Ptrs])
      
      for (Scenario in Scenarios){
        
        Ptr = Ptrs[which(FullDomainData$Scenario[Ptrs]==Scenario)]
        
        FullDomainData$RelDailyAvgQ_mmd[Ptr] = FullDomainData$DailyAvgQ_mmd[Ptr] - AvgDailyQ
        FullDomainData$RelDailyAvgQ_pct[Ptr] = FullDomainData$DailyAvgQ_mmd[Ptr] / AvgDailyQ - 1
        
      }
    }
  }
}

plot(FullDomainData$DayOfWaterYear, FullDomainData$RelDailyAvgQ_pct)
plot(FullDomainData$Scenario, FullDomainData$RelDailyAvgQ_pct)

FullDomainData$Group = paste0(FullDomainData$Scenario,FullDomainData$Model,FullDomainData$Climate)

FirstDayOfMonth = c(c(274,305,335)-273,c(1,32,60,91,121,152,182,213,244)+92,366)
MonthLabels = c("Oct.","Nov.","Dec.","Jan.","Feb.","Mar.","Apr.","May","Jun.","Jul.","Aug.","Sep.","Oct.")

# Absolute change (mm/d)
ggplot(data=FullDomainData, aes(x=DayOfWaterYear, y=RelDailyAvgQ_mmd)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model, group=Group)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(1,366),
                     expand=c(0,0),
                     breaks=FirstDayOfMonth,
                     labels=MonthLabels) +
  scale_y_continuous(limits=c(-0.2,0.2),
                     breaks=c(-0.2,-0.1,0,0.1,0.2),
                     labels=c("-0.2","-0.1","Mean","+0.1","+0.2")) +
  labs(x="", y="Δ Water Yield, mm / d",
       title="Timing of Full-Domain Water Yield Change") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color="black"),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_text(size=20, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=20, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(Climate))

# Percent change
ggplot(data=FullDomainData, aes(x=DayOfWaterYear, y=RelDailyAvgQ_pct)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model, group=Group)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(1,366),
                     expand=c(0,0),
                     breaks=FirstDayOfMonth,
                     labels=MonthLabels) +
  scale_y_continuous(limits=c(-0.075,0.075),
                     breaks=c(-0.075,-0.05,-0.025,0,0.025,0.05,0.075),
                     labels=c("-7.5%","-5%","-2.5%","Mean","+2.5%","+5%","+7.5%")) +
  labs(x="", y="Δ Water Yield",
       title="Timing of Full-Domain Water Yield Change") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color="black"),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_text(size=20, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=20, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(Climate))

################################################################################
# Quick look at individual watersheds

wshd = 18

SpecificWatershedData = DailyStreamData[which(DailyStreamData$WatershedNum==wshd),c(1,3,4,5,6)]

head(SpecificWatershedData)

# Change categorical variables to factors for predictable plotting
#SpecificWatershedData$DayOfWaterYear = factor(SpecificWatershedData$DayOfWaterYear, levels=1:365)
SpecificWatershedData$Model = SpecificWatershedData$DHSVMmodel
SpecificWatershedData$Model = factor(SpecificWatershedData$Model, levels=sort(unique(SpecificWatershedData$Model)))
SpecificWatershedData$Scenario = factor(SpecificWatershedData$Scenario, levels=sort(unique(SpecificWatershedData$Scenario)))
SpecificWatershedData$Climate = factor(SpecificWatershedData$Climate, levels=unique(SpecificWatershedData$Climate))

########## Normalize by area and compute scenario effect relative to mean

SpecificWatershedData$DailyAvgQ_mmd = NA
SpecificWatershedData$RelDailyAvgQ_mmd = NA
SpecificWatershedData$RelDailyAvgQ_pct = NA

WshdArea = length(which(WatershedRast[,]==wshd)) * 90^2 # m^2

# Convert to mm/d
SpecificWatershedData$DailyAvgQ_mmd = (SpecificWatershedData$DailyAvgQ_cms / WshdArea) * 1000 * 60 * 60 * 24

for (ClimateName in ClimateNames){
  for (DayOfWaterYear in 1:365){
    for (DHSVMmodel in DHSVMmodels){
      
      Ptrs = which(SpecificWatershedData$DHSVMmodel==DHSVMmodel &
                     SpecificWatershedData$Climate==ClimateName &
                     SpecificWatershedData$DayOfWaterYear==DayOfWaterYear)
      
      AvgDailyQ = mean(SpecificWatershedData$DailyAvgQ_mmd[Ptrs])
      
      for (Scenario in Scenarios){
        
        Ptr = Ptrs[which(SpecificWatershedData$Scenario[Ptrs]==Scenario)]
        
        SpecificWatershedData$RelDailyAvgQ_mmd[Ptr] = SpecificWatershedData$DailyAvgQ_mmd[Ptr] - AvgDailyQ
        SpecificWatershedData$RelDailyAvgQ_pct[Ptr] = SpecificWatershedData$DailyAvgQ_mmd[Ptr] / AvgDailyQ - 1
        
      }
    }
  }
}

SpecificWatershedData$Group = paste0(SpecificWatershedData$Scenario,SpecificWatershedData$Model,SpecificWatershedData$Climate)

FirstDayOfMonth = c(c(274,305,335)-273,c(1,32,60,91,121,152,182,213,244)+92,366)
MonthLabels = c("Oct.","Nov.","Dec.","Jan.","Feb.","Mar.","Apr.","May","Jun.","Jul.","Aug.","Sep.","Oct.")

# Absolute change (mm/d)
ggplot(data=SpecificWatershedData, aes(x=DayOfWaterYear, y=RelDailyAvgQ_mmd)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model, group=Group)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(1,366),
                     expand=c(0,0),
                     breaks=FirstDayOfMonth,
                     labels=MonthLabels) +
  labs(x="", y="Δ Water Yield, mm / d",
       title=paste0("Timing of Water Yield Change: ",HUCsavePoints$Name[which(HUCsavePoints$Watershed_Num==wshd)])) +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color="black"),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_text(size=20, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=20, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(Climate))

################################################################################
# Timing of center of mass for relative streamflow change

DailyStreamData = read.csv("DailyWatershedStreamflowResults.csv")[,-1]
head(DailyStreamData)

if (exists("CenterOfChangeData")){
  remove(CenterOfChangeData)
}

for (wshd in sort(unique(HUCsavePoints$Watershed_Num))){
  SpecificWatershedData = DailyStreamData[which(DailyStreamData$WatershedNum==wshd),c(1,3,4,5,6,7)]
  
  head(SpecificWatershedData)
  
  ########## Normalize by area and compute scenario effect relative to mean
  
  SpecificWatershedData$DailyAvgQ_mmd = NA
  SpecificWatershedData$DailyLocalAvgQ_mmd = NA
  SpecificWatershedData$RelDailyLocalAvgQ_mmd = NA
  SpecificWatershedData$RelDailyLocalAvgQ_pct = NA
  
  WshdArea = length(which(WatershedRast[,]==wshd)) * 90^2 # m^2
  
  # Convert to mm/d
  SpecificWatershedData$DailyAvgQ_mmd = (SpecificWatershedData$DailyAvgQ_cms / WshdArea) * 1000 * 60 * 60 * 24
  SpecificWatershedData$DailyLocalAvgQ_mmd = (SpecificWatershedData$DailyLocalAvgQ_cms / WshdArea) * 1000 * 60 * 60 * 24
  
  # Calculate streamflow generation relative to mean
  
  for (ClimateName in ClimateNames){
    for (DayOfWaterYear in 1:365){
      for (DHSVMmodel in DHSVMmodels){
        
        Ptrs = which(SpecificWatershedData$DHSVMmodel==DHSVMmodel &
                       SpecificWatershedData$Climate==ClimateName &
                       SpecificWatershedData$DayOfWaterYear==DayOfWaterYear)
        
        AvgLocalDailyQ = mean(SpecificWatershedData$DailyLocalAvgQ_mmd[Ptrs])
        
        for (Scenario in Scenarios){
          
          Ptr = Ptrs[which(SpecificWatershedData$Scenario[Ptrs]==Scenario)]
          
          SpecificWatershedData$RelDailyLocalAvgQ_mmd[Ptr] = SpecificWatershedData$DailyLocalAvgQ_mmd[Ptr] - AvgLocalDailyQ
          SpecificWatershedData$RelDailyLocalAvgQ_pct[Ptr] = SpecificWatershedData$DailyLocalAvgQ_mmd[Ptr] / AvgLocalDailyQ - 1
          
        }
      }
    }
  }
  
  # Calculate center of mass timing
  
  for (ClimateName in ClimateNames){
    for (Scenario in Scenarios){
      for (DHSVMmodel in DHSVMmodels){
        
        Ptrs = which(SpecificWatershedData$DHSVMmodel==DHSVMmodel &
                       SpecificWatershedData$Climate==ClimateName &
                       SpecificWatershedData$Scenario==Scenario)
        
        # Calculate actual streamflow center of mass timing
        CenterOfStreamflow = weighted.mean(SpecificWatershedData$DayOfWaterYear[Ptrs], SpecificWatershedData$DailyAvgQ_mmd[Ptrs])
        
        # plot(SpecificWatershedData$DayOfWaterYear[Ptrs], SpecificWatershedData$DailyAvgQ_mmd[Ptrs], type="l")
        # points(CenterOfStreamflow,200,col="red")
        
        # Normalize to smallest value to make all non-negative for weighted mean
        SpecificWatershedData$RelDailyLocalAvgQ_mmd[Ptrs] = SpecificWatershedData$RelDailyLocalAvgQ_mmd[Ptrs] - min(SpecificWatershedData$RelDailyLocalAvgQ_mmd[Ptrs])
        
        # Calculate center of mass timing for <relative> change in local streamflow
        CenterOfChange = weighted.mean(SpecificWatershedData$DayOfWaterYear[Ptrs], SpecificWatershedData$RelDailyLocalAvgQ_mmd[Ptrs])
        
        # plot(SpecificWatershedData$DayOfWaterYear[Ptrs], SpecificWatershedData$RelDailyLocalAvgQ_mmd[Ptrs], type="l")
        # points(CenterOfChange,0,col="red")
        
        if (!exists("CenterOfChangeData")){
          CenterOfChangeData = data.frame(WatershedNum=wshd,
                                         DHSVMmodel=DHSVMmodel,
                                         Scenario=Scenario,
                                         Climate=ClimateName,
                                         CenterOfStreamflow_dowy=CenterOfStreamflow,
                                         CenterOfChange_dowy=CenterOfChange)
        } else {
          CenterOfChangeData = rbind(CenterOfChangeData,
                                     data.frame(WatershedNum=wshd,
                                                DHSVMmodel=DHSVMmodel,
                                                Scenario=Scenario,
                                                Climate=ClimateName,
                                                CenterOfStreamflow_dowy=CenterOfStreamflow,
                                                CenterOfChange_dowy=CenterOfChange))
        }
        
      }
    }
  }
  
  print(wshd)
}

# Change categorical variables to factors for predictable plotting
CenterOfChangeData$Model = CenterOfChangeData$DHSVMmodel
CenterOfChangeData$Model = factor(CenterOfChangeData$Model, levels=sort(unique(CenterOfChangeData$Model)))
CenterOfChangeData$Scenario = factor(CenterOfChangeData$Scenario, levels=sort(unique(CenterOfChangeData$Scenario)))
CenterOfChangeData$Climate = factor(CenterOfChangeData$Climate, levels=unique(CenterOfChangeData$Climate))

plot(CenterOfChangeData$WatershedNum, CenterOfChangeData$CenterOfChange_dowy, ylim=c(-365,365))

# Fill into map
CenterOfStreamflowRast = WatershedRast * NA
CenterOfChangeRast = WatershedRast * NA

for (wshd in sort(unique(HUCsavePoints$Watershed_Num))){
  
  # Average across scenario, climate, and model
  
  DataPtrs = which(CenterOfChangeData$WatershedNum==wshd)
  MapPtrs = which(WatershedRast[,]==wshd)
  
  CenterOfStreamflowRast[MapPtrs] = mean(CenterOfChangeData$CenterOfStreamflow_dowy[DataPtrs])
  CenterOfChangeRast[MapPtrs] = mean(CenterOfChangeData$CenterOfChange_dowy[DataPtrs])
  
  print(wshd)
}

plot(CenterOfStreamflowRast)
plot(CenterOfChangeRast)

par(mar=c(5,5,5,5))

plot(CenterOfStreamflowRast, xlim=c(630000,780000), ylim=c(4270000,4420000), xaxs="i", yaxs="i",
     zlim=c(120,200), col=topo.colors(80), legend=FALSE,
     main=paste0("Center of Mass Timing: Total Daily Streamflow"),
     xlab="Easting", ylab="Northing", legend.width=2,
     cex.main=2, cex.lab=1.5, cex.axis=1.5)
plot(CenterOfStreamflowRast, zlim=c(120,200), legend.only=TRUE, col=topo.colors(80),
     legend.width=1, legend.shrink=0.75,
     axis.args=list(cex.axis=1.5),
     legend.args=list(text="Day of Water Year", side=2, line=1, cex=2))

plot(CenterOfChangeRast, xlim=c(630000,780000), ylim=c(4270000,4420000), xaxs="i", yaxs="i",
     zlim=c(120,200), col=topo.colors(80), legend=FALSE,
     main=paste0("Center of Mass Timing: Scenario Effect on Local Generation"),
     xlab="Easting", ylab="Northing", legend.width=2,
     cex.main=2, cex.lab=1.5, cex.axis=1.5)
plot(CenterOfChangeRast, zlim=c(120,200), legend.only=TRUE, col=topo.colors(80),
     legend.width=1, legend.shrink=0.75,
     axis.args=list(cex.axis=1.5),
     legend.args=list(text="Day of Water Year", side=2, line=1, cex=2))











































