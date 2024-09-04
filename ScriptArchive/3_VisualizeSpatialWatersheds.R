# Visualize streamflow records for HUC12s (includes some other watersheds)

library(raster)
library(ggplot2)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Results"
setwd(dir)

# Setup
Scenarios = 1:7
Climates = c("cnrm", "miroc")
ClimateNames = c("CNRM-CM5 RCP 8.5", "MIROC5 RCP 8.5")
LANDISruns = c(1,4)

# Look-up table of important information
HUCsavePoints = read.csv("TCSI_HUC12savePoints.csv")[,-1]
head(HUCsavePoints)

# Map of enumerated watersheds
WatershedRast = raster("TCSI_HUC12ish_Watersheds.tif")

################################################################################
# Aggregate yearly MACA data from MACAprocessForDHSVM.R

library(ncdf4)

ClimateNames = c("CNRM-CM5 RCP 8.5", "MIROC5 RCP 8.5")

resultsdir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/MACA/MetSim/output/"

domaindir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/WatershedBoundaries/"

dir = paste0("C:/Users/board/Desktop/DHSVM/TCSI_Setup/MACA/",GCM,"/StationForcingFiles/",Scenario,"/")
setwd(dir)

FirstYr = 2006
LastYr = 2099

WaterYears = 2015:2099

BasinNames = c("Truckee","Yuba","Bear","American")

BasinAreas = c(280558, # Truckee basin
               424083, # Yuba basin
               72275,  # Bear basin
               594092) # American basin
BasinAreas = BasinAreas * 90^2 # Cell counts of basin masks to m^2

YearlyClimateStats = data.frame(matrix(0, nrow=length(WaterYears)*length(ClimateNames), ncol=8))
names(YearlyClimateStats) = c("Climate","WaterYear","Temp","Wind","Humid","Short","Long","Prec")
YearlyClimateStats$WaterYear = sort(rep(WaterYears, length(ClimateNames)))
YearlyClimateStats$Climate = rep(ClimateNames, length(WaterYears))

for (ClimateName in ClimateNames){
  for (BasinNumber in 1:length(BasinNames)){
    BasinName = BasinNames[BasinNumber]
    BasinMaskRaster = raster(paste0(domaindir,"TCSImask_",BasinName,"Domain.tif"))
    
    # Read data from MetSim output
    nc_out = nc_open(paste0(resultsdir,sub(" .*", "", ClimateName),"-RCP8.5_",BasinName,"Output_",FirstYr,"0101-",LastYr,"1231.nc"))
    
    lats = ncvar_get(nc_out,"lat")
    lons = ncvar_get(nc_out,"lon")
    
    temp.array = ncvar_get(nc_out,"temp")
    wind.array = ncvar_get(nc_out,"wind")
    rel_humid.array = ncvar_get(nc_out,"rel_humid")
    shortwave.array = ncvar_get(nc_out,"shortwave")
    longwave.array = ncvar_get(nc_out,"longwave")
    prec.array = ncvar_get(nc_out,"prec")
    
    ClimateDates = as.POSIXct(ncvar_get(nc_out,"time")*60,origin=as.POSIXct("2000-01-01",tz="UTC"),tz="UTC")
    
    nc_close(nc_out)
    remove(nc_out)
    
    temp.rast = raster(t(temp.array[,,1]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats),
                       crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    
    BasinMaskRaster = projectRaster(BasinMaskRaster, temp.rast, method="ngb")
    BasinMaskRasterPtrs = which(BasinMaskRaster[,]==1)
    
    # This is kind of a hack but it works, so...
    # length(unique(temp.rast[,]))/length(temp.rast[,]) # This needs to equal 1
    BasinMaskPtrs = which(as.vector(temp.array[,,1]) %in% temp.rast[BasinMaskRasterPtrs])
    stopifnot(mean(temp.array[,,1][BasinMaskPtrs]) == mean(temp.rast[BasinMaskRasterPtrs]))
    
    # Aggregate data to yearly water years
    YrIndex = as.numeric(format(ClimateDates,"%Y"))
    OctNovDecPtrs = which(as.numeric(format(ClimateDates,"%m")) >= 10)
    YrIndex[OctNovDecPtrs] = YrIndex[OctNovDecPtrs] + 1
    
    # Add area-weighted data
    for (yr in WaterYears){
      YrPtrs = which(YrIndex==yr)
      
      YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName &
                            YearlyClimateStats$WaterYear==yr),"Temp"] = YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName &
                                                                                              YearlyClimateStats$WaterYear==yr),"Temp"] + mean(apply(temp.array[,,YrPtrs], c(1,2), mean)[BasinMaskPtrs]) * BasinAreas[BasinNumber] / sum(BasinAreas)
      YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName &
                            YearlyClimateStats$WaterYear==yr),"Wind"] = YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName &
                                                                                              YearlyClimateStats$WaterYear==yr),"Wind"] + mean(apply(wind.array[,,YrPtrs], c(1,2), mean)[BasinMaskPtrs]) * BasinAreas[BasinNumber] / sum(BasinAreas)
      YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName &
                            YearlyClimateStats$WaterYear==yr),"Humid"] = YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName &
                                                                                               YearlyClimateStats$WaterYear==yr),"Humid"] + mean(apply(rel_humid.array[,,YrPtrs], c(1,2), mean)[BasinMaskPtrs]) * BasinAreas[BasinNumber] / sum(BasinAreas)
      YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName &
                            YearlyClimateStats$WaterYear==yr),"Short"] = YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName &
                                                                                               YearlyClimateStats$WaterYear==yr),"Short"] + mean(apply(shortwave.array[,,YrPtrs], c(1,2), mean)[BasinMaskPtrs]) * BasinAreas[BasinNumber] / sum(BasinAreas)
      YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName &
                            YearlyClimateStats$WaterYear==yr),"Long"] = YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName &
                                                                                              YearlyClimateStats$WaterYear==yr),"Long"] + mean(apply(longwave.array[,,YrPtrs], c(1,2), mean)[BasinMaskPtrs]) * BasinAreas[BasinNumber] / sum(BasinAreas)
      YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName &
                            YearlyClimateStats$WaterYear==yr),"Prec"] = YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName &
                                                                                              YearlyClimateStats$WaterYear==yr),"Prec"] + mean(apply(prec.array[,,YrPtrs], c(1,2), mean)[BasinMaskPtrs]) * BasinAreas[BasinNumber] / sum(BasinAreas)
      
      print(yr)
    }
    
  }
}

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Results"
setwd(dir)

write.csv(YearlyClimateStats, "WaterYear_MACAclimateStats_TCSIbasins.csv")

################################################################################
# Aggregate HUC12 streamflow to yearly values

WaterYears = 2015:2099

########### Calculate water year average streamflow for each watershed in each run case

DHSVMmodels = c(195,270,276)
Scenarios = 1:7
Climates = c("cnrm", "miroc")
ClimateNames = c("CNRM-CM5 RCP 8.5", "MIROC5 RCP 8.5")
LANDIsruns = c(1,4)

if (exists("WaterYearStreamData")){
  remove(WaterYearStreamData)
}

for (DHSVMmodel in DHSVMmodels){
  for (Scenario in Scenarios){
    for (Climate in Climates){
      
      LANDIsrun = LANDIsruns[which(Climates==Climate)]
      ClimateName = ClimateNames[which(Climates==Climate)]
      RunCase = paste0("P",DHSVMmodel,"S",Scenario,Climate,"R",LANDIsrun)
      
      # Load streamflow data
      DHSVMstreamflow = read.csv(paste0(RunCase,"_WSHDsaveMatrix.csv"))
      DHSVMstreamflow$DateTime = as.Date(DHSVMstreamflow$DateTime)
      
      for (WaterYear in WaterYears){
        
        # Aggregate 3-hourly to full water year
        YrPtrs = which(DHSVMstreamflow$DateTime >= paste0(WaterYear-1,"-10-01") &
                       DHSVMstreamflow$DateTime < paste0(WaterYear,"-10-01"))
        
        for (wshd in sort(unique(HUCsavePoints$Watershed_Num))){
          
          YearlyAvgQ = mean(DHSVMstreamflow[YrPtrs,paste0("WSHD_",wshd)])
          
          ########## Take out upstream watershed(s)
          YearlyLocalAvgQ = YearlyAvgQ
          
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
            InboundYearlyAvgQ = mean(DHSVMstreamflow[YrPtrs,paste0("WSHD_",inbound)])
            YearlyLocalAvgQ = YearlyLocalAvgQ - InboundYearlyAvgQ
          }
          
          if (!exists("WaterYearStreamDataChunk")){
            WaterYearStreamDataChunk = data.frame(WaterYear=WaterYear,
                                         WatershedNum=wshd,
                                         DHSVMmodel=DHSVMmodel,
                                         Scenario=Scenario,
                                         Climate=ClimateName,
                                         YearlyAvgQ_cms=YearlyAvgQ,
                                         YearlyLocalAvgQ_cms=YearlyLocalAvgQ)
          } else {
            WaterYearStreamDataChunk = rbind(WaterYearStreamDataChunk,
                                    data.frame(WaterYear=WaterYear,
                                               WatershedNum=wshd,
                                               DHSVMmodel=DHSVMmodel,
                                               Scenario=Scenario,
                                               Climate=ClimateName,
                                               YearlyAvgQ_cms=YearlyAvgQ,
                                               YearlyLocalAvgQ_cms=YearlyLocalAvgQ))
          }
          
        }
        
        print(WaterYear)
      }
      
      # Only rbind the bigger dataframe infrequently
      if (!exists("WaterYearStreamData")){
        WaterYearStreamData = WaterYearStreamDataChunk
      } else {
        WaterYearStreamData = rbind(WaterYearStreamData,WaterYearStreamDataChunk)
      }
      remove(WaterYearStreamDataChunk)
      
      print(paste("Done with",Climate,Scenario,DHSVMmodel))
    }
  }
}

# PreProcessedWaterYearStreamData = read.csv("WaterYearWatershedStreamflowResults.csv")[,-1]
# 
# WaterYearStreamData = rbind(PreProcessedWaterYearStreamData[,1:7],WaterYearStreamData)

write.csv(WaterYearStreamData,"WaterYearWatershedStreamflowResults.csv")

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

WaterYearStreamData$YearlyAvgQ_mmd = NA
WaterYearStreamData$RelYearlyAvgQ_mmd = NA
WaterYearStreamData$RelYearlyAvgQ_pct = NA
WaterYearStreamData$S2RelYearlyAvgQ_mmd = NA
WaterYearStreamData$S2RelYearlyAvgQ_pct = NA

WaterYearStreamData$YearlyLocalAvgQ_mmd = NA
WaterYearStreamData$RelYearlyLocalAvgQ_mmd = NA
WaterYearStreamData$RelYearlyLocalAvgQ_pct = NA
WaterYearStreamData$S2RelYearlyLocalAvgQ_mmd = NA
WaterYearStreamData$S2RelYearlyLocalAvgQ_pct = NA

for (wshd in sort(unique(HUCsavePoints$Watershed_Num))){
  
  UpstreamArea = WatershedArea(wshd)
  WshdArea = length(which(WatershedRast[,]==wshd)) * 90^2 # m^2
  
  WshdPtrs = which(WaterYearStreamData$WatershedNum==wshd)
  
  # Convert to mm/d
  WaterYearStreamData$YearlyAvgQ_mmd[WshdPtrs] = (WaterYearStreamData$YearlyAvgQ_cms[WshdPtrs] / UpstreamArea) * 1000 * 60 * 60 * 24
  WaterYearStreamData$YearlyLocalAvgQ_mmd[WshdPtrs] = (WaterYearStreamData$YearlyLocalAvgQ_cms[WshdPtrs] / WshdArea) * 1000 * 60 * 60 * 24
  
  for (ClimateName in ClimateNames){
    for (WaterYear in WaterYears){
      for (DHSVMmodel in DHSVMmodels){
        
        Ptrs = which(WaterYearStreamData$DHSVMmodel==DHSVMmodel &
                       WaterYearStreamData$Climate==ClimateName &
                       WaterYearStreamData$WaterYear==WaterYear &
                       WaterYearStreamData$WatershedNum==wshd)
        
        AvgYearlyQ = mean(WaterYearStreamData$YearlyAvgQ_mmd[Ptrs])
        AvgLocalYearlyQ = mean(WaterYearStreamData$YearlyLocalAvgQ_mmd[Ptrs])
        
        for (Scenario in Scenarios){
          
          Ptr = Ptrs[which(WaterYearStreamData$Scenario[Ptrs]==Scenario)]
          S2Ptr = Ptrs[which(WaterYearStreamData$Scenario[Ptrs]==2)]
          
          # Relative to mean
          WaterYearStreamData$RelYearlyAvgQ_mmd[Ptr] = WaterYearStreamData$YearlyAvgQ_mmd[Ptr] - AvgYearlyQ
          WaterYearStreamData$RelYearlyLocalAvgQ_mmd[Ptr] = WaterYearStreamData$YearlyLocalAvgQ_mmd[Ptr] - AvgLocalYearlyQ
          
          WaterYearStreamData$RelYearlyAvgQ_pct[Ptr] = WaterYearStreamData$YearlyAvgQ_mmd[Ptr] / AvgYearlyQ - 1
          WaterYearStreamData$RelYearlyLocalAvgQ_pct[Ptr] = WaterYearStreamData$YearlyLocalAvgQ_mmd[Ptr] / AvgLocalYearlyQ - 1
          
          # Relative to Scenario 2
          WaterYearStreamData$S2RelYearlyAvgQ_mmd[Ptr] = WaterYearStreamData$YearlyAvgQ_mmd[Ptr] - WaterYearStreamData$YearlyAvgQ_mmd[S2Ptr]
          WaterYearStreamData$S2RelYearlyLocalAvgQ_mmd[Ptr] = WaterYearStreamData$YearlyLocalAvgQ_mmd[Ptr] -  WaterYearStreamData$YearlyLocalAvgQ_mmd[S2Ptr]
          
          WaterYearStreamData$S2RelYearlyAvgQ_pct[Ptr] = WaterYearStreamData$YearlyAvgQ_mmd[Ptr] / WaterYearStreamData$YearlyAvgQ_mmd[S2Ptr] - 1
          WaterYearStreamData$S2RelYearlyLocalAvgQ_pct[Ptr] = WaterYearStreamData$YearlyLocalAvgQ_mmd[Ptr] / WaterYearStreamData$YearlyLocalAvgQ_mmd[S2Ptr] - 1
          
        }
      }
    }
  }
  print(wshd)
}

write.csv(WaterYearStreamData, "WaterYearWatershedStreamflowResults.csv")

################################################################################
################################################################################
# Can start here
################################################################################
################################################################################

WaterYearStreamData = read.csv("WaterYearWatershedStreamflowResults.csv")[,-1]
YearlyClimateStats = read.csv("WaterYear_MACAclimateStats_TCSIbasins.csv")[,-1]

# Append yearly precip to each water year streamflow record

WaterYears = 2015:2099
ClimateNames = c("CNRM-CM5 RCP 8.5", "MIROC5 RCP 8.5")

WaterYearStreamData$YearlyAvgPrecip_m = NA

for (ClimateName in ClimateNames){
  for (WaterYear in WaterYears){
    
    Ptrs = which(WaterYearStreamData$WaterYear==WaterYear &
                   WaterYearStreamData$Climate==ClimateName)
    
    ClimPtr = which(YearlyClimateStats$WaterYear==WaterYear &
                      YearlyClimateStats$Climate==ClimateName)
    
    WaterYearStreamData$YearlyAvgPrecip_m[Ptrs] = YearlyClimateStats$Prec[ClimPtr]
    
  }
}

TestPtrs = which(WaterYearStreamData$WatershedNum==1 &
                   WaterYearStreamData$Scenario==1 &
                   WaterYearStreamData$Climate=="CNRM-CM5 RCP 8.5" &
                   WaterYearStreamData$DHSVMmodel==276)
plot(WaterYearStreamData$YearlyAvgPrecip_m[TestPtrs], WaterYearStreamData$YearlyAvgQ_cms[TestPtrs])

# Change model name and change to factor
WaterYearStreamData$Model = WaterYearStreamData$DHSVMmodel
WaterYearStreamData$Model = factor(WaterYearStreamData$Model, levels=sort(unique(WaterYearStreamData$Model)))

# Change scenario to factor
WaterYearStreamData$Scenario = factor(WaterYearStreamData$Scenario, levels=sort(unique(WaterYearStreamData$Scenario)))

# Add watershed x scenario column
WaterYearStreamData$WatershedScenario = paste0(WaterYearStreamData$WatershedNum,WaterYearStreamData$Scenario)

# For plot groups
WaterYearStreamData$YrSc = paste0(WaterYearStreamData$Scenario,WaterYearStreamData$WaterYear)

plot(factor(WaterYearStreamData$WaterYear), WaterYearStreamData$S2RelYearlyLocalAvgQ_pct)

ggplot(data=WaterYearStreamData, aes(x=WaterYear, y=RelYearlyAvgQ_pct)) + 
  geom_violin(aes(color=Scenario, group=YrSc)) +
  scale_shape_manual(values=c(0, 1, 2)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple","magenta")) +
  labs(x="", y="Δ Water Yield, %",
       title="Timing of Forest Thinning Effect\nOn HUC12 Streamflow Generation") +
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
  guides(color=guide_legend(override.aes=list(size=6),byrow=TRUE),
         shape=guide_legend(override.aes=list(size=4, stroke=2),byrow=TRUE)) +
  facet_grid(rows=vars(Climate))

ggplot(data=WaterYearStreamData, aes(x=YearlyAvgPrecip_m, y=RelYearlyAvgQ_pct)) + 
  geom_point(size=1, stroke=1, aes(color=Scenario, shape=Model)) +
  scale_shape_manual(values=c(0, 1, 2)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple","magenta")) +
  labs(x="Yearly Mean Precipitation, m", y="Δ Water Yield, %",
       title="Precipitation Mediation of Water Yield Effect") +
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
  guides(color=guide_legend(override.aes=list(size=6),byrow=TRUE),
         shape=guide_legend(override.aes=list(size=4, stroke=2),byrow=TRUE)) +
  facet_grid(rows=vars(Climate))

S6Ptrs = which(WaterYearStreamData$Scenario==6)

ggplot(data=WaterYearStreamData[S6Ptrs,], aes(x=YearlyAvgPrecip_m, y=S1RelYearlyAvgQ_pct)) + 
  geom_bin_2d() +
  scale_fill_continuous(type="viridis") +
  labs(x="Yearly Mean Precipitation, m", y="Δ Water Yield, %",
       title="Precipitation Mediation of Water Yield Effect") +
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
  guides(color=guide_legend(override.aes=list(size=6),byrow=TRUE),
         shape=guide_legend(override.aes=list(size=4, stroke=2),byrow=TRUE)) +
  facet_grid(rows=vars(Climate))

################################################################################
# Identify droughts: 10 (?) years with least precipitation in each climate

nDroughtYears = 10

plot(as.factor(YearlyClimateStats$WaterYear), YearlyClimateStats$Temp)

ClimateNames = c("CNRM-CM5 RCP 8.5", "MIROC5 RCP 8.5")
WaterYears = 2015:2099

DroughtYearData = data.frame(matrix(nrow=nDroughtYears*length(ClimateNames), ncol=3))
names(DroughtYearData) = c("Climate","WaterYear","MeanPrecip_m")
DroughtYearData$Climate = rep(ClimateNames, nDroughtYears)

for (ClimateName in ClimateNames){
  
  ClimatePtrs = which(DroughtYearData$Climate==ClimateName)
  
  # Find n driest values
  DroughtVals = sort(YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName),"Prec"])[1:nDroughtYears]
  DroughtPtrs = match(DroughtVals, YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName),"Prec"])
  
  DroughtYearData$MeanPrecip_m[ClimatePtrs] = YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName),"Prec"][DroughtPtrs]
  
  # Find associated water years
  DroughtYearData$WaterYear[ClimatePtrs] = YearlyClimateStats[(YearlyClimateStats$Climate==ClimateName),"WaterYear"][DroughtPtrs]
  
}

print(DroughtYearData)

hist(DroughtYearData[DroughtYearData$Climate=="CNRM-CM5 RCP 8.5","WaterYear"])
hist(DroughtYearData[DroughtYearData$Climate=="MIROC5 RCP 8.5","WaterYear"])

hist(DroughtYearData[DroughtYearData$Climate=="CNRM-CM5 RCP 8.5","MeanPrecip_m"])
hist(DroughtYearData[DroughtYearData$Climate=="MIROC5 RCP 8.5","MeanPrecip_m"])

################################################################################
# Make maps for the 10 driest vs. all 85 years

OutputDir = "HUC12timingMaps/Drought/"

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

hist(WaterYearStreamData$S1RelYearlyLocalAvgQ_pct)

for (Climate in Climates){
  ClimateName = ClimateNames[which(Climates==Climate)]
  LANDISrun = LANDISruns[which(Climates==Climate)]
  
  DroughtYears = DroughtYearData$WaterYear[DroughtYearData$Climate==ClimateName]
  
  for (Scenario in 7){
    
    DroughtHUC12map = WatershedRast * NA
    DroughtHUC12map195 = WatershedRast * NA
    DroughtHUC12map270 = WatershedRast * NA
    DroughtHUC12map276 = WatershedRast * NA
    
    FullHUC12map = WatershedRast * NA
    FullHUC12map195 = WatershedRast * NA
    FullHUC12map270 = WatershedRast * NA
    FullHUC12map276 = WatershedRast * NA
    
    for (wshd in unique(HUCsavePoints$Watershed_Num)){
      
      DroughtPtrs = which(WaterYearStreamData$WaterYear %in% DroughtYears &
                     WaterYearStreamData$Scenario==Scenario &
                     WaterYearStreamData$Climate==ClimateName &
                     WaterYearStreamData$WatershedNum==wshd)
      
      DroughtPtrs195 = DroughtPtrs[which(WaterYearStreamData$DHSVMmodel[DroughtPtrs]==195)]
      DroughtPtrs270 = DroughtPtrs[which(WaterYearStreamData$DHSVMmodel[DroughtPtrs]==270)]
      DroughtPtrs276 = DroughtPtrs[which(WaterYearStreamData$DHSVMmodel[DroughtPtrs]==276)]
      
      DroughtHUC12map195[which(WatershedRast[,]==wshd)] = mean(WaterYearStreamData$S2RelYearlyLocalAvgQ_pct[DroughtPtrs195])
      DroughtHUC12map270[which(WatershedRast[,]==wshd)] = mean(WaterYearStreamData$S2RelYearlyLocalAvgQ_pct[DroughtPtrs270])
      DroughtHUC12map276[which(WatershedRast[,]==wshd)] = mean(WaterYearStreamData$S2RelYearlyLocalAvgQ_pct[DroughtPtrs276])
      
      FullPtrs = which(WaterYearStreamData$Scenario==Scenario &
                            WaterYearStreamData$Climate==ClimateName &
                            WaterYearStreamData$WatershedNum==wshd)
      
      FullPtrs195 = FullPtrs[which(WaterYearStreamData$DHSVMmodel[FullPtrs]==195)]
      FullPtrs270 = FullPtrs[which(WaterYearStreamData$DHSVMmodel[FullPtrs]==270)]
      FullPtrs276 = FullPtrs[which(WaterYearStreamData$DHSVMmodel[FullPtrs]==276)]
      
      FullHUC12map195[which(WatershedRast[,]==wshd)] = mean(WaterYearStreamData$S2RelYearlyLocalAvgQ_pct[FullPtrs195])
      FullHUC12map270[which(WatershedRast[,]==wshd)] = mean(WaterYearStreamData$S2RelYearlyLocalAvgQ_pct[FullPtrs270])
      FullHUC12map276[which(WatershedRast[,]==wshd)] = mean(WaterYearStreamData$S2RelYearlyLocalAvgQ_pct[FullPtrs276])
      
      
      print(wshd)
    }
    
    DroughtHUC12map[seq4==1] = DroughtHUC12map195[seq4==1]
    DroughtHUC12map[seq4==2] = DroughtHUC12map270[seq4==2]
    DroughtHUC12map[seq4==3] = DroughtHUC12map276[seq4==3]
    
    FullHUC12map[seq4==1] = FullHUC12map195[seq4==1]
    FullHUC12map[seq4==2] = FullHUC12map270[seq4==2]
    FullHUC12map[seq4==3] = FullHUC12map276[seq4==3]
    
    pal = colorRampPalette(c("darkred","red3","red2","red","firebrick1","indianred1","lightcoral","lightpink2","lightpink","mistyrose",
                             "white",
                             "lightsteelblue1","skyblue1","deepskyblue1","dodgerblue","dodgerblue3","blue","blue3","navy","midnightblue","black"))
    
    # Clip % difference to reasonable range
    DroughtHUC12map[DroughtHUC12map[,] > 0.5] = 0.5
    DroughtHUC12map[DroughtHUC12map[,] < -0.5] = -0.5
    FullHUC12map[FullHUC12map[,] > 0.5] = 0.5
    FullHUC12map[FullHUC12map[,] < -0.5] = -0.5
    
    ########## Drought years only
    par(mar=c(5,5,2,0))
    plot(DroughtHUC12map, xlim=c(630000,775000), ylim=c(4200000,4500000), xaxs="i", yaxs="i",
         zlim=c(-0.5,0.5), col=pal(1000), legend=FALSE,
         xlab="Easting", ylab="Northing", legend.width=-2,
         cex.lab=1.5, cex.axis=1.5)
    
    lines(TruckeeWatershed, lwd=2)
    lines(YubaWatershed, lwd=2)
    lines(BearWatershed, lwd=2)
    lines(AmericanWatershed, lwd=2)
    
    text(TruckeeCenter[1]+5000, TruckeeCenter[2]-7000, "Truckee", cex=1.5, font=2)
    text(YubaCenter[1], YubaCenter[2]-2000, "Yuba", cex=1.5, font=2)
    text(BearCenter[1]-5000, BearCenter[2]-2000, "Bear", cex=1.5, font=2)
    text(AmericanCenter[1]+1000, AmericanCenter[2]+1000, "American", cex=1.5, font=2)
    
    text(745000, 4396000, "10 Driest Years", cex=3, font=2)
    
    # Save plots
    dev.print(png, paste0(OutputDir,"DroughtYears_LocalStreamflow_P195-270-276_S",Scenario,"-S2_",Climate,"R",LANDISrun,"_PctDiff.png"),
              width = 1000, height = 878, units = "px", res=96)
    
    ########## Baseline (all years)
    par(mar=c(5,5,2,0))
    plot(FullHUC12map, xlim=c(630000,775000), ylim=c(4200000,4500000), xaxs="i", yaxs="i",
         zlim=c(-0.5,0.5), col=pal(1000), legend=FALSE,
         xlab="Easting", ylab="Northing", legend.width=-2,
         cex.lab=1.5, cex.axis=1.5)
    
    lines(TruckeeWatershed, lwd=2)
    lines(YubaWatershed, lwd=2)
    lines(BearWatershed, lwd=2)
    lines(AmericanWatershed, lwd=2)
    
    text(TruckeeCenter[1]+5000, TruckeeCenter[2]-7000, "Truckee", cex=1.5, font=2)
    text(YubaCenter[1], YubaCenter[2]-2000, "Yuba", cex=1.5, font=2)
    text(BearCenter[1]-5000, BearCenter[2]-2000, "Bear", cex=1.5, font=2)
    text(AmericanCenter[1]+1000, AmericanCenter[2]+1000, "American", cex=1.5, font=2)
    
    text(745000, 4396000, "All 85 Years", cex=3, font=2)
    
    # Save plots
    dev.print(png, paste0(OutputDir,"AllYears_LocalStreamflow_P195-270-276_S",Scenario,"-S2_",Climate,"R",LANDISrun,"_PctDiff.png"),
              width = 1000, height = 878, units = "px", res=96)
  }
}

# Save legend

par(mar=c(5,5,2,10))
plot(FullHUC12map, xlim=c(630000,775000), ylim=c(4200000,4500000), xaxs="i", yaxs="i",
     zlim=c(-0.5,0.5), col=pal(1000), legend=FALSE,
     xlab="Easting", ylab="Northing", legend.width=2,
     cex.lab=1.5, cex.axis=1.5)

plot(FullHUC12map, zlim=c(-0.5,0.5), legend.only=TRUE, col=pal(1000),
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=seq(-0.5, 0.5, 0.1),
                    labels=c("<-50%","-40%","-30%","-20%","-10%","S1","+10%","+20%","+30%","+40%",">+50%"),
                    cex.axis=1.2),
     legend.args=list(text="Δ Streamflow Generation", side=2, line=1, cex=2))

dev.print(png, paste0(OutputDir,"Drought_RelativeStreamflowGeneration_Legend.png"),
          width = 10000, height = 8780, units = "px", res=960)




















