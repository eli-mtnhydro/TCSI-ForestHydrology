library(raster)

################################################################################
# Create a merged TCSI SWE map for each date with each set of parameters

nRasters = 3 # output 3 dates in each model run

InputFilePrefix = "SWEsensitivity_2-4-6_2016"
OutputYears = rep(2016,nRasters)
OutputMonths = c(2,4,6)
OutputDays = rep(1,nRasters)

BasinNames = c("Truckee","Yuba","Bear","American")

nRows = 1436

HeaderText = "ncols         2247\nnrows         1436\nxllcorner     620190\nyllcorner     4276890\ncellsize      90\nNODATA_value  -9999\n"

# Get Lake Tahoe mask (it is always snow-free)
LakeTahoeMask = raster("C:/Users/board/Desktop/DHSVM/TCSI_Setup/WatershedBoundaries/ModelingWatersheds/LakeTahoeMask.tif")

for (params in 8){
  # Separate each map in each basin for each date for a given set of parameters
  for (BasinNumber in 1:4){
    BasinName = BasinNames[BasinNumber]
    dir = paste0("C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/DHSVM/",BasinNumber,"_",BasinName,"/output/SnowSensitivity/Params",params,"/")
    setwd(dir)
    
    RawRasterStack = read.table(paste0(InputFilePrefix,".txt"))
    
    for (i in 1:nRasters){
      # Subset a raster from the stack
      SingleMap = RawRasterStack[((i-1)*nRows+1):(nRows*i),]
      
      # Write raster to single file with added header
      OutFile = file(paste0("SWE_Params",params,"_",OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i],"_",BasinName,".asc"),"w")
      cat(HeaderText, file=OutFile)
      write.table(SingleMap, file=OutFile, col.names=FALSE, row.names=FALSE)
      close(OutFile)
      remove(OutFile)
    }
  }
  
  # Combine the basin-specific maps into one map for each date for a given set of parameters
  dir = "C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/DHSVM/X.Results/SnowSensitivity/"
  setwd(dir)
  for (i in 1:nRasters){
    
    for (BasinNumber in 1:4){
      BasinName = BasinNames[BasinNumber]
      dir = paste0("C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/DHSVM/",BasinNumber,"_",BasinName,"/output/SnowSensitivity/Params",params,"/")
      
      RastFile = paste0(dir,"SWE_Params",params,"_",OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i],"_",BasinName,".asc")
      if (BasinNumber==1){
        RastMerged = raster(RastFile)
        crs(RastMerged) = CRS("+init=epsg:32610")
      } else {
        RastMerged = max(RastMerged, raster(RastFile))
      }
    }
    
    # Mask out SWE erroneously placed in Lake Tahoe
    RastMerged = RastMerged*(1-LakeTahoeMask)
    
    plot(RastMerged)
    
    writeRaster(RastMerged,paste0("SWE_",OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i],"_Params",params,"_MergedTCSI.tif"),overwrite=TRUE)
  }
}

################################################################################
# Plot graphics comparing the parameter combos across different months

library(raster)
library(ggplot2)

AggrFactor = 5 # For faster plotting

nRasters = 3 # output 3 dates in each model run

dir = "C:/Users/board/Documents/DHSVM/DHSVM-PNNL-master/DHSVM/X.Results/SnowSensitivity/"
setwd(dir)

OutputYears = rep(2016,nRasters)
OutputMonths = c(2,4,6)
OutputDays = rep(1,nRasters)

# Get TCSI mask to compute SWE average depth
TCSImask = raster("C:/Users/board/Desktop/DHSVM/TCSI_Setup/Final_Model_Inputs/TCSImask_MergedDomain.tif")

TCSIarea = sum(aggregate(TCSImask,AggrFactor)[,])

##################################
# Snow threshold comparison
##################################
for (i in 1:nRasters){
  Rast0 = aggregate(raster(paste0("SWE_",OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i],"_Params0_MergedTCSI.tif")),AggrFactor)
  RastLow = aggregate(raster(paste0("SWE_",OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i],"_Params1_MergedTCSI.tif")),AggrFactor)
  RastHigh = aggregate(raster(paste0("SWE_",OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i],"_Params2_MergedTCSI.tif")),AggrFactor)
  
  Rast0_DF = as.data.frame(as(Rast0,"SpatialPixelsDataFrame"))
  Rast0_DF$Date = rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),length(Rast0_DF$layer))
  Rast0_DF$Params = rep("Snow Threshold\n2.8 °C",length(Rast0_DF$layer))
  
  RastLow_DF = as.data.frame(as(RastLow,"SpatialPixelsDataFrame"))
  RastLow_DF$Date = rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),length(RastLow_DF$layer))
  RastLow_DF$Params = rep("Snow Threshold\n0 °C",length(RastLow_DF$layer))
  
  RastHigh_DF = as.data.frame(as(RastHigh,"SpatialPixelsDataFrame"))
  RastHigh_DF$Date = rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),length(RastHigh_DF$layer))
  RastHigh_DF$Params = rep("Snow Threshold\n5.6 °C",length(RastHigh_DF$layer))
  
  if (i==1){
    CombinedData = rbind(Rast0_DF,RastLow_DF,RastHigh_DF)
    DataText = data.frame(label=paste0("Avg.\n",c(round(sum(RastLow[,])*1000/TCSIarea),round(sum(Rast0[,])*1000/TCSIarea),round(sum(RastHigh[,])*1000/TCSIarea))," mm"),Date=rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),3),Params=c("Snow Threshold\n0 °C","Snow Threshold\n2.8 °C","Snow Threshold\n5.6 °C"))
  } else {
    CombinedData = rbind(CombinedData,Rast0_DF,RastLow_DF,RastHigh_DF)
    DataText = rbind(DataText,data.frame(label=paste0("Avg.\n",c(round(sum(RastLow[,])*1000/TCSIarea),round(sum(Rast0[,])*1000/TCSIarea),round(sum(RastHigh[,])*1000/TCSIarea))," mm"),Date=rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),3),Params=c("Snow Threshold\n0 °C","Snow Threshold\n2.8 °C","Snow Threshold\n5.6 °C")))
  }
  
}

ggplot() + geom_raster(data=CombinedData, aes(x=x,y=y,fill=layer)) +
  geom_text(data=DataText, mapping=aes(x=680000,y=4300000,label=label),hjust=0,vjust=0.5,size=6) +
  scale_fill_gradientn(colors=c("white","lightskyblue2","deepskyblue1","dodgerblue3","blue3","navy","black"),limits=c(0,2),breaks=seq(0,2,0.5),labels=paste0(seq(0,2,0.5)," m")) +
  guides(fill=guide_colorbar(ticks.colour=NA,frame.colour="black")) +
  coord_equal() +
  labs(fill="SWE") +
  theme_bw() + theme(plot.margin=margin(1,1,1,1),axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank()) +
  theme(legend.position="right",legend.key.height=unit(2,"cm"),legend.title=element_text(size=18), legend.text=element_text(size=18)) +
  theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA)) +
  scale_x_continuous(expand=c(0,0),limits=c(660000,770000)) +
  scale_y_continuous(expand=c(0,0),limits=c(4275000,4407000)) +
  facet_grid(rows=vars(Date),cols=vars(Params),switch="y")

##################################
# Fresh albedo comparison
##################################
for (i in 1:nRasters){
  Rast0 = aggregate(raster(paste0("SWE_",OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i],"_Params0_MergedTCSI.tif")),AggrFactor)
  RastLow = aggregate(raster(paste0("SWE_",OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i],"_Params3_MergedTCSI.tif")),AggrFactor)
  RastHigh = aggregate(raster(paste0("SWE_",OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i],"_Params4_MergedTCSI.tif")),AggrFactor)

  Rast0_DF = as.data.frame(as(Rast0,"SpatialPixelsDataFrame"))
  Rast0_DF$Date = rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),length(Rast0_DF$layer))
  Rast0_DF$Params = rep("Fresh Albedo\n0.84",length(Rast0_DF$layer))
  
  RastLow_DF = as.data.frame(as(RastLow,"SpatialPixelsDataFrame"))
  RastLow_DF$Date = rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),length(RastLow_DF$layer))
  RastLow_DF$Params = rep("Fresh Albedo\n0.69",length(Rast0_DF$layer))
  
  RastHigh_DF = as.data.frame(as(RastHigh,"SpatialPixelsDataFrame"))
  RastHigh_DF$Date = rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),length(RastHigh_DF$layer))
  RastHigh_DF$Params = rep("Fresh Albedo\n0.99",length(Rast0_DF$layer))
  
  if (i==1){
    CombinedData = rbind(Rast0_DF,RastLow_DF,RastHigh_DF)
    DataText = data.frame(label=paste0("Avg.\n",c(round(sum(RastLow[,])*1000/TCSIarea),round(sum(Rast0[,])*1000/TCSIarea),round(sum(RastHigh[,])*1000/TCSIarea))," mm"),Date=rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),3),Params=c("Fresh Albedo\n0.69","Fresh Albedo\n0.84","Fresh Albedo\n0.99"))
  } else {
    CombinedData = rbind(CombinedData,Rast0_DF,RastLow_DF,RastHigh_DF)
    DataText = rbind(DataText,data.frame(label=paste0("Avg.\n",c(round(sum(RastLow[,])*1000/TCSIarea),round(sum(Rast0[,])*1000/TCSIarea),round(sum(RastHigh[,])*1000/TCSIarea))," mm"),Date=rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),3),Params=c("Fresh Albedo\n0.69","Fresh Albedo\n0.84","Fresh Albedo\n0.99")))
  }
  
}

ggplot() + geom_raster(data=CombinedData, aes(x=x,y=y,fill=layer)) +
  geom_text(data=DataText, mapping=aes(x=680000,y=4300000,label=label),hjust=0,vjust=0.5,size=6) +
  scale_fill_gradientn(colors=c("white","lightskyblue2","deepskyblue1","dodgerblue3","blue3","navy","black"),limits=c(0,2),breaks=seq(0,2,0.5),labels=paste0(seq(0,2,0.5)," m")) +
  guides(fill=guide_colorbar(ticks.colour=NA,frame.colour="black")) +
  coord_equal() +
  labs(fill="SWE") +
  theme_bw() + theme(plot.margin=margin(1,1,1,1),axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank()) +
  theme(legend.position="right",legend.key.height=unit(2,"cm"),legend.title=element_text(size=18), legend.text=element_text(size=18)) +
  theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA)) +
  scale_x_continuous(expand=c(0,0),limits=c(660000,770000)) +
  scale_y_continuous(expand=c(0,0),limits=c(4275000,4407000)) +
  facet_grid(rows=vars(Date),cols=vars(Params),switch="y")

##################################
# Albedo accumulation lambda comparison
##################################
for (i in 1:nRasters){
  Rast0 = aggregate(raster(paste0("SWE_",OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i],"_Params0_MergedTCSI.tif")),AggrFactor)
  RastLow = aggregate(raster(paste0("SWE_",OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i],"_Params5_MergedTCSI.tif")),AggrFactor)
  RastHigh = aggregate(raster(paste0("SWE_",OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i],"_Params6_MergedTCSI.tif")),AggrFactor)
  
  Rast0_DF = as.data.frame(as(Rast0,"SpatialPixelsDataFrame"))
  Rast0_DF$Date = rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),length(Rast0_DF$layer))
  Rast0_DF$Params = rep("Albedo Accum. λ\n0.93",length(Rast0_DF$layer))
  
  RastLow_DF = as.data.frame(as(RastLow,"SpatialPixelsDataFrame"))
  RastLow_DF$Date = rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),length(RastLow_DF$layer))
  RastLow_DF$Params = rep("Albedo Accum. λ\n0.87",length(Rast0_DF$layer))
  
  RastHigh_DF = as.data.frame(as(RastHigh,"SpatialPixelsDataFrame"))
  RastHigh_DF$Date = rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),length(RastHigh_DF$layer))
  RastHigh_DF$Params = rep("Albedo Accum. λ\n0.99",length(Rast0_DF$layer))
  
  if (i==1){
    CombinedData = rbind(Rast0_DF,RastLow_DF,RastHigh_DF)
    DataText = data.frame(label=paste0("Avg.\n",c(round(sum(RastLow[,])*1000/TCSIarea),round(sum(Rast0[,])*1000/TCSIarea),round(sum(RastHigh[,])*1000/TCSIarea))," mm"),Date=rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),3),Params=c("Albedo Accum. λ\n0.87","Albedo Accum. λ\n0.93","Albedo Accum. λ\n0.99"))
  } else {
    CombinedData = rbind(CombinedData,Rast0_DF,RastLow_DF,RastHigh_DF)
    DataText = rbind(DataText,data.frame(label=paste0("Avg.\n",c(round(sum(RastLow[,])*1000/TCSIarea),round(sum(Rast0[,])*1000/TCSIarea),round(sum(RastHigh[,])*1000/TCSIarea))," mm"),Date=rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),3),Params=c("Albedo Accum. λ\n0.87","Albedo Accum. λ\n0.93","Albedo Accum. λ\n0.99")))
  }
  
}

ggplot() + geom_raster(data=CombinedData, aes(x=x,y=y,fill=layer)) +
  geom_text(data=DataText, mapping=aes(x=680000,y=4300000,label=label),hjust=0,vjust=0.5,size=6) +
  scale_fill_gradientn(colors=c("white","lightskyblue2","deepskyblue1","dodgerblue3","blue3","navy","black"),limits=c(0,2),breaks=seq(0,2,0.5),labels=paste0(seq(0,2,0.5)," m")) +
  guides(fill=guide_colorbar(ticks.colour=NA,frame.colour="black")) +
  coord_equal() +
  labs(fill="SWE") +
  theme_bw() + theme(plot.margin=margin(1,1,1,1),axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank()) +
  theme(legend.position="right",legend.key.height=unit(2,"cm"),legend.title=element_text(size=18), legend.text=element_text(size=18)) +
  theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA)) +
  scale_x_continuous(expand=c(0,0),limits=c(660000,770000)) +
  scale_y_continuous(expand=c(0,0),limits=c(4275000,4407000)) +
  facet_grid(rows=vars(Date),cols=vars(Params),switch="y")

##################################
# Albedo melting lambda comparison
##################################
for (i in 1:nRasters){
  Rast0 = aggregate(raster(paste0("SWE_",OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i],"_Params0_MergedTCSI.tif")),AggrFactor)
  RastLow = aggregate(raster(paste0("SWE_",OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i],"_Params7_MergedTCSI.tif")),AggrFactor)
  RastHigh = aggregate(raster(paste0("SWE_",OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i],"_Params8_MergedTCSI.tif")),AggrFactor)
  
  Rast0_DF = as.data.frame(as(Rast0,"SpatialPixelsDataFrame"))
  Rast0_DF$Date = rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),length(Rast0_DF$layer))
  Rast0_DF$Params = rep("Albedo Melt λ\n0.88",length(Rast0_DF$layer))
  
  RastLow_DF = as.data.frame(as(RastLow,"SpatialPixelsDataFrame"))
  RastLow_DF$Date = rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),length(RastLow_DF$layer))
  RastLow_DF$Params = rep("Albedo Melt λ\n0.77",length(Rast0_DF$layer))
  
  RastHigh_DF = as.data.frame(as(RastHigh,"SpatialPixelsDataFrame"))
  RastHigh_DF$Date = rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),length(RastHigh_DF$layer))
  RastHigh_DF$Params = rep("Albedo Melt λ\n0.99",length(Rast0_DF$layer))
  
  if (i==1){
    CombinedData = rbind(Rast0_DF,RastLow_DF,RastHigh_DF)
    DataText = data.frame(label=paste0("Avg.\n",c(round(sum(RastLow[,])*1000/TCSIarea),round(sum(Rast0[,])*1000/TCSIarea),round(sum(RastHigh[,])*1000/TCSIarea))," mm"),Date=rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),3),Params=c("Albedo Melt λ\n0.77","Albedo Melt λ\n0.88","Albedo Melt λ\n0.99"))
  } else {
    CombinedData = rbind(CombinedData,Rast0_DF,RastLow_DF,RastHigh_DF)
    DataText = rbind(DataText,data.frame(label=paste0("Avg.\n",c(round(sum(RastLow[,])*1000/TCSIarea),round(sum(Rast0[,])*1000/TCSIarea),round(sum(RastHigh[,])*1000/TCSIarea))," mm"),Date=rep(paste0(OutputMonths[i],"-",OutputDays[i],"-",OutputYears[i]),3),Params=c("Albedo Melt λ\n0.77","Albedo Melt λ\n0.88","Albedo Melt λ\n0.99")))
  }
  
}

ggplot() + geom_raster(data=CombinedData, aes(x=x,y=y,fill=layer)) +
  geom_text(data=DataText, mapping=aes(x=680000,y=4300000,label=label),hjust=0,vjust=0.5,size=6) +
  scale_fill_gradientn(colors=c("white","lightskyblue2","deepskyblue1","dodgerblue3","blue3","navy","black"),limits=c(0,2),breaks=seq(0,2,0.5),labels=paste0(seq(0,2,0.5)," m")) +
  guides(fill=guide_colorbar(ticks.colour=NA,frame.colour="black")) +
  coord_equal() +
  labs(fill="SWE") +
  theme_bw() + theme(plot.margin=margin(1,1,1,1),axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank()) +
  theme(legend.position="right",legend.key.height=unit(2,"cm"),legend.title=element_text(size=18), legend.text=element_text(size=18)) +
  theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA)) +
  scale_x_continuous(expand=c(0,0),limits=c(660000,770000)) +
  scale_y_continuous(expand=c(0,0),limits=c(4275000,4407000)) +
  facet_grid(rows=vars(Date),cols=vars(Params),switch="y")







