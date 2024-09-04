
library(raster)
library(ggplot2)

dir = "E:/DHSVM_ScenarioResults/SWEmaps/"
setwd(dir)

OutputDir = "C:/Users/board/Desktop/DHSVM/TCSI_Results/SWEresults/"

################################################################################
# Set up and fill data frame with TCSI-wide aggregated SWE data

# Only run if necessary!

TahoeMask = raster("C:/Users/board/Desktop/DHSVM/TCSI_Setup/WatershedBoundaries/ModelingWatersheds/LakeTahoeMask.tif")
BasinsMask = raster("C:/Users/board/Desktop/DHSVM/TCSI_Setup/WatershedBoundaries/TCSImask_MergedDomain.tif")
InMaskPtrs = which(TahoeMask[,]!=1 & BasinsMask[,]==1)

Years = 2015:2099
DHSVMmodels = c(195,270,276)
Scenarios = 1:6
Climates = c("cnrm", "miroc")
ClimateNames = c("CNRM-CM5 RCP 8.5", "MIROC5 RCP 8.5")
LANDISruns = c(1, 4)

AggregatedSWEdata = data.frame(expand.grid(Years, Scenarios, Climates, DHSVMmodels))
names(AggregatedSWEdata) = c("Year","Scenario","Climate","DHSVMmodel")
AggregatedSWEdata$April1sweMean_m = NA
AggregatedSWEdata$PeakSWEmean_m = NA
AggregatedSWEdata$PeakDateMedian_dowy = NA
AggregatedSWEdata$MeltDateMedian_dowy = NA
AggregatedSWEdata$MeltRateMedian_mmd = NA

for (DHSVMmodel in DHSVMmodels){
  for (Climate in Climates){
    LANDISrun = LANDISruns[which(Climates==Climate)]
    for (Scenario in Scenarios){
      for (Year in Years){
        
        ########## Load rasters
        
        April1rast = raster(paste0("April1_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_",Year,".tif"))
        PeakSWErast = raster(paste0("PeakSWE_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_",Year,".tif"))
        PeakDateRast = raster(paste0("PeakDate_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_",Year,".tif"))
        MeltDateRast = raster(paste0("MeltOutDate_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_",Year,".tif"))
        
        ########## Calculate melt rate
        
        # Logic check
        if (any(PeakDateRast[,]>MeltDateRast[,], na.rm=TRUE)){
          print("Potential issue with melt or peak dates, excluding problem pixels from analysis!")
          PeakSWErast[PeakDateRast[,]>MeltDateRast[,]] = NA
        }
        
        MeltRateRast = PeakSWErast / (MeltDateRast - PeakDateRast)
        #plot(MeltRateRast)
        
        # Only consider melt rate where peak SWE > threshold for melt out date (5 cm)
        MeltRateRast[which(PeakSWErast[,]<=0.05)] = NA
        
        # Only consider melt rate where melt out date is > 1 day after peak date
        #MeltRateRast[which((MeltDateRast - PeakDateRast)[,]<=1)] = NA
        
        # Write melt rate raster
        writeRaster(MeltRateRast, paste0("MeltRate_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_",Year,".tif"))
        
        ########## Fill data
        
        Ptr = which(AggregatedSWEdata$Year==Year &
                      AggregatedSWEdata$Scenario==Scenario &
                      AggregatedSWEdata$Climate==Climate &
                      AggregatedSWEdata$DHSVMmodel==DHSVMmodel)
        
        AggregatedSWEdata[Ptr,]$April1sweMean_m = mean(April1rast[InMaskPtrs], na.rm=TRUE)
        AggregatedSWEdata[Ptr,]$PeakSWEmean_m = mean(PeakSWErast[InMaskPtrs], na.rm=TRUE)
        AggregatedSWEdata[Ptr,]$PeakDateMedian_dowy = median(PeakDateRast[InMaskPtrs], na.rm=TRUE)
        AggregatedSWEdata[Ptr,]$MeltDateMedian_dowy = median(MeltDateRast[InMaskPtrs], na.rm=TRUE)
        AggregatedSWEdata[Ptr,]$MeltRateMedian_mmd = median(MeltRateRast[InMaskPtrs], na.rm=TRUE) * 1000
        
        print(paste(Year,Scenario,Climate,DHSVMmodel))
      }
    }
  }
}

# Compute scenario effect relative to mean for each year * climate * model
AggregatedSWEdata$RelPeakSWEmean_m = NA
AggregatedSWEdata$RelMeltRateMedian_mmd = NA

AggregatedSWEdata$RelPeakSWEmean_pct = NA
AggregatedSWEdata$RelMeltRateMedian_pct = NA

for (DHSVMmodel in DHSVMmodels){
  for (Climate in Climates){
    for (Year in Years){
      
      Ptrs = which(AggregatedSWEdata$DHSVMmodel==DHSVMmodel &
                     AggregatedSWEdata$Climate==Climate &
                     AggregatedSWEdata$Year==Year)
      
      AvgPeakSWE = mean(AggregatedSWEdata$PeakSWEmean_m[Ptrs])
      AvgMeltRate = mean(AggregatedSWEdata$MeltRateMedian_mmd[Ptrs])
      
      for (Scenario in Scenarios){
        
        Ptr = which(AggregatedSWEdata$DHSVMmodel==DHSVMmodel &
                      AggregatedSWEdata$Climate==Climate &
                      AggregatedSWEdata$Year==Year &
                      AggregatedSWEdata$Scenario==Scenario)
        
        AggregatedSWEdata$RelPeakSWEmean_m[Ptr] = AggregatedSWEdata$PeakSWEmean_m[Ptr] - AvgPeakSWE
        AggregatedSWEdata$RelMeltRateMedian_mmd[Ptr] = AggregatedSWEdata$MeltRateMedian_mmd[Ptr] - AvgMeltRate
        
        AggregatedSWEdata$RelPeakSWEmean_pct[Ptr] = AggregatedSWEdata$PeakSWEmean_m[Ptr] / AvgPeakSWE - 1
        AggregatedSWEdata$RelMeltRateMedian_pct[Ptr] = AggregatedSWEdata$MeltRateMedian_mmd[Ptr] / AvgMeltRate - 1
        
      }
      
    }
  }
}

write.csv(AggregatedSWEdata, paste0(OutputDir,"AggregatedSWEdata.csv"))

################################################################################
# Can start here

SWEdata = read.csv(paste0(OutputDir,"AggregatedSWEdata.csv"))[,-1]
head(SWEdata)

plot(SWEdata$Year, SWEdata$PeakSWEmean_m)
plot(SWEdata$Scenario, SWEdata$RelPeakSWEmean_m)
plot(SWEdata$PeakSWEmean_m, SWEdata$RelPeakSWEmean_m)
plot(SWEdata$Year, SWEdata$RelPeakSWEmean_m/SWEdata$PeakSWEmean_m)
plot(SWEdata$Year, SWEdata$RelMeltRateMedian_pct)
plot(SWEdata$RelPeakSWEmean_m, SWEdata$RelPeakSWEmean_pct)
plot(SWEdata$PeakSWEmean_m, SWEdata$RelMeltRateMedian_pct)

# Change climate name
SWEdata$ClimateName = SWEdata$Climate
SWEdata$ClimateName = sub("cnrm", "CNRM-CM5 RCP 8.5", SWEdata$ClimateName)
SWEdata$ClimateName = sub("miroc", "MIROC5 RCP 8.5", SWEdata$ClimateName)

# Change model name and change to factor
SWEdata$Model = SWEdata$DHSVMmodel
SWEdata$Model = factor(SWEdata$Model, levels=unique(SWEdata$Model))

# Change scenario to factor
SWEdata$Scenario = factor(SWEdata$Scenario, levels=unique(SWEdata$Scenario))

########## Peak SWE scenario effect by year: absolute change

ggplot(data=SWEdata, aes(x=Year, y=RelPeakSWEmean_m)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(breaks=c(-0.02,-0.01,0,0.01,0.02,0.03),
                     labels=c("-20","-10","Mean","+10","+20","+30")) +
  labs(x="", y="Δ Peak SWE, mm",
       title="Forest Thinning Effect on Peak SWE") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color="black"),
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
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName))

########## Peak SWE scenario effect by year: percent change

ggplot(data=SWEdata, aes(x=Year, y=RelPeakSWEmean_pct)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(breaks=c(-0.06,-0.04,-0.02,0,0.02,0.04,0.06),
                     labels=c("-6%","-4%","-2%","Mean","+2%","+4%","+6%")) +
  labs(x="", y="Δ Peak SWE",
       title="Forest Thinning Effect on Peak SWE") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color="black"),
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
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName))

########## Melt rate scenario effect by year

ggplot(data=SWEdata, aes(x=Year, y=RelMeltRateMedian_pct)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(breaks=c(-0.15,-0.1,-0.05,0,0.05,0.1,0.15),
                     labels=c("-15%","-10%","-5%","Mean","+5%","+10%","+15%")) +
  labs(x="", y="Δ Melt Rate",
       title="Forest Thinning Effect on Snow Melt Rate") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color="black"),
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
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName))

























