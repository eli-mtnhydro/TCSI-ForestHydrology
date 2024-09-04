library(raster)
library(rgdal)

BasinNames = c("Truckee","Yuba","Bear","American")
MonthlyLapseRates = data.frame(Month=sort(rep(1:12,4)),Watershed=rep(BasinNames,12),PrecipLapseRate=rep(0,12*4),PrecipLapseRateUnits=rep("m/m"))

# Read the PRISM DEM
dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/"
setwd(dir)
ElevMapRaw = raster("PRISM_us_dem_800m_asc.asc")

# Reproject DEM to the CRS of the redistributed GridMet data
dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/PRISM-GridMet_Multiplier/RescaledGridMet/"
setwd(dir)
PrecipMap = raster(paste0("GridMet_PRISMredistributed_Month1.tif"))
ElevMap = projectRaster(ElevMapRaw,PrecipMap,method="ngb")
plot(ElevMap)

for (basin in 1:4){
  # Read shapefile of model domain
  dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/"
  setwd(dir)
  ModelDomainSPDF = readOGR(dsn=paste0(BasinNames[basin],"Shape"))
  ModelDomainReproj = spTransform(ModelDomainSPDF,crs(PrecipMap))
  
  for (mon in 1:12){
    # Read redistributed GridMet monthly avg. precip
    dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/PRISM-GridMet_Multiplier/RescaledGridMet/"
    setwd(dir)
    PrecipMap = raster(paste0("GridMet_PRISMredistributed_Month",mon,".tif"))
    
    # Exract precip values that are within the model domain
    PrecipInBasin = unlist(extract(PrecipMap, ModelDomainReproj))
    
    # Extract elevation values that ware within the model domain
    ElevInBasin = unlist(extract(ElevMap, ModelDomainReproj))
    
    plot(ElevInBasin,PrecipInBasin)
    
    LinModel = lm(Precip ~ Elev, data=data.frame(Precip=PrecipInBasin,Elev=ElevInBasin))
    LapseRate = coef(LinModel)[2] # mm/m
    MonthlyLapseRates[(MonthlyLapseRates[,"Watershed"]==BasinNames[basin] & MonthlyLapseRates[,"Month"]==mon),]$PrecipLapseRate = LapseRate/1000 # m/m
  }
}

write.csv(MonthlyLapseRates,"TCSImonthlyLapseRates.csv")

# Plot the results
library(ggplot2)

barwidth = 0.8
xLabelOffset = (4*barwidth)/2 - barwidth/2 # center labels under all 4 bars
ggplot() + geom_bar(data=MonthlyLapseRates, mapping=aes(x=Month, y=1000*1000*PrecipLapseRate, fill=Watershed), stat="identity", position="dodge", width=barwidth) +
  labs(fill="Watershed") + labs(title="Precipitation Lapse Rates: gridMet Redistributed with PRISM", x="", y="Precip. Lapse Rate, mm/km") +
  scale_fill_manual(values=c("blue1","green4","gold2","red2")) +
  theme(plot.title=element_text(hjust=0.5,size=14), axis.title=element_text(size=14), axis.text=element_text(size=12,colour="black"), legend.title=element_text(size=14), legend.text=element_text(size=12)) +
  theme(panel.background=element_rect("white", "black", 1), panel.grid=element_blank()) +
  scale_x_continuous(breaks=1:12, labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))






