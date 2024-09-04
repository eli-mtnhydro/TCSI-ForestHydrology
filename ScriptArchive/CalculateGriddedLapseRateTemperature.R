library(raster)
library(rgdal)

BasinNames = c("Truckee","Yuba","Bear","American")
MonthlyLapseRates = data.frame(Month=sort(rep(1:12,4)),Watershed=rep(BasinNames,12),TempLapseRate=rep(0,12*4),TempLapseRateUnits=rep("degC/m"))

# Read the PRISM DEM
dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/"
setwd(dir)
ElevMap = raster("PRISM_us_dem_800m_asc.asc")

for (basin in 1:4){
  # Read shapefile of model domain
  dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/"
  setwd(dir)
  ModelDomainSPDF = readOGR(dsn=paste0(BasinNames[basin],"Shape"))
  ModelDomainReproj = spTransform(ModelDomainSPDF,crs(ElevMap))
  
  for (mon in 1:12){
    # Read redistributed GridMet monthly avg. precip
    dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/"
    setwd(dir)
    TempMap = raster(paste0("PRISM_tmean_30yr_normal_800mM3_",formatC(mon,width=2,flag="0"),"_asc.asc"))
    
    # Exract precip values that are within the model domain
    TempInBasin = unlist(extract(TempMap, ModelDomainReproj))
    
    # Extract elevation values that ware within the model domain
    ElevInBasin = unlist(extract(ElevMap, ModelDomainReproj))
    
    plot(ElevInBasin,TempInBasin)
    
    LinModel = lm(Temp ~ Elev, data=data.frame(Temp=TempInBasin,Elev=ElevInBasin))
    LapseRate = coef(LinModel)[2] # degC/m
    MonthlyLapseRates[(MonthlyLapseRates[,"Watershed"]==BasinNames[basin] & MonthlyLapseRates[,"Month"]==mon),]$TempLapseRate = LapseRate
  }
}

write.csv(MonthlyLapseRates,"TCSImonthlyTemperatureLapseRatesPRISM.csv")

# Plot the results
library(ggplot2)

barwidth = 0.8
xLabelOffset = (4*barwidth)/2 - barwidth/2 # center labels under all 4 bars
ggplot() + geom_bar(data=MonthlyLapseRates, mapping=aes(x=Month, y=1000*TempLapseRate, fill=Watershed), stat="identity", position="dodge", width=barwidth) +
  labs(fill="Watershed") + labs(title="Temperature Lapse Rates: PRISM Mean Temperature", x="", y="Temp. Lapse Rate, degC/km") +
  scale_fill_manual(values=c("blue1","green4","gold2","red2")) +
  theme(plot.title=element_text(hjust=0.5,size=14), axis.title=element_text(size=14), axis.text=element_text(size=12,colour="black"), legend.title=element_text(size=14), legend.text=element_text(size=12)) +
  theme(panel.background=element_rect("white", "black", 1), panel.grid=element_blank()) +
  scale_x_continuous(breaks=1:12, labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))






