
library(raster)
library(rgdal)

dir = "E:/DHSVM_ScenarioResults/"
setwd(dir)

OutputDir = "C:/Users/board/Desktop/DHSVM/TCSI_Results/SWEresults/"

TahoeMask = raster("C:/Users/board/Desktop/DHSVM/TCSI_Setup/WatershedBoundaries/ModelingWatersheds/LakeTahoeMask.tif")
TahoePtrs = which(TahoeMask[,]==1)

TruckeeWatershed = readOGR("1-TruckeeModelingDomain")
YubaWatershed = readOGR("2-YubaModelingDomain")
BearWatershed = readOGR("3-BearModelingDomain")
AmericanWatershed = readOGR("4-AmericanModelingDomain")

TruckeeCenter = getSpPPolygonsLabptSlots(TruckeeWatershed)
YubaCenter = getSpPPolygonsLabptSlots(YubaWatershed)
BearCenter = getSpPPolygonsLabptSlots(BearWatershed)
AmericanCenter = getSpPPolygonsLabptSlots(AmericanWatershed)

################################################################################
# Calculate pixel-wise peak SWE and April 1 SWE averages across years

Years = 2015:2099
DHSVMmodels = c(195,270,276)
Scenarios = 1:6
Climates = c("cnrm","miroc")
LANDISruns = c(1,4)

for (DHSVMmodel in DHSVMmodels){
  for (Climate in Climates){
    LANDISrun = LANDISruns[which(Climates==Climate)]
    for (Scenario in Scenarios){
      for (Year in Years){
        
        ######### Load rasters
        
        if (Year==Years[1]){
          April1rast = raster(paste0("SWEmaps/April1_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_",Year,".tif"))
          PeakSWErast = raster(paste0("SWEmaps/PeakSWE_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_",Year,".tif"))
        } else {
          April1rast = April1rast + raster(paste0("SWEmaps/April1_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_",Year,".tif"))
          PeakSWErast = PeakSWErast + raster(paste0("SWEmaps/PeakSWE_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_",Year,".tif"))
        }
        
        print(Year)
      }
      
      # Convert from sum to yearly average
      April1rast = April1rast / length(Years)
      PeakSWErast = PeakSWErast / length(Years)
      
      # Save average rasters for model * climate * scenario
      writeRaster(April1rast, paste0(OutputDir,"April1_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_YearlyAvg.tif"))
      writeRaster(PeakSWErast, paste0(OutputDir,"PeakSWE_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_YearlyAvg.tif"))
      
      # Make sure no NAs were introduced anywhere
      if (length(which(is.na(April1rast[,])))>0){
        print("NAs in April 1 raster!")
      }
      if (length(which(is.na(PeakSWErast[,])))>0){
        print("NAs in Peak SWE raster!")
      }
      
      print(paste(DHSVMmodel,Scenario,Climate))
    }
  }
}

# Calculate scenario effect relative to mean

for (DHSVMmodel in DHSVMmodels){
  for (Climate in Climates){
    LANDISrun = LANDISruns[which(Climates==Climate)]
    
    # Calculate mean across scenarios
    for (Scenario in Scenarios){
      
      if (Scenario==Scenarios[1]){
        MeanApril1rast = raster(paste0(OutputDir,"April1_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_YearlyAvg.tif"))
        MeanPeakSWErast = raster(paste0(OutputDir,"PeakSWE_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_YearlyAvg.tif"))
      } else {
        MeanApril1rast = MeanApril1rast + raster(paste0(OutputDir,"April1_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_YearlyAvg.tif"))
        MeanPeakSWErast = MeanPeakSWErast + raster(paste0(OutputDir,"PeakSWE_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_YearlyAvg.tif"))
      }
    }
    
    # Convert from sum to average
    MeanApril1rast = MeanApril1rast / length(Scenarios)
    MeanPeakSWErast = MeanPeakSWErast / length(Scenarios)
    
    # Calculate scenario effect relative to mean
    for (Scenario in Scenarios){
      
      April1rast = raster(paste0(OutputDir,"April1_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_YearlyAvg.tif"))
      PeakSWErast = raster(paste0(OutputDir,"PeakSWE_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_YearlyAvg.tif"))
      
      April1rast_Rel_m = April1rast - MeanApril1rast
      April1rast_Rel_pct = April1rast / MeanApril1rast - 1
      
      PeakSWErast_Rel_m = PeakSWErast - MeanPeakSWErast
      PeakSWErast_Rel_pct = PeakSWErast / MeanPeakSWErast - 1
      
      writeRaster(April1rast_Rel_m, paste0(OutputDir,"April1_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_Rel_m.tif"))
      writeRaster(April1rast_Rel_pct, paste0(OutputDir,"April1_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_Rel_pct.tif"))
      
      writeRaster(PeakSWErast_Rel_m, paste0(OutputDir,"PeakSWE_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_Rel_m.tif"))
      writeRaster(PeakSWErast_Rel_pct, paste0(OutputDir,"PeakSWE_P",DHSVMmodel,"S",Scenario,Climate,"R",LANDISrun,"_Rel_pct.tif"))
      
      print(paste(DHSVMmodel,Scenario,Climate))
    }
  }
}

################################################################################
# Can start here

pal = colorRampPalette(c("darkred","red","firebrick1","lightcoral","lightpink","white","skyblue1","deepskyblue1","dodgerblue3","blue3","navy"))

# Uncertainty for 3 variables
seq1 = c(rep(c(rep(1, 20),rep(2, 20),rep(3, 20)), 37), rep(1, 20), rep(2, 2247-60*37-20))
seq2 = c(rep(c(rep(2, 20),rep(3, 20),rep(1, 20)), 37), rep(2, 20), rep(3, 2247-60*37-20))
seq3 = c(rep(c(rep(3, 20),rep(1, 20),rep(2, 20)), 37), rep(3, 20), rep(1, 2247-60*37-20))
seq4 = c(rep(c(rep(seq1, 20), rep(seq2, 20), rep(seq3, 20)), 23), rep(seq1, 20), rep(seq2, 20), rep(seq3, (1436-60*23-40)))

Scenarios = 1:6
Climates = c("cnrm","miroc")
ClimateNames = c("CNRM-CM5 RCP 8.5", "MIROC5 RCP 8.5")
LANDISruns = c(1,4)

for (Climate in Climates){
  LANDISrun = LANDISruns[which(Climates==Climate)]
  ClimateName = ClimateNames[which(Climates==Climate)]
  
  for (Scenario in Scenarios){
    
    PeakSWErast_Rel_m_195 = raster(paste0(OutputDir,"PeakSWE_P",195,"S",Scenario,Climate,"R",LANDISrun,"_Rel_m.tif"))
    PeakSWErast_Rel_m_270 = raster(paste0(OutputDir,"PeakSWE_P",270,"S",Scenario,Climate,"R",LANDISrun,"_Rel_m.tif"))
    PeakSWErast_Rel_m_276 = raster(paste0(OutputDir,"PeakSWE_P",276,"S",Scenario,Climate,"R",LANDISrun,"_Rel_m.tif"))
    
    PeakSWErast_Rel_m = PeakSWErast_Rel_m_195
    PeakSWErast_Rel_m[seq4==2] = PeakSWErast_Rel_m_270[seq4==2]
    PeakSWErast_Rel_m[seq4==3] = PeakSWErast_Rel_m_276[seq4==3]
    
    par(mar=c(5,5,2,0))
    plot(PeakSWErast_Rel_m, xlim=c(630000,775000), ylim=c(4200000,4500000), xaxs="i", yaxs="i",
         zlim=c(-0.1,0.1), col=pal(1000), legend=FALSE,
         xlab="Easting", ylab="Northing", legend.width=-2,
         cex.main=2, cex.lab=1.5, cex.axis=1.5)
    # main=paste0("Effect of Forest Thinning on Average Annual Peak SWE"),
    
    lines(TruckeeWatershed, lwd=2)
    lines(YubaWatershed, lwd=2)
    lines(BearWatershed, lwd=2)
    lines(AmericanWatershed, lwd=2)
    
    text(745000, 4396000, paste0("Scenario ",Scenario), cex=3, font=2)
    
    text(TruckeeCenter[1]+5000, TruckeeCenter[2]-7000, "Truckee", cex=1.5, font=2)
    text(YubaCenter[1], YubaCenter[2]+2000, "Yuba", cex=1.5, font=2)
    text(BearCenter[1]-2000, BearCenter[2]+2000, "Bear", cex=1.5, font=2)
    text(AmericanCenter[1]-2000, AmericanCenter[2]+1000, "American", cex=1.5, font=2)
    
    # Save plots
    dev.print(png, paste0(OutputDir,"PeakSWE_P195-270-276S",Scenario,Climate,"R",LANDISrun,"_Rel_m.png"),
              width = 1000, height = 878, units = "px", res=96)
  }
  
}


par(mar=c(5,5,2,5))
plot(PeakSWErast_Rel_m, xlim=c(630000,775000), ylim=c(4200000,4500000), xaxs="i", yaxs="i",
     zlim=c(-0.1,0.1), col=pal(1000), legend=FALSE,
     xlab="Easting", ylab="Northing", legend.width=2,
     cex.main=2, cex.lab=1.5, cex.axis=1.5)

plot(PeakSWErast_Rel_m, zlim=c(-0.1,0.1), legend.only=TRUE, col=pal(1000),
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=seq(-0.1, 0.1, 0.05), labels=seq(-0.1, 0.1, 0.05), cex.axis=1.5),
     legend.args=list(text="Δ SWE Depth, m", side=2, line=1, cex=3))

dev.print(png, paste0(OutputDir,"PeakSWE_Legend.png"),
          width = 1000, height = 878, units = "px", res=96)








