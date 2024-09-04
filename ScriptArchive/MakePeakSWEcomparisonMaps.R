
library(terra)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Peak_SWE_Maps/"
setwd(dir)

################################################################################
# Make maps comparing scenarios with interlaced models

# Uncertainty for 3 variables
seq1 = c(rep(c(rep(1, 20),rep(2, 20),rep(3, 20)), 37), rep(1, 20), rep(2, 2247-60*37-20))
seq2 = c(rep(c(rep(2, 20),rep(3, 20),rep(1, 20)), 37), rep(2, 20), rep(3, 2247-60*37-20))
seq3 = c(rep(c(rep(3, 20),rep(1, 20),rep(2, 20)), 37), rep(3, 20), rep(1, 2247-60*37-20))
seq4 = c(rep(c(rep(seq1, 20), rep(seq2, 20), rep(seq3, 20)), 23), rep(seq1, 20), rep(seq2, 20), rep(seq3, (1436-60*23-40)))

Climates = c("cnrm","miroc")
LANDISruns = c(1,4)
Scenarios = c(6)

for (Climate in Climates){
  
  LANDISrun = LANDISruns[which(Climates==Climate)]
  
  PeakSWEmap195s2 = rast(paste0("PeakSWE_P",195,"S",2,Climate,"R",LANDISrun,"_YearlyAvg.tif"))
  PeakSWEmap270s2 = rast(paste0("PeakSWE_P",270,"S",2,Climate,"R",LANDISrun,"_YearlyAvg.tif"))
  PeakSWEmap276s2 = rast(paste0("PeakSWE_P",276,"S",2,Climate,"R",LANDISrun,"_YearlyAvg.tif"))
  
  for (Scenario in Scenarios){
    
    PeakSWEmap195 = rast(paste0("PeakSWE_P",195,"S",Scenario,Climate,"R",LANDISrun,"_YearlyAvg.tif"))
    PeakSWEmap270 = rast(paste0("PeakSWE_P",270,"S",Scenario,Climate,"R",LANDISrun,"_YearlyAvg.tif"))
    PeakSWEmap276 = rast(paste0("PeakSWE_P",276,"S",Scenario,Climate,"R",LANDISrun,"_YearlyAvg.tif"))
    
    PeakSWEcomparisonMap = ((PeakSWEmap195 - PeakSWEmap195s2) +
                              (PeakSWEmap270 - PeakSWEmap270s2) +
                              (PeakSWEmap276 - PeakSWEmap276s2)) / 3
    
    PeakSWEcomparisonMapInterlaced = 0 * PeakSWEmap195s2
    PeakSWEcomparisonMapInterlaced[which(seq4==1)] = PeakSWEmap195[which(seq4==1)] - PeakSWEmap195s2[which(seq4==1)]
    PeakSWEcomparisonMapInterlaced[which(seq4==2)] = PeakSWEmap270[which(seq4==2)] - PeakSWEmap270s2[which(seq4==2)]
    PeakSWEcomparisonMapInterlaced[which(seq4==3)] = PeakSWEmap276[which(seq4==3)] - PeakSWEmap276s2[which(seq4==3)]
    
    pal = colorRampPalette(c("darkred","red3","red2","red","firebrick1","indianred1","lightcoral","lightpink2","lightpink","mistyrose",
                             "white",
                             "lightsteelblue1","skyblue1","deepskyblue1","dodgerblue","dodgerblue3","blue","blue3","navy","midnightblue","black"))
    
    ########## Drought years only
    plot(PeakSWEcomparisonMap, range=c(-0.5,0.5),
         col=pal(1000), xlab="Easting", ylab="Northing", cex.lab=1.5, cex.axis=1.5)
    
    ##########
    # Save data
    
    writeRaster(PeakSWEcomparisonMap,paste0("DeltaPeakSWE_S",Scenario,"-S2_m_",
                                            Climate,".tif"),overwrite=TRUE)
    
    writeRaster(PeakSWEcomparisonMapInterlaced,paste0("DeltaPeakSWEinterlaced_S",Scenario,"-S2_m_",
                                            Climate,".tif"),overwrite=TRUE)
  }
}
