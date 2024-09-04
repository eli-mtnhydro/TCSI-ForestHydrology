
library(terra)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Peak_SWE_Maps/"
setwd(dir)

Mask = rast("../../../TCSI_Setup/WatershedBoundaries/TCSImask_MergedDomain.tif")

DeltaPeakSWEcnrm = rast("DeltaPeakSWE_S6-S2_m_cnrm.tif")
DeltaPeakSWEmiroc = rast("DeltaPeakSWE_S6-S2_m_miroc.tif")

PeakSWEcnrmS2 = mean(rast("PeakSWE_P195S2cnrmR1_YearlyAvg.tif"),
                     rast("PeakSWE_P270S2cnrmR1_YearlyAvg.tif"),
                     rast("PeakSWE_P276S2cnrmR1_YearlyAvg.tif"))

PeakSWEmirocS2 = mean(rast("PeakSWE_P195S2mirocR4_YearlyAvg.tif"),
                      rast("PeakSWE_P270S2mirocR4_YearlyAvg.tif"),
                      rast("PeakSWE_P276S2mirocR4_YearlyAvg.tif"))

plot(DeltaPeakSWEcnrm)

MeanDeltaCNRM = mean(unlist(DeltaPeakSWEcnrm[which(Mask[,]==1)]))
MeanDeltaMIROC = mean(unlist(DeltaPeakSWEmiroc[which(Mask[,]==1)]))

MeanCNRM = mean(unlist(PeakSWEcnrmS2[which(Mask[,]==1)]))
MeanMIROC = mean(unlist(PeakSWEmirocS2[which(Mask[,]==1)]))

MeanDeltaCNRM / MeanCNRM # 4.97%
MeanDeltaMIROC / MeanMIROC # 5.61%

quantile(unlist(DeltaPeakSWEcnrm[which(DeltaPeakSWEcnrm[,] > 0 | DeltaPeakSWEcnrm[,] < 0)]),
         c(0.01,0.1,0.25,0.5,0.75,0.9,0.99))
quantile(unlist(DeltaPeakSWEmiroc[which(DeltaPeakSWEmiroc[,] > 0 | DeltaPeakSWEmiroc[,] < 0)]),
         c(0.01,0.1,0.25,0.5,0.75,0.9,0.99))
# Both climates
quantile(c(unlist(DeltaPeakSWEcnrm[which(DeltaPeakSWEcnrm[,] > 0 | DeltaPeakSWEcnrm[,] < 0)]),
           unlist(DeltaPeakSWEmiroc[which(DeltaPeakSWEmiroc[,] > 0 | DeltaPeakSWEmiroc[,] < 0)])),
         c(0.01,0.1,0.25,0.5,0.75,0.9,0.99))

##########
# Restrict to only cells where LAI is lower

Scenario = 2
LAImapCNRM = rast(paste0("../../../TCSI_Setup/LANDIS/Processed_FutureMaps/Scenario",
                         Scenario,"_CNRM-CM5-RCP8.5_Run1_2094_LAIsummer.tif"))
FCmapCNRM = rast(paste0("../../../TCSI_Setup/LANDIS/Processed_FutureMaps/Scenario",
                        Scenario,"_CNRM-CM5-RCP8.5_Run1_2094_CanopyCover.tif"))
LAIFCyr80CNRMs2 = LAImapCNRM * abs(FCmapCNRM)
LAIFCyr80CNRMs2[which(LAIFCyr80CNRMs2[,] < 0)] = 0
LAIFCyr80CNRMs2[which(!is.finite(LAIFCyr80CNRMs2[,]))] = 0
LAImapMIROC = rast(paste0("../../../TCSI_Setup/LANDIS/Processed_FutureMaps/Scenario",
                          Scenario,"_MIROC5-RCP8.5_Run4_2094_LAIsummer.tif"))
FCmapMIROC = rast(paste0("../../../TCSI_Setup/LANDIS/Processed_FutureMaps/Scenario",
                         Scenario,"_MIROC5-RCP8.5_Run4_2094_CanopyCover.tif"))
LAIFCyr80MIROCs2 = LAImapMIROC * abs(FCmapMIROC)
LAIFCyr80MIROCs2[which(LAIFCyr80MIROCs2[,] < 0)] = 0
LAIFCyr80MIROCs2[which(!is.finite(LAIFCyr80MIROCs2[,]))] = 0

Scenario = 6
LAImapCNRM = rast(paste0("../../../TCSI_Setup/LANDIS/Processed_FutureMaps/Scenario",
                         Scenario,"_CNRM-CM5-RCP8.5_Run1_2094_LAIsummer.tif"))
FCmapCNRM = rast(paste0("../../../TCSI_Setup/LANDIS/Processed_FutureMaps/Scenario",
                        Scenario,"_CNRM-CM5-RCP8.5_Run1_2094_CanopyCover.tif"))
LAIFCyr80CNRMs6 = LAImapCNRM * abs(FCmapCNRM)
LAIFCyr80CNRMs6[which(LAIFCyr80CNRMs6[,] < 0)] = 0
LAIFCyr80CNRMs6[which(!is.finite(LAIFCyr80CNRMs6[,]))] = 0
LAImapMIROC = rast(paste0("../../../TCSI_Setup/LANDIS/Processed_FutureMaps/Scenario",
                          Scenario,"_MIROC5-RCP8.5_Run4_2094_LAIsummer.tif"))
FCmapMIROC = rast(paste0("../../../TCSI_Setup/LANDIS/Processed_FutureMaps/Scenario",
                         Scenario,"_MIROC5-RCP8.5_Run4_2094_CanopyCover.tif"))
LAIFCyr80MIROCs6 = LAImapMIROC * abs(FCmapMIROC)
LAIFCyr80MIROCs6[which(LAIFCyr80MIROCs6[,] < 0)] = 0
LAIFCyr80MIROCs6[which(!is.finite(LAIFCyr80MIROCs6[,]))] = 0

DeltaLAIcnrm = LAIFCyr80CNRMs6 - LAIFCyr80CNRMs2
DeltaLAImiroc = LAIFCyr80MIROCs6 - LAIFCyr80MIROCs2

PtrsCNRM = which(DeltaLAIcnrm[,] < 0)
hist(unlist(DeltaPeakSWEcnrm[PtrsCNRM]))
quantile(unlist(DeltaPeakSWEcnrm[PtrsCNRM[which(DeltaPeakSWEcnrm[PtrsCNRM] > 0 | DeltaPeakSWEcnrm[PtrsCNRM] < 0)]]),
         c(0.01,0.1,0.25,0.5,0.75,0.9,0.99))

PtrsMIROC = which(DeltaLAImiroc[,] < 0)
hist(unlist(DeltaPeakSWEmiroc[PtrsMIROC]))
quantile(unlist(DeltaPeakSWEmiroc[PtrsMIROC[which(DeltaPeakSWEmiroc[PtrsMIROC] > 0 | DeltaPeakSWEmiroc[PtrsMIROC] < 0)]]),
         c(0.01,0.1,0.25,0.5,0.75,0.9,0.99))

# Both climates
quantile(c(unlist(DeltaPeakSWEcnrm[PtrsCNRM[which(DeltaPeakSWEcnrm[PtrsCNRM] > 0 | DeltaPeakSWEcnrm[PtrsCNRM] < 0)]]),
           unlist(DeltaPeakSWEmiroc[PtrsMIROC[which(DeltaPeakSWEmiroc[PtrsMIROC] > 0 | DeltaPeakSWEmiroc[PtrsMIROC] < 0)]])),
         c(0.01,0.1,0.25,0.5,0.75,0.9,0.99))









