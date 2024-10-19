
dir = "C:/Users/board/Desktop/DHSVM/ForestPaper"
setwd(dir)

WaterData = read.csv("YearlyHUC12data_ForWaterEconAnalysis.csv")

# New Bullards Bar --> 57
# Folsom Lake --> 3

S6NewBullardsBarCNRM = mean(WaterData$Streamflow_TAF[which(WaterData$WatershedNumber==57 &
                                                             WaterData$Scenario==6 &
                                                             WaterData$Climate=="CNRM-CM5 RCP 8.5")])
S2NewBullardsBarCNRM = mean(WaterData$Streamflow_TAF[which(WaterData$WatershedNumber==57 &
                                                             WaterData$Scenario==2 &
                                                             WaterData$Climate=="CNRM-CM5 RCP 8.5")])
S6NewBullardsBarMIROC = mean(WaterData$Streamflow_TAF[which(WaterData$WatershedNumber==57 &
                                                              WaterData$Scenario==6 &
                                                              WaterData$Climate=="MIROC5 RCP 8.5")])
S2NewBullardsBarMIROC = mean(WaterData$Streamflow_TAF[which(WaterData$WatershedNumber==57 &
                                                              WaterData$Scenario==2 &
                                                              WaterData$Climate=="MIROC5 RCP 8.5")])

(S6NewBullardsBarCNRM - S2NewBullardsBarCNRM) / S2NewBullardsBarCNRM
(S6NewBullardsBarMIROC - S2NewBullardsBarMIROC) / S2NewBullardsBarMIROC

(S6NewBullardsBarCNRM - S2NewBullardsBarCNRM)
(S6NewBullardsBarMIROC - S2NewBullardsBarMIROC)

S6FolsomCNRM = mean(WaterData$Streamflow_TAF[which(WaterData$WatershedNumber==3 &
                                                             WaterData$Scenario==6 &
                                                             WaterData$Climate=="CNRM-CM5 RCP 8.5")])
S2FolsomCNRM = mean(WaterData$Streamflow_TAF[which(WaterData$WatershedNumber==3 &
                                                             WaterData$Scenario==2 &
                                                             WaterData$Climate=="CNRM-CM5 RCP 8.5")])
S6FolsomMIROC = mean(WaterData$Streamflow_TAF[which(WaterData$WatershedNumber==3 &
                                                              WaterData$Scenario==6 &
                                                              WaterData$Climate=="MIROC5 RCP 8.5")])
S2FolsomMIROC = mean(WaterData$Streamflow_TAF[which(WaterData$WatershedNumber==3 &
                                                              WaterData$Scenario==2 &
                                                              WaterData$Climate=="MIROC5 RCP 8.5")])

(S6FolsomCNRM - S2FolsomCNRM) / S2FolsomCNRM
(S6FolsomMIROC - S2FolsomMIROC) / S2FolsomMIROC

(S6FolsomCNRM - S2FolsomCNRM)
(S6FolsomMIROC - S2FolsomMIROC)

# Total reservoir gain

(S6FolsomCNRM - S2FolsomCNRM) + (S6NewBullardsBarCNRM - S2NewBullardsBarCNRM)
(S6FolsomMIROC - S2FolsomMIROC) + (S6NewBullardsBarMIROC - S2NewBullardsBarMIROC)
