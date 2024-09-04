
library(ggplot2)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Water_Balance_Partitioning/"
setwd(dir)

WaterBalanceData = read.csv("AverageWaterBalanceData.csv")[,-1]

Climates = c("cnrm","miroc")
ClimateNames = c("CNRM-CM5 RCP 8.5","MIROC5 RCP 8.5")
WaterBalanceData$ClimateName = ClimateNames[match(WaterBalanceData$Climate,Climates)]

# Reformat to ggplot style
WaterBalancePlotData = data.frame(Scenario=rep(WaterBalanceData$Scenario,6),
                                  Climate=rep(WaterBalanceData$ClimateName,6),
                                  WaterDepth_mPerYr=c(WaterBalanceData$OverstoryInterceptionLoss_mPerYr,
                                                      WaterBalanceData$UnderstoryInterceptionLoss_mPerYr,
                                                      WaterBalanceData$OverstoryTranspiration_mPerYr,
                                                      WaterBalanceData$UnderstoryTranspiration_mPerYr,
                                                      WaterBalanceData$SoilEvaporation_mPerYr,
                                                      WaterBalanceData$Runoff_mPerYr),
                                  VarType=c(rep("Overstory Interception Loss",nrow(WaterBalanceData)),
                                            rep("Understory Interception Loss",nrow(WaterBalanceData)),
                                            rep("Overstory Transpiration",nrow(WaterBalanceData)),
                                            rep("Understory Transpiration",nrow(WaterBalanceData)),
                                            rep("Soil Evaporation",nrow(WaterBalanceData)),
                                            rep("Streamflow",nrow(WaterBalanceData))))

WaterBalancePlotData$VarType = factor(WaterBalancePlotData$VarType,
                                      levels=unique(WaterBalancePlotData$VarType))
WaterBalancePlotData$Climate = factor(WaterBalancePlotData$Climate,
                                      levels=unique(WaterBalancePlotData$Climate))

################################################################################
# Simple stacked areas

ggplot(WaterBalancePlotData, aes(x=Scenario, y=WaterDepth_mPerYr, fill=VarType))+
  geom_area() +
  scale_fill_manual(values=c("darkred","red","darkgreen","green","yellow","dodgerblue")) +
  labs(x="Scenario", y="Water Balance Term, m / yr") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black")),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_text(size=28, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm")),
        axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=20),
        legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        legend.title.align=0.5,
        strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90"),
        panel.spacing=unit(1,"cm")) +
  guides(fill=guide_legend(byrow=TRUE, order=1)) +
  facet_grid(rows=vars(Climate))
  
################################################################################
# Stacked areas relative to Scenario 2

RelativeWaterBalancePlotData = WaterBalancePlotData
for (ClimateName in ClimateNames){
  for (VarType in unique(RelativeWaterBalancePlotData$VarType)){
    Ptrs = which(RelativeWaterBalancePlotData$Climate==ClimateName &
                   RelativeWaterBalancePlotData$VarType==VarType)
    S2ptr = Ptrs[which(RelativeWaterBalancePlotData$Scenario[Ptrs]==2)]
    
    S2val = RelativeWaterBalancePlotData$WaterDepth_mPerYr[S2ptr]
    RelativeWaterBalancePlotData$WaterDepth_mPerYr[Ptrs] = RelativeWaterBalancePlotData$WaterDepth_mPerYr[Ptrs] - S2val
  }
}

# Only keep certain scenarios, and rename them
# ChosenScenarios = c(2,4,5,6)
# ScenarioNames = c("Business\nAs Usual","Moderate\nThinning","Heavy\nThinning","Heavy\nThinning w/Fire")
# RelativeWaterBalancePlotData = RelativeWaterBalancePlotData[which(RelativeWaterBalancePlotData$Scenario %in% ChosenScenarios),]
# RelativeWaterBalancePlotData$ScenarioName = ScenarioNames[match(RelativeWaterBalancePlotData$Scenario,ChosenScenarios)]
# RelativeWaterBalancePlotData$ScenarioName = factor(RelativeWaterBalancePlotData$ScenarioName,
#                                                    levels=unique(RelativeWaterBalancePlotData$ScenarioName))

# Don't include soil evap., pretty much just 0 change
RelativeWaterBalancePlotData = RelativeWaterBalancePlotData[-which(RelativeWaterBalancePlotData$VarType=="Soil Evaporation"),]

# Change to cm units
RelativeWaterBalancePlotData$WaterDepth_cmPerYr = 100 * RelativeWaterBalancePlotData$WaterDepth_mPerYr

ggplot(RelativeWaterBalancePlotData, aes(x=Scenario, y=WaterDepth_cmPerYr, fill=VarType))+
  geom_area(color="black", linewidth=1) +
  geom_hline(yintercept=0, color="black", linewidth=1) +
  scale_fill_manual(values=c("goldenrod2","yellow","green4","green","dodgerblue")) +
  scale_x_continuous(breaks=1:6,
                     lab=1:6,
                     expand=c(0,0)) +
  scale_y_continuous(breaks=seq(-10,10,5)) +
  labs(x="Scenario", y="Water Balance Flux, cm / yr") +
  theme_bw() +
  theme(axis.text.x=element_text(size=18, vjust=0.5,color=c("black")),
        axis.text.y=element_text(size=18,color="black"),
        axis.title.x=element_text(size=24, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=24, margin=margin(0,0.5,0,0,"cm")),
        axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=18),
        legend.key.width=unit(1,"cm"),
        legend.key.height=unit(1,"cm"),
        legend.spacing.y=unit(1,"cm"),
        legend.title.align=0.5,
        strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90"),
        panel.spacing=unit(1,"cm")) +
  guides(fill=guide_legend(byrow=TRUE, order=1)) +
  facet_grid(rows=vars(Climate))





















