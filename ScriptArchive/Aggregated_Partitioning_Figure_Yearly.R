
library(ggplot2)
library(ggh4x)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Water_Balance_Partitioning/"
setwd(dir)

WaterBalanceData = read.csv("AverageWaterBalanceData_Yearly.csv")[,-1]

WaterYears = unique(WaterBalanceData$WaterYear)
Climates = c("cnrm","miroc")
#ClimateNames = c("CNRM-CM5 RCP 8.5","MIROC5 RCP 8.5")
ClimateNames = c("Wetter Future Climate","Drier Future Climate")
WaterBalanceData$ClimateName = ClimateNames[match(WaterBalanceData$Climate,Climates)]

# Reformat to ggplot style
WaterBalancePlotData = data.frame(WaterYear=rep(WaterBalanceData$WaterYear,6),
                                  Scenario=rep(WaterBalanceData$Scenario,6),
                                  Climate=rep(WaterBalanceData$ClimateName,6),
                                  WaterDepth_mPerYr=c(WaterBalanceData$OverstoryInterceptionLoss_mPerYr,
                                                      WaterBalanceData$UnderstoryInterceptionLoss_mPerYr,
                                                      WaterBalanceData$OverstoryTranspiration_mPerYr,
                                                      WaterBalanceData$UnderstoryTranspiration_mPerYr,
                                                      WaterBalanceData$SoilEvaporation_mPerYr,
                                                      WaterBalanceData$Runoff_mPerYr),
                                  VarType=c(rep("Overstory\nInterception Loss",nrow(WaterBalanceData)),
                                            rep("Understory\nInterception Loss",nrow(WaterBalanceData)),
                                            rep("Overstory\nTranspiration",nrow(WaterBalanceData)),
                                            rep("Understory\nTranspiration",nrow(WaterBalanceData)),
                                            rep("Soil Evaporation",nrow(WaterBalanceData)),
                                            rep("Streamflow",nrow(WaterBalanceData))))

WaterBalancePlotData$VarType = factor(WaterBalancePlotData$VarType,
                                      levels=c("Soil Evaporation",
                                               "Streamflow",
                                               "Understory\nInterception Loss",
                                               "Understory\nTranspiration",
                                               "Overstory\nInterception Loss",
                                               "Overstory\nTranspiration"))
WaterBalancePlotData$Climate = factor(WaterBalancePlotData$Climate,
                                      levels=unique(WaterBalancePlotData$Climate))

################################################################################
# Stacked areas relative to Scenario 2

RelativeWaterBalancePlotData = WaterBalancePlotData
for (WaterYear in WaterYears){
  for (ClimateName in ClimateNames){
    for (VarType in unique(RelativeWaterBalancePlotData$VarType)){
      Ptrs = which(RelativeWaterBalancePlotData$WaterYear==WaterYear &
                     RelativeWaterBalancePlotData$Climate==ClimateName &
                     RelativeWaterBalancePlotData$VarType==VarType)
      S2ptr = Ptrs[which(RelativeWaterBalancePlotData$Scenario[Ptrs]==2)]
      
      S2val = RelativeWaterBalancePlotData$WaterDepth_mPerYr[S2ptr]
      RelativeWaterBalancePlotData$WaterDepth_mPerYr[Ptrs] = RelativeWaterBalancePlotData$WaterDepth_mPerYr[Ptrs] - S2val
    }
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

# Change to mm units
RelativeWaterBalancePlotData$WaterDepth_mmPerYr = 1000 * RelativeWaterBalancePlotData$WaterDepth_mPerYr

write.csv(RelativeWaterBalancePlotData,"RelativeWaterBalancePlotData.csv")

g1 = ggplot(RelativeWaterBalancePlotData[RelativeWaterBalancePlotData$Scenario==6,],
            aes(x=WaterYear, y=WaterDepth_mmPerYr, fill=VarType)) +
  geom_vline(aes(xintercept=-9999, linetype="Vegetation Map\nUpdates"), lineend="round") +
  geom_vline(xintercept=seq(2015,2095,10),
             linetype=3, linewidth=0.75, color="black", lineend="round") +
  geom_area(color="black", linewidth=0.5) +
  geom_hline(yintercept=0, color="black", linewidth=1, linetype=1) +
  scale_fill_manual(breaks=c("Streamflow","Understory\nInterception Loss","Understory\nTranspiration",
                             "Overstory\nTranspiration","Overstory\nInterception Loss"),
                    values=c("dodgerblue","yellow","green","green4","goldenrod2")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     minor_breaks=seq(2014,2100,1),
                     expand=c(0,0),
                     guide="axis_minor") +
  scale_y_continuous(limits=c(-150,150),
                     breaks=c(seq(-150,150,50)),
                     labels=c(seq(-150,-50,50),"<strong>BAU<br>Scenario</strong>",seq(50,150,50)),
                     expand=c(0,0)) +
  scale_linetype_manual(name="",values=c(3),
                        guide=guide_legend(override.aes=list(color="black",
                                                             linewidth=0.75))) +
  labs(x="Scenario",
       y="Δ Water Balance Flux, mm / yr",
       fill="Full Disturbance (S6)\nRelative to\nBusiness-As-Usual (S2)") +
  theme_bw() +
  theme(axis.text.x=ggtext::element_markdown(color="black",size=14,angle=-90,vjust=0.5),
        axis.text.y=ggtext::element_markdown(color="black",size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18, margin=margin(0,0.5,0,0,"cm")),
        axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(color="black",size=18,margin=margin(0,0,1,0,"cm")),
        legend.text=element_text(size=16),
        legend.key.width=unit(1,"cm"),
        legend.key.height=unit(1,"cm"),
        legend.key.spacing.y=unit(1,"cm"),
        legend.title.align=0.5,
        legend.key=element_blank(),
        strip.text=element_text(color="black",size=24),
        strip.background=element_rect(color="black",fill="gray90"),
        panel.spacing=unit(1,"cm"),
        ggh4x.axis.ticks.length.minor=rel(0.5)) +
  guides(fill=guide_legend(byrow=TRUE, order=1)) +
  facet_grid(rows=vars(Climate))

print(g1)

ggsave("YearlyDeltaWaterBalancePartitioning.png", plot=g1,
       width=unit(12,"in"), height=unit(9,"in"), dpi=300)

















