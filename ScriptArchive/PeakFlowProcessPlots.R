
library(ggplot2)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Peak_Flow_Processes/"
setwd(dir)

PeakFlowData = read.csv("PeakFlowProcessData.csv")[,-1]

nBiggestStorms = 10

PeakFlowDataSubset = PeakFlowData[(PeakFlowData$PrecipIntensityRank <= nBiggestStorms),]

################################################################################
# Interception and snowmelt

SelectedScenarios = c(1,3:6)

# Reorganize to ggplot style
PeakFlowPlotData = data.frame(Values=c(PeakFlowDataSubset$IntTotal_DeltaStorm_RelS2_mm,
                                       PeakFlowDataSubset$IntEvapTotal_DuringStorm_RelS2_mm,
                                       PeakFlowDataSubset$SnowMelt_TotDuringStorm_RelS2_mm),
                              ValueType=c(rep("Storm Total\nInterception Storage",nrow(PeakFlowDataSubset)),
                                          rep("Cumulative Storm\nInterception Loss",nrow(PeakFlowDataSubset)),
                                          rep("Cumulative Storm\nSnowpack Outflow",nrow(PeakFlowDataSubset))),
                              Climate=rep(PeakFlowDataSubset$Climate,length(SelectedProcesses)),
                              Scenario=rep(PeakFlowDataSubset$Scenario,length(SelectedProcesses)),
                              Watershed=rep(PeakFlowDataSubset$Basin,length(SelectedProcesses)))

PeakFlowPlotData = PeakFlowPlotData[PeakFlowPlotData$Scenario %in% SelectedScenarios,]

# Plot order for ValueType
PeakFlowPlotData$ValueType = factor(PeakFlowPlotData$ValueType,
                                    levels=c("Storm Total\nInterception Storage",
                                             "Cumulative Storm\nInterception Loss",
                                             "Cumulative Storm\nSnowpack Outflow"))

# Change climate name
PeakFlowPlotData$ClimateName = PeakFlowPlotData$Climate
PeakFlowPlotData$ClimateName = sub("cnrm", "Wetter Future Climate", PeakFlowPlotData$ClimateName)
PeakFlowPlotData$ClimateName = sub("miroc", "Drier Future Climate", PeakFlowPlotData$ClimateName)
PeakFlowPlotData$ClimateName = factor(PeakFlowPlotData$ClimateName,levels=c("Wetter Future Climate","Drier Future Climate"))

PeakFlowPlotData$Scenario = paste0(PeakFlowPlotData$Scenario)
PeakFlowPlotData$Scenario = sub("1","Reduced Treatment",PeakFlowPlotData$Scenario)
PeakFlowPlotData$Scenario = sub("2","Business-As-Usual",PeakFlowPlotData$Scenario)
PeakFlowPlotData$Scenario = sub("3","Partial w/Less Fire",PeakFlowPlotData$Scenario)
PeakFlowPlotData$Scenario = sub("4","Partial Restoration",PeakFlowPlotData$Scenario)
PeakFlowPlotData$Scenario = sub("5","Full w/Less Fire",PeakFlowPlotData$Scenario)
PeakFlowPlotData$Scenario = sub("6","Full Restoration",PeakFlowPlotData$Scenario)
PeakFlowPlotData$Scenario = factor(PeakFlowPlotData$Scenario, levels=c("Reduced Treatment",
                                                                       "Business-As-Usual",
                                                                       "Partial w/Less Fire",
                                                                       "Partial Restoration",
                                                                       "Full w/Less Fire",
                                                                       "Full Restoration"))

# Trim to a reasonable range
MaxRange = 3
PeakFlowPlotData$Values = pmin(PeakFlowPlotData$Values,MaxRange)
PeakFlowPlotData$Values = pmax(PeakFlowPlotData$Values,-MaxRange)

# g1 = ggplot(data=PeakFlowPlotData, aes(x=Scenario, y=Values)) + 
#   geom_hline(yintercept=0, linewidth=1) +
#   geom_violin(aes(fill=Scenario),
#               scale="width", width=0.9, linewidth=0.5, color="black") +
#   scale_fill_manual(values=c("gold","skyblue1","deepskyblue","dodgerblue2","blue")) +
#   scale_y_continuous(breaks=seq(-0.4,0.4,0.1),
#                      labels=c(paste0(100*seq(-0.4,-0.1,0.1),"%"),
#                               "<strong>BAU<br>Scenario</strong>",
#                               paste0("+",100*seq(0.1,0.4,0.1),"%"))) +
#   labs(x="",
#        y="Δ Water Balance Flux",
#        fill="Management\nScenario") +
#   theme_bw() +
#   theme(axis.text.x=element_blank(),
#         axis.text.y=ggtext::element_markdown(color="black",size=14),
#         axis.title.x=element_blank(),
#         axis.title.y=element_text(size=18, margin=margin(0,0.5,0,0,"cm")),
#         axis.ticks.length=unit(0.3,"cm"),
#         plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
#         panel.background=element_rect("white", "black"),
#         panel.grid=element_blank(),
#         legend.title=element_text(size=24),
#         legend.text=element_text(size=18),
#         legend.key.width=unit(1,"cm"),
#         legend.key.height=unit(1,"cm"),
#         legend.spacing.y=unit(1,"cm"),
#         plot.title=element_blank(),
#         legend.title.align=0.5,
#         strip.text=element_text(color="black",size=24),
#         strip.background=element_rect(color="black",fill="gray90"),
#         panel.spacing=unit(1,"cm")) +
#   guides(fill=guide_legend(byrow=TRUE, order=1)) +
#   facet_grid(rows=vars(ClimateName),cols=vars(ValueType))
# 
# print(g1)
# 
# ggsave("PeakFlowProcesses.png", plot=g1,
#        width=unit(12,"in"), height=unit(9,"in"), dpi=300)

# Absolute units
g1 = ggplot(data=PeakFlowPlotData, aes(x=Scenario, y=Values)) + 
  geom_hline(yintercept=0, linewidth=1) +
  geom_violin(aes(fill=Scenario),
              scale="width", width=0.9, linewidth=0.5, color="black") +
  scale_fill_manual(values=c("gold","skyblue1","deepskyblue","dodgerblue2","blue")) +
  scale_y_continuous(breaks=seq(-5,5,1),
                     labels=c(seq(-5,-1,1),
                              "<strong>BAU<br>Scenario</strong>",
                              seq(1,5,1))) +
  labs(x="",
       y="Δ Water Balance Flux, mm",
       fill="Management\nScenario") +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=ggtext::element_markdown(color="black",size=18),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=24, margin=margin(0,0.5,0,0,"cm")),
        axis.ticks.length.x=unit(0,"cm"),
        axis.ticks.length.y=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=24),
        legend.text=element_text(size=18),
        legend.key.width=unit(1,"cm"),
        legend.key.height=unit(1,"cm"),
        legend.spacing.y=unit(1,"cm"),
        plot.title=element_blank(),
        legend.title.align=0.5,
        strip.text=element_text(color="black",size=24),
        strip.background=element_rect(color="black",fill="gray90"),
        panel.spacing=unit(1,"cm")) +
  guides(fill=guide_legend(byrow=TRUE, order=1)) +
  facet_grid(rows=vars(ClimateName),cols=vars(ValueType))

print(g1)

ggsave("PeakFlowProcessesAbsolute.png", plot=g1,
       width=unit(16,"in"), height=unit(9,"in"), dpi=300)

################################################################################
# Snow energy balance

PeakFlowDataSubset$SnowEnergyTotal_RelS2_wm2 = PeakFlowDataSubset$SnowSW_AvgDuringStorm_RelS2_wm2 +
  PeakFlowDataSubset$SnowLW_AvgDuringStorm_RelS2_wm2 +
  PeakFlowDataSubset$SnowSensible_AvgDuringStorm_RelS2_wm2 +
  PeakFlowDataSubset$SnowLatent_AvgDuringStorm_RelS2_wm2 +
  PeakFlowDataSubset$SnowAdvected_AvgDuringStorm_RelS2_wm2

SelectedProcesses = c("SnowSW_AvgDuringStorm_RelS2_wm2",
                      "SnowLW_AvgDuringStorm_RelS2_wm2",
                      "SnowSensible_AvgDuringStorm_RelS2_wm2",
                      "SnowLatent_AvgDuringStorm_RelS2_wm2",
                      "SnowAdvected_AvgDuringStorm_RelS2_wm2",
                      "SnowEnergyTotal_RelS2_wm2")

SelectedScenarios = c(1,3:6)

# Reorganize to ggplot style
PeakFlowPlotData = data.frame(Values=unname(unlist(PeakFlowDataSubset[,SelectedProcesses])),
                              ValueType=c(rep("Shortwave\nRadiation",nrow(PeakFlowDataSubset)),
                                          rep("Longwave\nRadiation",nrow(PeakFlowDataSubset)),
                                          rep("Sensible\nHeat",nrow(PeakFlowDataSubset)),
                                          rep("Latent\nHeat",nrow(PeakFlowDataSubset)),
                                          rep("Advected\nEnergy",nrow(PeakFlowDataSubset)),
                                          rep("Net",nrow(PeakFlowDataSubset))),
                              Climate=rep(PeakFlowDataSubset$Climate,length(SelectedProcesses)),
                              Scenario=rep(PeakFlowDataSubset$Scenario,length(SelectedProcesses)),
                              Watershed=rep(PeakFlowDataSubset$Basin,length(SelectedProcesses)))

PeakFlowPlotData = PeakFlowPlotData[PeakFlowPlotData$Scenario %in% SelectedScenarios,]

PeakFlowPlotData$ValueType = factor(PeakFlowPlotData$ValueType,
                                    levels=c("Shortwave\nRadiation","Longwave\nRadiation",
                                             "Sensible\nHeat","Latent\nHeat","Advected\nEnergy","Net"))

# Change climate name
PeakFlowPlotData$ClimateName = PeakFlowPlotData$Climate
PeakFlowPlotData$ClimateName = sub("cnrm", "Wetter Future Climate", PeakFlowPlotData$ClimateName)
PeakFlowPlotData$ClimateName = sub("miroc", "Drier Future Climate", PeakFlowPlotData$ClimateName)
PeakFlowPlotData$ClimateName = factor(PeakFlowPlotData$ClimateName,levels=c("Wetter Future Climate","Drier Future Climate"))

PeakFlowPlotData$Scenario = paste0(PeakFlowPlotData$Scenario)
PeakFlowPlotData$Scenario = sub("1","Reduced Treatment",PeakFlowPlotData$Scenario)
PeakFlowPlotData$Scenario = sub("2","Business-As-Usual",PeakFlowPlotData$Scenario)
PeakFlowPlotData$Scenario = sub("3","Partial w/Less Fire",PeakFlowPlotData$Scenario)
PeakFlowPlotData$Scenario = sub("4","Partial Restoration",PeakFlowPlotData$Scenario)
PeakFlowPlotData$Scenario = sub("5","Full w/Less Fire",PeakFlowPlotData$Scenario)
PeakFlowPlotData$Scenario = sub("6","Full Restoration",PeakFlowPlotData$Scenario)
PeakFlowPlotData$Scenario = factor(PeakFlowPlotData$Scenario, levels=c("Reduced Treatment",
                                                                       "Business-As-Usual",
                                                                       "Partial w/Less Fire",
                                                                       "Partial Restoration",
                                                                       "Full w/Less Fire",
                                                                       "Full Restoration"))

# Trim to a reasonable range
MaxRange = 2
PeakFlowPlotData$Values = pmin(PeakFlowPlotData$Values,MaxRange)
PeakFlowPlotData$Values = pmax(PeakFlowPlotData$Values,-MaxRange)

g1 = ggplot(data=PeakFlowPlotData, aes(x=Scenario, y=Values)) + 
  geom_hline(yintercept=0, linewidth=1) +
  geom_violin(aes(fill=Scenario),
              scale="width", width=0.9, linewidth=0.5, color="black") +
  scale_fill_manual(values=c("gold","skyblue1","deepskyblue","dodgerblue2","blue")) +
  scale_y_continuous(breaks=seq(-2,2,1),
                     labels=c(seq(-2,-1,1),
                              "<strong>BAU<br>Scenario</strong>",
                              seq(1,2,1))) +
  labs(x="",
       y=expression(Delta*" Snow Energy Balance Flux, W/m"^2),
       fill="Management\nScenario") +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=ggtext::element_markdown(color="black",size=18),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=24, margin=margin(0,0.5,0,0,"cm")),
        axis.ticks.length.x=unit(0,"cm"),
        axis.ticks.length.y=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=24),
        legend.text=element_text(size=18),
        legend.key.width=unit(1,"cm"),
        legend.key.height=unit(1,"cm"),
        legend.spacing.y=unit(1,"cm"),
        plot.title=element_blank(),
        legend.title.align=0.5,
        strip.text=element_text(color="black",size=24),
        strip.background=element_rect(color="black",fill="gray90"),
        panel.spacing=unit(0.5,"cm")) +
  guides(fill=guide_legend(byrow=TRUE, order=1)) +
  facet_grid(rows=vars(ClimateName),cols=vars(ValueType))

print(g1)

ggsave("PeakFlowSnowEnergyBalance.png", plot=g1,
       width=unit(18,"in"), height=unit(9,"in"), dpi=300)



















