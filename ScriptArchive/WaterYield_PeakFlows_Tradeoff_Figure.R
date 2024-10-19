
library(ggplot2)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Tradeoff_Figure"
setwd(dir)

# Can start here
WatershedData = read.csv("WatershedData_WithMeanAnnualPeak.csv")[,-1]
head(WatershedData)

# Add watershed x scenario column
WatershedData$WatershedScenario = paste0(WatershedData$WatershedNum,WatershedData$Scenario)

# Change climate name and change to factor
WatershedData$ClimateName = WatershedData$Climate
WatershedData$ClimateName = sub("cnrm", "Wetter Future Climate", WatershedData$ClimateName)
WatershedData$ClimateName = sub("miroc", "Drier Future Climate", WatershedData$ClimateName)
WatershedData$ClimateName = factor(WatershedData$ClimateName, levels=c("Wetter Future Climate","Drier Future Climate"))

# Change model name and change to factor
WatershedData$ModelName = as.character(WatershedData$DHSVMmodel)
WatershedData$ModelName = sub("195", "DHSVM Model C", WatershedData$ModelName)
WatershedData$ModelName = sub("270", "DHSVM Model B", WatershedData$ModelName)
WatershedData$ModelName = sub("276", "DHSVM Model A", WatershedData$ModelName)
WatershedData$ModelName = factor(WatershedData$ModelName, levels=paste0("DHSVM Model ",c("A","B","C")))

# Change scenario to factor
WatershedData$Scenario = factor(WatershedData$Scenario, levels=sort(unique(WatershedData$Scenario)))

# Subset to fewer scenarios
SelectedScenarios = c(1,3:6)
WatershedDataSubset = WatershedData[(WatershedData$Scenario %in% SelectedScenarios),]

# Change scenario names and change to factor
WatershedDataSubset$Scenario = paste0(WatershedDataSubset$Scenario)
WatershedDataSubset$Scenario = sub("1","Reduced Treatment",WatershedDataSubset$Scenario)
WatershedDataSubset$Scenario = sub("2","Business-As-Usual",WatershedDataSubset$Scenario)
WatershedDataSubset$Scenario = sub("3","Partial w/Less Fire",WatershedDataSubset$Scenario)
WatershedDataSubset$Scenario = sub("4","Partial Restoration",WatershedDataSubset$Scenario)
WatershedDataSubset$Scenario = sub("5","Full w/Less Fire",WatershedDataSubset$Scenario)
WatershedDataSubset$Scenario = sub("6","Full Restoration",WatershedDataSubset$Scenario)
WatershedDataSubset$Scenario = factor(WatershedDataSubset$Scenario, levels=c("Reduced Treatment",
                                                                             "Business-As-Usual",
                                                                             "Partial w/Less Fire",
                                                                             "Partial Restoration",
                                                                             "Full w/Less Fire",
                                                                             "Full Restoration"))

# Change peak flow trend to 85-year peak flow delta
WatershedDataSubset$S2RelYearlyPeakSensSlope_mmd85yr = 85 * WatershedDataSubset$S2RelYearlyPeakSensSlope_mmdyr

# Change water yield to yearly
WatershedDataSubset$S2RelContribStreamflow_mmyr = 365.25 * WatershedDataSubset$S2RelContribStreamflow_mmd

pal = colorRampPalette(c("gold","skyblue1","deepskyblue","dodgerblue2","blue"))

# Relative units
g1 = ggplot(data=WatershedDataSubset, aes(x=S2RelContribStreamflow_pct,
                                          y=RelS2YearlyPeakAvg_pct)) +
  geom_hline(yintercept=0,linewidth=1) +
  geom_vline(xintercept=0,linewidth=1) +
  geom_polygon(aes(group=WatershedScenario, color=Scenario), fill=NA, linewidth=0.25, show.legend=FALSE) +
  geom_point(size=1, stroke=0.5, aes(color=Scenario, shape=ModelName)) +
  geom_smooth(method="lm", formula=y~x, show.legend=FALSE, color="gray50") +
  scale_color_manual(values=pal(length(SelectedScenarios))) +
  scale_shape_manual(values=c(0, 1, 2)) +
  scale_x_continuous(breaks=seq(-1,1,0.1),
                     labels=c(paste0(seq(-100,-10,10),"%"),"<strong>BAU<br>Scenario</strong>",paste0("+",seq(10,100,10),"%"))) +
  scale_y_continuous(breaks=seq(-1,1,0.1),
                     labels=c(paste0(seq(-100,-10,10),"%"),"<strong>BAU<br>Scenario</strong>",paste0("+",seq(10,100,10),"%"))) +
  labs(x="Δ Streamflow Generation", y="Δ Yearly Peak Flow",
       color="Management Scenario",shape="Calibrated Model",
       title="") +
  theme_bw() +
  theme(axis.text.x=ggtext::element_markdown(color="black",size=14),
        axis.text.y=ggtext::element_markdown(color="black",size=14),
        axis.title.x=element_text(size=18, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=18, margin=margin(0,0.5,0,0,"cm")),
        axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=24),
        legend.text=element_text(size=18),
        legend.key.width=unit(2,"cm"),
        legend.spacing.y=unit(1,"cm"),
        plot.title=element_blank(),
        legend.title.align=0.5,
        strip.text=element_text(color="black",size=24),
        strip.background=element_rect(color="black",fill="gray90"),
        panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(size=6),byrow=TRUE, order=1),
         shape=guide_legend(override.aes=list(size=4, stroke=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName))

print(g1)

ggsave("WaterYieldVsPeakFlows_Relative.png", plot=g1,
       width=unit(12,"in"), height=unit(9,"in"), dpi=300)

# Absolute physical units
g1 = ggplot(data=WatershedDataSubset, aes(x=S2RelContribStreamflow_mmyr,
                                          y=S2RelYearlyPeakSensSlope_mmdyr)) +
  geom_hline(yintercept=0,linewidth=1) +
  geom_vline(xintercept=0,linewidth=1) +
  geom_polygon(aes(group=WatershedScenario, color=Scenario), fill=NA, linewidth=0.25, show.legend=FALSE) +
  geom_point(size=1, stroke=0.5, aes(color=Scenario, shape=ModelName)) +
  geom_smooth(method="lm", formula=y~x, show.legend=FALSE, color="gray50") +
  scale_color_manual(values=pal(length(SelectedScenarios))) +
  scale_shape_manual(values=c(0, 1, 2)) +
  scale_x_continuous(breaks=seq(-150,150,50),
                     labels=c(seq(-150,-50,50),"<strong>BAU<br>Scenario</strong>",paste0("+",seq(50,150,50)))) +
  scale_y_continuous(breaks=seq(-0.1,0.1,0.01),
                     labels=c(seq(-0.1,-0.01,0.01),"<strong>BAU<br>Scenario</strong>",paste0("+",seq(0.01,0.1,0.01)))) +
  labs(x="Δ Streamflow Generation, mm / yr", y="Δ Yearly Peak Flow Trend, mm / d / yr",
       color="Management Scenario",shape="Calibrated Model",
       title="") +
  theme_bw() +
  theme(axis.text.x=ggtext::element_markdown(color="black",size=14),
        axis.text.y=ggtext::element_markdown(color="black",size=14),
        axis.title.x=element_text(size=18, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=18, margin=margin(0,0.5,0,0,"cm")),
        axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=24),
        legend.text=element_text(size=18),
        legend.key.width=unit(2,"cm"),
        legend.spacing.y=unit(1,"cm"),
        plot.title=element_blank(),
        legend.title.align=0.5,
        strip.text=element_text(color="black",size=24),
        strip.background=element_rect(color="black",fill="gray90"),
        panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(size=6),byrow=TRUE, order=1),
         shape=guide_legend(override.aes=list(size=4, stroke=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName))

print(g1)

ggsave("WaterYieldVsPeakFlows.png", plot=g1,
       width=unit(12,"in"), height=unit(9,"in"), dpi=300)
# 
# # Larger fonts for poster
# g1 = ggplot(data=WatershedDataSubset, aes(x=S2RelContribStreamflow_mmd, y=S2RelYearlyPeakSensSlope_mmdyr)) +
#   geom_hline(yintercept=0,linewidth=1) +
#   geom_vline(xintercept=0,linewidth=1) +
#   geom_polygon(aes(group=WatershedScenario, color=Scenario), fill=NA, linewidth=0.25, show.legend=FALSE) +
#   geom_point(size=1, stroke=0.5, aes(color=Scenario, shape=ModelName)) +
#   geom_smooth(method="lm", formula=y~x, show.legend=FALSE, color="gray50") +
#   scale_color_manual(values=pal(length(SelectedScenarios))) +
#   scale_shape_manual(values=c(0, 1, 2)) +
#   scale_x_continuous(breaks=seq(-0.4,0.4,0.1),
#                      labels=c(seq(-0.4,-0.1,0.1),"<strong>BAU<br>Scenario</strong>",paste0("+",seq(0.1,0.4,0.1)))) +
#   scale_y_continuous(breaks=seq(-0.1,0.1,0.01),
#                      labels=c(seq(-0.1,-0.01,0.01),"<strong>BAU<br>Scenario</strong>",paste0("+",seq(0.01,0.1,0.01)))) +
#   labs(x="Δ Streamflow Generation, mm / d", y="Δ Peak Flow Trend, mm / d / yr",
#        color="Management Scenario",shape="DHSVM Ensemble",
#        title="") +
#   theme_bw() +
#   theme(axis.text.x=ggtext::element_markdown(color="black",size=16),
#         axis.text.y=ggtext::element_markdown(color="black",size=16),
#         axis.title.x=element_text(size=30, margin=margin(0.5,0,0,0,"cm")),
#         axis.title.y=element_text(size=30, margin=margin(0,0.5,0,0,"cm")),
#         axis.ticks.length=unit(0.3,"cm"),
#         plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
#         panel.background=element_rect("white", "black"),
#         panel.grid=element_blank(),
#         legend.title=element_text(size=30),
#         legend.text=element_text(size=24),
#         legend.key.width=unit(2,"cm"),
#         legend.spacing.y=unit(1,"cm"),
#         plot.title=element_blank(),
#         legend.title.align=0.5,
#         strip.text=element_text(color="black",size=24),
#         strip.background=element_rect(color="black",fill="gray90"),
#         panel.spacing=unit(1,"cm")) +
#   guides(color=guide_legend(override.aes=list(size=6),byrow=TRUE, order=1),
#          shape=guide_legend(override.aes=list(size=4, stroke=2),byrow=TRUE)) +
#   facet_grid(rows=vars(ClimateName))
# 
# plot(g1)
# 
# ggsave("WaterYieldVsPeakFlowsLarge.png", plot=g1,
#        width=unit(12,"in"), height=unit(10,"in"), dpi=600)

# Percent change vs. absolute magnitude
g1 = ggplot(data=WatershedDataSubset, aes(x=YearlyPeakAvg_cms,
                                          y=RelS2YearlyPeakAvg_pct)) +
  geom_hline(yintercept=0,linewidth=1) +
  geom_vline(xintercept=0,linewidth=1) +
  geom_polygon(aes(group=WatershedScenario, color=Scenario), fill=NA, linewidth=0.25, show.legend=FALSE) +
  geom_point(size=1, stroke=0.5, aes(color=Scenario, shape=ModelName)) +
  scale_color_manual(values=pal(length(SelectedScenarios))) +
  scale_shape_manual(values=c(0, 1, 2)) +
  scale_x_log10() +
  scale_y_continuous(breaks=seq(-1,1,0.1),
                     labels=c(paste0(seq(-100,-10,10),"%"),"<strong>BAU<br>Scenario</strong>",paste0("+",seq(10,100,10),"%"))) +
  labs(x=expression("Mean Yearly Peak Flow, m"^3/s), y="Δ Mean Yearly Peak Flow",
       color="Management Scenario",shape="Calibrated Model",
       title="") +
  theme_bw() +
  theme(axis.text.x=ggtext::element_markdown(color="black",size=14),
        axis.text.y=ggtext::element_markdown(color="black",size=14),
        axis.title.x=element_text(size=18, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=18, margin=margin(0,0.5,0,0,"cm")),
        axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=24),
        legend.text=element_text(size=18),
        legend.key.width=unit(2,"cm"),
        legend.spacing.y=unit(1,"cm"),
        plot.title=element_blank(),
        legend.title.align=0.5,
        strip.text=element_text(color="black",size=24),
        strip.background=element_rect(color="black",fill="gray90"),
        panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(size=6),byrow=TRUE, order=1),
         shape=guide_legend(override.aes=list(size=4, stroke=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName))

print(g1)

ggsave("PeakFlow_RelativeVsBaseline.png", plot=g1,
       width=unit(12,"in"), height=unit(9,"in"), dpi=300)


















