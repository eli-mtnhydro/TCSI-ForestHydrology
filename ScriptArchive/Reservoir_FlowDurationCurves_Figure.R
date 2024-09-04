
library(ggplot2)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Reservoir_FDCs"
setwd(dir)

# Read in post-processed DHSVM results

FlowDurationData = read.csv("FlowDurationData.csv")[,-1]

# Subset to fewer scenarios
SelectedScenarios = c(1:6)
FlowDurationDataSubset = FlowDurationData[(FlowDurationData$Scenario %in% SelectedScenarios),]

################################################################################
# Plot flow duration curves

# ggplot(data=FlowDurationData, aes(x=ExceedanceProbability,y=DailyFlow_cms)) + 
#   geom_point(size=1, aes(color=Scenario)) +
#   scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
#   scale_x_log10(breaks=scales::trans_breaks("log10", function(x) 10^x),
#                 labels=function(x) paste0(x*100, "%")) +
#   annotation_logticks(sides="b") +
#   labs(x="Probability of Exceedance",
#        y=expression("Daily Streamflow, m"^3/"s")) +
#   theme_bw() +
#   theme(axis.text.x=element_text(size=18, vjust=0.5,color=c("black")),
#         axis.text.y=element_text(size=18,color="black"),
#         axis.title.x=element_text(size=24, margin=margin(0.5,0,0,0,"cm")),
#         axis.title.y=element_text(size=24, margin=margin(0,0.5,0,0,"cm")),
#         axis.ticks.length=unit(0.3,"cm"),
#         plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
#         panel.background=element_rect("white", "black"),
#         panel.grid=element_blank(),
#         legend.title=element_text(color="black",size=20),
#         legend.text=element_text(size=18),
#         legend.key.width=unit(1,"cm"),
#         legend.key.height=unit(1,"cm"),
#         legend.spacing.y=unit(1,"cm"),
#         legend.title.align=0.5,
#         strip.text=element_text(color="black",size=20),
#         strip.background=element_rect(color="black",fill="gray90"),
#         panel.spacing.x=unit(2,"cm"),
#         panel.spacing.y=unit(1,"cm")) +
#   guides(color=guide_legend(override.aes=list(size=6),byrow=TRUE)) +
#   facet_grid(rows=vars(Climate),cols=vars(Reservoir))

################################################################################
# Area-normalized version

# ggplot(data=FlowDurationData, aes(x=ExceedanceProbability,y=DailyFlow_mmd)) + 
#   geom_point(size=1, aes(color=Scenario)) +
#   scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
#   scale_x_log10(breaks=scales::trans_breaks("log10", function(x) 10^x),
#                 labels=function(x) paste0(x*100, "%")) +
#   annotation_logticks(sides="b") +
#   labs(x="Probability of Exceedance",
#        y="Daily Streamflow, mm/d") +
#   theme_bw() +
#   theme(axis.text.x=element_text(size=18, vjust=0.5,color=c("black")),
#         axis.text.y=element_text(size=18,color="black"),
#         axis.title.x=element_text(size=24, margin=margin(0.5,0,0,0,"cm")),
#         axis.title.y=element_text(size=24, margin=margin(0,0.5,0,0,"cm")),
#         axis.ticks.length=unit(0.3,"cm"),
#         plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
#         panel.background=element_rect("white", "black"),
#         panel.grid=element_blank(),
#         legend.title=element_text(color="black",size=20),
#         legend.text=element_text(size=18),
#         legend.key.width=unit(1,"cm"),
#         legend.key.height=unit(1,"cm"),
#         legend.spacing.y=unit(1,"cm"),
#         legend.title.align=0.5,
#         strip.text=element_text(color="black",size=20),
#         strip.background=element_rect(color="black",fill="gray90"),
#         panel.spacing.x=unit(2,"cm"),
#         panel.spacing.y=unit(1,"cm")) +
#   guides(color=guide_legend(override.aes=list(size=6),byrow=TRUE)) +
#   facet_grid(rows=vars(Climate),cols=vars(Reservoir))

################################################################################
# Both climates on 1 plot

# Change climate names
FlowDurationDataSubset$Climate = sub("CNRM-CM5 RCP 8.5","Wetter Future Climate",FlowDurationDataSubset$Climate)
FlowDurationDataSubset$Climate = sub("MIROC5 RCP 8.5","Drier Future Climate",FlowDurationDataSubset$Climate)
FlowDurationDataSubset$Climate = factor(FlowDurationDataSubset$Climate, levels=c("Wetter Future Climate","Drier Future Climate"))

# Change scenario names and change to factor
FlowDurationDataSubset$Scenario = paste0(FlowDurationDataSubset$Scenario)
FlowDurationDataSubset$Scenario = sub("1","Reduced Treatment",FlowDurationDataSubset$Scenario)
FlowDurationDataSubset$Scenario = sub("2","Business-As-Usual",FlowDurationDataSubset$Scenario)
FlowDurationDataSubset$Scenario = sub("3","Partial w/Less Fire",FlowDurationDataSubset$Scenario)
FlowDurationDataSubset$Scenario = sub("4","Partial Restoration",FlowDurationDataSubset$Scenario)
FlowDurationDataSubset$Scenario = sub("5","Full w/Less Fire",FlowDurationDataSubset$Scenario)
FlowDurationDataSubset$Scenario = sub("6","Full Restoration",FlowDurationDataSubset$Scenario)
FlowDurationDataSubset$Scenario = factor(FlowDurationDataSubset$Scenario, levels=c("Reduced Treatment",
                                                                                   "Business-As-Usual",
                                                                                   "Partial w/Less Fire",
                                                                                   "Partial Restoration",
                                                                                   "Full w/Less Fire",
                                                                                   "Full Restoration"))

pal = colorRampPalette(c("gold","gray","skyblue1","deepskyblue","dodgerblue2","blue"))

# g1 = ggplot(data=FlowDurationDataSubset, aes(x=ExceedanceProbability,y=DailyFlow_mmd)) + 
#   geom_line(linewidth=1.5, lineend="round", aes(color=Scenario,linetype=Climate)) +
#   scale_color_manual(values=pal(length(SelectedScenarios))) +
#   scale_linetype_manual(values=c(1,3)) +
#   scale_x_log10(breaks=scales::trans_breaks("log10", function(x) 10^x),
#                 labels=function(x) paste0(x*100, "%")) +
#   annotation_logticks(sides="b") +
#   labs(x="Probability of Exceedance",
#        y="Daily Streamflow, mm / d",
#        color="Management Scenario") +
#   theme_bw() +
#   theme(axis.text.x=element_text(size=18, vjust=0.5,color=c("black")),
#         axis.text.y=element_text(size=18,color="black"),
#         axis.title.x=element_text(size=24, margin=margin(0.5,0,0,0,"cm")),
#         axis.title.y=element_text(size=24, margin=margin(0,0.5,0,0,"cm")),
#         axis.ticks.length=unit(0.3,"cm"),
#         plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
#         panel.background=element_rect("white", "black"),
#         panel.grid=element_blank(),
#         legend.title=element_text(color="black",size=20),
#         legend.text=element_text(size=18),
#         legend.key.width=unit(1,"cm"),
#         legend.spacing.y=unit(1,"cm"),
#         legend.title.align=0.5,
#         strip.text=element_text(color="black",size=20),
#         strip.background=element_rect(color="black",fill="gray90"),
#         panel.spacing.x=unit(1,"cm"),
#         panel.spacing.y=unit(1,"cm")) +
#   guides(color=guide_legend(override.aes=list(linewidth=3),byrow=TRUE,order=1),
#          linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
#   facet_grid(cols=vars(Reservoir))
# 
# print(g1)
# 
# ggsave("ReservoirFDCs.png", plot=g1,
#        width=unit(12,"in"), height=unit(10,"in"), dpi=300)

# Bottom legend version

# Shorten reservoir name
FlowDurationDataSubset$Reservoir = sub("New Bullards Bar Reservoir","New Bullards Bar",FlowDurationDataSubset$Reservoir)
FlowDurationDataSubset$Reservoir = factor(FlowDurationDataSubset$Reservoir,
                                          levels=unique(FlowDurationDataSubset$Reservoir))

g1 = ggplot(data=FlowDurationDataSubset, aes(x=ExceedanceProbability,y=DailyFlow_mmd)) + 
  geom_line(linewidth=1.5, lineend="round", aes(color=Scenario,linetype=Climate)) +
  scale_color_manual(values=pal(length(SelectedScenarios))) +
  scale_linetype_manual(values=c(1,3)) +
  scale_x_log10(breaks=scales::trans_breaks("log10", function(x) 10^x),
                labels=function(x) paste0(x*100, "%")) +
  # scale_y_log10() +
  annotation_logticks(sides="b") +
  labs(x="Probability of Exceedance",
       y="Daily Streamflow, mm / d",
       color="Management Scenario") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black")),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_text(size=18, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=18, margin=margin(0,0.5,0,0,"cm")),
        axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(color="black",size=24),
        legend.text=element_text(size=18),
        legend.key.width=unit(1,"cm"),
        legend.spacing.y=unit(0.25,"cm"),
        legend.title.align=0.5,
        strip.text=element_text(color="black",size=24),
        strip.background=element_rect(color="black",fill="gray90"),
        panel.spacing.x=unit(1,"cm"),
        panel.spacing.y=unit(1,"cm"),
        legend.position="bottom") +
  guides(color=guide_legend(override.aes=list(linewidth=3),nrow=6,title.position="top",byrow=TRUE,order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),nrow=2,title.position="top",byrow=TRUE)) +
  facet_grid(cols=vars(Reservoir))

print(g1)

ggsave("ReservoirFDCs.png", plot=g1,
       width=unit(12,"in"), height=unit(9,"in"), dpi=300)

g1 = ggplot(data=FlowDurationDataSubset, aes(x=ExceedanceProbability,y=DailyFlow_mmd)) + 
  geom_line(linewidth=1.5, lineend="round", aes(color=Scenario,linetype=Climate)) +
  scale_color_manual(values=pal(6)) +
  scale_linetype_manual(values=c(1,3)) +
  scale_x_log10(breaks=scales::trans_breaks("log10", function(x) 10^x),
                labels=function(x) paste0(x*100, "%")) +
  annotation_logticks(sides="b") +
  labs(x="Probability of Exceedance",
       y="Daily Streamflow, mm / d",
       color="Management Scenario") +
  theme_bw() +
  theme(axis.text.x=element_text(size=18, vjust=0.5,color=c("black")),
        axis.text.y=element_text(size=18,color="black"),
        axis.title.x=element_text(size=30, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=30, margin=margin(0,0.5,0,0,"cm")),
        axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(color="black",size=30, margin=margin(0.5,0,0,0,"cm")),
        legend.text=element_text(size=20),
        legend.key.width=unit(1,"cm"),
        legend.spacing.y=unit(0.25,"cm"),
        legend.title.align=0.5,
        strip.text=element_text(color="black",size=24),
        strip.background=element_rect(color="black",fill="gray90"),
        panel.spacing.x=unit(1,"cm"),
        panel.spacing.y=unit(1,"cm"),
        legend.position="bottom") +
  guides(color=guide_legend(override.aes=list(linewidth=3),nrow=6,title.position="top",byrow=TRUE,order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),nrow=2,title.position="top",byrow=TRUE)) +
  facet_grid(cols=vars(Reservoir))

print(g1)

ggsave("ReservoirFDCsLarge.png", plot=g1,
       width=unit(12,"in"), height=unit(12,"in"), dpi=300)

################################################################################
# Magnitude of change for highest floods

PeakCRNMptrsS2 = which(FlowDurationData$Scenario=="Business-As-Usual" &
                         FlowDurationData$Climate=="Wetter Future Climate" &
                         FlowDurationData$ExceedanceProbability < 0.01)
PeakCRNMptrsS6 = which(FlowDurationData$Scenario=="Extensive with Fire" &
                         FlowDurationData$Climate=="Wetter Future Climate" &
                         FlowDurationData$ExceedanceProbability < 0.01)

plot(FlowDurationData$DailyFlow_mmd[PeakCRNMptrsS6] / FlowDurationData$DailyFlow_mmd[PeakCRNMptrsS2])

PeakMIROCptrsS2 = which(FlowDurationData$Scenario=="Business-As-Usual" &
                         FlowDurationData$Climate=="Drier Future Climate" &
                         FlowDurationData$ExceedanceProbability < 0.01)
PeakMIROCptrsS6 = which(FlowDurationData$Scenario=="Extensive with Fire" &
                         FlowDurationData$Climate=="Drier Future Climate" &
                         FlowDurationData$ExceedanceProbability < 0.01)

plot(FlowDurationData$DailyFlow_mmd[PeakMIROCptrsS6] / FlowDurationData$DailyFlow_mmd[PeakMIROCptrsS2])









