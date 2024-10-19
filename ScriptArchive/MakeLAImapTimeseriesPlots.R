
library(ggplot2)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/LAI_Timeseries"
setwd(dir)

##########
# HUC12

LAItimeseriesHUC12 = read.csv("LAItimeseriesDataHUC12.csv")[,-1]

LAItimeseriesHUC12$Scenario = factor(LAItimeseriesHUC12$Scenario)

ggplot(data=LAItimeseriesHUC12, aes(x=Year, y=MeanLAI)) + 
  geom_violin(scale="width", width=5,
              aes(group=interaction(Year,Scenario),fill=Scenario)) +
  scale_fill_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  labs(x="", y="Mean Leaf Area Index") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14,angle=90,vjust=0.5,color="black"),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm")),
        axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20),
        legend.key.width=unit(2,"cm"),
        legend.spacing.y=unit(1,"cm"),
        legend.title.align=0.5,
        strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90"),
        panel.spacing=unit(0.5,"cm")) +
  guides(fill=guide_legend(byrow=TRUE, order=1)) +
  facet_grid(rows=vars(ClimateName))

##########
# HUC8

LAItimeseriesHUC8 = read.csv("LAItimeseriesDataHUC8.csv")[,-1]

LAItimeseriesHUC8$Scenario = factor(LAItimeseriesHUC8$Scenario)
LAItimeseriesHUC8$Basin = factor(LAItimeseriesHUC8$WatershedName)

ggplot(data=LAItimeseriesHUC8, aes(x=Year, y=MeanLAI)) + 
  geom_line(linewidth=2, aes(color=Scenario)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  labs(x="", y="Mean Leaf Area Index") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14,angle=90,vjust=0.5,color="black"),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm")),
        axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20),
        legend.key.width=unit(2,"cm"),
        legend.spacing.y=unit(1,"cm"),
        legend.title.align=0.5,
        strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90"),
        panel.spacing=unit(0.5,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))






