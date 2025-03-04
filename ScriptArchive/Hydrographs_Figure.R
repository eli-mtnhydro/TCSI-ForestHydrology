
library(ggplot2)
library(ggh4x)

dir = "C:/Users/board/Desktop/DHSVM/ForestPaper/Figures/Cal_Val_Hydrographs/"
setwd(dir)

# Read in DHSVM results from CompileDHSVMstreamflowTimeseries.R (calibration directory)

HydrographData = read.csv("Gauge11413000_Cal-Val-Ref_Hydrographs.csv")[,-1]

################################################################################

HydrographData$Date = as.Date(HydrographData$Date,format="%Y-%m-%d")

HydrographData$MeasOrMod = "Modeled"
HydrographData$MeasOrMod[grepl("Measured",HydrographData$ValueType)] = "Measured"
HydrographData$MeasOrMod = factor(HydrographData$MeasOrMod,
                                  levels=c("Measured","Modeled"))

HydrographData$Model = HydrographData$ValueType
HydrographData$Model = sub("DHSVM_195","DHSVM Model C",HydrographData$Model)
HydrographData$Model = sub("DHSVM_270","DHSVM Model B",HydrographData$Model)
HydrographData$Model = sub("DHSVM_276","DHSVM Model A",HydrographData$Model)
HydrographData$Model = factor(HydrographData$Model,
                              levels=c(paste0("DHSVM Model ",c("A","B","C"))))

CalValDate = as.Date("10/1/2011",format="%m/%d/%Y")

g1 = ggplot() +
  geom_line(data=HydrographData[HydrographData$MeasOrMod=="Modeled",],
            aes(x=Date,y=Streamflow_mmd,color=Model,linewidth=Model),
            lineend="round") +
  geom_line(data=HydrographData[HydrographData$MeasOrMod=="Measured",],
            aes(x=Date,y=Streamflow_mmd),
            linewidth=1, lineend="round", linetype=1, color="white") +
  geom_line(data=HydrographData[HydrographData$MeasOrMod=="Measured",],
            aes(x=Date,y=Streamflow_mmd,linetype="USGS 11413000"),
            linewidth=1, lineend="round", color="black") +
  geom_vline(xintercept=CalValDate,
             linewidth=1) +
  annotate("text", x=CalValDate - 30, y=50,
           label="← Validation",
           hjust=1,vjust=0,size=24/.pt,color="black") +
  annotate("text", x=CalValDate + 30, y=50,
           label="Calibration →",
           hjust=0,vjust=0,size=24/.pt,color="black") +
  scale_color_manual(values=c("red","violet","dodgerblue")) +
  scale_linewidth_manual(values=c(2,1.5,1),guide="none") +
  scale_linetype_manual(name="\nMeasured",values=c(3),
                        guide=guide_legend(override.aes=list(color="black",
                                                             linewidth=2))) +
  scale_x_date(limits=c(min(HydrographData$Date),
                        max(HydrographData$Date)),
               breaks=seq(min(HydrographData$Date),
                          max(HydrographData$Date),
                          by="years"),
               labels=function(x) as.numeric(format(x,"%Y")) + 1,
               expand=c(0,0)) +
  # scale_y_continuous(limits=c(0,80),
  #                    breaks=seq(0,80,10),
  #                    expand=c(0,0)) +
  scale_y_log10() +
  annotation_logticks(sides="l",outside=TRUE,
                      short=unit(0.1, "cm"),
                      mid=unit(0.2, "cm"),
                      long=unit(0.3, "cm")) +
  coord_cartesian(clip = "off") +
  labs(x="Water Year",
       y="Streamflow, mm / d",
       color="Calibrated Model") +
  theme_bw() +
  theme(axis.text.x=element_text(size=18,vjust=0.5,hjust=0,color=c("black")),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18, margin=margin(0,0.5,0,0,"cm")),
        axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.key=element_blank(),
        legend.title=element_text(color="black",size=24,margin=margin(1,0,1,0,"cm")),
        legend.text=element_text(size=18),
        legend.key.width=unit(1.5,"cm"),
        legend.key.spacing.y=unit(1,"cm"),
        legend.title.align=0.5,
        panel.spacing.y=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=4),byrow=TRUE))

################################################################################

StartDate = as.Date("1/1/2017",format="%m/%d/%Y")
EndDate = as.Date("3/1/2017",format="%m/%d/%Y")

g2 = ggplot() +
  geom_line(data=HydrographData[HydrographData$MeasOrMod=="Modeled",],
            aes(x=Date,y=Streamflow_mmd,color=Model,linewidth=Model),
            lineend="round") +
  geom_line(data=HydrographData[HydrographData$MeasOrMod=="Measured",],
            aes(x=Date,y=Streamflow_mmd),
            linewidth=2, lineend="round", linetype=1, color="white") +
  geom_line(data=HydrographData[HydrographData$MeasOrMod=="Measured",],
            aes(x=Date,y=Streamflow_mmd,linetype="USGS 11413000"),
            linewidth=2, lineend="round", color="black") +
  scale_color_manual(values=c("red","violet","dodgerblue")) +
  scale_linewidth_manual(values=c(4,3,2),guide="none") +
  scale_linetype_manual(name="\nMeasured",values=c(3),
                        guide=guide_legend(override.aes=list(color="black",
                                                             linewidth=2))) +
  scale_x_date(limits=c(StartDate,
                        EndDate),
               breaks=seq(min(HydrographData$Date),
                          max(HydrographData$Date),
                          by="months"),
               labels=function(x) format(x,"%b-%Y"),
               expand=c(0,0),
               minor_breaks=seq(min(HydrographData$Date),
                                max(HydrographData$Date),
                                by="days"),
               guide="axis_minor") +
  scale_y_continuous(limits=c(0,80),
                     breaks=seq(0,80,10),
                     expand=c(0,0)) +
  # scale_y_log10() +
  labs(x="Water Year",
       y="Streamflow, mm / d",
       color="Calibrated Model") +
  theme_bw() +
  theme(axis.text.x=element_text(size=18,vjust=0.5,hjust=0,color=c("black")),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18, margin=margin(0,0.5,0,0,"cm")),
        axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.key=element_blank(),
        legend.title=element_text(color="black",size=24,margin=margin(1,0,1,0,"cm")),
        legend.text=element_text(size=18),
        legend.key.width=unit(1.5,"cm"),
        legend.key.spacing.y=unit(1,"cm"),
        legend.title.align=0.5,
        panel.spacing.y=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=4),byrow=TRUE))

g = patchwork::wrap_plots(list(g1,g2), nrow=2, guides="collect") &
  theme(legend.position="right")
print(g)

ggsave("CalValHydrographs_Log.png", plot=g,
       width=unit(16,"in"), height=unit(9,"in"), dpi=300)
