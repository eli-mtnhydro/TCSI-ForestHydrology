################################################################################
# Visualize DHSVM climate projections (static veg. scenario)
# Eli Boardman, 2023
# Creates relevant visualizations of streamflow regime and peak flows
# Requires post-processed DHSVM outputs as CSV files
################################################################################

library(ggplot2)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Results/"
setwd(dir)

# Read in post-processed DHSVM results

StreamflowResults = read.csv("StaticVeg_TCSI_Gauge-11413000_daily_streamflow.csv")
StreamflowResults$Streamflow_mmd = StreamflowResults$Streamflow_mmd * 1000 # Fix units (erroneously saved as m/d, not mm/d)
StreamflowResults$Date = as.Date(StreamflowResults$Date, "%Y-%m-%d")
head(StreamflowResults)

# Read raw USGS data and process into appropriate format/structure

# CAUTION: assumes only one basin is being considered !!!
StreamflowRecords = read.table("HistoricData/USGS_11413000.txt", sep="\t")
StreamflowRecords = StreamflowRecords[-(1:2),] # Drop 2 header lines
names(StreamflowRecords) = c("Agency", "GaugeID", "Date", "Streamflow_cfs", "Flag")
StreamflowRecords$Date = as.Date(StreamflowRecords$Date, format="%Y-%m-%d")
StreamflowRecords$Streamflow_cms = as.numeric(StreamflowRecords$Streamflow_cfs) / (3.28084^3) # ft^3/s --> m^3/s
length(which(is.na(StreamflowRecords$Streamflow_cms))) # Missing values
StreamflowRecords$Streamflow_mmd = 1000 * (60*60*24) * StreamflowRecords$Streamflow_cms / StreamflowResults$Area[1] # Convert to mm/d

StreamflowRecords = data.frame(Date=StreamflowRecords$Date,
                               Streamflow_cms=StreamflowRecords$Streamflow_cms,
                               GaugeID=paste0("Gauge_",StreamflowRecords$GaugeID),
                               GaugeName=StreamflowResults$GaugeName[1],
                               Climate="Actual Historical",
                               Scenario="Historical",
                               DHSVMmodel=NA,
                               LANDISrun=NA,
                               Area=StreamflowResults$Area[1],
                               Streamflow_mmd=StreamflowRecords$Streamflow_mmd)
head(StreamflowRecords)

# For ease of use with ggplot
CombinedData = rbind(StreamflowRecords, StreamflowResults)

################################################################################
# Plot separate daily hydrographs for each GCM

ggplot(data=StreamflowResults, aes(x=Date,y=Streamflow_mmd, group=1)) + 
  geom_line(color="deepskyblue2", size=1) +
  scale_x_date(breaks=seq(as.Date("2020-01-01"),as.Date("2100-01-01"),by="10 year"),date_labels="%Y")  +
  labs(y="Streamflow, mm/d", title="North Yuba River Streamflow Projections (Static Vegetation)") +
  theme_bw() +
  theme(axis.text.x=element_text(size=24, angle=45, vjust=0.5,color="black"), axis.text.y=element_text(size=18,color="black"),
        axis.title.x=element_blank(), axis.title.y=element_text(size=36)) +
  theme(axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,1,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"), panel.grid=element_blank()) +
  theme(plot.title=element_text(color="black",size=36,hjust=0.5,face="bold"),legend.position="bottom") +
  theme(strip.text=element_text(color="black",size=24),strip.background=element_rect(color=NA,fill=NA)) +
  facet_grid(rows=vars(Climate))

################################################################################
# Plot combined daily hydrographs

ggplot(data=CombinedData, aes(x=Date,y=Streamflow_mmd, col=Climate)) + 
  geom_line(size=1) +
  scale_color_manual(values=c("black","dodgerblue","red")) +
  scale_x_date(breaks=seq(as.Date("1930-01-01"),as.Date("2100-01-01"),by="10 years"),date_labels="%Y")  +
  labs(y="Streamflow, mm/d", title="North Yuba River: Measured and Projected Streamflow") +
  theme_bw() +
  theme(axis.text.x=element_text(size=18, angle=45, vjust=0.5,color="black"), axis.text.y=element_text(size=18,color="black"),
        axis.title.x=element_blank(), axis.title.y=element_text(size=24)) +
  theme(axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"), panel.grid=element_blank(),
        legend.title=element_text(size=24), legend.text=element_text(size=18)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),legend.position="right") +
  theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA)) +
  guides(color=guide_legend(override.aes=list(size=2),byrow=TRUE))

################################################################################
# Make and plot flow duration curves

QofInterestHistoric = CombinedData[(CombinedData$Climate=="Actual Historical" &
                                      CombinedData$Date >= as.Date("1939-10-01") &
                                      CombinedData$Date <= as.Date("2019-9-30")),"Streamflow_mmd"]

QofInterestCNRM = CombinedData[(CombinedData$Climate=="CNRM-CM5 RCP8.5"),"Streamflow_mmd"]
QofInterestMIROC = CombinedData[(CombinedData$Climate=="MIROC5 RCP8.5"),"Streamflow_mmd"]

QofInterestHistoric = rev(sort(QofInterestHistoric))
QofInterestCNRM = rev(sort(QofInterestCNRM))
QofInterestMIROC = rev(sort(QofInterestMIROC))

ExceedanceValsHistoric = 1/(length(QofInterestHistoric)/(1:length(QofInterestHistoric)))
ExceedanceValsFuture = 1/(length(QofInterestCNRM)/(1:length(QofInterestCNRM)))

par(mar=c(5,6,5,3))
plot(ExceedanceValsHistoric, QofInterestHistoric, type="l", log="y", ylim=c(0.1, 200), lwd=3, col="black",
     xlab="Probability of Exceedance", ylab="Streamflow, mm/d",
     main="North Yuba River Flow Duration Curve", cex.main=2, cex.lab=2, cex.axis=1.5)
lines(ExceedanceValsFuture, QofInterestCNRM, lwd=3, col="dodgerblue")
lines(ExceedanceValsFuture, QofInterestMIROC, lwd=3, col="red")
legend("topright", inset=0.1, legend=c("Actual Historical","CNRM-CM5 RCP8.5", "MIROC5 RCP8.5"),
       lwd=c(5,5), col=c("black","dodgerblue","red"), cex=1.5)

# Subset for highest flows
plot(ExceedanceValsHistoric, QofInterestHistoric, type="l", log="y", xlim=c(0,0.1), ylim=c(5, 200), lwd=3, col="black",
     xlab="Probability of Exceedance", ylab="Streamflow, mm/d",
     main="North Yuba River Flow Duration Curve", cex.main=2, cex.lab=2, cex.axis=1.5)
lines(ExceedanceValsFuture, QofInterestCNRM, lwd=3, col="dodgerblue")
lines(ExceedanceValsFuture, QofInterestMIROC, lwd=3, col="red")
legend("topright", inset=0.1, legend=c("Actual Historical","CNRM-CM5 RCP8.5", "MIROC5 RCP8.5"),
       lwd=c(5,5), col=c("black","dodgerblue","red"), cex=1.5)
box(lwd=1)

# Log-log
plot(ExceedanceValsHistoric, QofInterestHistoric, type="l", log="xy", ylim=c(0.1,200), lwd=3, col="black",
     xlab="Probability of Exceedance", ylab="Streamflow, mm/d",
     main="North Yuba River Flow Duration Curve", cex.main=2, cex.lab=2, cex.axis=1.5)
lines(ExceedanceValsFuture, QofInterestCNRM, lwd=3, col="dodgerblue")
lines(ExceedanceValsFuture, QofInterestMIROC, lwd=3, col="red")
legend("bottomleft", inset=0.1, legend=c("Actual Historical","CNRM-CM5 RCP8.5", "MIROC5 RCP8.5"),
       lwd=c(5,5), col=c("black","dodgerblue","red"), cex=1.5)

# Nicer flow duration curves in ggplot

ExceedanceData = data.frame(Streamflow=c(QofInterestHistoric,QofInterestCNRM,QofInterestMIROC),
                            ExceedanceProb=c(ExceedanceValsHistoric,ExceedanceValsFuture,ExceedanceValsFuture),
                            Climate=c(rep("Actual Historical\n(1940-2019)",length(QofInterestHistoric)),
                                      rep("CNRM-CM5 RCP8.5\n(2020-2099)",length(QofInterestCNRM)),
                                      rep("MIROC5 RCP8.5\n(2020-2099)",length(QofInterestMIROC))))

ggplot(data=ExceedanceData, aes(x=ExceedanceProb,y=Streamflow, col=Climate)) + 
  geom_line(size=2) +
  scale_color_manual(values=c("black","dodgerblue","red")) +
  scale_x_log10(limits=c(1e-5,1.5), expand=c(0,0),
                breaks=scales::trans_breaks("log10", function(x) 10^x),
                labels=function(x) paste0(x*100, "%")) +
  scale_y_log10(limits=c(1e-1,3e2), expand=c(0,0),
                breaks=c(1e-1,1e0,1e1,1e2),
                labels=scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks() +
  labs(x="Probability of Exceedance", y="Streamflow, mm/d", title="North Yuba River Flow Duration Curves") +
  theme_bw() +
  theme(axis.text.x=element_text(size=18, vjust=0.5,color="black"), axis.text.y=element_text(size=18,color="black"),
        axis.title.x=element_text(size=24, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=24, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"), panel.grid=element_blank(),
        legend.title=element_text(size=24), legend.text=element_text(size=18)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),legend.position="right") +
  theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA)) +
  guides(color=guide_legend(override.aes=list(size=2),byrow=TRUE))

# Make samep plot with legend below using factor levels

ExceedanceData2 = ExceedanceData
ExceedanceData2$Climate = factor(ExceedanceData$Climate,
                                 levels=c("CNRM-CM5 RCP8.5\n(2020-2099)",
                                          "MIROC5 RCP8.5\n(2020-2099)",
                                          "Actual Historical\n(1940-2019)"))

ggplot(data=ExceedanceData2, aes(x=ExceedanceProb,y=Streamflow, col=Climate)) + 
  geom_line(size=2) +
  scale_color_manual(values=c("dodgerblue","red","black")) +
  scale_x_log10(limits=c(1e-5,1.5), expand=c(0,0),
                breaks=scales::trans_breaks("log10", function(x) 10^x),
                labels=function(x) paste0(x*100, "%")) +
  scale_y_log10(limits=c(1e-1,3e2), expand=c(0,0),
                breaks=c(1e-1,1e0,1e1,1e2),
                labels=scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks() +
  labs(x="Probability of Exceedance", y="Streamflow, mm/d", title="North Yuba River Flow Duration Curves") +
  theme_bw() +
  theme(axis.text.x=element_text(size=18, vjust=0.5,color="black"), axis.text.y=element_text(size=18,color="black"),
        axis.title.x=element_text(size=24, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=24, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"), panel.grid=element_blank(),
        legend.title=element_text(size=24), legend.text=element_text(size=18)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),legend.position="bottom") +
  theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA)) +
  guides(color=guide_legend(override.aes=list(size=2),nrow=2,byrow=TRUE))

# max(QofInterestHistoric)
# max(QofInterestCNRM)
# max(QofInterestMIROC)
max(QofInterestCNRM)/max(QofInterestHistoric)
max(QofInterestMIROC)/max(QofInterestHistoric)

# mean(QofInterestHistoric)
# mean(QofInterestCNRM)
# mean(QofInterestMIROC)
mean(QofInterestCNRM)/mean(QofInterestHistoric)
mean(QofInterestMIROC)/mean(QofInterestHistoric)

################################################################################
# Identify and plot yearly peaks

Years = 2020:2099
YearlyPeakCNRM = c()
YearlyPeakMIROC = c()

for (yr in Years){
  YearPtrs = (as.numeric(format(StreamflowResults$Date, "%Y")) == yr)
  
  YearlyPeakCNRM = c(YearlyPeakCNRM,
                     max(StreamflowResults[(YearPtrs &
                                              StreamflowResults$Climate == "CNRM-CM5 RCP8.5"),
                                           "Streamflow_mmd"]))
  YearlyPeakMIROC = c(YearlyPeakMIROC,
                      max(StreamflowResults[(YearPtrs &
                                               StreamflowResults$Climate == "MIROC5 RCP8.5"),
                                            "Streamflow_mmd"]))
  
  print(yr)
}

plot(Years, YearlyPeakCNRM, col="blue", type="l")
lines(Years, YearlyPeakMIROC, col="red")

# Nicer plot in ggplot

YearlyPeakData = data.frame(Years=1:length(Years), YearlyPeakCNRM=YearlyPeakCNRM, YearlyPeakMIROC=YearlyPeakMIROC)
YearlyPeakDataMerged = data.frame(Years=rep(Years,2),
                                  YearlyPeak=c(YearlyPeakCNRM, YearlyPeakMIROC),
                                  Climate=c(rep("CNRM-CM5 RCP8.5",length(Years)),
                                            rep("MIROC5 RCP8.5",length(Years))))

ggplot(data=YearlyPeakDataMerged, aes(x=Years,y=YearlyPeak, group=1)) + 
  geom_line(color="deepskyblue2", size=2) +
  labs(y="Daily Streamflow, mm/d", title="North Yuba River Yearly Peak Flows") +
  theme_bw() +
  theme(axis.text.x=element_text(size=24, angle=45, vjust=0.5,color="black"), axis.text.y=element_text(size=18,color="black"),
        axis.title.x=element_blank(), axis.title.y=element_text(size=36)) +
  theme(axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,1,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"), panel.grid=element_blank()) +
  theme(plot.title=element_text(color="black",size=36,hjust=0.5,face="bold"),legend.position="bottom") +
  theme(strip.text=element_text(color="black",size=24),strip.background=element_rect(color=NA,fill=NA)) +
  facet_grid(rows=vars(Climate))

################################################################################
# Calculate yearly peak statistics

library(Kendall)
library(trend)

MannKendall(YearlyPeakCNRM)
lm(YearlyPeakCNRM ~ Years, YearlyPeakData)
sens.slope(YearlyPeakCNRM)

MannKendall(YearlyPeakMIROC)
lm(YearlyPeakMIROC ~ Years, YearlyPeakData)
sens.slope(YearlyPeakMIROC)
