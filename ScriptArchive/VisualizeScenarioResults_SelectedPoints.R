library(ggplot2)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Results/"
setwd(dir)

# Read in pre-processed DHSVM results
StreamflowResults = read.csv("Scenarios1-6_CNRM-MIROC_DailyStreamflow_SelectedPoints.csv")
#StreamflowResults$Date = as.Date(StreamflowResults$Date, "%Y-%m-%d")
StreamflowResults$Date = rep(seq(as.Date("2014-10-01",tz="UTC"),as.Date("2099-09-30",tz="UTC"),by="days"), 6*2*7)
# For some reason the saved dates are somewhat corrupted--need to look into this

dim(StreamflowResults)
head(StreamflowResults)

GaugeNames = unique(StreamflowResults$GaugeName)
Climates = unique(StreamflowResults$Climate)

# ################################################################################
# # Separate daily hydrographs
# 
# ggplot(data=StreamflowResults[StreamflowResults$Scenario==1,], aes(x=Date,y=Streamflow_mmd, group=1)) + 
#   geom_line(color="deepskyblue2") +
#   scale_x_date(breaks=seq(as.Date("2020-01-01"),as.Date("2100-01-01"),by="10 year"),date_labels="%Y")  +
#   labs(y="Streamflow, mm/d", title="Streamflow Projections for TCSI\nScenario 1") +
#   theme_bw() +
#   theme(axis.text.x=element_text(size=18, angle=45, vjust=0.5,color="black"), axis.title.x=element_blank(), axis.title.y=element_text(size=24), axis.text.y=element_text(size=18,color="black")) +
#   theme(axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,1,0.5,0.5,"cm"),panel.background=element_rect("white", "black"), panel.grid=element_blank()) +
#   theme(plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),legend.position="bottom") +
#   theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA)) +
#   facet_grid(rows=vars(GaugeName))
# 
# ################################################################################
# # Ratio of daily flows
# 
# ScenarioData = StreamflowResults[(StreamflowResults$Climate=="CNRM-CM5 RCP 8.5" &
#                                      StreamflowResults$Scenario==1),c(1,4,5,7,8,9)]
# ScenarioData$Scenario1_mmd = StreamflowResults[(StreamflowResults$Climate=="CNRM-CM5 RCP 8.5" &
#                                                   StreamflowResults$Scenario==1),"Streamflow_mmd"]
# ScenarioData$Scenario2_mmd = StreamflowResults[(StreamflowResults$Climate=="CNRM-CM5 RCP 8.5" &
#                                                   StreamflowResults$Scenario==2),"Streamflow_mmd"]
# ScenarioData$Scenario3_mmd = StreamflowResults[(StreamflowResults$Climate=="CNRM-CM5 RCP 8.5" &
#                                                   StreamflowResults$Scenario==3),"Streamflow_mmd"]
# ScenarioData$Scenario4_mmd = StreamflowResults[(StreamflowResults$Climate=="CNRM-CM5 RCP 8.5" &
#                                                   StreamflowResults$Scenario==4),"Streamflow_mmd"]
# ScenarioData$Scenario5_mmd = StreamflowResults[(StreamflowResults$Climate=="CNRM-CM5 RCP 8.5" &
#                                                   StreamflowResults$Scenario==5),"Streamflow_mmd"]
# ScenarioData$Scenario6_mmd = StreamflowResults[(StreamflowResults$Climate=="CNRM-CM5 RCP 8.5" &
#                                                   StreamflowResults$Scenario==6),"Streamflow_mmd"]
# # Combinations
# ScenarioData$Sc2overSc1 = ScenarioData$Scenario2_mmd/ScenarioData$Scenario1_mmd
# ScenarioData$Sc3overSc1 = ScenarioData$Scenario3_mmd/ScenarioData$Scenario1_mmd
# ScenarioData$Sc4overSc1 = ScenarioData$Scenario4_mmd/ScenarioData$Scenario1_mmd
# ScenarioData$Sc5overSc1 = ScenarioData$Scenario5_mmd/ScenarioData$Scenario1_mmd
# ScenarioData$Sc6overSc1 = ScenarioData$Scenario6_mmd/ScenarioData$Scenario1_mmd
# 
# head(ScenarioData)
# 
# plot(ScenarioData$Scenario1_mmd, ScenarioData$Sc6overSc1)
# 
# ggplot(data=ScenarioData[ScenarioData$GaugeName=="Bear",], aes(x=Date,y=Sc2overSc1, group=1)) + 
#   geom_line(color="dodgerblue2") +
#   geom_hline(yintercept=1, linetype="dashed", color="black", size=1) +
#   scale_x_date(breaks=seq(as.Date("2020-01-01"),as.Date("2100-01-01"),by="10 year"),date_labels="%Y")  +
#   scale_y_continuous(limits=c(0.9,1.3), labels=function(x) paste0(x*100, "%")) +
#   labs(y="Daily Streamflow Ratio", title="Streamflow Projections for Bear River (CNRM-CM5 RCP 8.5)\nRatio of Scenario 2 to Scenario 1") +
#   theme_bw() +
#   theme(axis.text.x=element_text(size=18, angle=45, vjust=0.5,color="black"), axis.title.x=element_blank(), axis.title.y=element_text(size=24, margin=margin(0,0.5,0,0,"cm")), axis.text.y=element_text(size=18,color="black")) +
#   theme(axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,1,0.5,0.5,"cm"),panel.background=element_rect("white", "black"), panel.grid=element_blank()) +
#   theme(plot.title=element_text(color="black",size=20,hjust=0.5,face="bold"),legend.position="bottom") +
#   theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA))
# #facet_grid(rows=vars(GaugeName))
# 
# ggplot(data=ScenarioData[ScenarioData$GaugeName=="Bear",], aes(x=Date,y=Sc3overSc1, group=1)) + 
#   geom_line(color="dodgerblue2") +
#   geom_hline(yintercept=1, linetype="dashed", color="black", size=1) +
#   scale_x_date(breaks=seq(as.Date("2020-01-01"),as.Date("2100-01-01"),by="10 year"),date_labels="%Y")  +
#   scale_y_continuous(limits=c(0.9,1.3), labels=function(x) paste0(x*100, "%")) +
#   labs(y="Daily Streamflow Ratio", title="Streamflow Projections for Bear River (CNRM-CM5 RCP 8.5)\nRatio of Scenario 3 to Scenario 1") +
#   theme_bw() +
#   theme(axis.text.x=element_text(size=18, angle=45, vjust=0.5,color="black"), axis.title.x=element_blank(), axis.title.y=element_text(size=24, margin=margin(0,0.5,0,0,"cm")), axis.text.y=element_text(size=18,color="black")) +
#   theme(axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,1,0.5,0.5,"cm"),panel.background=element_rect("white", "black"), panel.grid=element_blank()) +
#   theme(plot.title=element_text(color="black",size=20,hjust=0.5,face="bold"),legend.position="bottom") +
#   theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA))
# #facet_grid(rows=vars(GaugeName))
# 
# ggplot(data=ScenarioData[ScenarioData$GaugeName=="Bear",], aes(x=Date,y=Sc4overSc1, group=1)) + 
#   geom_line(color="dodgerblue2") +
#   geom_hline(yintercept=1, linetype="dashed", color="black", size=1) +
#   scale_x_date(breaks=seq(as.Date("2020-01-01"),as.Date("2100-01-01"),by="10 year"),date_labels="%Y")  +
#   scale_y_continuous(limits=c(0.9,1.3), labels=function(x) paste0(x*100, "%")) +
#   labs(y="Daily Streamflow Ratio", title="Streamflow Projections for Bear River (CNRM-CM5 RCP 8.5)\nRatio of Scenario 4 to Scenario 1") +
#   theme_bw() +
#   theme(axis.text.x=element_text(size=18, angle=45, vjust=0.5,color="black"), axis.title.x=element_blank(), axis.title.y=element_text(size=24, margin=margin(0,0.5,0,0,"cm")), axis.text.y=element_text(size=18,color="black")) +
#   theme(axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,1,0.5,0.5,"cm"),panel.background=element_rect("white", "black"), panel.grid=element_blank()) +
#   theme(plot.title=element_text(color="black",size=20,hjust=0.5,face="bold"),legend.position="bottom") +
#   theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA))
# #facet_grid(rows=vars(GaugeName))
# 
# ggplot(data=ScenarioData[ScenarioData$GaugeName=="Bear",], aes(x=Date,y=Sc5overSc1, group=1)) + 
#   geom_line(color="dodgerblue2") +
#   geom_hline(yintercept=1, linetype="dashed", color="black", size=1) +
#   scale_x_date(breaks=seq(as.Date("2020-01-01"),as.Date("2100-01-01"),by="10 year"),date_labels="%Y")  +
#   scale_y_continuous(limits=c(0.9,1.3), labels=function(x) paste0(x*100, "%")) +
#   labs(y="Daily Streamflow Ratio", title="Streamflow Projections for Bear River (CNRM-CM5 RCP 8.5)\nRatio of Scenario 5 to Scenario 1") +
#   theme_bw() +
#   theme(axis.text.x=element_text(size=18, angle=45, vjust=0.5,color="black"), axis.title.x=element_blank(), axis.title.y=element_text(size=24, margin=margin(0,0.5,0,0,"cm")), axis.text.y=element_text(size=18,color="black")) +
#   theme(axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,1,0.5,0.5,"cm"),panel.background=element_rect("white", "black"), panel.grid=element_blank()) +
#   theme(plot.title=element_text(color="black",size=20,hjust=0.5,face="bold"),legend.position="bottom") +
#   theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA))
# #facet_grid(rows=vars(GaugeName))
# 
# ggplot(data=ScenarioData[ScenarioData$GaugeName=="Bear",], aes(x=Date,y=Sc6overSc1, group=1)) + 
#   geom_line(color="dodgerblue2") +
#   geom_hline(yintercept=1, linetype="dashed", color="black", size=1) +
#   scale_x_date(breaks=seq(as.Date("2020-01-01"),as.Date("2100-01-01"),by="10 year"),date_labels="%Y")  +
#   scale_y_continuous(limits=c(0.9,1.3), labels=function(x) paste0(x*100, "%")) +
#   labs(y="Daily Streamflow Ratio", title="Streamflow Projections for Bear River (CNRM-CM5 RCP 8.5)\nRatio of Scenario 6 to Scenario 1") +
#   theme_bw() +
#   theme(axis.text.x=element_text(size=18, angle=45, vjust=0.5,color="black"), axis.title.x=element_blank(), axis.title.y=element_text(size=24, margin=margin(0,0.5,0,0,"cm")), axis.text.y=element_text(size=18,color="black")) +
#   theme(axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,1,0.5,0.5,"cm"),panel.background=element_rect("white", "black"), panel.grid=element_blank()) +
#   theme(plot.title=element_text(color="black",size=20,hjust=0.5,face="bold"),legend.position="bottom") +
#   theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA))
#   #facet_grid(rows=vars(GaugeName))

################################################################################
# Cumulative runoff

for (GaugeName in GaugeNames){
  print(paste0("********** ",GaugeName," **********"))
  
  FirstClimate = 1
  for (Climate in Climates){
    S1cumulative = cumsum(StreamflowResults[(StreamflowResults$GaugeName==GaugeName &
                                               StreamflowResults$Climate==Climate &
                                               StreamflowResults$Scenario==1),"Streamflow_mmd"])/1000 # m depth
    
    S2cumulative = cumsum(StreamflowResults[(StreamflowResults$GaugeName==GaugeName &
                                               StreamflowResults$Climate==Climate &
                                               StreamflowResults$Scenario==2),"Streamflow_mmd"])/1000 # m depth
    
    S3cumulative = cumsum(StreamflowResults[(StreamflowResults$GaugeName==GaugeName &
                                               StreamflowResults$Climate==Climate &
                                               StreamflowResults$Scenario==3),"Streamflow_mmd"])/1000 # m depth
    
    S4cumulative = cumsum(StreamflowResults[(StreamflowResults$GaugeName==GaugeName &
                                               StreamflowResults$Climate==Climate &
                                               StreamflowResults$Scenario==4),"Streamflow_mmd"])/1000 # m depth
    
    S5cumulative = cumsum(StreamflowResults[(StreamflowResults$GaugeName==GaugeName &
                                               StreamflowResults$Climate==Climate &
                                               StreamflowResults$Scenario==5),"Streamflow_mmd"])/1000 # m depth
    
    S6cumulative = cumsum(StreamflowResults[(StreamflowResults$GaugeName==GaugeName &
                                               StreamflowResults$Climate==Climate &
                                               StreamflowResults$Scenario==6),"Streamflow_mmd"])/1000 # m depth
    
    if (FirstClimate){
      CumulativeData = data.frame(Date=rep(unique(StreamflowResults$Date), 6),
                                  Scenario=sort(rep(1:6, length(S1cumulative))),
                                  Climate=rep(Climate, length(S1cumulative)*6),
                                  CumulativeQ_m=c(S1cumulative,S2cumulative,S3cumulative,S4cumulative,S5cumulative,S6cumulative))
      FirstClimate = 0
      
    } else {
      CumulativeData = rbind(CumulativeData,
                             data.frame(Date=rep(unique(StreamflowResults$Date), 6),
                                        Scenario=sort(rep(1:6, length(S1cumulative))),
                                        Climate=rep(Climate, length(S1cumulative)*6),
                                        CumulativeQ_m=c(S1cumulative,S2cumulative,S3cumulative,S4cumulative,S5cumulative,S6cumulative)))
    }
    
    print(paste0(Climate, " S6/S1 cumulative: ", round(max(S6cumulative)/max(S1cumulative), 2)))
    
  }
  
  # Nicer plot titles
  if (!grepl(" ", GaugeName)){
    GaugeName = paste0(GaugeName, " River")
  }
  
  g1 = ggplot(data=CumulativeData, aes(x=Date,y=CumulativeQ_m, col=Scenario, group=Scenario)) +
    geom_line(size=0.5) +
    scale_x_date(breaks=seq(as.Date("2020-01-01"),as.Date("2100-01-01"),by="10 years"),date_labels="%Y")  +
    labs(y="Cumulative Streamflow, m", title=paste0("Streamflow Projections for ",GaugeName)) +
    theme_bw() +
    theme(axis.text.x=element_text(size=18, angle=45, vjust=0.5,color="black"), axis.title.x=element_blank(), axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm")), axis.text.y=element_text(size=18,color="black")) +
    theme(axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,1,0.5,0.5,"cm"),panel.background=element_rect("white", "black"), panel.grid=element_blank(), legend.title=element_text(size=28), legend.text=element_text(size=18)) +
    theme(plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),legend.position="bottom", legend.key.width=unit(1.5,"cm")) +
    theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA)) +
    guides(color=guide_legend(override.aes=list(size=2),byrow=TRUE)) +
    facet_grid(rows=vars(Climate))
  
  print(g1)
  Sys.sleep(10)
  
  print("**********")
}

################################################################################
# Flow duration curves

for (GaugeName in GaugeNames){
  print(paste0("********** ",GaugeName," **********"))
  
  FirstClimate = 1
  for (Climate in Climates){
    
    QofInterestS1 = StreamflowResults[(StreamflowResults$Climate==Climate &
                                         StreamflowResults$GaugeName==GaugeName &
                                         StreamflowResults$Scenario==1),"Streamflow_mmd"]
    
    QofInterestS2 = StreamflowResults[(StreamflowResults$Climate==Climate &
                                         StreamflowResults$GaugeName==GaugeName &
                                         StreamflowResults$Scenario==2),"Streamflow_mmd"]
    
    QofInterestS3 = StreamflowResults[(StreamflowResults$Climate==Climate &
                                         StreamflowResults$GaugeName==GaugeName &
                                         StreamflowResults$Scenario==3),"Streamflow_mmd"]
    
    QofInterestS4 = StreamflowResults[(StreamflowResults$Climate==Climate &
                                         StreamflowResults$GaugeName==GaugeName &
                                         StreamflowResults$Scenario==4),"Streamflow_mmd"]
    
    QofInterestS5 = StreamflowResults[(StreamflowResults$Climate==Climate &
                                         StreamflowResults$GaugeName==GaugeName &
                                         StreamflowResults$Scenario==5),"Streamflow_mmd"]
    
    QofInterestS6 = StreamflowResults[(StreamflowResults$Climate==Climate &
                                         StreamflowResults$GaugeName==GaugeName &
                                         StreamflowResults$Scenario==6),"Streamflow_mmd"]
    
    QofInterestS1 = rev(sort(QofInterestS1))
    QofInterestS2 = rev(sort(QofInterestS2))
    QofInterestS3 = rev(sort(QofInterestS3))
    QofInterestS4 = rev(sort(QofInterestS4))
    QofInterestS5 = rev(sort(QofInterestS5))
    QofInterestS6 = rev(sort(QofInterestS6))
    
    ExceedanceVals = 1/(length(QofInterestS1)/(1:length(QofInterestS1)))
    
    if (FirstClimate){
      ExceedanceData = data.frame(Streamflow=c(QofInterestS1,QofInterestS2,QofInterestS3,QofInterestS4,QofInterestS5,QofInterestS6),
                                  ExceedanceProb=c(rep(ExceedanceVals, 6)),
                                  Scenario=sort(rep(1:6, length(QofInterestS1))),
                                  Climate=rep(Climate, 6*length(QofInterestS1)))
      FirstClimate = 0
      
    } else {
      ExceedanceData = rbind(ExceedanceData,
                             data.frame(Streamflow=c(QofInterestS1,QofInterestS2,QofInterestS3,QofInterestS4,QofInterestS5,QofInterestS6),
                                        ExceedanceProb=c(rep(ExceedanceVals, 6)),
                                        Scenario=sort(rep(1:6, length(QofInterestS1))),
                                        Climate=rep(Climate, 6*length(QofInterestS1))))
    }
    
    print(paste0(Climate, " S6/S1 max daily flow: ", round(max(QofInterestS6)/max(QofInterestS1), 2)))
  }
  
  # Nicer plot titles
  if (!grepl(" ", GaugeName)){
    GaugeName = paste0(GaugeName, " River")
  }
  
  g1 = ggplot(data=ExceedanceData, aes(x=ExceedanceProb, y=Streamflow, col=Scenario)) + 
    geom_point(size=1) +
    labs(x="Probability of Exceedance", y="Streamflow, mm/d", title=paste0("Flow Duration Curves for ",GaugeName)) +
    scale_x_log10(limits=c(1e-5,1), expand=c(0,0), breaks=scales::trans_breaks("log10", function(x) 10^x), labels=function(x) paste0(x*100, "%")) +
    annotation_logticks(sides="b") +
    theme_bw() +
    theme(axis.text.x=element_text(size=18, vjust=0.5,color="black"), axis.title.x=element_text(size=28, margin=margin(0.5,0,0,0,"cm")), axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm")), axis.text.y=element_text(size=18,color="black")) +
    theme(axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),panel.background=element_rect("white", "black"), panel.grid=element_blank(), legend.title=element_text(size=28), legend.text=element_text(size=18)) +
    theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),legend.position="right") +
    theme(strip.text=element_text(color="black",size=18),strip.background=element_rect(color=NA,fill=NA)) +
    guides(color=guide_legend(override.aes=list(size=6),byrow=TRUE)) +
    facet_grid(rows=vars(Climate))
  
  print(g1)
  Sys.sleep(10)
  
  print("**********")
}



