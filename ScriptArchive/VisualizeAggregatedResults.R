
library(ggplot2)
library(data.table)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Results"
setwd(dir)

WaterYears = 2015:2099

BasinNames = c("Truckee", "Yuba", "Bear", "American")
DHSVMmodels = c(195,270,276)
Scenarios = 1:6
Climates = c("cnrm","miroc")

################################################################################

#AggregatedData = read.csv("AggregatedDailyData_AllBasinsModelsClimatesScenarios.csv")[,-1]
AggregatedData = as.data.frame(fread("AggregatedDailyData_AllBasinsModelsClimatesScenarios.csv"))[,-1]

ValueNames = names(AggregatedData)[-(1:5)]

# Overstory is "Story0"
# Undestory is "Story1"

QuickSubset = which(AggregatedData$DHSVMmodel==276 &
                      AggregatedData$Scenario==6 &
                      AggregatedData$Climate=="cnrm" &
                      AggregatedData$Basin=="Truckee")

plot(AggregatedData[QuickSubset,"ActTranspStory1"], type="l", ylim=c(0,0.0003))

################################################################################
# Aggregate to yearly values

# Numeric water year corresponding to each date
WaterYearOfDate = as.numeric(format(unique(AggregatedData$Date),"%Y"))
OctNovDecPtrs = which(as.numeric(format(unique(AggregatedData$Date),"%m")) >= 10)
WaterYearOfDate[OctNovDecPtrs] = WaterYearOfDate[OctNovDecPtrs] + 1

if (exists("YearlyAggregatedData")){
  remove(YearlyAggregatedData)
}

for (DHSVMmodel in DHSVMmodels){
  for (Scenario in Scenarios){
    for (Climate in Climates){
      for (BasinName in BasinNames){
        
        Ptrs = which(AggregatedData$Basin==BasinName &
                       AggregatedData$Climate==Climate &
                       AggregatedData$Scenario==Scenario &
                       AggregatedData$DHSVMmodel==DHSVMmodel)
        
        ResultsMatrix = data.frame(matrix(nrow=length(WaterYears),
                                          ncol=length(ValueNames)+5)) # Additional columns: water year, model, scenario, climate, basin
        names(ResultsMatrix) = c("WaterYear","DHSVMmodel","Scenario","Climate","Basin",ValueNames)
        ResultsMatrix$WaterYear = WaterYears
        
        yr = 1
        for (WaterYear in WaterYears){
          
          YearPtrs = Ptrs[which(WaterYearOfDate==WaterYear)]
          
          for (i in 1:length(ValueNames)){
            
            ResultsMatrix[yr,(i+5)] = mean(AggregatedData[YearPtrs,(i+5)])
          }
          
          yr = yr + 1
        }
        
        ResultsMatrix[,"DHSVMmodel"] = DHSVMmodel
        ResultsMatrix[,"Scenario"] = Scenario
        ResultsMatrix[,"Climate"] = Climate
        ResultsMatrix[,"Basin"] = BasinName
        
        # Append data from all basins / models / scenarios / climates
        if (!exists("YearlyAggregatedData")){
          YearlyAggregatedData = ResultsMatrix
        } else {
          YearlyAggregatedData = rbind(YearlyAggregatedData,ResultsMatrix)
        }
        
        print(paste("Done with",BasinName,Climate,Scenario,DHSVMmodel))
      }
    }
  }
}

########## Compute scenario effect relative to mean for selected variables

# Convert m/3-hour to mm/d for ET variables

YearlyAggregatedData$ActTranspStory0_mmd = YearlyAggregatedData$ActTranspStory0 * 1000 * 24 / 3
YearlyAggregatedData$ActTranspStory0_Rel_mmd = NA
YearlyAggregatedData$ActTranspStory0_Rel_pct = NA

YearlyAggregatedData$ActTranspStory1_mmd = YearlyAggregatedData$ActTranspStory1 * 1000 * 24 / 3
YearlyAggregatedData$ActTranspStory1_Rel_mmd = NA
YearlyAggregatedData$ActTranspStory1_Rel_pct = NA

YearlyAggregatedData$TotalET_mmd = YearlyAggregatedData$TotalET * 1000 * 24 / 3
YearlyAggregatedData$TotalET_Rel_mmd = NA
YearlyAggregatedData$TotalET_Rel_pct = NA

YearlyAggregatedData$EvapCanopyIntStory0_mmd = YearlyAggregatedData$EvapCanopyIntStory0 * 1000 * 24 / 3
YearlyAggregatedData$EvapCanopyIntStory0_Rel_mmd = NA
YearlyAggregatedData$EvapCanopyIntStory0_Rel_pct = NA

YearlyAggregatedData$EvapCanopyIntStory1_mmd = YearlyAggregatedData$EvapCanopyIntStory1 * 1000 * 24 / 3
YearlyAggregatedData$EvapCanopyIntStory1_Rel_mmd = NA
YearlyAggregatedData$EvapCanopyIntStory1_Rel_pct = NA

# Convert m to mm for interception variables

YearlyAggregatedData$IntRainStory0_mm = YearlyAggregatedData$IntRainStory0 * 1000
YearlyAggregatedData$IntRainStory0_Rel_mm = NA
YearlyAggregatedData$IntRainStory0_Rel_pct = NA

YearlyAggregatedData$IntRainStory1_mm = YearlyAggregatedData$IntRainStory1 * 1000
YearlyAggregatedData$IntRainStory1_Rel_mm = NA
YearlyAggregatedData$IntRainStory1_Rel_pct = NA

YearlyAggregatedData$IntSnowStory0_mm = YearlyAggregatedData$IntSnowStory0 * 1000
YearlyAggregatedData$IntSnowStory0_Rel_mm = NA
YearlyAggregatedData$IntSnowStory0_Rel_pct = NA

YearlyAggregatedData$IntSnowStory1_mm = YearlyAggregatedData$IntSnowStory1 * 1000
YearlyAggregatedData$IntSnowStory1_Rel_mm = NA
YearlyAggregatedData$IntSnowStory1_Rel_pct = NA

for (BasinName in BasinNames){
  for (Climate in Climates){
    for (DHSVMmodel in DHSVMmodels){
      for (WaterYear in WaterYears){
        
        Ptrs = which(YearlyAggregatedData$WaterYear==WaterYear & 
                       YearlyAggregatedData$DHSVMmodel==DHSVMmodel &
                       YearlyAggregatedData$Climate==Climate &
                       YearlyAggregatedData$Basin==BasinName)
        
        Avg_ActTranspStory0_mmd = mean(YearlyAggregatedData$ActTranspStory0_mmd[Ptrs])
        Avg_ActTranspStory1_mmd = mean(YearlyAggregatedData$ActTranspStory1_mmd[Ptrs])
        Avg_TotalET_mmd = mean(YearlyAggregatedData$TotalET_mmd[Ptrs])
        Avg_EvapCanopyIntStory0_mmd = mean(YearlyAggregatedData$EvapCanopyIntStory0_mmd[Ptrs])
        Avg_EvapCanopyIntStory1_mmd = mean(YearlyAggregatedData$EvapCanopyIntStory1_mmd[Ptrs])
        Avg_IntRainStory0_mm = mean(YearlyAggregatedData$IntRainStory0_mm[Ptrs])
        Avg_IntRainStory1_mm = mean(YearlyAggregatedData$IntRainStory1_mm[Ptrs])
        Avg_IntSnowStory0_mm = mean(YearlyAggregatedData$IntSnowStory0_mm[Ptrs])
        Avg_IntSnowStory1_mm = mean(YearlyAggregatedData$IntSnowStory1_mm[Ptrs])
        
        for (Scenario in Scenarios){
          
          Ptr = Ptrs[which(YearlyAggregatedData$Scenario[Ptrs]==Scenario)]
          
          YearlyAggregatedData$ActTranspStory0_Rel_mmd[Ptr] = YearlyAggregatedData$ActTranspStory0_mmd[Ptr] - Avg_ActTranspStory0_mmd
          YearlyAggregatedData$ActTranspStory0_Rel_pct[Ptr] = YearlyAggregatedData$ActTranspStory0_mmd[Ptr] / Avg_ActTranspStory0_mmd - 1
          
          YearlyAggregatedData$ActTranspStory1_Rel_mmd[Ptr] = YearlyAggregatedData$ActTranspStory1_mmd[Ptr] - Avg_ActTranspStory1_mmd
          YearlyAggregatedData$ActTranspStory1_Rel_pct[Ptr] = YearlyAggregatedData$ActTranspStory1_mmd[Ptr] / Avg_ActTranspStory1_mmd - 1
          
          YearlyAggregatedData$TotalET_Rel_mmd[Ptr] = YearlyAggregatedData$TotalET_mmd[Ptr] - Avg_TotalET_mmd
          YearlyAggregatedData$TotalET_Rel_pct[Ptr] = YearlyAggregatedData$TotalET_mmd[Ptr] / Avg_TotalET_mmd - 1
          
          YearlyAggregatedData$EvapCanopyIntStory0_Rel_mmd[Ptr] = YearlyAggregatedData$EvapCanopyIntStory0_mmd[Ptr] - Avg_EvapCanopyIntStory0_mmd
          YearlyAggregatedData$EvapCanopyIntStory0_Rel_pct[Ptr] = YearlyAggregatedData$EvapCanopyIntStory0_mmd[Ptr] / Avg_EvapCanopyIntStory0_mmd - 1
          
          YearlyAggregatedData$EvapCanopyIntStory1_Rel_mmd[Ptr] = YearlyAggregatedData$EvapCanopyIntStory1_mmd[Ptr] - Avg_EvapCanopyIntStory1_mmd
          YearlyAggregatedData$EvapCanopyIntStory1_Rel_pct[Ptr] = YearlyAggregatedData$EvapCanopyIntStory1_mmd[Ptr] / Avg_EvapCanopyIntStory1_mmd - 1
          
          YearlyAggregatedData$IntRainStory0_Rel_mm[Ptr] = YearlyAggregatedData$IntRainStory0_mm[Ptr] - Avg_IntRainStory0_mm
          YearlyAggregatedData$IntRainStory0_Rel_pct[Ptr] = YearlyAggregatedData$IntRainStory0_mm[Ptr] / Avg_IntRainStory0_mm - 1
          
          YearlyAggregatedData$IntRainStory1_Rel_mm[Ptr] = YearlyAggregatedData$IntRainStory1_mm[Ptr] - Avg_IntRainStory1_mm
          YearlyAggregatedData$IntRainStory1_Rel_pct[Ptr] = YearlyAggregatedData$IntRainStory1_mm[Ptr] / Avg_IntRainStory1_mm - 1
          
          YearlyAggregatedData$IntSnowStory0_Rel_mm[Ptr] = YearlyAggregatedData$IntSnowStory0_mm[Ptr] - Avg_IntSnowStory0_mm
          YearlyAggregatedData$IntSnowStory0_Rel_pct[Ptr] = YearlyAggregatedData$IntSnowStory0_mm[Ptr] / Avg_IntSnowStory0_mm - 1
          
          YearlyAggregatedData$IntSnowStory1_Rel_mm[Ptr] = YearlyAggregatedData$IntSnowStory1_mm[Ptr] - Avg_IntSnowStory1_mm
          YearlyAggregatedData$IntSnowStory1_Rel_pct[Ptr] = YearlyAggregatedData$IntSnowStory1_mm[Ptr] / Avg_IntSnowStory1_mm - 1
          
        }
      }
      print(paste(BasinName,Climate,DHSVMmodel))
    }
  }
}

# NaNs show up when dividing by zero interception
YearlyAggregatedData$IntRainStory0_Rel_pct[!is.finite(YearlyAggregatedData$IntRainStory0_Rel_pct)] = 0
YearlyAggregatedData$IntRainStory1_Rel_pct[!is.finite(YearlyAggregatedData$IntRainStory1_Rel_pct)] = 0
YearlyAggregatedData$IntSnowStory0_Rel_pct[!is.finite(YearlyAggregatedData$IntSnowStory0_Rel_pct)] = 0
YearlyAggregatedData$IntSnowStory1_Rel_pct[!is.finite(YearlyAggregatedData$IntSnowStory1_Rel_pct)] = 0

head(YearlyAggregatedData)

write.csv(YearlyAggregatedData, "AggregatedYearlyData_AllBasinsModelsClimatesScenarios.csv")

################################################################################
# Plotting yearly data

# Can start here
YearlyAggregatedData = read.csv("AggregatedYearlyData_AllBasinsModelsClimatesScenarios.csv")[,-1]

WaterYears = 2015:2099

# Change categorical variables to factors for predictable plotting
YearlyAggregatedData$Model = YearlyAggregatedData$DHSVMmodel
YearlyAggregatedData$Model = factor(YearlyAggregatedData$Model, levels=sort(unique(YearlyAggregatedData$Model)))
YearlyAggregatedData$Scenario = factor(YearlyAggregatedData$Scenario, levels=sort(unique(YearlyAggregatedData$Scenario)))
YearlyAggregatedData$Climate = factor(YearlyAggregatedData$Climate, levels=unique(YearlyAggregatedData$Climate))
YearlyAggregatedData$Basin = factor(YearlyAggregatedData$Basin, levels=unique(YearlyAggregatedData$Basin))

# Change climate name
YearlyAggregatedData$ClimateName = YearlyAggregatedData$Climate
YearlyAggregatedData$ClimateName = sub("cnrm", "CNRM-CM5 RCP 8.5", YearlyAggregatedData$ClimateName)
YearlyAggregatedData$ClimateName = sub("miroc", "MIROC5 RCP 8.5", YearlyAggregatedData$ClimateName)

########## AET / PET: canopy

YearlyAggregatedData$AEToverPETStory0 = YearlyAggregatedData$ActTranspStory0 / YearlyAggregatedData$PotTranspStory0

# Physical units (not scaled to mean!)
ggplot(data=YearlyAggregatedData[,], aes(x=WaterYear, y=AEToverPETStory0)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  labs(x="", y="AET / PET",
       title="Forest Thinning Effect on Canopy AET / PET") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black",NA)),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

########## Aridity index: understory

YearlyAggregatedData$AEToverPETStory1 = YearlyAggregatedData$ActTranspStory1 / YearlyAggregatedData$PotTranspStory1

# Physical units (not scaled to mean!)
ggplot(data=YearlyAggregatedData[,], aes(x=WaterYear, y=AEToverPETStory1)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  labs(x="", y="AET / PET",
       title="Forest Thinning Effect on Understory AET / PET") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black",NA)),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

########## Overstory ET

# Physical units
ggplot(data=YearlyAggregatedData[,], aes(x=WaterYear, y=ActTranspStory0_Rel_mmd)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(limits=c(-0.15,0.15),
                     breaks=c(-0.15,-0.1,-0.05,0,0.05,0.1,0.15),
                     labels=c("-0.15","-0.1","-0.05","Mean","+0.05","+0.1","+0.15"),
                     expand=c(0,0)) +
  labs(x="", y="Δ Yearly AET, mm / d",
       title="Forest Thinning Effect on Evapotranspiration:\nActual Canopy Transpiration") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black",NA)),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

# Percent change
ggplot(data=YearlyAggregatedData[,], aes(x=WaterYear, y=ActTranspStory0_Rel_pct)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(limits=c(-0.35,0.35),
                     breaks=c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3),
                     labels=c("-30%","-20%","-10%","Mean","+10%","+20%","+30%"),
                     expand=c(0,0)) +
  labs(x="", y="Δ Yearly AET",
       title="Forest Thinning Effect on Evapotranspiration:\nActual Canopy Transpiration") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black",NA)),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

########## Understory ET

# Physical units
ggplot(data=YearlyAggregatedData[,], aes(x=WaterYear, y=ActTranspStory1_Rel_mmd)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(limits=c(-0.15,0.15),
                     breaks=c(-0.15,-0.1,-0.05,0,0.05,0.1,0.15),
                     labels=c("-0.15","-0.1","-0.05","Mean","+0.05","+0.1","+0.15"),
                     expand=c(0,0)) +
  labs(x="", y="Δ Yearly AET, mm / d",
       title="Forest Thinning Effect on Evapotranspiration:\nActual Understory Transpiration") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black",NA)),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

# Percent change
ggplot(data=YearlyAggregatedData[,], aes(x=WaterYear, y=ActTranspStory1_Rel_pct)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(limits=c(-0.35,0.35),
                     breaks=c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3),
                     labels=c("-30%","-20%","-10%","Mean","+10%","+20%","+30%"),
                     expand=c(0,0)) +
  labs(x="", y="Δ Yearly AET",
       title="Forest Thinning Effect on Evapotranspiration:\nActual Understory Transpiration") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black",NA)),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

########## Total ET

# Physical units
ggplot(data=YearlyAggregatedData[,], aes(x=WaterYear, y=TotalET_Rel_mmd)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(limits=c(-0.2,0.2),
                     breaks=c(-0.2,-0.1,0,0.1,0.2),
                     labels=c("-0.2","-0.1","Mean","+0.1","+0.2"),
                     expand=c(0,0)) +
  labs(x="", y="Δ Yearly AET, mm / d",
       title="Forest Thinning Effect on Evapotranspiration:\nTotal Actual Evapotranspiration") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black",NA)),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

# Percent change
ggplot(data=YearlyAggregatedData[,], aes(x=WaterYear, y=TotalET_Rel_pct)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(limits=c(-0.1,0.1),
                     breaks=seq(-0.1,0.1,0.01),
                     labels=c("-10%","","-8%","","-6%","","-4%","","-2%","","Mean","","+2%","","+4%","","+6%","","+8%","","+10%"),
                     expand=c(0,0)) +
  labs(x="", y="Δ Yearly AET",
       title="Forest Thinning Effect on Evapotranspiration:\nTotal Actual Evapotranspiration") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black",NA)),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

########## Interception: canopy rain

# Physical units
ggplot(data=YearlyAggregatedData[,], aes(x=WaterYear, y=IntRainStory0_Rel_mm)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(limits=c(-0.05,0.05),
                     breaks=c(-0.05,-0.025,0,0.025,0.05),
                     labels=c("-0.05","-0.025","Mean","+0.025","+0.05")) +
  labs(x="", y="Δ Rain Storage, mm",
       title="Forest Thinning Effect on Mean Annual Interception Storage:\nCanopy Rain Interception") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black",NA)),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

# Percent change
ggplot(data=YearlyAggregatedData[,], aes(x=WaterYear, y=IntRainStory0_Rel_pct)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(limits=c(-0.3,0.3),
                     breaks=c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3),
                     labels=c("-30%","-20%","-10%","Mean","+10%","+20%","+30%")) +
  labs(x="", y="Δ Rain Storage",
       title="Forest Thinning Effect on Mean Annual Interception Storage:\nCanopy Rain Interception") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black",NA)),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

########## Interception: understory rain

# Physical units
ggplot(data=YearlyAggregatedData[,], aes(x=WaterYear, y=IntRainStory1_Rel_mm)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(limits=c(-0.05,0.05),
                     breaks=c(-0.05,-0.025,0,0.025,0.05),
                     labels=c("-0.05","-0.025","Mean","+0.025","+0.05")) +
  labs(x="", y="Δ Rain Storage, mm",
       title="Forest Thinning Effect on Mean Annual Interception Storage:\nUnderstory Rain Interception") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black",NA)),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

# Percent change
ggplot(data=YearlyAggregatedData[,], aes(x=WaterYear, y=IntRainStory1_Rel_pct)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(limits=c(-0.3,0.3),
                     breaks=c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3),
                     labels=c("-30%","-20%","-10%","Mean","+10%","+20%","+30%")) +
  labs(x="", y="Δ Rain Storage",
       title="Forest Thinning Effect on Mean Annual Interception Storage:\nUnderstory Rain Interception") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black",NA)),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

########## Interception: canopy snow

# Physical units
ggplot(data=YearlyAggregatedData[,], aes(x=WaterYear, y=IntSnowStory0_Rel_mm)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(limits=c(-0.05,0.05),
                     breaks=c(-0.05,-0.025,0,0.025,0.05),
                     labels=c("-0.05","-0.025","Mean","+0.025","+0.05")) +
  labs(x="", y="Δ Snow Storage, mm",
       title="Forest Thinning Effect on Mean Annual Interception Storage:\nCanopy Snow Interception") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black",NA)),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

# Percent change
ggplot(data=YearlyAggregatedData[,], aes(x=WaterYear, y=IntSnowStory0_Rel_pct)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(limits=c(-0.3,0.3),
                     breaks=c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3),
                     labels=c("-30%","-20%","-10%","Mean","+10%","+20%","+30%")) +
  labs(x="", y="Δ Snow Storage",
       title="Forest Thinning Effect on Mean Annual Interception Storage:\nCanopy Snow Interception") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black",NA)),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

########## Evap from interception: canopy

# Physical units
ggplot(data=YearlyAggregatedData[,], aes(x=WaterYear, y=EvapCanopyIntStory0_Rel_mmd)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(limits=c(-0.2,0.2),
                     breaks=c(-0.2,-0.1,0,0.1,0.2),
                     labels=c("-0.2","-0.1","Mean","+0.1","+0.2"),
                     expand=c(0,0)) +
  labs(x="", y="Δ Interception ET, mm / d",
       title="Forest Thinning Effect on Evaporation from Interception Storage:\nCanopy Interception Loss") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black",NA)),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

# Percent change
ggplot(data=YearlyAggregatedData[,], aes(x=WaterYear, y=EvapCanopyIntStory0_Rel_pct)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(limits=c(-0.3,0.3),
                     breaks=c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3),
                     labels=c("-30%","-20%","-10%","Mean","+10%","+20%","+30%"),
                     expand=c(0,0)) +
  labs(x="", y="Δ Interception ET",
       title="Forest Thinning Effect on Evaporation from Interception Storage:\nCanopy Interception Loss") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black",NA)),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

########## Evap from interception: understory

# Physical units
ggplot(data=YearlyAggregatedData[,], aes(x=WaterYear, y=EvapCanopyIntStory1_Rel_mmd)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(limits=c(-0.2,0.2),
                     breaks=c(-0.2,-0.1,0,0.1,0.2),
                     labels=c("-0.2","-0.1","Mean","+0.1","+0.2"),
                     expand=c(0,0)) +
  labs(x="", y="Δ Interception ET, mm / d",
       title="Forest Thinning Effect on Evaporation from Interception Storage:\nUnderstory Interception Loss") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black",NA)),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

# Percent change
ggplot(data=YearlyAggregatedData[,], aes(x=WaterYear, y=EvapCanopyIntStory1_Rel_pct)) + 
  geom_hline(yintercept=0) +
  geom_line(linewidth=1, aes(color=Scenario, linetype=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_x_continuous(limits=c(2014,2100),
                     breaks=seq(2020,2100,10),
                     expand=c(0,0)) +
  scale_y_continuous(limits=c(-0.3,0.3),
                     breaks=c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3),
                     labels=c("-30%","-20%","-10%","Mean","+10%","+20%","+30%"),
                     expand=c(0,0)) +
  labs(x="", y="Δ Interception ET",
       title="Forest Thinning Effect on Evaporation from Interception Storage:\nUnderstory Interception Loss") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color=c("black",NA)),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(linewidth=2),byrow=TRUE, order=1),
         linetype=guide_legend(override.aes=list(linewidth=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName), cols=vars(Basin))

################################################################################

# Budyko framework

YearlyAggregatedData$AridityIndexCanopy0 = YearlyAggregatedData$PotTranspStory0 / YearlyAggregatedData$Precip_m
YearlyAggregatedData$EvaporativeIndexCanopy0 = YearlyAggregatedData$ActTranspStory0 / YearlyAggregatedData$Precip_m

plot(YearlyAggregatedData$AridityIndexCanopy0, YearlyAggregatedData$EvaporativeIndexCanopy0)

# Aggregate years

if (exists("TotalAggregatedData")){
  remove(TotalAggregatedData)
}

for (DHSVMmodel in DHSVMmodels){
  for (Scenario in Scenarios){
    for (Climate in Climates){
      for (BasinName in BasinNames){
        
        Ptrs = which(YearlyAggregatedData$Basin==BasinName &
                       YearlyAggregatedData$Climate==Climate &
                       YearlyAggregatedData$Scenario==Scenario &
                       YearlyAggregatedData$DHSVMmodel==DHSVMmodel)
        
        ResultsMatrix = data.frame(matrix(nrow=1, ncol=(4+3)))
        names(ResultsMatrix) = c("DHSVMmodel","Scenario","Climate","Basin",
                                 "PotTranspStory0","ActTranspStory0","Precip_m")
        
        ResultsMatrix[1,"DHSVMmodel"] = DHSVMmodel
        ResultsMatrix[1,"Scenario"] = Scenario
        ResultsMatrix[1,"Climate"] = Climate
        ResultsMatrix[1,"Basin"] = BasinName
        
        ResultsMatrix[1,"PotTranspStory0"] = mean(YearlyAggregatedData$PotTranspStory0[Ptrs])
        ResultsMatrix[1,"ActTranspStory0"] = mean(YearlyAggregatedData$ActTranspStory0[Ptrs])
        ResultsMatrix[1,"Precip_m"] = mean(YearlyAggregatedData$Precip_m[Ptrs])
        
        # Append data from all basins / models / scenarios / climates
        if (!exists("TotalAggregatedData")){
          TotalAggregatedData = ResultsMatrix
        } else {
          TotalAggregatedData = rbind(TotalAggregatedData,ResultsMatrix)
        }
        
        print(paste("Done with",BasinName,Climate,Scenario,DHSVMmodel))
      }
    }
  }
}

TotalAggregatedData$AridityIndexCanopy0 = TotalAggregatedData$PotTranspStory0 / TotalAggregatedData$Precip_m
TotalAggregatedData$EvaporativeIndexCanopy0 = TotalAggregatedData$ActTranspStory0 / TotalAggregatedData$Precip_m

# Change categorical variables to factors for predictable plotting
TotalAggregatedData$Model = TotalAggregatedData$DHSVMmodel
TotalAggregatedData$Model = factor(TotalAggregatedData$Model, levels=sort(unique(TotalAggregatedData$Model)))
TotalAggregatedData$Scenario = factor(TotalAggregatedData$Scenario, levels=sort(unique(TotalAggregatedData$Scenario)))
TotalAggregatedData$Climate = factor(TotalAggregatedData$Climate, levels=unique(TotalAggregatedData$Climate))
TotalAggregatedData$Basin = factor(TotalAggregatedData$Basin, levels=unique(TotalAggregatedData$Basin))

# Change climate name
TotalAggregatedData$ClimateName = TotalAggregatedData$Climate
TotalAggregatedData$ClimateName = sub("cnrm", "CNRM-CM5 RCP 8.5", TotalAggregatedData$ClimateName)
TotalAggregatedData$ClimateName = sub("miroc", "MIROC5 RCP 8.5", TotalAggregatedData$ClimateName)

# Add watershed x scenario column
TotalAggregatedData$WatershedScenario = paste0(TotalAggregatedData$Basin,TotalAggregatedData$Scenario)

plot(TotalAggregatedData$AridityIndexCanopy0, TotalAggregatedData$EvaporativeIndexCanopy0)
plot(TotalAggregatedData$Basin, TotalAggregatedData$AridityIndexCanopy0)

ggplot(data=TotalAggregatedData[,], aes(x=AridityIndexCanopy0, y=EvaporativeIndexCanopy0)) + 
  geom_point(size=3, stroke=2, aes(color=Scenario, shape=Model)) +
  scale_color_manual(values=c("darkred","red","goldenrod1","cyan","blue","purple")) +
  scale_shape_manual(values=c(0, 1, 2)) +
  labs(x="Aridity Index, PET / P", y="Evaporative Index, AET / P",
       title="Forest Thinning Effect on\nBudyko Curve (Canopy Only!)") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14, vjust=0.5,color="black"),
        axis.text.y=element_text(size=14,color="black"),
        axis.title.x=element_text(size=28, margin=margin(0.5,0,0,0,"cm")),
        axis.title.y=element_text(size=28, margin=margin(0,0.5,0,0,"cm"))) +
  theme(axis.ticks.length=unit(0.3,"cm"),
        plot.margin=margin(0.5,0.5,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"),
        panel.grid=element_blank(),
        legend.title=element_text(size=28),
        legend.text=element_text(size=20)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        plot.title=element_text(color="black",size=28,hjust=0.5,face="bold"),
        legend.position="right",
        legend.title.align=0.5) +
  theme(strip.text=element_text(color="black",size=20),
        strip.background=element_rect(color="black",fill="gray90")) +
  theme(panel.spacing=unit(1,"cm")) +
  guides(color=guide_legend(override.aes=list(size=4),byrow=TRUE, order=1),
         shape=guide_legend(override.aes=list(size=4, stroke=2),byrow=TRUE)) +
  facet_grid(rows=vars(ClimateName))
































