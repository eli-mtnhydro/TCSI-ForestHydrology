###############################################################################
## Classification code credit Bea Gordon
###############################################################################

## Load in all your packages
library(soiltexture)

##==============================================================================
# Setup--set your WD, get the data from the website, and see what it looks like
##==============================================================================
dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/SSURGO/"
setwd(dir)

SoilAreaName = "TCSImerged_"

sand = read.csv(paste0(SoilAreaName,"Sand.txt"))
silt = read.csv(paste0(SoilAreaName,"Silt.txt"))
clay = read.csv(paste0(SoilAreaName,"Clay.txt"))

data = data.frame(sand$OBJECTID,sand$Sand,silt$Silt,clay$Clay)
names(data) = c("OBJECTID","SAND", "SILT", "CLAY")

SandMean = mean(data$SAND[!is.na(data$SAND)])
SiltMean = mean(data$SILT[!is.na(data$SAND)])
ClayMean = mean(data$CLAY[!is.na(data$SAND)])

data$SAND[is.na(data$SAND)] = SandMean
data$SILT[is.na(data$SILT)] = SiltMean
data$CLAY[is.na(data$CLAY)] = ClayMean

min(data$SAND+data$SILT+data$CLAY)

head(data)

class = soiltexture::TT.points.in.classes(
  tri.data  = data,
  class.sys = "USDA-NCSS.TT",
  PiC.type = 't',
  tri.sum.tst = FALSE)

data$CLASS = class

head(data)

data$CLASS[data$CLASS == "C"] = 1 # Clay
data$CLASS[data$CLASS == "SIC"] = 2 # Silty Clay
data$CLASS[data$CLASS == "SC"] = 3 # Sandy Clay
data$CLASS[data$CLASS == "CL"] = 4 # Clay Loam
data$CLASS[data$CLASS == "SICL"] = 5 # Silty Clay Loam
data$CLASS[data$CLASS == "SCL"] = 6 # Sandy Clay Loam
data$CLASS[data$CLASS == "L"] = 7 # Loam
data$CLASS[data$CLASS == "SIL"] = 8 # Silty Loam
data$CLASS[data$CLASS == "SL"] = 9 # Sandy Loam
data$CLASS[data$CLASS == "SI"] = 10 # Silt
data$CLASS[data$CLASS == "LS"] = 11 # Loamy Sand
data$CLASS[data$CLASS == "S"] = 12 # Sand

data$CLASS = as.numeric(data$CLASS)

Mode = function(x) {
  ux = unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

ClassMode = Mode(data$CLASS)
ClassMode
data$CLASS[is.na(data$CLASS)] = ClassMode
data$CLASS[(data$SAND+data$SILT+data$CLAY) == 0] = Mode(data$CLASS)

head(data)

min(data$CLASS)
max(data$CLASS)
sort(unique(data$CLASS))

write.csv(data, "SoilClassification.csv")
