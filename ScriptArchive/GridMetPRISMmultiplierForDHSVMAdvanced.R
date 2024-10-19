library(ncdf4)
library(raster)
library(rgdal)

# Read data from gridMet downloads
dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/"
setwd(dir)

# Read shapefile of model domain
ModelDomainSPDF = readOGR(dsn="TCSIshape")

FirstYr = 1991 # start of PRISM 30-year normals
LastYr = 2020 # end of PRISM 30-year normals (and end of GridMet record)
nYears = LastYr-FirstYr+1

# Create matching 30-year normals for GridMet
MonthlyGridMetP = array(0,dim=c(12,1386,585)) # x and y dimensions from dim(pr.array)
for (yr in FirstYr:LastYr){
  nc_data_pr = nc_open(paste0("pr_",yr,".nc"))
  
  # Convert weather variables into array data
  pr.array = ncvar_get(nc_data_pr,"precipitation_amount")
  
  lons = ncvar_get(nc_data_pr,"lon")
  lats = ncvar_get(nc_data_pr,"lat")
  days = ncvar_get(nc_data_pr,"day")
  
  dates = as.POSIXct(days*24*60*60,origin="1900-01-01",tz="UTC")
  
  for (mon in 1:12){
    DailyPforMonth = pr.array[,,as.integer(format(dates,"%m"))==mon]
    MonthlyGridMetP[mon,,] = MonthlyGridMetP[mon,,] + apply(DailyPforMonth,c(1,2),sum)/nYears
    print(mon)
  }
  
  nc_close(nc_data_pr)
  print(yr)
}

# Save completed monthly normals for whole US
dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/GridMetMonthlyNormals/"
setwd(dir)
for (mon in 1:12){
  PmonthlyRast = raster(t(MonthlyGridMetP[mon,,]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  writeRaster(PmonthlyRast,paste0("GridMet_1991-2020_MeanMonthlyClimatology_Month",mon,".tif"),overwrite=TRUE)
  plot(PmonthlyRast)
}

# Compute precip multiplier maps

library(raster)
library(sp)
library(dismo)
library(deldir)
library(gstat)
library(rgdal)
library(rgeos)

for (mon in 1:12){
  print("**************** MONTH ****************")
  print(mon)
  
  dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/GridMetMonthlyNormals/"
  setwd(dir)
  Pgridmet = raster(paste0("GridMet_1991-2020_MeanMonthlyClimatology_Month",mon,".tif"))
  dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/"
  setwd(dir)
  Pprism = raster(sprintf("PRISM_ppt_30yr_normal_800mM3_%02d_asc.asc",mon))
  dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/PRISM-GridMet_Multiplier/"
  setwd(dir)
  DomainTemplate = raster("TCSIdomainTemplate.tif")
  
  # Causes some weird error the first time it is run, which internet people say is fine
  # Reproject GridMet to UTM Zone 10 and clip to model domain
  PgridmetClippedRaw = crop(projectRaster(Pgridmet,crs=crs(DomainTemplate),method="ngb"),extent(DomainTemplate),na.rm=TRUE)
  
  # Extra setup steps
  if (mon==1){
    # Create a standardized grid in UTM
    GridMetPoints = as.data.frame(PgridmetClippedRaw, xy=TRUE)
    XGridRaw = unique(GridMetPoints$x)
    YGridRaw = unique(GridMetPoints$y)
    MaxX = round(max(XGridRaw),2)
    MaxY = round(max(YGridRaw),2)
    XRes = round((max(XGridRaw)-min(XGridRaw))/(length(XGridRaw)-1),2)
    YRes = round((max(YGridRaw)-min(YGridRaw))/(length(YGridRaw)-1),2)
    XGrid = MaxX - XRes*(1:length(XGridRaw)-1)
    YGrid = MaxY - YRes*(1:length(YGridRaw)-1)
    
    # Snap the GridMet cells to the center of 90 m model domain cells to remove rounding ambiguity in DHSVM model grid
    ModelDomainPoints = as.data.frame(DomainTemplate, xy=TRUE)
    XGridDomain = unique(ModelDomainPoints$x)
    YGridDomain = unique(ModelDomainPoints$y)
    GridMetPointsAdj = GridMetPoints
    for (i in 1:length(GridMetPointsAdj$x)){
      GridMetPointsAdj$x[i] = XGridDomain[(abs(GridMetPoints$x[i]-XGridDomain))==min((abs(GridMetPoints$x[i]-XGridDomain)))]
      GridMetPointsAdj$y[i] = YGridDomain[(abs(GridMetPoints$y[i]-YGridDomain))==min((abs(GridMetPoints$y[i]-YGridDomain)))]
    }
    
    # Save the cell-center points for use in generating the station file for DHSVM
    write.csv(GridMetPointsAdj[,1:2],"Reprojected_GridMet_CellCenterPoints.csv")
    
    # Some absolute bullshit to make everything play nicely with the way DHSVM associates domain grid cells with met stations
    GridMetPointsAdjExtra = GridMetPointsAdj
    #GridMetPointsAdjExtra$x = GridMetPointsAdj$x - 90/2 # Don't ask me why I don't fucking know
    GridMetPointsAdjExtra$y = GridMetPointsAdj$y - 90/2 # Don't ask me why I don't fucking know
    GridMetStationDomains = SpatialPoints(GridMetPointsAdjExtra[,1:2], proj4string=crs(DomainTemplate))
    GridMetStationDomains = SpatialPointsDataFrame(GridMetStationDomains,GridMetPointsAdjExtra)
    StationVoronoi = voronoi(GridMetStationDomains)
    #plot(StationVoronoi)
    #vr = rasterize(StationVoronoi, DomainTemplate, "GridMet_1991.2020_MeanMonthlyClimatology_Month1")
    #plot(vr)
    #writeRaster(vr, "PgridmetInUTMZone10_ExampleMonth1.tif", overwrite=TRUE)
    #writeOGR(obj=StationVoronoi, dsn="stations", layer="GridMet_1991.2020_MeanMonthlyClimatology_Month1", driver="ESRI Shapefile")
    
    AggrFactor = round(mean(res(Pgridmet))/mean(res(Pprism)),0) # 4.994 --> 5
  }
  
  # Snap the GridMet cells onto the standardized UTM grid
  GridMetPoints = as.data.frame(PgridmetClippedRaw, xy=TRUE)
  for (i in 1:length(GridMetPoints$x)){
    GridMetPoints$x[i] = XGrid[(abs(GridMetPoints$x[i]-XGrid))==min((abs(GridMetPoints$x[i]-XGrid)))]
    GridMetPoints$y[i] = YGrid[(abs(GridMetPoints$y[i]-YGrid))==min((abs(GridMetPoints$y[i]-YGrid)))]
  }
  PgridmetClipped = rasterFromXYZ(GridMetPoints,crs=crs(PgridmetClippedRaw))
  
  # Reproject PRISM to match GridMet coordinate system and extent
  PprismReproj = projectRaster(from=Pprism, to=disaggregate(PgridmetClipped,AggrFactor), method="ngb")

  # Calculate average PRISM values at the GridMet resolution
  PprismAggregated = aggregate(PprismReproj, AggrFactor, fun=mean)
  PprismAvg = disaggregate(PprismAggregated, AggrFactor)
  
  # Calculate the multiplier factor
  MultiplierMap = PprismReproj/PprismAvg
  #hist(MultiplierMap)
  #mean(MultiplierMap[,])
  # plot(MultiplierMap, main="Multiplier Map", cex.main=3)
  # plot(PprismReproj, main="PRISM Precip", cex.main=3)
  # plot(PgridmetClipped, main="gridMet Precip", cex.main=3)
  # plot(MultiplierMap*disaggregate(PgridmetClipped,AggrFactor), main="gridMet * Multiplier", cex.main=3)
  
  if (mon==1){
    # Some more absolute bullshit to make everything play nicely with the way DHSVM associates domain grid cells with met stations
    MultiplierPointsAdj = as.data.frame(MultiplierMap, xy=TRUE)
    for (i in 1:length(MultiplierPointsAdj$x)){
      # Find nearest GridMet station cell
      DistancesToStations = (MultiplierPointsAdj$x[i]-GridMetPoints$x)^2 + (MultiplierPointsAdj$y[i]-GridMetPoints$y)^2 # Leave out sqrt for efficiency
      j = which(DistancesToStations==min(DistancesToStations))
      # Adjust the PRISM cell centers by the same amount that their respective GridMet cells were adjusted by
      MultiplierPointsAdj$x[i] = MultiplierPointsAdj$x[i] + (GridMetPointsAdjExtra$x[j] - GridMetPoints$x[j])
      MultiplierPointsAdj$y[i] = MultiplierPointsAdj$y[i] + (GridMetPointsAdjExtra$y[j] - GridMetPoints$y[j])
    }
  }
  
  # Still more absolute bullshit to make everything play nicely with the way DHSVM associates domain grid cells with met stations
  MultiplierPoints = as.data.frame(MultiplierMap, xy=TRUE)
  MultiplierPoints$x = MultiplierPointsAdj$x
  MultiplierPoints$y = MultiplierPointsAdj$y
  MultiplierDomains = SpatialPoints(MultiplierPoints[,1:2], proj4string=crs(DomainTemplate))
  MultiplierDomains = SpatialPointsDataFrame(MultiplierDomains,MultiplierPoints)
  MultiplierVoronoi = voronoi(MultiplierDomains)
  MultiplierMapModelDomain = rasterize(MultiplierVoronoi, DomainTemplate, "layer")
  #plot(MultiplierMapModelDomain)
  #writeRaster(MultiplierMapModelDomain, "testmultiplier.tif", overwrite=TRUE)
  #writeOGR(obj=MultiplierVoronoi, dsn="multiplier", layer="layer", driver="ESRI Shapefile")
  
  # Fill NAs around edge from reprojection
  #sum(as.integer(is.na(MultiplierMapModelDomain[,])))/length(MultiplierMapModelDomain[,])
  MultiplierMapModelDomain[is.na(MultiplierMapModelDomain)] = 1
  
  print(mean(MultiplierMapModelDomain[,]))
  
  # Save the monthly multiplier map
  writeRaster(MultiplierMapModelDomain,paste0("MultiplierMap_ForGridMet_FromPRISM_Month",mon,".tif"),overwrite=TRUE)
  
  # Make and save the re-scaled GridMet monthly normals
  RedistrP = MultiplierMap*disaggregate(PgridmetClipped,AggrFactor)
  RedistrPPoints = as.data.frame(RedistrP, xy=TRUE)
  RedistrPPoints$x = MultiplierPointsAdj$x
  RedistrPPoints$y = MultiplierPointsAdj$y
  RedistrPDomains = SpatialPoints(RedistrPPoints[,1:2], proj4string=crs(DomainTemplate))
  RedistrPDomains = SpatialPointsDataFrame(RedistrPDomains,RedistrPPoints)
  RedistrPVoronoi = voronoi(RedistrPDomains)
  RedistrPMapModelDomain = rasterize(RedistrPVoronoi, DomainTemplate, "layer")
  RedistrPMapModelDomain[is.na(RedistrPMapModelDomain)] = 0
  dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/PRISM-GridMet_Multiplier/RescaledGridMet/"
  setwd(dir)
  writeRaster(RedistrPMapModelDomain,paste0("GridMet_PRISMredistributed_Month",mon,".tif"),overwrite=TRUE)
  
  # Compute yearly sum of redistributed precip
  if (mon==1){
    YearlyRedistrP = RedistrPMapModelDomain
  } else {
    YearlyRedistrP = YearlyRedistrP + RedistrPMapModelDomain
  }
  if (mon==12){
    writeRaster(YearlyRedistrP,"GridMet_PRISMredistributed_YearlyTotal.tif",overwrite=TRUE)
    
    print("Warnings from loss of comment in CRS are ok.") # Only changing extent and resolution, not CRS
  }
}

# Convert to ascii and concatenate
library(raster)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/PRISM-GridMet_Multiplier/"
setwd(dir)

for (mon in 1:12){
  # Read pre-existing multiplier map geotiffs
  MonthlyMultiplierMap = raster(paste0("MultiplierMap_ForGridMet_FromPRISM_Month",mon,".tif"))
  
  # Write raster to dummy file so that it can be re-read skipping the header (I'm sure there's a smarter way to do this but I don't care)
  writeRaster(MonthlyMultiplierMap, "TempASCIIraster.asc", format="ascii", overwrite=TRUE)
  MonthlyMultiplierASCII = read.table("TempASCIIraster.asc", skip = 6)
  
  # Create text file for myconvert.c to convert into binary: ESRI-style ascii except no header
  OutFile = file(paste0("PRISMmultiplier.",formatC(mon,width=2,flag="0"),".txt"),"w")
  write.table(MonthlyMultiplierASCII, file=OutFile, col.names=FALSE, row.names=FALSE)
  close(OutFile)

  print(mon)
}


# Old way of attempting to fix the misalignment
# # Make the GridMet coordinates match the "station" coordinates used by DHSVM
# PgridmetPoints = rasterToPoints(PgridmetClippedRaw)
# PgridmetPoints[,1] = round(PgridmetPoints[,1],5)
# PgridmetPoints[,2] = round(PgridmetPoints[,2],5)
# xRes = unique(PgridmetPoints[,1])[2]-unique(PgridmetPoints[,1])[1]
# yRes = unique(PgridmetPoints[,2])[1]-unique(PgridmetPoints[,2])[2]
# xExtent = c(min(PgridmetPoints[,1])-xRes/2,max(PgridmetPoints[,1])+xRes/2)
# yExtent = c(min(PgridmetPoints[,2])-xRes/2,max(PgridmetPoints[,2])+xRes/2)
# RoundedTemplate = raster(ncol=length(unique(PgridmetPoints[,1])), nrow=length(unique(PgridmetPoints[,2])), xmn=xExtent[1], xmx=xExtent[2], ymn=yExtent[1], ymx=yExtent[2], res=c(xRes,yRes), crs=crs(PgridmetClippedRaw))
# PgridmetClipped = rasterize(PgridmetPoints[,1:2], RoundedTemplate, PgridmetPoints[,3])
# 
# PgridmetClipped = rasterFromXYZ(PgridmetPoints,crs=crs(PgridmetClippedRaw))
# 
# print(format(unique(rasterToPoints(PgridmetClippedRaw)[,1]),nsmall=10))
# print(format(unique(rasterToPoints(PgridmetClipped)[,1]),nsmall=10))

# Roundabout way of (maybe?) doing it that I wrote originally

# MultiplierMap = 0*PgridmetClipped
# for (i in 1:dim(PgridmetClipped)[1]){
#   for (j in 1:dim(PgridmetClipped)[2]){
#     if (MultiplierMap[i,j]==0){
#       # [i,j] = [y,x] = [row,col] coordinates of cell-of-interest
#       POIy = yFromRow(PgridmetClipped,i)
#       POIx = xFromCol(PgridmetClipped,j)
#       
#       # All cells that have the same GridMet value
#       MatchingIndicesMap = (PgridmetClipped == PgridmetClipped[i,j])
#       
#       # All cells that are within 1 GridMet cell size (4 km) in distance from cell-of-interest
#       ClosePointsY = (abs(yFromRow(PgridmetClipped)-POIy) < 4500)
#       ClosePointsX = (abs(xFromCol(PgridmetClipped)-POIx) < 4500)
#       ClosePointsYindices = (1:length(ClosePointsY))*as.integer(ClosePointsY)
#       ClosePointsXindices = (1:length(ClosePointsX))*as.integer(ClosePointsX)
#       CloseIndicesMap = PgridmetClipped*0
#       CloseIndicesMap[ClosePointsYindices[ClosePointsYindices>0],ClosePointsXindices[ClosePointsXindices>0]] = 1
#       
#       # All adjacent cells with the same GridMet value (approximately the original GridMet cells)
#       IndicesMaskMap = MatchingIndicesMap*CloseIndicesMap
#       
#       # Get average value of PRISM within GridMet cell area
#       PRISMzone = PprismClipped*IndicesMaskMap
#       PRISMavgP = mean(PRISMzone[PRISMzone>0])
#       
#       MultiplierValue = PprismClipped[i,j]/PRISMavgP
#       
#       # All cells that have the same PRISM value
#       MatchingIndicesMap = (PprismClipped == PprismClipped[i,j])
#       
#       # All cells that are within 1 PRISM cell size (800 m) in distance from cell-of-interest
#       ClosePointsY = (abs(yFromRow(PprismClipped)-POIy) < 1000)
#       ClosePointsX = (abs(xFromCol(PprismClipped)-POIx) < 1000)
#       ClosePointsYindices = (1:length(ClosePointsY))*as.integer(ClosePointsY)
#       ClosePointsXindices = (1:length(ClosePointsX))*as.integer(ClosePointsX)
#       CloseIndicesMap = PprismClipped*0
#       CloseIndicesMap[ClosePointsYindices[ClosePointsYindices>0],ClosePointsXindices[ClosePointsXindices>0]] = 1
#       
#       # All adjacent cells with the same PRISM value (approximately the original PRISM cells)
#       IndicesMaskMap = MatchingIndicesMap*CloseIndicesMap
#       
#       MultiplierMap = MultiplierMap + IndicesMaskMap*MultiplierValue
#     }
#   }
#   print(i/1436)
# }




