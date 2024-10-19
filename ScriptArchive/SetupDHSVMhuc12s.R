library(whitebox)

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/StreamGauges/"
setwd(dir)

FlowDirRaster = paste0(dir,"../SRTM/TCSIdomain_90mDEMfilledFlowDirection.tif")
PourPoints = paste0(dir,"TCSI_HUC12savePoints/TCSI_HUC12savePoints.shp")
WatershedRaster = paste0(dir,"TCSI_HUC12ish_Watersheds_Temp.tif")

wbt_watershed(d8_pntr=FlowDirRaster,
              pour_pts=PourPoints,
              output=WatershedRaster,
              esri_pntr=TRUE)

# Run [Polygonize] --> [Join attributes by location] in QGIS
# To add correct HUC values to appropriate raster regions
# Then read it back in and add the right values everywhere
# Need to also convert "id" character attribute to "HUC" numeric

library(raster)
library(rgdal)

WatershedRast1 = raster("TCSI_HUC12ish_Watersheds_Temp.tif")
plot(WatershedRast1)

WatershedPoly = readOGR("TCSI_HUC12savePoints/TCSI_HUC12savePolygons.shp")
lines(WatershedPoly)

WatershedPoly$HUC = as.numeric(WatershedPoly$id)

WatershedRast2 = rasterize(WatershedPoly, WatershedRast1, "HUC")
plot(WatershedRast2)
unique(WatershedRast2[,])

# Replace NAs (massaging polygons into homogenous blocks) and enumerate
SortedHucs = sort(unique(WatershedPoly$HUC[is.finite(WatershedPoly$HUC)]))

i = 1
for (huc in SortedHucs){
  OriginalRastVal = WatershedRast1[which(WatershedRast2[,]==huc)[1]]
  WatershedRast2[which(WatershedRast1[,]==OriginalRastVal)] = i
  i = i + 1
}

plot(WatershedRast2)

sort(unique(WatershedRast2[,]))

writeRaster(WatershedRast2, "TCSI_HUC12ish_Watersheds.tif", overwrite=TRUE)


