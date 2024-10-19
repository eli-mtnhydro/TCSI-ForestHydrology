################################################################################
# Process MACA data into DHSVM format
# Eli Boardman, 2023
# Downloads and reads MACA netCDF data
# Sets up MetSim inputs and converts MetSim outputs to DHSVM ascii station files
################################################################################

library(ncdf4)
library(raster)
library(rgdal)

# GCM = "MIROC5"
GCM = "CNRM-CM5"
Scenario = "RCP8.5"

FileBase = "macav2metdata_"
FileMid = "_r1i1p1_rcp85_" # Would need to edit if changing scenario
FileExt = "_CONUS_daily.nc"
# Syntax used elsewhere: paste0(datadir,FileBase,"pr_",GCM,FileMid,YrStart,"_",YrStart+4,FileExt)

################################################################################
# Download MACA Data

DownloadOption = FALSE

if (DownloadOption){
  dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/MACA/"
  setwd(dir)
  
  # Read text file with URLs to download, created by https://climate.northwestknowledge.net/MACA/data_portal.php
  URLlist = readLines(paste0("macav2metdata_urls_",GCM,"_",Scenario,".txt"))
  URLlist = URLlist[which(URLlist!="")] # Delete blank lines at end
  
  for (i in 1:length(URLlist)){
    # Parse filename from URL
    fname = paste0(sub(".*/", "", sub("\\.nc.*", "", URLlist[i])),".nc")
    # Download
    download.file(URLlist[i],paste0(dir,GCM,"/",fname),mode="wb")
  }
  
}
# If it times out, see: https://stackoverflow.com/questions/61393298/how-to-resolve-download-file-error-in-operation-timed-out

################################################################################
# Create MetSim files

# Read data from MACA downloads
datadir = paste0(GCM,"/")

dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/MACA/"
setwd(dir)

FirstYr = 2006
LastYr = 2099

YrSegments = seq(FirstYr,LastYr,5)

BasinNames = c("Truckee","Yuba","Bear","American")

for (BasinNumber in 1:length(BasinNames)){
  BasinName = BasinNames[BasinNumber]
  
  # Read shapefile of model domain
  ModelDomainSPDF = readOGR(dsn=paste0(BasinName,"Shape"))
  
  #########################################
  ########## Create Forcing File ##########
  #########################################
  
  ForcingFilename = paste0("MetSim/",GCM,"-",Scenario,"_",BasinName,FirstYr,"-",LastYr,".nc")
  
  for (YrStart in YrSegments){
    nc_data_pr = nc_open(paste0(datadir,FileBase,"pr_",GCM,FileMid,YrStart,"_",min(YrStart+4,LastYr),FileExt))
    nc_data_tmmn = nc_open(paste0(datadir,FileBase,"tasmin_",GCM,FileMid,YrStart,"_",min(YrStart+4,LastYr),FileExt))
    nc_data_tmmx = nc_open(paste0(datadir,FileBase,"tasmax_",GCM,FileMid,YrStart,"_",min(YrStart+4,LastYr),FileExt))
    nc_data_uas = nc_open(paste0(datadir,FileBase,"uas_",GCM,FileMid,YrStart,"_",min(YrStart+4,LastYr),FileExt)) # Eastward wind
    nc_data_vas = nc_open(paste0(datadir,FileBase,"vas_",GCM,FileMid,YrStart,"_",min(YrStart+4,LastYr),FileExt)) # Northward wind
    
    # Convert weather variables into array data
    pr.array = ncvar_get(nc_data_pr,"precipitation")
    tmmn.array = ncvar_get(nc_data_tmmn,"air_temperature")
    tmmx.array = ncvar_get(nc_data_tmmx,"air_temperature")
    # Convert wind from west/east into total magnitude
    vs.array = sqrt(ncvar_get(nc_data_uas,"eastward_wind")^2 + ncvar_get(nc_data_vas,"northward_wind")^2)
    
    # NOTE that MACA is stored time, lat, lon, while GridMet is stored lon, lat, day
    # So we have to rotate 90 degrees counterclockwise so that it matches GridMet format
    # CAUTION double check maps and make sure they make sense, as always !!!
    dim(pr.array)
    pr.array = aperm(apply(pr.array, c(1,3), rev), c(2,1,3))
    tmmn.array = aperm(apply(tmmn.array, c(1,3), rev), c(2,1,3))
    tmmx.array = aperm(apply(tmmx.array, c(1,3), rev), c(2,1,3))
    vs.array = aperm(apply(vs.array, c(1,3), rev), c(2,1,3))
    dim(pr.array)
    
    # Set up dimensions and create the new NetCDF if necessary
    if (YrStart == FirstYr){
      # Generate vectors of all lat/lon and days in subset domain
      lons = ncvar_get(nc_data_pr,"lon")
      lons[which(lons>180)] = lons[which(lons>180)] - 360 # Convert from 0-->360 to -180-->180
      lats = ncvar_get(nc_data_pr,"lat")
      days = ncvar_get(nc_data_pr,"time")
      
      # Apparently the MACA coordinate system is slightly different from gridMet
      # So we have to snap MACA grid cells to the nearest gridMet cell to keep everything consistent
      GridMetTemplate = nc_open("C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/pr_1979.nc")
      GridMetLons = ncvar_get(GridMetTemplate,"lon")
      GridMetLats = ncvar_get(GridMetTemplate,"lat")
      for (i in 1:length(lats)){
        AbsDiff = abs(GridMetLats - lats[i])
        lats[i] = GridMetLats[which(AbsDiff == min(AbsDiff))]
      }
      for (i in 1:length(lons)){
        AbsDiff = abs(GridMetLons - lons[i])
        lons[i] = GridMetLons[which(AbsDiff == min(AbsDiff))]
      }
      nc_close(GridMetTemplate)
      
      # Visualize the precip map to make sure everything lines up where it should be (!)
      # MeanPrec = apply(pr.array,c(1,2),mean)
      # pr.rast = raster(t(MeanPrec), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats),
      #                  crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
      # plot(pr.rast)
      # lines(ModelDomainSPDF)
      # writeRaster(pr.rast*365.25,paste0(GCM,"-",Scenario,"-","SamplePrecip.tif"), overwrite=TRUE)
      
      # Generate sample single-day array to determine new lat/lon dimensions
      pr.slice = pr.array[,,1]
      pr.rast = raster(t(pr.slice), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats),
                       crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
      #plot(pr.rast)
      pr.clipped = crop(pr.rast,extent(ModelDomainSPDF))
      #plot(pr.clipped)
      
      # Define dimensions of new NetCDF
      FullDayRange = days[1]:(days[1]+as.numeric(difftime(as.POSIXct(paste0(LastYr,"-12-31")),as.POSIXct(paste0(FirstYr,"-1-1")))))
      lon = ncdim_def("lon", "degrees_east", unique(rasterToPoints(pr.clipped)[,1]), longname="longitude")
      lat = ncdim_def("lat", "degrees_north", unique(rasterToPoints(pr.clipped)[,2]), longname="latitude")
      time = ncdim_def("time", "days since 1900-01-01 00:00:00", FullDayRange, calendar="standard", unlim=TRUE)
      
      # Make weather variables with those dimensions
      prec = ncvar_def("prec", "mm", list(lat,lon,time), NA)
      t_max = ncvar_def("t_max", "C", list(lat,lon,time), NA)
      t_min = ncvar_def("t_min", "C", list(lat,lon,time), NA)
      wind = ncvar_def("wind", "m/s", list(lat,lon,time), NA)
      
      # Create new NetCDF file
      ncnew = nc_create(ForcingFilename, list(prec,t_max,t_min,wind))
      nc_close(ncnew)
      
      # Keep track of how many days of data have been written to the file
      tStart = 1
    }
    
    ncnew = nc_open(ForcingFilename, write=TRUE)
    
    # Load corresponding weather data into the file
    tLength = length(ncvar_get(nc_data_pr,"time"))
    TimeIndices = tStart:(tLength+tStart-1)
    for (i in 1:tLength){
      # Rasterize and clip weather data for each day
      pr.rast = raster(t(pr.array[,,i]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats),
                       crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
      prdata = c(as.matrix(crop(pr.rast,extent(ModelDomainSPDF))))
      tmmn.rast = raster(t(tmmn.array[,,i]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats),
                         crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
      tmmndata = c(as.matrix(crop(tmmn.rast,extent(ModelDomainSPDF)))) - 273.15
      tmmx.rast = raster(t(tmmx.array[,,i]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats),
                         crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
      tmmxdata = c(as.matrix(crop(tmmx.rast,extent(ModelDomainSPDF)))) - 273.15
      problemTemps = which(tmmxdata<tmmndata)
      tmmxdata[problemTemps] = tmmndata[problemTemps] + 1 # This should be fixed in gridMet
      vs.rast = raster(t(vs.array[,,i]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats),
                       crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
      vsdata = c(as.matrix(crop(vs.rast,extent(ModelDomainSPDF))))
      
      # Load each day's subset values into the new NetCDF
      ncvar_put(ncnew, prec, prdata, start=c(1,1,TimeIndices[i]), count=c(-1,-1,1))
      ncvar_put(ncnew, t_max, tmmxdata, start=c(1,1,TimeIndices[i]), count=c(-1,-1,1))
      ncvar_put(ncnew, t_min, tmmndata, start=c(1,1,TimeIndices[i]), count=c(-1,-1,1))
      ncvar_put(ncnew, wind, vsdata, start=c(1,1,TimeIndices[i]), count=c(-1,-1,1))
      
      print(paste0(round(100*i/tLength,2),"% done with segment ",YrStart," to ",min(YrStart+4,LastYr)))
    }
    tStart = tLength + tStart
    
    # View sample results
    ncvar_get(ncnew,"t_min")
    plot(ncvar_get(ncnew,"t_min")[1,1,],type='l')
    
    nc_close(ncnew)
    nc_close(nc_data_pr)
    nc_close(nc_data_tmmn)
    nc_close(nc_data_tmmx)
    nc_close(nc_data_uas)
    nc_close(nc_data_vas)
  }
  
  ########################################
  ########## Create Domain File ##########
  ########################################
  
  # Read elevation metadata from gridMet download
  elevdir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/"
  nc_data_elevation = nc_open(paste0(elevdir,"metdata_elevationdata.nc"))
  
  # Convert elevation variable into array data
  elevation.array = ncvar_get(nc_data_elevation,"elevation")
  
  # Generate vectors of all lat/lon in full domain
  # Seems to have different coordinates than weather data even though same x/y size
  GridMetTemplate = nc_open("C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/pr_1979.nc")
  lons = ncvar_get(GridMetTemplate,"lon")
  lats = ncvar_get(GridMetTemplate,"lat")
  nc_close(GridMetTemplate)
  
  # Generate array of elevation values
  elevation.rast = raster(t(elevation.array), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats),
                          crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  plot(elevation.rast)
  elevation.clipped = crop(elevation.rast,extent(ModelDomainSPDF))
  plot(elevation.clipped)
  
  # Define dimensions of new NetCDF
  lon = ncdim_def( "lon", "degrees_east", unique(rasterToPoints(elevation.clipped)[,1]), longname="longitude")
  lat = ncdim_def( "lat", "degrees_north", unique(rasterToPoints(elevation.clipped)[,2]), longname="latitude")
  
  # Make domain variables with those dimensions
  elev = ncvar_def("elev", "m", list(lon,lat), NA)
  mask = ncvar_def("mask", "", list(lon,lat), NA)
  
  # Create new NetCDF file
  ncnew = nc_create(paste0("MetSim/",BasinName,"Domain.nc"), list(elev,mask))
  
  # Load corresponding domain data into file
  ncvar_put(ncnew, elev, c(t(as.matrix(elevation.clipped))), start=c(1,1), count=c(-1,-1))
  ncvar_put(ncnew, mask, c(t(as.matrix(elevation.clipped)))*0+1, start=c(1,1), count=c(-1,-1))
  
  # View sample results
  ncvar_get(ncnew,"elev")
  ncvar_get(ncnew,"mask")
  
  nc_close(ncnew)
  nc_close(nc_data_elevation)
  
  #######################################
  ########## Create State File ##########
  #######################################
  
  YrStart = FirstYr
  nc_data_pr = nc_open(paste0(datadir,FileBase,"pr_",GCM,FileMid,YrStart,"_",min(YrStart+4,LastYr),FileExt))
  nc_data_tmmn = nc_open(paste0(datadir,FileBase,"tasmin_",GCM,FileMid,YrStart,"_",min(YrStart+4,LastYr),FileExt))
  nc_data_tmmx = nc_open(paste0(datadir,FileBase,"tasmax_",GCM,FileMid,YrStart,"_",min(YrStart+4,LastYr),FileExt))
  
  # Convert weather variables into array data
  pr.array = ncvar_get(nc_data_pr,"precipitation")
  tmmn.array = ncvar_get(nc_data_tmmn,"air_temperature")
  tmmx.array = ncvar_get(nc_data_tmmx,"air_temperature")
  # NOTE that MACA is stored time, lat, lon, while GridMet is stored lon, lat, day
  # So we have to rotate 90 degrees counterclockwise so that it matches GridMet format
  # CAUTION double check maps and make sure they make sense, as always !!!
  pr.array = aperm(apply(pr.array, c(1,3), rev), c(2,1,3))
  tmmn.array = aperm(apply(tmmn.array, c(1,3), rev), c(2,1,3))
  tmmx.array = aperm(apply(tmmx.array, c(1,3), rev), c(2,1,3))
  
  # Generate vectors of all lat/lon and days in subset domain and snap to GridMet coordinates
  lons = ncvar_get(nc_data_pr,"lon")
  lons[which(lons>180)] = lons[which(lons>180)] - 360 # Convert from 0-->360 to -180-->180
  lats = ncvar_get(nc_data_pr,"lat")
  for (i in 1:length(lats)){
    AbsDiff = abs(GridMetLats - lats[i])
    lats[i] = GridMetLats[which(AbsDiff == min(AbsDiff))]
  }
  for (i in 1:length(lons)){
    AbsDiff = abs(GridMetLons - lons[i])
    lons[i] = GridMetLons[which(AbsDiff == min(AbsDiff))]
  }
  days = ncvar_get(nc_data_pr,"time")
  
  # Define dimensions of new NetCDF
  lon = ncdim_def("lon", "degrees_east", unique(rasterToPoints(pr.clipped)[,1]), longname="longitude")
  lat = ncdim_def("lat", "degrees_north", unique(rasterToPoints(pr.clipped)[,2]), longname="latitude")
  time = ncdim_def("time", paste0("days since ",FirstYr-1,"-10-03 00:00:00"), 0:89, calendar="proleptic_gregorian")
  
  # Make weather variables with those dimensions
  prec = ncvar_def("prec", "mm", list(time,lat,lon), NA)
  t_max = ncvar_def("t_max", "C", list(time,lat,lon), NA)
  t_min = ncvar_def("t_min", "C", list(time,lat,lon), NA)
  
  # Create new NetCDF file
  ncnew = nc_create(paste0("MetSim/",GCM,"-",Scenario,"_",BasinName,"State.nc"), list(prec,t_max,t_min))
  
  # Use last 90 days of 2006 as fake 2005 data
  TimeIndices = (365-90+1):365
  
  # Load corresponding weather data into the file
  tLength = length(TimeIndices)
  for (i in 1:tLength){
    # Rasterize and clip weather data for each day
    pr.rast = raster(t(pr.array[,,TimeIndices[i]]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats),
                     crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    prdata = c(as.matrix(crop(pr.rast,extent(ModelDomainSPDF))))
    tmmn.rast = raster(t(tmmn.array[,,TimeIndices[i]]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats),
                       crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    tmmndata = c(as.matrix(crop(tmmn.rast,extent(ModelDomainSPDF)))) - 273.15
    tmmx.rast = raster(t(tmmx.array[,,TimeIndices[i]]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats),
                       crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    tmmxdata = c(as.matrix(crop(tmmx.rast,extent(ModelDomainSPDF)))) - 273.15
    problemTemps = which(tmmxdata<tmmndata)
    tmmxdata[problemTemps] = tmmndata[problemTemps] + 1 # This should be fixed in gridMet
    
    # Load each day's subset values into the new NetCDF
    ncvar_put(ncnew, prec, prdata, start=c(i,1,1), count=c(1,-1,-1))
    ncvar_put(ncnew, t_max, tmmxdata, start=c(i,1,1), count=c(1,-1,-1))
    ncvar_put(ncnew, t_min, tmmndata, start=c(i,1,1), count=c(1,-1,-1))
    
    print(i/tLength)
  }
  
  # View sample results
  ncvar_get(ncnew,"t_min")
  plot(ncvar_get(ncnew,"t_max")[,1,1],type='l')
  
  nc_close(ncnew)
  nc_close(nc_data_pr)
  nc_close(nc_data_tmmn)
  nc_close(nc_data_tmmx)
  
  # End of loop over basins
}

# Running MetSim:
# Open Windows command prompt, cd to MetSim folder
# Run "ms -v FILENAME.conf"
# It's probably ok if this produces a scary-looking error, see if it worked anyway

################################################################################
# Handle MetSim Output Data 

library(ncdf4)
library(raster)

# GCM = "MIROC5"
GCM = "CNRM-CM5"
Scenario = "RCP8.5"

resultsdir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/MACA/MetSim/output/"

dir = paste0("C:/Users/board/Desktop/DHSVM/TCSI_Setup/MACA/",GCM,"/StationForcingFiles/",Scenario,"/")
setwd(dir)

FirstYr = 2006
LastYr = 2099

BasinNames = c("Truckee","Yuba","Bear","American")

for (BasinNumber in 1:length(BasinNames)){
  BasinName = BasinNames[BasinNumber]
  
  # Read data from MetSim output
  nc_out = nc_open(paste0(resultsdir,GCM,"-",Scenario,"_",BasinName,"Output_",FirstYr,"0101-",LastYr,"1231.nc"))
  
  # View sample results
  plot(ncvar_get(nc_out,"longwave")[1,1,],type='l')
  
  lats = ncvar_get(nc_out,"lat")
  lons = ncvar_get(nc_out,"lon")
  
  # Save rasters for sanity-check and visualization
  rasterdir = paste0("C:/Users/board/Desktop/DHSVM/TCSI_Setup/MACA/",GCM,"/")
  writeRaster(raster(t(apply(ncvar_get(nc_out,"temp"),c(1,2),mean)), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats),
                     crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")),
              paste0(rasterdir,GCM,"-",Scenario,"-",BasinName,"MeanTemp.tif"), overwrite=TRUE)
  writeRaster(raster(t(apply(ncvar_get(nc_out,"wind"),c(1,2),mean)), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats),
                     crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")),
              paste0(rasterdir,GCM,"-",Scenario,"-",BasinName,"MeanWind.tif"), overwrite=TRUE)
  writeRaster(raster(t(apply(ncvar_get(nc_out,"rel_humid"),c(1,2),mean)), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats),
                     crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")),
              paste0(rasterdir,GCM,"-",Scenario,"-",BasinName,"MeanHumid.tif"), overwrite=TRUE)
  writeRaster(raster(t(apply(ncvar_get(nc_out,"shortwave"),c(1,2),mean)), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats),
                     crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")),
              paste0(rasterdir,GCM,"-",Scenario,"-",BasinName,"MeanShort.tif"), overwrite=TRUE)
  writeRaster(raster(t(apply(ncvar_get(nc_out,"longwave"),c(1,2),mean)), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats),
                     crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")),
              paste0(rasterdir,GCM,"-",Scenario,"-",BasinName,"MeanLong.tif"), overwrite=TRUE)
  writeRaster(raster(t(apply(ncvar_get(nc_out,"prec"),c(1,2),mean)), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats),
                     crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")),
              paste0(rasterdir,GCM,"-",Scenario,"-",BasinName,"MeanPrecip.tif"), overwrite=TRUE)
  
  
  # Write data to DHSVM station files
  for (i in 1:length(ncvar_get(nc_out,"lat"))){
    Lat = ncvar_get(nc_out,"lat")[i]
    for (j in 1:length(ncvar_get(nc_out,"lon"))){
      Lon = ncvar_get(nc_out,"lon")[j]
      
      if (!file.exists(paste0("./data_",format(round(Lat,5),nsmall=5),"_",format(round(Lon,5),nsmall=5)))){
        DateTimes = as.POSIXct(ncvar_get(nc_out,"time")*60,origin=as.POSIXct("2000-01-01",tz="UTC"),tz="UTC")
        
        OutMatrix = matrix(0,nrow=length(ncvar_get(nc_out,"time")),ncol=7)
        
        OutMatrix[,1] = as.character(DateTimes,"%m/%d/%Y-%H")
        OutMatrix[,2] = ncvar_get(nc_out,"temp")[j,i,]
        OutMatrix[,3] = ncvar_get(nc_out, "wind")[j,i,]
        OutMatrix[,4] = ncvar_get(nc_out,"rel_humid")[j,i,]
        OutMatrix[,5] = ncvar_get(nc_out,"shortwave")[j,i,]
        OutMatrix[,6] = ncvar_get(nc_out,"longwave")[j,i,]
        OutMatrix[,7] = ncvar_get(nc_out,"prec")[j,i,]/1000 # Convert to m
        
        # Need to write in Unix (LF not CRLF) format
        OutFile = file(paste0("./data_",format(round(Lat,5),nsmall=5),"_",format(round(Lon,5),nsmall=5)), "wb")
        write(t(OutMatrix), file=OutFile, ncolumns=7)
        close(OutFile)
      }
      
      print(paste0("Lat: ",Lat," Lon: ",Lon))
    }
  }
  
  nc_close(nc_out)
  remove(nc_out)
}

################################################################################
# Add Pre-Computed Temperature  Lapse Rates

library(raster)
library(rgdal)

# GCM = "MIROC5"
GCM = "CNRM-CM5"
Scenario = "RCP8.5"

# Get lapse rate data generated by SpatialLapseRatesForDHSVM.R
lapsedir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/"
SpatialLapseRates = read.csv(paste0(lapsedir,"TCSImonthlyTemperatureLapseRatesPRISM_ByGridMetStation.csv"))

# Re-create standardized UTM grid
AllowedX = unique(SpatialLapseRates$StationX)
AllowedY = unique(SpatialLapseRates$StationY)

domainrastdir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/PRISM-GridMet_Multiplier/"
DomainTemplate = raster(paste0(domainrastdir,"TCSIdomainTemplate.tif"))

# Get pre-existing meteorology files
dir = paste0("C:/Users/board/Desktop/DHSVM/TCSI_Setup/MACA/",GCM,"/StationForcingFiles/",Scenario,"/")
setwd(dir)
StationFiles = list.files(dir)

for (i in 1:length(StationFiles)){
  OriginalData = read.table(StationFiles[i])
  
  # Only append a new column if it isn't already there
  if (dim(OriginalData)[2] == 7){
    # Convert coordinates in filename into UTM and snap to the allowed grid center points
    Lat = as.numeric(substr(StationFiles[i],6,13))
    Lon = as.numeric(substr(StationFiles[i],15,24))
    coords = SpatialPoints(cbind(Lon,Lat), proj4string=crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    coordsUTM = spTransform(coords, crs(DomainTemplate))
    x = as.data.frame(coordsUTM)$Lon
    y = as.data.frame(coordsUTM)$Lat
    xAdj = AllowedX[abs(AllowedX-x)==min(abs(AllowedX-x))]
    yAdj = AllowedY[abs(AllowedY-y)==min(abs(AllowedY-y))]
    
    Months = as.integer(substr(as.data.frame(OriginalData)$V1,1,2))
    TempLapseRates = rep(0,length(Months))
    
    for (mon in 1:12){
      TempLapseRates[Months==mon] = SpatialLapseRates[(SpatialLapseRates[,"StationX"]==xAdj &
                                                         SpatialLapseRates[,"StationY"]==yAdj &
                                                         SpatialLapseRates[,"Month"]==mon),]$TemperatureLapseRate
    }
    
    UpdatedData = cbind(OriginalData, TempLapseRates)
    
    OutFile = file(StationFiles[i], "wb")
    write.table(UpdatedData, file=OutFile, col.names=FALSE, row.names=FALSE, quote=FALSE)
    close(OutFile)
  }
  print(i/length(StationFiles))
}

################################################################################
# Make actual "DHSVM-normal" precipitation map for visualization/justification

library(dismo)
library(sf)

FirstYr = 2070 # last 30 yrs of record
LastYr = 2099

# Hack for now: just tell it which lines to use
FirstLine = 187009
LastLine = 274664

# GCM = "MIROC5"
GCM = "CNRM-CM5"
Scenario = "RCP8.5"

domaindir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/WatershedBoundaries/"

# Read basin domain template
dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/PRISM-GridMet_Multiplier/"
setwd(dir)
DomainTemplate = raster("TCSIdomainTemplate.tif")

shapedir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/"

stationdir = paste0("C:/Users/board/Desktop/DHSVM/TCSI_Setup/MACA/",GCM,"/StationForcingFiles/",Scenario,"/")
StationMonths = as.numeric(format(as.POSIXct(read.table(paste0(stationdir,"data_39.77066_-121.41492"))[,1],format="%m/%d/%Y-%H"),"%m"))
LinesEnum = 1:length(read.table(paste0(stationdir,"data_39.77066_-121.41492"))[,1])

redistrdir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/PRISM/PRISM-GridMet_Multiplier/"

configdir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/MetSim/"

dir = paste0("C:/Users/board/Desktop/DHSVM/TCSI_Setup/MACA/",GCM,"/")
setwd(dir)

BasinNames = c("Truckee","Yuba","Bear","American")

for (BasinNumber in 1:length(BasinNames)){
  BasinName = BasinNames[BasinNumber]
  # Read basin shape and mask
  BasinMask = read_sf(paste0(shapedir,BasinName,"Shape"))
  BasinMaskRaster = raster(paste0(domaindir,"TCSImask_",BasinName,"Domain.tif"))
  BasinMaskRaster[BasinMaskRaster==0] = NA
  
  MetSection = readLines(paste0(configdir,"MeteorologySection_",BasinName,".txt"))
  
  StationName = c()
  StationNorth = c()
  StationEast = c()
  
  for (i in 1:length(MetSection)){
    if (grepl("Name",MetSection[i])){
      StationName = c(StationName, sub(".*= ","",MetSection[i]))
      StationNorth = c(StationNorth, sub(".*= ","",MetSection[i+1]))
      StationEast = c(StationEast, sub(".*= ","",MetSection[i+2]))
    }
  }
  
  # DHSVM keeps the first station only when 2 are equally "close"
  UniqueStationPtrs = match(unique(paste0(StationNorth,StationEast)), paste0(StationNorth,StationEast))
  nUniqueStations = length(UniqueStationPtrs)
  print(nUniqueStations)
  PointsNorth = c()
  PointsEast = c()
  PointsAccumPrecip = c()
  for (i in UniqueStationPtrs){
    StationFile = read.table(paste0(stationdir,StationName[i]))
    AccumPrecip = sum(StationFile[FirstLine:LastLine,7])/(LastYr-FirstYr+1)
    PointsAccumPrecip = c(PointsAccumPrecip, AccumPrecip)
    PointsNorth = c(PointsNorth, StationNorth[i])
    PointsEast = c(PointsEast, StationEast[i])
    print(i)
  }
  
  # The -90/2 is because of some funny business with the way DHSVM chooses nearest neighbor
  # It's supposed to be consistent with GridMetPRISMmultiplierForDHSVMadvanced.R
  RedistrPPoints = data.frame(x=as.numeric(PointsEast),y=as.numeric(PointsNorth)-90/2,PointsAccumPrecip)
  plot(DomainTemplate)
  plot(st_transform(BasinMask,crs=crs(DomainTemplate))[2],add=TRUE,col=NA,lwd=2)
  RedistrPDomains = SpatialPoints(RedistrPPoints[,1:2], proj4string=crs(DomainTemplate))
  RedistrPDomains = SpatialPointsDataFrame(RedistrPDomains,RedistrPPoints)
  plot(RedistrPDomains,add=TRUE)
  RedistrPVoronoi = voronoi(RedistrPDomains)
  plot(RedistrPVoronoi,add=TRUE)
  RedistrPMapModelDomain = rasterize(RedistrPVoronoi, DomainTemplate, "PointsAccumPrecip") * BasinMaskRaster
  plot(RedistrPMapModelDomain)
  
  writeRaster(RedistrPMapModelDomain,
              paste0(GCM,"-",Scenario,"_",FirstYr,"-",LastYr,"_MeanYearlyPrecip_DHSVM-",BasinName,".tif"), overwrite=TRUE)
  
  ##########################
  # For PRISM redistribution
  for (mon in 1:12){
    MultiplierMap = raster(paste0(redistrdir,"MultiplierMap_ForGridMet_FromPRISM_Month",mon,".tif"))
    
    MonPointers = which(StationMonths == mon & LinesEnum >= FirstLine & LinesEnum <= LastLine)
    UniqueStationPtrs = match(unique(paste0(StationNorth,StationEast)), paste0(StationNorth,StationEast))
    nUniqueStations = length(UniqueStationPtrs)
    print(nUniqueStations)
    PointsNorth = c()
    PointsEast = c()
    PointsAccumPrecip = c()
    for (i in UniqueStationPtrs){
      StationFile = read.table(paste0(stationdir,StationName[i]))
      AccumPrecip = sum(StationFile[MonPointers,7])/(LastYr-FirstYr+1)
      PointsAccumPrecip = c(PointsAccumPrecip, AccumPrecip)
      PointsNorth = c(PointsNorth, StationNorth[i])
      PointsEast = c(PointsEast, StationEast[i])
      print(i)
    }
    
    # The -90/2 is because of some funny business with the way DHSVM chooses nearest neighbor
    # It's supposed to be consistent with GridMetPRISMmultiplierForDHSVMadvanced.R
    RedistrPPoints = data.frame(x=as.numeric(PointsEast),y=as.numeric(PointsNorth)-90/2,PointsAccumPrecip)
    RedistrPDomains = SpatialPoints(RedistrPPoints[,1:2], proj4string=crs(DomainTemplate))
    RedistrPDomains = SpatialPointsDataFrame(RedistrPDomains,RedistrPPoints)
    RedistrPVoronoi = voronoi(RedistrPDomains)
    RedistrPMapModelDomain = rasterize(RedistrPVoronoi, DomainTemplate, "PointsAccumPrecip") * MultiplierMap * BasinMaskRaster
    plot(RedistrPMapModelDomain)
    
    # Compute yearly sum of redistributed precip
    if (mon==1){
      YearlyRedistrP = RedistrPMapModelDomain
    } else {
      YearlyRedistrP = YearlyRedistrP + RedistrPMapModelDomain
    }
    
    print(mon)
    print("**********")
  }
  
  writeRaster(YearlyRedistrP,
              paste0(GCM,"-",Scenario,"_PRISMredistributed_",FirstYr,"-",LastYr,"_MeanYearlyPrecip_DHSVM-",BasinName,".tif"),
              overwrite=TRUE)
}

################################################################################
# Make some visualizations

library(ncdf4)
library(raster)
library(ggplot2)

GCM = "MIROC5"
#GCM = "CNRM-CM5"
Scenario = "RCP8.5"

resultsdir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/MACA/MetSim/output/"

domaindir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/WatershedBoundaries/"

dir = paste0("C:/Users/board/Desktop/DHSVM/TCSI_Setup/MACA/",GCM,"/StationForcingFiles/",Scenario,"/")
setwd(dir)

FirstYr = 2006
LastYr = 2099

BasinNames = c("Truckee","Yuba","Bear","American")

for (BasinNumber in 1:length(BasinNames)){
  BasinName = BasinNames[BasinNumber]
  BasinMaskRaster = raster(paste0(domaindir,"TCSImask_",BasinName,"Domain.tif"))
  
  # Read data from MetSim output
  nc_out = nc_open(paste0(resultsdir,GCM,"-",Scenario,"_",BasinName,"Output_",FirstYr,"0101-",LastYr,"1231.nc"))
  
  lats = ncvar_get(nc_out,"lat")
  lons = ncvar_get(nc_out,"lon")
  
  temp.array = ncvar_get(nc_out,"temp")
  wind.array = ncvar_get(nc_out,"wind")
  rel_humid.array = ncvar_get(nc_out,"rel_humid")
  shortwave.array = ncvar_get(nc_out,"shortwave")
  longwave.array = ncvar_get(nc_out,"longwave")
  prec.array = ncvar_get(nc_out,"prec")
  
  ClimateDates = as.POSIXct(ncvar_get(nc_out,"time")*60,origin=as.POSIXct("2000-01-01",tz="UTC"),tz="UTC")
  
  # Additional setup
  if (BasinNumber==1){
    # Results structure
    YearlyClimateStats = data.frame(matrix(nrow=length(BasinNames)*(LastYr-FirstYr+1),ncol=8))
    names(YearlyClimateStats) = c("Year","Watershed",c("Temp","Wind","Humid","Short","Long","Prec"))
    YearlyClimateStats$Year = sort(rep(FirstYr:LastYr, length(BasinNames)))
    YearlyClimateStats$Watershed = rep(BasinNames, (LastYr-FirstYr+1))
  }
  
  nc_close(nc_out)
  remove(nc_out)
  
  temp.rast = raster(t(temp.array[,,1]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats),
                     crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

  BasinMaskRaster = projectRaster(BasinMaskRaster, temp.rast, method="ngb")
  BasinMaskRasterPtrs = which(BasinMaskRaster[,]==1)
  
  # This is kind of a hack but it works, so...
  # length(unique(temp.rast[,]))/length(temp.rast[,]) # This needs to equal 1
  BasinMaskPtrs = which(as.vector(temp.array[,,1]) %in% temp.rast[BasinMaskRasterPtrs])
  stopifnot(mean(temp.array[,,1][BasinMaskPtrs]) == mean(temp.rast[BasinMaskRasterPtrs]))
  
  # Aggregate data to yearly timescales
  YrIndex = as.numeric(format(ClimateDates,"%Y"))
  
  for (yr in unique(YearlyClimateStats$Year)){
    YrPtrs = which(YrIndex==yr)
    
    YearlyClimateStats[(YearlyClimateStats$Watershed==BasinName & YearlyClimateStats$Year==yr),"Temp"] = mean(apply(temp.array[,,YrPtrs], c(1,2), mean)[BasinMaskPtrs])
    YearlyClimateStats[(YearlyClimateStats$Watershed==BasinName & YearlyClimateStats$Year==yr),"Wind"] = mean(apply(wind.array[,,YrPtrs], c(1,2), mean)[BasinMaskPtrs])
    YearlyClimateStats[(YearlyClimateStats$Watershed==BasinName & YearlyClimateStats$Year==yr),"Humid"] = mean(apply(rel_humid.array[,,YrPtrs], c(1,2), mean)[BasinMaskPtrs])
    YearlyClimateStats[(YearlyClimateStats$Watershed==BasinName & YearlyClimateStats$Year==yr),"Short"] = mean(apply(shortwave.array[,,YrPtrs], c(1,2), mean)[BasinMaskPtrs])
    YearlyClimateStats[(YearlyClimateStats$Watershed==BasinName & YearlyClimateStats$Year==yr),"Long"] = mean(apply(longwave.array[,,YrPtrs], c(1,2), mean)[BasinMaskPtrs])
    YearlyClimateStats[(YearlyClimateStats$Watershed==BasinName & YearlyClimateStats$Year==yr),"Prec"] = mean(apply(prec.array[,,YrPtrs], c(1,2), mean)[BasinMaskPtrs])
    
    print(yr)
  }
  
}

YearlyClimateStats$YearDate = as.Date(paste0(YearlyClimateStats$Year,"-01-01"))

write.csv(YearlyClimateStats, paste0("C:/Users/board/Desktop/DHSVM/TCSI_Setup/MACA/MACA_",GCM,"-",Scenario,"_YearlyClimateStats_TCSIbasins.csv"))

################################################################################
# Visualize climate timeseries with ggplot

# Temperature
ggplot(data=YearlyClimateStats, aes(x=YearDate,y=Temp,group=Watershed)) + 
  geom_line(aes(linetype=Watershed,col=Watershed), size=1.5) +
  scale_color_manual(breaks=c("Truckee","Yuba","Bear","American"), values=c("darkgoldenrod2","red","limegreen","blue")) +
  scale_linetype_manual(breaks=c("Truckee","Yuba","Bear","American"), values=c("solid","31","3111","11")) +
  scale_x_date(breaks=seq(as.Date(paste0(FirstYr-1,"-01-01")),as.Date(paste0(LastYr+1,"-01-01")),by="5 year"),date_labels="%Y")  +
  labs(y="Mean Annual Temperature, ?C", title=paste0("Climate Projections for TCSI, MACA ",GCM,", ",Scenario)) +
  theme_bw() +
  theme(axis.text.x=element_text(size=18, angle=45, vjust=0.5,color="black"), axis.text.y=element_text(size=18,color="black"),
        axis.title.x=element_blank(), axis.title.y=element_text(size=24),
        legend.title=element_blank(), legend.text=element_text(size=24)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,1,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"), panel.grid=element_blank()) +
  theme(plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),legend.position="bottom") +
  guides(color=guide_legend(override.aes=list(size=3),byrow=TRUE))

# Precipitation
ggplot(data=YearlyClimateStats, aes(x=YearDate,y=Prec,group=Watershed)) + 
  geom_line(aes(linetype=Watershed,col=Watershed), size=1.5) +
  scale_color_manual(breaks=c("Truckee","Yuba","Bear","American"), values=c("darkgoldenrod2","red","limegreen","blue")) +
  scale_linetype_manual(breaks=c("Truckee","Yuba","Bear","American"), values=c("solid","31","3111","11")) +
  scale_x_date(breaks=seq(as.Date(paste0(FirstYr-1,"-01-01")),as.Date(paste0(LastYr+1,"-01-01")),by="5 year"),date_labels="%Y")  +
  labs(y="Annual Precipitation, m", title=paste0("Climate Projections for TCSI, MACA ",GCM,", ",Scenario)) +
  theme_bw() +
  theme(axis.text.x=element_text(size=18, angle=45, vjust=0.5,color="black"), axis.text.y=element_text(size=18,color="black"),
        axis.title.x=element_blank(), axis.title.y=element_text(size=24),
        legend.title=element_blank(), legend.text=element_text(size=24)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,1,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"), panel.grid=element_blank()) +
  theme(plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),legend.position="bottom") +
  guides(color=guide_legend(override.aes=list(size=3),byrow=TRUE))

# Humidity
ggplot(data=YearlyClimateStats, aes(x=YearDate,y=Humid,group=Watershed)) + 
  geom_line(aes(linetype=Watershed,col=Watershed), size=1.5) +
  scale_color_manual(breaks=c("Truckee","Yuba","Bear","American"), values=c("darkgoldenrod2","red","limegreen","blue")) +
  scale_linetype_manual(breaks=c("Truckee","Yuba","Bear","American"), values=c("solid","31","3111","11")) +
  scale_x_date(breaks=seq(as.Date(paste0(FirstYr-1,"-01-01")),as.Date(paste0(LastYr+1,"-01-01")),by="5 year"),date_labels="%Y")  +
  labs(y="Mean Annual Humidity, %", title=paste0("Climate Projections for TCSI, MACA ",GCM,", ",Scenario)) +
  theme_bw() +
  theme(axis.text.x=element_text(size=18, angle=45, vjust=0.5,color="black"), axis.text.y=element_text(size=18,color="black"),
        axis.title.x=element_blank(), axis.title.y=element_text(size=24),
        legend.title=element_blank(), legend.text=element_text(size=24)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,1,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"), panel.grid=element_blank()) +
  theme(plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),legend.position="bottom") +
  guides(color=guide_legend(override.aes=list(size=3),byrow=TRUE))

# Shortwave
ggplot(data=YearlyClimateStats, aes(x=YearDate,y=Short,group=Watershed)) + 
  geom_line(aes(linetype=Watershed,col=Watershed), size=1.5) +
  scale_color_manual(breaks=c("Truckee","Yuba","Bear","American"), values=c("darkgoldenrod2","red","limegreen","blue")) +
  scale_linetype_manual(breaks=c("Truckee","Yuba","Bear","American"), values=c("solid","31","3111","11")) +
  scale_x_date(breaks=seq(as.Date(paste0(FirstYr-1,"-01-01")),as.Date(paste0(LastYr+1,"-01-01")),by="5 year"),date_labels="%Y")  +
  labs(y=expression("Mean Annual Incoming Shortwave Radiation, w/m "^2), title=paste0("Climate Projections for TCSI, MACA ",GCM,", ",Scenario)) +
  theme_bw() +
  theme(axis.text.x=element_text(size=18, angle=45, vjust=0.5,color="black"), axis.text.y=element_text(size=18,color="black"),
        axis.title.x=element_blank(), axis.title.y=element_text(size=24),
        legend.title=element_blank(), legend.text=element_text(size=24)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,1,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"), panel.grid=element_blank()) +
  theme(plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),legend.position="bottom") +
  guides(color=guide_legend(override.aes=list(size=3),byrow=TRUE))

# Longwave
ggplot(data=YearlyClimateStats, aes(x=YearDate,y=Long,group=Watershed)) + 
  geom_line(aes(linetype=Watershed,col=Watershed), size=1.5) +
  scale_color_manual(breaks=c("Truckee","Yuba","Bear","American"), values=c("darkgoldenrod2","red","limegreen","blue")) +
  scale_linetype_manual(breaks=c("Truckee","Yuba","Bear","American"), values=c("solid","31","3111","11")) +
  scale_x_date(breaks=seq(as.Date(paste0(FirstYr-1,"-01-01")),as.Date(paste0(LastYr+1,"-01-01")),by="5 year"),date_labels="%Y")  +
  labs(y=expression("Mean Annual Incoming Longwave Radiation, w/m "^2), title=paste0("Climate Projections for TCSI, MACA ",GCM,", ",Scenario)) +
  theme_bw() +
  theme(axis.text.x=element_text(size=18, angle=45, vjust=0.5,color="black"), axis.text.y=element_text(size=18,color="black"),
        axis.title.x=element_blank(), axis.title.y=element_text(size=24),
        legend.title=element_blank(), legend.text=element_text(size=24)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,1,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"), panel.grid=element_blank()) +
  theme(plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),legend.position="bottom") +
  guides(color=guide_legend(override.aes=list(size=3),byrow=TRUE))

# Wind
ggplot(data=YearlyClimateStats, aes(x=YearDate,y=Wind,group=Watershed)) + 
  geom_line(aes(linetype=Watershed,col=Watershed), size=1.5) +
  scale_color_manual(breaks=c("Truckee","Yuba","Bear","American"), values=c("darkgoldenrod2","red","limegreen","blue")) +
  scale_linetype_manual(breaks=c("Truckee","Yuba","Bear","American"), values=c("solid","31","3111","11")) +
  scale_x_date(breaks=seq(as.Date(paste0(FirstYr-1,"-01-01")),as.Date(paste0(LastYr+1,"-01-01")),by="5 year"),date_labels="%Y")  +
  labs(y="Mean Annual Wind Speed, m/s", title=paste0("Climate Projections for TCSI, MACA ",GCM,", ",Scenario)) +
  theme_bw() +
  theme(axis.text.x=element_text(size=18, angle=45, vjust=0.5,color="black"), axis.text.y=element_text(size=18,color="black"),
        axis.title.x=element_blank(), axis.title.y=element_text(size=24),
        legend.title=element_blank(), legend.text=element_text(size=24)) +
  theme(legend.key.width=unit(2,"cm"),legend.spacing.y=unit(1,"cm"),
        axis.ticks.length=unit(0.3,"cm"),plot.margin=margin(0.5,1,0.5,0.5,"cm"),
        panel.background=element_rect("white", "black"), panel.grid=element_blank()) +
  theme(plot.title=element_text(color="black",size=24,hjust=0.5,face="bold"),legend.position="bottom") +
  guides(color=guide_legend(override.aes=list(size=3),byrow=TRUE))
