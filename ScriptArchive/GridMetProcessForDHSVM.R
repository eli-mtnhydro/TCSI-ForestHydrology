library(ncdf4)
library(raster)
library(rgdal)

#########################################
########## Create Forcing File ##########
#########################################

# Set up basin name for later use
BasinNumber = 1
BasinNames = c("Truckee","Yuba","Bear","American")
BasinName = BasinNames[BasinNumber]
#BasinName = "TCSI"

# Read data from gridMet downloads
dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/"
setwd(dir)

# Read shapefile of model domain
ModelDomainSPDF = readOGR(dsn=paste0(BasinName,"Shape"))
#ModelDomainSPDF = readOGR(dsn="TCSIshape")

FirstYr = 1979 # Hard coded later to use 1979 as starting point when creating state file
LastYr = 2020
ForcingFilename = paste0(BasinName,FirstYr,"-",LastYr,".nc")

for (yr in FirstYr:LastYr){
  nc_data_pr = nc_open(paste0("pr_",yr,".nc"))
  nc_data_tmmn = nc_open(paste0("tmmn_",yr,".nc"))
  nc_data_tmmx = nc_open(paste0("tmmx_",yr,".nc"))
  nc_data_vs = nc_open(paste0("vs_",yr,".nc"))
  # nc_data_srad = nc_open(paste0("srad_",yr,".nc"))
  # nc_data_rmin = nc_open(paste0("rmin_",yr,".nc"))
  # nc_data_rmax = nc_open(paste0("rmax_",yr,".nc"))

  # Convert weather variables into array data
  pr.array = ncvar_get(nc_data_pr,"precipitation_amount")
  #lons = ncvar_get(nc_data_pr,"lon")
  #lats = ncvar_get(nc_data_pr,"lat")
  #plot(crop(raster(t(apply(pr.array,c(1,2),mean)), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")),extent(ModelDomainSPDF)))
  tmmn.array = ncvar_get(nc_data_tmmn,"air_temperature")
  tmmx.array = ncvar_get(nc_data_tmmx,"air_temperature")
  vs.array = ncvar_get(nc_data_vs,"wind_speed")
  # srad.array = ncvar_get(nc_data_srad,"surface_downwelling_shortwave_flux_in_air")
  # rmin.array = ncvar_get(nc_data_rmin,"relative_humidity")
  # rmax.array = ncvar_get(nc_data_rmax,"relative_humidity")
  
  # Set up dimensions and create the new NetCDF if necessary
  if (yr == FirstYr){
    # Generate vectors of all lat/lon and days in full gridMet domain
    lons = ncvar_get(nc_data_pr,"lon")
    lats = ncvar_get(nc_data_pr,"lat")
    days = ncvar_get(nc_data_pr,"day")
    
    # Generate sample single-day array to determine new lat/lon dimensions
    pr.slice = pr.array[,,1]
    pr.rast = raster(t(pr.slice), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
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
    # shortwave = ncvar_def("shortwave", "W/m^2", list(lat,lon,time), NA)
    # r_min = ncvar_def("r_min", "%", list(lat,lon,time), NA)
    # r_max = ncvar_def("r_max", "%", list(lat,lon,time), NA)
    
    # Create new NetCDF file
    ncnew = nc_create(ForcingFilename, list(prec,t_max,t_min,wind))
    nc_close(ncnew)
    
    # Keep track of how many days of data have been written to the file
    tStart = 1
  }
  
  ncnew = nc_open(ForcingFilename, write=TRUE)
  
  #lons = ncvar_get(ncnew,"lon")
  #lats = ncvar_get(ncnew,"lat")
  #pr.array = ncvar_get(ncnew,"prec")
  #plot(crop(raster(t(apply(pr.array,c(1,2),mean)), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")),extent(ModelDomainSPDF)))
  
  # Load corresponding weather data into the file
  tLength = length(ncvar_get(nc_data_pr,"day"))
  TimeIndices = tStart:(tLength+tStart-1)
  for (i in 1:tLength){
    # Rasterize and clip weather data for each day
    pr.rast = raster(t(pr.array[,,i]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    prdata = c(as.matrix(crop(pr.rast,extent(ModelDomainSPDF))))
    tmmn.rast = raster(t(tmmn.array[,,i]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    tmmndata = c(as.matrix(crop(tmmn.rast,extent(ModelDomainSPDF)))) - 273.15
    tmmx.rast = raster(t(tmmx.array[,,i]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    tmmxdata = c(as.matrix(crop(tmmx.rast,extent(ModelDomainSPDF)))) - 273.15
    problemTemps = which(tmmxdata<tmmndata)
    tmmxdata[problemTemps] = tmmndata[problemTemps] + 1 # This should be fixed in gridMet
    vs.rast = raster(t(vs.array[,,i]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    vsdata = c(as.matrix(crop(vs.rast,extent(ModelDomainSPDF))))
    # srad.rast = raster(t(srad.array[,,i]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    # sraddata = c(as.matrix(crop(srad.rast,extent(ModelDomainSPDF))))
    # rmin.rast = raster(t(rmin.array[,,i]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    # rmindata = c(as.matrix(crop(rmin.rast,extent(ModelDomainSPDF))))
    # rmax.rast = raster(t(rmax.array[,,i]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    # rmaxdata = c(as.matrix(crop(rmax.rast,extent(ModelDomainSPDF))))
    
    # Load each day's subset values into the new NetCDF
    ncvar_put(ncnew, prec, prdata, start=c(1,1,TimeIndices[i]), count=c(-1,-1,1))
    ncvar_put(ncnew, t_max, tmmxdata, start=c(1,1,TimeIndices[i]), count=c(-1,-1,1))
    ncvar_put(ncnew, t_min, tmmndata, start=c(1,1,TimeIndices[i]), count=c(-1,-1,1))
    ncvar_put(ncnew, wind, vsdata, start=c(1,1,TimeIndices[i]), count=c(-1,-1,1))
    # ncvar_put(ncnew, shortwave, sraddata, start=c(1,1,TimeIndices[i]), count=c(-1,-1,1))
    # ncvar_put(ncnew, r_min, rmindata, start=c(1,1,TimeIndices[i]), count=c(-1,-1,1))
    # ncvar_put(ncnew, r_max, rmaxdata, start=c(1,1,TimeIndices[i]), count=c(-1,-1,1))
    print(paste0(100*i/tLength,"% done with year ",yr))
  }
  tStart = tLength + tStart
  
  # View sample results
  #ncvar_get(ncnew,"t_min")
  #plot(ncvar_get(ncnew,"t_min")[,1,1],type='l')
  #plot(rmax.rast)
  #plot(raster((ncvar_get(ncnew,prec)[,,4]), xmn=min(ncvar_get(ncnew,lon)), xmx=max(ncvar_get(ncnew,lon)), ymn=min(ncvar_get(ncnew,lat)), ymx=max(ncvar_get(ncnew,lat)), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")))
  
  nc_close(ncnew)
  nc_close(nc_data_pr)
  nc_close(nc_data_tmmn)
  nc_close(nc_data_tmmx)
  nc_close(nc_data_vs)
  # nc_close(nc_data_srad)
  # nc_close(nc_data_rmin)
  # nc_close(nc_data_rmax)
}

########################################
########## Create Domain File ##########
########################################

# Read elevation metadata from gridMet download
dir = "C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/"
setwd(dir)
nc_data_elevation = nc_open('metdata_elevationdata.nc')

# Read shapefile of model domain
# ModelDomainSPDF = readOGR(dsn=paste0(BasinName,"Shape"))

# Convert elevation variable into array data
elevation.array = ncvar_get(nc_data_elevation,"elevation")

# Generate vectors of all lat/lon in full gridMet domain
#lon = ncvar_get(nc_data_elevation,"lon")
#lat = ncvar_get(nc_data_elevation,"lat")
# Seems to have different coordinates than weather data even though same x/y size
nc_data_pr = nc_open(paste0("pr_",1979,".nc"))
lons = ncvar_get(nc_data_pr,"lon")
lats = ncvar_get(nc_data_pr,"lat")
nc_close(nc_data_pr)

# Generate array of elevation values
elevation.rast = raster(t(elevation.array), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(elevation.rast)
elevation.clipped = crop(elevation.rast,extent(ModelDomainSPDF))
plot(elevation.clipped)

# Define dimensions of new NetCDF
### Changed from pr.clipped
lon = ncdim_def( "lon", "degrees_east", unique(rasterToPoints(elevation.clipped)[,1]), longname="longitude")
lat = ncdim_def( "lat", "degrees_north", unique(rasterToPoints(elevation.clipped)[,2]), longname="latitude")

# Make domain variables with those dimensions
elev = ncvar_def("elev", "m", list(lon,lat), NA)
mask = ncvar_def("mask", "", list(lon,lat), NA)

# Create new NetCDF file
ncnew = nc_create(paste0(BasinName,"Domain.nc"), list(elev,mask))

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

nc_data_pr = nc_open(paste0("pr_",1979,".nc"))
nc_data_tmmn = nc_open(paste0("tmmn_",1979,".nc"))
nc_data_tmmx = nc_open(paste0("tmmx_",1979,".nc"))

# Convert weather variables into array data
pr.array = ncvar_get(nc_data_pr,"precipitation_amount")
tmmn.array = ncvar_get(nc_data_tmmn,"air_temperature")
tmmx.array = ncvar_get(nc_data_tmmx,"air_temperature")

# Define dimensions of new NetCDF
lon = ncdim_def("lon", "degrees_east", unique(rasterToPoints(pr.clipped)[,1]), longname="longitude")
lat = ncdim_def("lat", "degrees_north", unique(rasterToPoints(pr.clipped)[,2]), longname="latitude")
time = ncdim_def("time", "days since 1978-10-03 00:00:00", 0:89, calendar="proleptic_gregorian")

# Make weather variables with those dimensions
prec = ncvar_def("prec", "mm", list(time,lat,lon), NA)
t_max = ncvar_def("t_max", "C", list(time,lat,lon), NA)
t_min = ncvar_def("t_min", "C", list(time,lat,lon), NA)

# Create new NetCDF file
ncnew = nc_create(paste0(BasinName,"State.nc"), list(prec,t_max,t_min))

# Use last 90 days of 1979 as fake 1978 data
TimeIndices = (length(ncvar_get(nc_data_pr,"day"))-90+1):length(ncvar_get(nc_data_pr,"day"))

# Load corresponding weather data into the file
tLength = 90
for (i in 1:tLength){
  # Rasterize and clip weather data for each day
  pr.rast = raster(t(pr.array[,,TimeIndices[i]]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  prdata = c(as.matrix(crop(pr.rast,extent(ModelDomainSPDF))))
  tmmn.rast = raster(t(tmmn.array[,,TimeIndices[i]]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  tmmndata = c(as.matrix(crop(tmmn.rast,extent(ModelDomainSPDF)))) - 273.15
  tmmx.rast = raster(t(tmmx.array[,,TimeIndices[i]]), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
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

######################################
########## Handle Output Data ########
######################################

library(ncdf4)
library(raster)

BasinNumber = 2
BasinNames = c("Truckee","Yuba","Bear","American")
BasinName = BasinNames[BasinNumber]

FirstYr = 1979
LastYr = 2020

# Read data from MetSim output
dir = paste0("C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/MetSim/output/",BasinName)
setwd(dir)
nc_out = nc_open(paste0(BasinName,"Output_",FirstYr,"0101-",LastYr,"1231.nc"))

# View sample results
plot(ncvar_get(nc_out,"prec")[1,1,],type='l')

lats = ncvar_get(nc_out,"lat")
lons = ncvar_get(nc_out,"lon")

plot(raster(t(apply(ncvar_get(nc_out,"temp"),c(1,2),mean)), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")))
plot(raster(t(apply(ncvar_get(nc_out,"wind"),c(1,2),mean)), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")))
plot(raster(t(apply(ncvar_get(nc_out,"rel_humid"),c(1,2),mean)), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")))
plot(raster(t(apply(ncvar_get(nc_out,"shortwave"),c(1,2),mean)), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")))
plot(raster(t(apply(ncvar_get(nc_out,"longwave"),c(1,2),mean)), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")))
plot(raster(t(apply(ncvar_get(nc_out,"prec"),c(1,2),mean)), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")))

dir = paste0("C:/Users/board/Desktop/DHSVM/TCSI_Setup/GridMet/MetSim/output/",BasinName,"/StationForcingFiles/")
setwd(dir)

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












