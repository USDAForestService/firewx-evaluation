############# EXTRACT DATA FROM LANDFIRE ASOS/RAWS DATA ##################

### Set needed packages
library(geosphere)
library(raster) 
library(rgdal)
library(sp)

### Set Working Directory
setwd("C:/Users/wesleygpage/Desktop/NDFD_Project/Weather/")

### Get RAWS lat/long / Format
raws.loc = read.csv("Misc/raws_longs.csv")
raws.loc = SpatialPoints(raws.loc)
proj4string(raws.loc) = CRS("+init=epsg:4326")

### Get lat/longs from RAW ASOS data
 ## Read-in files
 files = list.files("ASOS_raw/")

 ## Create vector of file names
 name = vector()
 for (i in 2:length(files)) {
  name[i-1] = gsub("2015.txt","",files[i])  }
 name[15] = "Kentucky"

 ## Read-in observed weather data / Set formating
 asos.loc = data.frame()
 for (i in 2:length(files))  {
  temp = read.delim(paste("ASOS_raw/",files[i],sep=""),skip=5)
  location = unique(cbind(temp$lon,temp$lat))
  asos.loc = rbind(location,asos.loc)  }
  asos.loc = SpatialPoints(loc)
  proj4string(asos.loc) = CRS("+init=epsg:4326")

### Extract LANDFIRE data with lat/longs
 ## Create raster for each data type
  # Fuel model (40 fm)
  fbfm40 = raster("F:/conus_fuel/tiff/us_130fbfm40.tif")
  # Aspect (degrees)
  aspect = raster("F:/conus_fuel/tiff/us_asp.tif")
  # Elevation (meters)
  elev = raster("F:/conus_fuel/tiff/us_dem.tif")
  # Slope (degrees)
  slope = raster("F:/conus_fuel/tiff/us_slp.tif")
  # CBD (kg m-3 * 100)
  CBD = raster("F:/conus_fuel/tiff/us_130cbd.tif")
  # CBH (m * 10)
  CBH = raster("F:/conus_fuel/tiff/us_130cbh.tif")
  # CC (percent)
  CC = raster("F:/conus_fuel/tiff/us_130cc.tif")
  # CH (m * 10)
  CH = raster("F:/conus_fuel/tiff/us_130ch.tif")
  
 ## Extract data values / currently set to run asos or raws separately, i.e. must change manually
 LandFire = data.frame(lon=loc$lon,lat=loc$lat)
 fm = extract(fbfm40,asos.loc)
 asp = extract(aspect,asos.loc)
 ele = extract(elev,asos.loc)
 sl = extract(slope,asos.loc)
 cbd = extract(CBD,asos.loc)
 cbh = extract(CBH,asos.loc)
 cc = extract(CC,asos.loc)
 ch = extract(CH,asos.loc)
 LandFire = cbind(LandFire,fm,asp,ele,sl,cbd,cbh,cc,ch)  
 colnames(LandFire) = c("lon","lat","FM40","asp_deg","elev_m","slope_deg","CBD_kgm3","CBH_m",
 "CC_percent","CH_m")

 ## Fix units
 LandFire$CBD_kgm3 = LandFire$CBD_kgm3 / 100
 LandFire$CBH_m = LandFire$CBH_m / 10
 LandFire$CH_m = LandFire$CH_m / 10

### Remove those observations from stations w/ non-burnable fuel model
 ## Determine which lat/longs are non-burnable and exclude
 noburn.fm = c(-9999,91,92,93,98,99) 
 good.loc = subset(LandFire, !(FM40 %in% noburn.fm))
 good.loc = data.frame(good.loc$lon,good.loc$lat)

### Export out results
write.csv(good.loc,"asos_longs.csv") #export out good lat/longs
