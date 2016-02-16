#############  RAWS FORECAST DATA - EXTRACT LANDFIRE / FIX FORMATTING

### Set needed packages
library(geosphere)
library(raster) 
library(rgdal)
library(sp)
library(data.table)
library(plyr)

### Set Working Directory
setwd("/home/wpage/Documents/Output/RAWS/mod")

### Read-in Observed lat/long and Landfire data
loc = read.csv("/home/wpage/Documents/Output/Misc/landfire_raws.csv")
stn.id = read.csv("/home/wpage/Documents/Output/Misc/raws_station_id.csv")
stn.id$WRCC_ID = tolower(stn.id$WRCC_ID)
colnames(stn.id)[2] = "lat"
colnames(stn.id)[3] = "lon"
raws.obs = merge(loc,stn.id,by=c('lon','lat'))
colnames(raws.obs)[11]="station_id"
LandFire = raws.obs

### Read-in and fix FORECAST weather data / Format
 ## Get forecast data for each month
 files = list.files("/home/wpage/Documents/Output/RAWS/raw")
 for (i in 1:length(files))  {
  temp = data.table(read.csv(paste("/home/wpage/Documents/Output/RAWS/raw/",
  files[i],sep=""),header=TRUE))
  assign(files[i],temp)  }

 ## Start for loop to work with each file (month) one at a time
 for (j in 1:length(files)) {

 ## Fix misc variables
 temp = get(files[j])
 drop = c("30-36 hour acc fcst","6-12 hour acc fcst","2-8 hour acc fcst",
 "3-9 hour acc fcst","4-10 hour acc fcst","5-11 hour acc fcst","30 min fcst",
 "90-96 hour acc fcst","150-156 hour acc fcst","210-216 hour acc fcst",
 "270-276 hour acc fcst","330-336 hour acc fcst")
 temp = subset(temp, !Forecast %in% drop)
 temp$Value = as.numeric(as.character(temp$Value))
 temp$Long = as.numeric(as.character(temp$Long))
 temp$Lat = as.numeric(as.character(temp$Lat))
 Fct_Old = temp
 Fct_Old$X = NULL

 ## Match forecast lat/longs to station lat/long and add station id to Fct data
 ObsLoc1 = data.frame(unique(raws.obs[,c("station_id","lon","lat")]))
 ObsLoc = as.data.frame(cbind(ObsLoc1$lon,ObsLoc1$lat)) #get observed lat/longs
 FixLong = function(long) {  #Fix forecast long
  out = -(360-long)
  return(out)  }  
 Fct_Old$Long = unlist(lapply(Fct_Old$Long,FixLong)) #Fix long data
 FctLoc = unique(data.frame(Fct_Old$Long,Fct_Old$Lat)) #Get fixed forc lat/long data
 FctLoc = FctLoc[complete.cases(FctLoc),] # remove NAs
 Match = data.frame(Obs.lon=NA,Obs.lat=NA,Fct.lon=NA,Fct.lat=NA) #Find the match
 for (i in 1:length(ObsLoc[,2]))  {    #Create data frame that matches lat/longs
  distance = distGeo(ObsLoc[i,],FctLoc,a=6378137, f=1/298.257223563)
  MinDist = grep(min(distance),distance)
  Match[i,1] = ObsLoc[i,1]
  Match[i,2] = ObsLoc[i,2]
  Match[i,3] = FctLoc[MinDist,1]
  Match[i,4] = FctLoc[MinDist,2]  }
 Fct.n = data.frame()                           #Change data to appropriate lat/long
 for (i in 1:length(Match[,3])) {
  try = subset(Fct_Old,Long == Match[i,3] & Lat == Match[i,4])
  try$Long = Match[i,1]
  try$Lat = Match[i,2]
  try$station_id = ObsLoc1[i,1]
  Fct.n = rbind(try,Fct.n)}
 Fct_Old = Fct.n
 
 ## Work on precip
 extract = data.frame(subset(subset(Fct_Old, Variable == "APCP"),
 Forecast == "1-7 hour acc fcst" & Value > 0))
 for (i in 1:length(extract$Date)) {
  extract$Date[i] = paste(substr(extract$Date[i],1,4),"-",substr(extract$Date[i],5,6),
  "-",substr(extract$Date[i],7,8)," ",substr(extract$Date[i],9,10),sep="")  } 
 extract$Date = strptime(extract$Date, "%Y-%m-%d %H",tz="UTC")
 precip = data.frame(datetime=as.POSIXlt("2014-01-02 05", 
 format="%Y-%m-%d %H",tz="UTC"),lon=NA,lat=NA,precip=NA,stringsAsFactors=FALSE)
 precip.1 = data.frame(datetime=as.POSIXlt("2014-01-02 05", 
 format="%Y-%m-%d %H",tz="UTC"),lon=NA,lat=NA,precip=NA,stringsAsFactors=FALSE)

  #Break into smaller pieces
  extract1 = extract[1:(length(extract$Date)/2),]
  extract2 = extract[(length(extract$Date)/2)+1:length(extract$Date),]
  for (i in 1:length(extract1$Date)) {
   pre = data.frame(datetime=as.POSIXlt("2014-01-02 05", 
   format="%Y-%m-%d %H",tz="UTC"),lon=NA,lat=NA,precip=NA,stringsAsFactors=FALSE)
   pre[1,1] = extract1$Date[i] +3600
   pre[2,1] = extract1$Date[i] +7200
   pre[3,1] = extract1$Date[i] +10800
   pre[4,1] = extract1$Date[i] +14400
   pre[5,1] = extract1$Date[i] +18000
   pre[6,1] = extract1$Date[i] +21600
   pre$lon = extract1$Long[i]
   pre$lat = extract1$Lat[i]
   pre$precip = (extract1$Value[i]) / 6
   precip = rbind(pre,precip)  }
  for (i in 1:length(extract2$Date)) {
   pre = data.frame(datetime=as.POSIXlt("2014-01-02 05", 
   format="%Y-%m-%d %H",tz="UTC"),lon=NA,lat=NA,precip=NA,stringsAsFactors=FALSE)
   pre[1,1] = extract2$Date[i] +3600
   pre[2,1] = extract2$Date[i] +7200
   pre[3,1] = extract2$Date[i] +10800
   pre[4,1] = extract2$Date[i] +14400
   pre[5,1] = extract2$Date[i] +18000
   pre[6,1] = extract2$Date[i] +21600
   pre$lon = extract2$Long[i]
   pre$lat = extract2$Lat[i]
   pre$precip = (extract2$Value[i]) / 6
   precip.1 = rbind(pre,precip.1)  }
  precip = rbind(precip.1,precip)

 ## Change rows to columns according to type
 Temp = subset(Fct_Old, Variable == "TMP" & Forecast == "1 hour fcst")
 RH = subset(Fct_Old, Variable == "RH" & Forecast == "1 hour fcst")
 Wind = subset(Fct_Old, Variable == "WIND" & Forecast == "1 hour fcst")
 WDir = subset(Fct_Old, Variable == "WDIR" & Forecast == "1 hour fcst")
 TCDC = subset(Fct_Old, Variable == "TCDC" & Forecast == "1 hour fcst")
 t1 = merge(Temp,RH,by = c('Date','station_id'),all=TRUE)
 t2 = merge(t1,Wind,by = c('Date','station_id'),all=TRUE)
 t21 = data.frame(t2$Date,t2$station_id,t2$Long,t2$Lat,t2$Value.x,t2$Value.y,t2$Value)
 colnames(t21) = c("Date", "station_id","Long","Lat","tempC","rh","windKn")
 t3 = merge(t21,WDir,by = c('Date','station_id'),all=TRUE)
 t4 = merge(t3,TCDC,by = c('Date','station_id'),all=TRUE)
 Fct = data.frame(t4$Date,t4$station_id,t4$Long,t4$Lat,t4$tempC,t4$rh,t4$windKn,
 t4$Value.x,t4$Value.y)
 colnames(Fct) = c("datetime","station_id","lon","lat","tempK","rh","windKn",
 "wind_direction_deg","cloud_cover_percent")

 ## Convert forecast Date to date/time / increase time by 1 hour (match obs/forecast)
 fixDate = function(datetime) {
  time = paste(substr(datetime,1,4),"-",substr(datetime,5,6),"-",
  substr(datetime,7,8)," ",substr(datetime,9,10),sep="") }
 Fct$datetime = unlist(lapply(Fct$datetime,fixDate)) #Fix datetime
 Fct$datetime = strptime(Fct$datetime, "%Y-%m-%d %H",tz="UTC")
 Fct$datetime = Fct$datetime +3600  # add 1 hour to fore time to match obs time

 ## Convert Kelvin to celsius
 KtoC = function(tempK)  {
  tempC = tempK - 273.15
  return(tempC)  }
 Fct$tempK = unlist(lapply(Fct$tempK,KtoC)) 
 colnames(Fct)[5] = "air_temp_c"
   
 ## Convert wind speed from knots to mps
 Fct$windKn = Fct$windKn * 0.514444  
 colnames(Fct)[7] = "wind_speed_mps"

 ## Merge precip data & convert precip from inches to mm
 Fct = merge(Fct,precip,by = c('datetime','lon','lat'),all.x=TRUE,all.y=TRUE)
 Fct = Fct[-c(1,2), ]
 Fct$precip[is.na(Fct$precip)] = 0
 Fct$precip = Fct$precip * 25.4   
 colnames(Fct)[10] = "precip_mm"
   
 ## Add columns data_type, station_type
 Fct$data_type = "pred"
 Fct$station_type = "raws"

### Get LANDFIRE data attach to Forecast data
colnames(LandFire)[11]="station_id"
Fct = merge(Fct,LandFire,by=c('station_id'),all=TRUE)
Fct$station_id = as.character(Fct$station_id)
Fct = subset(Fct,!is.na(Fct$station_id))             #remove any NAs in station_id
colnames(Fct)[c(13,14)] = c("lon","lat")

### Convert 10 m wind to 20 ft; 10m wind in m/s, Canopy height needs to be in meters   
Wind10to20_mps = function(m10Wind,CanopyH,FuelMod)  {
 z.m = ifelse(FuelMod ==101|FuelMod==102|FuelMod==103|FuelMod==104|
 FuelMod==105|FuelMod==106|FuelMod==107|FuelMod==108|FuelMod==109|
 FuelMod==121|FuelMod==122|FuelMod==123|FuelMod==124,0.01,
 ifelse(FuelMod==141|FuelMod==142|FuelMod==143|FuelMod==144|FuelMod==145|
 FuelMod==146|FuelMod==147|FuelMod==148|FuelMod==149,0.43,1))
 d = 0.65*CanopyH
 u.star = (m10Wind*0.4) / log(((10+CanopyH) - d)/z.m)
 newWind = (u.star/0.4)*log(((6.1+CanopyH)-d)/z.m)
 return(newWind)  }
Fct$wind_speed20ft_mps = mapply(Wind10to20_mps,Fct$wind_speed_mps,Fct$CH_m,Fct$FM40)
      
### Convert 20ft wind to mid-flame wind (per Andrews 2012 and Finney 2004) 
FBD = c(0.4,1,2,2,1.5,1.5,3,4,5,0.9,1.5,1.8,2.1,1.0,1.0,2.4,3,6,2,6,3,4.4,0.6,1,1.3,.5,
1,.2,.2,.3,.4,.6,.3,.4,.3,.6,1,1,1.2,2.7)
FM = c(101,102,103,104,105,106,107,108,109,121,122,123,124,141,142,143,144,145,146,147,
148,149,161,162,163,164,165,181,182,183,184,185,186,187,188,189,201,202,203,204)
FBD_ft = data.frame(cbind(FM,FBD))
Wind20ft_Mid_mps = function(Wind20ft,FuelMod,CC,CanopyH)  {
 FH = grep(FuelMod,FBD_ft$FM)
 FH_ft = FBD_ft$FBD[FH]
 un.WAF = 1.83 / log((20+0.36*FH_ft)/(0.13*FH_ft))
 f = (CC/100)*(pi/12)
 sh.WAF = 0.555 / (sqrt(f*3.28*CanopyH) * log(20+(1.18*CanopyH)/(0.43*CanopyH)))
 WAF = ifelse(CC>5,sh.WAF,un.WAF)
 mid_wind = Wind20ft * WAF
 return(mid_wind)  }
Fct$wind_speedMid_mps = mapply(Wind20ft_Mid_mps,Fct$wind_speed20ft_mps,Fct$FM40,
Fct$CC_percent,Fct$CH_m)

### Clean up output / remove any duplicates
Fct = Fct[c("station_id","station_type","data_type","lon","lat","datetime",
"air_temp_c","rh","wind_speed20ft_mps","wind_speedMid_mps","wind_direction_deg",
"cloud_cover_percent","precip_mm","FM40","asp_deg","elev_m","slope_deg",
"CBD_kgm3","CBH_m","CC_percent","CH_m")]
 
 ## Remove any duplicated rows
 n.df = data.frame()
 stn = unique(Fct$station_id)
 for (i in 1:length(stn)) {
  try = subset(Fct, station_id == stn[i])
  t = subset(try,!duplicated(try$datetime))
  n.df = rbind(t,n.df) }
 Fct = n.df

write.csv(Fct,file=files[j])

#Fct$datetime = as.character(Fct$datetime)
#out = rbind.fill(Fct,asos.obs)

### End for loop for all files
assign(files[j],Fct) }

