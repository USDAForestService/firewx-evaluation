#############  ASOS DATA - EXTRACT LANDFIRE / FIX FORMATTING  ############

### Set needed packages
library(raster) 
library(rgdal)
library(sp)

### Set Working Directory
setwd("C:/Users/wesleygpage/Desktop/NDFD_Project/Weather/ASOS_raw")

### Read-in files
files = list.files()
LandFire = read.csv("C:/Users/wesleygpage/Desktop/NDFD_Project/Weather/Misc/landfire_asos.csv")
LandFire$X = NULL

### Create vector of file names
name = vector()
for (i in 2:length(files)) {
  name[i-1] = gsub("2015.txt","",files[i])  }
name[15] = "Kentucky"

### Read-in observed weather data
for (i in 2:length(files))  {
  temp = read.delim(files[i],skip=5)
  assign(name[i-1],temp)  }

### Remove those observations from stations w/ non-burnable fuel model
 ## Determine which lat/longs are non-burnable and exclude
 noburn.fm = c(-9999,91,92,93,98,99) 
 good.loc = subset(LandFire, !(FM40 %in% noburn.fm))
 good.loc = data.frame(good.loc$lon,good.loc$lat)
 
 ## Only keep obs from good lat/longs
 for (i in 1:length(name)) {
 temp = subset(get(name[i]), (lon %in% good.loc$good.loc.lon))
 assign(name[i],temp) }

for (j in 1:length(name)) {
### Fix format issues with ASOS data
 ## Remove other observations within the same hour
 temp = get(name[j])
 temp$station = as.character(temp$station)
 temp$valid = as.character(temp$valid)
 temp$valid = strptime(temp$valid, "%Y-%m-%d %H:%M",tz="UTC")
 stn = unique(temp$station)
 n.df = data.frame()
  for (i in 1:length(stn)) {
   try = subset(temp,station==stn[i])
   times = unique(round(try$valid,units="hours"))
   ti = lapply(as.POSIXct(times), `-`, as.POSIXct(try$valid))
   ti = unlist(lapply(ti,function(x)which(abs(x)==min(abs(x)))))
   try = try[ti,]
   try$valid = round(try$valid,units="hours") 
   n.df = rbind(n.df,try) }
 temp = n.df 

 ## Drop unused columns
 drops = c("dwpc","mslp","gust","skyc2","skyc3","skyl1","skyl2","skyl3")
 temp  = temp[,!(names(temp) %in% drops)]
 colnames(temp) = c("station_id","datetime","lon","lat","air_temp_c","rh",
 "wind_direction_deg","windKn","precip","cloud")

 ## Convert class to numeric
 temp[,c("air_temp_c","rh","wind_direction_deg","windKn")] = 
 as.numeric(as.character(unlist(temp[,c("air_temp_c","rh","wind_direction_deg","windKn")])))

 ## Convert sky cover to percent
 temp$cloud = as.character(temp$cloud)
 for (i in 1:length(temp$cloud)) {
  cld = ifelse(temp$cloud[i]=="CLR",0,ifelse(temp$cloud[i]=="FEW",19,ifelse(temp$cloud[i]=="BKN",75,
  ifelse(temp$cloud[i]=="OVC",100,ifelse(temp$cloud[i]=="SCT",44,ifelse(temp$cloud[i]=="VV",100,0))))))
  temp$cloud[i] = cld }  
 colnames(temp)[10] = "cloud_cover_percent"
 
 ## Convert wind speed from knots to mps
 temp$windKn = temp$windKn * 0.514444  
 colnames(temp)[8] = "wind_speed10m_mps"

 ## Convert precip from inches to mm
 temp$precip = temp$precip * 25.4  
 colnames(temp)[9] = "precip_mm" 
 
 ## Add columns data_type, station_type
   temp$data_type = "obs"
   temp$station_type = "asos"  
 
### Add Landfire data 
Land.fire = subset(LandFire, !(FM40 %in% noburn.fm))
temp = merge(temp,Land.fire,by=c("lon","lat")) 

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
temp$wind_speed20ft_mps = mapply(Wind10to20_mps,temp$wind_speed10m_mps,temp$CH_m,temp$FM40) 
     
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
temp$wind_speedMid_mps = mapply(Wind20ft_Mid_mps,temp$wind_speed20ft_mps,temp$FM40,temp$CC_percent,temp$CH_m)

### Clean up output
temp$wind_speed10m_mps = NULL  
temp = temp[c("station_id","station_type","data_type","lon","lat","datetime",
"air_temp_c","rh","wind_speed20ft_mps","wind_speedMid_mps","wind_direction_deg",
"cloud_cover_percent","precip_mm","FM40","asp_deg","elev_m","slope_deg",
"CBD_kgm3","CBH_m","CC_percent","CH_m")]
 
 ## Remove any duplicated rows
 n.df = data.frame()
 for (i in 1:length(stn)) {
  try = subset(temp, station_id == stn[i])
  t = subset(try,!duplicated(try$datetime))
  n.df = rbind(t,n.df) }
 temp = n.df

### Export file out
out.file = paste(name[j],".csv",sep="")
write.csv(temp,file=out.file)

### End for loop for all files
assign(name[j],temp) }




