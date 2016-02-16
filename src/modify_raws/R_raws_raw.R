#############  RAWS DATA - EXTRACT LANDFIRE / FIX FORMATTING  ############

### Set needed packages
library(Hmisc) 
library(data.table)

### Set Working Directory
setwd("/home/wpage/Documents/RAWS")

### Read-in 2015 RAWS data
data = read.csv("wrcc_final_2015.csv")
data$Stn = as.character(data$Stn)

### Get RAWS lat/long 
loc = read.csv("raws_longs.csv")
stn.id = read.csv("raws_station_id.csv")
stn.id$WRCC_ID = tolower(stn.id$WRCC_ID)
colnames(stn.id)[2] = "lat"
colnames(stn.id)[3] = "lon"
sub = merge(loc,stn.id,by=c('lon','lat'))

### Only keep obs from good lat/longs
temp = subset(data, (Stn %in% sub$WRCC_ID))

### Drop unused columns
drops = c("Tzone","lst","bv","gust_d","gust_s","f_temp")
temp  = temp[,!(names(temp) %in% drops)]
colnames(temp) = c("station_id","lat","lon","datetime","rgauge","wind_speed_mph",
"wind_direction_deg","air_temp_f","rh","solar_wm2")
temp$solar_wm2 = as.numeric(as.character(temp$solar_wm2))

### Format time
temp$datetime = as.character(temp$datetime)
temp$datetime = strptime(temp$datetime, "%m/%d/%Y %H:%M:%S",tz="GMT")

### Convert wind speed from mph to mps
temp$wind_speed_mph = temp$wind_speed_mph * 0.44704  
colnames(temp)[6] = "wind_speed20ft_mps"

### Convert air temp farenheit to celcius
temp$air_temp_f = ((temp$air_temp_f - 32) * (5/9))
colnames(temp)[8] = "air_temp_c"

### Fix precip
temp = temp[with(temp,order(station_id,datetime)),]   # order by station_id & datetime
precip = diff(temp$rgauge,1)                      # subtract value from next hour 
precip_in = append(precip,0,after=1)              # increase index by 1            
fix = grep("TRUE",precip_in<0)                    # find negatives
for (i in 1:length(fix)) {                        # set negatives to 0
 precip_in[fix[i]] = 0 }
fix = grep("TRUE",precip_in>50)                   # find high values
for (i in 1:length(fix)) {                        # set high values to 0
 precip_in[fix[i]] = 0 }
temp = cbind(temp,precip_in)
   
### Convert precip from inches to mm
temp$precip_in = temp$precip_in * 25.4  
colnames(temp)[11] = "precip_mm" 
temp$rgauge = NULL

### Add columns data_type, station_type
temp$data_type = "obs"
temp$station_type = "raws"  
 
### Add Landfire data 
LandFire = read.csv("landfire_raws.csv")
n.loc = merge(LandFire,sub,by=c("lon","lat"),all.x=TRUE)
colnames(n.loc)[11] = "station_id"  
work = merge(temp,n.loc,by=c("station_id"),all.x=TRUE)

### Clean up merge
work = work[c("station_id","station_type","data_type","lon.y","lat.y","datetime", "air_temp_c","rh","wind_speed20ft_mps","wind_direction_deg","precip_mm","solar_wm2",
"FM40","asp_deg","elev_m","slope_deg","CBD_kgm3","CBH_m","CC_percent","CH_m")]
colnames(work)[4] = "lon"
colnames(work)[5] = "lat"

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
work$wind_speedMid_mps = mapply(Wind20ft_Mid_mps,work$wind_speed20ft_mps,work$FM40,work$CC_percent,work$CH_m)

## Clean up output  
work = work[c("station_id","station_type","data_type","lon","lat","datetime",
"air_temp_c","rh","wind_speed20ft_mps","wind_speedMid_mps","wind_direction_deg",
"precip_mm","solar_wm2","FM40","asp_deg","elev_m","slope_deg","CBD_kgm3","CBH_m",
"CC_percent","CH_m")]

### Export file out
write.csv(work,file="raws2015_mod.csv")





