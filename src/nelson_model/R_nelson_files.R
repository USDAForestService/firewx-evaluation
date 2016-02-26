#############  CREATE FILES TO RUN NELSON FMS

### Start with RAWS observed data
 ## Use QC data set / set Working Directory
 setwd("/home/wpage/Documents/RAWS/nelson_fms/obs")

 ## Read-in QC obs data
 data = read.csv("/home/wpage/Documents/RAWS/raws2015_final.csv")
 data$X = NULL
 data$datetime = as.character(data$datetime)

 ## Note discrepency between compute_dfm required input for air humidity / The
 ## compute_dfm function wants humidity values with (g/g) units which the following
 ## function calculates, however these values are outside the range of input required
 ## (0.001 to 1), for now we will use fractional rh for this input (2/8/2016) 
 ## Create function to calculate mixing ratio (g/g)
 mixRatio_gg = function(air_temp_c,rh,elev_m) {
  # Calculate Dew Point (C) andrew.rsmas.miami.edu/bmcnoldy/Humidity.html
  dew = 243.04*(log(rh/100)+((17.625*air_temp_c)/(243.04+air_temp_c)))/
  (17.625-log(rh/100)-((17.625*air_temp_c)/(243.04+air_temp_c))) 
  # Calculate air pressure (millibars) en.wikipedia.org/wiki/Atmospheric_pressure 
  press.pa = 101325*(1-((0.0065*elev_m)/288.15))^((9.80665*0.0289644)/
  (8.31447*0.0065))
  press.milli = press.pa * 0.01
  # Calc mixing ratio (g/g) www.srh.noaa.gov/epz/wxcalc/vaporPressure.pdf
  # www.srh.noaa.gov/images/epz/wxcalc/mixingRatio.pdf
  e = 6.11 * 10^((7.5*dew)/(237.3+dew))
  mix = 621.97 * (e/(press.milli-e))
  mix_gg = mix/1000
  return(mix_gg) }

 ## Create FMS files for each station using for loop
 stn = unique(data$station_id)      #get unique station info
 for (i in 1:length(stn)) {
  temp = subset(data,station_id==stn[i])
  
  # Remove any row with a NA in it 
  temp = temp[complete.cases(temp$air_temp_c),]
  temp = temp[complete.cases(temp$rh),]

  # Sort data by datetime
  temp = temp[with(temp,order(datetime)),]
  
  # Break up datetime
  temp$year = 2015
  for (j in 1:length(temp$datetime)) {
   temp$month[j] = unlist(strsplit(temp$datetime[j],"[-: ]"))[2]
   temp$day[j] = unlist(strsplit(temp$datetime[j],"[-: ]"))[3]
   temp$hour[j] = unlist(strsplit(temp$datetime[j],"[-: ]"))[4]
   temp$min[j] = unlist(strsplit(temp$datetime[j],"[-: ]"))[5]
   temp$sec[j] = unlist(strsplit(temp$datetime[j],"[-: ]"))[6] }
  temp$milsec = 0

  # Make necessary unit conversions
  temp$airhumidity = temp$rh/100
  #temp$airhumidity_gg = mapply(mixRatio_gg,temp$air_temp_c,temp$rh,temp$elev_m)
  temp$precip_cm = temp$precip_mm * 0.1
  for (j in 1:length(temp$year)) {
   temp$stickT[j] = ""
   temp$stickHum[j] = ""
   temp$moisture[j] = "" }

  # Create output file
  out = data.frame(cbind(temp$year,temp$month,temp$day,temp$hour,temp$min,temp$sec,
  temp$milsec,temp$air_temp_c,temp$airhumidity,temp$solar_wm2,temp$precip_cm,
  temp$stickT,temp$stickHum,temp$moisture))

  # Make sure order is good
  out = out[with(out,order(X2,X3,X4)),]

  # Create starting values
  out$X12 = as.character(out$X12)
  out$X13 = as.character(out$X13)
  out$X14 = as.character(out$X14)
  out$X12[1] = "20"
  out$X13[1] = "0.006"
  out$X14[1] = "0.05"

  # Save output
  filename = paste(stn[i],"_asos_input",".txt",sep="")
  filename.out = paste(stn[i],"_asos_out",".txt",sep="")
  X = data.frame()
  write.table(out,file=filename,sep=" ",col.names=FALSE,row.names=FALSE,quote=FALSE)
  write.table(X,file=filename.out,sep=" ",col.names=FALSE,row.names=FALSE) }
  

### Get the ASOS observed data
 ## Use QC data set / set Working Directory
 setwd("/home/wpage/Documents/ASOS/nelson_fms/obs")

 ## Read-in data
 data = read.csv("/home/wpage/Documents/ASOS/asos2015_final.csv")
 data$X = NULL
 data$datetime = as.character(data$datetime)

 ## Re-run the above for loop based on new data frame


### Get the ASOS forecast data
 ## Use QC data set / set Working Directory
 setwd("/home/wpage/Documents/ASOS/nelson_fms/forecast")

 ## Read-in data
 data = read.csv("/Elements/Page/NDFD_Project/Weather/ASOS/asos2015pred_final.csv")
 data$X = NULL
 data$datetime = as.character(data$datetime)

 ## Re-run the above for loop based on new data frame


### Get the RAWS forecast data
 ## Use QC data set / set Working Directory
 setwd("/home/wpage/Documents/RAWS/nelson_fms/forecast")

 ## Read-in data
 data = read.csv("/Elements/Page/NDFD_Project/Weather/RAWS/raws2015pred_final.csv")
 data$X = NULL
 data$datetime = as.character(data$datetime)

 ## Re-run the above for loop based on new data frame

