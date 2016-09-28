#############  MERGE ALL WEATHER DATA  #######

### Prepare ASOS observed data
 ## Use QC data set / set Working Directory
 setwd("/media/wpage/Elements/Page/NDFD_Project/Weather/ASOS")

 ## Read-in QC obs data
 asos.obs = read.csv("Obs_mod/asos2015_final.csv",stringsAsFactors=FALSE)
 asos.obs$X = NULL
 asos.obs$datetime = as.character(asos.obs$datetime)
 asos.obs$datetime = strptime(asos.obs$datetime, "%Y-%m-%d %H:%M:%S",tz="UTC")

 ## Read-in dead FM
 asos.obs.dfm = read.csv("nelson_fms/deadfms_asos_obs.csv",stringsAsFactors=FALSE)
 asos.obs.dfm$X = NULL
 asos.obs.dfm$datetime = as.character(asos.obs.dfm$datetime)
 asos.obs.dfm$datetime = strptime(asos.obs.dfm$datetime,"%m/%d/%Y %H:%M",tz="UTC")

 ## Merge QC data with dfm
 asos = merge(asos.obs,asos.obs.dfm,by=c("station_id","datetime"))

 ## Add live fuel moisture info
 asos$LiveHerb_frac_percent = 1.20
 asos$LiveWood_frac_percent = 0.60

 ## Remove duplicated rows
 final = data.frame()
 stn = unique(asos$station_id)
 for(i in 1:length(stn)) {
  temp = subset(asos, station_id ==stn[i])
  temp = temp[!duplicated(temp$datetime),]
  final = rbind(temp,final) }

### Prepare ASOS forecast data
 ## Read-in QC obs data
 asos.pred = read.csv("NDFD_forecast_mod/asos2015pred_final.csv",
 stringsAsFactors=FALSE)
 asos.pred$X = NULL
 asos.pred$datetime = as.character(asos.pred$datetime)
 asos.pred$datetime = strptime(asos.pred$datetime, "%Y-%m-%d %H:%M:%S",tz="UTC")
 asos.pred = na.omit(asos.pred)  ## make sure no na's in forecast

 ## Read-in dead FM
 asos.pred.dfm = read.csv("nelson_fms/deadfms_asos_pred.csv",stringsAsFactors=FALSE)
 asos.pred.dfm$X = NULL
 asos.pred.dfm$datetime = as.character(asos.pred.dfm$datetime)
 asos.pred.dfm$datetime = strptime(asos.pred.dfm$datetime,"%m/%d/%Y %H:%M",tz="UTC")

 ## Merge QC data with dfm
 asos2 = merge(asos.pred,asos.pred.dfm,by=c("station_id","datetime"))

 ## Add live fuel moisture info
 asos2$LiveHerb_frac_percent = 1.20
 asos2$LiveWood_frac_percent = 0.60

 ## Remove duplicated rows
 final2 = data.frame()
 stn = unique(asos2$station_id)
 for(i in 1:length(stn)) {
  temp = subset(asos2, station_id ==stn[i])
  temp = temp[!duplicated(temp$datetime),]
  final2 = rbind(temp,final2) }

### Merge the ASOS forecast and observed data
 ## Combine and order the data frames
 asos.c = rbind(final,final2)
 asos.c = asos.c[with(asos.c,order(station_id,datetime,data_type)),]

 ## Keep rows that have both observed and forecast info (for same time and station)
 temp1 = asos.c[duplicated(asos.c[1:2]),]
 temp2 = asos.c[duplicated(asos.c[1:2],fromLast=TRUE),]
 asos_final = rbind(temp1,temp2)
 asos_final = asos_final[with(asos_final,order(station_id,datetime,data_type)),]

 ## Save combined ASOS data frame
 write.csv(asos_final,file="asos_final.csv",row.names=FALSE)


### Prepare RAWS observed data
 ## Use QC data set / set Working Directory
 setwd("/media/wpage/Elements/Page/NDFD_Project/Weather/RAWS")

 ## Read-in QC obs data
 raws.obs = read.csv("Obs_mod/raws2015_final.csv",stringsAsFactors=F)
 raws.obs$X = NULL
 raws.obs$datetime = as.character(raws.obs$datetime)
 raws.obs$datetime = strptime(raws.obs$datetime, "%Y-%m-%d %H:%M:%S",tz="UTC")

 ## Read-in dead FM
 raws.obs.dfm = read.csv("nelson_fms/deadfms_raws_obs.csv",stringsAsFactors=F)
 raws.obs.dfm$datetime = as.character(raws.obs.dfm$datetime)
 raws.obs.dfm$datetime = strptime(raws.obs.dfm$datetime,"%m/%d/%Y %H:%M",tz="UTC")

 ## Merge QC data with dfm
 raws = merge(raws.obs,raws.obs.dfm,by=c("station_id","datetime"))

 ## Add live fuel moisture info
 raws$LiveHerb_frac_percent = 1.20
 raws$LiveWood_frac_percent = 0.60

 ## Remove duplicated rows
 final3 = data.frame()
 stn = unique(raws$station_id)
 for(i in 1:length(stn)) {
  temp = subset(raws, station_id ==stn[i])
  temp = temp[!duplicated(temp$datetime),]
  final3 = rbind(temp,final3) }

final3$cloud_cover_percent = NA
final3 = final3[c("station_id","station_type","data_type","lon","lat","datetime",
 "air_temp_c","rh","wind_speed20ft_mps","wind_speedMid_mps","wind_direction_deg",
 "cloud_cover_percent","precip_mm","solar_wm2","FM40","asp_deg","elev_m","slope_deg",
 "CBD_kgm3","CBH_m","CC_percent","CH_m","X1hrfm","X10hrfm","X100hrfm",
 "LiveHerb_frac_percent","LiveWood_frac_percent")]

write.csv(final3,file="raws_obs_temp.csv",row.names=F)

### Prepare RAWS forecast data
 ## Use QC data set / set Working Directory
 setwd("/media/wpage/Elements/Page/NDFD_Project/Weather/RAWS")

 ## Read-in QC obs data
 raws.pred = read.csv("NDFD_Forecast_mod/raws2015pred_final.csv",stringsAsFactors=F)
 raws.pred$X = NULL
 raws.pred$datetime = as.character(raws.pred$datetime)
 raws.pred$datetime = strptime(raws.pred$datetime, "%Y-%m-%d %H:%M:%S",tz="UTC")

 ## Read-in dead FM
 raws.pred.dfm = read.csv("nelson_fms/deadfms_raws_pred.csv",stringsAsFactors=F)
 raws.pred.dfm$datetime = as.character(raws.pred.dfm$datetime)
 raws.pred.dfm$datetime = strptime(raws.pred.dfm$datetime,"%m/%d/%Y %H:%M",tz="UTC")

 ## Merge QC data with dfm
 raws2 = merge(raws.pred,raws.pred.dfm,by=c("station_id","datetime"))

 ## Add live fuel moisture info
 raws2$LiveHerb_frac_percent = 1.20
 raws2$LiveWood_frac_percent = 0.60

 ## Remove duplicated rows
 final4 = data.frame()
 stn = unique(raws2$station_id)
 for(i in 1:length(stn)) {
  temp = subset(raws2, station_id ==stn[i])
  temp = temp[!duplicated(temp$datetime),]
  final4 = rbind(temp,final4) }

final4 = final4[c("station_id","station_type","data_type","lon","lat","datetime",
 "air_temp_c","rh","wind_speed20ft_mps","wind_speedMid_mps","wind_direction_deg",
 "cloud_cover_percent","precip_mm","solar_wm2","FM40","asp_deg","elev_m","slope_deg",
 "CBD_kgm3","CBH_m","CC_percent","CH_m","X1hrfm","X10hrfm","X100hrfm",
 "LiveHerb_frac_percent","LiveWood_frac_percent")]

### Merge the RAWS forecast and observed data

 ## Combine and order the data frames
 raws.c = rbind(final3,final4)
 raws.c = raws.c[with(raws.c,order(station_id,datetime,data_type)),]

 ## Keep rows that have both observed and forecast info (for same time and station)
 temp1 = raws.c[duplicated(raws.c[1:2]),]
 temp2 = raws.c[duplicated(raws.c[1:2],fromLast=TRUE),]
 raws_final = rbind(temp1,temp2)
 raws_final = raws_final[with(raws_final,order(station_id,datetime,data_type)),]

 ## Save combined RAWS data frame
 write.csv(raws_final,file="raws_final.csv",row.names=FALSE)

