#############  FINISH SOLAR/FM/MERGING RAWS DATA

### STEP #1 / Finish solar radiation
 ## Function to correct max solar to station solar
 cloudFun = function(cloud_percent,maxsolar) {
  try1 = ifelse(cloud_percent < 10, (0.93*maxsolar),ifelse(cloud_percent >= 10&
  (cloud_percent < 50), (0.8*maxsolar),ifelse((cloud_percent >= 50) & 
  (cloud_percent <90),(0.63*maxsolar),ifelse(cloud_percent >= 90, 
  (0.25*maxsolar),"error"))))
  return(try1)  }

 ## Add solarMax to dataframe
 data = cbind(data,solarMax_wm2)

 ## Run solar correction function
 solar_wm2 = mapply(cloudFun,data$cloud_cover_percent,data$solarMax_wm2)
 data = cbind(data,solar_wm2)

 ## Fix the output order to match other data
 data = data[c("station_id","station_type","data_type","lon","lat","datetime",
 "air_temp_c","rh","wind_speed20ft_mps","wind_speedMid_mps","wind_direction_deg",
"cloud_cover_percent","precip_mm","solar_wm2","FM40","asp_deg","elev_m","slope_deg",
 "CBD_kgm3","CBH_m","CC_percent","CH_m")]

 ## Save final output
 setwd("/media/wpage/Elements/Page/NDFD_Project/Weather/RAWS/NDFD_Forecast_mod")
 write.csv(data,file="raws2015pred_final.csv")


### STEP #2 / Finish fuel moisture / create files
setwd("/media/wpage/Elements/Page/NDFD_Project/Weather/RAWS/nelson_fms/forecast")

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
  filename = paste(stn[i],"_raws_input",".txt",sep="")
  filename.out = paste(stn[i],"_raws_out",".txt",sep="")
  X = data.frame()
  write.table(out,file=filename,sep=" ",col.names=FALSE,row.names=FALSE,quote=FALSE)
  write.table(X,file=filename.out,sep=" ",col.names=FALSE,row.names=FALSE) }



### STEP #3 / Finish fuel moisture / run files
### Move copy of output files to /home/wpage/Documents/firewx-evaluation/build
move.files = "cd /media/wpage/Elements/Page/NDFD_Project/Weather/RAWS/nelson_fms/forecast && cp * /home/wpage/Documents/firewx-evaluation/src/nelson_model/build"
system(move.files)

setwd("/media/wpage/Elements/Page/NDFD_Project/Weather/RAWS/nelson_fms/forecast")

 ## Read-in prepared txt files (see R_nelson_files script)
 files = list.files()
 input.loc = grep("input",files)
 
 ## Run the Nelson dfm program / Start for loop for each input file
 for (i in 1:length(input.loc)) { 
  # Get input file name
  input = files[input.loc[i]] 
  
  # Make output file name (just to make sure)
  out.name = unlist(strsplit(input,"[_]"))
  out = paste(out.name[1],"_",out.name[2],"_","out.txt",sep="")

  # Build call
  changedir = "cd /home/wpage/Documents/firewx-evaluation/src/nelson_model/build && "
  dfm = paste(changedir,"./compute_dfm ","--input_file ",input,
  " --output_file ",out,sep="")

  # Run the program
  run = system(dfm)   }

 ## Extract output from Nelson / Save output
 dir = "/home/wpage/Documents/firewx-evaluation/src/nelson_model/build/"
 out.loc = grep("out",files)
 dead.fms = data.frame()
 for (i in 1:length(out.loc)) {
  output = files[out.loc[i]]
  out.name = unlist(strsplit(output,"[_]"))[1]
  fms = read.csv(paste(dir,output,sep=""),header=TRUE)
  fms$datetime = paste(fms$month,"/",fms$day,"/",fms$year," ",fms$hour,":","00",sep="")
  fms$station_id = out.name
  drops = c("year","month","day","hour")
  fms = fms[,!(names(fms) %in% drops)]
  dead.fms = rbind(fms,dead.fms) }
 
 ## Save the output for later
 write.csv(dead.fms,file="/media/wpage/Elements/Page/NDFD_Project/Weather/RAWS/nelson_fms/deadfms_raws_pred.csv")



### STEP #4 / Merge the raws data

 ## Read-in dead FM
 raws.pred.dfm = read.csv("/media/wpage/Elements/Page/NDFD_Project/Weather/RAWS/nelson_fms/deadfms_raws_pred.csv",stringsAsFactors=F)
 raws.pred.dfm$datetime = as.character(raws.pred.dfm$datetime)
 raws.pred.dfm$datetime = strptime(raws.pred.dfm$datetime,"%m/%d/%Y %H:%M",tz="UTC")
 raws.pred.dfm = raws.pred.dfm[with(raws.pred.dfm,order(station_id,datetime)),]

 ## Add original RAWS pred data
 data = read.csv("/media/wpage/Elements/Page/NDFD_Project/Weather/RAWS/NDFD_Forecast_mod/raws2015pred_final.csv",stringsAsFactors=F)
 data$datetime = as.character(data$datetime)
 data$datetime = strptime(data$datetime, "%Y-%m-%d %H:%M:%S",tz="UTC")
 
 ## Break-up datasets
 stn1 = unique(data$station_id)[1:460]
 stn2 = unique(data$station_id)[461:920]
 stn3 = unique(data$station_id)[921:length(raws.pred.dfm$station_id)]
 
 dfm1 = raws.pred.dfm[raws.pred.dfm$station_id %in% stn1,]
 data1 = data[data$station_id %in% stn1,]
 
 dfm2 = raws.pred.dfm[raws.pred.dfm$station_id %in% stn2,]
 data2 = data[data$station_id %in% stn2,]

 dfm3 = raws.pred.dfm[raws.pred.dfm$station_id %in% stn3,]
 data3 = data[data$station_id %in% stn3,]

 ## Merge QC data with dfm
 raws2_1 = merge(data1,dfm1,by=c("station_id","datetime"))
 raws2_2 = merge(data2,dfm2,by=c("station_id","datetime"))
 raws2_3 = merge(data3,dfm3,by=c("station_id","datetime"))

 ## Clean up memory
 rm(data)
 rm(raws.pred.dfm)
 rm(data1)
 rm(data2)
 rm(data3)
 rm(dfm1)
 rm(dfm2)
 rm(dfm3)
 gc()

 ## Combine all data frames
 raws2 = rbind(raws2_1,raws2_2,raws2_3)

 ## Clean up memory
 rm(raws2_1)
 rm(raws2_2)
 rm(raws2_3)
 gc()

 ## Add live fuel moisture info
 raws2$LiveHerb_frac_percent = 1.20
 raws2$LiveWood_frac_percent = 0.60

 ## Organize data
raws2 = raws2[c("station_id","station_type","data_type","lon","lat","datetime",
 "air_temp_c","rh","wind_speed20ft_mps","wind_speedMid_mps","wind_direction_deg",
"cloud_cover_percent","precip_mm","solar_wm2","FM40","asp_deg","elev_m","slope_deg",
 "CBD_kgm3","CBH_m","CC_percent","CH_m","X1hrfm","X10hrfm","X100hrfm",
 "LiveHerb_frac_percent","LiveWood_frac_percent")]

 ## Save the output for later
 write.csv(raws2,file="/media/wpage/Elements/Page/NDFD_Project/Weather/RAWS/raws_pred_temp.csv") 

 ## Read-in formatted raws data
 final1 = read.csv("/media/wpage/Elements/Page/NDFD_Project/Weather/RAWS/raws_pred_temp.csv")
 final1 = final1[,-c(1)]
 final3 = read.csv("/media/wpage/Elements/Page/NDFD_Project/Weather/RAWS/raws_obs_temp.csv")

 ## Merge two datasets using SQLite
 library(RSQLite)
 db = dbConnect(SQLite(),dbname="raws_final.sqlite")
 dbWriteTable(db,"raws_pred",final1,row.names=FALSE) 
 dbWriteTable(db,"raws_obs",final3,row.names=FALSE)
 merge.raws = dbGetQuery(db,"SELECT * FROM raws_pred UNION SELECT * FROM raws_obs")

 ## Order data
 merge.raws = merge.raws[with(merge.raws,order(station_id,datetime,data_type)),]

 ## Keep rows that have both observed and forecast info (for same time and  station)
 temp1 = merge.raws[duplicated(merge.raws[c(1,6)]),]
 temp2 = merge.raws[duplicated(merge.raws[c(1,6)],fromLast=TRUE),]
 
 dbWriteTable(db,"temp1",temp1,row.names=FALSE) 
 dbWriteTable(db,"temp2",temp2,row.names=FALSE)

 raws_final = dbGetQuery(db,"SELECT * FROM temp1 UNION SELECT * FROM temp2")
 dbDisconnect(db)
 
 ## Save combined raws data frame
 raws_final = raws_final[with(raws_final,order(station_id,datetime,data_type)),]

 setwd("/media/wpage/Elements/Page/NDFD_Project/Weather/RAWS")
 write.csv(raws_final,file="raws_final.csv",row.names=FALSE)



 

