#############  COMPUTE FIRE BEHAVIOR - behave

### Start with ASOS data
 ## Set Working Directory
 setwd("/media/wpage/Elements/Page/NDFD_Project/Weather/ASOS")
 
 ## Read-in data
 asos.data = read.csv("asos_final2.csv",stringsAsFactors=FALSE)
 asos.data$X = NULL
 asos.data$X100hrfm = as.numeric(asos.data$X100hrfm)

 ## Create input file to read into behave-cli
 run = cbind(asos.data$station_id,asos.data$datetime,asos.data$data_type,asos.data$FM40,(asos.data$X1hrfm*100),(asos.data$X10hrfm*100),(asos.data$X100hrfm*100),(asos.data$LiveHerb_frac_percent*100),(asos.data$LiveWood_frac_percent*100),asos.data$wind_speedMid_mps,asos.data$wind_direction_deg,asos.data$slope_deg,asos.data$asp_deg)

 ## Save input file
 setwd("/home/wpage/Documents/behave/build")  
 filename = paste("behave","_asos_input",".txt",sep="")
 write.table(run,file=filename,sep=",",col.names=FALSE,row.names=FALSE,quote=FALSE)

 ## Read in output & fix colnames
 setwd("/media/wpage/Elements/Page/NDFD_Project/Weather/ASOS/behave")
 behave.data = read.csv("asos_output.csv",stringsAsFactors=FALSE)
 colnames(behave.data)[1] = "station_id"
 colnames(behave.data)[2] = "datetime"
 colnames(behave.data)[3] = "data_type"
 colnames(behave.data)[4] = "ROS_mps"
 colnames(behave.data)[5] = "flame_length_m"

 ## Merge output with data
 library(RSQLite)
 db = dbConnect(SQLite(),dbname="asos_final.sqlite")
 dbWriteTable(db,"asos_data",asos.data,row.names=FALSE) 
 dbWriteTable(db,"behave_data",behave.data,row.names=FALSE)
 merge.asos = dbGetQuery(db,"SELECT * FROM asos_data JOIN behave_data USING (station_id, datetime, data_type)")
 dbDisconnect(db)

 ## Save output
 setwd("/media/wpage/Elements/Page/NDFD_Project/Weather/ASOS")
 write.csv(merge.asos,file="asos_final3.csv",row.names=FALSE)


### Work with RAWS data
 ## Set Working Directory
 setwd("/media/wpage/Elements/Page/NDFD_Project/Weather/RAWS")
 
 ## Read-in data
 raws.data = read.csv("raws_final2.csv",stringsAsFactors=FALSE)
 raws.data$X = NULL
 raws.data$X100hrfm = as.numeric(raws.data$X100hrfm)
 raws.data$wind_direction_deg = replace(raws.data$wind_direction_deg,grep(TRUE,is.na(raws.data$wind_direction_deg)),0)

 ## Create input file to read into behave-cli
 run = cbind(raws.data$station_id,raws.data$datetime,raws.data$data_type,raws.data$FM40,(raws.data$X1hrfm*100),(raws.data$X10hrfm*100),(raws.data$X100hrfm*100),(raws.data$LiveHerb_frac_percent*100),(raws.data$LiveWood_frac_percent*100),raws.data$wind_speedMid_mps,raws.data$wind_direction_deg,raws.data$slope_deg,raws.data$asp_deg)

 ## Save input file
 setwd("/home/wpage/Documents/behave/build")  
 filename = paste("behave","_raws_input",".txt",sep="")
 write.table(run,file=filename,sep=",",col.names=FALSE,row.names=FALSE,quote=FALSE)

 ## Read in output & fix colnames
 setwd("/media/wpage/Elements/Page/NDFD_Project/Weather/RAWS/behave")
 behave.data = read.csv("raws_output.csv",stringsAsFactors=FALSE)
 colnames(behave.data)[1] = "station_id"
 colnames(behave.data)[2] = "datetime"
 colnames(behave.data)[3] = "data_type"
 colnames(behave.data)[4] = "ROS_mps"
 colnames(behave.data)[5] = "flame_length_m"

 ## Merge output with data
 library(RSQLite)
 db = dbConnect(SQLite(),dbname="raws_final.sqlite")
 dbWriteTable(db,"raws_data",raws.data,row.names=FALSE) 
 dbWriteTable(db,"behave_data",behave.data,row.names=FALSE)
 merge.raws = dbGetQuery(db,"SELECT * FROM raws_data JOIN behave_data USING (station_id, datetime, data_type)")
 dbDisconnect(db)

 ## Save output
 setwd("/media/wpage/Elements/Page/NDFD_Project/Weather/RAWS")
 write.csv(merge.raws,file="raws_final3.csv",row.names=FALSE)
 
