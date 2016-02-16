###### This will extract the forecast for six variables for each day
###### in months Jan-June. This corresponds to the months on the 'My Passport' drive

## Define variable Year/Month/Day and base path name
Month = c("01","02","03","04","05","06")
blabel = "/media/wpage/My Passport/NDFD/nomads.ncdc.noaa.gov/NDFD/"
Year = "2015"
Day = as.character(seq(1,31,1))
Day[1:9] = c("01","02","03","04","05","06","07","08","09")
Variables = c("YEUZ98_KWBN_","YAUZ98_KWBN_","YRUZ98_KWBN_","YBUZ98_KWBN_",
"YCUZ98_KWBN_","YIUZ98_KWBN_")

### Import/Create locations
Location = read.csv("/home/wpage/Documents/raws_longs.csv")
Location$X = NULL
Locations = as.matrix(Location)
#Locations = Location[1:(length(Location[,1])/2),]
#Locations = Location[((length(Location[,1])/2)+1):(length(Location[,1])),]
#Matrix of long and lats
#Locations = matrix(c(-112.4975,45.9548,-112.5525,45.2554,-114.2636,48.3042,
#-110.45,45.7,-114.0925,46.9208),ncol=2,byrow=T)

## For loop for month
for (m in 1:length(Month)) {

## Set working directory
wd = paste(blabel,Year,Month[m],"/",sep="")
setwd(wd)

# Read in all files for month
files = list.files(".",recursive = TRUE,full.names=FALSE) 

#Master Output
M.out = data.frame(Date=NA,Variable=NA,Forecast=NA,Long=NA,Lat=NA,Value=NA)

### For loop for all days in month
for (l in 1:length(Day)) {
Days = paste(Year,Month[m],Day[l],"/",sep="")
label = paste(blabel,Year,Month[m],"/",Days,sep="")

### For loop for all 6 variables
for (k in 1:length(Variables)) {

### Get the file names for each day
CONUS_TMP = Variables[k]
list.files = grep(paste(Year,Month[m],Day[l],"/",CONUS_TMP,sep=""),files)
names = as.character() #get file names 
for (i in 1:length(list.files)) {
	names[i] = files[list.files[i]]   }

### Pull the 1 hour forecast out for each lat long for each hour in day

for (j in 1:length(names)) { tryCatch({

   # Build file names to pull from system
   dir = "cd /media/wpage/'My Passport'/NDFD/nomads.ncdc.noaa.gov/NDFD/"
   changeDir = paste(dir,Year,Month[m],"/",Year,Month[m],Day[l],"/"," && ",sep="")
   file.name = gsub(".*/","",paste(names[j],sep=""))
   tmp = character()
   loc = character()
   for (i in 1:length(Locations[,1])) {
      tmp[i] = paste(" -lon ",Locations[i,1]," ",Locations[i,2],sep="") 
      loc = paste(tmp,collapse="") }
   wgrib2 = paste(changeDir,"wgrib2 -d 1 -s ",file.name,loc,sep="")
   wgrib3 = paste(changeDir,"wgrib2 -d 2 -s ",file.name,loc,sep="")
   
   #Run wgrib2
   run = system(wgrib2,intern=TRUE)
   run2 = system(wgrib3,intern=TRUE)
  
   run = paste(unlist(run),collapse="")
   run2 = paste(unlist(run2),collapse="")

   #Pull out results and organize
   temp3 = unlist(strsplit(run,"[,:]"))[6]
   temp4 = unlist(strsplit(run2,"[,:]"))[6]
   if(temp3 == "1 hour fcst" | temp3 == "1-7 hour acc fcst") {
   temp2 = unlist(strsplit(run,"[,:]"))} else if(temp4 == "1 hour fcst") {
   temp2 = unlist(strsplit(run2,"[,:]"))} else temp2=0

   Output = data.frame(Date=NA,Variable=NA,Forecast=NA,Long=NA,Lat=NA,Value=NA)
   Output[1:length(Locations[,1]),1] = gsub("[^0-9]","",temp2[grep("d=",temp2)])
   Output[1:length(Locations[,1]),2] = temp2[4]
   Output[1:length(Locations[,1]),3] = temp2[6]
   for (i in 1:length(Locations[,1])) {
      Output[i,4] = gsub("lon=","",temp2[grep("lon",temp2)[i]])
      Output[i,5] = gsub("lat=","",temp2[grep("lat",temp2)[i]])
      Output[i,6] = gsub("val=","",temp2[grep("val",temp2)[i]])  }
   Output = Output[complete.cases(Output),] # removes any NA's
   
   #Take Output and merge with master output file
   M.out = rbind(Output,M.out)  }, error=function(e){cat     ("ERROR :",conditionMessage(e), "\n")})
}
}
}

#### Send output to csv file
bsavedir = "/home/wpage/Documents/Output/"
savedir = paste(bsavedir,"RAWS","_",Year,Month[m],".csv",sep="")
write.csv(M.out,file = savedir)
}




