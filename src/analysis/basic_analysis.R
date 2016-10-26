######################################################
#                                                    #
#      Analysis of FireWx-Evaluation Database        #
#                                                    #
###################################################### 

#### PREPARE DATA
 ### Import data
 setwd("/media/wpage/Elements/Page/NDFD_Project/Weather")
 library(data.table)
 data = fread("final.csv")
 data = as.data.frame(data)

 ### Order data
 data = data[with(data,order(station_type,station_id,datetime,data_type)),]

 ### Crown fire impact
 data$crown = ifelse(data$CBH_m>0 & data$flame_length_m >= 
              (0.0775*(0.010*data$CBH_m*(460+25.9*100))^0.69),'Yes','No')

 ### Break out observed and predicted values
 obs = data[which(data$data_type == "obs"),]
 pred = data[which(data$data_type == "pred"),]

 ### Subset data to obs ROS > 0 and no precip
  ## Create new data sets and organize
  rev.data.obs = obs[which(obs$ROS_mps > 0 & obs$precip_mm == 0),]
  rev.data.obs = rev.data.obs[with(rev.data.obs,order(station_type,station_id,datetime)),]
 
  rev.data.pred = merge(pred,rev.data.obs,by.x=c('station_id','datetime'),
                       by.y=c ('station_id','datetime'))
 
  colnames(rev.data.pred)[3:30] = c('station_type','data_type','lon','lat','air_temp_c','rh',
                                   'wind_speed20ft_mps','wind_speedMid_mps','wind_direction_deg','cloud_cover_percent','precip_mm',
                                   'solar_wm2','FM40','asp_deg','elev_m','slope_deg','CBD_kgm3','CBH_m','CC_percent','CH_m',
                                   'X1hrfm','X10hrfm','X100hrfm','LiveHerb_frac_percent','LiveWood_frac_percent','ROS_mps',
                                   'flame_length_m','crown')
 
  rev.data.pred = rev.data.pred[c('station_id','station_type','data_type','lon','lat','datetime',
                                 'air_temp_c','rh','wind_speed20ft_mps','wind_speedMid_mps','wind_direction_deg',
                                 'cloud_cover_percent','precip_mm','solar_wm2','FM40','asp_deg','elev_m','slope_deg','CBD_kgm3',
                                 'CBH_m','CC_percent','CH_m','X1hrfm','X10hrfm','X100hrfm','LiveHerb_frac_percent',
                                 'LiveWood_frac_percent','ROS_mps','flame_length_m','crown')]
 
  rev.data.pred = rev.data.pred[with(rev.data.pred,order(station_type,station_id,datetime)),]
 
setwd('/home/wpage/Documents/backup')
load("SubsetData.RData")
#### SUMMARIZE ROS ERRORS
 ### Compile/organize data
 unique.stn = unique(rev.data.obs[,c('station_id','station_type','lon','lat',
 'datetime','FM40','asp_deg','elev_m','slope_deg')])

 ROS_error_per = ((rev.data.pred$ROS_mps - rev.data.obs$ROS_mps) 
                / rev.data.obs$ROS_mps) * 100
 ROS_Error = cbind(unique.stn,ROS_error_per)
 ROS_Error$datetime = as.POSIXct(ROS_Error$datetime,tz="UTC")

 ROS_Error$Cat = ifelse(abs(ROS_Error$ROS_error_per) <=3,"exact",
 ifelse(abs(ROS_Error$ROS_error_per) > 3 & abs(ROS_Error$ROS_error_per) <=35,"good","bad"))

  ## Summarize datetime to month/time of day
  ROS_Error$month = format(ROS_Error$datetime,"%b")
  ROS_Error$hour = format(ROS_Error$datetime,"%H")
  
  ## Summarize FM40 to fuel type
  ROS_Error$FM40 = as.numeric(ROS_Error$FM40)
  ROS_Error$FMtype = ifelse(ROS_Error$FM40<=119,'GR',
                     ifelse(ROS_Error$FM40>119 & ROS_Error$FM40<=139,'GS',
                     ifelse(ROS_Error$FM40>139 & ROS_Error$FM40<=159,'SH',
                     ifelse(ROS_Error$FM40>159 & ROS_Error$FM40<=179,'TU',
                     ifelse(ROS_Error$FM40>179 & ROS_Error$FM40<=199,'TL',
                     ifelse(ROS_Error$FM40>199 & ROS_Error$FM40<=219,'SB',NA))))))

  ## Convert to factors
  ROS_Error$station_type = as.factor(ROS_Error$station_type)
  ROS_Error$Cat = as.factor(ROS_Error$Cat)
  ROS_Error$month = as.factor(ROS_Error$month)
  ROS_Error$hour = as.factor(ROS_Error$hour)
  ROS_Error$FMtype = as.factor(ROS_Error$FMtype)
  
 ### Look at frequencies
  ## By station
  all.freq = prop.table(table(ROS_Error$Cat,ROS_Error$station_id),2)
  mean(temp[1,]) # 70.2% avg bad ROS
  mean(temp[2,]) # 3.4% avg exact ROS
  mean(temp[3,]) # 26.4% avg good ROS
 
 ### Tests of independence
 library(MASS)
  ## Three-way Cat/Station/Month
  mytable = xtabs(~Cat+station_id+month,data=ROS_Error)
  test = summary(mytable)
  test = loglm(~Cat+station_id+month, mytable)
  
  ## Spatial test
  library(MASS)
  tbl = table(ROS_Error$Cat,ROS_Error$station_id)
  test = chisq.test(tbl) # P < 0.001, the two are related
   
   # Check for distrubution of stations
   temp = unique(rev.data.obs$station_id)
   temp2 = unique(exact$station_id)
   temp3 = unique(good$station_id)
   temp[!temp %in% temp2]  #stations ABH, NMT, wsch not in exact
   temp[!temp %in% temp3]  #stations ABH and wsch not in good
  
   # Find stations with more exact predictions then expected
   exact.stn = data.frame()
   for(i in 1:ncol(test$expected)) {
    stn = colnames(test$expected)[i]
    temp = ifelse(test$observed[2,i]>test$expected[2,i],1,0)
    out = as.data.frame(cbind(stn,temp))
    exact.stn = rbind(out,exact.stn)
   }
   stn.exact = as.vector(exact.stn$stn[grep(1,exact.stn$temp)])
  
   # Pull out exact station data
   exact.err = ROS_Error[ROS_Error$station_id %in% stn.exact, ]
   
   # Determine if different by t-test
   ex.freq = as.data.frame(prop.table(table(exact.err$Cat,exact.err$station_id),2))
   all = as.data.frame(all.freq)
   exact.1 = all[which(all$Var1=='exact'),]
   exact.2 = ex.freq[which(ex.freq$Var1=='exact'), ]
   var.test(exact.1$Freq,exact.2$Freq)
   t.test(exact.1$Freq,exact.2$Freq,var.equal=FALSE,paired=FALSE)
   
   # Look at clustering
   stn.unique = unique(exact.err[,c('lon','lat')])
   dbscan::kNNdistplot(stn.unique,k=5)
   res.db = dbscan::dbscan(stn.unique,2, 5)
   library(factoextra)  
   fviz_cluster(res.db,stn.unique,geom="point")  
  
  ## Time test
  library(MASS)
  tbl = table(ROS_Error$Cat,ROS_Error$month)
  test = chisq.test(tbl) # P < 0.001, the two are related
   
  tbl = table(ROS_Error$Cat,ROS_Error$hour)
  test = chisq.test(tbl) # P < 0.001, the two are related

 ### Classification methods
  ## Rpart
  library(rpart)
  library(partykit)
  ROS.rpart = rpart(as.factor(Cat) ~ as.factor(station_type) + as.factor(FMtype) + 
                    elev_m + as.factor(month) + as.factor(as.character(hour)),
                    method="class", data = ROS_Error)
  plotcp(ROS.rpart)
  printcp(ROS.rpart)
  ROS.rpart2 = prune(ROS.rpart, cp = 0.02)
  plot(as.party(ROS.rpart), type="simple")
 
  ## Random Forests
   # Randomly select 1/16 of data for training and testing
   small = ROS_Error[sample(nrow(ROS_Error),nrow(ROS_Error)/16), ]
   small2 = ROS_Error[sample(nrow(ROS_Error),nrow(ROS_Error)/16), ]
   # Run Random Forests
   library(randomForest)
   small.rf = randomForest(Cat ~ station_type + elev_m + FMtype + month + 
                            hour, data =small, ntree=100,importance=T)
   # Look at output
   plot(small.rf)
   varImpPlot(small.rf, sort = T, main="Variable Importance", n.var=5)
   var.imp = data.frame(importance(small.rf,type=2))
   var.imp$Variables <- row.names(var.imp)
   var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]
   partialPlot(small.rf, small2, FMtype,which.class="good") 
   # Validation
   library(caret)
   library(e1071)
   small2$predicted.response <- predict(small.rf, small2)
   confusionMatrix(data=small2$predicted.response,reference=small2$Cat)

  
 


 ### Spatial analysis
 p1 = ggplot(exact,aes(lon,lat)) + geom_point() 
 
 dbscan::kNNdistplot(exact[,3:4],k=5)
 res.db = dbscan::dbscan(exact[,3:4],0.01, 5)
 library(factoextra)  
 fviz_cluster(res.db,exact,geom="point")
  
### Time analysis
p1 = ggplot(ROS_Error,aes(datetime,ROS_error_per,color=Cat)) + geom_point() +
     scale_y_continuous(limits=c(0,400))


small = ROS_Error[sample(nrow(ROS_Error),nrow(ROS_Error)/2),]


###
library(scales) 
ROS_Error$Cat = as.factor(ROS_Error$Cat)
p1 = ggplot(data=ROS_Error) + aes(ROS_error_per,group=Cat,fill=Cat) + 
     geom_histogram(breaks=seq(-100,400,by=10),colour='black') +
     theme_bw() + theme(axis.title.y = element_text(size=11,face="bold"), 
     axis.title.x = element_text(size=11,face="bold"), panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),panel.background = element_rect(colour = "black",
     linetype="solid",size=1)) + theme(legend.position='none') +
     scale_y_continuous(limits=c(0,800000),expand=c(0,0),labels=comma) + 
     scale_x_continuous(limits = c(-100,403), expand=c(0,0)) + 
     theme(legend.position=c(0.8,0.6)) +labs(y = 'Observations') + 
     labs(x = 'Error (%)') 
p1$labels$group = 'Prediction type'
p1 # Plot historgram

mean(ROS_Error$ROS_error_per) # Show various measures of central tendancy
median(ROS_Error$ROS_error_per)
sd(ROS_Error$ROS_error_per)


  
  unique.stn = unique(rev.data.obs[,c('station_id','station_type','datetime')])
  resid.lm = cbind(unique.stn,abs(rev.data.pred$air_temp_c-rev.data.obs$air_temp_c),
                   abs(rev.data.pred$rh-rev.data.obs$rh),abs(rev.data.pred$wind_speed20ft_mps-rev.data.obs$wind_speed20ft_mps),
                   abs(rev.data.pred$wind_speedMid_mps-rev.data.obs$wind_speedMid_mps),
                   abs(rev.data.pred$cloud_cover_percent-rev.data.obs$cloud_cover_percent),
                   abs(rev.data.pred$precip_mm-rev.data.obs$precip_mm),
                   abs(rev.data.pred$solar_wm2-rev.data.obs$solar_wm2),
                   abs(rev.data.pred$X1hrfm-rev.data.obs$X1hrfm),
                   abs(rev.data.pred$X10hrfm-rev.data.obs$X10hrfm),
                   abs(rev.data.pred$X100hrfm-rev.data.obs$X100hrfm),
                   abs(rev.data.pred$ROS_mps-rev.data.obs$ROS_mps),
                   abs(rev.data.pred$flame_length_m-rev.data.obs$flame_length_m))
  
  colnames(resid.lm) = c('station_id','station_type','datetime','air_temp_c','rh',
                         'wind_speed20ft_mps','wind_speedMid_mps','cloud_cover_percent',
                         'precip_mm','solar_wm2','X1hrfm','X10hrfm','X100hrfm',
                         'ROS_mps','flame_length_m')
  
  
  

  
 

### Analyze all data (except cloud cover, RAWS obs no cloud cover info)
 ## Build results data frame in for loop
 var = c('air_temp_c','rh','wind_speed20ft_mps','wind_speedMid_mps','precip_mm',   'solar_wm2','X1hrfm','X10hrfm','X100hrfm','ROS_mps','flame_length_m')
 out.df = data.frame()

 for (i in 1:length(var)) {
 title = var[i]
 name = as.data.frame(cbind(obs.asos[,c(var[5])],pred.asos[,c(var[5])]))
 colnames(name)[1:2] = c('obs','pred')

 name.n = n(name,mod="pred",obs="obs")
 name.FAC2 = FAC2(name,mod="pred",obs="obs")
 name.MB = MB(name,mod="pred",obs="obs")
 name.MGE = MGE(name,mod="pred",obs="obs")
 name.NMB = NMB(name,mod="pred",obs="obs")
 name.NMGE = NMGE(name,mod="pred",obs="obs")
 name.RMSE = RMSE(name,mod="pred",obs="obs")
 name.r = r(name,mod="pred",obs="obs")
 name.COE = COE(name,mod="pred",obs="obs")
 name.IOA = IOA(name,mod="pred",obs="obs")
 
 temp = cbind(name.n,name.FAC2,name.MB,name.MGE,name.NMB,name.NMGE,  
 name.RMSE,name.r,name.COE,name.IOA)
 row.names(temp) = title
 out.df = rbind(out.df,temp)  }

 write.csv(out.df,"stats_all.csv")




 ## Complete for loop to calculate error stats
 var = c('air_temp_c','rh','wind_speed20ft_mps','wind_speedMid_mps','precip_mm',   'solar_wm2','X1hrfm','X10hrfm','X100hrfm','ROS_mps','flame_length_m')
 out.df = data.frame()

 for (i in 1:length(var)) {
 title = var[i]
 name = as.data.frame(cbind(rev.data.obs[,c(var[i])],rev.data.pred[,c(var[i])]))
 colnames(name)[1:2] = c('obs','pred')

 name.n = n(name,mod="pred",obs="obs")
 name.FAC2 = FAC2(name,mod="pred",obs="obs")
 name.MB = MB(name,mod="pred",obs="obs")
 name.MGE = MGE(name,mod="pred",obs="obs")
 name.NMB = NMB(name,mod="pred",obs="obs")
 name.NMGE = NMGE(name,mod="pred",obs="obs")
 name.RMSE = RMSE(name,mod="pred",obs="obs")
 name.r = r(name,mod="pred",obs="obs")
 name.COE = COE(name,mod="pred",obs="obs")
 name.IOA = IOA(name,mod="pred",obs="obs")
 
 temp = cbind(name.n,name.FAC2,name.MB,name.MGE,name.NMB,name.NMGE, name.RMSE,name.r,name.COE,name.IOA)
 row.names(temp) = title
 out.df = rbind(out.df,temp)  }

 ## Save output
 write.csv(out.df,"stats_mod.csv")


 ## Plot summary data of subsetted ros
 jpeg("Plot1.jpeg",width=4,height=4,units='in',res=300)
 p1 = ggplot()+ geom_point(aes(x=rev.data.pred$air_temp_c, y=rev.data.obs$air_temp_c),size=0.5, 
      shape=1) + geom_abline(intercept=0, slope = 1) + 
      theme_bw() + theme(axis.title.y = element_text(size=11,face="bold"), 
      axis.title.x = element_text(size=11,face="bold"),panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),panel.background = element_rect(colour = "black",
      linetype="solid",size=1)) + theme(legend.position='none') + 
      scale_y_continuous(limits = c(-30,60),breaks=seq(-30,60,10), expand=c(0,0)) + 
      scale_x_continuous(limits=c(-30,60),breaks=seq(-30,60,10),expand=c(0,0)) + 
      labs(y = 'Observed air temperature (C)') + labs(x = 'Predicted air temperature (C)') 
 p1
 dev.off()

 jpeg("Plot2.jpeg",width=4,height=4,units='in',res=300)
 p2 = ggplot()+ geom_point(aes(x=rev.data.pred$rh, y=rev.data.obs$rh),size=0.5,shape=1) +
      geom_abline(intercept=0, slope = 1) + 
      theme_bw() + theme(axis.title.y = element_text(size=11,face="bold"), 
      axis.title.x = element_text(size=11,face="bold"),panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),panel.background = element_rect(colour = "black",
      linetype="solid",size=1)) + theme(legend.position='none') + 
      scale_y_continuous(limits = c(0,100),breaks=seq(0,100,10), expand=c(0,0)) + 
      scale_x_continuous(limits=c(0,100),breaks=seq(0,100,10),expand=c(0,0)) + 
      labs(y = 'Observed relative humidity (%)') + labs(x = 'Predicted relative humidity (%)') 
 p2
 dev.off()

 jpeg("Plot3.jpeg",width=4,height=4,units='in',res=300)
 p3 = ggplot()+ geom_point(aes(x=rev.data.pred$wind_speed20ft_mps, 
      y=rev.data.obs$wind_speed20ft_mps),size=0.5,shape=1) +
      geom_abline(intercept=0, slope = 1) + 
      theme_bw() + theme(axis.title.y = element_text(size=11,face="bold"), 
      axis.title.x = element_text(size=11,face="bold"),panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),panel.background = element_rect(colour = "black",
      linetype="solid",size=1)) + theme(legend.position='none') + 
      scale_y_continuous(limits = c(0,30),breaks=seq(0,30,5), expand=c(0,0)) + 
      scale_x_continuous(limits=c(0,30),breaks=seq(0,30,5),expand=c(0,0)) + 
      labs(y = 'Observed 20ft wind speed (m/s)') + labs(x = 'Predicted 20ft wind speed (m/s)') 
 p3
 dev.off()

 jpeg("Plot4.jpeg",width=4,height=4,units='in',res=300)
 p4 = ggplot()+ geom_point(aes(x=rev.data.pred$precip_mm, y=rev.data.obs$precip_mm),size=0.5,
      shape=1) + geom_abline(intercept=0, slope = 1) + 
      theme_bw() + theme(axis.title.y = element_text(size=11,face="bold"), 
      axis.title.x = element_text(size=11,face="bold"),panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),panel.background = element_rect(colour = "black",
      linetype="solid",size=1)) + theme(legend.position='none') + 
      scale_y_continuous(limits = c(0,200),breaks=seq(0,200,20), expand=c(0,0)) + 
      scale_x_continuous(limits=c(0,200),breaks=seq(0,200,20),expand=c(0,0)) + 
      labs(y = 'Observed precipitation (mm)') + labs(x = 'Predicted precipitation (mm)') 
 p4
 dev.off()

 jpeg("Plot5.jpeg",width=4,height=4,units='in',res=300)
 p5 = ggplot()+ geom_point(aes(x=rev.data.pred$ROS_mps, y=rev.data.obs$ROS_mps),size=0.5,shape=1) +
      geom_abline(intercept=0, slope = 1) + 
      theme_bw() + theme(axis.title.y = element_text(size=11,face="bold"), 
      axis.title.x = element_text(size=11,face="bold"),panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),panel.background = element_rect(colour = "black",
      linetype="solid",size=1)) + theme(legend.position='none') + 
      scale_y_continuous(limits = c(0,4),breaks=seq(0,4,0.5), expand=c(0,0)) + 
      scale_x_continuous(limits=c(0,4),breaks=seq(0,4,0.5),expand=c(0,0)) + 
      labs(y = 'Observed rate of spread (m/s)') + labs(x = 'Predicted rate of spread (m/s)') 
 p5
 dev.off()

 jpeg("Plot6.jpeg",width=4,height=4,units='in',res=300)
 p6 = ggplot()+ geom_point(aes(x=rev.data.pred$flame_length_m, 
      y=rev.data.obs$flame_length_m),size=0.5,shape=1) +
      geom_abline(intercept=0, slope = 1) + 
      theme_bw() + theme(axis.title.y = element_text(size=11,face="bold"), 
      axis.title.x = element_text(size=11,face="bold"),panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),panel.background = element_rect(colour = "black",
      linetype="solid",size=1)) + theme(legend.position='none') + 
      scale_y_continuous(limits = c(0,16),breaks=seq(0,16,2), expand=c(0,0)) + 
      scale_x_continuous(limits=c(0,16),breaks=seq(0,16,2),expand=c(0,0)) + 
      labs(y = 'Observed flame length (m)') + labs(x = 'Predicted flame length (m)') 
 p6
 dev.off()


### Analyze residual of ROS using lm
 ## Prepare data

 ## Build model
 library(leaps)
 regsubsets.out = 
   regsubsets(ROS_mps ~ air_temp_c + rh + cloud_cover_percent + wind_speed20ft_mps +
                solar_wm2, data = resid.lm, nbest=1, nvmax=NULL, force.in=NULL,
                force.out=NULL, method='exhaustive')
 plot(regsubsets.out,scale='adjr2', main='Adjusted R^2')
 best.subset = lm(ROS_mps ~ wind_speed20ft_mps, data=resid.lm)
 summary(best.subset)
 
 fit = lm(ROS_mps ~ air_temp_c + rh + cloud_cover_percent + wind_speed20ft_mps + solar_wm2, data=resid.lm)
 summary(fit)
 
 library(QuantPsyc)
 lm.beta(fit) #Standardized coefficients
 
 fit2 = lm(ROS_mps ~ X1hrfm + X10hrfm + X100hrfm + wind_speed20ft_mps, data=resid.lm)
 summary(fit2)
 lm.beta(fit2)

 
### PCA Analysis
log.resid = resid.lm[,c(4,5,6,11,12,13)]
log.resid = log.resid[complete.cases(log.resid),]

resid.pca = prcomp(resid.lm[,c(4,5,6,11,12,13)], center=TRUE, scale.=TRUE)
pca = as.data.frame(resid.pca$x)

fit = lm(resid.lm$ROS_mps ~ PC1 + PC2 + PC3 + PC4 + PC5, data=pca)
summary(fit)


### Hierarchical clustering
library(fastcluster)
clusters = hclust(dist(resid.lm[1:10000,c(4,5)]))
clusterCut = cutree(clusters,10)
table(clusterCut,resid.lm[1:10000,c('station_id')])

set.seed(20)
residCluster = kmeans(resid.lm[,4:5],5, nstart=20)
table(residCluster$cluster,resid.lm$station_id)

library(ggplot2)
residCluster$cluster = as.factor(residCluster$cluster)
p1 = ggplot(resid.lm,aes(air_temp_c,rh,color=residCluster$cluster)) + geom_point()

sumSquare = data.frame()
for(i in 1:50) {
  residCluster = kmeans(resid.lm[,4:5],i,nstart=20)
  numcluster = i
  sums = as.numeric(residCluster[6])/as.numeric(residCluster[3])
  out = cbind(numcluster,sums)
  sumSquare = rbind(out,sumSquare)
}




getmode = function(v)  {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

getmode(test[,1])

library(dbscan)
test = dbscan::kNNdistplot(resid.lm[,4:5],k=5)
library(fpc)
res.db = fpc::dbscan(resid.lm[,4:5],eps=0.1,MinPts=5)


### Look at correlations of residuals with observed data
 ## Complete for loop to calculate corr's
 var = c('air_temp_c','rh','wind_speed20ft_mps','wind_speedMid_mps','precip_mm',   'solar_wm2','X1hrfm','X10hrfm','X100hrfm','ROS_mps','flame_length_m')
 out.df = data.frame()

 for (i in 1:length(var)) {
 title = var[i]
 resid = rev.data.pred[,c(var[i])] - rev.data.obs[,c(var[i])]
 name = as.data.frame(cbind(resid,rev.data.obs[,c(var[i])]))
 colnames(name)[2] = 'obs'

 name.rcorr = cor(name$resid,name$obs)
 out.df = rbind(out.df,name.rcorr)  
 rownames(out.df)[i] = title     }

 ## Save output
 write.csv(out.df,"residcor_mod.csv")


### Look at ROS errors using absolute percent errors: exact considered +/- 2.5%
 ## Prepare data
 unique.stn = unique(rev.data.obs[,c('station_id','station_type','datetime')])
 mape.ros = ((abs(rev.data.pred[,c('ROS_mps')] - rev.data.obs[,c('ROS_mps')]))/rev.data.obs[,c('ROS_mps')])*100
 mape = cbind(unique.stn,mape.ros)

 out.df = data.frame()
 errors = seq(2,202,10)
 
  for (i in 1:length(errors)) {
  # Subset out data 
  ros.error = mape[which(mape$mape.ros <= errors[i]),]
  ros.obs = merge(rev.data.obs,ros.error,by=c('station_id','station_type','datetime'))
  ros.obs = ros.obs[with(ros.obs,order(station_type,station_id,datetime)),]
  ros.pred = merge(rev.data.pred,ros.error,by=c('station_id','station_type','datetime'))
  ros.pred = ros.pred[with(ros.pred,order(station_type,station_id,datetime)),]

  error = paste("Error_",errors[i],sep='')
  resid = abs(ros.pred[,c('wind_speed20ft_mps')] - ros.obs[,c('wind_speed20ft_mps')])
  min.resid = summary(resid)[1]
  Qu1st.resid = summary(resid)[2]
  med.resid = summary(resid)[3]
  mean.resid = summary(resid)[4]
  Qu3rd.resid = summary(resid)[5]
  max.resid = summary(resid)[6]

  temp = cbind(error,min.resid,Qu1st.resid,med.resid,mean.resid,Qu3rd.resid,max.resid)
  out.df = rbind(out.df,temp)  }

library(ggplot2)
jpeg("Plot1.jpeg",width=4,height=4,units='in',res=300)
p1 = ggplot() + geom_line(data=wind, aes(x=Error_g, y=mean.resid),size=1) + 
     theme_bw() + theme(axis.title.y = element_text(size=11,face="bold"), 
     axis.title.x = element_text(size=11,face="bold"),panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),panel.background = element_rect(colour = "black",
     linetype="solid",size=1)) + theme(legend.position='none') + 
     scale_y_continuous(limits = c(0,2),breaks=seq(0,2,0.25), expand=c(0,0)) + 
     scale_x_continuous(limits=c(0,212),breaks=seq(0,210,20),expand=c(0,0)) + 
     labs(y = 'Required 20 ft wind speed accuracy (m/s)') + labs(x = 'Rate of spread error (%)') 
p1
dev.off()
    
 ## Save output
 write.csv(out.df,"resid35_mod.csv")




### Error functions
## number of valid readings
n <- function(x, mod = "mod", obs = "obs") {

    x <- na.omit(x[ , c(mod, obs)])
    res <- nrow(x)
    data.frame(n = res)
}

## fraction within a factor of two
FAC2 <- function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[ , c(mod, obs)])
    ratio <- x[[mod]] / x[[obs]]
    ratio <- na.omit(ratio)
    len <- length(ratio)
    if (len > 0) {
        res <- length(which(ratio >= 0.5 & ratio <= 2)) / len
    } else {
        res <- NA
    }
    data.frame(FAC2 = res)
}

## mean bias
MB <- function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[ , c(mod, obs)])
    res <- mean(x[[mod]] - x[[obs]])
    data.frame(MB = res)
}

## mean gross error
MGE <- function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[ , c(mod, obs)])
    res <- mean(abs(x[[mod]] - x[[obs]]))
    data.frame(MGE = res)
}

## normalised mean bias
NMB <- function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[ , c(mod, obs)])
    res <- sum(x[[mod]] - x[[obs]]) / sum(x[[obs]])
    data.frame(NMB = res)
}

## normalised mean gross error
NMGE <- function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[ , c(mod, obs)])
    res <- sum(abs(x[[mod]] - x[[obs]])) / sum(x[[obs]])
    data.frame(NMGE = res)
}

## root mean square error
RMSE <- function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[ , c(mod, obs)])
    res <- mean((x[[mod]] - x[[obs]]) ^ 2) ^ 0.5
    data.frame(RMSE = res)
}

## correlation coefficient
r <- function(x, mod = "mod", obs = "obs", ...) {

    x <- na.omit(x[ , c(mod, obs)])
    res <- suppressWarnings(cor(x[[mod]], x[[obs]], ...)) ## when SD=0; will return NA

    data.frame(r = res)
}

##  Coefficient of Efficiency
COE <- function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[ , c(mod, obs)])

    res <-  1 - sum(abs(x[[mod]] - x[[obs]])) / sum(abs(x[[obs]] - mean(x[[obs]])))

    data.frame(COE = res)
}

##  Index of Agreement
IOA <- function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[ , c(mod, obs)])

    LHS <- sum(abs(x[[mod]] - x[[obs]]))
    RHS <- 2 * sum(abs(x[[obs]] - mean(x[[obs]])))

    if (LHS <= RHS) res <- 1 - LHS / RHS else res <- RHS / LHS - 1

    data.frame(IOA = res)
}





