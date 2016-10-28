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
#### ANALYZE ROS ERRORS
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
  ROS_Error$hour = as.factor(as.character(ROS_Error$hour))
  ROS_Error$FMtype = as.factor(ROS_Error$FMtype)

 ### Plot errors
 library(scales) 
 p1 = ggplot(data=ROS_Error) + aes(ROS_error_per,group=Cat,fill=Cat) + 
      geom_histogram(breaks=seq(-100,400,by=10),colour='black') +
      theme_bw() + theme(axis.title.y = element_text(size=11,face="bold"), 
      axis.title.x = element_text(size=11,face="bold"), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),panel.background = element_rect(colour = "black",
      linetype="solid",size=1)) + theme(legend.position='none') +
      scale_y_continuous(limits=c(0,800000),expand=c(0,0),labels=comma) + 
      scale_x_continuous(limits = c(-100,403), expand=c(0,0)) + 
      theme(legend.position=c(0.8,0.6)) +labs(y = 'Observations') + 
      labs(x = 'Rate of spread error (%)') 
  p1$labels$fill = 'Prediction type'
  p1 # Plot historgram
  
  mean(ROS_Error$ROS_error_per) # Show various measures of central tendancy
  median(ROS_Error$ROS_error_per)
  sd(ROS_Error$ROS_error_per)
  
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

  ## Station type 
  library(MASS)
  tbl = table(ROS_Error$Cat,ROS_Error$station_type)
  test = chisq.test(tbl) # P < 0.001, the two are related
  
 ### Classification methods
  ## Rpart
  library(rpart)
  library(partykit)
  ROS.rpart = rpart(Cat ~ station_type + FMtype + elev_m + month + hour,
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

   

#### ANALYZE WEATHER DATA
 ### Compile/organize data
 unique.stn = unique(rev.data.obs[,c('station_id','station_type','lon','lat','datetime')])
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
  abs(rev.data.pred$flame_length_m-rev.data.obs$flame_length_m),
  rev.data.pred$wind_speed20ft_mps-rev.data.obs$wind_speed20ft_mps,
  rev.data.pred$rh-rev.data.obs$rh,rev.data.pred$air_temp_c-rev.data.obs$air_temp_c)
 resid.lm = cbind(resid.lm,ROS_Error$Cat,abs(ROS_Error$ROS_error_per),ROS_Error$FMtype)
 colnames(resid.lm) = c('station_id','station_type','lon','lat','datetime','air_temp_c','rh',
  'wind_speed20ft_mps','wind_speedMid_mps','cloud_cover_percent','precip_mm',
  'solar_wm2','X1hrfm','X10hrfm','X100hrfm','ROS_mps','flame_length_m','Wind_error',
  'rh_error','temp_error','Cat','ROS_Error_per','FMtype')
 resid.lm$FMtype = as.factor(resid.lm$FMtype)
 
  ### Determine relative importance of each weather variable on ROS error
   ## Rpart
   library(rpart)
   library(partykit)
   ROS.rpart = rpart(ROS_mps ~ FMtype + air_temp_c + rh + wind_speed20ft_mps + 
               cloud_cover_percent + solar_wm2, data = resid.lm)
   plotcp(ROS.rpart)
   printcp(ROS.rpart)
   plot(as.party(ROS.rpart), type="simple")
   
   ## Random Forests by ROS error
    # Randomly select 1/16 of data for training and testing
    small = resid.lm[sample(nrow(resid.lm),nrow(resid.lm)/100), ]
    small2 = resid.lm[sample(nrow(resid.lm),nrow(resid.lm)/16), ]
    
    # Run Random Forests
    library(randomForestSRC)
    options(mc.cores=4)
    small.rf = rfsrc(ROS_mps ~ FMtype + air_temp_c + rh + wind_speed20ft_mps + 
                cloud_cover_percent + solar_wm2, data=small, ntree=100,
                importance=T)
    
     #Look at output
     library(ggplot2)
     library(dplyr)
     library(RColorBrewer)
     library(ggRandomForests)
     plot(small.rf)
     print(small.rf) #variance explained 29%
     plot(gg_vimp(small.rf)) #variable importance
     
     #Partial dependence
     gg_md = gg_minimal_depth(small.rf)
     topvars = c('wind_speed20ft_mps','FMtype','rh','air_temp_c',
                 'cloud_cover_percent')
     partial.small = plot.variable(small.rf,xvar=topvars,partial=TRUE,
      sorted=FALSE, show.plots=FALSE)
     gg_p = gg_partial(partial.small)
     plot(gg_p,xvar=topvars,panel=TRUE, se=FALSE) 
     
     temp = as.data.frame(gg_p) #plot with estimated lm slope for wind speed
     ggplot(temp,aes(wind_speed20ft_mps.wind_speed20ft_mps,wind_speed20ft_mps.yhat))+
      geom_point() + geom_abline(intercept=0,slope=0.03169)
     
     plot.variable(small.rf,xvar='wind_speed20ft_mps',partial=TRUE,smooth.lines = TRUE)
     plot.variable(small.rf,xvar='FMtype',partial=TRUE,smooth.lines = TRUE)
     plot.variable(small.rf,xvar='rh',partial=TRUE,smooth.lines = TRUE)
      
     #Interaction
     inter = find.interaction(small.rf)
     data(inter)
     plot(gg_interaction(inter),xvar=topvars,panel=TRUE)
     
     #Coplots
     gg_v = gg_variable(small.rf)
     rm_grp = small.rf$xvar$FMtype
     rhpts = quantile_pts(small.rf$xvar$rh,groups=10,intervals=TRUE)
     rh_grp = cut(small.rf$xvar$rh,breaks=rhpts)
     gg_v$rh_grp = rh_grp
     levels(gg_v$rh_grp) = paste('rh in ',levels(gg_v$rh_grp)," (%)",sep="")
     
     plot(gg_v,xvar='wind_speed20ft_mps',smooth=TRUE) + facet_wrap(~rh_grp)
     plot(gg_v,xvar='wind_speed20ft_mps',smooth=TRUE) + facet_wrap(~FMtype)
     
     par.coplot.small1 = gg_partial_coplot(small.rf,xvar="wind_speed20ft_mps",
                        groups=rm_grp,show.plots=FALSE)
     plot(par.coplot.small1)
     par.coplot.small2 = gg_partial_coplot(small.rf,xvar="wind_speed20ft_mps",
                         groups=rh_grp,show.plots=FALSE)
     plot(par.coplot.small2)
    
     #Validation 10-fold cross-validation
     library(caret)
     library(e1071)
     small2$predicted.response <- predict(small.rf, small2)
     confusionMatrix(data=small2$predicted.response,reference=small2$Cat)

  ## Use lm by ROS_error
  library(leaps)
  regsubsets.out = 
   regsubsets(ROS_mps ~ FMtype + air_temp_c + rh + cloud_cover_percent + 
   wind_speed20ft_mps + solar_wm2, data = resid.lm, nbest=1, nvmax=NULL, 
   force.in=NULL, force.out=NULL, method='exhaustive')
  plot(regsubsets.out,scale='adjr2', main='Adjusted R^2')
  fit = lm(ROS_mps ~ FMtype + air_temp_c + rh + cloud_cover_percent + wind_speed20ft_mps +
           solar_wm2, data = resid.lm)
  best.subset = lm(ROS_mps ~ FMtype*rh*wind_speed20ft_mps, data=resid.lm)
  summary(best.subset)
  summary(fit)
  
   # Standardize coefficients  
   library(QuantPsyc)
   lm.beta(fit) #Standardized coefficients
   lm.beta(best.subset) #Standardized coefficients

 ### Maximum error in 20ft wind speed for RH and Fuel type
  ## Plots
  library(cowplot)
  rh_pts = quantile_pts(resid.lm$rh_error,groups=5,intervals=TRUE)
  resid.lm$rh_grp = cut(resid.lm$rh_error,breaks=rh_pts)
  resid.lm$timber = ifelse(resid.lm$FMtype=='GR'|resid.lm$FMtype=='GS'|
                    resid.lm$FMtype=='SH','Non-timber','Timber')
  
  p1 = ggplot(resid.lm[which(resid.lm$timber=='Non-timber'),],
       aes(rh_grp,Wind_error)) + coord_flip() + theme_bw() +
       geom_boxplot(aes(fill=factor(Cat)),outlier.shape=NA) +
       theme(axis.title.x=element_blank(), axis.title.y = element_text(size=11,face="bold"), 
       panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
       panel.background = element_rect(colour = "black", linetype="solid",size=1)) +
       scale_y_continuous(limits = c(-2,7.5),expand=c(0,0)) +theme(legend.position=c(0.8,0.5))+
       labs(x = 'Relative humidity error (%)') 
  p1$labels$fill = 'ROS prediction type'

  p2 = ggplot(resid.lm[which(resid.lm$timber=='Timber'),],
       aes(rh_grp,Wind_error)) + coord_flip() + theme_bw() +
       geom_boxplot(aes(fill=factor(Cat)),outlier.shape=NA) +
       theme(axis.title.y = element_text(size=11,face="bold"), 
       axis.title.x = element_text(size=11,face="bold"), 
       panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
       panel.background = element_rect(colour = "black", linetype="solid",size=1)) +
       scale_y_continuous(limits = c(-2,7.5),expand=c(0,0)) +theme(legend.position='none')+
       labs(x = 'Relative humidity error (%)') + labs(y = '20ft wind speed error (m/s)') 
  
  plot_grid(p1,p2,align='h',labels=c('NT',"T"),ncol=1,nrow=2)
  
 ### Plot weather errors
  ## 20ft wind speed
 p1 = ggplot(data=resid.lm, aes(Wind_error, fill=Cat)) + 
      geom_density(alpha=0.2) + theme_bw() + theme(axis.title.y = element_text(size=11,face="bold"), 
      axis.title.x = element_text(size=11,face="bold"), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),panel.background = element_rect(colour = "black",
      linetype="solid",size=1)) + theme(legend.position='none') +
      scale_y_continuous(expand=c(0,0)) + scale_x_continuous(limits = c(-10,5), expand=c(0,0)) + 
      theme(legend.position=c(-0.8,0.6)) + labs(y = 'Density') + 
      labs(x = '20ft wind speed error (m/s)') 
 p1$labels$fill = 'Prediction type'
 p1 # Plot 
 
 ## RH
 p1 = ggplot(data=resid.lm, aes(rh_error, fill=Cat)) + 
      geom_density(alpha=0.2) + theme_bw() + theme(axis.title.y = element_text(size=11,face="bold"), 
      axis.title.x = element_text(size=11,face="bold"),panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),panel.background = element_rect(colour = "black",
      linetype="solid",size=1)) + theme(legend.position='none') +   
      scale_y_continuous(expand=c(0,0)) + scale_x_continuous(limits = c(-100,100), expand=c(0,0)) + 
      theme(legend.position=c(-0.8,0.6)) + labs(y = 'Density') + 
      labs(x = 'Relative humidity error (%)') 
 p1$labels$fill = 'Prediction type'
 p1 # Plot    
 
 ## Air temp
 p1 = ggplot(data=resid.lm, aes(temp_error, fill=Cat)) + geom_density(alpha=0.2) + 
      theme_bw() + theme(axis.title.y = element_text(size=11,face="bold"), 
      axis.title.x = element_text(size=11,face="bold"), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),panel.background = element_rect(colour = "black",
      linetype="solid",size=1)) + theme(legend.position='none') +                                                                 
      scale_y_continuous(expand=c(0,0)) + scale_x_continuous(limits = c(-20,20), expand=c(0,0)) + 
      theme(legend.position=c(-0.8,0.6)) + labs(y = 'Density') + 
      labs(x = 'Air temperature error (C)') 
 p1$labels$fill = 'Prediction type'
 p1 # Plot  
 
 ## 20ft wind speed + rh
 med = resid.lm[sample(nrow(resid.lm),nrow(resid.lm)/5), ]
 p1 = ggplot(data=med, aes(Wind_error,rh_error,fill=Cat,colour=Cat)) + geom_density_2d(bins=2) +
      theme_bw() + theme(axis.title.y = element_text(size=11,face="bold"), 
      axis.title.x = element_text(size=11,face="bold"),panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),panel.background = element_rect(colour = "black",
      linetype="solid",size=1)) + scale_y_continuous(expand=c(0,0)) + scale_x_continuous(expand=c(0,0)) + 
      labs(y = 'Relative humidity error (%)') + labs(x = 'Wind speed error (m/s)') 
 p1 # Plot  
 
 
 
 
 
    ### Analyze residual of ROS using lm
  ## Prepare data
  
  ## Build model

  
  fit = lm(ROS_mps ~ air_temp_c + rh + cloud_cover_percent + wind_speed20ft_mps + solar_wm2, data=resid.lm)
  summary(fit)
  

  
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
  
  

  
 

### Analyze all data (except cloud cover, RAWS obs no cloud cover info)
 ## Build results data frame in for loop
 var = c('air_temp_c','rh','wind_speed20ft_mps','wind_speedMid_mps','precip_mm',   
         'solar_wm2','X1hrfm','X10hrfm','X100hrfm','ROS_mps','flame_length_m')
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
 var = c('air_temp_c','rh','wind_speed20ft_mps','wind_speedMid_mps','precip_mm',   
         'solar_wm2','X1hrfm','X10hrfm','X100hrfm','ROS_mps','flame_length_m')
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
 var = c('air_temp_c','rh','wind_speed20ft_mps','wind_speedMid_mps','precip_mm',   
         'solar_wm2','X1hrfm','X10hrfm','X100hrfm','ROS_mps','flame_length_m')
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





