#############  Analysis of FireWx-Evaluation database
 
### Import data
setwd("/media/wpage/Elements/Page/NDFD_Project/Weather")
library(data.table)
data = fread("final.csv")
data = as.data.frame(data)

### Order data
data = data[with(data,order(station_type,station_id,datetime,data_type)),]

### Crown fire impact
data$crown = ifelse(data$CBH_m>0 & data$flame_length_m >= (0.0775*(0.010*data$CBH_m*(460+25.9*100))^0.69),'Yes','No')
test = grep('Yes',data$crown)

### Break out observed and predicted values
obs = data[which(data$data_type == "obs"),]
pred = data[which(data$data_type == "pred"),]

### Plot observed and predicted data
library(ggplot2)
library(cowplot)

jpeg("Plot1.jpeg",width=4,height=4,units='in',res=300)
p1 = ggplot()+ geom_point(aes(x=pred$air_temp_c, y=obs$air_temp_c),size=0.5,shape=1) +
     geom_abline(intercept=0, slope = 1) + 
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
p2 = ggplot()+ geom_point(aes(x=pred$rh, y=obs$rh),size=0.5,shape=1) +
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
p3 = ggplot()+ geom_point(aes(x=pred$wind_speed20ft_mps, 
     y=obs$wind_speed20ft_mps),size=0.5,shape=1) +
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
p4 = ggplot()+ geom_point(aes(x=pred$precip_mm, y=obs$precip_mm),size=0.5,shape=1) +
     geom_abline(intercept=0, slope = 1) + 
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
p5 = ggplot()+ geom_point(aes(x=pred$ROS_mps, y=obs$ROS_mps),size=0.5,shape=1) +
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
p6 = ggplot()+ geom_point(aes(x=pred$flame_length_m, y=obs$flame_length_m),size=0.5,shape=1) +
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


### Analyze all data (except cloud cover, RAWS obs no cloud cover info)
 ## Build results data frame in for loop
 var = c('air_temp_c','rh','wind_speed20ft_mps','wind_speedMid_mps','precip_mm',   'solar_wm2','X1hrfm','X10hrfm','X100hrfm','ROS_mps','flame_length_m')
 out.df = data.frame()

 for (i in 1:length(var)) {
 title = var[i]
 name = as.data.frame(cbind(obs[,c(var[i])],pred[,c(var[i])]))
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


### Analyze data based on Subset of data to obs ROS > 0 and no precip
 ## Create new data sets and organize
 rev.data.obs = obs[which(obs$ROS_mps > 0 & obs$precip_mm == 0),]
 rev.data.obs = rev.data.obs[with(rev.data.obs,order(station_type,station_id,datetime)),]

 rev.data.pred = merge(pred,rev.data.obs,by.x=c('station_id','datetime'),
 by.y=c ('station_id','datetime'))

 colnames(rev.data.pred)[3:29] = c('station_type','data_type','lon','lat','air_temp_c','rh',
 'wind_speed20ft_mps','wind_speedMid_mps','wind_direction_deg','cloud_cover_percent','precip_mm',
 'solar_wm2','FM40','asp_deg','elev_m','slope_deg','CBD_kgm3','CBH_m','CC_percent','CH_m',
 'X1hrfm','X10hrfm','X100hrfm','LiveHerb_frac_percent','LiveWood_frac_percent','ROS_mps',
 'flame_length_m')

 rev.data.pred = rev.data.pred[c('station_id','station_type','data_type','lon','lat','datetime',
 'air_temp_c','rh','wind_speed20ft_mps','wind_speedMid_mps','wind_direction_deg',
 'cloud_cover_percent','precip_mm','solar_wm2','FM40','asp_deg','elev_m','slope_deg','CBD_kgm3',
 'CBH_m','CC_percent','CH_m','X1hrfm','X10hrfm','X100hrfm','LiveHerb_frac_percent',
 'LiveWood_frac_percent','ROS_mps','flame_length_m')]

 rev.data.pred = rev.data.pred[with(rev.data.pred,order(station_type,station_id,datetime)),]

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

### Look at ROS errors +/- 35% absolute percent error exact considered +/- 2.5%
 ## Prepare data
 unique.stn = unique(rev.data.obs[,c('station_id','station_type','datetime')])
 mape.ros = ((abs(rev.data.pred[,c('ROS_mps')] - rev.data.obs[,c('ROS_mps')]))/rev.data.obs[,c('ROS_mps')])*100
 resid.n = cbind(unique.stn,mape.ros)

 ## Subset out data where +/- 35% MAPE (3009849/9067420) = 33.2% of obs
 ros.35 = resid.n[which(resid.n$mape.ros <= 35),]
 ros.obs = merge(rev.data.obs,ros.35,by=c('station_id','station_type','datetime'))
 ros.obs = ros.obs[with(ros.obs,order(station_type,station_id,datetime)),]
 ros.pred = merge(rev.data.pred,ros.35,by=c('station_id','station_type','datetime'))
 ros.pred = ros.pred[with(ros.pred,order(station_type,station_id,datetime)),]

 ## Determine difference in weather in for loop
 var = c('air_temp_c','rh','wind_speed20ft_mps','wind_speedMid_mps','precip_mm',   'solar_wm2','X1hrfm','X10hrfm','X100hrfm')
 out.df = data.frame()

 for (i in 1:length(var)) {
 title = var[i]
 resid = abs(ros.pred[,c(var[i])] - ros.obs[,c(var[i])])
 min.resid = summary(resid)[1]
 Qu1st.resid = summary(resid)[2]
 med.resid = summary(resid)[3]
 mean.resid = summary(resid)[4]
 Qu3rd.resid = summary(resid)[5]
 max.resid = summary(resid)[6]

 temp = cbind(title,min.resid,Qu1st.resid,med.resid,mean.resid,Qu3rd.resid,max.resid)
 out.df = rbind(out.df,temp)  }

 ## Save output
 write.csv(out.df,"resid35_mod.csv")

### Analyze residual of ROS using lm
 ## Prepare data
 resid.lm = cbind(unique.stn,abs(rev.data.pred$air_temp_c-rev.data.obs$air_temp_c),
 abs(rev.data.pred$rh-rev.data.obs$rh),abs(rev.data.pred$wind_speed20ft_mps-rev.data.obs$wind_speed20ft_mps),abs(rev.data.pred$wind_speedMid_mps-rev.data.obs$wind_speedMid_mps),
 abs(rev.data.pred$precip_mm-rev.data.obs$precip_mm),abs(rev.data.pred$solar_wm2-rev.data.obs$solar_wm2),abs(rev.data.pred$X1hrfm-rev.data.obs$X1hrfm),abs(rev.data.pred$X10hrfm-rev.data.obs$X10hrfm),abs(rev.data.pred$X100hrfm-rev.data.obs$X100hrfm),abs(rev.data.pred$ROS_mps-rev.data.obs$ROS_mps),abs(rev.data.pred$flame_length_m-rev.data.obs$flame_length_m))

 colnames(resid.lm) = c('station_id','station_type','datetime','air_temp_c','rh',
 'wind_speed20ft_mps','wind_speedMid_mps','precip_mm','solar_wm2','X1hrfm','X10hrfm','X100hrfm',
 'ROS_mps','flame_length_m')

 ## Build model
 fit = lm(ROS_mps ~ air_temp_c*rh*precip_mm + air_temp_c*rh*precip_mm+ wind_speedMid_mps*rh*air_temp_c + wind_speedMid_mps,data=resid.lm)
 summary(fit)



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





