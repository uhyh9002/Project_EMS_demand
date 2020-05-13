
par(mfrow = c(2,2))
for(i in 1:8){
  
  plot(dj_data$x[dj_data$call_t > (i-1)*300 & dj_data$call_t < (i)*300 ] ,dj_data$y[dj_data$call_t > (i-1)*300 & dj_data$call_t < (i)*300],pch = 20,font = 2,xlab = "", ylab = ""
       ,main=paste("Time from" ,(i-1)*3,  "to" ,i*3),xlim=c(min(dj_data$x)-100,max(dj_data$x)+100),ylim=c(min(dj_data$y)-100,max(dj_data$y)+100))
  mtext(side=1,text="Longitude",line=2.5,font = 2)
  mtext(side=2,text="Latitute",line=2.5,font = 2)
}
par(mfrow = c(1,1))



par_GMM_MCMC_30_3000 <-  read.csv("C:/Users/user/Desktop/Ambulance demand Estimation/GMM&GQM/result_MCMC_1020_M30.csv")

NOM = 30

pi_exp = par_GMM_MCMC_30_3000[which(par_GMM_MCMC_30_3000[,2]=="pi_exp"):(which(par_GMM_MCMC_30_3000[,2]=="pi_exp")+NOM-1)+2,1:12+1]
rownames(pi_exp) = 1:nrow(pi_exp)
colnames(pi_exp) = 1:ncol(pi_exp)
for(i in 1:ncol(pi_exp)){
  pi_exp[,i] = as.numeric(paste(pi_exp[,i]))
}

mu_exp = par_GMM_MCMC_30_3000[which(par_GMM_MCMC_30_3000[,2]=="mu_exp"):(which(par_GMM_MCMC_30_3000[,2]=="mu_exp")+NOM-1)+2,1:2+1]
rownames(mu_exp) = 1:nrow(mu_exp)
colnames(mu_exp) = c("x","y")
for(i in 1:ncol(mu_exp)){
  mu_exp[,i] = as.numeric(paste(mu_exp[,i]))
}

cov_mat_exp = list()

for(i in 1:NOM){
  cov_mat_exp[[i]] = par_GMM_MCMC_30_3000[which(par_GMM_MCMC_30_3000[,2]=="cov_mat_exp"):(which(par_GMM_MCMC_30_3000[,2]=="cov_mat_exp")+1)+2,0:1+i*2]
  for(j in 1:ncol(cov_mat_exp[[i]])){
    cov_mat_exp[[i]][,j] = as.numeric(paste(cov_mat_exp[[i]][,j]))
    rownames(cov_mat_exp[[i]]) = c("x","y")
    colnames(cov_mat_exp[[i]]) = c("x","y")
  }
}

# Plot STGMM -----------
library(mvtnorm)
library(mvtnorm)
library(MASS)
library(car)

x = matrix(0,2000,nrow(pi_exp))
y = matrix(0,2000,nrow(pi_exp))
for (i in 1:nrow(pi_exp)){
  MEAN = as.numeric(mu_exp[i,])
  SIGMA = matrix(as.numeric(c(cov_mat_exp[[i]][1,],cov_mat_exp[[i]][2,])),ncol = 2)
  temp = rmvnorm(200,mean=MEAN,sigma=SIGMA)
  x[,i] = temp[,1]
  y[,i] = temp[,2]
}


x_lim_ = c(min(dj_data$x)-100,max(dj_data$x)+100)
y_lim_ =c(min(dj_data$y)-100,max(dj_data$y)+100)

x_lim_ = c(min(dj_data$x)-100,max(dj_data$x)+100)
y_lim_ =c(295000,335000)

color = palette(rainbow(nrow(pi_exp)))
plot(dj_data$x,dj_data$y,cex = .5,xlim = x_lim_,ylim = y_lim_,main="",xlab="latitude",ylab="longitude")
plot(mu_exp,xlim = x_lim_,ylim = y_lim_,main="",xlab="latitude",ylab="longitude")
for (i in 1:nrow(pi_exp)){
  par(new=TRUE)
  dataEllipse(x[,i], y[,i], levels=c(0.95),col=color[i],xlim = x_lim_,ylim = y_lim_,xlab="latitude",ylab="longitude",plot.points = F)
}
par(new=FALSE)

# Demand Scatter plot---------------
library(ggplot2)

length(dj_data$x[dj_data$call_t > 000 & dj_data$call_t < 0300 ])
length(dj_data$x[dj_data$call_t > 900 & dj_data$call_t < 1200 ])

EMS_demand = data.frame(cbind(dj_data$x[dj_data$call_t > 900 & dj_data$call_t < 1200 ],dj_data$y[dj_data$call_t > 900 & dj_data$call_t < 1200 ]))
colnames(EMS_demand) = c("long","lat")

EMS_demand = data.frame(cbind(dj_data$x[dj_data$call_t > 2000 & dj_data$call_t < 2300 ],dj_data$y[dj_data$call_t > 2000 & dj_data$call_t < 2300 ]))
colnames(EMS_demand) = c("long","lat")

EMS_demand = data.frame(cbind(dj_data$x[dj_data$call_t > 000 & dj_data$call_t < 0300 ],dj_data$y[dj_data$call_t > 000 & dj_data$call_t < 0300 ]))
colnames(EMS_demand) = c("long","lat")

tx <- readOGR("대전시군구.shp", stringsAsFactors=FALSE)
tx <- readOGR("대전행정동_TM.shp", stringsAsFactors=FALSE)


sp = ggplot(EMS_demand, aes(x = long, y = lat), xaxt = "n") + 
  geom_point(size = 1, alpha = 1) +
  xlab('') + 
  ylab('') + 
  coord_cartesian(xlim = x_lim_, 
                  ylim = y_lim_) +
  geom_density_2d(colour="white")+
  stat_density2d(aes(fill = ..level..), alpha = .5, h = 1800
                 , geom = "polygon", data = EMS_demand) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_polygon(data = tx, aes(x = long, y = lat,group = group), colour = "black", fill = NA)
  

  # theme(legend.position = 'none')

sp +   ggtitle("") +
  scale_fill_gradient(low="grey", high="black")+
  theme(plot.title=element_text(face="bold", hjust = 0.5, size=15, vjust=2, color="black"))
  

sp +   ggtitle("EMS call location in 20-23") +
  scale_fill_gradient(low="grey", high="black")+
  theme(plot.title=element_text(face="bold", hjust = 0.5, size=15, vjust=2, color="black"))

sp +   ggtitle("") +
  scale_fill_gradient(low="grey", high="black")+
  theme(plot.title=element_text(face="bold", hjust = 0.5, size=15, vjust=2, color="black"))


# Shape file loading-------------
require(sf)
library(sp)
library(rgdal)

shape <- read_sf(dsn = ".", layer = "대전행정동_TM")

shape <- read_sf(dsn = ".", layer = "대전시군구")

ggplot() + 
  geom_sf(data = shape, size = 1, color = "black") + 
  ggtitle("AOI Boundary Plot") 
  
proj4string(shape)
  
shape$geometry

tx <- readOGR("대전시군구.shp", stringsAsFactors=FALSE)
tx <- readOGR("대전행정동_TM.shp", stringsAsFactors=FALSE)

plot(tx)

proj4string(tx)
## [1] "+proj=lcc +lat_1=34.91666666666666 +lat_2=27.41666666666667 +lat_0=31.16666666666667 +lon_0=-100 +x_0=1000000 +y_0=1000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"


tx_ll <- spTransform(tx, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

tx <- fortify(tx)

head(coordinates(tx_ll))

map <- ggplot() + geom_polygon(data = tx, aes(x = long, y = lat,group = group), colour = "black", fill = NA)


# Temporal variation----------

dj_time_bin = binning(dj_data,"call_t","numeric",24)

barplot(lengths(dj_time_bin),width = 0.832,xaxt="n",cex.axis = 0.9,col = "black",font = 2)
axis(side=1,at=seq(0.6,23.6,1),labels=seq(1,24,1),lwd=1,cex.axis = 0.9,font = 2)

axis(side=1,at=seq(0.6,23.6,1),labels=c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10"
                                        ,"10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18"
                                        ,"18-19","19-20","20-21","21-22","22-23","23-24"),lwd=1,cex.axis = 0.7,font = 2)

mtext(side=1,text="Time of day",line=2.5,font = 2)
mtext(side=2,text="Number of patients",line=2.5,font = 2)

#MCI distribution plot----------

hist(MCI_dj_distribution[MCI_dj_distribution>=2],breaks = seq(1,10,1),xaxt="n", ylim = c(0,800),xlab= "",ylab = "",main = "",font = 2)
axis(side=1,at=seq(0.5,23.5,1),labels=seq(1,24,1),lwd=1,cex.axis = 0.9,font = 2)
mtext(side=1,text="Size of calls",line=2.5,font = 2)
mtext(side=2,text="Number of calls",line=2.5,font = 2)

mean(MCI_dj_distribution[MCI_dj_distribution>=2])
median(MCI_dj_distribution[MCI_dj_distribution>=2])
MCI_dj_distribution


# Mean variance analysis ------

mean_mat = matrix(0,7,24)
var_mat = matrix(0,7,24)
for(i in 1:length(dj_day_bin)){
  id_dow = dj_day_bin[[i]]
  dj_data_dow = dj_data[id_dow,]
  time_bin_day = binning(dj_data_dow,"call_t","numeric",24)
  for(j in 1:24){
    dj_dow_time = dj_data_dow[time_bin_day[[j]],]
    days = unique(dj_dow_time$start_d)
    temp_array = array(0,length(days))
    for(k in 1:length(days)){
      temp_array[k] = nrow(dj_dow_time[dj_dow_time$start_d == days[k],])
    }
    mean_mat[i,j] = mean(temp_array)
    var_mat[i,j] = var(temp_array)
  }
}

plot(as.numeric(mean_mat),as.numeric(var_mat),xlab = "",ylab = "",xlim = c(0,11),ylim = c(0,18),xaxt = "n",yaxt ="n")
abline(a = 0,b = 1, lty = 2)
axis(side=1,at=seq(0,18,2),labels=seq(0,18,2),lwd=1,cex.axis = 1.1,font = 2)
axis(side=2,at=seq(0,18,2),labels=seq(0,18,2),lwd=1,cex.axis = 1.1,font = 2)
mtext(side=1,text="Mean",line=2,font = 2,cex = 1.4)
mtext(side=2,text="Variance",line=2.2,font = 2,cex = 1.4)

par(family = "serif",font = 2)

#For call------

dj_day_bin = binning(dj_single,"dow","numeric",c(0,1,2,3,4,5,6,7))
dj_day_bin = binning(dj_data_call,"dow","numeric",c(0,1,2,3,4,5,6,7))
dj_day_bin = binning(dj_data_emerg,"dow","numeric",c(0,1,2,3,4,5,6,7))
dj_day_bin = binning(dj_data_emerg_single,"dow","numeric",c(0,1,2,3,4,5,6,7))
dj_day_bin = binning(dj_data_emerg_call,"dow","numeric",c(0,1,2,3,4,5,6,7))
  
mean_mat_call = matrix(0,7,24)
var_mat_call = matrix(0,7,24)
for(i in 1:length(dj_day_bin)){
  id_dow = dj_day_bin[[i]]
  
  # dj_data_dow = dj_data_emerg_call[id_dow,]
  # dj_data_dow = dj_data_emerg_single[id_dow,]
  # dj_data_dow = dj_data_emerg[id_dow,]
  # dj_data_dow = dj_data_call[id_dow,]
  dj_data_dow = dj_single[id_dow,]
  
  time_bin_day = binning(dj_data_dow,"call_t","numeric",24)
  for(j in 1:24){
    dj_dow_time = dj_data_dow[time_bin_day[[j]],]
    days = unique(dj_dow_time$start_d)
    temp_array = array(0,length(days))
    for(k in 1:length(days)){
      temp_array[k] = nrow(dj_dow_time[dj_dow_time$start_d == days[k],])
    }
    mean_mat_call[i,j] = mean(temp_array)
    var_mat_call[i,j] = var(temp_array)
  }
}

plot(as.numeric(mean_mat_call),as.numeric(var_mat_call),xlab = "",ylab = "",xlim = c(0,11),ylim = c(0,18),xaxt = "n",yaxt ="n")
abline(a = 0,b = 1, lty = 2)
axis(side=1,at=seq(0,18,2),labels=seq(0,18,2),lwd=1,cex.axis = 1.1,font = 2)
axis(side=2,at=seq(0,18,2),labels=seq(0,18,2),lwd=1,cex.axis = 1.1,font = 2)
mtext(side=1,text="Mean",line=2,font = 2,cex = 1.4)
mtext(side=2,text="Variance",line=2.2,font = 2,cex = 1.4)

# regular data ------------

dj_regular = binning(dj_data,"name","string")
id_regular = c(1:length(dj_regular))[lengths(dj_regular)> 30]

regular_id = list()
for(i in 1:length(id_regular)){
  regular_id[[i]] = dj_regular[[id_regular[i]]]
}
regular_id = unlist(regular_id)

dj_regular_proc = dj_data[regular_id,]

dj_data_emerg =  dj_data[!(c(1:nrow(dj_data)) %in% regular_id),]

dj_data_emerg_single = dj_data_emerg[!(dj_data_emerg$jaenan_sn %in% (dj_MCI_alldata$jaenan_sn)), ]

dj_data_emerg_call = dj_data_emerg[(unique(dj_data_emerg$jaenan_sn)), ]

# Over dispersion--------------

mean(as.numeric(var_mat_call)/as.numeric(mean_mat_call))
1.96*var(as.numeric(var_mat_call)/as.numeric(mean_mat_call))
mean(as.numeric(var_mat)/as.numeric(mean_mat))
1.96*var(as.numeric(var_mat)/as.numeric(mean_mat))