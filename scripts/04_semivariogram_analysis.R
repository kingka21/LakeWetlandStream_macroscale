##Written by: Katelyn King
##Start Date: Oct 24, 2017 

#load libraries 
library(sp)
library(rgdal)
library(gstat)

### Load data from data wrangling code 
lake <- read.csv("Data/NLA2012_data.csv", header=T)
stream<-read.csv("Data/NRSA0809_data.csv", header=T)
wetland<-read.csv("Data/NWCA2011_data.csv", header=T)

### ---- checking normality and log transformation ------ 
##Lakes
hist(lake$TP)
hist(lake$TN)
hist(lake$CHLA)
hist(lake$aqveg) 

#log transform variables and add 1 if there 0s  
lake$logTP<-log(lake$TP)
lake$logTN<-log(lake$TN)
lake$logCHLA<-log(1+lake$CHLA)
lake$logaqveg<-log(1+lake$aqveg)

#### Streams
#checking normality using histogram 
hist(stream$TP) 
hist(stream$TN)  
hist(stream$CHLA)
hist(stream$aqveg) 

#log transform variables new columns 
stream$logTP<-log(1+stream$TP)
stream$logTN<-log(1+stream$TN)
stream$logCHLA<-log(stream$CHLA)
stream$logaqveg<-log(1+stream$aqveg)

### wetlands 
#checking normality using histogram 
hist(wetland$TP)
hist(wetland$TN)
hist(wetland$CHLA)
hist(wetland$aqveg) 

#log transform variables new columns 
wetland$logTP<-log(wetland$TP)
wetland$logTN<-log(wetland$TN)
wetland$logCHLA<-log(1+wetland$CHLA)
wetland$logaqveg<-log(1+wetland$aqveg)

###------------------- semivariogram analysis ---------------- ###

#convert to spatial object
lake.ll <- SpatialPointsDataFrame(coords=lake[,c("LON_DD83","LAT_DD83")], data=lake,
                                  proj4string=CRS("+proj=longlat +datum=NAD83 +ellps=GRS80")) 

# transform the lat&lon coordinates to USA_Contiguous_Albers_Equal_Area_Conic (aea)
prj.new<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=km +ellps=GRS80")
lake.aea <- spTransform(lake.ll, prj.new)

stream.ll<-SpatialPointsDataFrame(coords=stream[,c("LON_DD83","LAT_DD83")], data=stream,
                       proj4string=CRS("+proj=longlat +datum=NAD83 +ellps=GRS80")) 
stream.aea <- spTransform(stream.ll, prj.new)

wetland.ll<-SpatialPointsDataFrame(coords=wetland[,c("LON_DD83","LAT_DD83")], data=wetland,
                                  proj4string=CRS("+proj=longlat +datum=NAD83 +ellps=GRS80")) 
wetland.aea <- spTransform(wetland.ll, prj.new)

##################  Total Phosphorus ###################
#lake
lake.TP.v <- variogram(logTP~1, lake.aea, cutoff=2500, width=20)  
min(lake.TP.v$np) #ensure there are more than 50 pairs in each bin distance 
lake.TP.fit<-fit.variogram(lake.TP.v, vgm("Sph", "Exp")) #fit a model variogram
lake.TP.fit #shows parameters

#wetland
wlTPnoNAs<-wetland.aea[!(is.na(wetland.aea$logTP)),] 
wl.TP.v <- variogram(logTP~1, wlTPnoNAs, cutoff=2500, width=20)  
min(wl.TP.v$np)
wl.TP.fit<-vgm(model="Exp", nugget=.8, psill=1.9, range=605)  
wl.TP.fit

#stream
strTPnoNAs<-stream.aea[!(is.na(stream.aea$logTP)),] 
stream.TP.v <- variogram(logTP~1, strTPnoNAs, cutoff=2500, width=20)  
min(stream.TP.v$np)
stream.TP.fit <- fit.variogram(stream.TP.v, vgm("Sph", "Exp"))
stream.TP.fit

########################### Total Nitrogen #############################
#lake
lake.TN.v <- variogram(logTN~1, lake.aea, cutoff=2500, width=20)
min(lake.TN.v$np)
lake.TN.fit <- fit.variogram(lake.TN.v, vgm("Sph", "Exp"))
lake.TN.fit #shows parameters

#wetland
wlTNnoNAs<-wetland.aea[!(is.na(wetland.aea$logTN)),] 
wl.TN.v <- variogram(logTN~1, wlTNnoNAs, cutoff=2500, width=20)  
min(wl.TN.v$np)
wl.TN.fit <- fit.variogram(wl.TN.v, vgm("Sph", "Exp"))
wl.TN.fit

#stream
strTNnoNAs<-stream.aea[!(is.na(stream.aea$logTN)),] 
stream.TN.v <- variogram(logTN~1, strTNnoNAs, cutoff=2500, width=20)  
min(stream.TN.v$np)
stream.TN.fit <- fit.variogram(stream.TN.v, vgm("Sph", "Exp"))
stream.TN.fit 

########################### Chla #############################
#lake
LCHnoNAs<-lake.aea[!(is.na(lake.aea$logCHLA)),] 
lake.CHL.v <- variogram(logCHLA~1, LCHnoNAs, cutoff=2500, width=20)  
min(lake.CHL.v$np)
lake.CHL.fit<-fit.variogram(lake.CHL.v, vgm(model="Sph", "Exp") )
lake.CHL.fit 

#wetland 
wlCHnoNAs<-wetland.aea[!(is.na(wetland.aea$logCHLA)),] 
wl.CHL.v <- variogram(logCHLA~1, wlCHnoNAs, cutoff=2500, width=20)  
min(wl.CHL.v$np)      
wl.CHL.fit <- vgm(model="Sph", nugget=1.1, psill=1.2, range=101)
wl.CHL.fit

#stream
strCHnoNAs<-stream.aea[!(is.na(stream.aea$logCHLA)),] 
stream.CHL.v <- variogram(logCHLA~1, strCHnoNAs, cutoff=2500, width=20)  
min(stream.CHL.v$np)
stream.CHL.fit <- fit.variogram(stream.CHL.v, vgm("Sph", "Exp"))
stream.CHL.fit

#################################### AQM ##################################
#lake 
LaqnoNAs<-lake.aea[!(is.na(lake.aea$logaqveg)),] 
lake.aq.v<- variogram(logaqveg~1, LaqnoNAs, cutoff=2500, width=20)
min(lake.aq.v$np)
lake.aq.fit <- fit.variogram(lake.aq.v, vgm( "Sph", "Exp"))
lake.aq.fit

###wetland 
wlaqnoNAs<-wetland.aea[!(is.na(wetland.aea$logaqveg)),] 
wl.aq.v <- variogram(logaqveg~1, wlaqnoNAs, cutoff=2500, width=20)  
min(wl.aq.v$np)
wl.aq.fit<- vgm("Sph", nugget=1.4, psill=2.1, range=1335)
wl.aq.fit

#streams##
straqnoNAs<-stream.aea[!(is.na(stream.aea$logaqveg)),] 
stream.aq.v <- variogram(logaqveg~1, straqnoNAs, cutoff=2500, width=20)  
min(stream.aq.v$np)
stream.aq.fit <- fit.variogram(stream.aq.v, vgm( "Sph", "Exp"))
stream.aq.fit

#####################   PLOT    ##################

jpeg('12panel.jpeg',width = 6, height = 7, units = 'in', res = 600)
par(mfrow=c(4,3))
par(mar=c(3,3,0,0), oma=c(3,3,2,1))

#top left panel 
plot(variogramLine(lake.TP.fit, 2500), type='l', ylim=c(0,2.5)) 
points(lake.TP.v[,2:3], pch=21, bg="#7570b3", col='black')  
abline(v=lake.TP.fit[2,3], col="#7570b3") 
abline(v=1950, col="#7570b3") 
#mtext('a', side = 3, line = -1.5, adj = .02, cex = 1.0)

#top middle 
plot(variogramLine(wl.TP.fit, 2500), type='l', ylim=c(0,3.5)) #this plots the line from the model 
points(wl.TP.v[,2:3], pch=21, bg='#d95f02',  col='black')  #this plots the points 
abline(v=wl.TP.fit[2,3], col="#d95f02") #this plots the R range 
abline(v=1930, col="#d95f02") #this plots the R range 
#mtext('b', side = 3, line = -1.5, adj = .02, cex = 1.0)

#top right 
plot(variogramLine(stream.TP.fit, 2500), type='l', ylim=c(0,2.2)) #this plots the line from the model 
points(stream.TP.v[,2:3], pch=21, bg='#1b9e77', col="black")  #this plots the points 
abline(v=stream.TP.fit[2,3], col="#1b9e77") 
abline(v=1950, col="#1b9e77") #this plots the R range 
#mtext('c', side = 3, line = -1.5, adj = .02, cex = 1.0)

#left TN 
plot(variogramLine(lake.TN.fit, 2500), type='l', ylim=c(0,2)) #this plots the line from the model 
points(lake.TN.v[,2:3], pch=21, bg='#7570b3', col="black")  
abline(v=lake.TN.fit[2,3], col="#7570b3") 
abline(v=1750, col="#7570b3") 
#mtext('d', side = 3, line = -1.5, adj = .02, cex = 1.0)

# middle 
plot(variogramLine(wl.TN.fit, 2500), type='l', ylim=c(0,2)) 
points(wl.TN.v[,2:3], pch=21, bg='#d95f02', col="black")  #this plots the points 
abline(v=wl.TN.fit[2,3], col="#d95f02") #this plots the R range 
#mtext('e', side = 3, line = -1.5, adj = .02, cex = 1.0)
# right 
plot(variogramLine(stream.TN.fit, 2500), type='l', ylim=c(0,2)) 
points(stream.TN.v[,2:3], pch=21, bg='#1b9e77', col="black")  #this plots the points 
abline(v=stream.TN.fit[2,3], col="#1b9e77") 
#mtext('f', side = 3, line = -1.5, adj = .02, cex = 1.0)

#Chla 
plot(variogramLine(lake.CHL.fit, 2500), type='l', ylim=c(0,2.0)) #this plots the line from the model 
points(lake.CHL.v[,2:3], pch=21, bg='#7570b3', col="black")  
abline(v=lake.CHL.fit[2,3], col="#7570b3") 
abline(v=1800, col="#7570b3") 
#mtext('g', side = 3, line = -1.5, adj = .02, cex = 1.0)
# middle 
plot(variogramLine(wl.CHL.fit, 2500), type='l', ylim=c(0,3.5)) 
points(wl.CHL.v[,2:3], pch=21, bg='#d95f02', col="black")  #this plots the points 
abline(v=wl.CHL.fit[2,3], col="#d95f02") #this plots the R range 
abline(v=1500, col="#d95f02")
#mtext('h', side = 3, line = -1.5, adj = .02, cex = 1.0)
# right 
plot(variogramLine(stream.CHL.fit, 2500), type='l', ylim=c(0,3.5)) 
points(stream.CHL.v[,2:3], pch=21, bg='#1b9e77', col="black")  #this plots the points 
abline(v=stream.CHL.fit[2,3], col="#1b9e77") 
abline(v=1700, col="#1b9e77") 
#mtext('i', side = 3, line = -1.5, adj = .02, cex = 1.0)

#AQM 
plot(variogramLine(lake.aq.fit, 2500), type='l', ylim=c(0,2.5)) #this plots the line from the model 
points(lake.aq.v[,2:3], pch=21, bg='#7570b3', col="black")  
abline(v=lake.aq.fit[2,3], col="#7570b3") 
#mtext('j', side = 3, line = -1.5, adj = .02, cex = 1.0)
# middle 
plot(variogramLine(wl.aq.fit, 2500), type='l', ylim=c(0,4.2)) 
points(wl.aq.v[,2:3], pch=21, bg='#d95f02', col="black")  #this plots the points 
abline(v=wl.aq.fit[2,3], col="#d95f02") #this plots the R range 
#mtext('k', side = 3, line = -1.5, adj = .02, cex = 1.0)
# right 
plot(variogramLine(stream.aq.fit, 2500), type='l', ylim=c(0,2.2)) 
points(stream.aq.v[,2:3], pch=21, bg='#1b9e77', col="black")  #this plots the points 
abline(v=stream.aq.fit[2,3], col="#1b9e77") 
abline(v=1630, col="#1b9e77") 
#mtext('l', side = 3, line = -1.5, adj = .02, cex = 1.0)

#add axis labels
mtext("distance (km)", side = 1, outer = TRUE, cex = 1, line = 1)
mtext("semivariance", side = 2, outer = TRUE, adj=0.53, cex = 1, line = 1)

mtext( 'lake', side=3, line=0.25, adj=0.18, cex = .9, outer=TRUE ) # 3=top, line=where in the margin, adj=left to right adjustment
mtext( 'wetland', side=3, line=0.25, adj=0.52,cex = .9, outer=TRUE ) 
mtext( 'stream', side=3, line=0.25, adj=0.87, cex = .9, outer=TRUE ) 
mtext( 'TP', side=2, line=-0.5, adj=0.915, cex = .9, outer=TRUE ) # side 2=left #line is which MAR line starting at 0 and counting out
mtext( 'TN', side=2, line=-0.5, adj=0.653, cex = .9, outer=TRUE )
mtext( 'CHL', side=2, line=-0.5, adj=0.400, cex = .9, outer=TRUE )
mtext( 'AqVeg', side=2,  line=-0.5, adj=0.128, cex = .9, outer=TRUE )

dev.off()


#####################   Predictor variables    ##################

allecos<-gtools::smartbind(lake, stream, wetland)

#convert to spatial object
all.ll <- SpatialPointsDataFrame(coords=allecos[,c("LON_DD83","LAT_DD83")], data=allecos,
                                 proj4string=CRS("+proj=longlat +datum=NAD83 +ellps=GRS80")) 

# transform the lat&lon coordinates to USA_Contiguous_Albers_Equal_Area_Conic (aea)
prj.new<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=km +ellps=GRS80")
all.aea <- spTransform(all.ll, prj.new)

#Forest 
hist(allecos$FOREST_PCT)

#variogram
FOR<-all.aea[!(is.na(all.aea$FOREST_PCT)),] 
for.v <- variogram(FOREST_PCT~1, FOR, cutoff=2500, width=20)  
min(for.v$np) #ensure there are more than 50 pairs in each bin distance 
for.fit<-fit.variogram(for.v, vgm("Sph", "Exp")) #fit a model variogram
for.fit #shows parameters
#plot 
plot(variogramLine(for.fit, 2500), type='l', ylim=c(0,2000), main = "percent forest cover") #this plots the line from the model 
points(for.v[,2:3], pch=21, bg='blue', col="black")

#Agriculture
hist(allecos$AG_PCT)

#variogram
AG<-all.aea[!(is.na(all.aea$AG_PCT)),] 
ag.v <- variogram(AG_PCT~1, AG, cutoff=2500, width=20)  
min(ag.v$np) #ensure there are more than 50 pairs in each bin distance 
ag.fit<-fit.variogram(ag.v, vgm("Sph", "Exp")) #fit a model variogram
ag.fit #shows parameters
#plot 
plot(variogramLine(ag.fit, 2500), type='l', ylim=c(0,1500), main = "percent agriculture cover") #this plots the line from the model 
points(ag.v[,2:3], pch=21, bg='blue', col="black")

#Depth 
hist(allecos$DEPTH)
hist(log(allecos$DEPTH))

Dep<-all.aea[!(is.na(all.aea$DEPTH)),] 
Dep.v <- variogram(log(DEPTH)~1, Dep, cutoff=2500, width=20)  
min(Dep.v$np) #ensure there are more than 50 pairs in each bin distance 
Dep.fit<-fit.variogram(Dep.v, vgm("Sph", "Exp")) #fit a model variogram
Dep.fit #shows parameters
#plot 
plot(variogramLine(Dep.fit, 2500), type='l', ylim=c(0,3), main = "depth") #this plots the line from the model 
points(Dep.v[,2:3], pch=21, bg='blue', col="black")


#Elevation
hist(allecos$ELEVMEAN)

elev<-all.aea[!(is.na(all.aea$ELEVMEAN)),] 
elev.v <- variogram(ELEVMEAN~1, elev, cutoff=2500, width=20)  
min(elev.v$np) #ensure there are more than 50 pairs in each bin distance 
elev.fit<-fit.variogram(elev.v, vgm("Sph", "Exp")) #no convergence 

#plot 
plot(variogramLine(elev.fit, 2500), type='l', main = "mean elevation")
points(elev.v[,2:3], pch=21, bg='blue', col="black")

#Summer Temp 
hist(allecos$Tsummer)

temp<-all.aea[!(is.na(all.aea$Tsummer)),] 
temp.v <- variogram(Tsummer~1, temp, cutoff=2500, width=20)  
min(temp.v$np) #ensure there are more than 50 pairs in each bin distance 
temp.fit<-fit.variogram(temp.v, vgm("Sph", "Exp")) #no convergence 

#plot 
plot(variogramLine(temp.fit, 2500), type='l', main = "summer temperature")  
points(temp.v[,2:3], pch=21, bg='blue', col="black")

##### ##### ##### ##### 
##### panel Plot ##### 
jpeg('pred_varios.jpeg',width = 10, height = 3, units = 'in', res = 600)
par(mfrow=c(1,5))
par(mar=c(2,2,1,1), oma=c(2,2,2,1))
#top left panel forest
plot(variogramLine(for.fit, 2500), type='l', ylim=c(0,2000), main = "% forest cover") #this plots the line from the model 
points(for.v[,2:3], pch=21, bg='blue', col="black") 
abline(v=for.fit[2,3], col="blue") 
abline(v=1900, col="blue") 
#mtext('a', side = 3, line = -1.5, adj = .02, cex = .7)

#top mid ag
plot(variogramLine(ag.fit, 2500), type='l', ylim=c(0,1500), main = "% agriculture cover") #this plots the line from the model 
points(ag.v[,2:3], pch=21, bg='blue', col="black")
abline(v=ag.fit[2,3], col="blue") #this plots the R range 
abline(v=1950, col="blue") #this plots the R range 
#mtext('b', side = 3, line = -1.5, adj = .02, cex = .7)

#top right depth
plot(variogramLine(Dep.fit, 2500), type='l', ylim=c(0,3), main = "depth") #this plots the line from the model 
points(Dep.v[,2:3], pch=21, bg='blue', col="black")
abline(v=Dep.fit[2,3], col="blue") 
#mtext('c', side = 3, line = -1.5, adj = .02, cex = .7)

#bottom mid elev
plot(variogramLine(elev.fit, 2500), type='l', main = "mean elevation") #this plots the line from the model 
points(elev.v[,2:3], pch=21, bg='blue', col="black")

#bottom right temp 
plot(variogramLine(temp.fit, 2500), type='l', main = "summer temperature") #this plots the line from the model 
points(temp.v[,2:3], pch=21, bg='blue', col="black")

#add axis labels
mtext("distance (km)", side = 1, outer = TRUE, cex = .9, line = 1)
mtext("semivariance", side = 2, outer = TRUE, cex = .9, line = 1)

dev.off()